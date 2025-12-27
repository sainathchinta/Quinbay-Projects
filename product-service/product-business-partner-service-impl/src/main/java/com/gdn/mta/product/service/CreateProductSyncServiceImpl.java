package com.gdn.mta.product.service;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemSyncStatus;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.enums.ProductSyncStatus;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductItemBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.partners.pbp.helper.ProductItemCreationRequestHelper;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author anand
 * @since Sep 2019
 */
@Slf4j
@Service
public class CreateProductSyncServiceImpl implements CreateProductSyncService {

  @Autowired
  private ProductItemSyncService syncStatusService;

  @Autowired
  private ProductStatusPublisherService publisherService;

  @Autowired
  private ProductItemBusinessPartnerRepository productItemBusinessPartnerRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductWorkflowServiceWrapper productWorkflowServiceWrapper;

  @Override
  public void createSyncProduct(String storeId, String username, String partnerCode, String pickupPoint, List<String> itemSkus) throws Exception{
    List<ProductItemBusinessPartner> productItemsBusinessPartner = productItemBusinessPartnerRepository
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(storeId, itemSkus);
    log.info("copying product {} to business partner {} and pickup point {}", itemSkus, partnerCode, pickupPoint);
    ProductBusinessPartner productBusinessPartner = productItemsBusinessPartner.stream()
      .findFirst().map(ProductItemBusinessPartner::getProductBusinessPartner).orElse(null);
    Optional.ofNullable(productBusinessPartner)
      .map(details -> createProductCreationPayload(details, productItemsBusinessPartner, storeId, partnerCode,
        pickupPoint))
      .map(status -> processUpdateSyncItemStatus(storeId, partnerCode, itemSkus, status))
      .orElseGet(() -> processUpdateSyncItemStatus(storeId, partnerCode, itemSkus, false));
  }

  private boolean createProductCreationPayload(ProductBusinessPartner productBusinessPartner,
    List<ProductItemBusinessPartner> productItemsBusinessPartner, String storeId, String partnerCode,
    String pickupPoint) {
    Map<String, ProductItemBusinessPartner> productItemsBusinessPartnerMap = productItemsBusinessPartner.stream()
      .collect(Collectors.toMap(ProductItemBusinessPartner::getProductItemId, Function.identity()));
    log.info("product item business partner details:{}", productItemsBusinessPartnerMap);
    ProductDetailResponse productData;
    ProfileResponse businessPartnerProfile;
    try {
      productData = this.getProductDetail(productBusinessPartner.getProductId());
      businessPartnerProfile = this.getBusinessPartnerProfile(partnerCode);
    } catch(Exception ex) {
      log.error("exception occurred {}",ex.getMessage(), ex);
      return false;
    }
    String productCode = productService.generateProductCode();
    log.debug("new product-code generated {} for create new sync-product", productCode);

    ProductCreationRequest request = ProductItemCreationRequestHelper.createProductItemRequestPayload(productData,
      businessPartnerProfile, productItemsBusinessPartnerMap, ProductCreationType.FLOW2_FBB, false, productCode,
      pickupPoint, productBusinessPartner.getCategoryName(), partnerCode);
    boolean isLogisticsSaveSuccess = false;
    try {
      log.info("for new sync product creation request {}", request);
      addJsonLogForProductItemCreationRequest(request);
      isLogisticsSaveSuccess = productWorkflowServiceWrapper.create(storeId, request, true, false, null);
    } catch(Exception ex) {
      log.error("failed to create sync product for productCode : {}", productCode, ex);
    }
    return isLogisticsSaveSuccess;
  }

  private void addJsonLogForProductItemCreationRequest(ProductCreationRequest request) {
    ObjectMapper mapper = new ObjectMapper();
    try {
      String jsonRequest = mapper.writeValueAsString(request);
      log.info("json log for ProductItemCreationRequest:{}", jsonRequest);
    } catch(Exception ex) {
      log.error("error occurred while generating json for ProductCreationRequest: {}, exception: {}", request, ex);
    }
  }

  private boolean processUpdateSyncItemStatus(String storeId, String partnerCode, List<String> itemSkus, boolean isSuccess) {
    if (isSuccess) {
      itemSkus.forEach(itemSku -> updateProductCopySyncStatus(storeId, partnerCode, itemSku, ProductSyncStatus.SUCCESS));
    } else {
      itemSkus.forEach(itemSku -> updateProductCopySyncStatus(storeId, partnerCode, itemSku, ProductSyncStatus.FAIL));
    }
    return true;
  }

  private boolean updateProductCopySyncStatus(String storeId, String partnerCode, String itemSku, ProductSyncStatus status) {
    ProductItemSyncStatus syncStatus = syncStatusService
      .findByItemSkuAndBusinessPartnerCode(storeId, itemSku, partnerCode);

    syncStatus = Optional.ofNullable(syncStatus)
      .map(process -> process.withStatus(status))
      .map(process -> syncStatusService.save(process))
      .orElseThrow(() -> new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
        "no item copy process exists for partner : " + partnerCode + " & item sku : " + itemSku));

    if (ProductSyncStatus.SUCCESS.equals(syncStatus.getProductSyncStatus())) {
      this.publisherService.publishProductSyncSuccessEvent(storeId, itemSku, partnerCode);
    }
    return true;
  }

  private ProfileResponse getBusinessPartnerProfile(String partnerCode) throws Exception {
    ProfileResponse businessPartnerProfile;
    try {
      businessPartnerProfile = businessPartnerRepository.filterDetailByBusinessPartnerCode(partnerCode);
      log.debug("ProfileResponse {} for Business Partner : {}", businessPartnerProfile, partnerCode);
    } catch(Exception ex) {
      log.error("ProfileResponse not found for Business Partner : {}", partnerCode);
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "Exception occurred while fetching ProfileResponse : " + ex.getMessage());
    }
    return businessPartnerProfile;
  }

  private ProductDetailResponse getProductDetail(String productId) throws Exception {
    ProductDetailResponse productData;
    try {
      productData = this.productRepository.findDetailById(productId);
      log.debug("Product Id {} with product detail {}", productId, productData);
    } catch(Exception ex) {
      log.error("ProductDetailResponse not found for Product Id : {}", productId);
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
        "Exception occurred while fetching ProductDetailResponse : " + ex.getMessage());
    }

    if (Objects.nonNull(productData) && !productData.isViewable()) {
      throw new ApplicationException(ErrorCategory.INVALID_STATE,
        "Product Code : " + productData.getProductCode() + ". Error Code : state of product is invalid");
    }
    return productData;
  }
}
