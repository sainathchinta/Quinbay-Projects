package com.gdn.x.mta.distributiontask.service.impl;

import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import com.gdn.x.mta.distributiontask.model.AppealedProduct;
import com.gdn.x.mta.distributiontask.model.enums.SellerBadge;
import com.gdn.x.mta.distributiontask.model.enums.SellerBadgeConstants;
import com.gdn.x.mta.distributiontask.model.enums.SellerType;
import com.gdn.x.mta.distributiontask.service.api.AppealProductService;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.ScreeningProductApprovalEvent;
import com.gdn.partners.pdt.repository.configuration.distribution.AutoDistributionConfigurationRepository;
import com.gdn.partners.pdt.service.distribution.DistributionTaskHistoryService;
import com.gdn.partners.pdt.service.distribution.DistributionTaskService;
import com.gdn.x.mta.distributiontask.config.BeanConfiguration;
import com.gdn.x.mta.distributiontask.dao.api.ProductDistributionTaskRepository;
import com.gdn.x.mta.distributiontask.inbound.util.ProductDomainEventModelConverterUtils;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemAttribute;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.RevisedProductService;
import com.gdn.x.mta.distributiontask.service.api.VendorService;
import com.gdn.x.mta.distributiontask.service.impl.util.ConverterUtil;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class RevisedProductServiceImpl implements RevisedProductService {

  @Autowired
  private ProductService productService;

  @Autowired
  private DistributionTaskService distributionTaskService;

  @Autowired
  private AutoDistributionConfigurationRepository autoDistributionConfigurationRepository;

  @Autowired
  private VendorService vendorService;

  @Autowired
  private ProductDistributionTaskRepository distributionTaskRepository;

  @Autowired
  private DistributionTaskHistoryService distributionTaskHistoryService;

  @Autowired
  private ProductDistributionTaskService productDistributionTaskService;

  @Autowired
  private ProductReviewerService productReviewerService;

  @Autowired
  private AppealProductService appealProductService;

  @Value("${auto.distribute.switch}")
  private Boolean autoDistributeSwitch;

  @Value("${auto.distribute.default.vendorCode}")
  private String autoDistributeDefaultVendorCode;

  @Value("${appeal.product.enabled}")
  private boolean appealProductEnabled;

  @Override
  @Transactional(readOnly = false)
  public ProductAndReviewerDetailsDTO addRevisedProductOnSubmit(AddRevisedProductToPDTEvent addRevisedProductToPDTEvent,
      ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse) throws Exception {
    ProductDetailResponse productDetailResponse = distributionTaskService
        .getProductDetailByProductCode(BeanConfiguration.USER_NAME, addRevisedProductToPDTEvent.getProductCode());
    ProductAndReviewerDetailsDTO productAndReviewerDetailsDTO;
    boolean updateTaskCode = false;
    boolean updateState = false;
    Product existingProduct = this.productService.getProductByCode(addRevisedProductToPDTEvent.getProductCode());
    if (Objects.nonNull(existingProduct)) {
      existingProduct.setSellerType(addRevisedProductToPDTEvent.isTrustedSeller() ?
        SellerType.TRUSTED_SELLER :
        SellerType.NON_TRUSTED_SELLER);
      existingProduct.setSellerBadge(SellerBadgeConstants.fromSellerBadgeConstants(
        addRevisedProductToPDTEvent.getSellerBadge()));
      existingProduct.setDistributionMappingStatus(addRevisedProductToPDTEvent.getDistributionMappingStatus());
    }
    if (Objects.isNull(existingProduct)) {
      log.info("Add revised product to vendor : {} ", addRevisedProductToPDTEvent.getProductCode());
      productAndReviewerDetailsDTO = addRevisedProductToVendor(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse,
          productDetailResponse);
    } else {
      if (!(existingProduct.isMarkForDelete() && WorkflowState.PASSED.equals(existingProduct.getState()))) {
        if (!WorkflowState.NEED_CORRECTION.equals(existingProduct.getState())) {
          throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
              "Product not in correct state : " + addRevisedProductToPDTEvent.getProductCode());
        }
      }

      Product newProduct = ConverterUtil
          .convertProductDetailResponseToProduct(productDetailResponse, addRevisedProductToPDTEvent,
              imageQcProcessedAndBrandResponse);
      newProduct.setAppealedProduct(addRevisedProductToPDTEvent.isAppealedProduct());
      updateNewProductData(newProduct, existingProduct);
      existingProduct = overwriteDeletedData(existingProduct);

      log.info("product revised data, productCode {} , existing product state : {}, new product state : {}",
          addRevisedProductToPDTEvent.getProductCode(), existingProduct.getState(), newProduct.getState());

      ProductReviewer productReviewer = updateProductReviewerDetails(existingProduct, newProduct);
      productService.updateRevisedProductData(existingProduct, newProduct,
          addRevisedProductToPDTEvent.getAllModifiedFields());
      updateAppealProduct(addRevisedProductToPDTEvent, newProduct);

      productAndReviewerDetailsDTO = new ProductAndReviewerDetailsDTO(newProduct, productReviewer);
      updateTaskCode = true;
      updateState = true;

      productAndReviewerDetailsDTO.getProduct().setRevised(existingProduct.isRevised());
      productAndReviewerDetailsDTO.getProduct().setEdited(existingProduct.isEdited());
    }

    if (updateState) {
      ProductDistributionTask productDistributionTask =
          productDistributionTaskService.findByProductId(existingProduct.getId());
      this.productDistributionTaskService.updateState(productDistributionTask, productAndReviewerDetailsDTO.getProduct().getState());
    }

    if (updateTaskCode && Objects.nonNull(productAndReviewerDetailsDTO.getProduct().getCurrentVendor())) {
      generateDistributionTask(existingProduct, productAndReviewerDetailsDTO.getProduct(), existingProduct.getCurrentVendor());
    }
    return productAndReviewerDetailsDTO;
  }

  private void updateNewProductData(Product newProduct, Product existingProduct) {
    newProduct.setState(WorkflowState.IN_REVIEW);
    newProduct.setCurrentVendor(existingProduct.getCurrentVendor());
    newProduct.setEdited(existingProduct.isEdited());
    newProduct.setReviewType(existingProduct.getReviewType());
    existingProduct.setRestrictedKeywordsPresent(newProduct.isRestrictedKeywordsPresent());
    existingProduct.setForceReview(newProduct.isForceReview());
    existingProduct.setSellerType(Optional.ofNullable(newProduct).map(Product::getSellerType).orElse(null));
    existingProduct.setSellerBadge(Optional.ofNullable(newProduct.getSellerBadge()).orElse(
      SellerBadge.NONE_MERCHANT));
  }

  private ProductAndReviewerDetailsDTO addRevisedProductToVendor(AddRevisedProductToPDTEvent addRevisedProductToPDTEvent,
      ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse, ProductDetailResponse productDetailResponse)
      throws Exception {
    ScreeningProductApprovalEvent screeningProductApprovalEvent = new ScreeningProductApprovalEvent();
    BeanUtils.copyProperties(addRevisedProductToPDTEvent, screeningProductApprovalEvent);
    screeningProductApprovalEvent.setRestrictedKeywordsDetected(addRevisedProductToPDTEvent.getRestrictedKeywordsDetected());
    screeningProductApprovalEvent.setTrustedSeller(addRevisedProductToPDTEvent.isTrustedSeller());
    screeningProductApprovalEvent.setDistributionMappingStatus(addRevisedProductToPDTEvent.getDistributionMappingStatus());
    Product product = ProductDomainEventModelConverterUtils
        .convertProductDomainEventModelToProduct(productDetailResponse, screeningProductApprovalEvent,
          false, imageQcProcessedAndBrandResponse);
    product.setRevised(true);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    product.setAppealedProduct(addRevisedProductToPDTEvent.isAppealedProduct());

    // get vendor data
    Vendor vendor = getVendorData(product);

    if (Objects.nonNull(vendor)) {
      product.setCurrentVendor(vendor);
    }
    initializeProductData(product);
    productService.updateProductNotesForRevisedProducts(product, addRevisedProductToPDTEvent.getAllModifiedFields());
    Product newProduct = this.productService.createProduct(product);
    newProduct.setSellerType(addRevisedProductToPDTEvent.isTrustedSeller() ?
      SellerType.TRUSTED_SELLER :
      SellerType.NON_TRUSTED_SELLER);
    newProduct.setSellerBadge(
      SellerBadgeConstants.fromSellerBadgeConstants(addRevisedProductToPDTEvent.getSellerBadge()));
    ProductReviewer productReviewer = updateProductReviewerDetails(newProduct, null);

    updateAppealProduct(addRevisedProductToPDTEvent, newProduct);

    if (Objects.nonNull(vendor)) {
      generateDistributionTask(newProduct, product, vendor);
    }
    return new ProductAndReviewerDetailsDTO(newProduct, productReviewer);
  }

  private void updateAppealProduct(AddRevisedProductToPDTEvent addRevisedProductToPDTEvent,
    Product product) {
    if (appealProductEnabled && addRevisedProductToPDTEvent.isAppealedProduct()) {
      AppealedProduct appealedProduct = Optional.ofNullable(
        appealProductService.findAppealProductByProductCode(product.getProductCode())).orElseGet(
        () -> AppealedProduct.builder().productCode(product.getProductCode())
          .businessPartnerCode(product.getBusinessPartnerCode()).build());
      appealedProduct.setAppealedProductNotes(
        addRevisedProductToPDTEvent.getAppealedProductNotes());
      appealProductService.upsertAppealProduct(appealedProduct);
    }
  }

  private void initializeProductData(Product product) {
    product.setState(WorkflowState.IN_REVIEW);
    product.setProductCreatedDate(Calendar.getInstance().getTime());
    product.setEdited(false);
  }

  private Vendor getVendorData(Product newProduct) throws Exception {
    String vendorCode;
    if (autoDistributeSwitch) {
      vendorCode = this.autoDistributionConfigurationRepository
          .findVendorCodeByStoreIdAndPriorityValuesAndMarkForDeleteFalse(newProduct.getStoreId(), Arrays
              .asList(newProduct.getCreatedBy(), newProduct.getBusinessPartnerCode(), newProduct.getCategoryCode()));
    } else {
      vendorCode = autoDistributeDefaultVendorCode;
    }
    return this.vendorService.findByVendorCode(vendorCode);
  }

  private void generateDistributionTask(Product product, Product newProduct, Vendor vendor) throws Exception {
    List<ProductDistributionTask> distributionTasks = this.distributionTaskService
        .generateDistributionTaskForProduct(product.getStoreId(), vendor, Collections.singletonList(product),
            newProduct.getState());
    this.distributionTaskRepository.updateProductDistributionTask(Collections.singletonList(product));
    this.distributionTaskRepository.saveAll(distributionTasks);
    this.distributionTaskHistoryService.create(distributionTasks);
  }


  private Product overwriteDeletedData(Product existingProduct) {
    log.info("overwrite deleted data for revised product: {} ", existingProduct.getProductCode());
    existingProduct.setMarkForDelete(false);
    this.updateMFDFlag(existingProduct);
    return existingProduct;
  }


  private void updateMFDFlag(Product savedProduct) {
    for (ProductItem productItem : savedProduct.getProductItems()) {
      productItem.setMarkForDelete(false);
      productItem.getProductItemImages().forEach(this::setMFDProductItemImage);
      productItem.getProductItemAttributes().forEach(this::setMFDFlagInAttributes);
    }
    savedProduct.getProductAttributes().forEach(productAttribute -> productAttribute.setMarkForDelete(false));
    savedProduct.getProductImages().forEach(productImage -> productImage.setMarkForDelete(false));
  }

  private ProductItemImage setMFDProductItemImage(ProductItemImage productItemImage) {
    productItemImage.setMarkForDelete(false);
    return productItemImage;
  }

  private ProductItemAttribute setMFDFlagInAttributes(ProductItemAttribute productItemAttribute) {
    productItemAttribute.setMarkForDelete(false);
    return productItemAttribute;
  }

  private ProductReviewer updateProductReviewerDetails(Product existingProduct, Product newProduct) {
    ProductReviewer productReviewer =
        productReviewerService.findProductReviewerByStoreIdAndProductCode(existingProduct.getStoreId(), existingProduct.getProductCode());
    if(Objects.isNull(productReviewer)){
      return productReviewerService.addNewProduct(existingProduct.getStoreId(), existingProduct.getProductCode());
    }
    else {
      boolean clearAssignee = false;
      // Check if product can be moved between prelive - post-live
      if (Objects.nonNull(newProduct) && existingProduct.isPostLive() != newProduct.isPostLive()) {
        clearAssignee = true;
      }
      return productReviewerService.resetAssignmentData(productReviewer, clearAssignee);
    }
  }
}
