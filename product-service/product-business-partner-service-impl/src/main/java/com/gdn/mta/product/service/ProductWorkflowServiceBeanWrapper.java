package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemLogisticsRequest;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ItemFlagDetails;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.dto.workflow.product.ProductResubmitRequest;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEventsEnum;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3Wip;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3LogisticsService;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.businesspartner.commons.enums.MerchantStatus;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductWorkflowServiceBeanWrapper implements ProductWorkflowServiceWrapper {

  @Autowired
  private ProductWfService productWorkflowService;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Autowired
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Autowired
  private ProductMailEventService productMailEventService;

  @Autowired
  private ProductPublisherService productPublisherService;

  @Autowired
  private ProductService productService;

  @Lazy
  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ProductLevel3LogisticsService productLevel3LogisticsService;

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductAppealService productAppealService;

  @Override
  public void returnForCorrection(String storeId, String productCode, String notes) throws Exception {
    productWorkflowService.returnForCorrection(productCode, notes, null, false, false, true);
    deleteProductFromSolrCollection(storeId, productCode);
  }

  @Override
  public void resubmit(String storeId, ProductResubmitRequest productResubmitRequest,
      UpdateProductLevel3Wip updateProductLevel3Wip) throws Exception {
    productWorkflowService.resubmit(productResubmitRequest.getProductRequest(), updateProductLevel3Wip);
    ProductCollection productCollection = this.productCollectionRepository
        .findByStoreIdAndProductCode(storeId, productResubmitRequest.getProductRequest().getProductCode());
    this.solrReviewProductCollectionService.publishKafkaEventToAddProductToReviewProductCollection(productCollection);
    this.productLevel1HistoryService
        .addHistoryForProductResubmissionDueToContentOrImageRejection(productResubmitRequest);
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void deleteProductCollection(String storeId, String productCode, String notes, boolean needEmailNotification,
      boolean deleteFromPdt)
      throws Exception {
    ProductDetailResponse productDetailResponse = productService.findProductDetailByProductCode(productCode, false);
    this.productWorkflowService.delete(productCode, notes, needEmailNotification);
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    List<ProductBusinessPartner> savedProductBusinessPartners =
        this.productBusinessPartnerRepository.findByStoreIdAndProductId(storeId,
            productCollection.getProductId());
    savedProductBusinessPartners.stream().filter(ProductBusinessPartner::isAppealedProduct)
        .findFirst()
        .ifPresent(pbp -> savedProductBusinessPartners.forEach(productBusinessPartner -> {
          productBusinessPartner.setAppealedProduct(false);
          productAppealService.decrementCounterForProductAppeal(storeId,
              productBusinessPartner.getBusinessPartnerId());
        }));
    if (needEmailNotification && !productCollection.isPostLive()) {
      productMailEventService.createAndSaveMailEvent(productCode, notes, ProductMailEventsEnum.REJECTED);
    } else if (needEmailNotification) {
      productMailEventService
          .createAndSaveMailEvent(productCode, notes, ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED);
    }
    if (productCollection.isPostLive() || productCollection.isEdited()) {
      productService.deleteProductBusinessPartnerForPostLiveRejection(savedProductBusinessPartners,
          productCollection, notes);
      if (deleteFromPdt && productService.checkIfProductExistsInPDT(productCode, true)) {
          productService.removeProductFromPDT(productCode);
      }
    }
    itemService.publishItemStatusEvent(savedProductBusinessPartners, ProductStatus.REJECTED,
        storeId);
    productService.publishProductStatusEvent(productDetailResponse, productCollection, ProductStatus.REJECTED, notes);
    deleteProductFromSolrCollection(storeId, productCode);
  }

  private void deleteProductFromSolrCollection(String storeId, String productCode) throws Exception {
    String id = this.productCollectionRepository.findIdByStoreIdAndProductCode(storeId, productCode);
    this.solrReviewProductCollectionService.deleteProductFromReviewProductCollection(id);
  }

  @Override
  public void approveDraft(String storeId, String productCode) throws Exception {
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    ProfileResponse profileResponse =
        businessPartnerRepository.filterDetailByBusinessPartnerCode(productCollection.getBusinessPartnerCode());
    GdnPreconditions.checkArgument(
        Objects.nonNull(profileResponse) && MerchantStatus.ACTIVE.name().equals(profileResponse.getMerchantStatus()),
        ErrorMessages.INACTIVE_BUSINESS_PARTNER_MSG + productCollection.getBusinessPartnerCode());
    this.productWorkflowService.approveDraft(productCode);
  }

  @Override
  public boolean create(String storeId, ProductCreationRequest productCreationRequest, boolean isSkipNotification,
      boolean MPPFlow, ProfileResponse profileResponse) throws Exception {
    ProductCollection oldProductCollection = null;
    if (StringUtils.isNotEmpty(productCreationRequest.getOldProductCode())) {
      oldProductCollection = this.productCollectionRepository.findByStoreIdAndProductCode(storeId,
          productCreationRequest.getOldProductCode());
      if (oldProductCollection.isPostLive() || oldProductCollection.isEdited()) {
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
            ErrorMessages.PRODUCT_NEED_REVISION_MOBILE_ERROR);
      }
    }
    this.productWorkflowService.create(productCreationRequest, isSkipNotification, MPPFlow);
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCode(storeId, productCreationRequest.getProductCode());
    productCreationRequest.setUpdatedBy(productCollection.getCreatedBy());
    boolean isLogisticsSaveSuccess = true;
    if (StringUtils.isNotBlank(productCreationRequest.getBusinessPartnerCode())) {
      List<ItemFlagDetails> itemFlagDetails = productBusinessPartnerService
          .getAllItemSkusViewConfigByProductId(productCollection.getProductId());
      List<String> itemSkus =
          itemFlagDetails.stream().map(ItemFlagDetails::getItemSku).collect(Collectors.toList());
      List<ProductLevel3Logistics> logistics = getProductLevel3Logistics(productCreationRequest, MPPFlow);
      isLogisticsSaveSuccess = productLevel3LogisticsService.saveLogisticsByItemSku(itemSkus,
          productCreationRequest.getBusinessPartnerCode(), logistics, false);
      if (productCreationRequest.isContainsUrlImage()) {
        List<ProductBusinessPartner> productBusinessPartnerList = productBusinessPartnerService
            .findByStoreIdAndProductIdAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
                productCollection.getProductId());
        productCreationRequest.setGdnProductSku(productBusinessPartnerList.get(0).getGdnProductSku());
        log.info("Saving url history for productCode : {} ", productCreationRequest.getProductCode());
        productService
            .saveHistoryForUrlImage(productCreationRequest.getProductCode(), itemFlagDetails, productCreationRequest);
      }
    }
    if (StringUtils.isNotEmpty(productCreationRequest.getOldProductCode())) {
      productCollection.setResubmitCount(oldProductCollection.getResubmitCount() + 1);
      productCollection.setNeedRevision(true);
      productCollection.setAssignedBy(Constants.DEFAULT_ASSINGER);
      productCollection.setAssignedTo(oldProductCollection.getAssignedTo());
      this.productWorkflowService.updateResubmitCountOnProductResubmission(productCollection);
      productLevel1HistoryService
          .updateProductIdForRevisedProducts(oldProductCollection.getProductId(), productCollection.getProductId());
      productLevel1HistoryService.addHistoryForProductResubmitted(productCreationRequest);
      productBusinessPartnerService.markItemsAsDeletedOnProductResubmission(oldProductCollection.getBusinessPartnerCode(),
          oldProductCollection.getProductId());
      this.productService.clearMasterProductCache(productCreationRequest.getProductCode());
    }
    if (productCollection.isSkipReview()) {
      try {
        productServiceWrapper.skipScreeningForSkipReviewProduct(productCollection.getProductCode(), profileResponse);
      } catch (Exception e) {
        log.error("Error while skipping the screening for product : {}", productCollection.getProductCode(), e);
      }
    } else {
      if (productCollection.getPrioritySeller() > 0) {
        productPublisherService.publishProductImageResizeEventForPrioritySeller(productCollection.getProductCode(),
            productCollection.getStoreId(), productCollection.getPrioritySeller());
      } else {
        productPublisherService.publishProductImageResizeEvent(productCollection.getProductCode(),
            productCollection.getStoreId());
      }
    }
    return isLogisticsSaveSuccess;
  }

  private List<ProductLevel3Logistics> getProductLevel3Logistics(ProductCreationRequest productCreationRequest,
      boolean MPPFlow) {
    List<ProductLevel3Logistics> logistics = new ArrayList<>();
    if (MPPFlow) {
      getLogisticsRequest(productCreationRequest.getProductItemLogisticsRequests(), logistics);
    } else {
      List<ProductItemCreationRequest> productItemCreationRequests = productCreationRequest.getProductItemRequests();
      if (!CollectionUtils.isEmpty(productItemCreationRequests)) {
        getLogisticsRequest(productItemCreationRequests.get(0).getProductItemLogisticsRequests(), logistics);
      }
    }
    return logistics;
  }

  private void getLogisticsRequest(List<ProductItemLogisticsRequest> productItemLogisticsRequests,
      List<ProductLevel3Logistics> logistics) {
    for (ProductItemLogisticsRequest logisticsRequest : productItemLogisticsRequests) {
      ProductLevel3Logistics productLevel3Logistics =
          ProductLevel3Logistics.builder().logisticProductCode(logisticsRequest.getLogisticProductCode())
              .selected(logisticsRequest.isSelected()).build();
      logistics.add(productLevel3Logistics);
    }
  }
}
