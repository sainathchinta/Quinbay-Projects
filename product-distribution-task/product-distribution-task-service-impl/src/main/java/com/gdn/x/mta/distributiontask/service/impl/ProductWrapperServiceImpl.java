package com.gdn.x.mta.distributiontask.service.impl;

import static com.gdn.x.mta.distributiontask.model.Constants.MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gdn.partners.pdt.dto.configuration.distribution.ApproveProductResponseDto;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.dao.exception.ValidationException;
import com.gdn.x.mta.distributiontask.domain.event.InternalHistoryEventModel;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTPermanentDeleteResultEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductCombinedUpdateToSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrDeleteDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductChange;
import com.gdn.x.mta.distributiontask.model.ProductActionRetry;
import com.gdn.x.mta.distributiontask.model.ProductMigration;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.cache.CacheKeys;
import com.gdn.x.mta.distributiontask.model.dto.CategoryDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.dto.RejectReasonDto;
import com.gdn.x.mta.distributiontask.model.enums.ActionRetryStatus;
import com.gdn.x.mta.distributiontask.model.enums.DistributionStatus;
import com.gdn.x.mta.distributiontask.model.enums.ProductMigrationStatus;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.PublishAndSavedProductAndHistoryModel;
import com.gdn.x.mta.distributiontask.rest.model.response.QuickApprovalResponse;
import com.gdn.x.mta.distributiontask.service.api.ErrorMessages;
import com.gdn.x.mta.distributiontask.service.api.ProductMigrationService;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;

import com.gdn.x.mta.distributiontask.service.api.ProductsPermanentDeleteService;
import com.gdn.x.mta.distributiontask.service.api.TaskHistoryService;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;
import com.gdn.x.mta.distributiontask.service.impl.util.ConverterUtil;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import jakarta.transaction.Transactional;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import com.gdn.mta.domain.event.modal.ImageQcProcessedResponseDomainEvent;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.x.mta.distributiontask.dao.util.VendorProductSolrHelper;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.SystemParameterConfig;
import com.gdn.x.mta.distributiontask.model.dto.BulkScreeningProductActionsDTO;
import com.gdn.x.mta.distributiontask.model.dto.BulkVendorProductActionsDTO;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import com.gdn.x.mta.distributiontask.model.solr.SolrConstants;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.NeedRevisionRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.BulkVendorProductActionsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.NeedRevisionResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorProductActionsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.mta.distributiontask.service.api.ProductAutoApprovalService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductActionRetryService;
import com.gdn.x.mta.distributiontask.service.api.ProductWipService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import com.gdn.x.mta.distributiontask.service.api.SystemParameterConfigService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ApprovedProductPublisherService;
import com.gdn.x.mta.distributiontask.service.api.publisher.SolrReindexPublisherService;
import com.gdn.x.mta.distributiontask.service.impl.util.ProductUtils;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductWrapperServiceImpl implements ProductWrapperService {

  @Autowired
  private ProductService productService;

  @Autowired
  private ApprovedProductPublisherService approvedProductPublisherService;

  @Autowired
  private SolrVendorCollectionService solrVendorCollectionService;

  private static final String CONTENT = "content";

  @Autowired
  private ProductWipService productWipService;

  @Autowired
  private ProductAutoApprovalService productAutoApprovalService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private SolrReindexPublisherService solrReindexPublisherService;

  @Autowired
  private ProductReviewerService productReviewerService;

  @Autowired
  private ProductMigrationService commonImageMigrationService;

  @Autowired
  private ProductUtils productUtils;

  @Autowired
  private ProductActionRetryService productActionRetryService;

  @Autowired
  private ProductServiceRepository productServiceRepository;

  @Autowired
  private TaskHistoryService taskHistoryService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductsPermanentDeleteService productsPermanentDeleteService;

  @Value("${auto.heal.auto.approval.product.data}")
  private boolean autoHealAutoApprovalProductData;

  @Value("${auto.distribute.default.vendorCode}")
  private String autoDistributeDefaultVendorCode;

  @Value("${auto.solr.reindexing}")
  private boolean autoSolrReindexingEnabled;

  @Value("${replace.empty.review.type}")
  private boolean replaceEmptyReviewType;

  @Value("${permanent.delete.update.date.difference}")
  private long permanentDeleteUpdateDateDifference;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Value("${distribution.seller.list}")
  private Set<String> distributionSellerList;

  @Override
  public void updateAndApproveProduct(Product product, String vendorCode, String notes) throws Exception {
    String username = GdnMandatoryRequestParameterUtil.getUsername();
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductReviewer productReviewer =
        productReviewerService.findProductReviewerByProductCode(product.getProductCode());
    ApproveProductResponseDto responseDto =
        this.productService.updateAndApproveProduct(vendorCode, product, notes, false, productReviewer);
    Product existingProduct = responseDto.getProduct();
    log.info("Product published on updateAndApproveProduct: {}", existingProduct);
    if (WorkflowState.PASSED.equals(existingProduct.getState())) {
      if (existingProduct.isRevised()) {
        this.approvedProductPublisherService.publishRevisedVendorApprovedEvent(existingProduct, false);
      } else if (existingProduct.isEdited()) {
        this.approvedProductPublisherService.publishEditedVendorApprovedEvent(existingProduct);
      } else {
        this.approvedProductPublisherService.publishVendorApprovedEvent(existingProduct, false);
      }
    }
    if (responseDto.isPublishHistoryEvent()) {
      log.info("Publishing pdt history event {} for {} ", DomainEventName.PDT_PRODUCT_HISTORY_EVENT,
        product.getProductCode());
      kafkaProducer.send(DomainEventName.PDT_PRODUCT_HISTORY_EVENT, existingProduct.getProductCode(),
        taskHistoryService.generatePDTHistoryEventModel(storeId, username, existingProduct,
          existingProduct.getCurrentVendor(), responseDto.getReason(), responseDto.getWorkFlowState(),
          responseDto.getTaskCode()));
      log.info("Publishing internal history event {} for {} ",
        kafkaTopicProperties.getInternalHistoryEventName(), product.getProductCode());
      kafkaProducer.send(kafkaTopicProperties.getInternalHistoryEventName(),
        product.getProductCode(),
        ConverterUtil.convertToInternalHistoryModel(product.getStoreId(), product.getProductCode(),
          GdnMandatoryRequestParameterUtil.getUsername(), responseDto.getWorkFlowState().getDesc(),
          responseDto.getReason()));
    }
    handleNullReviewType(product);
    solrReindexPublisherService.publishPDTProductApprovalToSolr(
        VendorProductSolrHelper.toProductUpdateProductToSolrEventModel(existingProduct, productReviewer));
  }

  @Override
  public Product updateProductDetails(Product existingProduct, Product newProduct) throws Exception {
    Product product = productService.updateProductDetails(existingProduct, newProduct);
    ProductReviewer productReviewer = productReviewerService
        .findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, existingProduct.getProductCode());
    solrVendorCollectionService.updateSolrOnApprovalOrSave(product, productReviewer, CONTENT);
    return product;
  }

  @Override
  public Product updateEditedProductDetails(Product existingProduct, Product newProduct, List<String> modifiedFields) throws Exception {
    return productService.updateEditedProductDetails(existingProduct, newProduct, modifiedFields);
  }

  @Override
  public void updateImageQcResponseByProductCode(
      ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent) throws Exception {
    Product product = productService.getDetailsForProductByProductCode(imageQcProcessedResponseDomainEvent.getProductCode());
    if (Objects.nonNull(product)) {
      PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
          productService.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
      boolean publishEvent = publishAndSavedProductAndHistoryModel.getDoPublish();
      if(autoHealAutoApprovalProductData) {
        product = productService.autoHealProductData(product, Constants.AUTOHEAL_AUTO_APPROVE);
      }
      ProductReviewer productReviewer = productReviewerService.findProductReviewerByProductCode(product.getProductCode());
      solrVendorCollectionService.publishSolrAddPDTProductBatchEvent(
          Collections.singletonList(new ProductAndReviewerDetailsDTO(product, productReviewer)));
      if (publishEvent) {
        publishImageQcProcessedEvent(imageQcProcessedResponseDomainEvent, product, publishEvent);
      }
      if (Objects.nonNull(publishAndSavedProductAndHistoryModel.getInternalHistoryEventModel())) {
        productService.publishInternalHistoryEventForProduct(
            publishAndSavedProductAndHistoryModel.getInternalHistoryEventModel());
      }
      if (StringUtils.isNotEmpty(imageQcProcessedResponseDomainEvent.getCategoryCode())) {
        log.info("Product eligible for category change for productCode : {} ",
            imageQcProcessedResponseDomainEvent.getProductCode());
        productAutoApprovalService.addProductsToAutoApprovalTable(Constants.DEFAULT_STORE_ID,
            Collections.singletonList(imageQcProcessedResponseDomainEvent.getProductCode()),
            Collections.singletonMap(imageQcProcessedResponseDomainEvent.getProductCode(),
                imageQcProcessedResponseDomainEvent.getCategoryCode()));
      }
    } else {
      if (AutoApprovalType.CONTENT_AND_IMAGE.equals(imageQcProcessedResponseDomainEvent.getAutoApprovalType())
          || StringUtils.isNotEmpty(imageQcProcessedResponseDomainEvent.getCategoryCode())) {
        productAutoApprovalService.addProductsToAutoApprovalTable(Constants.DEFAULT_STORE_ID,
          Collections.singletonList(imageQcProcessedResponseDomainEvent.getProductCode()),
          Collections.singletonMap(imageQcProcessedResponseDomainEvent.getProductCode(),
            imageQcProcessedResponseDomainEvent.getCategoryCode()));
      }
    }
    log.info("Image qc data updated in PDT for productCode : {} ",
        imageQcProcessedResponseDomainEvent.getProductCode());
  }

  private void publishImageQcProcessedEvent(ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent,
    Product product, boolean publishEvent) {
    try {
      productService.publishAutoApprovalEvents(publishEvent, product);
    } catch (Exception e) {
      log.error(ErrorMessages.ERROR_PUBLISHING_EVENT, product.getProductCode(), e);
      productAutoApprovalService.addProductsToAutoApprovalTable(product.getStoreId(),
        Collections.singletonList(imageQcProcessedResponseDomainEvent.getProductCode()),
        Collections.singletonMap(imageQcProcessedResponseDomainEvent.getProductCode(),
          product.getCategoryCode()));
      throw e;
    }
  }

  @Override
  public void updateBrandApprovalStatusAndUpdateSolr(
      BrandApprovedOrRejectedDomainEventModel brandApprovedOrRejectedDomainEventModel) {
    List<String> productCodes =
        productService.updateBrandApprovalStatus(brandApprovedOrRejectedDomainEventModel);
    this.solrVendorCollectionService
        .updateSolrOnBrandApprovalAndRejection(productCodes, brandApprovedOrRejectedDomainEventModel.getBrandName(),
            brandApprovedOrRejectedDomainEventModel.getBrandApprovalStatus());
    if (SolrConstants.APPROVED.equalsIgnoreCase(brandApprovedOrRejectedDomainEventModel.getBrandApprovalStatus())) {
      productAutoApprovalService.addProductsToAutoApprovalTable(Constants.DEFAULT_STORE_ID,
        productCodes, Collections.emptyMap());
    }
  }

  @Override
  public void deleteProductWipAndReindexSolr(String productCode, String notes) throws Exception {
    this.productWipService.deleteProductWip(productCode, notes);
    solrVendorCollectionService.deleteProductFromSolr(productCode);
  }

  @Override
  public void removeProductAndDeleteOriginalImages(RemoveProductRequest removeProductRequest) throws Exception {
    Product product = productService.getProductByCode(removeProductRequest.getProductCode());
    updateStateOfProduct(product, removeProductRequest.getState());
    productService.removeProductWithMarkForDelete(product, StringUtils.EMPTY);
    solrVendorCollectionService.deleteProductFromSolr(product.getProductCode());
    productService.deleteOriginalImagesForProductAndItems(product);
  }

  private void updateStateOfProduct(Product product, String state) {
    if (StringUtils.isNotEmpty(state)) {
      product.setState(WorkflowState.valueOf(state));
    }
  }

  @Override
  public NeedRevisionResponse doProductNeedForCorrectionAndReindexSolr(String storeId, String requestId, String username,
    String vendorCode, NeedRevisionRequest request) throws Exception {
    return productService.doProductNeedForCorrection(storeId, requestId, username, vendorCode, request);
  }

  @Override
  public void sendProductBackToVendorAndReindexSolr(String productCode) throws Exception {
    productReviewerService.clearExistingReviewDatesDetails(productCode);
    Product savedProduct = productService.sendProductBackToVendor(productCode);
    solrVendorCollectionService.updateSolrOnProductSentBackToVendor(savedProduct);
  }

  @Override
  public void rejectProductByVendorAndDeleteFromSolr(RejectProductDTO rejectProductDTO,
      String vendorCode) throws Exception {
    this.productService.rejectProductByVendor(rejectProductDTO, vendorCode);
    this.solrVendorCollectionService.deleteProductFromSolr(rejectProductDTO.getProductCode());
  }

  @Override
  public void pdtAutoRejectForPendingProducts(List<ProductActionRetry> productActionRetryList) {
    for (ProductActionRetry productActionRetry : productActionRetryList) {
      try {
        log.info("Performing rejection for product code = {} having auto reject as definitive actions",
                productActionRetry.getProductCode());
        RejectProductDTO rejectProductDTO = new RejectProductDTO();
        rejectProductDTO.setProductCode(productActionRetry.getProductCode());
        rejectProductDTO.setNotes(productActionRetry.getData());
        rejectProductDTO.setBulkAction(false);
        rejectProductDTO.setSchedulerAction(true);
        RejectReasonDto rejectReasonDto = new RejectReasonDto();
        rejectReasonDto.setProduct(Collections.singletonList(StringUtils.EMPTY));
        rejectProductDTO.setRejectReasonDto(rejectReasonDto);
        this.rejectProductByVendorAndDeleteFromSolr(rejectProductDTO, autoDistributeDefaultVendorCode);
        productActionRetry.setStatus(ActionRetryStatus.SUCCESS);
        productActionRetryService.updateProductActionRetryDetails(productActionRetry);
      } catch (ValidationException e) {
        log.error("Error while performing rejection for product code = {} having auto reject as definitive actions ",
            productActionRetry.getProductCode(), e);
        productActionRetry.setStatus(ActionRetryStatus.FAILED);
        productActionRetry.setFailedReason(e.getErrorMessage());
        productActionRetryService.updateProductActionRetryDetails(productActionRetry);
      }
      catch (Exception e) {
        log.error("Error while performing rejection for product code = {} having auto reject as definitive actions ",
                productActionRetry.getProductCode(), e);
        productActionRetry.setStatus(ActionRetryStatus.FAILED);
        productActionRetry.setFailedReason(e.getMessage());
        productActionRetryService.updateProductActionRetryDetails(productActionRetry);
      }
    }
  }

  @Override
  public BulkVendorProductActionsResponse bulkUpdateProductAssignee(String storeId,
      BulkVendorProductActionsDTO bulkVendorProductActionsDTO) {
    Date date = new Date();
    BulkVendorProductActionsResponse bulkVendorProductActionsResponse = new BulkVendorProductActionsResponse();
    for (BulkScreeningProductActionsDTO bulkScreeningProductActionsDTO : bulkVendorProductActionsDTO
        .getBulkScreeningProductActionsRequests()) {
      VendorProductActionsResponse vendorProductActionsResponse = new VendorProductActionsResponse();
      try {
        productService.bulkVendorProductAction(storeId, date, bulkScreeningProductActionsDTO);
        solrVendorCollectionService.updateReviewerByProductCodes(bulkScreeningProductActionsDTO.getProductCodes(),
            bulkScreeningProductActionsDTO.getAssignTo(), date);
      } catch (Exception e) {
        log.error("Error when updating bulk assignee ", e);
        vendorProductActionsResponse =
            new VendorProductActionsResponse(bulkScreeningProductActionsDTO.getProductCodes(), false, e.getMessage());
      }
      bulkVendorProductActionsResponse.getVendorProductActionsResponses().add(vendorProductActionsResponse);
    }
    return bulkVendorProductActionsResponse;
  }

  @Override
  public void updateAssigneeDetails(String storeId, List<String> productCodes, String assignedBy, String action,
      String assignedTo) throws Exception {
    Date date = new Date();
    List<String> updatedProductCodes = productService.doVendorProductAction(storeId, productCodes,
        assignedBy, action, assignedTo, date);
    solrVendorCollectionService.updateReviewerByProductCodes(updatedProductCodes, assignedTo, date);
  }

  @Override
  public void autoApproveOfPendingProductsAfterEligibilityCheck(String storeId, String productCode,
      boolean isEligibleForAutoApproval, AutoApprovalTypeResponse autoApprovalTypeResponse) {
    try {
      SystemParameterConfig maxNumberOfDaysToApproveProducts = systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
          MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS);
      int maxNumberOfDaysToApproveAssignedProducts = Integer.parseInt(maxNumberOfDaysToApproveProducts.getValue());
      Product product = productService.getDetailsForProductByProductCodeAndMarkForDeleteFalse(productCode);
      if (autoHealAutoApprovalProductData) {
        product = productService.autoHealProductData(product, Constants.AUTOHEAL_AUTO_APPROVE);
      }
      boolean categoryDataToBeUpdated =
          ConverterUtil.isCategoryDataToBeUpdated(productCode, autoApprovalTypeResponse, product);
      Pair<AutoApprovalStatus, InternalHistoryEventModel> autoApprovalStatusAndHistoryModel =
          productService.autoApproveOfPendingProductsAfterEligibilityCheck(product, isEligibleForAutoApproval,
              maxNumberOfDaysToApproveAssignedProducts);
      AutoApprovalStatus autoApprovalStatus = autoApprovalStatusAndHistoryModel.getLeft();
      if (AutoApprovalStatus.SUCCESS.equals(autoApprovalStatus)) {
        solrVendorCollectionService.autoApprovalReviewerDetailsAndUpdateState(productCode, WorkflowState.PASSED);
        productService.publishAutoApprovalEvents(Boolean.TRUE, product);
        productAutoApprovalService.updateProductAutoApprovalDetailsByProductCode(storeId,
            productCode, autoApprovalStatus, true);
      } else if (categoryDataToBeUpdated) {
        log.info("Saving category updated product : {} ", product.getProductCode());
        productService.updateProduct(product);
        ProductReviewer productReviewer =
            productReviewerService.findProductReviewerByProductCode(product.getProductCode());
        solrVendorCollectionService.publishSolrAddPDTProductBatchEvent(
            Collections.singletonList(new ProductAndReviewerDetailsDTO(product, productReviewer)));
      }
      if (Objects.nonNull(autoApprovalStatusAndHistoryModel.getRight())) {
        productService.publishInternalHistoryEventForProduct(autoApprovalStatusAndHistoryModel.getRight());
      }
    } catch (Exception e) {
      log.error("Error in system while auto approving product : {} ", productCode, e);
      productAutoApprovalService.updateProductAutoApprovalDetailsByProductCode(storeId,
          productCode, AutoApprovalStatus.FAILED, true);
    }
  }

  @Override
  public void updateProductToAutoNeedRevision(String storeId, AutoNeedRevisionRequest autoNeedRevisionRequest,
      boolean validateAssignment) throws Exception {
    Product product =
        productService.updateProductToAutoNeedRevision(storeId, autoNeedRevisionRequest, validateAssignment);
    productService.updateProductTaskAndHistory(product, autoNeedRevisionRequest);
    ProductReviewer productReviewer = productReviewerService.findProductReviewerByProductCode(product.getProductCode());
    solrVendorCollectionService.publishSolrAddPDTProductBatchEvent(
        Collections.singletonList(new ProductAndReviewerDetailsDTO(product, productReviewer)));
  }

  @Override
  public VendorQuickApprovalResponse quickApproveProduct(VendorQuickApprovalRequest vendorQuickApprovalRequest)
      throws Exception {
    String username = GdnMandatoryRequestParameterUtil.getUsername();
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
      ProductReviewer productReviewer =
        productReviewerService.findProductReviewerByProductCode(vendorQuickApprovalRequest.getProductCode());
    Product product =
      productService.getDetailsForProductByProductCode(vendorQuickApprovalRequest.getProductCode());
    if (autoSolrReindexingEnabled) {
      if (product.isMarkForDelete()) {
        PDTProductSolrDeleteDomainEventModel pdtProductSolrDeleteDomainEventModel =
            new PDTProductSolrDeleteDomainEventModel();
        pdtProductSolrDeleteDomainEventModel.setProductCodes(Collections.singletonList(product.getProductCode()));
        log.info(
            "Publishing combined update to solr event for productCode {} , pdtProductSolrDeleteDomainEventModel {} ",
            product.getProductCode(), pdtProductSolrDeleteDomainEventModel);
        solrReindexPublisherService
            .publishPDTProductSolrBatchDeleteDomainEventModelForReindex(pdtProductSolrDeleteDomainEventModel);
      }
    }
    QuickApprovalResponse quickApproveProduct =
        this.productService.quickApproveProduct(vendorQuickApprovalRequest.getVendorCode(),
            vendorQuickApprovalRequest.getProductCode(), vendorQuickApprovalRequest.getNotes(), productReviewer,
          vendorQuickApprovalRequest.isBulkAction(), product);
    ApproveProductResponseDto responseDto = quickApproveProduct.getApproveProductResponseDto();
    log.info("Product published on updateAndApproveProduct: {} vendorQuickApprovalResponse : {}",
        quickApproveProduct.getProduct(), quickApproveProduct.getVendorQuickApprovalResponse());
    if (WorkflowState.PASSED.equals(quickApproveProduct.getProduct().getState()) && CollectionUtils.isEmpty(
        quickApproveProduct.getVendorQuickApprovalResponse().getErrorCodes())) {
      if (quickApproveProduct.getProduct().isRevised()) {
        this.approvedProductPublisherService.publishRevisedVendorApprovedEvent(quickApproveProduct.getProduct(), false);
      } else if (quickApproveProduct.getProduct().isEdited()) {
        this.approvedProductPublisherService.publishEditedVendorApprovedEvent(quickApproveProduct.getProduct());
      } else {
        this.approvedProductPublisherService.publishVendorApprovedEvent(quickApproveProduct.getProduct(), false);
      }
      if (responseDto.isPublishHistoryEvent()) {
        log.info("Publishing pdt history event for {} ", product.getProductCode());
        kafkaProducer.send(DomainEventName.PDT_PRODUCT_HISTORY_EVENT,
          quickApproveProduct.getProduct().getProductCode(),
          taskHistoryService.generatePDTHistoryEventModel(storeId, username,
            quickApproveProduct.getProduct(), quickApproveProduct.getProduct().getCurrentVendor(),
            responseDto.getReason(), responseDto.getWorkFlowState(), responseDto.getTaskCode()));
        log.info("Publishing internal history event for {} ", product.getProductCode());
        kafkaProducer.send(kafkaTopicProperties.getInternalHistoryEventName(),
          product.getProductCode(),
          ConverterUtil.convertToInternalHistoryModel(product.getStoreId(),
            product.getProductCode(), GdnMandatoryRequestParameterUtil.getUsername(),
            responseDto.getWorkFlowState().getDesc(), responseDto.getReason()));
      }
      handleNullReviewType(quickApproveProduct.getProduct());
      solrReindexPublisherService.publishPDTProductApprovalToSolr(
          VendorProductSolrHelper.toProductUpdateProductToSolrEventModel(quickApproveProduct.getProduct(), productReviewer));
    }
    return quickApproveProduct.getVendorQuickApprovalResponse();
  }

  private void handleNullReviewType(Product product) {
    if(replaceEmptyReviewType && Objects.isNull(product.getReviewType())){
      log.info("Review Type was override for product : {} ", product.getProductCode());
      product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    }
  }

  @Override
  public void backfillCommonImageFlagInProductAndItemImages(String storeId, String productCode) {
    ProductMigration commonImageMigration = null;
    try {
      commonImageMigration = commonImageMigrationService.findProductMigrationByProductCodeAndStatus(productCode,
          ProductMigrationStatus.PUBLISHED.name());
      commonImageMigration =
          updateCommonImageMigration(commonImageMigration, ProductMigrationStatus.IN_PROGRESS.name());
      Product product = productService.getDetailsForProductByProductCodeAndMarkForDeleteFalse(productCode);
      productUtils.setCommonImageFlagForProductAndItemImages(product);
      productService.update(product);
      updateCommonImageMigration(commonImageMigration, ProductMigrationStatus.SUCCESS.name());
    } catch (Exception e) {
      log.error("Error while back filling product and item common images for productCode : {} ", productCode, e);
      updateCommonImageMigration(commonImageMigration, ProductMigrationStatus.FAILED.name());
    }
  }

  private ProductMigration updateCommonImageMigration(ProductMigration commonImageMigration, String status) {
    commonImageMigration.setStatus(status);
    return commonImageMigrationService.saveProductMigration(commonImageMigration);
  }

  @Override
  public Product getAllProductDetailsByCodeAndMarkForDeleteFalse(String code) throws Exception {
    Product product = productService.getAllProductDetailsByCode(code);
    product = productService.autoHealProductData(product, Constants.AUTOHEAL);
    productService.autoHealProductDistributionTask(product);
    return product;
  }

  @Override
  public void processVendorSearchAutoHealProduct(String storeId, String productCode) {
    Product product = productService.getProductByCode(productCode);
    if (Objects.isNull(product)) {
      return;
    }
    if (!product.isMarkForDelete() && WorkflowState.IN_REVIEW.equals(product.getState())) {
      solrVendorCollectionService.deltaReindexPDTProductSolr(storeId, productCode);
    } else if (product.isMarkForDelete() && WorkflowState.PASSED.equals(product.getState())) {
      log.info("Product is in passed state. Calling PBP for product activation productCode : {} ", productCode);
      productServiceRepository.processProductVendorSearchAutoHeal(storeId, productCode);
    }
  }

  @Override
  public void updateBrandInProductAndProductItems(ChangeBrandRequest changeBrandRequest) throws Exception {
    Product product = productService.updateBrandOfProduct(changeBrandRequest);
    if (Objects.nonNull(product)) {
      solrReindexPublisherService.publishPDTProductApprovalToSolr(
          VendorProductSolrHelper.toProductUpdateProductToSolrEventModelForBrandUpdate(product));
    }
  }

  @Override
  public void processProductsPermanentDelete(String productCode, String sellerCode) {
    try {
      Product product = productRepository.findByProductCode(productCode);
      if (Objects.isNull(product)) {
        log.info("No such product present as productCode : {}", productCode);
        publishPermanentDeleteResultKafkaEvent(productCode, sellerCode, Constants.SUCCESS);
        return;
      }
      // return if isPickedForDeletion is true and the updated time is within 30 mins
      if (product.isPickedForDeletion() && checkUpdatedDate(product.getUpdatedDate().getTime())) {
        log.info("Ignoring the process of permanent delete of product");
        return;
      }
      product.setPickedForDeletion(true);
      this.productRepository.save(product);
      productsPermanentDeleteService.deleteProducts(productCode, product.getId(), product.getStoreId());
      log.info("Publishing the event : {} , pdtProductSolrDeleteDomainEventModel : {} ",
        kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent(), productCode);
      kafkaProducer.send(kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent(), productCode,
        PDTProductCombinedUpdateToSolrEventModel.builder().productCode(productCode).build());
      publishPermanentDeleteResultKafkaEvent(productCode, sellerCode, Constants.SUCCESS);
    } catch (Exception ex) {
      log.error("Error occurred while processing permanent delete for product code : {} ",
        productCode, ex);
      publishPermanentDeleteResultKafkaEvent(productCode, sellerCode, Constants.FAILED);
    }
  }

  @Override
  @Cacheable(cacheManager = Constants.REDIS_MORE_THAN_ONE_DAT_HIGHER_TTL_MANAGER, value = {
    CacheKeys.C1_CATEGORY_CODE}, key = "#categoryCode", unless = "#result == null")
  public CategoryDTO fetchParentCategoryFromCnCategoryCode(String categoryCode) {
    return productServiceRepository.fetchParentCategoryFromCnCategoryCode(categoryCode);
  }

  @Override
  public void updateDistributionMappingStatusOnChange(ProductChange productChange) {
    if (ranchIntegrationEnabled && distributionSellerList.contains(productChange.getMerchantCode())
      && DistributionStatus.valueOf(productChange.getDistributionMappingStatus()).getCode() > 0) {
      Product updatedProduct = updateProductDistributionStatus(productChange);
      if (Objects.nonNull(updatedProduct)) {
        publishDistributionMappingEvent(updatedProduct);
      }
    }
  }

  @Transactional
  private Product updateProductDistributionStatus(ProductChange productChange) {
    int newDistributionStatus =
      DistributionStatus.valueOf(productChange.getDistributionMappingStatus()).getCode();
    Product product =
      productRepository.findByProductCodeAndMarkForDeleteFalse(productChange.getProductCode());
    if (Objects.nonNull(product) && product.getState().equals(WorkflowState.IN_REVIEW)
      && newDistributionStatus != product.getDistributionMappingStatus()) {
      product.setDistributionMappingStatus(newDistributionStatus);
      return productRepository.save(product);
    }
    return null;
  }

  private void publishDistributionMappingEvent(Product product) {
    PDTProductUpdateProductToSolrEventModel pdtProductUpdateProductToSolrEventModel =
      VendorProductSolrHelper.toProductUpdateProductToSolrEventModelForBrandUpdate(product);
    solrReindexPublisherService.publishPDTProductApprovalToSolr(
      pdtProductUpdateProductToSolrEventModel);
  }

  private boolean checkUpdatedDate(Long updatedDate) {
    long timeDifference = new Date().getTime() - updatedDate;
    long minutesDifference = timeDifference / (60 * 1000);
    return minutesDifference < permanentDeleteUpdateDateDifference;
  }

  private void publishPermanentDeleteResultKafkaEvent(String productCode, String sellerCode,
    String result) {
    PDTPermanentDeleteResultEventModel pdtPermanentDeleteResultEventModel =
      PDTPermanentDeleteResultEventModel.builder().productCode(productCode).sellerCode(sellerCode)
        .service(Constants.SERVICE_NAME).result(result).build();
    log.info("Publishing the event : {} , permanentDeleteProductResult : {} for payload : {}",
      kafkaTopicProperties.getPermanentDeleteProductResult(), productCode,
      pdtPermanentDeleteResultEventModel);
    kafkaProducer.send(kafkaTopicProperties.getPermanentDeleteProductResult(), productCode,
      pdtPermanentDeleteResultEventModel);
  }
}
