package com.gdn.mta.product.service;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.ProductBrandUpdateRequest;
import com.gda.mta.product.dto.response.AgpSimpleQueryResponse;
import com.gda.mta.product.dto.response.HitsResponse;
import com.gda.mta.product.dto.response.UpdatedProductHistoryRequest;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.domain.event.modal.VendorPublishEventModel;
import com.gdn.mta.product.entity.ProductBusinessPartnerCounter;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.enums.ProductType;
import com.gdn.mta.product.service.solr.SolrActiveProductCollectionService;
import com.gdn.mta.product.util.BeanUtils;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.partners.pbp.outbound.AGPQuery.AGPQueryFeign;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutbound;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.AutoNeedRevisionAndForceReviewResponse;
import com.gda.mta.product.dto.BulkMasterProductUpdateResponse;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.EditedResizeAndImagesUpdateStatusResponse;
import com.gda.mta.product.dto.ImageQcHashCodeAndLocationPathRequest;
import com.gda.mta.product.dto.ItemNotesDto;
import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gda.mta.product.dto.NeedRevisionReasonRequest;
import com.gda.mta.product.dto.NeedRevisionSubmitRequest;
import com.gda.mta.product.dto.ProductRevisionInfoResponse;
import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gda.mta.product.dto.RestrictedKeywordsByFieldAndActionType;
import com.gda.mta.product.dto.RetryNeedRevisionRequest;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gda.mta.product.dto.VendorNotesRequest;
import com.gda.mta.product.dto.generator.ProductWfStateResponse;
import com.gda.mta.product.dto.generator.StuckProductResponse;
import com.gda.mta.product.dto.response.AuditTrailListRequest;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gda.mta.product.dto.response.ImageQcEnableAndSyncResponse;
import com.gda.mta.product.dto.response.ImageQcPredictionResponse;
import com.gda.mta.product.dto.response.InternalProductHistoryEventModel;
import com.gda.mta.product.dto.response.RetryAutoNeedRevisionResponse;
import com.gda.mta.product.dto.response.VendorNotesResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.domain.event.modal.TerminatedSellerSkuCleanupStatusEventModel;
import com.gdn.mta.product.commons.constant.TerminatedSellerSkuStatus;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AutoNeedRevisionDomainEvent;
import com.gdn.mta.domain.event.modal.EditedImageResizeEvent;
import com.gdn.mta.domain.event.modal.ImageQcRequestDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcResponseDomainEvent;
import com.gdn.mta.domain.event.modal.ProductActionRetryEvent;
import com.gdn.mta.domain.event.modal.ProductQCRetryEvent;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionDeleteEvent;
import com.gdn.mta.domain.event.modal.StuckProductEventPublishDto;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductFieldHistory;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductImageQcBacklog;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.mta.product.enums.BulkActionType;
import com.gdn.mta.product.enums.ImageQcStatus;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.enums.RestrictedKeywordActionType;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductDistributionTaskRepositoryBean;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.SolrHistoryCollectionRepository;
import com.gdn.mta.product.service.config.GcsProperties;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.ProductWorkflowLookup;
import com.gdn.mta.product.valueobject.BulkMasterProductUpdateRequestDTO;
import com.gdn.mta.product.valueobject.SimpleMasterProductUpdateRequestDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.EditedReviewTypeConstants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEventsEnum;
import com.gdn.partners.pbp.helper.ResponseHelper;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.productPricing.ProductPricingOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.service.notification.ProductNotificationService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.service.productlevel3.ProductItemWholesalePriceService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Helper;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Service;
import com.gdn.partners.pbp.workflow.WorkflowProcessCode;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceBulkUpdateResponse;
import com.gdn.x.businesspartner.dto.BusinessPartnerCodesRequest;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.rest.web.model.request.ItemsSummaryDetailRequest;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AttributeHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;

import lombok.extern.slf4j.Slf4j;

import static com.gdn.mta.domain.event.config.DomainEventName.PRODUCT_INTERNAL_HISTORY_SAVE;

@Service
@Slf4j
public class ProductServiceWrapperBean implements ProductServiceWrapper {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductServiceWrapperBean.class);
  private static final String PRODUCT_COLLECTION_DATA_NOT_FOUND = "Product collection data is not found for: ";
  private static final String PRODUCT_IS_NOT_ACTIVE = "Product collection is not active for: ";
  private static final String REVIEW_PENDING_DISABLE_UPDATE = "Review pending for this product, update is disabled";
  private static final String COLOUR_FAMILY = "Family Colour";
  private static final String REVIEW_CONFIG_CHANGE_DESCRIPTION =
      "Auto screening approval failed, so product is made pre-live";
  private static final String WARNA = "Warna";
  private static final String REVIEW_TYPE_IMAGE = "IMAGE";

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductWfService productWfService;

  @Autowired
  private ProductImageQcProcessingResponseService productImageQcProcessingResponseService;

  @Autowired
  private ProductDistributionTaskRepositoryBean productDistributionTaskRepositoryBean;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Autowired
  private SolrActiveProductCollectionService solrActiveProductCollectionService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Lazy
  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private ProductMailEventService productMailEventService;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductPublisherService productPublisherService;

  @Autowired
  private ImageProcessorService imageProcessorService;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private ProductNotificationService productNotificationService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Autowired
  private ProductLevel1WipService productLevel1WipService;

  @Autowired
  private ApproveProductService approveProductService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private SolrHistoryCollectionRepository solrHistoryCollectionRepository;

  @Autowired
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Autowired
  private ProductItemWholesalePriceService productItemWholesalePriceService;

  @Autowired
  private ProductPricingOutbound productPricingOutbound;

  @Autowired
  private ProductLevel3Helper productLevel3Helper;

  @Autowired
  private NeedCorrectionServiceBean needCorrectionService;

  @Autowired
  private ProductImagePredictionService productImagePredictionService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductImageQcBacklogService productImageQcBacklogService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private GcsProperties gcsProperties;

  @Autowired
  private ProductDistributionService productDistributionService;

  @Lazy
  @Autowired
  private ProductWorkflowServiceWrapper productWorkflowServiceWrapper;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private com.gdn.mta.product.service.ProductLevel3Service productLevel3ServiceV1;

  @Autowired
  private ProductStatusPublisherService productStatusPublisherService;

  @Autowired
  private AGPQueryFeign agpQueryFeign;

  @Autowired
  private TerminatedSellerSkuCleanupServiceBean terminatedSellerSkuCleanupServiceBean;

  @Autowired
  private ProductAppealService productAppealService;

  @Autowired
  private InventoryOutbound inventoryOutbound;

  @Value("${image.resize.batch.time}")
  private String batchTime;

  @Value("${image.resize.retry:3}")
  private int maxRetryCount;

  @Value("${skip.screening.switch}")
  private boolean isSkipScreeningSwitch;

  @Value("${image.source.directory}")
  private String imageSourceDirectory;

  @Value("${product.attribute.autofill.enabled}")
  private boolean productAttributeAutoFillEnabled;

  @Value("${retry.time.span}")
  private int retryTimeSpan;

  @Value("${prevent.edited.event.publish.need.revision.products}")
  private boolean preventEditedEventPublishForNeedRevisionProducts;

  @Value("${publish.auto.need.revision.event}")
  private boolean publishAutoNeedRevisionEvent;

  @Value("${product.states.eligible.to.add.to.vendor}")
  private String productStateEligibleToAddToVendor;

  @Value("${check.restricted.keywords.in.edited.image}")
  private boolean checkRestrictedKeywordsInEditedImage;

  @Value("${skip.definitive.action}")
  private boolean skipDefinitiveAction;

  @Value("${terminated.seller.sku.cleanup.service.name}")
  private String terminatedSellerSkuCleanupServiceName;

  @Value("${terminated.seller.sku.picked.for.deletion.threshold.in.minutes}")
  private long terminatedSellerSkuPickedForDeletionThresholdInMinutes;

  @Value("${price.info.vendor.revised.enabled}")
  private boolean priceInfoVendorRevisedEnabled;

  @Value("${price.info.max.variant.limit}")
  private int priceInfoMaxVariantLimit;

  @Value("${bopis.category.action.on.category.change.switch}")
  private boolean bopisCategoryActionOnCategoryChangeSwitch;

  @Value("${skip.straightforward.rejection.warehouse.stock.validation}")
  private boolean skipStraightforwardRejectionWarehouseStockValidation;

  @Value("${validate.warehouse.deletion.eligible.sellers}")
  private String warehouseMerchantCommissionTypes;

  @Value("${send.product.to.auto.nr.on.brand.and.category.take.down}")
  private boolean sendProductToAutoNROnBrandOrCategoryTakeDown;

  private static final String ASSIGNED_TO_IS_MISSING = "assignTo is required";
  private static final String REASON_OR_NOTES_IS_MISSING = "Reason or notes is required";
  private static final String PRODUCT_STATE_VALIDATION = "Product is not in screening anymore";
  private static final String SOLR_UPDATE_DONE_FOR_PRODUCT_COLLECTION =
      "Product collection update in Solr is successful, product-code :{}";
  public  static final String STATE_IN_PROGRESS = "IN_PROGRESS";

  @Override
  public BulkMasterProductUpdateResponse bulkUpdateActivatedProducts(
      BulkMasterProductUpdateRequestDTO bulkMasterProductUpdateRequestDTO, String storeId) {
    BulkMasterProductUpdateResponse bulkMasterProductUpdateResponse = new BulkMasterProductUpdateResponse();
    for (SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO : bulkMasterProductUpdateRequestDTO
        .getSimpleMasterProductUpdateRequestDTOS()) {
      SimpleMasterProductUpdateResponse response;
      try {
        ProductCollection productCollection = this.productCollectionRepository
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId,
                simpleMasterProductUpdateRequestDTO.getProductCode());
        if (Objects.isNull(productCollection)) {
          throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
              PRODUCT_COLLECTION_DATA_NOT_FOUND + simpleMasterProductUpdateRequestDTO.getProductCode());
        }
        if (!productCollection.isActivated() || !productCollection.isViewable()) {
          throw new ApplicationException(ErrorCategory.VALIDATION,
              PRODUCT_IS_NOT_ACTIVE + simpleMasterProductUpdateRequestDTO.getProductCode());
        }
        // we cannot update brand and name if the product is in vendor
        if (productCollection.isReviewPending()) {
          simpleMasterProductUpdateRequestDTO.setBrand(productCollection.getBrand());
          simpleMasterProductUpdateRequestDTO.setName(productCollection.getProductName());
        }
        Product existingProduct = this.productRepository.findOne(productCollection.getProductId());
        simpleMasterProductUpdateRequestDTO.setPostLive(productCollection.isPostLive());
        response = productService.updateForBulk(simpleMasterProductUpdateRequestDTO, storeId, productCollection);
        if (response.getUpdateSuccess()) {
          Map<Boolean, ProductCollection> productCollectionMap = productService
              .checkAndUpdateSolr(storeId, simpleMasterProductUpdateRequestDTO, existingProduct, productCollection);
          if (productCollectionMap.containsKey(Boolean.TRUE)) {
            productService.updateSolrProductCollection(productCollectionMap.get(Boolean.TRUE));
            LOGGER
                .info(SOLR_UPDATE_DONE_FOR_PRODUCT_COLLECTION, productCollectionMap.get(Boolean.TRUE).getProductCode());
          }
        }
      } catch (Exception e) {
        response = new SimpleMasterProductUpdateResponse.Builder()
            .productCode(simpleMasterProductUpdateRequestDTO.getProductCode()).reasonOfFailure(e.getMessage())
            .updateSuccess(Boolean.FALSE).build();
      }
      bulkMasterProductUpdateResponse.getSimpleMasterProductUpdateResponses().add(response);
    }
    return bulkMasterProductUpdateResponse;
  }

  @Override
  public void doScreeningProductsBulkActions(String storeId, BulkActionType bulkActionType,
      ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest) throws Exception {
    if (BulkActionType.ASSIGN.equals(bulkActionType)) {
      handleBulkAssign(storeId, screeningProductBulkActionsRequest);
    } else if (BulkActionType.UN_ASSIGN.equals(bulkActionType)) {
      handleBulkUnAssign(storeId, screeningProductBulkActionsRequest);
    } else if (BulkActionType.SEND_FOR_REVISION.equals(bulkActionType)) {
      handleBulkSendForCorrection(storeId, screeningProductBulkActionsRequest);
    } else if (BulkActionType.REJECT.equals(bulkActionType)) {
      handleBulkRejection(storeId, screeningProductBulkActionsRequest);
    }
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void handleBulkRejection(String storeId,
      ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest) throws Exception {
    if (StringUtils.isEmpty(screeningProductBulkActionsRequest.getRejectionReason()) || StringUtils
        .isEmpty(screeningProductBulkActionsRequest.getAdditionalNotes())) {
      throw new ApplicationRuntimeException(ErrorCategory.REQUIRED_PARAMETER, REASON_OR_NOTES_IS_MISSING);
    }
    validateStateBeforeRejectionAndRevision(storeId, screeningProductBulkActionsRequest);
    String notes =
        new StringBuilder(screeningProductBulkActionsRequest.getRejectionReason()).append(Constants.DASH_DELIMITER).append(screeningProductBulkActionsRequest.getAdditionalNotes()).toString();
    productWfService.delete(screeningProductBulkActionsRequest.getProductCodes(), notes);
    for (String productCode : screeningProductBulkActionsRequest.getProductCodes()) {
      this.productMailEventService.createAndSaveMailEvent(productCode, notes, ProductMailEventsEnum.REJECTED);
      this.itemService.publishItemStatusEvent(productCode, ProductStatus.REJECTED);
    }
    deleteSolrReviewProducts(storeId, screeningProductBulkActionsRequest.getProductCodes());
  }

  private void validateStateBeforeRejectionAndRevision(String storeId,
      ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest) {
    List<ProductCollection> productCollections = productCollectionRepository
        .findByStoreIdAndProductCodeIn(storeId, screeningProductBulkActionsRequest.getProductCodes());
    for (ProductCollection productCollection : productCollections) {
      if (productCollection.isMarkForDelete() || !WorkflowStates.DRAFT.name().equalsIgnoreCase(productCollection.getState())) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, PRODUCT_STATE_VALIDATION);
      }
    }
  }

  private void handleBulkSendForCorrection(String storeId,
      ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest) throws Exception {
    if (CollectionUtils.isEmpty(screeningProductBulkActionsRequest.getVendorNotes()) && CollectionUtils.isEmpty(
        screeningProductBulkActionsRequest.getImageReason()) && CollectionUtils.isEmpty(
        screeningProductBulkActionsRequest.getCommonImageReason())) {
      throw new ApplicationRuntimeException(ErrorCategory.REQUIRED_PARAMETER, REASON_OR_NOTES_IS_MISSING);
    }
    String notes = generateNeedForCorrectionNotes(screeningProductBulkActionsRequest);
    NeedRevisionNotes revisionNotes = new NeedRevisionNotes();
    BeanUtils.copyProperties(screeningProductBulkActionsRequest, revisionNotes);
    revisionNotes.setAllVariants(Boolean.TRUE.equals(screeningProductBulkActionsRequest.getAllVariants()));
    productWfService.returnForCorrection(screeningProductBulkActionsRequest.getProductCodes(), notes, revisionNotes,
        screeningProductBulkActionsRequest.isScreeningAction());
    deleteSolrReviewProducts(storeId, screeningProductBulkActionsRequest.getProductCodes());
  }

  private String generateNeedForCorrectionNotes(ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest) {
    StringBuilder stringBuilder = new StringBuilder();
    List<String> errorReasons = new ArrayList<>();
    List<String> errorNotes = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(screeningProductBulkActionsRequest.getVendorNotes())) {
      errorReasons.addAll(screeningProductBulkActionsRequest.getVendorNotes());
      errorNotes.add(screeningProductBulkActionsRequest.getContentAdditionalNotes());
    }
    if (CollectionUtils.isNotEmpty(screeningProductBulkActionsRequest.getImageReason())) {
      errorReasons.addAll(screeningProductBulkActionsRequest.getImageReason());
    }
    if (CollectionUtils.isNotEmpty(screeningProductBulkActionsRequest.getCommonImageReason())) {
      errorReasons.addAll(screeningProductBulkActionsRequest.getCommonImageReason());
    }
    if (StringUtils.isNotEmpty(screeningProductBulkActionsRequest.getImagesAdditionalNotes())) {
      errorNotes.add(screeningProductBulkActionsRequest.getImagesAdditionalNotes());
    }
    stringBuilder.append(StringUtils.join(errorReasons, Constants.COMMA)).append(Constants.DASH_DELIMITER)
        .append(StringUtils.join(errorNotes, Constants.COMMA));
    return stringBuilder.toString();
  }

  private void handleBulkUnAssign(String storeId, ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest) {
    productService.assignProducts(storeId, screeningProductBulkActionsRequest.getProductCodes(), SolrConstants.ASSIGNED_TO_PREFIX,
        screeningProductBulkActionsRequest.getAssignedBy());
    updateSolrAssignReviewProducts(storeId, screeningProductBulkActionsRequest.getProductCodes(),
        SolrConstants.ASSIGNED_TO_PREFIX);
    productLevel1HistoryService
        .addProductHistoryForProductAssignment(screeningProductBulkActionsRequest.getProductCodes(),
            screeningProductBulkActionsRequest.getAssignTo(), screeningProductBulkActionsRequest.getAssignedBy());
  }

  private void handleBulkAssign(String storeId, ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest) {
    if (StringUtils.isEmpty(screeningProductBulkActionsRequest.getAssignTo())) {
      throw new ApplicationRuntimeException(ErrorCategory.REQUIRED_PARAMETER, ASSIGNED_TO_IS_MISSING);
    }
    productService.assignProducts(storeId, screeningProductBulkActionsRequest.getProductCodes(),
        screeningProductBulkActionsRequest.getAssignTo(), screeningProductBulkActionsRequest.getAssignedBy());
    updateSolrAssignReviewProducts(storeId, screeningProductBulkActionsRequest.getProductCodes(),
        screeningProductBulkActionsRequest.getAssignTo());
    productLevel1HistoryService
        .addProductHistoryForProductAssignment(screeningProductBulkActionsRequest.getProductCodes(),
            screeningProductBulkActionsRequest.getAssignTo(), screeningProductBulkActionsRequest.getAssignedBy());
  }

  private void updateSolrAssignReviewProducts(String storeId, List<String> productCodes, String assignedTo) {
    for (String productCode : productCodes) {
      String id = productCollectionRepository.findIdByStoreIdAndProductCode(storeId, productCode);
      if (Objects.nonNull(id)) {
        solrReviewProductCollectionService.updateAssignedToInReviewProductCollection(id, assignedTo);
      }
    }
  }

  private void deleteSolrReviewProducts(String storeId, List<String> productCodes) {
    for (String productCode : productCodes) {
      String id = productCollectionRepository.findIdByStoreIdAndProductCode(storeId, productCode);
      if (Objects.nonNull(id)) {
        solrReviewProductCollectionService.deleteProductFromReviewProductCollection(id);
      }
    }
  }

  @Override
  public void update(String storeId, Product product, String notes, boolean marginExceed, String brandCode, boolean postLive,
    String forceReviewNotes, String brandApprovalStatus, boolean onlyVatChanged) throws Exception {
    this.productService.update(product, false, notes, brandCode, brandApprovalStatus, marginExceed, postLive,
      forceReviewNotes, onlyVatChanged);
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCode(storeId, product.getProductCode());
    if (CollectionUtils.isNotEmpty(product.getProductAttributes())) {
      List<ProductAttribute> response = product.getProductAttributes().stream().filter(p -> p.getAttribute().isSkuValue())
          .filter(p -> AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(p.getAttribute().getAttributeType())).collect(Collectors.toList());
      if (CollectionUtils.isNotEmpty(response)) {
        productBusinessPartnerService
            .updateSkuValueTrueInProductBusinessPartnerAttribute(response, productCollection.getProductId());
      }
    }
    if (!productCollection.isImageResized()) {
      productPublisherService
          .publishProductImageResizeEvent(productCollection.getProductCode(), productCollection.getStoreId());
    } else if (!productCollection.isActivated() && !productCollection.isViewable() && !WorkflowStates.NEED_CORRECTION
        .getValue().equals(productCollection.getState()) && !productCollection.isMarkForDelete()) {
      this.solrReviewProductCollectionService.addProductToReviewProductCollection(productCollection);
    }
    if (marginExceed) {
      log.debug("Margin exceed for product : {}", product.getProductCode());
      productBusinessPartnerService.markItemsAsUnBuyableAndUnViewable(productCollection.getProductId());
    }
  }

  @Override
  public void delete(String storeId, String productId, String productCode) throws Exception {
    this.productService.delete(productId);
    String id = this.productCollectionRepository.findIdByStoreIdAndProductCode(storeId, productCode);
    this.solrReviewProductCollectionService.deleteProductFromReviewProductCollection(id);
  }

  @Override
  public void updateProductAssignmentStatus(String storeId, String productCode, String assignedTo, String assignedBy) {
    assignedTo = StringUtils.isEmpty(assignedTo) ? SolrConstants.ASSIGNED_TO_PREFIX : assignedTo;
    this.productService.assignProducts(storeId, Collections.singletonList(productCode), assignedTo, assignedBy);
    String id = this.productCollectionRepository.findIdByStoreIdAndProductCode(storeId, productCode);
    this.solrReviewProductCollectionService.updateAssignedToInReviewProductCollection(id, assignedTo);
    productLevel1HistoryService
        .addProductHistoryForProductAssignment(Collections.singletonList(productCode), assignedTo, assignedBy);
  }

  @Override
  public List<ProductRevisionInfoResponse> getProductRevisionInfo(String storeId, String productCode) {
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    List<ProductRevisionInfoResponse> productRevisionInfoResponseList = new ArrayList<>();
    if (Objects.nonNull(productCollection)) {
      List<ProductHistory> productHistoryList =
          productLevel1HistoryService.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId, productCollection.getProductId());
      productHistoryList.stream().filter(ProductServiceWrapperBean::isRevisionHistory)
          .sorted(Comparator.comparing(ProductHistory::getCreatedDate).reversed())
          .map(ConverterUtil::toProductRevisionInfoResponse)
          .collect(Collectors.toCollection(() -> productRevisionInfoResponseList));
    }
    return productRevisionInfoResponseList;
  }

  @Override
  public String getScreeningNotes(String storeId, String productCode) {
    return productCollectionRepository.getReviewerNotesByStoreIdAndProductCode(storeId, productCode);
  }

  @Override
  public void resizeImages(String storeId, String productCode, boolean deltaIndex, int page, int size)
      throws Exception {
    if (!deltaIndex) {
      if (StringUtils.isNotBlank(productCode)) {
        imageProcessorService.resizeImage(storeId, productCode, 0);
      } else {
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT, "Product code must not be blank");
      }
    } else {
      Pageable pageable = PageRequest.of(page, size);
      Page<ProductCollection> productCollections =
          productCollectionRepository.findByStoreIdAndStateInAndViewableAndImageResizedAndImageResizeRetryLessThanAndMarkForDeleteFalseAndUpdatedDateLessThanOrderByUpdatedDateDesc(
              storeId, Arrays.asList(WorkflowStates.DRAFT.getValue(), Constants.IN_PROGRESS_STATE), false, false,
              maxRetryCount, Date.from(
                  Instant.now().minus(Duration.ofMinutes(Integer.parseInt(batchTime))).truncatedTo(ChronoUnit.MINUTES)),
              pageable);
      Map<String, Boolean> merchantCodeProfileResponseMap =
        businessPartnerRepository.filterDetailsByBusinessPartnerCodeList(
          new BusinessPartnerCodesRequest(
            productCollections.getContent().stream().map(ProductCollection::getBusinessPartnerCode)
              .distinct().collect(Collectors.toList()))).stream().collect(
          Collectors.toMap(ProfileResponse::getBusinessPartnerCode,
            ProfileResponse::isTrustedSeller, (a, b) -> a));
      for (ProductCollection productCollection : productCollections.getContent()) {
        boolean publishEvent = false;
        try {
          productCollection.setImageResizeRetry(productCollection.getImageResizeRetry() + 1);
          publishEvent = imageProcessorService.resizeImage(storeId, productCollection.getProductCode(), 0);
          if (publishEvent) {
            productCollection.setImageResized(true);
          }
        } catch (Exception e) {
          LOGGER.error("Image resize failed for productCode: {} ", productCollection.getProductCode(), e);
        }
        productService.saveProductCollection(productCollection);
        if (publishEvent) {
          log.info("Corrected image resize flag and published screening approval flag for productCode : {} ",
              productCollection.getProductCode());
          this.productPublisherService.publish(productCollection.getProductCode(), productCollection.getBusinessPartnerCode(),
              productCollection.getBusinessPartnerName(), GdnMandatoryRequestParameterUtil.getUsername(),
              productCollection.isPostLive(), productCollection.isRestrictedKeywordsPresent(),
              productCollection.getRestrictedKeywordsDetected(),productCollection.getPrioritySeller(),
            merchantCodeProfileResponseMap.getOrDefault(productCollection.getBusinessPartnerCode(),false),
              productCollection.getProductId(), productCollection);
        }
      }
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void changeBrandCodeAndBrandApprovalStatusInScreeningProducts(String brandCode, String brandRequestCode,
      String brandApprovalStatus, String brandName) {
    Pageable pageable = PageRequest.of(0, 100);
    Page<ProductCollection> productCollectionPage;
    do {
      productCollectionPage = this.productCollectionRepository
          .findByBrandCodeAndBrandApprovalStatusAndMarkForDeleteFalse(brandRequestCode, BrandApprovalStatus.DRAFT,
              pageable);
      if (Objects.nonNull(productCollectionPage) && CollectionUtils.isNotEmpty(productCollectionPage.getContent())) {
        if (BrandApprovalStatus.APPROVED.name().equals(brandApprovalStatus)) {
          updateBrandCodeAndStatusInDbForApprovedBrands(brandCode, brandName, productCollectionPage.getContent());
        } else if (BrandApprovalStatus.REJECTED.name().equals(brandApprovalStatus)) {
          updateBrandApprovalStatusInDbForRejectedBrand(
              productCollectionPage.getContent().stream().map(ProductCollection::getProductCode)
                  .collect(Collectors.toList()));
        }
      }
      List<String> productCodes =
          productCollectionPage.getContent().stream().map(ProductCollection::getProductCode)
              .collect(Collectors.toList());
      try {
        productOutbound.clearProductCacheByProductCodes(productCodes);
      } catch (Exception e) {
        log.error("Error in clearing product cache for products : {}", productCodes, e);
      }
      pageable = productCollectionPage.nextPageable();
    } while (productCollectionPage.hasNext());
  }

  @Override
  public void updateImagePathsAndSkipScreeningForPostLiveProducts(BulkImageProcessResponse bulkImageProcessResponse)
      throws Exception {
    ProductDetailResponse productDetailResponse =
        productService.findProductDetailByProductCode(bulkImageProcessResponse.getGroupCode(), false);
    if (Objects.nonNull(productDetailResponse)) {
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
          new RestrictedKeywordsByFieldAndActionType();
      ProductCollection productCollection = productCollectionRepository
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(bulkImageProcessResponse.getStoreId(),
              bulkImageProcessResponse.getGroupCode());
      ProfileResponse profileResponse =
        businessPartnerRepository.filterDetailByBusinessPartnerCode(productCollection.getBusinessPartnerCode());
      if (!productCollection.isImageResized()) {
      ProductAndItemImageRequest productAndItemImageRequest =
          ConverterUtil.toProductAndItemImageRequest(bulkImageProcessResponse, productDetailResponse);
      ImageQcEnableAndSyncResponse imageQcEnableAndSyncResponse = new ImageQcEnableAndSyncResponse();
      if (!Constants.INTERNAL.equals(productCollection.getBusinessPartnerCode())) {
        imageQcEnableAndSyncResponse = productService
            .getImageQcStatus(bulkImageProcessResponse.getStoreId(), productCollection.getProductCode(),
                productDetailResponse.getCategoryCodes().get(0));
      }
      productService.updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequest, productCollection,
          !imageQcEnableAndSyncResponse.isSync());
      autoFillProductAttributeValue(bulkImageProcessResponse.getStoreId(), productCollection.getProductId(),
          bulkImageProcessResponse.getGroupCode());
      if (imageQcEnableAndSyncResponse.isEnable()) {
        productCollection.setImageQcState(1);
      }
      log.info(
          "Product-workflow-tracker Image qc status for productCode : {} , Image qc enable : {}, image qc sync : {}",
          productDetailResponse.getProductCode(), imageQcEnableAndSyncResponse.isEnable(),
          imageQcEnableAndSyncResponse.isSync());
      boolean isProductCollectionSaveRequired = true;
      if (!imageQcEnableAndSyncResponse.isSync()) {
        restrictedKeywordsByFieldAndActionType =
            productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse,
                productCollection.getCategoryCode());
        List<RestrictedKeywordsByField> restrictedKeywordsByFieldList =
            restrictedKeywordsByFieldAndActionType.getRestrictedKeywordsByFieldList();
        productCollection.setRestrictedKeywordsPresent(!CollectionUtils.isEmpty(restrictedKeywordsByFieldList));
        boolean isAutoCategoryChange = restrictedKeywordsByFieldAndActionType.getAction()
            == RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType();
        productCollection
            .setRestrictedKeywordsDetected(objectMapper.writeValueAsString(restrictedKeywordsByFieldList));
        if (isEligibleForScreeningSkip(productDetailResponse, productCollection) && skipRestrictedKeywordsCheck(
            restrictedKeywordsByFieldAndActionType, restrictedKeywordsByFieldList)) {
          skipScreeningForProduct(productCollection);
        } else if (STATE_IN_PROGRESS.equalsIgnoreCase(productCollection.getState()) && skipRestrictedKeywordsCheck(
            restrictedKeywordsByFieldAndActionType, restrictedKeywordsByFieldList)) {
          productCollectionRepository.saveAndFlush(productCollection);
          productService.updateSolrOrPublishEvent(productCollection,
            Optional.ofNullable(profileResponse).map(ProfileResponse::isTrustedSeller).orElse(false));
          isProductCollectionSaveRequired = false;
        } else {
          if (isAutoCategoryChange && !Constants.INTERNAL.equals(productCollection.getBusinessPartnerCode())) {
            try {
              productCollection.setRestrictedKeywordsDetected(StringUtils.EMPTY);
              productCollection = categoryChangeInPbpPcbAndHistoryInCreationFlow(restrictedKeywordsByFieldAndActionType,
                  productCollection, bulkImageProcessResponse.getStoreId(), StringUtils.EMPTY, false, profileResponse);
            } catch (ApplicationRuntimeException e) {
              log.error("Error while caching category for product : {} ", productCollection.getProductCode(), e);
              if (e.getErrorMessage().contains(ErrorMessages.ERROR_WHEN_UPDATING_DATA_IN_PCB)) {
                productCollection
                    .setRestrictedKeywordsDetected(objectMapper.writeValueAsString(restrictedKeywordsByFieldList));
                sendProductForPreliveReview(productCollection, profileResponse, restrictedKeywordsByFieldList,
                    restrictedKeywordsByFieldAndActionType);
              } else {
                throw e;
              }
            }
          } else {
            sendProductForPreliveReview(productCollection, profileResponse, restrictedKeywordsByFieldList,
                restrictedKeywordsByFieldAndActionType);
          }
          isProductCollectionSaveRequired = false;
        }
      }
      if (isProductCollectionSaveRequired) {
        productCollectionRepository.saveAndFlush(productCollection);
      }
      productService.publishProductStatusEvent(productDetailResponse, productCollection, ProductStatus.CREATED,
          StringUtils.EMPTY);
      if (imageQcEnableAndSyncResponse.isEnable()) {
        ImageQcRequestDomainEvent event =
            productService.publishImageQcEvent(bulkImageProcessResponse, productDetailResponse,
                restrictedKeywordsByFieldAndActionType);
        LOGGER.debug("Published com.gdn.image.qc.prediction.request event for productCode : {}, event : {}",
            productDetailResponse.getProductCode(), event);
      }
      if (!Constants.INTERNAL.equals(productCollection.getBusinessPartnerCode())) {
        performResultantActionBasedOnRestrictedKeywords(bulkImageProcessResponse.getStoreId(),
            productDetailResponse.getProductCode(), restrictedKeywordsByFieldAndActionType.getAction(),
            restrictedKeywordsByFieldAndActionType.getCategoryRestrictedKeywordId(), true, false,
            productCollection.isAutoNeedRevision(),
            new ArrayList<>(getVendorErrorFields(restrictedKeywordsByFieldAndActionType)), true,
          productCollection.getBusinessPartnerCode(), profileResponse, null, null);
      }
    }
    }
  }

  private static boolean skipRestrictedKeywordsCheck(RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType,
      List<RestrictedKeywordsByField> restrictedKeywordsByFieldList) {
    return CollectionUtils.isEmpty(restrictedKeywordsByFieldList)
        || restrictedKeywordsByFieldAndActionType.isSkipAllActions();
  }

  private void sendProductForPreliveReview(ProductCollection productCollection, ProfileResponse profileResponse,
      List<RestrictedKeywordsByField> restrictedKeywordsByFieldList,
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType) throws Exception {
    sendNotificationForRestrictedPostLiveProducts(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList),
        productCollection, profileResponse);
    productCollection.setPostLive(
        Optional.ofNullable(profileResponse).map(ProfileResponse::isTrustedSeller).orElse(false) ?
            productCollection.isPostLive() :
            false);
    productCollectionRepository.saveAndFlush(productCollection);
    if (Constants.INTERNAL.equals(productCollection.getBusinessPartnerCode())
        || publishEventToPDTIfProductNotEligibleForAutoNeedRevisionOrReject(
        restrictedKeywordsByFieldAndActionType.getAction()) || Optional.ofNullable(profileResponse)
        .map(ProfileResponse::isTrustedSeller).orElse(false)) {
      // Publish screening approval event only if product is not eligible for auto reject / need revision
      // or If it's INTERNAL PRODUCT
      productService.updateSolrOrPublishEvent(productCollection,
          Optional.ofNullable(profileResponse).map(ProfileResponse::isTrustedSeller).orElse(false));
    }
  }

  private void autoFillProductAttributeValue(String storeId, String productId, String productCode) {
    if (productAttributeAutoFillEnabled) {
      try {
        List<AttributeHistoryResponse> attributeHistoryResponseList =
            productRepository.autoFillProductAttribute(storeId, productCode);
        if (CollectionUtils.isNotEmpty(attributeHistoryResponseList)) {
          productService.updateHistoryOnAttributeAutoFill(storeId, productId, attributeHistoryResponseList);
        }
      } catch (Exception e) {
        log.error("Error while auto filling product attribute. productCode : {} ", productCode, e);
      }
    }
  }

  @Override
  public ProductCollection categoryChangeInPbpPcbAndHistoryInCreationFlow(
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType,
      ProductCollection productCollection, String storeId, String destinationCategoryCode, boolean updateSalesCategory,
      ProfileResponse profileResponse)
      throws Exception {
    if (StringUtils.isEmpty(destinationCategoryCode)) {
      LOGGER.info("Getting CategoryRestrictedKeywordResponse from pcb by id : {}",
          restrictedKeywordsByFieldAndActionType.getCategoryRestrictedKeywordId());
      CategoryRestrictedKeywordResponse messageById =
          getMessageById(restrictedKeywordsByFieldAndActionType.getCategoryRestrictedKeywordId());
      destinationCategoryCode = messageById.getDestinationCategory();
    }
    if (destinationCategoryCode.equals(productCollection.getCategoryCode())) {
      productCollection = productCollectionRepository.save(productCollection);
      productService.updateSolrOrPublishEvent(productCollection,
          Optional.ofNullable(profileResponse).map(ProfileResponse::isTrustedSeller).orElse(false));
    } else {
      LOGGER.info("Updating the new Category in pcb and pbp for productCode : {} and with destinationCategory : {}",
          productCollection.getProductCode(), destinationCategoryCode);
      productCollection = updateProductCategoryForActionCategoryChange(storeId, productCollection.getProductCode(),
          destinationCategoryCode, updateSalesCategory, restrictedKeywordsByFieldAndActionType.getKeyword());
      LOGGER.info("Updating Category in prd_product_business_partner in pbp for productId : {} with productCode : {}",
          productCollection.getProductId(), productCollection.getProductCode());
      updateProductBusinessPartnerInformation(storeId, productCollection);
      productService.updateSolrOrPublishEvent(productCollection,
          Optional.ofNullable(profileResponse).map(ProfileResponse::isTrustedSeller).orElse(false));
    }
    return productCollection;
  }

  private void updateProductBusinessPartnerInformation(String storeId, ProductCollection productCollection)
      throws Exception {
    List<ProductBusinessPartner> productBusinessPartners =
      this.productBusinessPartnerService.findByStoreIdAndProductId(storeId,
        productCollection.getProductId());
    for (ProductBusinessPartner productBusinessPartner : productBusinessPartners) {
      productBusinessPartner.setCategoryName(productCollection.getCategoryName());
      productBusinessPartner.setCategoryCode(productCollection.getCategoryCode());
      productBusinessPartner.setUpdatedDate(Calendar.getInstance().getTime());
      this.productBusinessPartnerService.saveProductBusinessPartner(productBusinessPartner);
    }
    updateProductItemBusinessPartnerOnAutoCategoryChange(productBusinessPartners);
  }

  private void updateProductItemBusinessPartnerOnAutoCategoryChange(
    List<ProductBusinessPartner> productBusinessPartners) {
    List<ProductItemBusinessPartner> productItemBusinessPartners =
      productBusinessPartners.stream().map(ProductBusinessPartner::getProductItemBusinessPartners)
        .flatMap(List::stream).collect(Collectors.toList());
    boolean bopisProduct =
      productItemBusinessPartners.stream().map(ProductItemBusinessPartner::getProductType)
        .allMatch(productType -> ProductType.BOPIS.getProductType().equals(productType));
    if (bopisProduct && bopisCategoryActionOnCategoryChangeSwitch) {
      CategoryDetailResponse categoryDetails = productOutbound.getCategoryDetailByCategoryCode(
        productBusinessPartners.stream().map(ProductBusinessPartner::getCategoryCode)
          .filter(StringUtils::isNotBlank).findFirst().get());
      if (Boolean.FALSE.equals(categoryDetails.isBopisEligible())) {
        CommonUtils.setProductItemBusinessPartnerData(productItemBusinessPartners);
      }
    }
    productItemBusinessPartnerService.saveAll(productItemBusinessPartners);
  }

  private Set<String> getVendorErrorFields(RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType) {
    return restrictedKeywordsByFieldAndActionType.getRestrictedKeywordsByFieldList().stream()
            .map(RestrictedKeywordsByField::getFieldIdentifier).collect(Collectors.toSet());
  }

  private boolean publishEventToPDTIfProductNotEligibleForAutoNeedRevisionOrReject(int action) {
    return RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType() != action
        && RestrictedKeywordActionType.AUTO_NEED_REVISION.getRestrictedKeywordActionType() != action;
  }

  public boolean isWarehouseStockPresent(Set<String> itemCodes) throws Exception {
    for(String itemCode : itemCodes) {
      MandatoryRequestParam mandatoryRequestParam =
          MandatoryRequestParam
              .generateMandatoryRequestParam(GdnMandatoryRequestParameterUtil.getStoreId(),
                  GdnMandatoryRequestParameterUtil.getChannelId(),
                  GdnMandatoryRequestParameterUtil.getClientId(),
                  GdnMandatoryRequestParameterUtil.getRequestId());
      mandatoryRequestParam.setUsername(GdnMandatoryRequestParameterUtil.getUsername());
      try {
        if (inventoryOutbound.isWarehouseStockPresent(mandatoryRequestParam, itemCode)) {
          return true;
        }
      } catch (ApplicationRuntimeException ex) {
        log.error(
            "Failed to validate warehouse stock from Inventory and hence allowing rejection for "
                + "the item:{}",
            itemCode);
        return false;
      }
    }
    return false;
  }

  @Override
  public void performResultantActionBasedOnRestrictedKeywords(String storeId, String productCode, int action,
      String categoryRestrictedKeywordId, boolean screeningAction, boolean validateDraftState,
      boolean currentlyAutoNeedRevisionState, List<String> vendorErrorFields, boolean overrideDataFromPDT,
    String businessPartnerCode, ProfileResponse profileResponse, Set<String> itemCodes,
      EditProductResponse editResponse)
      throws Exception {
    if (Objects.isNull(profileResponse)) {
      profileResponse =
        Optional.ofNullable(businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode))
        .orElse(new ProfileResponse());
    }
    if (!profileResponse.isTrustedSeller()) {
      if (RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType() == action) {
        String commissionType =
            Optional.of(profileResponse).map(ProfileResponse::getCompany)
                .map(CompanyDTO::getMerchantType).orElse(StringUtils.EMPTY);
        if (!validateWarehouseStockAvailabilityAndPublishHistoryIfRejectionSkipped(commissionType,
            itemCodes, productCode, editResponse)) {
          MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
              Constants.SYSTEM + Constants.HYPHEN + Constants.AUTO_REJECT);
          productWorkflowServiceWrapper.deleteProductCollection(storeId, productCode,
              getMessageById(categoryRestrictedKeywordId).getMessage(), true, true);
        }
      } else if (RestrictedKeywordActionType.AUTO_NEED_REVISION.getRestrictedKeywordActionType() == action
        && !currentlyAutoNeedRevisionState) {
        AutoNeedRevisionDomainEvent autoNeedRevisionDomainEvent = new AutoNeedRevisionDomainEvent();
        autoNeedRevisionDomainEvent.setStoreId(storeId);
        autoNeedRevisionDomainEvent.setProductCode(productCode);
        autoNeedRevisionDomainEvent.setNotes(getMessageById(categoryRestrictedKeywordId).getMessage());
        autoNeedRevisionDomainEvent.setVendorErrorFields(vendorErrorFields);
        autoNeedRevisionProduct(autoNeedRevisionDomainEvent, true, screeningAction,
          validateDraftState, overrideDataFromPDT, false);
      }
    }
  }

  private boolean validateWarehouseStockAvailabilityAndPublishHistoryIfRejectionSkipped(
      String commissionType, Set<String> itemCodes, String productCode, EditProductResponse editResponse) throws Exception {
    if (skipStraightforwardRejectionWarehouseStockValidation && Set.of(
        warehouseMerchantCommissionTypes.split(Constants.COMMA)).contains(commissionType)
        && Objects.nonNull(itemCodes) && isWarehouseStockPresent(itemCodes)) {
      publishHistoryForRestrictingStraightforwardRejectionForWarehouseStockAvailability(
          productCode);
      if (Objects.nonNull(editResponse)) {
        editResponse.setReviewType(editResponse.isPostLive() ? Constants.POST_LIVE_REVIEW_TYPE :
            editResponse.getReviewType());
      }
      return true;
    }
    return false;
  }

  private void publishHistoryForRestrictingStraightforwardRejectionForWarehouseStockAvailability(
      String productCode) {
    log.info(
        "Restricting rejection of the product, with productCode:{}, since there is "
            + "warehouse stock available",
        productCode);
    InternalProductHistoryEventModel internalProductHistoryEventModel =
        ConverterUtil.generateInternalProductHistoryEventModel(productCode,
            Constants.SKIP_REJECTION, Constants.SYSTEM,
            Constants.SKIP_REJECTION_DESCRIPTION);
    log.info("Publishing {} event for capturing history for productCode {} with message {} ",
        PRODUCT_INTERNAL_HISTORY_SAVE, productCode, internalProductHistoryEventModel);
    kafkaProducer.send(PRODUCT_INTERNAL_HISTORY_SAVE, productCode,
        internalProductHistoryEventModel);
  }

  private CategoryRestrictedKeywordResponse getMessageById(String categoryRestrictedKeywordId) throws Exception {
    return productRepository.getCategoryRestrictedKeywordDetail(categoryRestrictedKeywordId);
  }

  @Override
  public void skipScreeningForSkipReviewProduct(String productCode, ProfileResponse profileResponse) throws Exception {
    log.info("Skipping screening process for product :{}", productCode);
    ProductCollection productCollection = this.productLevel1WipService.approveDraft(productCode, profileResponse);
    this.productLevel1HistoryService.create(productCollection.getProductCode(), "APPROVE_DRAFT", null);
    ProductDetailResponse productDetailResponse = productService.findProductDetailByProductCode(productCode, false);
    approveProductService.processImage(productDetailResponse);
    productService
        .publishProductStatusEvent(productDetailResponse, productCollection, ProductStatus.CREATED, StringUtils.EMPTY);
  }

  @Override
  public void retrySkipReviewProductActivation(String productCode) throws Exception {
    log.info("Retry product activation for product :{}", productCode);
    ProductDetailResponse productDetailResponse = productService.findProductDetailByProductCode(productCode, false);
    approveProductService.processImage(productDetailResponse);
  }

  private void processProductAfterReceivingImageQcEvent(ProductDetailResponse productDetailResponse,
      ProductCollection productCollection, boolean imageViolation, ProfileResponse profileResponse) throws Exception {
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList = productLevel3Helper
        .getRestrictedKeywordsInProductDetails(productDetailResponse, productCollection.getCategoryCode());
    productCollection.setRestrictedKeywordsPresent(!CollectionUtils.isEmpty(restrictedKeywordsByFieldList));
    productCollection.setRestrictedKeywordsDetected(objectMapper.writeValueAsString(restrictedKeywordsByFieldList));
    if (isEligibleForScreeningSkip(productDetailResponse, productCollection) && CollectionUtils
        .isEmpty(restrictedKeywordsByFieldList) && !imageViolation) {
      skipScreeningForProduct(productCollection);
    } else {
      sendNotificationForRestrictedPostLiveProducts(
          CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList) || !imageViolation,
        productCollection, profileResponse);
      // Set PostLive True for Trusted Sellers With Restricted Keyword
      productService.updatePostLiveStatus(productCollection,
        Optional.ofNullable(profileResponse).map(ProfileResponse::isTrustedSeller).orElse(false));
      productService.updateSolrOrPublishEvent(productCollection,
        Optional.ofNullable(profileResponse).map(ProfileResponse::isTrustedSeller).orElse(false));
    }
  }

  private void sendNotificationForRestrictedPostLiveProducts(boolean isKeywordPresent,
      ProductCollection productCollection, ProfileResponse profileResponse) throws Exception {
    if (isKeywordPresent && productCollection.isPostLive()) {
      LOGGER.info("Sending BliBli standardization notification to business partner :{} for product : {}",
          productCollection.getBusinessPartnerCode(), productCollection.getProductCode());
      productNotificationService
          .sendNotificationForProductWithRestrictedKeyword(profileResponse.getBusinessPartnerCode(),
              productCollection.getProductName(), profileResponse.getCompany().isInternationalFlag());
    }
  }

  @Override
  public void updatePostLiveFlagAndSkipScreening(ProductCollection productCollection) throws Exception {
    productCollection = productService.updatePostLiveStatus(productCollection, true);
    boolean response = skipScreeningForProduct(productCollection);
    if (response) {
      this.solrReviewProductCollectionService.deleteProductFromReviewProductCollection(productCollection.getId());
    }
  }

  private boolean skipScreeningForProduct(ProductCollection productCollection) throws Exception {
    try {
      productWfService.approveDraft(productCollection.getProductCode());
      return true;
    } catch (Exception e) {
      // On exception in approving product , product will be made pre-live
      log.error("Exception caught while approving product on auto approval of screening, productCode:{}",
          productCollection.getProductCode(), e);
      productCollection = productService.updatePostLiveStatus(productCollection, false);
      productService.updateSolrOrPublishEvent(productCollection, false);
      productLevel1HistoryService.saveProductHistory(productCollection.getProductCode(), Constants.DEFAULT_USERNAME,
          SaveHistoryConstants.REVIEW_CONFIG_CHANGE, REVIEW_CONFIG_CHANGE_DESCRIPTION);
    }
    return false;
  }

  @Override
  public void updateImagePathsAndFlagOnResizingImageFailure(BulkImageProcessResponse bulkImageProcessResponse)
      throws Exception {
    productService.updateImagePathsAndFlagOnResizingImageFailure(bulkImageProcessResponse);
  }

  @Override
  public boolean checkIfProductIsEligibleForScreeningSkip(ProductCollection productCollection) throws Exception {
    if (!productCollection.isImageResized()) {
      return false;
    }
    if (!StringUtils.equalsIgnoreCase(Constants.NOT_APPLICABLE, productCollection.getAssignedTo())) {
      return false;
    }
    ProductDetailResponse productDetailResponse =
        productService.findProductDetailByProductCode(productCollection.getProductCode(), false);
    return CollectionUtils.isEmpty(productLevel3Helper
        .getRestrictedKeywordsInProductDetails(productDetailResponse, productCollection.getCategoryCode()));
  }

  private boolean isEligibleForScreeningSkip(ProductDetailResponse productDetailResponse,
      ProductCollection productCollection) {
    return productCollection.isPostLive() && WorkflowStates.DRAFT.getValue()
        .equalsIgnoreCase(productCollection.getState());
  }

  private boolean isFamilyColourSet(ProductDetailResponse productDetailResponse) {
    if (productDetailResponse.getProductAttributeResponses().stream().anyMatch(
        productAttributeResponse -> WARNA.equalsIgnoreCase(productAttributeResponse.getProductAttributeName()))) {
      return productDetailResponse.getProductItemResponses().stream()
          .allMatch(productItemResponse -> isFamilyColourSetAtItem(productItemResponse));
    }
    return true;
  }

  private boolean isFamilyColourSetAtItem(ProductItemResponse productItemResponse) {
    if (CollectionUtils.isNotEmpty(productItemResponse.getProductItemAttributeValueResponses())) {
      return productItemResponse.getProductItemAttributeValueResponses().stream().filter(
          productItemAttributeValueResponse -> COLOUR_FAMILY
              .equalsIgnoreCase(productItemAttributeValueResponse.getAttributeResponse().getName())).anyMatch(
          productItemAttributeValueResponse -> StringUtils.isNotBlank(productItemAttributeValueResponse.getValue()));
    }
    return false;
  }

  private void updateBrandCodeAndStatusInDbForApprovedBrands(String brandCode, String brandName,
      List<ProductCollection> productCollectionList) {
    for (ProductCollection productCollection : productCollectionList) {
      this.productCollectionRepository
          .updateBrandCodeAndBrandApprovalStatus(brandCode, BrandApprovalStatus.APPROVED, brandName,
              productCollection.getProductCode());
      this.solrReviewProductCollectionService
          .updateBrandApprovedInReviewProductCollection(productCollection.getId(), Boolean.TRUE, brandName);
    }
  }

  private void updateBrandApprovalStatusInDbForRejectedBrand(List<String> productCollectionList) {
    for (String id : productCollectionList) {
      this.productCollectionRepository.updateBrandApprovalStatus(id, BrandApprovalStatus.REJECTED);
    }
  }

  private static boolean isRevisionHistory(ProductHistory productHistory) {
    return WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc().equals(productHistory.getDescription());
  }

  @Override
  public ProductCollection updateProductCategory(String storeId, String productCode, String categoryCode,
      boolean updateSalesCategory) throws Exception {
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (Objects.isNull(productCollection) || !productCollection.isActivated() || !productCollection.isViewable()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.PRODUCT_NOT_FOUND);
    }
    if (productCollection.isReviewPending()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.PRODUCT_NOT_REVIEWED);
    }
    CategorySummaryResponse response =
        productOutbound.updateProductCategory(productCode, categoryCode, updateSalesCategory,
            isb2bSellerOrNot(productCollection.getBusinessPartnerCode()));
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, ErrorMessages.ERROR_WHEN_UPDATING_DATA_IN_PCB);
    }
    return updateProductCategoryAndHistory(productCollection,response);
  }

  private ProductCollection updateProductCategoryForActionCategoryChange(String storeId, String productCode,
      String destinationCategory, boolean updateSalesCategory, String keyword) throws Exception {
    ProductCollection productCollection = findProductCollectionByProductCode(storeId, productCode);
    CategorySummaryResponse response = updateCategoryInPcb(productCode, destinationCategory, updateSalesCategory,
        productCollection.getBusinessPartnerCode());
    return updateProductCategoryAndHistoryForCategoryChange(productCollection, response, keyword);
  }

  @Override
  public CategorySummaryResponse updateCategoryInPcb(String productCode, String categoryCode,
    boolean updateSalesCategory, String businessPartnerCode) throws Exception {
    CategorySummaryResponse response =
        productOutbound.updateProductCategory(productCode, categoryCode, updateSalesCategory,
            isb2bSellerOrNot(businessPartnerCode));
    if (Objects.isNull(response)) {
      LOGGER.error("Error when updating product category for productCode : {} , errorCategory : {} , errorMessage : {}",
          productCode, ErrorCategory.UNSPECIFIED, ErrorMessages.ERROR_WHEN_UPDATING_DATA_IN_PCB);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, ErrorMessages.ERROR_WHEN_UPDATING_DATA_IN_PCB);
    }
    return response;
  }

  private boolean isb2bSellerOrNot(String businessPartnercode) throws Exception {
    ProfileResponse profileResponse = productService.getProfileResponse(businessPartnercode);
    List<String> salesChannel = CommonUtils.salesChannelFromProfileResponse(profileResponse);
    return !(salesChannel.size() == 1 && salesChannel.get(0).equals(Constants.B2C_SELLER_CHANNEL));
  }

  private ProductCollection updateProductCategoryAndHistoryForCategoryChange(ProductCollection productCollection,
      CategorySummaryResponse response, String keyword) {
    String oldCategoryName = productCollection.getCategoryName();
    ConverterUtil.setProductCollectionDetailsForCategoryChange(productCollection,response);
    productCollectionRepository.save(productCollection);
    productService.updateSolrProductCollection(productCollection);
    InternalProductHistoryEventModel internalProductHistoryEventModel =
        ConverterUtil.createInternalProductHistoryEventModel(productCollection, oldCategoryName, keyword);
    kafkaProducer.send(DomainEventName.PRODUCT_INTERNAL_HISTORY_SAVE, productCollection.getProductCode(),
        internalProductHistoryEventModel);
    List<ProductBusinessPartner> productBusinessPartnerList = productBusinessPartnerService
        .findByStoreIdAndProductId(Constants.DEFAULT_STORE_ID, productCollection.getProductId());
    AuditTrailListRequest auditTrailListRequest = CommonUtils
        .getAuditTrailRequestForL3History(productCollection.getBusinessPartnerCode(),
            productBusinessPartnerList.get(0).getGdnProductSku(), productCollection.getProductName(),
            UpdateProductActivity.AUTO_CATEGORY_CHANGE.name(), StringUtils.EMPTY, oldCategoryName,
            productCollection.getCategoryName(), SaveHistoryConstants.AUTO_CATEGORY_CHANGE,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT, Constants.HYPHEN);
    kafkaProducer
        .send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, productBusinessPartnerList.get(0).getGdnProductSku(),
            auditTrailListRequest);
    return productCollection;
  }

  private ProductCollection findProductCollectionByProductCode(String storeId, String productCode) {
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (Objects.isNull(productCollection)) {
      LOGGER.error(
          "Error while finding product with productCode : {} , unable to perform category change , errorCategory: {} , errorMessage : {}",
          productCode, ErrorCategory.VALIDATION, ErrorMessages.PRODUCT_NOT_FOUND);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.PRODUCT_NOT_FOUND);
    }
    return productCollection;
  }

  private ProductCollection updateProductCategoryAndHistory(ProductCollection productCollection,
      CategorySummaryResponse response) {
    saveHistory(productCollection, response);
    productCollection.setUpdatedDate(new Date());
    productCollection.setCategoryCode(response.getCategoryCode());
    productCollection.setCategoryName(response.getCategoryName());
    productCollectionRepository.save(productCollection);
    productService.updateSolrProductCollection(productCollection);
    return productCollection;
  }

  private void saveHistory(ProductCollection productCollection, CategorySummaryResponse categorySummaryResponse) {
    productLevel1HistoryService
        .saveProductHistory(productCollection.getProductCode(), GdnMandatoryRequestParameterUtil.getUsername(),
            ProductWorkflowLookup.STATE_EDIT_DESCRIPTION, String.valueOf(
                new ProductFieldHistory(SaveHistoryConstants.CATEGORY, productCollection.getCategoryName(),
                    categorySummaryResponse.getCategoryName())));
  }

  @Override
  public void processImageQcResponse(String storeId, ImageQcResponseDomainEvent imageQcResponseDomainEvent)
      throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(imageQcResponseDomainEvent.getProductCode()),
        ErrorMessages.EMPTY_PRODUCT_CODE);
    ProductDetailResponse productDetailResponse =
        productService.findProductDetailByProductCode(imageQcResponseDomainEvent.getProductCode(), true);
    if (Objects.nonNull(productDetailResponse)) {
      AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse;
      ProductImageQcProcessingResponse productImageQcProcessingResponse =
          productService.getProductImageQcProcessingResponse(storeId, imageQcResponseDomainEvent.getProductCode());
      Map<String, Long> imageCountMap = getImageCountMap(productDetailResponse);
      ProductCollection productCollection = productCollectionRepository
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, imageQcResponseDomainEvent.getProductCode());
      if (Objects.isNull(productCollection)) {
        log.info("Cannot find the requested product : {} ", imageQcResponseDomainEvent.getProductCode());
        return;
      }
      if (StringUtils.equals(GdnBaseLookup.INTERNAL_BUSINESS_PARTNER_CODE,
        productCollection.getBusinessPartnerCode())) {
        log.error("Skipping image qc response validation for product code : {} , because business "
            + "partner code is : {} ", productCollection.getProductCode(),
          productCollection.getBusinessPartnerCode());
        return;
      }
        ProfileResponse profileResponse =
            businessPartnerRepository.filterDetailByBusinessPartnerCode(productCollection.getBusinessPartnerCode());
      if (Objects.isNull(productImageQcProcessingResponse)) {
        autoNeedRevisionAndForceReviewResponse =
          productService.processImageQcResponse(storeId, imageQcResponseDomainEvent, imageCountMap,
            productCollection, productDetailResponse, profileResponse.isTrustedSeller(), profileResponse);
        CommonUtils.overrideAutoNeedRevisionAndTakeDownFlags(autoNeedRevisionAndForceReviewResponse);
        performResultantActionOnBrandAndCategoryTakeDown(autoNeedRevisionAndForceReviewResponse, profileResponse, productCollection);
        productCollection = productCollectionRepository
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, imageQcResponseDomainEvent.getProductCode());
        if (WorkflowStates.DRAFT.getValue().equals(productCollection.getState())) {
          processProductAfterReceivingImageQcEvent(productDetailResponse, productCollection,
            autoNeedRevisionAndForceReviewResponse.isSendProductToReview(), profileResponse);
        } else if (!WorkflowStates.NEED_CORRECTION.name().equals(productCollection.getState()) && productCollection
            .isReviewPending()) {
          productService.publishImageQcProcessedResponseEvent(storeId, imageQcResponseDomainEvent.getProductCode(),
              productCollection, productDetailResponse, autoNeedRevisionAndForceReviewResponse, profileResponse,
              imageQcResponseDomainEvent);
          if (productService.isForceReview(storeId, productCollection.getProductCode())) {
            log.info("Take down product after image image-qc: {}", imageQcResponseDomainEvent.getProductCode());
            productService
                .takeDownOrActivateProductByProductCode(storeId, imageQcResponseDomainEvent.getProductCode(), true);
          }
        }
      } else {
        log.info("Update image qc response : {}", imageQcResponseDomainEvent.getProductCode());
        ProductImageQcFeedbackResponse imageQcResponseFromPDT = new ProductImageQcFeedbackResponse();
        PDTProductDomainEventModel pdtDomainModelResponseByCode = new PDTProductDomainEventModel();
        if (productCollection.isPostLive()) {
          imageQcResponseFromPDT =
              productService.getImageQcResponseFromPDT(storeId, imageQcResponseDomainEvent.getProductCode());
          pdtDomainModelResponseByCode =
              productService.getPDTDomainModelResponseByCode(imageQcResponseDomainEvent.getProductCode());
        }
        autoNeedRevisionAndForceReviewResponse = productService
            .updateImageQcResponse(storeId, imageQcResponseDomainEvent, productImageQcProcessingResponse,
                productCollection, imageCountMap, imageQcResponseFromPDT,
              pdtDomainModelResponseByCode, productDetailResponse, profileResponse);
        CommonUtils.overrideAutoNeedRevisionAndTakeDownFlags(autoNeedRevisionAndForceReviewResponse);
        performResultantActionOnBrandAndCategoryTakeDown(autoNeedRevisionAndForceReviewResponse, profileResponse, productCollection);
        productService.publishImageQcProcessedResponseEvent(storeId, imageQcResponseDomainEvent.getProductCode(),
            productCollection, productDetailResponse, autoNeedRevisionAndForceReviewResponse,
          profileResponse, imageQcResponseDomainEvent);
        if (autoNeedRevisionAndForceReviewResponse.isForceReview() && productCollection.isPostLive()
          && productCollection.isReviewPending() && !WorkflowStates.NEED_CORRECTION.name()
          .equals(productCollection.getState())) {
          log.info("Take down product after image image-qc: {}",
            imageQcResponseDomainEvent.getProductCode());
          productService.takeDownOrActivateProductByProductCode(storeId,
            imageQcResponseDomainEvent.getProductCode(), true);
        }
      }
      if (autoNeedRevisionAndForceReviewResponse.isAutoNeedRevision()
        && !productCollection.isAutoNeedRevision() && productCollection.isReviewPending()) {
        boolean contentNeedRevision = false;
        String notes = StringUtils.EMPTY;
        if (Objects.nonNull(autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType())) {
          if (RestrictedKeywordActionType.AUTO_NEED_REVISION.getRestrictedKeywordActionType()
              == autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType().getAction()) {
            contentNeedRevision = true;
            notes = autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType().getMessage();
          }
        }
        if (publishAutoNeedRevisionEvent) {
          productService.publishAutoNeedRevisionEvent(storeId, imageQcResponseDomainEvent.getProductCode(),
              autoNeedRevisionAndForceReviewResponse.getPredictionTypeSet(), contentNeedRevision, notes);
        }
      }
    }
  }

  private void performResultantActionOnBrandAndCategoryTakeDown(
      AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse, ProfileResponse profileResponse,
      ProductCollection productCollection) {
    if (CommonUtils.eligibleForAutoNeedRevision(sendProductToAutoNROnBrandOrCategoryTakeDown,
        autoNeedRevisionAndForceReviewResponse, profileResponse, warehouseMerchantCommissionTypes)) {
      if (hasOrder(productCollection.getProductId(), productCollection.getProductCode(),
          productCollection.getStoreId())) {
        log.info("Sending Product {} to Manual review on brand TakeDown {} / category TakeDown as it has order {} ",
            productCollection.getProductCode(), autoNeedRevisionAndForceReviewResponse.isBrandTakeDown(),
            autoNeedRevisionAndForceReviewResponse.isCategoryTakeDown());
        productService.overRideRestrictedKeywordActionOnActiveOrders(productCollection.getProductCode(),
            autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType());
      } else {
        log.info("Sending Product {} to Auto Need revision on brand TakeDown {} / category TakeDown {} ",
            productCollection.getProductCode(), autoNeedRevisionAndForceReviewResponse.isBrandTakeDown(),
            autoNeedRevisionAndForceReviewResponse.isCategoryTakeDown());
        autoNeedRevisionAndForceReviewResponse.setForceReview(false);
        autoNeedRevisionAndForceReviewResponse.setAutoNeedRevision(true);
        autoNeedRevisionAndForceReviewResponse.setSendProductToReview(false);
      }
    }
  }

  private boolean hasOrder(String productId, String productCode, String storeId) {
    ProductBusinessPartner productBusinessPartner =
        productBusinessPartnerService.findFirstByStoreIdAndProductId(storeId, productId);
    AgpSimpleQueryResponse agpSimpleQueryResponse =
        productService.getQueryResponseForActiveOrderData(productCode, productBusinessPartner.getGdnProductSku());
    return Optional.ofNullable(agpSimpleQueryResponse.getHits()).map(HitsResponse::getTotal).orElse(0) > 0;
  }

  private Map<String, Long> getImageCountMap(ProductDetailResponse productDetailResponse) {
    return productDetailResponse.getProductItemResponses().stream()
      .map(ProductItemResponse::getImages).flatMap(List::stream).map(Image::getHashCode)
      .filter(Objects::nonNull)
      .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()));
  }

  @Override
  public void updateProductHistoryOnWholesaleChangesByScheduler(String merchantCode, String itemSku,
      boolean wholesalePriceActivated) throws Exception {
    ItemsSummaryDetailRequest itemFilterRequest = new ItemsSummaryDetailRequest();
    itemFilterRequest.setItemSku(itemSku);
    Page<ItemSummaryDetailResponse> productDatas =
        xProductOutbound.findSummaryDetailsByFilter(itemFilterRequest, PageRequest.of(0, 1));
    if (CollectionUtils.isNotEmpty(productDatas.getContent())) {
      ItemSummaryDetailResponse itemData = productDatas.getContent().get(0);
      updatedProductHistoryService.saveUpdateProductLevel3AuditForWholeSale(merchantCode, itemSku, UpdateProductActivity.WHOLE_PRICE_FLAG.getDesc(),
          String.valueOf(wholesalePriceActivated), String.valueOf(!wholesalePriceActivated), itemData.getProductSku(), itemData.getGeneratedItemName());
    }
  }

  @Override
  public void updateProductHistoryLevel3Audit(List<AuditTrailDto> auditTrailRequests, String accessChannel,
      boolean updateDirectly, boolean historySolrUpdateNewEvent) throws Exception {
    List<UpdatedProductHistory> audit;
    productLevel3Service.setProductNameInHistoryIfEmpty(auditTrailRequests);
    List<UpdatedProductHistory> auditLogs = new ArrayList<>();
    for (AuditTrailDto auditTrailRequest : auditTrailRequests) {
      updatedProductHistoryService.addAuditLogsForProductHistoryUpdate(auditTrailRequest, auditLogs, accessChannel);
    }
    if (updateDirectly) {
      audit = updatedProductHistoryService.createAudit(auditLogs, historySolrUpdateNewEvent);
    } else {
      GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = this.xProductOutbound
          .getItemPickupPointCodeByItemSkus(ConverterUtil.toSimpleListStringRequest(auditLogs));
      Map<String, String> itemSkuAndPickupPointCodeMap =
          ConverterUtil.toItemSkuAndPickupPointCodeMap(itemPickupPointCodeByItemSkus.getContent());
      auditLogs =
          CommonUtils.setOnlineStatusAndPickupPointCode(auditLogs, itemSkuAndPickupPointCodeMap);
      audit = updatedProductHistoryService.createAudit(auditLogs, historySolrUpdateNewEvent);
    }
    if (historySolrUpdateNewEvent) {
      publishSolrHistoryUpdateEvent(audit);
    }
  }

  @Override
  public void publishSolrHistoryUpdateEvent(List<UpdatedProductHistory> audit) {
    if (CollectionUtils.isNotEmpty(audit)) {
      UpdatedProductHistoryRequest updatedProductHistoryRequest = new UpdatedProductHistoryRequest();
      updatedProductHistoryRequest.setUpdatedProductHistories(audit);
      kafkaProducer.send(kafkaTopicProperties.getProductSkuSolrUpdateEvent(), audit.get(0).getProductSku(),
          updatedProductHistoryRequest);
    }
  }


  @Override
  public void updateImagePathsForEditedResizeImages(BulkImageProcessResponse bulkImageProcessResponse,
      boolean isRevised) throws Exception {
    ProductDetailResponse productDetailResponse =
        productService.findProductDetailByProductCode(bulkImageProcessResponse.getGroupCode(), false);
    if (Objects.nonNull(productDetailResponse)) {
      ProductCollection productCollection = productCollectionRepository
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(bulkImageProcessResponse.getStoreId(),
              bulkImageProcessResponse.getGroupCode());
      ProductAndItemImageRequest productAndItemImageRequest = ConverterUtil
          .toProductAndItemImageRequestForEditedResizedImages(bulkImageProcessResponse, productDetailResponse, isRevised);
      productService
          .updateEditedImagePathsAndFlagAfterResizingImage(productAndItemImageRequest, productCollection, true, false);
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
          new RestrictedKeywordsByFieldAndActionType();
      if (checkRestrictedKeywordsInEditedImage) {
        RestrictedKeywordsByFieldAndActionType restrictedKeywords =
            productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse,
                productCollection.getCategoryCode());
        if (Objects.nonNull(restrictedKeywords)) {
          restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(
              restrictedKeywords.getRestrictedKeywordsByFieldList());
          restrictedKeywordsByFieldAndActionType.setKeywordToKeywordRequestDTOMap(
            restrictedKeywords.getKeywordToKeywordRequestDTOMap());
        }
      }
      if (!isRevised) {
        if (preventEditedEventPublishForNeedRevisionProducts && Constants.NEED_CORRECTION.equals(
            productCollection.getState())) {
          log.info("Not publishing edited event to PDT as product is in need correction state : productCode {}",
              productCollection.getProductCode());
        } else {
          AddEditedProductToPDTEvent addEditedProductToPDTEvent =
              productService.publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE,
                  productCollection, null);
          if (!productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(productCollection.getProductCode(),
              false)) {
            productService.publishAddEditedProductToPDTEvent(productCollection.getStoreId(),
                EditedReviewTypeConstants.CONTENT_REFRESH, productCollection, null);
          }
          LOGGER.debug("Published com.gdn.image.qc.prediction.request event for productCode : {}, event : {}",
              productDetailResponse.getProductCode(), addEditedProductToPDTEvent);
        }
      } else {
        log.info("Updating states after image resizing and publish revised product to PDT. productCode = {}",
            productCollection.getProductCode());
        RestrictedKeywordsByFieldAndActionType restrictedKeywordsWithActionTypeInProductDetails = productLevel3Helper
            .getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse,
                productCollection.getCategoryCode());
        if (skipDefinitiveAction && CommonUtils.isDefinitiveActionToBeSkipped(
          restrictedKeywordsWithActionTypeInProductDetails.getAction())) {
          productService.validateSkipDefinitiveAction(bulkImageProcessResponse.getStoreId(),
            productCollection,
            restrictedKeywordsWithActionTypeInProductDetails);
        }
        ProfileResponse profileResponse = Optional.ofNullable(
          businessPartnerRepository.filterDetailByBusinessPartnerCode(
            productCollection.getBusinessPartnerCode())).orElse(new ProfileResponse());
        if (restrictedKeywordsWithActionTypeInProductDetails.getAction()
            == RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType()) {
          productCollection.setRestrictedKeywordsDetected(null);
          productCollection.setRestrictedKeywordsPresent(false);
        }
        Pair<RestrictedKeywordsByFieldAndActionType, Boolean> restrictedKeywordsByFieldAndActionTypeBooleanPair =
            updateProductStateAfterNeedRevisionSubmit(Constants.DEFAULT_STORE_ID, Constants.SYSTEM, productCollection,
                productDetailResponse,
                hasPostLiveConfig(productCollection, productDetailResponse, profileResponse.isTrustedSeller(),
                    restrictedKeywordsWithActionTypeInProductDetails.isSkipAllActions()),
                restrictedKeywordsWithActionTypeInProductDetails, profileResponse.isTrustedSeller(),
                false, null, null);
        restrictedKeywordsByFieldAndActionType = restrictedKeywordsByFieldAndActionTypeBooleanPair.getLeft();
        performResultantActionBasedOnRestrictedKeywords(bulkImageProcessResponse.getStoreId(),
            productDetailResponse.getProductCode(), restrictedKeywordsByFieldAndActionType.getAction(),
            restrictedKeywordsByFieldAndActionType.getCategoryRestrictedKeywordId(), true, false,
            productCollection.isAutoNeedRevision(),
            new ArrayList<>(getVendorErrorFields(restrictedKeywordsByFieldAndActionType)), true,
            productCollection.getBusinessPartnerCode(), profileResponse, null, null);
        if (restrictedKeywordsByFieldAndActionTypeBooleanPair.getRight()
            && RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType()
            == restrictedKeywordsByFieldAndActionType.getAction() && !profileResponse.isTrustedSeller()) {
          productDistributionTaskRepositoryBean.productRetryStatusUpdate(productCollection.getProductCode(),
              ProductRetryStatusUpdate.builder().state(WorkflowState.REJECTED.name()).markForDelete(true).build());
        }
      }
      ImageQcRequestDomainEvent imageQcRequestDomainEvent =
          productService.publishImageQcEventForEditedImages(productDetailResponse.getProductCode(),
              productAndItemImageRequest.getProductImages(), productDetailResponse, isRevised,
              restrictedKeywordsByFieldAndActionType, productCollection.getBusinessPartnerCode());
      LOGGER.debug("Published com.gdn.image.qc.prediction.request event for productCode : {}, event : {}",
          productDetailResponse.getProductCode(), imageQcRequestDomainEvent);
    }
  }

  @Override
  public void processPcbVendorPublishEvent(VendorPublishEventModel vendorPublishEventModel) throws Exception {
    ProductCollection productCollection = productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        vendorPublishEventModel.getStoreId(), vendorPublishEventModel.getProductCode());
    if (Objects.nonNull(productCollection) && productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(
        productCollection.getProductCode(), false)) {
      productService.publishAddEditedProductToPDTEvent(productCollection.getStoreId(),
          vendorPublishEventModel.getReviewType(), productCollection, null);
    }
  }

  private void updateItemNotes(String storeId, VendorNotesResponse vendorNotesResponse) {
    List<String> skuCodes = Optional.ofNullable(vendorNotesResponse.getItemNotes()).orElse(new ArrayList<>()).stream()
        .filter(itemNotes -> StringUtils.isEmpty(itemNotes.getItemSku())).map(ItemNotesDto::getSkuCode)
        .collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(skuCodes)) {
      Map<String, String> skuCodesAndProductItemIdsMap = productOutbound.getProductItemIdsBySkuCodes(skuCodes);
      Map<String, String> skuCodesAndItemSkusMap =
          productItemBusinessPartnerService.getSkuCodesAndItemSkusMap(storeId, skuCodesAndProductItemIdsMap);
      for (ItemNotesDto itemNotes : vendorNotesResponse.getItemNotes()) {
        if (skuCodesAndItemSkusMap.containsKey(itemNotes.getSkuCode())) {
          itemNotes.setItemSku(skuCodesAndItemSkusMap.get(itemNotes.getSkuCode()));
          itemNotes.setItemNumber(Integer.parseInt(
              StringUtils.substringAfterLast(itemNotes.getItemSku(), Constants.DASH_DELIMITER)
                  .replaceFirst(Constants.LEADINGZERO, StringUtils.EMPTY)));
        }
      }
    }
  }

  @Override
  public VendorNotesResponse getVendorNotes(String storeId, String productCode) throws IOException {
    LOGGER.debug("Fetching need revision notes for productCode : {}", productCode);
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    VendorNotesResponse vendorNotesResponse = new VendorNotesResponse();
    if (Objects.nonNull(productCollection)) {
      String vendorNotes = productCollection.getNeedCorrectionNotes();
      if (Objects.nonNull(vendorNotes)) {
        vendorNotesResponse = objectMapper.readValue(vendorNotes, VendorNotesResponse.class);
        updateItemNotes(storeId, vendorNotesResponse);
      } else {
        LOGGER.debug("Fetching need revision notes from history for productCode : {}", productCode);
        List<ProductRevisionInfoResponse> productRevisionInfoResponseList = new ArrayList<>();
        List<ProductHistory> productHistoryList =
            productLevel1HistoryService.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId, productCollection.getProductId());
        productHistoryList.stream().filter(ProductServiceWrapperBean::isRevisionHistory)
            .sorted(Comparator.comparing(ProductHistory::getCreatedDate).reversed())
            .map(ConverterUtil::toProductRevisionInfoResponse)
            .collect(Collectors.toCollection(() -> productRevisionInfoResponseList));
        if (CollectionUtils.isNotEmpty(productRevisionInfoResponseList)) {
          ProductRevisionInfoResponse productRevisionInfoResponse = productRevisionInfoResponseList.get(0);
          vendorNotesResponse.getVendorErrorFields().add(Constants.DESCRIPTION);
          vendorNotesResponse.getVendorErrorFields().add(Constants.DIMENSION);
          vendorNotesResponse.setAllVariants(true);
          vendorNotesResponse.getVendorNotes().add(productRevisionInfoResponse.getCorrectionReason());
          vendorNotesResponse.setContentAdditionalNotes(
              Optional.ofNullable(productRevisionInfoResponse.getAdditionalNotes()).orElse(StringUtils.EMPTY));
        }
      }
    }
    return constructVendorNotesResponse(vendorNotesResponse);
  }

  private VendorNotesResponse constructVendorNotesResponse(VendorNotesResponse vendorNotesResponse) {
    if (CollectionUtils.isNotEmpty(vendorNotesResponse.getItemNotes())) {
      List<ItemNotesDto> itemNotes =
          vendorNotesResponse.getItemNotes().stream().sorted(Comparator.comparing(ItemNotesDto::getItemSku))
              .collect(Collectors.toList());
      vendorNotesResponse.setItemNotes(itemNotes);
    }
    return vendorNotesResponse;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateVendorNotes(String storeId, String productCode, VendorNotesRequest vendorNotesRequest)
      throws IOException {
    ProductCollection productCollection = productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    if (Objects.nonNull(productCollection)) {
      String vendorNotes = productCollection.getNeedCorrectionNotes();
      VendorNotesResponse vendorNotesResponse = new VendorNotesResponse();
      if (Objects.nonNull(vendorNotes)) {
        vendorNotesResponse = objectMapper.readValue(vendorNotes, VendorNotesResponse.class);
      }
      vendorNotesRequest.setAllModifiedFields(vendorNotesResponse.getAllModifiedFields());
      productCollection.setNeedCorrectionNotes(objectMapper.writeValueAsString(vendorNotesRequest));
      productCollectionRepository.save(productCollection);
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorCategory.INVALID_STATE.getMessage());
    }
  }

  @Override
  public EditProductResponse needRevisionSubmit(String storeId, String username,
      NeedRevisionSubmitRequest needRevisionSubmitRequest) throws Exception {
    boolean postlive = false;
    ApiErrorCode apiErrorCode = null;
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCode(storeId, needRevisionSubmitRequest.getProductCode());
    ProfileResponse profileResponse = Optional.ofNullable(
        businessPartnerRepository.filterDetailByBusinessPartnerCode(
          productCollection.getBusinessPartnerCode())).orElse(new ProfileResponse());
    ProductBusinessPartnerCounter businessPartnerCounter = null;
    if (needRevisionSubmitRequest.isAppealedProduct()) {
      Pair<Integer, ProductBusinessPartnerCounter> pair =
          productAppealService.fetchThresholdAndCounterForAppealProduct(storeId,
              productCollection.getBusinessPartnerCode());
      businessPartnerCounter = pair.getRight();
      if (pair.getLeft() <= businessPartnerCounter.getAppealedProductCount()) {
        return EditProductResponse.builder().apiErrorCode(ApiErrorCode.APPEAL_LIMIT_CROSSED)
            .build();
      }
    }
    if (WorkflowStates.NEED_CORRECTION.getValue().equals(productCollection.getState())) {
      ProductDetailResponse productDetailResponse =
          productService.findProductDetailByProductCode(needRevisionSubmitRequest.getProductCode(), false);
      List<ImageRequest> revisedImageList =
          fileStorageService.getRevisedImageRequests(productDetailResponse.getImages(), imageSourceDirectory);
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsWithActionTypeInProductDetails = productLevel3Helper
          .getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse,
              productCollection.getCategoryCode());
      if (skipDefinitiveAction && CommonUtils.isDefinitiveActionToBeSkipped(
          restrictedKeywordsWithActionTypeInProductDetails.getAction())) {
        productService.validateSkipDefinitiveAction(storeId, productCollection,
          restrictedKeywordsWithActionTypeInProductDetails);
      }
      if (restrictedKeywordsWithActionTypeInProductDetails.getAction()
          == RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType()) {
        productCollection.setRestrictedKeywordsDetected(null);
        productCollection.setRestrictedKeywordsPresent(false);
      }
      postlive = hasPostLiveConfig(productCollection, productDetailResponse, profileResponse.isTrustedSeller(),
          ConverterUtil.isSkipAllAction(restrictedKeywordsWithActionTypeInProductDetails));
      ProductImageQcProcessingResponse productImageQcProcessingResponse = productImageQcProcessingResponseService
          .findByStoreIdAndProductCode(storeId, productCollection.getProductCode());
      if(Objects.nonNull(productImageQcProcessingResponse)) {
        ImageQcResponseDomainEvent imageQcResponseDomainEvent = objectMapper
            .readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class);
        Set<String> imagesMappedToProduct =
            productDetailResponse.getImages().stream().filter(image -> !image.isMarkForDelete()).map(Image::getHashCode).collect(Collectors.toSet());
        Set<String> predictionList =
          imageQcResponseDomainEvent.getImages().stream().filter(imageQcResponse -> imagesMappedToProduct.contains(imageQcResponse.getHashCode()
            .replaceAll(Constants.FILESTORE_PATH + "|" + gcsProperties.getPathPrefix(), "")))
            .flatMap(imageQcResponse -> imageQcResponse.getPredictions().stream()).filter(ImageQcPredictionResponse::isPresent).map(ImageQcPredictionResponse::getDisplayName)
            .collect(Collectors.toSet());
        productImageQcProcessingResponse.setImageViolations(String.join(Constants.COMMA, predictionList));
        productImageQcProcessingResponseService.save(productImageQcProcessingResponse);
      }
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType = null;
      Pair<RestrictedKeywordsByFieldAndActionType, Boolean> restrictedKeywordsByFieldAndActionTypeBooleanPair = null;
      if (CollectionUtils.isNotEmpty(revisedImageList)) {
        log.info("Publishing revised images for need revision request : {} ", needRevisionSubmitRequest);
        publishRevisedImages(storeId, needRevisionSubmitRequest, revisedImageList);
      } else {
        restrictedKeywordsByFieldAndActionTypeBooleanPair =
            updateProductStateAfterNeedRevisionSubmit(storeId, username, productCollection, productDetailResponse,
                postlive, restrictedKeywordsWithActionTypeInProductDetails, profileResponse.isTrustedSeller(),
                needRevisionSubmitRequest.isAppealedProduct(), needRevisionSubmitRequest.getAppealedProductNotes(),
                businessPartnerCounter);
        restrictedKeywordsByFieldAndActionType = restrictedKeywordsByFieldAndActionTypeBooleanPair.getLeft();
        publishImageQcEventForNeedRevisionSubmit(restrictedKeywordsByFieldAndActionType, productCollection, productDetailResponse);
      }
      productLevel1HistoryService
          .saveProductHistory(productCollection.getProductCode(), username, SaveHistoryConstants.PRODUCT_RESUBMITTED,
              null);
      if (Objects.nonNull(restrictedKeywordsByFieldAndActionType)) {
        performResultantActionBasedOnRestrictedKeywords(storeId, productDetailResponse.getProductCode(),
            restrictedKeywordsByFieldAndActionType.getAction(),
            restrictedKeywordsByFieldAndActionType.getCategoryRestrictedKeywordId(), true, false,
            productCollection.isAutoNeedRevision(),
            new ArrayList<>(getVendorErrorFields(restrictedKeywordsByFieldAndActionType)), true,
            productCollection.getBusinessPartnerCode(), profileResponse, null, null);
        if (restrictedKeywordsByFieldAndActionTypeBooleanPair.getRight()
            && RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType()
            == restrictedKeywordsByFieldAndActionType.getAction() && !profileResponse.isTrustedSeller()) {
          productDistributionTaskRepositoryBean.productRetryStatusUpdate(productCollection.getProductCode(),
              ProductRetryStatusUpdate.builder().state(WorkflowState.REJECTED.name()).markForDelete(true).build());
        }
      }
    } else {
      apiErrorCode = ApiErrorCode.PRODUCT_NOT_PRESENT;
    }
    return new EditProductResponse(true, postlive ? Constants.POST_LIVE : Constants.PRE_LIVE,
      apiErrorCode, false, new ArrayList<>());
  }

  private void publishImageQcEventForNeedRevisionSubmit(
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType,
      ProductCollection productCollection, ProductDetailResponse productDetailResponse) {
    if (StringUtils.isNotEmpty(productCollection.getRestrictedKeywordsDetected())) {
      productService.publishImageQcEventForContentEdit(
          Optional.ofNullable(productDetailResponse.getProductCategoryResponses()).orElse(new ArrayList<>()).stream()
              .map(ProductCategoryResponse::getCategory).collect(Collectors.toList()),
          restrictedKeywordsByFieldAndActionType,
          ConverterUtil.generateProductLevel3FromProductDetailResponse(productCollection, productDetailResponse), true);
    }
  }

  private Pair<RestrictedKeywordsByFieldAndActionType, Boolean> updateProductStateAfterNeedRevisionSubmit(String storeId,
      String username, ProductCollection productCollection, ProductDetailResponse productDetailResponse,
      boolean postLive,RestrictedKeywordsByFieldAndActionType restrictedKeywordsWithActionTypeInProductDetails,
      Boolean trustedSeller, boolean appealedProduct, String appealedProductNotes,
      ProductBusinessPartnerCounter businessPartnerCounter) throws Exception {
    String oldCategoryName = StringUtils.EMPTY;
    boolean isCategoryChange = restrictedKeywordsWithActionTypeInProductDetails.getAction()
        == RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType();
    List<ProductBusinessPartner> productBusinessPartners = this.productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId, productDetailResponse.getId());
    ProductBusinessPartner productBusinessPartner;
    if (CollectionUtils.isNotEmpty(productBusinessPartners)) {
      productBusinessPartner = productBusinessPartners.get(0);
      productBusinessPartner.setState(Constants.IN_PROGRESS_STATE);
      productBusinessPartner.setActivated(false);
      productBusinessPartner.setSubmittedDate(new Date());
      productBusinessPartner.setExpectedActivationDate(
          productBusinessPartnerService.getExpectedActivationDateByCategoryCode(productDetailResponse, new Date()));
    } else {
      log.error("Product business partner not found not found productCode : {} ", productCollection.getProductCode());
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "Product Business partner not found");
    }
    if (isCategoryChange) {
      productCollection.setRestrictedKeywordsDetected(StringUtils.EMPTY);
      LOGGER.info("Getting CategoryRestrictedKeywordResponse from pcb by id : {}",
          restrictedKeywordsWithActionTypeInProductDetails.getCategoryRestrictedKeywordId());
      CategoryRestrictedKeywordResponse messageById =
          getMessageById(restrictedKeywordsWithActionTypeInProductDetails.getCategoryRestrictedKeywordId());
      LOGGER.info(
          "Updating the new Category in pcb for need revision flow for productCode : {} and with destinationCategory : {}",
          productCollection.getProductCode(), messageById.getDestinationCategory());
      if (!productCollection.getCategoryCode().equals(messageById.getDestinationCategory())) {
        CategorySummaryResponse response =
            updateCategoryInPcb(productCollection.getProductCode(), messageById.getDestinationCategory(), false,productCollection.getBusinessPartnerCode());
        oldCategoryName=productCollection.getCategoryName();
        ConverterUtil.setProductCollectionDetailsForCategoryChangeNeedRevision(productCollection, messageById, response);
        restrictedKeywordsWithActionTypeInProductDetails.setRestrictedKeywordsByFieldList(new ArrayList<>());
        ConverterUtil.setProductBusinessPartnerForCategoryChange(productCollection, productBusinessPartner);
        xProductOutbound.generateProductScoreByProductSkuOrProductCode(productBusinessPartner.getGdnProductSku(),
            productCollection.getProductCode(), true);
      }
    }
    productBusinessPartner.setAppealedProduct(appealedProduct);
    productBusinessPartnerService.saveProductBusinessPartner(productBusinessPartner);
    updateProductItemWholeSalePrice(storeId, productBusinessPartner.getGdnProductSku(), productBusinessPartner.getBusinessPartnerId(), productDetailResponse);
    boolean ifProductExistsInPDT =
        productService.checkIfProductExistsInPDT(productCollection.getProductCode(), true);
    if (!postLive && !ifProductExistsInPDT && !isSkipScreeningSwitch) {
      productCollection.setState(WorkflowStates.DRAFT.getValue());
      productCollection.setActivated(false);
      productCollection.setViewable(false);
    } else {
      productCollection.setState(Constants.IN_PROGRESS_STATE);
      productCollection.setActivated(true);
      productCollection.setViewable(false);
    }
    productCollection.setPostLive(postLive);
    productCollection.setResubmitCount(productCollection.getResubmitCount() + 1);
    productCollection.setNeedRevision(true);
    if (!postLive && !ifProductExistsInPDT && !isSkipScreeningSwitch) {
      productService.addProductToReviewCollection(productCollection);
      productWfService
          .deleteAllExistingWorkFlowAndCreateNewState(storeId, productCollection.getProductCode(),
              WorkflowStates.DRAFT.getValue());
    } else {
      if (ResponseHelper.isProductEligibleForVendorPublish(restrictedKeywordsWithActionTypeInProductDetails.getAction(),
          productCollection.isAutoNeedRevision(), trustedSeller)) {
        // If product is already in auto need revision, won't send it 2nd time
        productPublisherService.publishRevisedProductToPDT(
            ConverterUtil.toAddRevisedProductToPDTEvent(productCollection, username, trustedSeller,
                productBusinessPartner, appealedProduct, appealedProductNotes,
                priceInfoVendorRevisedEnabled, priceInfoMaxVariantLimit));
      }
    }
    productService.saveProductCollection(productCollection);
    updateCounterForAppealProduct(appealedProduct, businessPartnerCounter);
    if (isCategoryChange && StringUtils.isNotEmpty(oldCategoryName)){
      InternalProductHistoryEventModel internalProductHistoryEventModel =
          ConverterUtil.createInternalProductHistoryEventModel(productCollection, oldCategoryName,
              restrictedKeywordsWithActionTypeInProductDetails.getKeyword());
      kafkaProducer.send(DomainEventName.PRODUCT_INTERNAL_HISTORY_SAVE, productCollection.getProductCode(),
          internalProductHistoryEventModel);
      AuditTrailListRequest auditTrailListRequest = CommonUtils
          .getAuditTrailRequestForL3History(productCollection.getBusinessPartnerCode(),
              productBusinessPartners.get(0).getGdnProductSku(), productCollection.getProductName(),
              UpdateProductActivity.AUTO_CATEGORY_CHANGE.name(), StringUtils.EMPTY, oldCategoryName,
              productCollection.getCategoryName(), SaveHistoryConstants.AUTO_CATEGORY_CHANGE,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT, Constants.HYPHEN);
      kafkaProducer
          .send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, productBusinessPartners.get(0).getGdnProductSku(),
          auditTrailListRequest);
      }
    return Pair.of(restrictedKeywordsWithActionTypeInProductDetails, ifProductExistsInPDT);
  }

  private void updateCounterForAppealProduct(boolean appealedProduct,
      ProductBusinessPartnerCounter businessPartnerCounter) {
    if (appealedProduct) {
      productAppealService.incrementCounterForProductAppeal(businessPartnerCounter);
    }
  }

  private boolean hasPostLiveConfig(ProductCollection productCollection, ProductDetailResponse productDetailResponse,
    Boolean trustedSeller, boolean skipAllAction) {
    ConfigurationStatusRequest configurationStatusRequest = ConfigurationStatusRequest.builder()
        .categoryCode(productDetailResponse.getCategoryCodes().get(productDetailResponse.getCategoryCodes().size() - 1))
        .businessPartnerCode(productCollection.getBusinessPartnerCode()).build();
    List<ConfigurationStatusResponse> configurationStatusResponseList;
    try {
      configurationStatusResponseList =
          productOutbound.getReviewConfiguration(Collections.singletonList(configurationStatusRequest));
    } catch (Exception e) {
      LOGGER.error("Error while getting configuration for categoryCode : {}, businessPartnerCode : {} ",
          configurationStatusRequest.getCategoryCode(), configurationStatusRequest.getBusinessPartnerCode(), e);
      configurationStatusResponseList = Collections.singletonList(
          new ConfigurationStatusResponse(productCollection.getBusinessPartnerCode(),
              productDetailResponse.getCategoryCodes().get(0), Constants.PRE_LIVE));
    }
    boolean relaxRestrictedKeywordCheck = trustedSeller ? Boolean.TRUE : skipAllAction;
    boolean postLive =
        Constants.POST_LIVE.equals(configurationStatusResponseList.get(0).getReviewConfig()) && relaxRestrictedKeywordCheck;
    productCollection.setPostLive(postLive);

    return postLive;
  }

  private void publishRevisedImages(String storeId, NeedRevisionSubmitRequest needRevisionSubmitRequest,
      List<ImageRequest> revisedImageList) {
    EditedImageResizeEvent editedImageResizeEvent = new EditedImageResizeEvent();
    editedImageResizeEvent.setProductCode(needRevisionSubmitRequest.getProductCode());
    editedImageResizeEvent.setStoreId(storeId);
    editedImageResizeEvent.setImageRequests(revisedImageList);
    productPublisherService.publishReviseImageResizeEvent(editedImageResizeEvent);
  }

  private void updateProductItemWholeSalePrice(String storeId, String productSku, String businessPartnerCode,
      ProductDetailResponse productDetailResponse) throws Exception {
    List<String> productItemIds =
        productDetailResponse.getProductItemResponses().stream().map(ProductItemResponse::getId)
            .collect(Collectors.toList());
    List<ProductItemWholesalePrice> productItemWholesalePrices =
        productItemWholesalePriceService.findByStoreIdAndProductItemId(storeId, productItemIds);
    productItemWholesalePrices = productItemWholesalePrices.stream().filter(ProductItemWholesalePrice::isUpdatePending)
        .collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(productItemWholesalePrices)) {
      WholesalePriceBulkUpdateResponse wholesalePriceResponse = productPricingOutbound.upsertWholesalePrice(
          ConverterUtil.toWholesalePriceRequest(productSku, businessPartnerCode, false, productItemWholesalePrices));
      Map<String, String> productItemIdAndItemNameMap = productDetailResponse.getProductItemResponses().stream()
          .collect(Collectors.toMap(ProductItemResponse::getId, ProductItemResponse::getGeneratedItemName));
      List<Map<String, String>> wholesaleHistory = new ArrayList<>();
        Map<String, Boolean> updatedItemSkusAndSkuStatusMap =
            Optional.ofNullable(wholesalePriceResponse.getWholesalePriceSkuStatus()).orElse(new ArrayList<>()).stream()
                .collect(Collectors.toMap(
                    response -> CommonUtils.toL5Id(response.getItemSku(), response.getPickUpPointCode()),
                    wholeSalePriceSkuStatusDto -> Constants.ACTIVE.equals(wholeSalePriceSkuStatusDto.getSkuStatus())));
        for (ProductItemWholesalePrice productItemWholesalePrice : productItemWholesalePrices) {
          Boolean newWholesaleFlag = updatedItemSkusAndSkuStatusMap.get(
              CommonUtils.toL5Id(productItemWholesalePrice.getItemSku(),
                  productItemWholesalePrice.getPickupPointCode()));
          wholesaleHistory.add(
              CommonUtils.generateWholesaleHistory(productItemWholesalePrice.isWholesalePriceActivated(), newWholesaleFlag,
                  productItemWholesalePrice.getItemSku(), productItemWholesalePrice.getPickupPointCode(),
                  productItemIdAndItemNameMap.get(productItemWholesalePrice.getProductItemId())));
          productItemWholesalePrice.setWholesalePriceActivated(Boolean.TRUE.equals(newWholesaleFlag));
          productItemWholesalePrice.setUpdatePending(false);
        }
      productItemWholesalePriceService.saveWholesalePrice(productItemWholesalePrices);

      wholesaleHistory = wholesaleHistory.stream().filter(MapUtils::isNotEmpty).collect(Collectors.toList());
      if (CollectionUtils.isNotEmpty(wholesaleHistory)) {
        for (Map<String, String> historyAudit : wholesaleHistory) {
          this.updatedProductHistoryService.createProductL3AuditLog(businessPartnerCode, null, productSku,
              historyAudit.get(Constants.ITEM_NAME), historyAudit.get(Constants.HISTORY_ACTIVITY),
              historyAudit.get(Constants.PREVIOUS_VALUE), historyAudit.get(Constants.CURRENT_VALUE), false,
              historyAudit.get(Constants.PICKUP_POINT_CODE));
        }
      }
    }
  }

  @Override
  public void autoApproveProduct(String storeId, String productCode) throws Exception {
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (Objects.nonNull(productCollection)) {
      LOGGER.info("Auto Approving the product productCode = {} , reviewPending = {} , isEdited = {} ,"
              + " isPostLive = {} , isAutoNeedRevision = {} ",
          productCode, productCollection.isReviewPending(), productCollection.isEdited(),
          productCollection.isPostLive(), productCollection.isAutoNeedRevision());
    }
    if (Objects.nonNull(productCollection) && productCollection.isReviewPending() && !productCollection.isEdited()
        && productCollection.isPostLive() && !productCollection.isAutoNeedRevision()) {
      if (!Constants.ACTIVE.equals(productCollection.getState())) {
        LOGGER.info("Auto Approval failed because product is not active for productCode = {} ", productCode);
        saveProductHistory(storeId, productCode, SaveHistoryConstants.CANNOT_AUTO_APPROVED,
            SaveHistoryConstants.NOT_ACTIVE_ERROR_MESSAGE);
        productDistributionTaskRepositoryBean.sendProductBackToVendor(storeId, productCode);
        productPublisherService
            .publishProductQCRetryEvent(new ProductQCRetryEvent(storeId, Constants.DEFAULT_USERNAME, productCode));
      } else {
        LOGGER.info("Auto Approving the product for productCode = {} ", productCode);
        productOutbound.deleteOriginalImages(productCode);
        productCollection.setReviewPending(false);
        productCollection.setRestrictedKeywordsPresent(false);
        productCollection.setAutoApprovalType(AutoApprovalType.NA);
        productService.saveProductCollection(productCollection);
        saveProductHistory(storeId, productCode, SaveHistoryConstants.PRODUCT_AUTO_APPROVAL_SUCCESSFUL, null);
        productService.updateSolrProductCollection(productCollection);
        productService.deleteFromReviewProductCollection(Collections.singletonList(productCollection.getId()));
        productService.removeProductFromPDT(productCode);
        productService.updateImageQcDataAfterVendorApproval(storeId, productCode);
      }
    }
  }

  private void saveProductHistory(String storeId, String productCode, String description, String notes) throws Exception {
    ProductHistory productHistory = new ProductHistory();
    productHistory.setDescription(description);
    productHistory.setNotes(notes);
    productHistory.setState(5);
    productHistory.setStoreId(storeId);
    productHistory.setCreatedDate(new Date());
    productHistory.setUpdatedDate(new Date());
    productHistory.setCreatedBy(Constants.DEFAULT_USERNAME);
    productHistory.setUpdatedBy(Constants.DEFAULT_USERNAME);
    productService.saveProductHistory(productCode, productHistory);
  }

  @Override
  public void autoNeedRevisionProduct(AutoNeedRevisionDomainEvent autoNeedRevisionDomainEvent,
      boolean contentNeedRevision, boolean screeningAction, boolean validateDraftState, boolean overrideDataFromPDT,
      boolean validateAssignment)
      throws Exception {
    ProductCollection productCollection = productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(autoNeedRevisionDomainEvent.getStoreId(),
            autoNeedRevisionDomainEvent.getProductCode());
    if (Objects.nonNull(productCollection) && productCollection.isReviewPending()) {
      AutoNeedRevisionRequest autoNeedRevisionRequest =
          getAutoNeedRevisionRequest(autoNeedRevisionDomainEvent, productCollection, contentNeedRevision);
      if (productCollection.isPostLive() || productCollection.isEdited()) {
        if (productDistributionTaskRepositoryBean
            .checkIfProductExistsInPDT(productCollection.getProductCode(), false)) {
          postLiveAutoNeedRevision(autoNeedRevisionDomainEvent, productCollection, autoNeedRevisionRequest,
              overrideDataFromPDT, validateAssignment);
        } else {
          // This block for fully approved products and not present in vendor anymore, we don't want to update anything in vendor
          // take down the product and make product need revision in PBP
          needCorrectionService.takeDownNeedForCorrectionProduct(productCollection.getProductCode(), getItemCodeAndIdMap(productCollection),
              null);
          needCorrectionService
              .updateStateInPBPAndSendProductToNeedRevision(autoNeedRevisionDomainEvent.getProductCode(),
                  autoNeedRevisionRequest.getReason(),
                  getNeedRevisionNotes(autoNeedRevisionRequest, contentNeedRevision), true, screeningAction,
                  validateDraftState, productCollection);
        }
      } else if (!Constants.ACTIVE.equals(productCollection.getState())) {
        log.info("Pre-live Auto need revision for productCode : {} ", productCollection.getProductCode());
        if (isSkipScreeningSwitch && !screeningAction) {
          productDistributionTaskRepositoryBean
              .sendProductToAutoNeedRevision(autoNeedRevisionRequest, false);
          needCorrectionService.sendForNeedCorrectionByProductCode(autoNeedRevisionDomainEvent.getProductCode(), true,
              autoNeedRevisionRequest, true);
        } else {
          needCorrectionService
              .updateStateInPBPAndSendProductToNeedRevision(autoNeedRevisionDomainEvent.getProductCode(),
                  autoNeedRevisionRequest.getReason(),
                  getNeedRevisionNotes(autoNeedRevisionRequest, contentNeedRevision), true, screeningAction,
                  validateDraftState, productCollection);
        }
        publishReviewProductCollectionDeleteEvent(productCollection);
      }
    }
  }

  private Map<String, String> getItemCodeAndIdMap(ProductCollection productCollection) {
    if (ProductCreationType.MIGRATION.getProductCreationType().equals(productCollection.getProductCreationType())) {
      ProductDetailResponse productDetailResponse =
          productOutbound.getProductDetailByProductCode(productCollection.getProductCode(), false, false);
      return productDetailResponse.getProductItemResponses().stream()
          .collect(Collectors.toMap(ProductItemResponse::getSkuCode, ProductItemResponse::getId));
    }
    return new HashMap<>();
  }

  private void publishReviewProductCollectionDeleteEvent(ProductCollection productCollection) {
    SolrReviewProductCollectionDeleteEvent solrReviewProductCollectionDeleteEvent =
        new SolrReviewProductCollectionDeleteEvent();
    solrReviewProductCollectionDeleteEvent.setIds(Collections.singletonList(productCollection.getId()));
    kafkaProducer.send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
        solrReviewProductCollectionDeleteEvent);
  }

  @Override
  public RetryAutoNeedRevisionResponse retryAutoNeedRevision(RetryNeedRevisionRequest retryNeedRevisionRequest,
      boolean validateAssignment)
      throws Exception {
    RetryAutoNeedRevisionResponse retryAutoNeedRevisionResponse =
        getRetryAutoNeedRevisionResponse(retryNeedRevisionRequest);
    ProductCollection productCollection = productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(retryNeedRevisionRequest.getStoreId(),
            retryNeedRevisionRequest.getProductCode());
    if (Objects.nonNull(productCollection) && productCollection.isReviewPending()) {
      AutoNeedRevisionRequest autoNeedRevisionRequest = getAutoNeedRevisionRequest(retryNeedRevisionRequest);
      AutoNeedRevisionDomainEvent autoNeedRevisionDomainEvent =
          getAutoNeedRevisionDomainEvent(retryNeedRevisionRequest);
      if (productCollection.isPostLive()) {
        if (!Constants.ACTIVE.equals(productCollection.getState())) {
          retryAutoNeedRevisionResponse.setProductActive(false);
          return retryAutoNeedRevisionResponse;
        }
        postLiveAutoNeedRevision(autoNeedRevisionDomainEvent, productCollection, autoNeedRevisionRequest, true,
            validateAssignment);
      } else if (!Constants.ACTIVE.equals(productCollection.getState())) {
        log.info("Pre-live Auto need revision for productCode : {} ", productCollection.getProductCode());
        productWfService
            .returnForCorrection(autoNeedRevisionDomainEvent.getProductCode(), autoNeedRevisionRequest.getReason(),
                getNeedRevisionNotes(autoNeedRevisionRequest, false), true, true, true);
      } else {
        retryAutoNeedRevisionResponse.setSuccess(false);
      }
    } else {
      retryAutoNeedRevisionResponse.setSuccess(false);
    }
    return retryAutoNeedRevisionResponse;
  }

  private RetryAutoNeedRevisionResponse getRetryAutoNeedRevisionResponse(
      RetryNeedRevisionRequest retryNeedRevisionRequest) {
    RetryAutoNeedRevisionResponse retryAutoNeedRevisionResponse = new RetryAutoNeedRevisionResponse();
    try{
      NeedRevisionReasonRequest needRevisionReasonRequest = objectMapper
        .readValue(retryNeedRevisionRequest.getNotes(), NeedRevisionReasonRequest.class);
      retryAutoNeedRevisionResponse.setReason(needRevisionReasonRequest.getNotes());
      retryAutoNeedRevisionResponse.setImageReason(needRevisionReasonRequest.getImageNotes());
    }
    catch (Exception e){
      LOGGER.error("Exception caught while mapping Need Revision Request for product code {} ",
        retryNeedRevisionRequest.getProductCode(),e);
      retryAutoNeedRevisionResponse.setReason(retryNeedRevisionRequest.getNotes());
      retryAutoNeedRevisionResponse.setImageReason(Constants.OTHERS_NOTES_REASON);
    }
    retryAutoNeedRevisionResponse.setProductCode(retryNeedRevisionRequest.getProductCode());
    return retryAutoNeedRevisionResponse;
  }

  private AutoNeedRevisionDomainEvent getAutoNeedRevisionDomainEvent(
      RetryNeedRevisionRequest retryNeedRevisionRequest) {
    AutoNeedRevisionDomainEvent autoNeedRevisionDomainEvent = new AutoNeedRevisionDomainEvent();
    autoNeedRevisionDomainEvent.setStoreId(retryNeedRevisionRequest.getStoreId());
    autoNeedRevisionDomainEvent.setProductCode(retryNeedRevisionRequest.getProductCode());
    return autoNeedRevisionDomainEvent;
  }

  private AutoNeedRevisionRequest getAutoNeedRevisionRequest(RetryNeedRevisionRequest retryNeedRevisionRequest) {
    AutoNeedRevisionRequest autoNeedRevisionRequest = new AutoNeedRevisionRequest();
    try {
      NeedRevisionReasonRequest needRevisionReasonRequest = objectMapper
        .readValue(retryNeedRevisionRequest.getNotes(), NeedRevisionReasonRequest.class);
      autoNeedRevisionRequest.setReason(needRevisionReasonRequest.getNotes());
      autoNeedRevisionRequest.setImageReason(needRevisionReasonRequest.getImageNotes());
    } catch (Exception e) {
      LOGGER.error("Exception caught while mapping Need Revision Request for product code {} ",
        retryNeedRevisionRequest.getProductCode(), e);
      autoNeedRevisionRequest.setReason(retryNeedRevisionRequest.getNotes());
      autoNeedRevisionRequest.setImageReason(Constants.OTHERS_NOTES_REASON);
    }
    autoNeedRevisionRequest.setProductCode(retryNeedRevisionRequest.getProductCode());
    autoNeedRevisionRequest.setState(WorkflowState.NEED_CORRECTION.name());
    return autoNeedRevisionRequest;
  }

  private void postLiveAutoNeedRevision(AutoNeedRevisionDomainEvent autoNeedRevisionDomainEvent,
      ProductCollection productCollection, AutoNeedRevisionRequest autoNeedRevisionRequest, boolean overrideDataFromPDT,
      boolean validateAssignment)
      throws Exception {
    if (Constants.ACTIVE.equals(productCollection.getState())) {
      log.info("Post-Live auto need revision for productCode : {} ", autoNeedRevisionDomainEvent.getProductCode());
      productDistributionTaskRepositoryBean.sendProductToAutoNeedRevision(autoNeedRevisionRequest, validateAssignment);
      log.info("Post-Live auto need revision updated in PDT for productCode : {} ",
          autoNeedRevisionDomainEvent.getProductCode());
      needCorrectionService.sendForNeedCorrectionByProductCode(autoNeedRevisionDomainEvent.getProductCode(), true,
          autoNeedRevisionRequest, overrideDataFromPDT);
    } else {
      log.info("Product not yet active for auto need revision productCode : {} ",
          autoNeedRevisionDomainEvent.getProductCode());
      productService.publishProductActionRetryEvent(
          new ProductActionRetryEvent(autoNeedRevisionDomainEvent.getStoreId(),
              autoNeedRevisionDomainEvent.getProductCode(), Constants.AUTO_NEED_REVISION_ACTION,
            objectMapper.writeValueAsString(
              new NeedRevisionReasonRequest(autoNeedRevisionRequest.getReason(),
                autoNeedRevisionRequest.getImageReason()))));
    }
  }

  private AutoNeedRevisionRequest getAutoNeedRevisionRequest(AutoNeedRevisionDomainEvent autoNeedRevisionDomainEvent,
      ProductCollection productCollection, boolean contentNeedRevision) throws Exception {
    ProfileResponse profileResponse =
        businessPartnerRepository.filterDetailByBusinessPartnerCode(productCollection.getBusinessPartnerCode());
    boolean internationalFlag = profileResponse.getCompany().isInternationalFlag();
    List<String> imagePredictionList = getImagePredictionList(autoNeedRevisionDomainEvent, internationalFlag);
    return getAutoNeedRevisionRequest(productCollection, imagePredictionList, internationalFlag, contentNeedRevision,
        autoNeedRevisionDomainEvent.getNotes(), autoNeedRevisionDomainEvent.getVendorErrorFields());
  }

  private List<String> getImagePredictionList(AutoNeedRevisionDomainEvent autoNeedRevisionDomainEvent,
      boolean internationalFlag) {
    return autoNeedRevisionDomainEvent.getPredictionTypeList().stream().map(
        predictionType -> productImagePredictionService
            .findByStoreIdAndPredictionType(autoNeedRevisionDomainEvent.getStoreId(), predictionType)).map(
        productImagePrediction -> internationalFlag ?
            productImagePrediction.getDisplayName() :
            productImagePrediction.getDisplayNameIn()).collect(Collectors.toList());
  }

  private NeedRevisionNotes getNeedRevisionNotes(AutoNeedRevisionRequest autoNeedRevisionRequest,
      boolean contentNeedRevision) {
    NeedRevisionNotes needRevisionNotes = new NeedRevisionNotes();
    needRevisionNotes.setAllVariants(true);
    if (!contentNeedRevision) {
      needRevisionNotes.setImagesAdditionalNotes(autoNeedRevisionRequest.getReason());
    }
    if (StringUtils.isNotBlank(autoNeedRevisionRequest.getImageReason())) {
      needRevisionNotes.setImageReason(Collections.singletonList(autoNeedRevisionRequest.getImageReason()));
    }
    needRevisionNotes.setContentAdditionalNotes(autoNeedRevisionRequest.getContentReason());
    needRevisionNotes.setItemNotes(new ArrayList<>());
    needRevisionNotes.setVendorErrorFields(autoNeedRevisionRequest.getVendorErrorFields());
    if (contentNeedRevision) {
      needRevisionNotes.setVendorNotes(Collections.singletonList(Constants.OTHERS_PRE_LIVE_NOTES_REASON));
    }
    return needRevisionNotes;
  }

  private AutoNeedRevisionRequest getAutoNeedRevisionRequest(ProductCollection productCollection,
      List<String> imagePredictionList, boolean internationalFlag, boolean contentNeedRevision, String notes,
      List<String> vendorErrorFields) {
    AutoNeedRevisionRequest autoNeedRevisionRequest = new AutoNeedRevisionRequest();
    autoNeedRevisionRequest.setEdited(productCollection.isEdited());
    autoNeedRevisionRequest.setEditedState(productCollection.getReviewType());
    autoNeedRevisionRequest.setProductCode(productCollection.getProductCode());
    autoNeedRevisionRequest.setState(WorkflowWebState.NEED_CORRECTION.name());
    autoNeedRevisionRequest.setVendorErrorFields(vendorErrorFields);
    if (contentNeedRevision) {
      autoNeedRevisionRequest.setContentReason(notes);
      autoNeedRevisionRequest.setReason(notes);
    } else {
      if (internationalFlag) {
        autoNeedRevisionRequest.setReason(
            SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE_EN + StringUtils.join(imagePredictionList, Constants.COMMA)
                + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE_EN);
        autoNeedRevisionRequest.setImageReason(getImageReasonEN(imagePredictionList));
      } else {
        autoNeedRevisionRequest.setReason(
            SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + StringUtils.join(imagePredictionList, Constants.COMMA)
                + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE);
        autoNeedRevisionRequest.setImageReason(getImageReasonIN(imagePredictionList));
      }
    }
    return autoNeedRevisionRequest;
  }

  private String getImageReasonEN(List<String> imagePredictionList) {
    if (imagePredictionList.contains(Constants.BLUR)) {
      if (imagePredictionList.size() > 1) {
        imagePredictionList.remove(Constants.BLUR);
        return SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_BLUR_IMAGE_EN
          + SaveHistoryConstants.AUTO_QC_NEED_REVISION_POST_MESSAGE_FOR_MULTIPLE_PREDICTION_EN
          + StringUtils.join(imagePredictionList, Constants.COMMA);
      } else {
        return SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_BLUR_IMAGE_EN;
      }
    } else {
      return SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_IMAGE_EN + StringUtils
        .join(imagePredictionList, Constants.COMMA);
    }
  }

  private String getImageReasonIN(List<String> imagePredictionList) {
    if (imagePredictionList.contains(Constants.BLUR)) {
      if (imagePredictionList.size() > 1) {
        imagePredictionList.remove(Constants.BLUR);
        return SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_BLUR_IMAGE_IN
          + SaveHistoryConstants.AUTO_QC_NEED_REVISION_POST_MESSAGE_FOR_MULTIPLE_PREDICTION_IN
          + StringUtils.join(imagePredictionList, Constants.COMMA);
      } else {
        return SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_BLUR_IMAGE_IN;
      }
    } else {
      return SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_IMAGE_IN + StringUtils
        .join(imagePredictionList, Constants.COMMA);
    }
  }

  @Override
  public void publishRevisedEvent(String storeId, String productCode) throws Exception {
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    Boolean trustedSeller = Optional.ofNullable(
        businessPartnerRepository.filterDetailByBusinessPartnerCode(
          productCollection.getBusinessPartnerCode())).map(ProfileResponse::isTrustedSeller)
      .orElse(false);
    ProductBusinessPartner productBusinessPartner = productBusinessPartnerService
        .findFirstByStoreIdAndProductId(productCollection.getStoreId(), productCollection.getProductId());
    productPublisherService.publishRevisedProductToPDT(
        ConverterUtil.toAddRevisedProductToPDTEvent(productCollection,
            productCollection.getUpdatedBy(), trustedSeller, productBusinessPartner, false, null,
            priceInfoVendorRevisedEnabled, priceInfoMaxVariantLimit));
  }

  @Override
  public void updateReviewPending(String storeId, String productCode, boolean reviewPending) {
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (Objects.isNull(productCollection)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.PRODUCT_NOT_FOUND);
    }
    productCollection.setReviewPending(reviewPending);
    productCollection.setUpdatedDate(new Date());
    productService.saveProductCollection(productCollection);
    productService.updateSolrProductCollection(productCollection);
  }

  @Override
  public void updateActivatedAndViewable(String storeId, String productCode, boolean activated, boolean viewable) {
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (Objects.isNull(productCollection)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.PRODUCT_NOT_FOUND);
    }
    productCollection.setActivated(activated);
    productCollection.setViewable(viewable);
    productService.saveProductCollection(productCollection);
    productService.updateSolrProductCollection(productCollection);
  }

  @Override
  public void processImageQcForBacklogProducts(String storeId, ImageQcResponseDomainEvent imageQcResponseDomainEvent)
      throws Exception {
    if (Objects.nonNull(imageQcResponseDomainEvent)) {
      ProductImageQcBacklog productImageQcBacklog = productImageQcBacklogService
          .findByStoreIdAndProductCodeAndStatus(storeId,
              imageQcResponseDomainEvent.getProductCode().replace(Constants.BACKLOG, StringUtils.EMPTY),
              ImageQcStatus.IN_PROGRESS.getImageQcStatus());
      computeImageQcData(storeId, productImageQcBacklog, imageQcResponseDomainEvent);
      productImageQcBacklogService.saveProductImageQcBacklog(productImageQcBacklog);
    }
  }

  private void computeImageQcData(String storeId, ProductImageQcBacklog productImageQcBacklog,
      ImageQcResponseDomainEvent imageQcResponse) throws JsonProcessingException {
    String imageViolations = productService.processImageQcForBacklogProducts(storeId, imageQcResponse);
    productImageQcBacklog.setImageQcResponse(objectMapper.writeValueAsString(imageQcResponse));
    productImageQcBacklog.setImageViolations(imageViolations);
    productImageQcBacklog.setStatus(ImageQcStatus.COMPLETED.getImageQcStatus());
  }

  @Override
  public ImageQcRequestDomainEvent getImageQcRequestDomainEvent(String productCode) throws Exception {
    ProductDetailResponse productDetailResponse = productService.findProductDetailByProductCode(productCode, false);
    List<ImageQcHashCodeAndLocationPathRequest> imageQcHashCodeAndLocationPathRequestList = new ArrayList<>();
    if (Objects.nonNull(productDetailResponse)) {
      imageQcHashCodeAndLocationPathRequestList =
          productDetailResponse.getImages().stream().filter(image -> !image.isMarkForDelete())
              .map(this::getImageQcRequest).collect(Collectors.toList());
    }
    return getImageQcRequestDomainEvent(productCode, imageQcHashCodeAndLocationPathRequestList);
  }

  private ImageQcRequestDomainEvent getImageQcRequestDomainEvent(String productCode,
      List<ImageQcHashCodeAndLocationPathRequest> imageQcHashCodeAndLocationPathRequestList) {
    ImageQcRequestDomainEvent imageQcRequestDomainEvent = new ImageQcRequestDomainEvent();
    imageQcRequestDomainEvent.setImages(imageQcHashCodeAndLocationPathRequestList);
    imageQcRequestDomainEvent.setProductCode(productCode);
    return imageQcRequestDomainEvent;
  }

  private ImageQcHashCodeAndLocationPathRequest getImageQcRequest(Image image) {
    ImageQcHashCodeAndLocationPathRequest imageQcHashCodeAndLocationPathRequest =
        new ImageQcHashCodeAndLocationPathRequest();
    String imagePathPrefix = image.isActive() ?
      fileStorageService.generateFinalImageFullPath(image.getLocationPath()) :
      image.getLocationPath().contains(fileStorageService.getImagePathPrefix()) ?
        fileStorageService.getCompleteSourceUrlPrefix() :
        imageSourceDirectory;
    if (!image.getLocationPath().startsWith(Constants.DELIMITER_SLASH)) {
      imagePathPrefix = imagePathPrefix.concat(Constants.DELIMITER_SLASH);
    }
    imageQcHashCodeAndLocationPathRequest.setLocationPath(imagePathPrefix.concat(image.getLocationPath()));
    imageQcHashCodeAndLocationPathRequest.setHashCode(image.getHashCode());
    return imageQcHashCodeAndLocationPathRequest;
  }

  @Override
  public void deleteTerminatedSellerNonSharedProducts(String storeId, String productSku) {
    try {
      ProductCollection productCollection = productService.getProductCollectionByProductSku(productSku);
      GdnPreconditions.checkArgument(Objects.nonNull(productCollection), ErrorMessages.PRODUCT_COLLECTION_NOT_FOUND);

      List<ProductBusinessPartner> productBusinessPartnerList =
          productBusinessPartnerService.findByStoreIdAndProductId(storeId, productCollection.getProductId());
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productBusinessPartnerList),
          String.format(ErrorMessages.PRODUCT_BUSINESS_PARTNER_NOT_FOUND, productCollection.getProductId()));

      if (productBusinessPartnerList.size() > 1) {
        log.info("Product trying to delete is a shared product productSku: {} ", productSku);
      } else {
        if (productCollection.isReviewPending()) {
          productDistributionService.removeProductFromPDT(GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(),
          new RemoveProductRequest(productCollection.getProductCode(), WorkflowState.REJECTED.toString()));
        }
        productRepository.discardProduct(
            ConverterUtil.getProductRequestForProductDiscard(productCollection.getCreatedBy(),
                productCollection.getCreatedDate(), productCollection.getProductId()));
      }
    } catch (Exception e) {
      log.error("Error while deleteTerminatedSellerNonSharedProducts storeId : {}, productSku : {} ", storeId,
          productSku, e);
    }
  }

  @Async
  @Override
  public void getStuckProductCodeAndState(int retryBatchSizeCount)
      throws Exception {
    StuckProductResponse stuckProductResponse = productWfService.getStuckProducts(retryBatchSizeCount, retryTimeSpan);
    if (Objects.nonNull(stuckProductResponse) && CollectionUtils.isNotEmpty(
        stuckProductResponse.getProductWfStateResponseList())) {
      for (ProductWfStateResponse productWfStateResponse : stuckProductResponse.getProductWfStateResponseList()) {
        StuckProductEventPublishDto stuckProductEventPublishDto =
            productService.getPDTEventDomainModel(productWfStateResponse);
        if (Objects.nonNull(stuckProductEventPublishDto.getAddRevisedProductToPDTEvent())) {
          productPublisherService.publishRevisedProductToPDT(
              stuckProductEventPublishDto.getAddRevisedProductToPDTEvent());
          log.info("[Retry-product-activation] Publish revised event for productCode : {}",
              stuckProductEventPublishDto.getAddRevisedProductToPDTEvent().getProductCode());
        } else if (Objects.nonNull(stuckProductEventPublishDto.getEditedPublishProductCollection())) {
          productService.publishAddEditedProductToPDTEvent(
              stuckProductEventPublishDto.getEditedPublishProductCollection().getStoreId(),
              stuckProductEventPublishDto.getEditedPublishProductCollection().getReviewType(),
              stuckProductEventPublishDto.getEditedPublishProductCollection(), null);
          log.info("[Retry-product-activation] Publish edited event for productCode : {}",
              stuckProductEventPublishDto.getEditedPublishProductCollection().getProductCode());
        } else if (Objects.nonNull(stuckProductEventPublishDto.getProductCollection())) {
          productPublisherService.publish(stuckProductEventPublishDto.getProductCollection().getProductCode(),
              stuckProductEventPublishDto.getProductCollection().getBusinessPartnerCode(),
              stuckProductEventPublishDto.getProductCollection().getBusinessPartnerName(),
              stuckProductEventPublishDto.getProductCollection().getUpdatedBy(),
              stuckProductEventPublishDto.getProductCollection().isPostLive(),
              stuckProductEventPublishDto.getProductCollection().isRestrictedKeywordsPresent(),
              stuckProductEventPublishDto.getProductCollection().getRestrictedKeywordsDetected(),
              stuckProductEventPublishDto.getProductCollection().getPrioritySeller(),
              stuckProductEventPublishDto.isTrustedSeller(),
              stuckProductEventPublishDto.getProductCollection().getProductId(), null);
          log.info("[Retry-product-activation] Publish screening approval event for productCode : {}",
              stuckProductEventPublishDto.getProductCollection().getProductCode());
        } else if (Objects.nonNull(stuckProductEventPublishDto.getVendorApprovalProductCollection())) {
          productStatusPublisherService.publishVendorApprovedEventToPBP(
              stuckProductEventPublishDto.getVendorApprovalProductCollection());
          log.info("[Retry-product-activation] Publish vendor approval event for productCode : {}",
              stuckProductEventPublishDto.getVendorApprovalProductCollection().getProductCode());
        }
      }
    }
    LOGGER.info("PBP API of get stuck products is finished");
  }

  @Override
  public AutoApprovalTypeResponse findAutoApprovalTypeByRequest(String storeId, String username, String productCode,
      boolean onlyCategoryChange, AutoApprovalTypeRequest autoApprovalTypeRequest) throws Exception {
    AutoApprovalTypeResponse autoApprovalTypeResponse = new AutoApprovalTypeResponse();
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    ProfileResponse profileResponse =
        businessPartnerRepository.filterDetailByBusinessPartnerCode(productCollection.getBusinessPartnerCode());
    if (StringUtils.isNotEmpty(autoApprovalTypeRequest.getDestinationCategoryCode())) {
      //If destination category is not empty, we will update product's category and then check for auto approval rules
      productCollection = categoryChangeInPbpPcbAndHistoryInCreationFlow(new RestrictedKeywordsByFieldAndActionType(),
          productCollection, storeId, autoApprovalTypeRequest.getDestinationCategoryCode(), true, profileResponse);
      autoApprovalTypeResponse.setCategoryCode(productCollection.getCategoryCode());
      autoApprovalTypeResponse.setCategoryName(productCollection.getCategoryName());
    }
    if (onlyCategoryChange) {
      autoApprovalTypeResponse.setAutoApprovalType(AutoApprovalType.NA.name());
    } else {
      AutoApprovalType autoApprovalType =
          productService.findAutoApprovalTypeByRequest(storeId, username, productCode, autoApprovalTypeRequest,
              profileResponse, productCollection);
      autoApprovalTypeResponse.setAutoApprovalType(autoApprovalType.name());
    }
    return autoApprovalTypeResponse;
  }

  @Override
  public void publishEditedImageResizeEvent(String productCode,
      EditedResizeAndImagesUpdateStatusResponse editedResizeAndImagesUpdateStatusResponse) {
    if (Objects.nonNull(editedResizeAndImagesUpdateStatusResponse) && CollectionUtils.isNotEmpty(
        editedResizeAndImagesUpdateStatusResponse.getEditedImages())) {
      productPublisherService.publishEditImageResizeEvent(
          new EditedImageResizeEvent(productCode, Constants.DEFAULT_STORE_ID,
              editedResizeAndImagesUpdateStatusResponse.getEditedImages()));
    }
  }

  @Override
  public void processProductVendorSearchAutoHeal(String storeId, String productCode) throws Exception {
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (productCollection.isReviewPending() && Arrays.asList(productStateEligibleToAddToVendor.split(Constants.COMMA))
        .contains(productCollection.getState())) {
      processProductEligibleToBeAddedToVendor(productCollection);
    } else if (!productCollection.isReviewPending() && Constants.ACTIVE.equals(productCollection.getState())) {
      processProductEligibleToBeActivated(productCollection);
    }
  }

  private void processProductEligibleToBeAddedToVendor(ProductCollection productCollection) throws Exception {
    log.info("Product is eligible to event publish to PDT. productCode : {} ", productCollection.getProductCode());
    productService.publishToPDTByProductCollection(productCollection);
  }

  private void processProductEligibleToBeActivated(ProductCollection productCollection) throws Exception {
    List<ProductBusinessPartner> productBusinessPartners =
        productBusinessPartnerService.findByStoreIdAndProductId(productCollection.getStoreId(),
            productCollection.getProductId());
    for (ProductBusinessPartner productBusinessPartner : productBusinessPartners) {
      BasicProductResponse basicProductResponse =
          xProductOutbound.getBasicProductInfoV2(productBusinessPartner.getGdnProductSku());
      if (Objects.isNull(basicProductResponse)) {
        log.info("Create new L3/L4 for product : {} ", productCollection.getProductCode());
        this.productBusinessPartnerService.retryCreate(productBusinessPartner.getStoreId(),
            productBusinessPartner.getId(), null);
        kafkaProducer.send(com.gdn.mta.domain.event.config.DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY,
            productCollection.getProductCode(),
            ConverterUtil.convertToProductDataAutoFixHistoryListRequest(productCollection.getProductCode(),
                EditedReviewTypeConstants.RETRY_BEFORE_APPROVAL_TYPE, StringUtils.EMPTY));
      } else if (basicProductResponse.isForceReview() || basicProductResponse.isTakenDown()) {
        log.info("Reactivating product : {} ", productCollection.getProductCode());
        ProfileResponse profileResponse =
            businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
        productLevel3ServiceV1.activateProductOnNeedCorrection(productBusinessPartner.getStoreId(),
            productBusinessPartner.getGdnProductSku(), profileResponse, new ArrayList<>());
        kafkaProducer.send(com.gdn.mta.domain.event.config.DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY,
            productCollection.getProductCode(),
            ConverterUtil.convertToProductDataAutoFixHistoryListRequest(productCollection.getProductCode(),
                EditedReviewTypeConstants.RETRY_X_PRODUCT_ACTIVATION_BEFORE_APPROVAL, StringUtils.EMPTY));
      } else {
        log.info("Product is fully activated no action to be taken productCode : {} ",
            productCollection.getProductCode());
      }
    }
  }

  public void terminatedSellerSkuCleanup(String productCode, String sellerCode) throws Exception {
    boolean productDeletedFromDB = false;
    boolean productDeletedFromSolr = false;
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            productCode);
    if (Objects.isNull(productCollection)) {
      publishTerminatedSellerSkuCleanupStatusEvent(productCode, sellerCode, TerminatedSellerSkuStatus.SUCCESS.name());
      return;
    }
    Date pickedForDeletionTimeStamp = Optional.ofNullable(productCollection.getUpdatedDate()).orElse(new Date());
    if (productCollection.getPickedForDeletion() && checkPickedForDeletionTimeStamp(pickedForDeletionTimeStamp)) {
      log.info("Deletion for productCode {} is already in progress", productCollection.getProductCode());
      return;
    }
    try {
      // sku cleanup from DBs
      productCollection = updatePickedForDeletionInProductCollection(productCollection, true);
      productDeletedFromDB = terminatedSellerSkuCleanupServiceBean.terminatedSellerSkuCleanup(productCollection);
      //sku cleanup from Solr
      if (productDeletedFromDB) {
        productDeletedFromSolr = terminatedSellerSkuCleanupFromSolr(productCollection.getId());
      }
      if (productDeletedFromDB && productDeletedFromSolr) {
        log.info("product deleted successfully for terminated seller {} and productCode {} ", sellerCode, productCode);
        publishTerminatedSellerSkuCleanupStatusEvent(productCode, sellerCode, TerminatedSellerSkuStatus.SUCCESS.name());
      } else {
        log.info("product deletion failed for terminated seller {} and productCode {} ", sellerCode, productCode);
        publishTerminatedSellerSkuCleanupStatusEvent(productCode, sellerCode, TerminatedSellerSkuStatus.FAILED.name());
      }
    } catch (Exception e) {
      log.error("Error while deleting terminated seller product where productCode : {} , sellerCode : {} ", productCode,
          sellerCode, e);
      publishTerminatedSellerSkuCleanupStatusEvent(productCode, sellerCode, TerminatedSellerSkuStatus.FAILED.name());
    }
  }

  public void updateProductBrandValue(String storeId,
      ProductBrandUpdateRequest productBrandUpdateRequest) throws Exception {
    List<ProductBusinessPartner> productBusinessPartners =
        productBusinessPartnerService.findByProductCode(productBrandUpdateRequest.getProductCode());
    Set<String> businessPartnerCodes =
        productBusinessPartners.stream().map(ProductBusinessPartner::getBusinessPartnerId)
            .collect(Collectors.toSet());
    productBrandUpdateRequest.setBusinessPartnerCodes(businessPartnerCodes);
    ProductBrandUpdateResponse productBrandUpdateResponse =
        productOutbound.updateProductBrandValue(productBrandUpdateRequest);
    productBrandUpdateRequest.setNewBrandName(productBrandUpdateResponse.getBrandName());
    productBrandUpdateRequest.setNewBrandCode(productBrandUpdateResponse.getBrandCode());
    Pair<ProductCollection, String> updatedProductAndOldBrandPair =
        productService.updateBrandData(storeId, productBrandUpdateRequest, productBusinessPartners);
    productService.updateSolrProductCollection(updatedProductAndOldBrandPair.getLeft());
    xProductOutbound.generateProductScoreByProductSkuOrProductCode(null,
        productBrandUpdateRequest.getProductCode(), false);
    if (updatedProductAndOldBrandPair.getLeft().isReviewPending()) {
      //Update pdt
      productDistributionTaskRepositoryBean.updateProductBrand(
          ChangeBrandRequest.builder().productCode(productBrandUpdateRequest.getProductCode())
              .brandCode(productBrandUpdateRequest.getNewBrandCode())
              .brandName(productBrandUpdateRequest.getNewBrandName()).build());
      log.info("Brand successfully updated in PDT for productCode = {} ",
          productBrandUpdateRequest.getProductCode());
    }
    if (!updatedProductAndOldBrandPair.getRight()
        .equals(productBrandUpdateRequest.getNewBrandName())) {
      publishInternalHistoryForBrandUpdate(storeId, productBrandUpdateRequest.getProductCode(),
          updatedProductAndOldBrandPair.getRight(), productBrandUpdateRequest.getNewBrandName());
    }
  }

  private void publishInternalHistoryForBrandUpdate(String storeId, String productCode,
      String oldBrandName, String updatedBrandName) {
    InternalProductHistoryEventModel internalProductHistoryEventModel =
        new InternalProductHistoryEventModel();
    internalProductHistoryEventModel.setStoreId(storeId);
    internalProductHistoryEventModel.setProductCode(productCode);
    internalProductHistoryEventModel.setActivity(ProductWorkflowLookup.STATE_EDIT_DESCRIPTION);
    internalProductHistoryEventModel.setUsername(mandatoryParameterHelper.getUsername());
    List<ProductFieldHistory> productFieldHistoryList = new ArrayList<>();
    productFieldHistoryList.add(
        new ProductFieldHistory(SaveHistoryConstants.BRAND_FIELD, oldBrandName, updatedBrandName));
    internalProductHistoryEventModel.setNotes(String.valueOf(productFieldHistoryList));
    kafkaProducer.send(PRODUCT_INTERNAL_HISTORY_SAVE, productCode,
        internalProductHistoryEventModel);
  }

  private boolean terminatedSellerSkuCleanupFromSolr(String id) {
    try {
      solrActiveProductCollectionService.deleteSolrProductCollectionDocument(id);
      solrReviewProductCollectionService.deleteProductFromReviewProductCollection(id);
      return true;
    } catch (Exception e) {
      log.error("Error while deleting terminated seller product from solr where Id : {} ", id, e);
      return false;
    }
  }

  private void publishTerminatedSellerSkuCleanupStatusEvent(String productCode, String sellerCode, String status) {
    TerminatedSellerSkuCleanupStatusEventModel terminatedSellerSkuCleanupStatusEventModel =
        TerminatedSellerSkuCleanupStatusEventModel.builder().productCode(productCode).sellerCode(sellerCode)
            .service(terminatedSellerSkuCleanupServiceName).result(status).build();
    log.info("Publishing event topic : {} , payload : {} ", kafkaTopicProperties.getTerminatedSellerSkuCleanupStatus(),
        terminatedSellerSkuCleanupStatusEventModel);
    kafkaProducer.send(kafkaTopicProperties.getTerminatedSellerSkuCleanupStatus(),
        terminatedSellerSkuCleanupStatusEventModel.getProductCode(), terminatedSellerSkuCleanupStatusEventModel);
  }

  private boolean checkPickedForDeletionTimeStamp (Date pickedForDeletionTimeStamp) {
    long timeDifference = new Date().getTime() - pickedForDeletionTimeStamp.getTime();
    long minutesDifference = timeDifference / (60 * 1000);
    return minutesDifference < terminatedSellerSkuPickedForDeletionThresholdInMinutes;
  }

  private ProductCollection updatePickedForDeletionInProductCollection(ProductCollection productCollection,
      boolean pickedForDeletion) {
    productCollection.setPickedForDeletion(pickedForDeletion);
    return productService.saveProductCollection(productCollection);
  }

}
