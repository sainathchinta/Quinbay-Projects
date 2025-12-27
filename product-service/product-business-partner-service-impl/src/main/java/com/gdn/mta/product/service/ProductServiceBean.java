package com.gdn.mta.product.service;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.x.product.domain.event.config.ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gda.mta.product.dto.DistributionInfoRequest;
import com.gda.mta.product.dto.DistributionItemRequest;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.MasterProductEditDTO;
import com.gda.mta.product.dto.NeedRevisionProductsRequest;
import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gda.mta.product.dto.ProductItemDistributionInfoRequest;
import com.gda.mta.product.dto.ProductBrandUpdateRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.response.AgpSimpleQueryResponse;
import com.gda.mta.product.dto.response.CategoryModelPredictionResponse;
import com.gda.mta.product.dto.response.InternalProductHistoryEventModel;
import com.gda.mta.product.dto.response.OmniChannelSkuResponse;
import com.gda.mta.product.dto.response.ProductAndBrandResponse;
import com.gda.mta.product.dto.response.ProductCodeAndNameDetails;
import com.gda.mta.product.dto.response.ProductMigrationRequest;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.domain.event.modal.XProdAttributeMigrationEventModel;
import com.gdn.mta.product.enums.AddDeleteVariantStatus;
import com.gdn.mta.product.enums.L3InfoUpdateChangeType;
import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.product.service.config.PreOrderConfig;
import com.gdn.mta.product.service.solr.SolrActiveProductCollectionService;
import com.gdn.partners.pbp.helper.RequestHelper;
import com.gdn.partners.pbp.helper.ResponseHelper;
import com.gdn.partners.pbp.outbound.AGPQuery.AGPQueryFeign;
import com.gdn.partners.pbp.outbound.xProduct.feign.ProductBasicMasterFieldsRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import org.apache.commons.collections.map.HashedMap;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.common.SolrException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.AutoApprovalsDetailDto;
import com.gda.mta.product.dto.AutoNeedRevisionAndForceReviewResponse;
import com.gda.mta.product.dto.DalamProductListRequest;
import com.gda.mta.product.dto.DimensionRefreshRequest;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.ImageQcProcessedResponse;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.PredictionScoreAndViolationsDto;
import com.gda.mta.product.dto.ProductBusinessPartnerAttributeResponse;
import com.gda.mta.product.dto.ProductBusinessPartnerResponse;
import com.gda.mta.product.dto.ProductCollectionElement;
import com.gda.mta.product.dto.ProductCollectionResponse;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductDetailCompleteResponse;
import com.gda.mta.product.dto.ProductFilterRequest;
import com.gda.mta.product.dto.ProductItemBusinessPartnerResponse;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gda.mta.product.dto.RestrictedKeywordsByFieldAndActionType;
import com.gda.mta.product.dto.ValidateDuplicateProductRequest;
import com.gda.mta.product.dto.ValidateDuplicateProductResponse;
import com.gda.mta.product.dto.generator.ProductWfStateResponse;
import com.gda.mta.product.dto.response.AuditTrailListRequest;
import com.gda.mta.product.dto.response.AutoApprovalRuleDetailsDto;
import com.gda.mta.product.dto.response.BrandPredictionResponse;
import com.gda.mta.product.dto.response.BrandRestrictedModelsResponse;
import com.gda.mta.product.dto.response.BrandAndCategoryTakeDownResponse;
import com.gda.mta.product.dto.response.CogsValueResponse;
import com.gda.mta.product.dto.response.ImageQcEnableAndSyncResponse;
import com.gda.mta.product.dto.response.ImageQcPredictionResponse;
import com.gda.mta.product.dto.response.ImageQcResponse;
import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gda.mta.product.dto.response.InProgressProductResponsePageResponse;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.KeywordRecommendationsResponse;
import com.gda.mta.product.dto.response.KeywordRestrictionModelsResponse;
import com.gda.mta.product.dto.response.PreOrderResponse;
import com.gda.mta.product.dto.response.ProductFilterResponse;
import com.gda.mta.product.dto.response.RestrictedKeywordsByFieldResponse;
import com.gda.mta.product.dto.response.RestrictionModelsResponse;
import com.gda.mta.product.dto.response.SimpleStringResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.domain.event.modal.SolrProductCollectionUpdateEvent;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.micro.graphics.domain.event.model.ScaleImageResponse;
import com.gdn.mta.domain.event.config.ApproveImageMessageConstants;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AddProductToVendorCombinedEventModel;
import com.gdn.mta.domain.event.modal.ApproveProductResponse;
import com.gdn.mta.domain.event.modal.AutoNeedRevisionDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcProcessedResponseDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcRequestDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcResponseDomainEvent;
import com.gdn.mta.domain.event.modal.ProcessImageDomainEvent;
import com.gdn.mta.domain.event.modal.ProductActionRetryEvent;
import com.gdn.mta.domain.event.modal.ProductStatusDomainEvent;
import com.gdn.mta.domain.event.modal.ProductWipDeleteResponse;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionDeleteEvent;
import com.gdn.mta.domain.event.modal.StuckProductEventPublishDto;
import com.gdn.mta.domain.event.modal.UserFeedbackImageQcListResponse;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.AutoApprovalRules;
import com.gdn.mta.product.entity.ItemFlagDetails;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductFieldHistory;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductImagePrediction;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductMigration;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.enums.RestrictedKeywordActionType;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.CategoryRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductDistributionTaskRepositoryBean;
import com.gdn.mta.product.repository.ProductHistoryRepository;
import com.gdn.mta.product.repository.ProductItemBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductItemRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.ProductWorkflowRepository;
import com.gdn.mta.product.repository.SequenceRepository;
import com.gdn.mta.product.repository.SolrActiveProductCollectionRepository;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.generator.GeneratorService;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.mta.product.util.BarcodeGenerator;
import com.gdn.mta.product.util.BarcodeValidator;
import com.gdn.mta.product.util.BaseGenerator;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.util.ProductChangeUtil;
import com.gdn.mta.product.util.ProductContentUtil;
import com.gdn.mta.product.util.ProductWorkflowLookup;
import com.gdn.mta.product.valueobject.BasicProductDetail;
import com.gdn.mta.product.valueobject.SimpleMasterProductUpdateRequestDTO;
import com.gdn.mta.product.valueobject.SolrCategoryCodeDTO;
import com.gdn.mta.product.valueobject.SolrProductCodeDTO;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.entity.mailEvent.CategoryChangeMailEvent;
import com.gdn.partners.pbp.entity.workflow.product.ProductWorkflowStatus;
import com.gdn.partners.pbp.model.productlevel3.ProductCollectionCountRequest;
import com.gdn.partners.pbp.model.productlevel3.ProductCollectionCountResponse;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.outbound.pickuppoint.PickupPointOutbound;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.partners.pbp.outbound.productAnalytics.ProductAnalyticsOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.outbound.xbp.feign.XbpFeign;
import com.gdn.partners.pbp.service.notification.ProductNotificationService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Helper;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3InventoryService;
import com.gdn.partners.pbp.workflow.WorkflowProcessCode;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import com.gdn.x.businesspartner.dto.BusinessPartnerCodesRequest;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.entity.Profile;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.ProductSpecialAttributeDTO;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuAndProductCodeRequest;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.DuplicateProductDetailsResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.MinMaxItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.brand.ProductBrandValidationRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import com.gdn.x.productcategorybase.dto.response.AttributeHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailMapResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductPredictionCategoryMappingResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsMappedToCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Lazy
@Service
@Transactional(readOnly = true, rollbackFor = Exception.class)
@Slf4j
public class ProductServiceBean implements ProductService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductServiceBean.class);
  private static final Integer DEFAULT_PAGE = 0;
  private static final Integer DEFAULT_SIZE = 1000;
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "INTERNAL";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "INTERNAL";
  public  static final String STATE_IN_PROGRESS = "IN_PROGRESS";
  public  static final String STATE_DRAFT = "DRAFT";
  public  static final String EXTERNAL = "EXTERNAL";
  private static final String STATE_DELETED = "DELETED";
  public  static final int PARTITION_SIZE = 145;
  private static final String PRODUCT_COLLECTION_DATA_NOT_FOUND = "Product collection data is not found for: ";
  private static final String PRODUCT_IS_NOT_ACTIVE = "Product collection is not active for: ";
  private static final String SOLR_UPDATE_DONE_FOR_PRODUCT_COLLECTION =
      "Product collection update in Solr is successful, product-code :{}";
  private static final String CHANGED_FIELDS_FETCH_ERROR =
      "Exception Occurred while getting product field difference on updating, continue with process";
  private static final String REVIEW_CONFIG_CHANGE_DESCRIPTION = "Image resizing failed, so product is made pre-live";
  private static final String SIMPLE_PRODUCT_UPDATE_IN_PCB = "Product update in PCB successful product-code :{}";
  private static final String ERROR_IN_SHIPPING_WEIGHT_CALCULATION = "Error in shipping weight calculation";
  private static final String CHECK_IF_PRODUCT_IS_IN_DRAFT_STATE= "Check if product is in product draft state";
  private static final String FAILED_TO_ASSIGN_PRODUCTS= "Failed to assign products";
  private static final String FAILED_TO_UN_ASSIGN_PRODUCTS= "Failed to un assign products";
  private static final String STATE = "IN_PROGRESS";
  private static final String POST_LIVE = "Post-Live";
  private static final ImmutableList<String> NOT_IN_PROGRESS_STATE =
      ImmutableList.of("NEED_CORRECTION", "DRAFT");
  private static final String NOT_A_VALID_TYPE = "Not a valid time filter type";
  private static final int HISTORY_NOTES_SIZE = 10000;
  private static final ImmutableList<String> IN_PROGRESS_STATE =
      ImmutableList.of("DRAFT", "IN_PROGRESS", "NEED_CORRECTION");

  public static final String INVALID_DETECTION = "invalid-detection";
  public static final String NOT_SURE = "not-sure";
  public static final int SKIP_ALL_ACTIONS = -1;
  private static final String SYSTEM_REVIEW = "system_review";
  private static final String ACTIVE = "ACTIVE";
  private static final String NEED_CORRECTION = "NEED_CORRECTION";
  private static final String IN_PROGRESS = "IN_PROGRESS";
  public static final String PRODUCT_ATTRIBUTE_UPDATE = "PRODUCT_ATTRIBUTE_UPDATE";


  @Value("${product.visibility.switch.enabled}")
  private boolean isProductVisibilityEnabled;

  @Value("${image.source.directory}")
  private String imageSourceDirectory;

  @Value("${product.status.event.enabled}")
  private boolean productStatusEventEnabled;

  @Value("${validate.pickup.points}")
  private boolean validatePickupPoints;

  @Value("${validate.create.request.for.fbb}")
  private boolean validateCreateRequestForFbb;

  @Value("${enable.price.update.in.image.qc.request}")
  private boolean enablePriceUpdateInImageQcRequest;

  @Value("${enable.keywords.action.in.image.qc}")
  private boolean enableKeywordsActionInImageQcRequest;

  @Value("${auto.approval.content.image.violation}")
  private boolean autoApprovalContentImageViolation;

  @Value("${product.history.update.event}")
  private boolean productHistoryUpdateThroughEvent;

  @Value("${bp.bopis.restriction.enabled}")
  private boolean bpBopisRestrictionEnabled;

  @Value("${override.restricted.keywords.flag.by.ds.response}")
  private boolean overrideRestrictedKeywordsFlagByDsResponse;

  @Value("${category.prediction.enabled}")
  private boolean categoryPredictionEnabled;

  @Value("${restricted.keyword.not.sure.enabled}")
  private boolean restrictedKeywordNotSureEnabled;

  @Value("${price.info.vendor.revised.enabled}")
  private boolean priceInfoVendorRevisedEnabled;

  @Value("${price.info.max.variant.limit}")
  private int priceInfoMaxVariantLimit;

  @Value("${price.info.vendor.edited.enabled}")
  private boolean priceInfoVendorEditedEnabled;

  @Value("${bopis.category.validation.for.merchant.types}")
  private String bopisUnsupportedMerchantTypes;

  @Value("${bopis.cnc.restriction.feature.switch}")
  private boolean bopisCNCRestrictionEnabled;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${size.chart.addition.for.product}")
  private boolean sizeChartAdditionForProduct;

  @Value("${merchant.type.to.skip.brand.model.check}")
  private String merchantTypeToSkipBrandModelCheck;

  @Value("${product.image.prediction.dirty.checking.fix.enabled}")
  private boolean productImagePredictionDirtyCheckingFixEnabled;

  @Value("${enable.21.plus.model}")
  private boolean enable21PlusModel;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Value("${distribution.seller.list}")
  private Set<String> distributionSellerList;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductItemRepository productItemRepository;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private ProductWorkflowRepository productWorkflowRepository;

  @Autowired
  private ProductHistoryRepository productHistoryRepository;

  @Autowired
  private SequenceRepository sequenceRepository;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private CategoryRepository categoryRepository;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private ProductWorkflowService productWorkflowService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private SolrActiveProductCollectionRepository solrActiveProductCollectionRepository;

  @Autowired
  private SolrActiveProductCollectionService solrActiveProductCollectionService;

  @Autowired
  private ProductChangeUtil productChangeUtil;

  @Autowired
  private ProductGdnSkuGeneratorService productGdnSkuGeneratorService;

  @Autowired
  private ProductPublisherService productPublisherService;

  @Autowired
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Autowired
  private ProductNotificationService productNotificationService;

  @Autowired
  private ApplicationProperties applicationProperties;

  @Autowired
  private ProductStatusPublisherService productStatusPublisherService;

  @Autowired
  private ProductWfService productWfService;

  @Autowired
  private GeneratorService generatorService;

  @Autowired
  private ProductMailEventService productMailEventService;

  @Autowired
  private ProductLevel3AggregatorService productLevel3AggregatorService;

  @Autowired
  ProductDistributionTaskRepositoryBean productDistributionTaskRepositoryBean;

  @Autowired
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Lazy
  @Autowired
  private ProductImagePredictionService productImagePredictionService;

  @Autowired
  private ProductImageQcProcessingResponseService productImageQcProcessingResponseService;

  @Autowired
  private ProductItemBusinessPartnerRepository productItemBusinessPartnerRepository;

  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private DistributionInfoServiceBean distributionInfoServiceBean;

  @Lazy
  @Autowired
  private AutoApprovalService autoApprovalService;

  @Autowired
  private ProductLevel3RetryService productLevel3RetryService;

  @Autowired
  private ProductLevel3Helper productLevel3Helper;

  @Autowired
  private ProductAnalyticsOutbound productAnalyticsOutbound;

  @Autowired
  private EmailNotificationService emailNotificationService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private PickupPointOutbound pickupPointOutbound;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private XbpFeign xbpFeign;

  @Autowired
  private ProductDistributionService productDistributionService;

  @Autowired
  private ProductLevel3V2Service productLevel3V2Service;

  @Autowired
  private AGPQueryFeign agpQueryFeign;

  @Autowired
  private PreOrderConfig preOrderConfig;

  @Autowired
  private ProductAppealService productAppealService;

  @Value("${phone.number.detection.regex}")
  private String phoneNumberDetectionRegex;

  @Value("${mpp.allowed.sellers}")
  private String mppAllowedSellers;

  @Value("${unset.viewable.in.pcb}")
  private boolean unsetViewableFlagInPCB;

  @Value("${mpp.for.wh.enabled}")
  private boolean mppForWhEnabled;

  @Value("${skip.definitive.action}")
  private boolean skipDefinitiveAction;

  @Value("${set.waiting.deletion.for.delete.pickup.point}")
  private boolean setWaitingDeletionForDeletePickupPoint;

  @Value("${override.force.review}")
  private boolean overrideForceReview;

  @Value("${l4.fetch.size.for.migration}")
  private int l4FetchSizeForMigration;

  @Value("${bopis.category.action.on.vendor.category.change.switch}")
  private boolean bopisCategoryActionOnVendorCategoryChangeSwitch;

  @Value("${fetch.nr.products.from.last.x.days}")
  private int fetchNRProductsFromLastXDays;

  @Value("${keyword.reset.on.multiple.destination.categories.enabled}")
  private boolean keywordResetOnMultipleDestinationCategoriesEnabled;

  @Value("${throw.exception.on.image.qc.failure}")
  private boolean throwExceptionOnImageQcFailure;

  @Value("${brand.category.edit.enabled.for.external}")
  private boolean brandCategoryEditEnabledForExternal;

  public ProductLevel3InventoryService getProductLevel3InventoryService() {
    return this.productLevel3InventoryService;
  }

  private void createProductLevel3(String storeId, ProductDetailResponse productData,
      boolean isSkipNotification) throws Exception {
    LOGGER.info(
        "creating productLevel3 requests for product and inventory services. storeId: {}, "
            + "productCode: {}", storeId, productData.getProductCode());
    List<ProductBusinessPartner> productBusinessPartners =
        this.productBusinessPartnerRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            storeId, productData.getId());
    for (ProductBusinessPartner productBusinessPartner : productBusinessPartners) {
      productBusinessPartner.setActivated(true);
      if (StringUtils.isEmpty(productBusinessPartner.getGdnProductSku()))
        this.productGdnSkuGeneratorService.generateGdnSkuOnProduct(productBusinessPartner, false);
      try {
        this.productLevel3Service.create(productBusinessPartner.getBusinessPartnerId(), productData,
            productBusinessPartner, true, new ArrayList<>());
        LOGGER.info("Setting state to active and mark for delete to true in createProductLevel3 for product sku : {}",
            productBusinessPartner.getGdnProductSku());
        if (isForceReview(storeId, productData.getProductCode())) {
          productBusinessPartner.setActivated(false);
        } else {
          productBusinessPartner.setMarkForDelete(true);
          productBusinessPartner.setState("ACTIVE");
        }
        if (!isSkipNotification) {
            CategoryChangeMailEvent categoryChangeMailEvent = this.productMailEventService
                .getCategoryChangeMailEvent(storeId, productBusinessPartner.getGdnProductSku());
          if (Objects.nonNull(categoryChangeMailEvent)) {
            this.productNotificationService.sendProductActiveNotification(productBusinessPartner.getBusinessPartnerId(),
                categoryChangeMailEvent, productBusinessPartner.getGdnProductSku(), productBusinessPartner.isBundleProduct());
          } else {
            this.productNotificationService.sendProductActiveNotification(productBusinessPartner.getBusinessPartnerId(),
                productBusinessPartner.getGdnProductSku(), productData.getName(), productBusinessPartner.isBundleProduct());
          }
        }
      } catch (Exception e) {
        this.productLevel3RetryService.upsertProductLevel3FailureLog(storeId, productBusinessPartner.getGdnProductSku());
        String generatedGdnSkus =
            this.productGdnSkuGeneratorService.convertToGeneratedGdnSkus(productBusinessPartner);
        ProductServiceBean.LOGGER.error(
            "error invoking create product level 3 at service. Business Partner Code : "
                + productBusinessPartner.getBusinessPartnerId() + " . Product Id : "
                + productBusinessPartner.getId() + " . Product Code : "
                + productData.getProductCode() + " Generated GDN SKU : " + generatedGdnSkus, e);
      }
    }
    this.productBusinessPartnerRepository.saveAll(productBusinessPartners);
  }

  private void generateProductCollection(String businessPartnerCode, String businessPartnerName,
      ProductDetailResponse productData, ProductCollection productCollection) throws Exception {
    List<CategoryResponse> categories = new ArrayList<CategoryResponse>();
    for (ProductCategoryResponse productCategory : productData.getProductCategoryResponses()) {
      if (!productCategory.isMarkForDelete()) {
        categories.add(productCategory.getCategory());
      }
    }
    productCollection.setStoreId(productData.getStoreId());
    productCollection.setProductId(productData.getId());
    productCollection.setProductCode(productData.getProductCode());
    productCollection.setProductName(productData.getName());
    productCollection.setBrand(productData.getBrand());
    productCollection.setCategoryCode(categories.get(0).getCategoryCode());
    productCollection.setCategoryName(categories.get(0).getName());
    productCollection.setBusinessPartnerCode(businessPartnerCode);
    productCollection.setBusinessPartnerName(businessPartnerName);
    productCollection.setActivated(productData.isActivated());
    productCollection.setViewable(productData.isViewable());
  }

  private void updateProductBusinessPartnerInformation(String storeId,
      ProductDetailResponse productData) throws Exception {
    List<ProductBusinessPartner> productBusinessPartners =
        this.productBusinessPartnerRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            storeId, productData.getId());
    List<CategoryResponse> categories = new ArrayList<CategoryResponse>();
    for (ProductCategoryResponse productCategory : productData.getProductCategoryResponses()) {
      if (!productCategory.isMarkForDelete()) {
        categories.add(productCategory.getCategory());
      }
    }
    for (ProductBusinessPartner productBusinessPartner : productBusinessPartners) {
      productBusinessPartner.setProductName(productData.getName());
      productBusinessPartner.setCategoryName(categories.get(0).getName());
      productBusinessPartner.setBrand(productData.getBrand());
    }
    this.productBusinessPartnerRepository.saveAll(productBusinessPartners);
  }

  /**
   * approve content
   * @param storeId store id
   * @param product product which content to be approved
   * @throws Exception when failed to approve content
   * @deprecated use {@link #approveContent(String, String, boolean, boolean)}
   */
  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Deprecated
  public void approvedContent(String storeId, Product product) throws Exception {
    Product savedProduct = this.productRepository.findOne(product.getId());
    if (savedProduct == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "at product, but try to approved content it with id : " + product.getId());
    }
    BeanUtils.copyProperties(product, savedProduct, "createdDate", "createdBy");
    boolean active = workflowForApprovedContent(storeId, savedProduct);
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId,
            savedProduct.getProductCode());
    generateProductCollection(productCollection.getBusinessPartnerCode(),
        productCollection.getBusinessPartnerName(), savedProduct, productCollection);
    if (active) {
      productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    }
    updateProductBusinessPartnerInformation(savedProduct);
    this.productCollectionRepository.save(productCollection);
    updateSolrProductCollectionDocument(productCollection);
    if (!savedProduct.isActivated() || !savedProduct.isViewable()) {
      this.productRepository.update(savedProduct, null, false, true, false, new ArrayList<>(), new HashedMap(),
          new ArrayList<>(), false, false, new ArrayList<>(), new ProductL3Response(), EditProductResponse.builder().build());
    }
  }

  @Deprecated
  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void approvedImage(String storeId, Product product) throws Exception {
    Product savedProduct = this.productRepository.findOne(product.getId());
    if (savedProduct == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "at product, but try to approved image it with id : " + product.getId());
    }
    BeanUtils.copyProperties(product, savedProduct, "createdDate", "createdBy");
    boolean active = workflowForApprovedImage(storeId, savedProduct);
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId,
            savedProduct.getProductCode());
    generateProductCollection(productCollection.getBusinessPartnerCode(),
        productCollection.getBusinessPartnerName(), savedProduct, productCollection);
    if (active) {
      productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    }
    updateProductBusinessPartnerInformation(savedProduct);
    this.productCollectionRepository.save(productCollection);
    updateSolrProductCollectionDocument(productCollection);
    if (!savedProduct.isActivated() || !savedProduct.isViewable()) {
      this.productRepository.update(savedProduct, null, false, true, false, new ArrayList<>(), new HashedMap(),
          new ArrayList<>(), false, false, new ArrayList<>(), new ProductL3Response(), EditProductResponse.builder().build());
    }
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public List<ProductWorkflow> checkProductWorkflowsAlreadyExist(Product product) throws Exception {
    List<ProductWorkflow> productWorkflows = null;
    List<ProductWorkflow> existingProductWorkflows =
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            product.getStoreId(), product.getId());
    if (existingProductWorkflows != null && !existingProductWorkflows.isEmpty())
      productWorkflows = existingProductWorkflows;
    else {
      List<ProductWorkflow> productWorkflowsForSave = new ArrayList<ProductWorkflow>();
      productWorkflowsForSave.add(new ProductWorkflow(product.getId(),
          ProductWorkflowLookup.STATE_REVIEW_CONTENT,
          ProductWorkflowLookup.STATE_REVIEW_CONTENT_DESCRIPTION, product.getUpdatedBy(), product
          .getUpdatedDate(), product.getStoreId()));
      productWorkflowsForSave.add(new ProductWorkflow(product.getId(),
          ProductWorkflowLookup.STATE_REVIEW_IMAGE,
          ProductWorkflowLookup.STATE_REVIEW_IMAGE_DESCRIPTION, product.getUpdatedBy(), product
          .getUpdatedDate(), product.getStoreId()));
      List<ProductHistory> productHistoriesForSave = new ArrayList<ProductHistory>();
      productHistoriesForSave.add(new ProductHistory(product.getId(),
          ProductWorkflowLookup.STATE_REVIEW_CONTENT,
          ProductWorkflowLookup.STATE_REVIEW_CONTENT_DESCRIPTION,
          ProductWorkflowLookup.STATE_DRAFT_TO_REVIEW_CONTENT_NOTES, product.getUpdatedBy(),
          product.getUpdatedDate(), product.getStoreId()));
      productHistoriesForSave.add(new ProductHistory(product.getId(),
          ProductWorkflowLookup.STATE_REVIEW_IMAGE,
          ProductWorkflowLookup.STATE_REVIEW_IMAGE_DESCRIPTION,
          ProductWorkflowLookup.STATE_DRAFT_TO_REVIEW_IMAGE_NOTES, product.getUpdatedBy(), product
          .getUpdatedDate(), product.getStoreId()));
      this.productWorkflowRepository.saveAll(productWorkflowsForSave);
      this.productWorkflowRepository.flush();
      this.productHistoryRepository.saveAll(productHistoriesForSave);
      this.productHistoryRepository.flush();
      productWorkflows =
          this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
              product.getStoreId(), product.getId());
    }
    return productWorkflows;

  }

  private void convertProductDetailResponseToProduct(ProductDetailResponse productDetailResponse,
      Product product) throws Exception {
    BeanUtils.copyProperties(productDetailResponse, product, "productItemResponses",
        "productAttributeResponses", "productCategoryResponses", "images");
    if (productDetailResponse.getProductItemResponses() != null) {
      for (ProductItemResponse productItemResponse : productDetailResponse
          .getProductItemResponses()) {
        ProductItem productItem = new ProductItem();
        BeanUtils.copyProperties(productItemResponse, productItem, "images");
        for (Image image : productItemResponse.getImages()) {
          ProductItemImage productItemImage = new ProductItemImage();
          BeanUtils.copyProperties(image, productItemImage);
          productItem.getProductItemImages().add(productItemImage);
        }
        product.getProductItems().add(productItem);
      }
    }
    if (productDetailResponse.getProductCategoryResponses() != null) {
      for (ProductCategoryResponse productCategoryResponse : productDetailResponse
          .getProductCategoryResponses()) {
        ProductCategory productCategory = new ProductCategory();
        Category category = new Category();
        BeanUtils.copyProperties(productCategoryResponse, productCategory, "category");
        BeanUtils.copyProperties(productCategoryResponse.getCategory(), category);
        productCategory.setCategory(category);
        product.getProductCategories().add(productCategory);
      }
    }
    if (productDetailResponse.getProductAttributeResponses() != null) {
      for (ProductAttributeResponse productAttributeResponse : productDetailResponse
          .getProductAttributeResponses()) {
        ProductAttribute productAttribute = new ProductAttribute();
        Attribute attribute = new Attribute();
        BeanUtils.copyProperties(productAttributeResponse, productAttribute, "attribute");
        BeanUtils.copyProperties(productAttributeResponse.getAttribute(), attribute);
        attribute.setAttributeType(AttributeType.valueOf(productAttributeResponse.getAttribute()
            .getAttributeType()));
        productAttribute.setAttribute(attribute);
        product.getProductAttributes().add(productAttribute);
      }
    }
    if (productDetailResponse.getImages() != null) {
      for (Image image : productDetailResponse.getImages()) {
        ProductImage productImage = new ProductImage();
        BeanUtils.copyProperties(image, productImage);
        product.getProductImages().add(productImage);
      }
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void delete(String id) throws Exception {
    ProductDetailResponse savedProduct = this.productRepository.findDetailById(id);
    if (savedProduct == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "at product, but try to delete it with id : " + id);
    }
    savedProduct.setMarkForDelete(true);
    for (ProductItemResponse savedProductItem : savedProduct.getProductItemResponses()) {
      savedProductItem.setMarkForDelete(true);
    }
    Page<ProductBusinessPartner> savedProductBusinessPartners =
        this.productBusinessPartnerRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            savedProduct.getStoreId(), savedProduct.getId(),
            PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE));
    if (!savedProductBusinessPartners.getContent().isEmpty()) {
      for (ProductBusinessPartner savedProductBusinessPartner : savedProductBusinessPartners
          .getContent()) {
        savedProductBusinessPartner.setMarkForDelete(true);
        for (ProductItemBusinessPartner savedProductItemBusinessPartner : savedProductBusinessPartner
            .getProductItemBusinessPartners()) {
          savedProductItemBusinessPartner.setMarkForDelete(true);
        }
        for (ProductBusinessPartnerAttribute savedProductBusinessPartnerAttribute : savedProductBusinessPartner
            .getProductBusinessPartnerAttributes()) {
          savedProductBusinessPartnerAttribute.setMarkForDelete(true);
        }
      }
      this.productBusinessPartnerRepository.saveAll(savedProductBusinessPartners);
      List<ProductBusinessPartnerResponse> wrapper = new ArrayList<ProductBusinessPartnerResponse>();
      for (ProductBusinessPartner productBusinessPartner : savedProductBusinessPartners.getContent()) {
        ProductBusinessPartnerResponse wrapperElement = new ProductBusinessPartnerResponse();
        BeanUtils.copyProperties(productBusinessPartner, wrapperElement, "productItemBusinessPartners",
            "productBusinessPartnerAttributes");
        convertProductBusinessPartnerToProductBusinessPartnerResponse(productBusinessPartner, wrapperElement);
        wrapper.add(wrapperElement);
      }
      Page<ProductBusinessPartnerResponse> productBusinessPartnerResponsePage =
          new PageImpl<ProductBusinessPartnerResponse>(wrapper,
              PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE), wrapper.size());
      emailNotificationService.sendEmailDeleteProductBusinessPartner(
          savedProduct, productBusinessPartnerResponsePage, null, null);
    }
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCode(
            savedProduct.getStoreId(), savedProduct.getProductCode());
    generateProductCollection(productCollection.getBusinessPartnerCode(),
        productCollection.getBusinessPartnerName(), savedProduct, productCollection);
    productCollection.setMarkForDelete(true);
    this.productCollectionRepository.save(productCollection);
    if (productCollection.isActivated() && productCollection.isViewable()) {
      this.solrActiveProductCollectionService
          .deleteSolrProductCollectionDocument(productCollection.getId());
    }
    this.productRepository.delete(savedProduct);
  }

  private void convertProductBusinessPartnerToProductBusinessPartnerResponse(
      ProductBusinessPartner productBusinessPartner, ProductBusinessPartnerResponse response) throws Exception {
    for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartnerResponse productItemBusinessPartnerResponse = new ProductItemBusinessPartnerResponse();
      BeanUtils.copyProperties(productItemBusinessPartner, productItemBusinessPartnerResponse);
      response.getProductItemBusinessPartners().add(productItemBusinessPartnerResponse);
    }
    for (ProductBusinessPartnerAttribute productBusinessPartnerAttribute : productBusinessPartner
        .getProductBusinessPartnerAttributes()) {
      ProductBusinessPartnerAttributeResponse productBusinessPartnerAttributeResponse =
          new ProductBusinessPartnerAttributeResponse();
      BeanUtils.copyProperties(productBusinessPartnerAttribute, productBusinessPartnerAttributeResponse);
      response.getProductBusinessPartnerAttributes().add(productBusinessPartnerAttributeResponse);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void delete(String productCode, String notes) throws Exception {
    delete(productCode, null, notes, StringUtils.EMPTY);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void delete(String productCode, String notes, String state) throws Exception {
    delete(productCode, null, notes, state);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void delete(String productCode, String productName, String notes, String state) throws Exception {
    ProductDetailResponse product = deletePBPProduct(productCode, productName, notes, state, true);
    //delete PCB product, if it failed then rollback and PBP listening the kafka for PCB status
    this.productRepository.delete(product);
  }

  /**
   * Purpose: method to retry delete PBP product when
   *    reject product is failed due to timeout exception when invoking discard product in PCB
   * Trigger: listening kafka from PCB for 'Product' topic
   */
  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void retryDelete(String id, String storeId, String productCode, String productName, Boolean isMarkForDelete) throws Exception {
    if(id == null || productCode == null || storeId == null || isMarkForDelete == null){
      LOGGER.error("null is passed as id or code");
      throw new ApplicationException(ErrorCategory.INVALID_FORMAT,"null is passed as id or code");
    }
    Page<ProductBusinessPartner> savedProductBusinessPartners =
        this.productBusinessPartnerRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId, id,
            PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE));

    if (!savedProductBusinessPartners.getContent().isEmpty()) {
      for (ProductBusinessPartner savedProductBusinessPartner : savedProductBusinessPartners
          .getContent()) {

        LOGGER.info("Retry delete product with id: {} " + savedProductBusinessPartner.getProductId());
        savedProductBusinessPartner.setMarkForDelete(isMarkForDelete);
        for (ProductItemBusinessPartner savedProductItemBusinessPartner : savedProductBusinessPartner
            .getProductItemBusinessPartners()) {
          savedProductItemBusinessPartner.setMarkForDelete(isMarkForDelete);
        }
        for (ProductBusinessPartnerAttribute savedProductBusinessPartnerAttribute : savedProductBusinessPartner
            .getProductBusinessPartnerAttributes()) {
          savedProductBusinessPartnerAttribute.setMarkForDelete(isMarkForDelete);
        }
      }
      this.productBusinessPartnerRepository.saveAll(savedProductBusinessPartners);
    }

    try{
      ProductCollection productCollection =
          this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
      checkArgument(Objects.nonNull(productCollection), "product collection not present");
      productCollection.setMarkForDelete(isMarkForDelete);
      this.productCollectionRepository.save(productCollection);
      if (!productCollection.isMarkForDelete()) {
        updateSolrProductCollectionDocument(productCollection);
      } else if (productCollection.isViewable() && productCollection.isActivated()) {
        this.solrActiveProductCollectionService
            .deleteSolrProductCollectionDocument(productCollection.getId());
      }
      String notes = "See your email for the rejection note";
      ProductDetailResponse savedProduct = new ProductDetailResponse();
      savedProduct.setId(id);
      savedProduct.setStoreId(storeId);
      savedProduct.setUpdatedBy(productCollection.getUpdatedBy());
      savedProduct.setUpdatedDate(new Date());

      workflowForDelete(savedProduct, notes);
    } catch(Exception e){
      LOGGER.debug("Product already deleted by happy flow of PBP", e);
    }
  }

  private ProductDetailResponse deletePBPProduct(String productCode, String productName, String notes, String state,
      Boolean isMarkForDelete) throws Exception {

    ProductDetailResponse savedProduct =
        this.productRepository.findProductDetailByProductCode(productCode);
    if (savedProduct == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "at product, but try to delete it with productCode : " + productCode);
    }

    savedProduct.setMarkForDelete(isMarkForDelete);
    Page<ProductBusinessPartner> savedProductBusinessPartners =
        this.productBusinessPartnerRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            savedProduct.getStoreId(), savedProduct.getId(),
            PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE));
    if (!savedProductBusinessPartners.getContent().isEmpty()) {
      for (ProductBusinessPartner savedProductBusinessPartner : savedProductBusinessPartners
          .getContent()) {
        savedProductBusinessPartner.setMarkForDelete(isMarkForDelete);
        savedProductBusinessPartner.setState("DELETED");
        for (ProductItemBusinessPartner savedProductItemBusinessPartner : savedProductBusinessPartner
            .getProductItemBusinessPartners()) {
          savedProductItemBusinessPartner.setMarkForDelete(isMarkForDelete);
        }
        for (ProductBusinessPartnerAttribute savedProductBusinessPartnerAttribute : savedProductBusinessPartner
            .getProductBusinessPartnerAttributes()) {
          savedProductBusinessPartnerAttribute.setMarkForDelete(isMarkForDelete);
        }
      }
      this.productBusinessPartnerRepository.saveAll(savedProductBusinessPartners);
      List<ProductBusinessPartnerResponse> wrapper = new ArrayList<ProductBusinessPartnerResponse>();
      for (ProductBusinessPartner productBusinessPartner : savedProductBusinessPartners.getContent()) {
        ProductBusinessPartnerResponse wrapperElement = new ProductBusinessPartnerResponse();
        BeanUtils.copyProperties(productBusinessPartner, wrapperElement, "productItemBusinessPartners",
            "productBusinessPartnerAttributes");
        convertProductBusinessPartnerToProductBusinessPartnerResponse(productBusinessPartner, wrapperElement);
        wrapper.add(wrapperElement);
      }
      Page<ProductBusinessPartnerResponse> productBusinessPartnerResponsePage =
          new PageImpl<ProductBusinessPartnerResponse>(wrapper,
              PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE), wrapper.size());
      emailNotificationService.sendEmailDeleteProductBusinessPartner(savedProduct, productBusinessPartnerResponsePage, notes,
          productName);
    }

    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            savedProduct.getStoreId(), savedProduct.getProductCode());
    if(productCollection == null){
      LOGGER.error("ProductCollection data already deleted present");
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "ProductCollection data not present");
    }
    generateProductCollection(productCollection.getBusinessPartnerCode(),
        productCollection.getBusinessPartnerName(), savedProduct, productCollection);
    productCollection.setMarkForDelete(isMarkForDelete);
    productCollection.setState("DELETED");
    workflowForDelete(savedProduct, notes);
    this.productCollectionRepository.save(productCollection);
    if (!productCollection.isMarkForDelete()) {
      updateSolrProductCollectionDocument(productCollection);
    } else if (productCollection.isViewable() && productCollection.isActivated()) {
      this.solrActiveProductCollectionService
          .deleteSolrProductCollectionDocument(productCollection.getId());
    }

    return savedProduct;
  }

  @Override
  public void deleteProductBusinessPartnerForPostLiveRejection(
      List<ProductBusinessPartner> savedProductBusinessPartners,
      ProductCollection productCollection, String notes) throws Exception {
    List<String> gdnSkus = new ArrayList<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(productCollection, productDetailResponse);
    productDetailResponse.setId(productCollection.getProductId());
    if (CollectionUtils.isNotEmpty(savedProductBusinessPartners)) {
      for (ProductBusinessPartner savedProductBusinessPartner : savedProductBusinessPartners) {
        savedProductBusinessPartner.setMarkForDelete(true);
        savedProductBusinessPartner.setState(STATE_DELETED);
        savedProductBusinessPartner.setProductName(productCollection.getProductName());
        processAppealedProduct(savedProductBusinessPartner, productCollection);
        for (ProductItemBusinessPartner savedProductItemBusinessPartner : savedProductBusinessPartner
            .getProductItemBusinessPartners()) {
          savedProductItemBusinessPartner.setMarkForDelete(true);
          gdnSkus.add(savedProductItemBusinessPartner.getGdnProductItemSku());
        }
        if (CollectionUtils.isNotEmpty(savedProductBusinessPartner.getProductBusinessPartnerAttributes())) {
          for (ProductBusinessPartnerAttribute savedProductBusinessPartnerAttribute : savedProductBusinessPartner
              .getProductBusinessPartnerAttributes()) {
            savedProductBusinessPartnerAttribute.setMarkForDelete(true);
          }
        }
      }
    }
    this.productBusinessPartnerRepository.saveAll(savedProductBusinessPartners);
    this.productLevel3AggregatorService.delete(productDetailResponse.getStoreId(), gdnSkus);
    workflowForDelete(productDetailResponse, notes);
  }

  private void processAppealedProduct(ProductBusinessPartner productBusinessPartner,
    ProductCollection productCollection) {
    if (productBusinessPartner.isAppealedProduct()) {
      productBusinessPartner.setAppealedProduct(false);
      productAppealService.decrementCounterForProductAppeal(productCollection.getStoreId(),
        productCollection.getBusinessPartnerCode());
    }
  }

  @Override
  public Product findById(String id) throws Exception {
    return this.productRepository.findOne(id);
  }

  @Override
  public ProductDetailResponse findDetailById(String id) throws Exception {
    return this.productRepository.findDetailById(id);
  }

  @Override
  public List<ProductItemResponse> findProductItemByProductId(String productId) throws Exception {
    return this.productRepository.findProductItemsByProductId(productId);
  }

  @Override
  public Page<ProductItem> findByKeywordAndViewable(String storeId, String keyword,
      boolean viewable, boolean isOnlyExternal, Pageable pageable) throws Exception {
    return this.productItemRepository.findByStoreIdAndKeywordAndViewable(storeId, keyword,
        viewable, isOnlyExternal, pageable);
  }

  @Override
  public Page<Product> findByName(String storeId, String name, Pageable pageable) throws Exception {
    return this.productRepository.findByStoreIdAndName(storeId, name, pageable);
  }

  @Override
  public Page<Product> findByNameAndViewableAndActivated(String storeId, String name,
      boolean viewable, boolean activated, Pageable pageable) throws Exception {
    return this.productRepository.findByStoreIdAndNameAndViewableAndActivated(storeId, name,
        viewable, activated, pageable);
  }

  @Override
  public Page<Product> findByProductCode(String storeId, String productCode, Pageable pageable)
      throws Exception {
    return this.productRepository.findByStoreIdAndProductCode(storeId, productCode, pageable);
  }

  @Override
  public Page<Product> findByStoreId(String storeId, Pageable pageable) throws Exception {
    return this.productRepository.findByStoreId(storeId, pageable);
  }

  @Override
  public Page<ProductItem> findByUpcCode(String storeId, String upcCode, Pageable pageable)
      throws Exception {
    return this.productItemRepository.findByStoreIdAndUpcCode(storeId, upcCode, pageable);
  }

  @Override
  public Page<ProductCodeResponse> findByNameOrUpcCode(String storeId, String productName,
      String upcCode, String finalCategoryId, List<AttributeReqModel> modelList, Pageable pageable)
      throws Exception {
    return this.productItemRepository.findByStoreIdAndNameOrUpcCode(storeId, productName, upcCode,
        finalCategoryId, modelList, pageable);
  }

  @Override
  public Page<Product> findByViewable(String storeId, boolean viewable, Pageable pageable)
      throws Exception {
    return this.productRepository.findByStoreIdAndViewable(storeId, viewable, pageable);
  }

  @Override
  public Page<Product> findByViewableAndActivated(String storeId, boolean viewable,
      boolean activated, Pageable pageable) throws Exception {
    return this.productRepository.findByStoreIdAndViewableAndActivated(storeId, viewable,
        activated, pageable);
  }

  @Override
  public Page<ProductCollection> findProductsForDalamProcess(DalamProductListRequest dalamProductListRequest)
      throws Exception {
    Pageable pageable =
        PageRequest.of(dalamProductListRequest.getPage(), dalamProductListRequest.getSize());
    return this.solrReviewProductCollectionService.findProductsForDalamProcess(dalamProductListRequest, pageable);
  }

  @Override
  public Page<ProductCollection> findProductsForDalamProcessInBetweenAge(
      DalamProductListRequest dalamProductListRequest) throws Exception {
    Pageable pageable =
        PageRequest.of(dalamProductListRequest.getPage(), dalamProductListRequest.getSize());
    return this.solrReviewProductCollectionService.findProductsForDalamProcess(dalamProductListRequest, pageable);
  }

  @Override
  public Page<ProductCollection> findProductsForDalamProcessAgeLessThen(DalamProductListRequest dalamProductListRequest)
      throws Exception {
    Pageable pageable =
        PageRequest.of(dalamProductListRequest.getPage(), dalamProductListRequest.getSize());
    return this.solrReviewProductCollectionService.findProductsForDalamProcess(dalamProductListRequest, pageable);
  }

  @Override
  public void setReviewPendingFlagToTrue(String storeId, String productCode) throws Exception{
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (Objects.nonNull(productCollection)) {
      productCollection.setReviewPending(true);
      this.productCollectionRepository.save(productCollection);
      updateSolrProductCollectionDocument(productCollection);
      pcbFeign.updateProductReviewPending(storeId, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCode, true);
    }
  }

  @Override
  public ValidateDuplicateProductResponse validateDuplicateProduct(String storeId, String merchantCode,
      ValidateDuplicateProductRequest validateDuplicateProductRequest) throws Exception {
    checkArgument(StringUtils.isNotEmpty(validateDuplicateProductRequest.getKeyword()),
        ErrorMessages.KEYWORD_MUST_NOT_BE_EMPTY);
    ValidateDuplicateProductResponse validateDuplicateProductResponse = new ValidateDuplicateProductResponse();
    if (validateDuplicateProductRequest.isByProductName()) {
      validateDuplicateProductByProductName(storeId, merchantCode, validateDuplicateProductRequest,
          validateDuplicateProductResponse);
    }
    if (validateDuplicateProductRequest.isBySellerSku()) {
      validateDuplicateProductBySellerSku(storeId, merchantCode, validateDuplicateProductRequest,
          validateDuplicateProductResponse);
    }
    return validateDuplicateProductResponse;
  }

  private void validateDuplicateProductBySellerSku(String storeId, String merchantCode,
      ValidateDuplicateProductRequest validateDuplicateProductRequest,
      ValidateDuplicateProductResponse validateDuplicateProductResponse) {
    List<String> productBusinessPartnerIds =
        this.productBusinessPartnerRepository.findByStoreIdAndMerchantSkuAndBusinessPartnerId(storeId,
            validateDuplicateProductRequest.getKeyword(), merchantCode);
    List<ProductBusinessPartner> productBusinessPartnerList = new ArrayList<>();
    List<ProductCollection> productCollectionList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productBusinessPartnerIds)) {
      productBusinessPartnerList =
          this.productBusinessPartnerRepository.findByStoreIdAndId(storeId, productBusinessPartnerIds.get(0));
      if (CollectionUtils.isNotEmpty(productBusinessPartnerList)) {
        productCollectionList = this.productCollectionRepository.findByStoreIdAndProductIds(storeId,
            Arrays.asList(productBusinessPartnerList.get(0).getProductId()));
      }
    }
    if (CollectionUtils.isNotEmpty(productBusinessPartnerList) && CollectionUtils.isNotEmpty(productCollectionList)) {
      validateDuplicateProductResponse.setProductName(productCollectionList.get(0).getProductName());
      validateDuplicateProductResponse.setStatus(productBusinessPartnerList.get(0).getState());
      validateDuplicateProductResponse.setProductSku(productBusinessPartnerList.get(0).getGdnProductSku());
    } else {
      DuplicateProductDetailsResponse duplicateProductDetailsResponse =
          xProductOutbound.validateDuplicateProductBySellerSku(validateDuplicateProductRequest.getKeyword(),
              merchantCode);
      if (Objects.nonNull(duplicateProductDetailsResponse)) {
        BeanUtils.copyProperties(duplicateProductDetailsResponse, validateDuplicateProductResponse);
      }
    }
  }

  private void validateDuplicateProductByProductName(String storeId, String merchantCode,
      ValidateDuplicateProductRequest validateDuplicateProductRequest,
      ValidateDuplicateProductResponse validateDuplicateProductResponse) {
    List<ProductCollection> productCollectionList =
        this.productCollectionRepository.findByStoreIdAndProductNameAndBusinessPartnerCodeAndMarkForDeleteFalse(storeId,
            validateDuplicateProductRequest.getKeyword(), merchantCode);
    if (CollectionUtils.isNotEmpty(productCollectionList)) {
      List<ProductBusinessPartner> productBusinessPartnerList =
          this.productBusinessPartnerRepository.findByStoreIdAndProductId(storeId,
              productCollectionList.get(0).getProductId());
      if (CollectionUtils.isNotEmpty(productBusinessPartnerList)) {
        if (productBusinessPartnerList.get(0).isMarkForDelete()) {
          ProductSkuAndProductCodeRequest prdProductRequest = new ProductSkuAndProductCodeRequest();
          prdProductRequest.setProductSku(productBusinessPartnerList.get(0).getGdnProductSku());
          List<PrdProductResponse> prdProductResponseList =
              this.xProductOutbound.getPrdProductDetailByProductSkuOrProductCode(prdProductRequest);
          if (CollectionUtils.isNotEmpty(prdProductResponseList)) {
            setValidateDuplicateProductResponse(validateDuplicateProductResponse, prdProductResponseList);
          }
        } else {
          validateDuplicateProductResponse.setProductSku(productBusinessPartnerList.get(0).getGdnProductSku());
          validateDuplicateProductResponse.setProductName(productCollectionList.get(0).getProductName());
          validateDuplicateProductResponse.setStatus(productBusinessPartnerList.get(0).getState());
        }
      }
    }
  }

  private static void setValidateDuplicateProductResponse(
      ValidateDuplicateProductResponse validateDuplicateProductResponse,
      List<PrdProductResponse> prdProductResponseList) {
    validateDuplicateProductResponse.setProductSku(prdProductResponseList.get(0).getProductSku());
    validateDuplicateProductResponse.setProductName(prdProductResponseList.get(0).getProductName());
    if (prdProductResponseList.get(0).isSuspended()) {
      validateDuplicateProductResponse.setStatus(com.gdn.x.product.enums.Constants.SUSPENDED);
    } else if (prdProductResponseList.get(0).isArchived()) {
      validateDuplicateProductResponse.setStatus(com.gdn.x.product.enums.Constants.ARCHIVED);
    } else {
      validateDuplicateProductResponse.setStatus(com.gdn.x.product.enums.Constants.ACTIVE);
    }
  }

  @Override
  public Page<SolrProductCollectionDTO> getActiveProductCollectionFromSolr(String storeId, String keyword,
      String categoryCode, Boolean reviewPending, String sortBy, Pageable pageable) throws Exception {
    return this.solrActiveProductCollectionRepository
        .getProductCollectionListFromSolrCollection(storeId, keyword, categoryCode, reviewPending, sortBy, pageable);
  }

  @Override
  public ProductDetailResponse findProductDetailByProductCode(String productCode, boolean inAllProducts) throws Exception {
    ProductDetailResponse productResponse =
        this.productRepository.findProductDetailByProductCode(productCode, inAllProducts);
    return productResponse;
  }

  @Override
  public ProductDetailCompleteResponse findProductDetailWithPreOrderByProductCode(String storeId, String productCode,
      boolean inAllProducts, String businessPartnerCode) throws Exception {
    ProductDetailResponse productResponse =
        this.productRepository.findProductDetailByProductCode(productCode, inAllProducts);
    List<RestrictedKeywordsByFieldResponse> restrictedKeywordsByFieldResponses =
        this.setBrandCodeAndBrandApprovalStatus(storeId, productResponse);
    ProductDetailCompleteResponse productDetailCompleteResponse = new ProductDetailCompleteResponse();
    BeanUtils.copyProperties(productResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setRestrictedKeywordsDetected(restrictedKeywordsByFieldResponses);
    List<ProductBusinessPartner> productBusinessPartnerList =
        this.productBusinessPartnerRepository.findByStoreIdAndProductId(storeId, productResponse.getId());
    if (CollectionUtils.isNotEmpty(productBusinessPartnerList)) {
      if (Objects.nonNull(productBusinessPartnerList.get(0).isPreOrder())) {
        setPreOrderDetails(productBusinessPartnerList.get(0), productDetailCompleteResponse);
      }
      setSizeChartDetails(businessPartnerCode, productResponse, productDetailCompleteResponse,
          productBusinessPartnerList);
    }
    return productDetailCompleteResponse;
  }

  private void setSizeChartDetails(String businessPartnerCode, ProductDetailResponse productResponse,
      ProductDetailCompleteResponse productDetailCompleteResponse,
      List<ProductBusinessPartner> productBusinessPartnerList) {
    if (sizeChartAdditionForProduct) {
      Set<String> sizeChartCodes =
          productBusinessPartnerList.stream().map(ProductBusinessPartner::getSizeChartCode).filter(Objects::nonNull)
              .collect(Collectors.toSet());
      if (CollectionUtils.isNotEmpty(sizeChartCodes)) {
        try {
          fetchAndSetSizeChartDetail(businessPartnerCode, productDetailCompleteResponse, sizeChartCodes);
        } catch (Exception e) {
          log.info("Error in fetching size chart detail for product code : {} ",
              productDetailCompleteResponse.getProductCode());
        }
      }
    }
  }

  private void fetchAndSetSizeChartDetail(String businessPartnerCode,
      ProductDetailCompleteResponse productDetailCompleteResponse, Set<String> sizeChartCodes) {
    BasicSizeChartDetailMapResponse basicSizeChartDetailMapResponse =
        getSizeChartBasicDetailBySizeChartCode(new ArrayList<>(sizeChartCodes));
    CommonUtils.setSizeChartCodeAndSizeChartBusinessPartnerCode(businessPartnerCode, productDetailCompleteResponse,
        basicSizeChartDetailMapResponse);
  }

  private void setPreOrderDetails(ProductBusinessPartner response, ProductDetailCompleteResponse productDetailCompleteResponse) {
    PreOrderResponse preOrderResponse =
        PreOrderResponse.builder().isPreOrder(response.isPreOrder()).preOrderType(response.getPreOrderType())
            .preOrderValue(response.getPreOrderValue()).preOrderDate(response.getPreOrderDate()).build();
    productDetailCompleteResponse.setPreOrder(preOrderResponse);
  }

  public List<RestrictedKeywordsByFieldResponse> setBrandCodeAndBrandApprovalStatus(String storeId, ProductDetailResponse productDetailResponse) {
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCode(storeId, productDetailResponse.getProductCode());
    productDetailResponse.setBrandCode(productCollection.getBrandCode());
    productDetailResponse.setBrandApprovalStatus(productCollection.getBrandApprovalStatus().toString());
    productDetailResponse.setReviewerNotes(productCollection.getReviewerNotes());
    productDetailResponse.setBusinessPartnerCode(productCollection.getBusinessPartnerCode());
    productDetailResponse.setBrand(productCollection.getBrand());
    productDetailResponse.setPostLive(productCollection.isPostLive());
    productDetailResponse.setRestrictedKeywordsPresent(productCollection.isRestrictedKeywordsPresent());
    productDetailResponse.setEdited(productCollection.isEdited());
    productDetailResponse.setReviewPending(productCollection.isReviewPending());
    productDetailResponse
        .setRevised(ProductStatus.NEED_CORRECTION.getProductStatus().equalsIgnoreCase(productCollection.getState()));
    List<RestrictedKeywordsByFieldResponse> restrictedKeywordsByFieldResponseList = new ArrayList<>();
    try {
      if (StringUtils.isNotBlank(productCollection.getRestrictedKeywordsDetected())) {
        restrictedKeywordsByFieldResponseList = objectMapper
            .readValue(productCollection.getRestrictedKeywordsDetected(),
                new TypeReference<List<RestrictedKeywordsByFieldResponse>>() {
                });
      }
    } catch (IOException e) {
      log.error("Error while serialising restricted keyword object. ", e);
    }
    return restrictedKeywordsByFieldResponseList;
  }

  @Override
  public ProductResponse findProductBasicDetailByProductCode(String productCode)
      throws Exception {
    ProductResponse productResponse =
        this.productRepository.findProductBasicDetailByProductCode(productCode);
    return productResponse;
  }

  @Override
  public Page<ProductHistory> findProductHistoryByStoreIdAndProductId(String storeId,
      String productId, Pageable pageable) throws Exception {
    return this.productHistoryRepository
        .findByStoreIdAndProductIdAndMarkForDeleteFalseOrderByCreatedDateDesc(storeId, productId,
            pageable);
  }

  @Override
  public List<ProductWorkflow> findProductWorkflows(String storeId, String productId)
      throws Exception {
    return this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId,
        productId);
  }

  @Override
  public String generateBarcode(String storeId) throws Exception {
    boolean exist;
    String barcode;
    do {
      exist = false;
      barcode = BarcodeGenerator.generateBan17();
      Page<ProductItem> productItems =
          this.productItemRepository.findByStoreIdAndUpcCodeExactMatch(storeId, barcode,
              PageRequest.of(0, 1));
      if (!productItems.getContent().isEmpty()) {
        exist = true;
      }
    } while (exist);
    return barcode;
  }


  private List<ProductLevel3Inventory> generateInventoryForInsert(Product product,
      ProductBusinessPartner productBusinessPartner, Profile businessPartner) throws Exception {
    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
        .getProductItemBusinessPartners()) {
      ProductItem savedProductItem = null;
      for (ProductItem productItem : product.getProductItems()) {
        if (productItem.getId().equals(productItemBusinessPartner.getProductItemId())) {
          savedProductItem = productItem;
          break;
        }
      }
      ProductLevel3Inventory inventory = new ProductLevel3Inventory();
      if (isPurchaseOrderPurchaseTerm(businessPartner)) {
        inventory.setWarehouseMerchantCode(GdnBaseLookup.DEFAULT_BUSINESS_PARTNER_CODE);
      } else {
        inventory.setWarehouseMerchantCode(businessPartner.getBusinessPartnerCode());
      }
      if (savedProductItem==null) {
        throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "[productItem for:" + productItemBusinessPartner.getGdnProductItemSku() + " not found]");
      }
      inventory.setWarehouseItemSku(savedProductItem.getSkuCode());
      inventory.setWebMerchantCode(businessPartner.getBusinessPartnerCode());
      inventory.setWebItemSku(productItemBusinessPartner.getGdnProductItemSku());
      inventory.setWebAvailable(productItemBusinessPartner.getStock());
      inventory.setDistributionPickupPoint(productItemBusinessPartner.isDistribution());
      inventory.setWebMinAlert(productItemBusinessPartner.getMinimumStock());
      inventory.setWebSyncStock(false);
      inventory.setWebPickupPointCode(productItemBusinessPartner.getPickupPointId());
      inventory.setProductSku(productBusinessPartner.getGdnProductSku());
      CommonUtils.setPreOrderFields(preOrderConfig.isPoQuotaFeatureSwitch(),
          ProfileResponse.builder().flags(businessPartner.getFlags()).build(),
          productBusinessPartner.getPreOrderDate(), inventory,
          productItemBusinessPartner.getPreOrderQuota());
      inventories.add(inventory);
    }
    return inventories;
  }

  private void generateProductCollection(String businessPartnerCode, String businessPartnerName,
      Product product, ProductCollection productCollection) throws Exception {
    productCollection.setStoreId(product.getStoreId());
    productCollection.setProductId(product.getId());
    productCollection.setProductCode(product.getProductCode());
    productCollection.setProductName(product.getName());
    productCollection.setBrand(product.getBrand());
    ProductCategory validProductCategory = null;
    for (ProductCategory productCategory : product.getProductCategories()) {
      if (!productCategory.isMarkForDelete()) {
        validProductCategory = productCategory;
        break;
      }
    }
    if (validProductCategory == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "[While building ProductCollection. ProductCategory for Product:" + product.getProductCode() + " not found]");
    }
    productCollection.setCategoryCode(validProductCategory.getCategory().getCategoryCode());
    productCollection.setCategoryName(validProductCategory.getCategory().getName());
    productCollection.setBusinessPartnerCode(businessPartnerCode);
    productCollection.setBusinessPartnerName(businessPartnerName);
    productCollection.setActivated(product.isActivated());
    productCollection.setViewable(product.isViewable());
  }

  @Override
  @Deprecated
  public Double generateShippingWeight(Product product) throws Exception {
    Category category = new Category();
    for (ProductCategory productCategory : product.getProductCategories()) {
      if (productCategory.getCategory().getCatalog().getCatalogType()
          .equals(CatalogType.MASTER_CATALOG)) {
        category = productCategory.getCategory();
      }
    }
    return BaseGenerator.generateShippingWeight(product.getLength(), product.getWidth(),
        product.getHeight(), product.getWeight(), category.getLogisticAdjustment());
  }

  private boolean isPurchaseOrderPurchaseTerm(Profile businessPartner) throws Exception {
    return businessPartner.getCompany().getPurchaseTerm()
        .equals(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Deprecated
  public String save(Product product) throws Exception {
    if (!StringUtils.isEmpty(product.getId())) {
      Product savedProduct = this.productRepository.findOne(product.getId());
      if (savedProduct != null) {
        throw new ApplicationException(ErrorCategory.DATA_ACCESS,
            "duplicate data detected at product with id : " + product.getId());
      }
    }
    product.setProductCode(getProductCodeNextSequence());
    this.productRepository.save(product, DEFAULT_BUSINESS_PARTNER_CODE);

    // Create state save and submit by productCode
    Page<Product> products =
        this.productRepository.findByStoreIdAndProductCodeExactMatch(product.getStoreId(),
            product.getProductCode(), PageRequest.of(0, 10));
    if (!products.getContent().isEmpty()) {
      Product savedProduct = products.getContent().get(0);
      saveCallback(savedProduct);
    }
    return product.getId();
  }

  private String getProductCodeNextSequence() {
    return Constants.PRODUCT_CODE_PREFIX + Constants.DASH_DELIMITER + StringUtils
        .leftPad(String.valueOf(this.sequenceRepository.findByCode(Constants.PRODUCT_CODE_PREFIX)),
            7, '0');
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void saveCallback(Product product) throws Exception {
    workflowForSave(product);
    workflowForSubmit(product.getStoreId(), product);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Deprecated
  public String saveProductCollection(String businessPartnerCode, String businessPartnerName,
      Product product) throws Exception {
    if (!StringUtils.isEmpty(product.getId())) {
      Product savedProduct = this.productRepository.findOne(product.getId());
      if (savedProduct != null) {
        throw new ApplicationException(ErrorCategory.DATA_ACCESS,
            "duplicate data detected at product with id : " + product.getId());
      }
    }
    if (StringUtils.isEmpty(businessPartnerCode)) {
      product.setActivated(true);
      product.setViewable(false);
      businessPartnerCode = DEFAULT_BUSINESS_PARTNER_CODE;
      businessPartnerName = DEFAULT_BUSINESS_PARTNER_NAME;
    } else {
      product.setActivated(false);
      product.setViewable(false);
    }
    product.setProductCode(getProductCodeNextSequence());
    this.productRepository.save(product, businessPartnerCode);
    ProductDetailResponse savedProduct =
        this.productRepository.findProductDetailByProductCode(product.getProductCode());
    product.setId(savedProduct.getId());
    product.setStoreId(savedProduct.getStoreId());
    product.setCreatedBy(savedProduct.getCreatedBy());
    product.setCreatedDate(savedProduct.getCreatedDate());
    product.setUpdatedBy(savedProduct.getUpdatedBy());
    product.setUpdatedDate(savedProduct.getUpdatedDate());
    ProductCollection productCollection = new ProductCollection();
    generateProductCollection(businessPartnerCode, businessPartnerName, product, productCollection);
    productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    this.productCollectionRepository.saveAndFlush(productCollection);
    updateSolrProductCollectionDocument(productCollection);
    saveCallback(product);
    return product.getProductCode();
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void submit(String storeId, Product product) throws Exception {
    Product savedProduct = this.productRepository.findOne(product.getId());
    if (savedProduct == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "at product, but try to submit it with id : " + product.getId());
    }
    workflowForSubmit(storeId, product);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateForMerge(Product product) throws Exception {
    this.productRepository.updateForMerge(product);
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public SimpleMasterProductUpdateResponse updateForBulk(
      SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO, String storeId,
      ProductCollection productCollection) throws Exception {
    String categoryCode = productCollection.getCategoryCode();
    if (setShippingWeight(simpleMasterProductUpdateRequestDTO, categoryCode)) {
      return new SimpleMasterProductUpdateResponse.Builder()
          .productCode(simpleMasterProductUpdateRequestDTO.getProductCode()).updateSuccess(Boolean.FALSE)
          .reasonOfFailure(ERROR_IN_SHIPPING_WEIGHT_CALCULATION).build();
    }
    if (!Constants.INTERNAL.equals(productCollection.getBusinessPartnerCode()) && !validateDimensions(
        simpleMasterProductUpdateRequestDTO, productCollection)) {
      LOGGER.error(ErrorMessages.INVALID_DIMENSIONS , simpleMasterProductUpdateRequestDTO.getProductCode());
      return new SimpleMasterProductUpdateResponse.Builder()
          .productCode(simpleMasterProductUpdateRequestDTO.getProductCode()).updateSuccess(Boolean.FALSE)
          .reasonOfFailure(ErrorMessages.INVALID_DIMENSIONS).build();
    }
    SimpleMasterProductUpdateResponse response =
        this.productRepository.updateSimpleMasterProduct(simpleMasterProductUpdateRequestDTO);
    LOGGER.info(SIMPLE_PRODUCT_UPDATE_IN_PCB, simpleMasterProductUpdateRequestDTO.getProductCode());
    xProductOutbound.generateProductScoreByProductSkuOrProductCode(null,
        simpleMasterProductUpdateRequestDTO.getProductCode(), false);
    if (productCollection.isReviewPending() && response.isDimensionOrDgLevelUpdated()) {
      productPublisherService.publishProductDimensionRefreshEvent(
          ConverterUtil.toPDTDimensionRefreshEventModel(productCollection.getStoreId(),
              productCollection.getProductCode(),
              ConverterUtil.toDimensionRefreshRequest(simpleMasterProductUpdateRequestDTO.getLength(),
                  simpleMasterProductUpdateRequestDTO.getWidth(), simpleMasterProductUpdateRequestDTO.getHeight(),
                  simpleMasterProductUpdateRequestDTO.getWeight(),
                  simpleMasterProductUpdateRequestDTO.getShippingWeight(),
                  simpleMasterProductUpdateRequestDTO.getDangerousGoodsLevel(), null)));
    }
    return response;
  }

  private boolean validateDimensions(SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO,
      ProductCollection productCollection) throws Exception {
    int productType = productBusinessPartnerService.getProductTypeBasedOnProductId(productCollection.getProductId());
    ProfileResponse businessPartner = this.businessPartnerRepository
        .filterDetailByBusinessPartnerCode(productCollection.getBusinessPartnerCode());
    if (simpleMasterProductUpdateRequestDTO.getShippingWeight() == 0 && (ProductType.REGULAR.getCode() == productType
        || ProductType.BIG_PRODUCT.getCode() == productType)) {
      return false;
    }
    return true;
  }

  @Override
  @Transactional(readOnly = false)
  public Map<Boolean, ProductCollection> checkAndUpdateSolr(String storeId,
      SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO, Product existingProduct,
      ProductCollection productCollection) throws Exception {
    boolean changed = isChangeInProductCollectionExists(simpleMasterProductUpdateRequestDTO, productCollection);
    if (changed) {
      productCollection = this.productCollectionRepository.save(productCollection);
    }
    updateHistory(storeId, simpleMasterProductUpdateRequestDTO, productCollection, existingProduct);
    Map<Boolean, ProductCollection> productCollectionMap = new HashMap<>();
    productCollectionMap.put(changed, productCollection);
    return productCollectionMap;
  }

  @Async
  public void updateHistory(String storeId, SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO,
      ProductCollection productCollection, Product existingProduct) {
    String changedFieldNote = getChangedFieldNote(simpleMasterProductUpdateRequestDTO, existingProduct);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        simpleMasterProductUpdateRequestDTO.getUpdatedBy());
    if (StringUtils.isNotEmpty(changedFieldNote)) {
      ProductHistory productHistory =
          new ProductHistory(productCollection.getProductId(), ProductWorkflowLookup.STATE_EDIT, ProductWorkflowLookup.STATE_EDIT_DESCRIPTION, changedFieldNote,
              simpleMasterProductUpdateRequestDTO.getUpdatedBy(), simpleMasterProductUpdateRequestDTO.getUpdatedDate(),
              storeId);
      this.productHistoryRepository.save(productHistory);
    }
  }

  @Async
  @Override
  @Transactional(readOnly = false)
  public void updateHistoryOnAttributeAutoFill(String storeId, String productId,
      List<AttributeHistoryResponse> attributeHistoryResponseList) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.ATTRIBUTE_AUTO_FILL);
    List<ProductFieldHistory> changedFieldList = attributeHistoryResponseList.stream().map(
        attributeHistoryResponse -> new ProductFieldHistory(attributeHistoryResponse.getAttributeName(),
            Constants.HYPHEN, attributeHistoryResponse.getNewValue())).collect(Collectors.toList());
    String changedFields = ProductWorkflowLookup.STATE_EDIT_DESCRIPTION + " : " + changedFieldList.toString();
    String changedFieldsNotes = StringUtils.abbreviate(changedFields, 2000);
    ProductHistory productHistory =
        new ProductHistory(productId, ProductWorkflowLookup.STATE_EDIT, ProductWorkflowLookup.STATE_EDIT_DESCRIPTION,
            changedFieldsNotes, Constants.ATTRIBUTE_AUTO_FILL, new Date(), storeId);
    this.productHistoryRepository.save(productHistory);
  }

  private String getChangedFieldNote(SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO,
      Product existingProduct) {
    String changedFieldNote = StringUtils.EMPTY;
    try {
      List<ProductFieldHistory> changedFieldList =
          productChangeUtil.getChangedProductFields(existingProduct, simpleMasterProductUpdateRequestDTO);
      if (CollectionUtils.isNotEmpty(changedFieldList)) {
        String changedFields = ProductWorkflowLookup.STATE_EDIT_DESCRIPTION + " : " + changedFieldList.toString();
        changedFieldNote = StringUtils.abbreviate(changedFields, 2000);
      }
    } catch (Exception e) {
      LOGGER.error(CHANGED_FIELDS_FETCH_ERROR, e);
    }
    return changedFieldNote;
  }

  private void checkAndUpdateSolrCollection(SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO,
      ProductCollection productCollection) {
    boolean changed = isChangeInProductCollectionExists(simpleMasterProductUpdateRequestDTO, productCollection);
    if(changed) {
      this.productCollectionRepository.save(productCollection);
      updateSolrProductCollection(productCollection);
      LOGGER.info(SOLR_UPDATE_DONE_FOR_PRODUCT_COLLECTION, productCollection.getProductCode());
    }
  }

  private boolean setShippingWeight(SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO,
      String categoryCode) {
    try{
      Double shippingWeight = generatorService.generateShippingWeight(
          simpleMasterProductUpdateRequestDTO.getLength(), simpleMasterProductUpdateRequestDTO.getWidth(),
          simpleMasterProductUpdateRequestDTO.getHeight(), simpleMasterProductUpdateRequestDTO.getWeight(), categoryCode);
      simpleMasterProductUpdateRequestDTO.setShippingWeight(shippingWeight);
    }catch (Exception e){
      LOGGER.error(ERROR_IN_SHIPPING_WEIGHT_CALCULATION, e);
      return true;
    }
    return false;
  }

  private boolean isChangeInProductCollectionExists(SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO,
      ProductCollection productCollection) {
    boolean changed = false;
    if(!StringUtils.equals(simpleMasterProductUpdateRequestDTO.getBrand(), productCollection.getBrand())) {
      productCollection.setBrand(simpleMasterProductUpdateRequestDTO.getBrand());
      changed = true;
    }
    if(Objects.nonNull(simpleMasterProductUpdateRequestDTO.getName()) &&
        !simpleMasterProductUpdateRequestDTO.getName().equals(productCollection.getProductName())) {
      productCollection.setProductName(simpleMasterProductUpdateRequestDTO.getName());
      changed = true;
    }
    return changed;
  }


  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void update(Product product) throws Exception {
    update(product, false, null, null, null);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void update(Product product, String notes, String brandCode, String brandApprovalStatus) throws Exception {
    update(product, false, notes, brandCode, brandApprovalStatus);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void update(Product product, boolean onlyProduct, String notes, String brandCode, String brandApprovalStatus,
      boolean isMarginExceeded, boolean postLive, String forceReviewNotes, boolean onlyVatChanged) throws Exception {
    Product savedProduct = this.productRepository.findOne(product.getId());
    if (savedProduct == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "at product, but try to update it with id : " + product.getId());
    }
    String changedFieldNote = ProductWorkflowLookup.STATE_EDIT_DESCRIPTION;
    List<ProductFieldHistory> changedFieldList = new ArrayList<>();
    try {
      fileStorageService.editImageNameIfGcsEnabled(product, savedProduct);
      changedFieldList = this.productChangeUtil.getProductDiff(product, savedProduct);
      String changedFields = changedFieldList.toString();
      changedFields = ProductWorkflowLookup.STATE_EDIT_DESCRIPTION + " : " + changedFields;
      changedFieldNote = StringUtils.abbreviate(changedFields, HISTORY_NOTES_SIZE);
    } catch (Exception e) {
      LOGGER.error("Exception Occurred while getting product field difference on updating, continue " +
          "with process", e);
    }

    fileStorageService.validateImageExists(changedFieldList);

    //check for pristine category
    Boolean isPristineCategory = null;
    Set<String> categoryIds = this.productChangeUtil.getCategoryIds(product, savedProduct);
    if (CollectionUtils.isNotEmpty(categoryIds)) {
      for (String categoryId : categoryIds) {
        isPristineCategory = this.productLevel3Service.isPristineCategory(categoryId);
        if (Boolean.TRUE.equals(isPristineCategory)) {
          break;
        }
      }
    }

    boolean activating = false;
    boolean changeStep = false;
    if (!savedProduct.isActivated() && product.isActivated()) {
      activating = true;
    } else if (savedProduct.isActivated()) {
      for (ProductItem productItem : product.getProductItems()) {
        if (!productItem.isActivated()) {
          activating = true;
          changeStep = true;
          break;
        }
      }
    }
    BeanUtils.copyProperties(product, savedProduct, "createdDate", "createdBy");
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCode(
            savedProduct.getStoreId(), savedProduct.getProductCode());
    if(productCollection == null){
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Product collection data is not found for: " + savedProduct.getProductCode());
    }
    String existingCategoryCode = productCollection.getCategoryCode();
    String existingCategoryName = productCollection.getCategoryName();
    productCollection.setMarkForDelete(false);
    productCollection.setReviewerNotes(notes);
    if (Objects.nonNull(brandCode) && Objects.nonNull(brandApprovalStatus)) {
      productCollection.setBrandCode(brandCode);
      productCollection.setBrandApprovalStatus(BrandApprovalStatus.valueOf(brandApprovalStatus));
    }
    generateProductCollection(productCollection.getBusinessPartnerCode(),
        productCollection.getBusinessPartnerName(), savedProduct, productCollection);
    if (changeStep) {
      productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    }
    if (!onlyProduct) {
      updateProductBusinessPartnerInformation(savedProduct);
    }
    if (postLive != productCollection.isPostLive()) {
      // Need to check config again if needed
      changedFieldNote = changedFieldNote
          .concat(String.valueOf(new ProductFieldHistory(POST_LIVE, productCollection.isPostLive(), postLive)));
      changedFieldList.add(new ProductFieldHistory(POST_LIVE, productCollection.isPostLive(), postLive));
      productCollection.setPostLive(postLive);
    }
    this.productCollectionRepository.save(productCollection);
    updateSolrProductCollection(productCollection);
    LOGGER.info("Product collection update in Solr is successful, product-code :{}",productCollection.getProductCode());
    this.productRepository.update(savedProduct, isPristineCategory, onlyVatChanged, true, false, new ArrayList<>(),
        new HashedMap(), new ArrayList<>(), false, false, new ArrayList<>(), new ProductL3Response(), EditProductResponse.builder().build());
    LOGGER.info("Product update in PCB successful product-code :{}",savedProduct.getProductCode());
    if (activating) {
      this.productRepository.activate(savedProduct);
      LOGGER.info("Activation successful");
    }

    Set<String> changedField =
        changedFieldList.stream().map(ProductFieldHistory::getFieldName).collect(Collectors.toSet());
    xProductOutbound.generateProductScoreByProductSkuOrProductCode(null, savedProduct.getProductCode(),
        changedField.contains(ProductChangeUtil.CATEGORY));
    if(isMarginExceeded){
      CategoryChangeMailEvent categoryChangeMailEvent = CategoryChangeMailEvent.builder()
          .existingCategoryCode(existingCategoryCode).existingCategoryName(existingCategoryName)
          .build();
      this.productMailEventService.createAndSaveCategoryChangeMailEvent(productCollection , categoryChangeMailEvent);
    }
    saveProductHistoryAndAudit(product, forceReviewNotes, changedFieldNote, changedFieldList, activating,
      productCollection);
    takeActionsOnCategoryChangeFromInternal(product, changedField, productCollection.getStoreId());
    if (productCollection.isReviewPending() && productChangeUtil.isDimensionFieldsChanged(changedFieldList)) {
      productPublisherService.publishProductDimensionRefreshEvent(
          ConverterUtil.toPDTDimensionRefreshEventModel(productCollection.getStoreId(),
              productCollection.getProductCode(),
              ConverterUtil.toDimensionRefreshRequest(savedProduct.getLength(), savedProduct.getWidth(),
                  savedProduct.getHeight(), savedProduct.getWeight(), savedProduct.getShippingWeight(),
                  savedProduct.getProductItems().get(0).getDangerousGoodsLevel(), null)));
    }
  }

  public void takeActionsOnCategoryChangeFromInternal(Product product, Set<String> changedField,
    String storeId) throws Exception {
    // Check if product is eligible for shipping migration based on BOPIS eligibility
    boolean isEligibleForShippingMigration = Boolean.FALSE.equals(
      product.getProductCategories().stream().map(ProductCategory::getCategory)
        .filter(Objects::nonNull).reduce((first, second) -> second).map(Category::isBopisEligible)
        .orElse(null));

    //  category change and eligible for migration and switch ON and Dimensions not added by User
    if (migrateProductForCategoryUpdate(changedField, isEligibleForShippingMigration, product)) {
      List<BasicProductResponse> basicProductResponses = getBasicProductResponses(product);
      List<ProductType> productTypes =
        basicProductResponses.stream().map(BasicProductResponse::getProductType)
          .filter(Objects::nonNull).distinct().collect(Collectors.toList());
      //Fetch productTypes to find product that are BOPIS

      if (productTypes.contains(ProductType.BOPIS)) {
        // For Active product update x-product else update product item businessPartner
        migrateActiveAndInActiveProducts(storeId, isEligibleForShippingMigration,
          basicProductResponses.stream().map(BasicProductResponse::getProductSku).collect(Collectors.toList()));
      }
    }
  }

  private boolean migrateProductForCategoryUpdate(Set<String> changedField,
    boolean isEligibleForShippingMigration, Product product) {
    boolean dimensionLess =
      CommonUtils.isDimensionLess(product.getHeight(), product.getWeight(), product.getLength(),
        product.getShippingWeight());
    return changedField.contains(ProductChangeUtil.CATEGORY) && isEligibleForShippingMigration
      && bopisCategoryActionOnVendorCategoryChangeSwitch && dimensionLess;
  }

  private List<BasicProductResponse> getBasicProductResponses(Product product) {
    List<String> productSkus =
      productBusinessPartnerService.getProductSkusByProductCode(product.getProductCode());
    // List will not be singleton for migrated products only
    return productSkus.stream()
      .map(productSku -> xProductOutbound.getBasicProductInfoV2(productSku))
      .collect(Collectors.toList());
  }

  private void markL5OfflineInXproduct(Set<String> productSkus) {
    productSkus.forEach(productSku -> {
      com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest
        productAndL5MigrationRequest =
        com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest.builder()
          .buyable(false).discoverable(false).dimensionsMissing(true).productSku(productSku)
          .productType(ProductType.REGULAR).build();
      xProductOutbound.migrateProductAndL5DetailByProductSku(productAndL5MigrationRequest,
        GdnMandatoryRequestParameterUtil.getStoreId(), SYSTEM_REVIEW);
    });
  }

  private void saveProductHistoryAndAudit(Product product, String forceReviewNotes,
      String changedFieldNote, List<ProductFieldHistory> changedFieldList, boolean activating,
      ProductCollection productCollection) {
    if (CollectionUtils.isNotEmpty(changedFieldList)) {
      ProductHistory productHistory = new ProductHistory(product.getId(), ProductWorkflowLookup.STATE_EDIT,
          ProductWorkflowLookup.STATE_EDIT_DESCRIPTION, changedFieldNote, product.getUpdatedBy(),
          product.getUpdatedDate(), product.getStoreId());
      this.productHistoryRepository.save(productHistory);
    }
    if (StringUtils.isNotEmpty(forceReviewNotes)) {
      ProductHistory productHistory = new ProductHistory(product.getId(), ProductWorkflowLookup.STATE_EDIT,
          ProductWorkflowLookup.STATE_PRODUCT_FORCE_REVIEW_DESCRIPTION, forceReviewNotes, product.getUpdatedBy(),
          product.getUpdatedDate(), product.getStoreId());
      this.productHistoryRepository.save(productHistory);
    }
  }

  @Override
  public void update(Product product, boolean onlyProduct, String notes,
      String brandCode, String brandApprovalStatus) throws Exception {
    update(product,onlyProduct,notes,brandCode,brandApprovalStatus, false, false, StringUtils.EMPTY, false);
  }


  private void updateProductBusinessPartnerInformation(Product product) throws Exception {
    Page<ProductBusinessPartner> productBusinessPartners =
        this.productBusinessPartnerRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            product.getStoreId(), product.getId(), PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE));
    if (!productBusinessPartners.getContent().isEmpty()) {
      ProductCategory validProductCategory = null;
      for (ProductCategory productCategory : product.getProductCategories()) {
        if (!productCategory.isMarkForDelete()) {
          validProductCategory = productCategory;
          break;
        }
      }
      if (validProductCategory == null) {
        throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "[While updating product info. ProductCategory for Product:" + product.getProductCode() + " not found]");
      }
      for (ProductBusinessPartner productBusinessPartner : productBusinessPartners.getContent()) {
        productBusinessPartner.setProductName(product.getName());
        productBusinessPartner.setCategoryName(validProductCategory.getCategory().getName());
        productBusinessPartner.setBrand(product.getBrand());
      }
      this.productBusinessPartnerRepository.saveAll(productBusinessPartners);
    }
  }

  @Override
  public Boolean validateBarcode(String storeId, String barcode) throws Exception {
    Page<ProductItem> productItems =
        this.productItemRepository.findByStoreIdAndUpcCodeExactMatch(storeId, barcode, PageRequest.of(0, 1));
    if (!productItems.getContent().isEmpty()) {
      throw new ApplicationException(ErrorCategory.DATA_ACCESS,
          "duplicate data detected at product item with upcCode : " + barcode);
    }
    return BarcodeValidator.validateEan13(barcode);
  }

  private boolean workflowForApprovedContent(String storeId, Product product) throws Exception {
    boolean active = false;
    Page<ProductWorkflow> savedProductWorkflows =
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId,
            product.getId(), PageRequest.of(0, 10));
    if (savedProductWorkflows.getContent().size() > 3) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "Product id : " + product.getId()
          + ". Error code : status product invalid: " + savedProductWorkflows.getSize()
          + ". Silakan refresh halaman dan coba kembali.");
    }
    ProductWorkflow savedProductWorkflow = null;
    for (ProductWorkflow productWorkflow : savedProductWorkflows.getContent()) {
      if (productWorkflow.getState() == 1) {
        savedProductWorkflow = productWorkflow;
      }
    }
    if (savedProductWorkflow == null) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "Product id : " + product.getId()
          + ". Error code : product state is "
          + savedProductWorkflows.getContent().get(0).getState()
          + "Silakan refresh halaman dan coba kembali.");
    }
    ProductWorkflow productWorkflow = null;
    List<ProductHistory> productHistories = new ArrayList<ProductHistory>();
    productHistories.add(new ProductHistory(product.getId(),
        ProductWorkflowLookup.STATE_REVIEW_CONTENT,
        ProductWorkflowLookup.STATE_APPROVED_CONTENT_DESCRIPTION,
        ProductWorkflowLookup.STATE_APPROVED_CONTENT_NOTES, product.getUpdatedBy(), product
        .getUpdatedDate(), storeId));
    this.productWorkflowRepository.delete(savedProductWorkflow);
    if (savedProductWorkflows.getContent().size() == 1) {
      productWorkflow =
          new ProductWorkflow(product.getId(), ProductWorkflowLookup.STATE_ACTIVE,
              ProductWorkflowLookup.STATE_ACTIVE_DESCRIPTION, product.getUpdatedBy(),
              product.getUpdatedDate(), storeId);
      productHistories.add(new ProductHistory(product.getId(), ProductWorkflowLookup.STATE_ACTIVE,
          ProductWorkflowLookup.STATE_ACTIVE_DESCRIPTION, ProductWorkflowLookup.STATE_ACTIVE_NOTES,
          product.getUpdatedBy(), product.getUpdatedDate(), storeId));
      product.setActivated(true);
      product.setViewable(true);
      for (ProductItem productItem : product.getProductItems()) {
        productItem.setActivated(true);
        productItem.setViewable(true);
      }
      // Update product level 1/2 here if become active
      this.productRepository.update(product, null, false, true, false, new ArrayList<>(), new HashedMap(),
          new ArrayList<>(), false, false, new ArrayList<>(), new ProductL3Response(), EditProductResponse.builder().build());

      // Create new products to wcs
      Page<ProductBusinessPartner> productBusinessPartners =
          this.productBusinessPartnerRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
              product.getStoreId(), product.getId(), PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE));
      for (ProductBusinessPartner productBusinessPartner : productBusinessPartners.getContent()) {
        Profile businessPartner =
            this.businessPartnerRepository
                .filterByCode(productBusinessPartner.getBusinessPartnerId());
        productBusinessPartner.setActivated(true);
        try {
          LOGGER.info("Saving product data in X-Product for productCode : {} in workflowForApprovedContent api",
              product.getProductCode());
          this.productLevel3Service.create(businessPartner.getBusinessPartnerCode(), product,
              productBusinessPartner);
          LOGGER.info(
              "Product data saved successfully in X-Product for productCode : {} in workflowForApprovedContent api",
              product.getProductCode());
          List<ProductLevel3Inventory> inventories =
              generateInventoryForInsert(product, productBusinessPartner, businessPartner);
          LOGGER.info("Saving item data in X-inventory for product code in workflowForApprovedContent api: {}",
              product.getProductCode());
          getProductLevel3InventoryService().insertInventory(inventories);
          LOGGER.info("Items successfully saved in x-inventory for productCode in workflowForApprovedContent api: {}",
              product.getProductCode());
          productBusinessPartner.setMarkForDelete(true);
        } catch (Exception e) {
          LOGGER.error("error invoking save new product to wcs for productCode {} in workflowForApprovedContent : {} ",
              product.getProductCode(), e);
        }
        this.productBusinessPartnerRepository.save(productBusinessPartner);
      }
      this.productWorkflowRepository.saveAndFlush(productWorkflow);
      active = true;
    }
    this.productHistoryRepository.saveAll(productHistories);
    return active;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateWCSAndInventoryForMergedProduct(Product masterDataProduct,
      ProductBusinessPartner productBusinessPartner, Map<String, String> oldToNewProductItemIdMap)
      throws Exception {
    ProductDetailResponse productData =
        this.productRepository.findProductDetailByProductCode(masterDataProduct.getProductCode());
    ProductBusinessPartner savedProductBusinessPartner =
        this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()).orElse(null);
    updateProductItemBusinessPartnerForMerge(oldToNewProductItemIdMap,
        savedProductBusinessPartner);
    if (StringUtils.isEmpty(savedProductBusinessPartner.getGdnProductSku()))
      this.productGdnSkuGeneratorService.generateGdnSkuOnProduct(savedProductBusinessPartner, false);
    savedProductBusinessPartner.setActivated(true);
    try {
      this.productLevel3Service.create(savedProductBusinessPartner.getBusinessPartnerId(),
          productData, savedProductBusinessPartner, false, new ArrayList<>());
      savedProductBusinessPartner.setMarkForDelete(true);
    } catch (Exception e) {
      ProductServiceBean.LOGGER.error("error invoking create product level 3 at service.", e);
    }
    this.productBusinessPartnerRepository.save(savedProductBusinessPartner);
  }

  private void updateProductItemBusinessPartnerForMerge(
      Map<String, String> oldToNewProductItemIdMap,
      ProductBusinessPartner savedProductBusinessPartner) {
    Set<ProductItemBusinessPartner> productItemBusinessPartners =
        new HashSet<>(savedProductBusinessPartner.getProductItemBusinessPartners());
    List<ProductItemBusinessPartner> itemBusinessPartners = new ArrayList<>();
    for(ProductItemBusinessPartner itemBusinessPartner : productItemBusinessPartners){
      String productItemId = oldToNewProductItemIdMap.get(itemBusinessPartner.getProductItemId());
      if(StringUtils.isNotEmpty(productItemId)) {
        itemBusinessPartner.setProductItemId(productItemId);
      }
      itemBusinessPartners.add(itemBusinessPartner);
    }
    savedProductBusinessPartner.setProductItemBusinessPartners(itemBusinessPartners);
  }

  private boolean workflowForApprovedImage(String storeId, Product product) throws Exception {
    boolean active = false;
    Page<ProductWorkflow> savedProductWorkflows =
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId,
            product.getId(), PageRequest.of(0, 10));
    if (savedProductWorkflows.getContent().size() > 2) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "Product id : " + product.getId()
          + ". Error code : status product invalid: " + savedProductWorkflows.getSize()
          + ". Silakan refresh halaman dan coba kembali.");
    }
    ProductWorkflow savedProductWorkflow = null;
    for (ProductWorkflow productWorkflow : savedProductWorkflows.getContent()) {
      if (productWorkflow.getState() == 2) {
        savedProductWorkflow = productWorkflow;
      }
    }
    if (savedProductWorkflow == null) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "Product id : " + product.getId()
          + ". Error code : product state is "
          + savedProductWorkflows.getContent().get(0).getState()
          + "Silakan refresh halaman dan coba kembali.");
    }
    ProductWorkflow productWorkflow = null;
    List<ProductHistory> productHistories = new ArrayList<ProductHistory>();
    productHistories.add(new ProductHistory(product.getId(),
        ProductWorkflowLookup.STATE_REVIEW_IMAGE,
        ProductWorkflowLookup.STATE_APPROVED_IMAGE_DESCRIPTION,
        ProductWorkflowLookup.STATE_APPROVED_IMAGE_NOTES, product.getUpdatedBy(), product
        .getUpdatedDate(), storeId));
    this.productWorkflowRepository.delete(savedProductWorkflow);
    if (savedProductWorkflows.getContent().size() == 1) {
      productWorkflow =
          new ProductWorkflow(product.getId(), ProductWorkflowLookup.STATE_ACTIVE,
              ProductWorkflowLookup.STATE_ACTIVE_DESCRIPTION, product.getUpdatedBy(),
              product.getUpdatedDate(), storeId);
      productHistories.add(new ProductHistory(product.getId(), ProductWorkflowLookup.STATE_ACTIVE,
          ProductWorkflowLookup.STATE_ACTIVE_DESCRIPTION, ProductWorkflowLookup.STATE_ACTIVE_NOTES,
          product.getUpdatedBy(), product.getUpdatedDate(), storeId));
      product.setActivated(true);
      product.setViewable(true);
      for (ProductItem productItem : product.getProductItems()) {
        productItem.setActivated(true);
        productItem.setViewable(true);
      }
      // Update product level 1/2 here if become active
      this.productRepository.update(product, null, false, true, false, new ArrayList<>(), new HashedMap(),
          new ArrayList<>(), false, false, new ArrayList<>(), new ProductL3Response(), EditProductResponse.builder().build());

      // Create new products to wcs
      Page<ProductBusinessPartner> productBusinessPartners =
          this.productBusinessPartnerRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
              product.getStoreId(), product.getId(), PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE));
      for (ProductBusinessPartner productBusinessPartner : productBusinessPartners.getContent()) {
        Profile businessPartner =
            this.businessPartnerRepository
                .filterByCode(productBusinessPartner.getBusinessPartnerId());
        productBusinessPartner.setActivated(true);
        try {
          LOGGER.info("Saving product data in X-Product for productCode : {} in workflowForApprovedImage api:", product.getProductCode());
          this.productLevel3Service.create(businessPartner.getBusinessPartnerCode(), product,
              productBusinessPartner);
          LOGGER.info("Saving product data in X-Product for productCode : {} in workflowForApprovedImage api:", product.getProductCode());
          List<ProductLevel3Inventory> inventories =
              generateInventoryForInsert(product, productBusinessPartner, businessPartner);
          LOGGER.info("Saving item data in X-inventory for product code in workflowForApprovedImage api: {}", product.getProductCode());
          getProductLevel3InventoryService().insertInventory(inventories);
          LOGGER.info("Saving item data in X-inventory for product code in workflowForApprovedImage api: {}", product.getProductCode());
          productBusinessPartner.setMarkForDelete(true);
        } catch (Exception e) {
          LOGGER.error(
              "error invoking save new product to wcs at service in workflowForApprovedImage api for product code : {} ",
              product.getProductCode(), e);
        }
        this.productBusinessPartnerRepository.save(productBusinessPartner);
      }
      this.productWorkflowRepository.saveAndFlush(productWorkflow);
      active = true;
    }
    this.productHistoryRepository.saveAll(productHistories);
    return active;
  }

  private void workflowForDelete(ProductDetailResponse product, String notes) throws Exception {
    Page<ProductWorkflow> savedProductWorkflows =
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            product.getStoreId(), product.getId(), PageRequest.of(0, 10));
    ProductWorkflow productWorkflow =
        new ProductWorkflow(product.getId(), ProductWorkflowLookup.STATE_DELETE,
            ProductWorkflowLookup.STATE_DELETE_DESCRIPTION, notes, product.getUpdatedBy(),
            product.getUpdatedDate(), product.getStoreId());
    ProductHistory productHistory =
        new ProductHistory(product.getId(), ProductWorkflowLookup.STATE_DELETE,
            ProductWorkflowLookup.STATE_DELETE_DESCRIPTION, notes, product.getUpdatedBy(),
            product.getUpdatedDate(), product.getStoreId());
    this.productWorkflowRepository.deleteAll(savedProductWorkflows);
    this.productWorkflowRepository.saveAndFlush(productWorkflow);
    this.productHistoryRepository.saveAndFlush(productHistory);
  }

  private void workflowForSave(Product product) throws Exception {
    ProductWorkflow productWorkflow =
        new ProductWorkflow(product.getId(), ProductWorkflowLookup.STATE_DRAFT,
            ProductWorkflowLookup.STATE_DRAFT_DESCRIPTION, product.getCreatedBy(),
            product.getCreatedDate(), product.getStoreId());
    String historyNotes = ProductWorkflowLookup.STATE_CREATE_TO_DRAFT_NOTES;
    ProductHistory productHistory =
        new ProductHistory(product.getId(), ProductWorkflowLookup.STATE_DRAFT,
            ProductWorkflowLookup.STATE_DRAFT_DESCRIPTION, historyNotes.replace("\\{name\\}",
            product.getName()), product.getCreatedBy(), product.getCreatedDate(),
            product.getStoreId());
    this.productWorkflowRepository.saveAndFlush(productWorkflow);
    this.productHistoryRepository.saveAndFlush(productHistory);
  }

  private void workflowForSubmit(String storeId, Product product) throws Exception {
    Page<ProductWorkflow> savedProductWorkflows =
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId,
            product.getId(), PageRequest.of(0, 10));
    if (savedProductWorkflows.getContent().size() > 1) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "Product id : " + product.getId()
          + ". Error code : status product invalid: " + savedProductWorkflows.getSize()
          + ". Silakan refresh halaman dan coba kembali.");
    }
    ProductWorkflow savedProductWorkflow = null;
    for (ProductWorkflow productWorkflow : savedProductWorkflows.getContent()) {
      if (productWorkflow.getState() == 0) {
        savedProductWorkflow = productWorkflow;
      }
    }
    if (savedProductWorkflow == null) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "Product id : " + product.getId()
          + ". Error code : product state is "
          + savedProductWorkflows.getContent().get(0).getState()
          + "Silakan refresh halaman dan coba kembali.");
    }
    List<ProductWorkflow> productWorkflows = new ArrayList<ProductWorkflow>();
    productWorkflows.add(new ProductWorkflow(product.getId(),
        ProductWorkflowLookup.STATE_REVIEW_CONTENT,
        ProductWorkflowLookup.STATE_REVIEW_CONTENT_DESCRIPTION, product.getUpdatedBy(), product
        .getUpdatedDate(), storeId));
    productWorkflows.add(new ProductWorkflow(product.getId(),
        ProductWorkflowLookup.STATE_REVIEW_IMAGE,
        ProductWorkflowLookup.STATE_REVIEW_IMAGE_DESCRIPTION, product.getUpdatedBy(), product
        .getUpdatedDate(), storeId));
    List<ProductHistory> productHistories = new ArrayList<ProductHistory>();
    productHistories.add(new ProductHistory(product.getId(),
        ProductWorkflowLookup.STATE_REVIEW_CONTENT,
        ProductWorkflowLookup.STATE_REVIEW_CONTENT_DESCRIPTION,
        ProductWorkflowLookup.STATE_DRAFT_TO_REVIEW_CONTENT_NOTES, product.getUpdatedBy(), product
        .getUpdatedDate(), storeId));
    productHistories.add(new ProductHistory(product.getId(),
        ProductWorkflowLookup.STATE_REVIEW_IMAGE,
        ProductWorkflowLookup.STATE_REVIEW_IMAGE_DESCRIPTION,
        ProductWorkflowLookup.STATE_DRAFT_TO_REVIEW_IMAGE_NOTES, product.getUpdatedBy(), product
        .getUpdatedDate(), storeId));
    this.productWorkflowRepository.delete(savedProductWorkflow);
    this.productWorkflowRepository.saveAll(productWorkflows);
    this.productHistoryRepository.saveAll(productHistories);
  }

  public void setProductChangeUtil(ProductChangeUtil productChangeUtil) {
    this.productChangeUtil = productChangeUtil;
  }
  @Override
  public Page<Product> findByProductCodeExactMatch(String storeId, String productCode, Pageable
      pageable) throws Exception {
    return this.productRepository.findByStoreIdAndProductCodeExactMatch(storeId, productCode,
        pageable);
  }

  @Override
  public Integer getProductCountByViewable(String storeId, boolean viewable) throws Exception {
    if(storeId != null && StringUtils.isNotEmpty(storeId)) {
      return this.solrActiveProductCollectionRepository.getProductCountByStoreId(storeId);
    }
    else{
      throw new ApplicationRuntimeException(ErrorCategory.REQUIRED_PARAMETER, "storeId : " + storeId);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void processImage(String storeId, String productCode, boolean retryProcessImage)
      throws Exception {
    this.productWorkflowService.processImage(storeId, productCode, retryProcessImage);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void rejectProcessImage(String storeId, String productCode) throws Exception {
    this.productWorkflowService.rejectProcessImage(storeId, productCode);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public boolean approveImage(String storeId, String productCode, boolean retryApproveImage)
      throws Exception {
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            storeId, productCode);
    boolean skipReview = productCollection.isSkipReview();
    boolean isActive = false;
    if(!skipReview) {
      isActive = this.productWorkflowService.approveImage(storeId, productCode, retryApproveImage);
    }
    if (skipReview) {
      productCollection.setActivated(true);
      productCollection.setViewable(true);
      productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
      productCollection.setState("ACTIVE");
      productCollection.setReviewPending(false);
      productCollection.setRestrictedKeywordsPresent(false);
      productCollection.setAutoNeedRevision(false);
      productCollection.setNeedRevision(false);
      productCollection.setAutoNeedRevisionCount(0);
      productCollection.setAutoApprovalType(AutoApprovalType.NA);
      ProductDetailResponse productData =
          this.productRepository.findProductDetailByProductCode(productCode);
      this.productCollectionRepository.save(productCollection);
      this.productRepository.updateViewable(productCode, true);
      createProductLevel3(storeId, productData, false);
      pcbFeign.updateProductReviewPending(storeId, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCode, false);
      updateSolrProductCollectionDocument(productCollection);
      return true;
    }
    if (isActive) {
      productCollection.setActivated(true);
      productCollection.setViewable(true);
      productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
      productCollection.setState("ACTIVE");
      ProductDetailResponse productData =
          this.productRepository.findProductDetailByProductCode(productCode);
      this.productCollectionRepository.save(productCollection);
      updateSolrProductCollectionDocument(productCollection);
      this.productRepository.updateViewable(productCode, true);
      createProductLevel3(storeId, productData, false);
      return true;
    }
    return false;
  }

  @Trace(dispatcher=true)
  @Async
  @Override
  public void deleteImages(String storeId, String productCode) throws IOException {
    File folderImage =
        new File(imageSourceDirectory + File.separator + productCode);
    FileUtils.deleteDirectory(folderImage);
  }

  private void updateSolrProductCollectionDocument(ProductCollection productCollection) {
    if (productCollection.isActivated() && productCollection.isViewable()) {
      SolrProductCollectionDTO solrProductCollectionDTO = new SolrProductCollectionDTO();
      BeanUtils.copyProperties(productCollection, solrProductCollectionDTO);
      SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent =
          new SolrProductCollectionUpdateEvent(solrProductCollectionDTO);
      kafkaProducer.send(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE,
          solrProductCollectionUpdateEvent.getSolrProductCollectionDTO().getProductCode(),
          solrProductCollectionUpdateEvent);
    }
  }

  /**
   * updates product collection in solr
   *
   * @param productCollection
   */
  @Override
  public void updateSolrProductCollection(ProductCollection productCollection) {
    if (productCollection.isActivated() && productCollection.isViewable()) {
      SolrProductCollectionDTO solrProductCollectionDTO = new SolrProductCollectionDTO();
      BeanUtils.copyProperties(productCollection, solrProductCollectionDTO);
      this.productStatusPublisherService.publishSolrProductCollectionUpdateEvent(solrProductCollectionDTO);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public ActivateImageResponse updateProductImageName(ActivateImageRequest request)
      throws Exception {
    return this.productRepository.updateProductImageName(request);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public ActivateImageResponse updateProductImagesName(ProductActivateImageRequest request, boolean skipReview)
      throws Exception {
    return this.productOutbound.updateProductImagesName(request, skipReview);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void approveContent(String storeId, String productCode, boolean retryApproveContent,
      boolean isProductActivated) throws Exception {
    boolean isActive =
        this.productWorkflowService.approveContent(storeId, productCode, retryApproveContent);
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId,
            productCode);
    ProductDetailResponse productData =
        this.productRepository.findProductDetailByProductCode(productCode);
    generateProductCollection(productCollection.getBusinessPartnerCode(),
        productCollection.getBusinessPartnerName(), productData, productCollection);
    if (isActive) {
      productCollection.setActivated(true);
      productCollection.setViewable(true);
      productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
      productCollection.setState("ACTIVE");
    }
    updateProductBusinessPartnerInformation(storeId, productData);
    this.productCollectionRepository.save(productCollection);
    updateSolrProductCollectionDocument(productCollection);
    if (isActive) {
      this.productRepository.updateViewable(productCode, true);
      if (!isProductActivated) {
        createProductLevel3(storeId, productData, false);
      }
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void approveDraft(String storeId, String productCode) throws Exception {
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId,
            productCode);
    productCollection.setActivated(true);
    productCollection.setViewable(false);
    productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    this.productCollectionRepository.save(productCollection);
    this.productWorkflowService.submit(storeId, productCode);
    this.productRepository.updateActivated(productCode, true);
    itemService.publishItemStatusEvent(productCode, ProductStatus.ACTIVE);
  }

  @Async
  @Override
  public void notifyMerchantOfRecentApprovedProducts(Date startUpdatedDate, Date endUpdatedDate) {
    String storeId = mandatoryParameterHelper.getStoreId();
    LOGGER.info("Starting [notifyMerchantOfRecentApprovedProducts] for between {} and {} ",
        startUpdatedDate, endUpdatedDate);
    List<ProductCollection> productCollectionList =
        this.productCollectionRepository
            .findByStoreIdAndUpdatedDateBetweenAndActivatedAndViewableAndMarkForDeleteFalse(
                startUpdatedDate, endUpdatedDate, true, true);
    if (productCollectionList.isEmpty()) {
      return;
    }
    Map<String, List<ProductCollectionElement>> productCollectionUpdateByMap =
        getProductCollectionUpdateByMap(productCollectionList);
    BusinessPartnerCodesRequest businessPartnerCodesRequest =
        new BusinessPartnerCodesRequest(new ArrayList<>(productCollectionUpdateByMap.keySet()));
    List<List<String>> businessPartnerCodesPartitionList =
        Lists.partition(businessPartnerCodesRequest.getBusinessPartnerCodes(), PARTITION_SIZE);
    List<ProfileResponse> profileList = new ArrayList<>();
    for (List<String> businessPartnerCodeSubList : businessPartnerCodesPartitionList) {
      List<ProfileResponse> tempProfileList = new ArrayList<>();
      try {
        tempProfileList = this.businessPartnerRepository
            .filterDetailsByBusinessPartnerCodeList(new BusinessPartnerCodesRequest(businessPartnerCodeSubList));
      } catch (Exception e) {
        LOGGER
            .error("Error while fetching profile response list for business partners : {}", businessPartnerCodeSubList);
      }
      profileList.addAll(tempProfileList);
    }
    emailNotificationService.sendEmailForProductLive(storeId, profileList, productCollectionUpdateByMap);
    LOGGER.info(" end [notifyMerchantOfRecentApprovedProducts] at [{}] ", Calendar.getInstance()
        .getTime());
  }

  private Map<String, List<ProductCollectionElement>> getProductCollectionUpdateByMap(
      List<ProductCollection> productCollectionList) {
    Map<String, List<ProductCollectionElement>> productCollectionUpdateByMap = new HashMap<>();
    SimpleDateFormat defaultDateformat = new SimpleDateFormat("dd-MMM-yyyy HH:mm:ss");
    for (ProductCollection productCollection : productCollectionList) {
      List<ProductCollectionElement> productCollectionsTemp =
          productCollectionUpdateByMap.get(productCollection.getBusinessPartnerCode());
      if (productCollectionsTemp == null) {
        productCollectionsTemp = new ArrayList<>();
        productCollectionUpdateByMap.put(productCollection.getBusinessPartnerCode(),
            productCollectionsTemp);
      }
      ProductCollectionElement collectionElement =
          getProductCollectionElement(productCollection, defaultDateformat);
      productCollectionsTemp.add(collectionElement);
    }
    return productCollectionUpdateByMap;
  }

  private ProductCollectionElement getProductCollectionElement(ProductCollection productCollection,
      SimpleDateFormat defaultDateformat) {
    ProductCollectionElement collectionElement = new ProductCollectionElement();
    collectionElement.setProductCode(productCollection.getProductCode());
    collectionElement.setProductName(productCollection.getProductName());
    collectionElement.setUploader(productCollection.getCreatedBy());
    collectionElement.setCreatedDate(defaultDateformat.format(productCollection.getCreatedDate()));
    return collectionElement;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateProductContent(ProductRequest request) throws Exception {
    LOGGER.info("Product-workflow-tracker : update product content request sent to PCB : {}", request.getProductCode());
    request.setSpecificationDetail(CommonUtils.generateSpecificationDetail(request));
    ProductHistory productHistory = generateUpdateContentProcessProductHistory(request);
    this.productOutbound.updateProductContent(request, false);
    ProductDetailResponse productData =
        this.productRepository.findProductDetailByProductCode(request.getProductCode());
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(request.getStoreId(),
            request.getProductCode());
    generateProductCollection(productCollection.getBusinessPartnerCode(),
        productCollection.getBusinessPartnerName(), productData, productCollection);
    productCollection.setBrand(request.getBrand());
    productCollection.setBrandCode(request.getBrandCode());
    if (StringUtils.isNotEmpty(request.getBrandApprovalStatus())) {
      productCollection.setBrandApprovalStatus(BrandApprovalStatus.valueOf(request.getBrandApprovalStatus()));
    }
    productCollection.setReviewPending(request.isReviewPending());
    if (!request.isReviewPending()) {
      productCollection.setAutoApprovalType(AutoApprovalType.NA);
      productCollection.setAutoNeedRevision(false);
      productCollection.setNeedRevision(false);
      productCollection.setAutoNeedRevisionCount(0);
      productCollection.setRestrictedKeywordsPresent(false);
    }
    this.productCollectionRepository.saveAndFlush(productCollection);
    updateProductBusinessPartnerInformation(request.getProductCode(), productData);
    updateSolrProductCollectionDocument(productCollection);
    if(Objects.nonNull(productHistory)) {
      this.productHistoryRepository.save(productHistory);
    }
  }

  private ProductHistory generateUpdateContentProcessProductHistory(ProductRequest request)
      throws Exception {
    Calendar now = Calendar.getInstance();
    String username = GdnMandatoryRequestParameterUtil.getUsername();
    ProductDetailResponse productData =
        this.productRepository.findProductDetailByProductCode(request.getProductCode());
    String productContentDifference = null;
    try {
      List<ProductFieldHistory> productFieldHistories =
          ProductContentUtil.getProductContentDifference(request, productData);
      if(CollectionUtils.isEmpty(productFieldHistories)) {
        return null;
      }
      productContentDifference = StringUtils.abbreviate(productFieldHistories.toString(),
          GdnBaseLookup.PRODUCT_HISTORY_NOTES_MAX_VALUE);
    } catch (Exception e) {
      ProductServiceBean.LOGGER.error("error generate information of product content difference",
          e);
    }
    ProductHistory productHistory = new ProductHistory(productData.getId(),
        ProductWorkflowLookup.STATE_EDIT, ProductWorkflowLookup.STATE_EDIT_DESCRIPTION,
        productContentDifference, StringUtils.isBlank(username) ? request.getUpdatedBy() : username,
        now.getTime(), productData.getStoreId());
    productHistory.setUpdatedBy(StringUtils.isBlank(username) ? request.getUpdatedBy() : username);
    productHistory.setUpdatedDate(now.getTime());
    return productHistory;
  }

  @Override
  public void validateCategory(String requstId, String username, String bpCode, String categoryId) throws Exception {
    ProfileResponse response =
        this.businessPartnerRepository.filterDetailByBusinessPartnerCode(bpCode);

    if(response.getCategories().size() == 0){
      throw new Exception("Product category is not valid for business partner: " + bpCode + ". The granted category is null.");
    }

    GdnRestSingleResponse<SingleObjectResponse> categoryParent =
        this.categoryRepository.getFinalParentCategoryCached(requstId, username, categoryId);

    Boolean isValidCategory = false;
    for(String masterCategoryCode : response.getCategories()){
      if(masterCategoryCode.equals(categoryParent.getValue().getValue())){
        isValidCategory = true;
        break;
      }
    }

    if(! isValidCategory){
      throw new Exception("Product category is not valid for business partner: " + bpCode +
          " and category code: " + categoryId);
    }
  }

  @Override
  public void publishToPDTByProductCode(String storeId, String productCode) throws Exception {
    try {
      LOGGER.info("Invoke PDT Publisher {}", productCode);
      ProductCollection productCollection =
          this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
      checkArgument(Objects.nonNull(productCollection), PRODUCT_COLLECTION_DATA_NOT_FOUND + productCode);
      publishToPDTByProductCollection(productCollection);
    } catch(Exception e){
      LOGGER.error("Publish product code {} error", productCode, e);
    }
  }

  @Override
  public void publishToPDTByProductCollection(ProductCollection productCollection) throws Exception {
    boolean trustedSeller = Optional.ofNullable(
            businessPartnerRepository.filterDetailByBusinessPartnerCode(productCollection.getBusinessPartnerCode()))
        .map(ProfileResponse::isTrustedSeller).orElse(false);
    if (productCollection.isNeedRevision() || productCollection.getResubmitCount() > 0) {
      log.info("Publishing revision event to PDT. productCode : {} ", productCollection.getProductCode());
      ProductBusinessPartner productBusinessPartner =
          productBusinessPartnerService.findFirstByStoreIdAndProductId(productCollection.getStoreId(),
              productCollection.getProductId());
      productPublisherService.publishRevisedProductToPDT(
          ConverterUtil.toAddRevisedProductToPDTEvent(productCollection, Constants.DEFAULT_USERNAME,
              trustedSeller, productBusinessPartner, false, null,
              priceInfoVendorRevisedEnabled, priceInfoMaxVariantLimit));
    } else if (productCollection.isEdited()) {
      log.info("Publishing edit event to PDT. productCode : {} ", productCollection.getProductCode());
      publishAddEditedProductToPDTEvent(productCollection.getStoreId(), productCollection.getReviewType(),
          productCollection, new ArrayList<>());
    } else {
      log.info("Publishing screening approval event to PDT. productCode : {} ", productCollection.getProductCode());
      this.productPublisherService.publish(productCollection.getProductCode(),
          productCollection.getBusinessPartnerCode(), productCollection.getBusinessPartnerName(),
          GdnMandatoryRequestParameterUtil.getUsername(), productCollection.isPostLive(),
          productCollection.isRestrictedKeywordsPresent(), productCollection.getRestrictedKeywordsDetected(),
          productCollection.getPrioritySeller(), trustedSeller, productCollection.getProductId(),
          productCollection);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateProductImage(ProductRequest request) throws Exception {
    LOGGER.info("Product-workflow-tracker : update product image details sent to PCB : {}", request.getProductCode());
    ProductDetailResponse productData =
        this.productRepository.findProductDetailByProductCode(request.getProductCode());
    ProductHistory productHistory =
        new ProductHistory(productData.getId(), ProductWorkflowLookup.STATE_EDIT,
            ProductWorkflowLookup.STATE_EDIT_DESCRIPTION, null, null, productData.getStoreId());
    request.setVersion(productData.getVersion());
    this.productRepository.updateProductImage(request);
    this.productHistoryRepository.save(productHistory);
  }

  @Override
  public GdnRestListResponse<ProductDetailResponse> getProductDetailsByProductCodes(
      String requestId, String username, List<String> productCodeList) throws Exception {
    return this.productRepository.getProductDetailsByProductCodes(requestId, username, productCodeList);
  }

  @Override
  public Page<ProductItem> findByProductItemNameAndCategoryId(String storeId,
      String productItemName, String categoryId, Pageable pageable) throws Exception {
    return this.productItemRepository.findByStoreIdAndProductItemNameAndCategoryId(storeId,
        productItemName, categoryId, pageable);
  }

  @Override
  public void activateAndUpdateImageName(ActivateImageRequest request) throws Exception {
    this.productRepository.activateAndUpdateImageName(request);
  }

  @Override
  public ActivateImageResponse isProductImagesActivated(String productCode) throws Exception {
    return this.productRepository.isProductImagesActivated(productCode);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void createProductLevel3(String productCode, boolean isSkipNotification) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductDetailResponse productData = this.productRepository.findProductDetailByProductCode(productCode);
    this.createProductLevel3(storeId, productData, isSkipNotification);
  }

  @Override
  public void updateRejectedProduct(ProductRequest productRequest) throws Exception {
    this.productRepository.updateRejectedProduct(productRequest);
  }

  @Override
  public Map<String, Object> getCategoryHierarchyByProductNameOrProductCode(String storeId,
      String keyword, Pageable pageable, String businessPartnerCode) throws Exception {
    Integer rows = Integer.parseInt(this.applicationProperties.getRowsCategoryCodeSolrSearch());
    Set<SolrCategoryCodeDTO> solrCategoryCodeDTOS = null;
    if(!StringUtils.isEmpty(keyword)) {
      solrCategoryCodeDTOS = this.solrActiveProductCollectionRepository
          .getCategoryCodesSolrByKeyword(keyword, rows, businessPartnerCode);
    }
    Map<String, List<CategoryHierarchyResponse>> categoryHierarchyMap = new LinkedHashMap<>();
    Map<String, Object> datas = new HashMap<>();
    List<CategoryHierarchyResponse> categoryResult = new ArrayList<>();
    if(CollectionUtils.isNotEmpty(solrCategoryCodeDTOS)){
      for (SolrCategoryCodeDTO categoryCode : solrCategoryCodeDTOS) {
        categoryHierarchyMap.put(categoryCode.getCategoryCode(), new ArrayList<>());
        datas.put(categoryCode.getCategoryCode(), categoryCode.getProductCount());
      }
      CategoryCodeRequest request = new CategoryCodeRequest();
      request.setCategoryCodes(new ArrayList<>());
      List<String> categoryList = solrCategoryCodeDTOS.stream().map(dto -> dto.getCategoryCode()).collect(Collectors.toList());
      request.getCategoryCodes().addAll(categoryList);
      List<CategoryHierarchyResponse> categoryHierarchyResponses = this.categoryRepository.findHierarchyByCategoryCodes(request);
      for (CategoryHierarchyResponse categoryHierarchyResponse : categoryHierarchyResponses) {
        categoryHierarchyMap.get(categoryHierarchyResponse.getCategoryCode()).add(categoryHierarchyResponse);
      }
      for (List<CategoryHierarchyResponse> categoryHierarchyList : categoryHierarchyMap.values()) {
        categoryResult.addAll(categoryHierarchyList);
      }
      datas.put("result", categoryResult);
    }
    return datas;
  }

  public void setApplicationProperties(ApplicationProperties applicationProperties) {
    this.applicationProperties = applicationProperties;
  }

  @Override
  public Page<ProductItemResponse> getProductItemByKeywordAndCategoryCode(String keyword,
      String categoryCode, Pageable pageable, Boolean isOnlyExternal) throws Exception {
    SolrProductCodeDTO solrProductCodeDTO = null;
    if (!StringUtils.isEmpty(keyword) && !StringUtils.isEmpty(categoryCode)) {
      solrProductCodeDTO = this.solrActiveProductCollectionRepository
          .getProductCodesFromSolrByKeywordAndCategoryCode(keyword, categoryCode, pageable);
    }
    return findProductItemsByProductCodes(pageable, isOnlyExternal, solrProductCodeDTO);
  }

  @Override
  public Page<ProductItemResponse> findProductItemByKeywordAndCategoryCodes(String keyword, List<String> categoryCodes,
      Pageable pageable, Boolean isOnlyExternal) throws Exception {
    SolrProductCodeDTO solrProductCodeDTO = this.solrActiveProductCollectionRepository
        .findProductCodesByKeywordAndCategoryCodes(keyword, categoryCodes, pageable);
    return findProductItemsByProductCodes(pageable, isOnlyExternal, solrProductCodeDTO);
  }

  private Page<ProductItemResponse> findProductItemsByProductCodes(Pageable pageable, Boolean isOnlyExternal,
      SolrProductCodeDTO solrProductCodeDTO) throws Exception {
    Map<String, List<ProductItemResponse>> productCodeMap = new LinkedHashMap<>();
    List<String> productCodeList = new ArrayList<>();
    if (!CollectionUtils.isEmpty(solrProductCodeDTO.getProductCodes())) {
      productCodeList.addAll(solrProductCodeDTO.getProductCodes());
      for (String productCode : solrProductCodeDTO.getProductCodes()) {
        productCodeMap.put(productCode, new ArrayList<>());
      }
    }
    List<ProductItemDetailResponse> productItemDetailResponses =
        this.productRepository.getProductItemByListOfProductCode(productCodeList, isOnlyExternal, true);
    if (!CollectionUtils.isEmpty(productItemDetailResponses)) {
      for (ProductItemDetailResponse product : productItemDetailResponses) {
        if (productCodeMap.get(product.getProductResponse().getProductCode()) != null) {
          ProductItemResponse productItem = new ProductItemResponse();
          BeanUtils.copyProperties(product, productItem);
          productCodeMap.get(product.getProductResponse().getProductCode()).add(productItem);
        }
      }
    }
    List<ProductItemResponse> list = new ArrayList<>();
    for (List<ProductItemResponse> values : productCodeMap.values()) {
      list.addAll(values);
    }
    long total = list.size();
    if (solrProductCodeDTO.getTotalCount() != null && solrProductCodeDTO.getTotalCount() > list.size()) {
      total = solrProductCodeDTO.getTotalCount();
    }
    return new PageImpl<ProductItemResponse>(list, pageable, total);
  }

  @Override
  public Page<ProductItemDetailResponse> getProductItemDetailByKeywordAndCategoryCodes(String keyword,
      List<String> categoryCodes, Pageable pageable, Boolean isOnlyExternal) throws Exception {
    try {
      checkArgument(!(StringUtils.isEmpty(keyword)), "Product name can't be null/empty");
      SolrProductCodeDTO solrProductCodeDTO = this.solrActiveProductCollectionRepository
          .findProductCodesByKeywordAndCategoryCodes(keyword, categoryCodes, pageable);
      if(CollectionUtils.isEmpty(solrProductCodeDTO.getProductCodes())){
        return new PageImpl<ProductItemDetailResponse>(new ArrayList<>(), pageable, 0);
      }
      List<String> productCodeList = new ArrayList<>();
      productCodeList.addAll(solrProductCodeDTO.getProductCodes());
      List<ProductItemDetailResponse> productItemDetailResponses =
          this.productRepository.getProductItemByListOfProductCode(productCodeList, isOnlyExternal, false);
      long total = 0;
      if (!CollectionUtils.isEmpty(productItemDetailResponses)) {
        if (solrProductCodeDTO.getTotalCount() != null
            && solrProductCodeDTO.getTotalCount() > productItemDetailResponses.size()) {
          total = solrProductCodeDTO.getTotalCount();
        } else {
          total = productItemDetailResponses.size();
        }
      } else {
        productItemDetailResponses = new ArrayList<>();
      }
      return new PageImpl<ProductItemDetailResponse>(productItemDetailResponses, pageable, total);
    } catch (Exception e) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, e.getMessage());
    }
  }

  @Override
  public void clearMasterProductCache(String productCode) throws Exception {
    ProductDetailResponse product = this.productRepository.findProductDetailByProductCode(productCode);
    if(product!=null) {
      this.productRepository.clearMasterProductCache(product.getProductCode(), product.getId());
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, productCode + " not found !");
    }
  }

  @Override
  public void clearMasterProductCacheSync(String productId, String productCode) throws Exception {
    this.productRepository.clearMasterProductCacheSync(productId, productCode);
  }

  @Override
  public ProductCollectionCountResponse countProductCollectionBySpecifiedDateRange(
      ProductCollectionCountRequest request) throws SolrServerException, IOException {

    List<String> stateList = request.isActivated() ? NOT_IN_PROGRESS_STATE : Collections.singletonList(STATE_DRAFT);
    request.setBusinessPartnerCode(StringUtils.isEmpty(request.getBusinessPartnerCode()) ?
        StringUtils.EMPTY :
        request.getBusinessPartnerCode().trim());
    request.setCategoryCode(
        StringUtils.isEmpty(request.getCategoryCode()) ? StringUtils.EMPTY : request.getCategoryCode().trim());
    request.setKeyword((StringUtils.isEmpty(request.getKeyword()) ? StringUtils.EMPTY : request.getKeyword().trim()));
    try {
      return solrReviewProductCollectionService
          .getInReviewProducts(request.getStoreId(), request.getKeyword(), request.getBusinessPartnerCode(),
              request.getCategoryCode());
    } catch (SolrServerException | IOException | SolrException exception) {
      LOGGER.error("Error while fetching in review products count from solr ", exception);
      throw  exception;
//      return this.productCollectionRepository
//          .countAllDetailsForInProgressProducts(stateList, request.getCategoryCode(), request.getStoreId(),
//              request.getKeyword(), request.getBusinessPartnerCode());
//       Removing this as this query on db would be expensive
    }
  }

  @Override
  public ProcessImageDomainEvent publishProcessImageRequest(ProductRequest productRequest, String message) {
    LOGGER.info("publish ProcessImage Request for product: {} and message : {} ,  to MTA",
        productRequest.getProductCode(), message);
    ProcessImageDomainEvent processImageDomainEvent = new ProcessImageDomainEvent(productRequest, message);
    kafkaProducer.send(DomainEventName.PRODUCT_PROCESS_IMAGE_REQUEST,
        processImageDomainEvent.getProductRequest().getProductCode(), processImageDomainEvent);
    return processImageDomainEvent;
  }

  @Override
  public ApproveProductResponse publishProductDetailsEvent(String productCode, String message) throws Exception {
    LOGGER.info("publish approve image response with product code {} and message : {} ,  to MTA", productCode, message);
    ProductResponse productResponse = ApproveImageMessageConstants.EMPTY_MESSAGE_RECEIVED.equals(message) ?
        new ProductResponse() :
        findProductBasicDetailByProductCode(productCode);
    ApproveProductResponse approveProductResponse = new ApproveProductResponse(message, productResponse);
    kafkaProducer.send(DomainEventName.PRODUCT_APPROVE_IMAGE_RESPONSE, approveProductResponse.getProductResponse().getProductCode(),
        approveProductResponse);
    return approveProductResponse;
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public void saveProductHistory(String productCode, ProductHistory productHistory)
      throws ApplicationException {
    ProductCollection productCollection = this.productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(productHistory.getStoreId(), productCode);
    if (productCollection == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Product collection data is not found for: " + productCode);
    }
    productHistory.setProductId(productCollection.getProductId());
    this.productHistoryRepository.saveAndFlush(productHistory);
  }

  @Override
  public ProductWipDeleteResponse publishProductWipDeleteEvent(String productCode, String updatedBy,
      String notes) {
    ProductWipDeleteResponse productWipDeleteResponse = new ProductWipDeleteResponse(productCode, updatedBy, notes);
    kafkaProducer
        .send(DomainEventName.PRODUCT_DELETE_EVENT_REQUEST, productWipDeleteResponse.getProductCode(), productWipDeleteResponse);
    return productWipDeleteResponse;
  }

  @Override
  public String generateProductCode() {
    return getProductCodeNextSequence();
  }

  @Override
  public void deleteProductInSolrProductCollectionByIds(List<String> ids) throws Exception {
    checkArgument(CollectionUtils.isNotEmpty(ids), ErrorMessages.ID_LIST_MUST_NOT_BE_NULL_OR_EMPTY);
    List<String> idList = new ArrayList<>();
    for (String id : ids) {
      if (StringUtils.isNotEmpty(id)){
        idList.add(id);
      } else {
        log.error("Deleting product in prd_collection by Id. Id must not be null or empty. id : {} ", id);
      }
    }
    if (CollectionUtils.isNotEmpty(idList)) {
      try {
        for (String id : idList) {
          SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent =
              CommonUtils.getSolrProductCollectionUpdateEvent(id);
          kafkaProducer.send(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE, id, solrProductCollectionUpdateEvent);
        }
      } catch (Exception e) {
        LOGGER.error(
            "error while deleting productCollection solr document from solr core: " + "prd_collection. Id: {} ", ids,
            e);
      }
    }
  }

  @Override
  public List<ProductCollectionResponse> getProductCollectionsWithUpdatedFlags(String storeId,
      Page<ProductCollection> productCollections) throws Exception {
    List<ProductCollectionResponse> productCollectionResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productCollections.getContent())) {
      Map<String, ProductWorkflowStatus> productWorkflowMap = new HashMap<>();
      List<String> productCodeList = productCollections.getContent().stream().map(p->p.getProductCode())
          .collect(Collectors.toList());
      List<ProductWorkflowStatus> productWorkflowStatusList =
          this.productWfService.getProductWorkFlowByProductCodes(productCodeList);
      if (CollectionUtils.isNotEmpty(productWorkflowStatusList)) {
        productWorkflowMap = productWorkflowStatusList.stream().collect(Collectors
            .toMap(ProductWorkflowStatus::getProductCode, Function.identity(),
                (oldValue, newValue)->oldValue));
      }
      convertToProductCollectionResponses(productCollections.getContent(), productCollectionResponses);
      updateProductCollectionState(productCollectionResponses, productWorkflowMap);
    }
    return productCollectionResponses;
  }

  @Override
  public List<ProductCollectionResponse> getProductsByProductCodesInAndActivatedTrueAndViewableTrue(
      String storeId, List<ProductCollection> productCollections) {
    List<ProductCollectionResponse> productCollectionResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productCollections)) {
      List<String> productCodes = productCollections.stream().map(ProductCollection::getProductCode)
          .collect(Collectors.toList());
      productCollections = productCollectionRepository
          .findByProductCodeInAndStoreIdAndActivatedTrueAndViewableTrueAndMarkForDeleteFalse(
              productCodes, storeId);
      convertToProductCollectionResponses(productCollections, productCollectionResponses);
    }
    return productCollectionResponses;
  }

  @Override
  public List<String> getActiveProductCodesFromSolr(String storeId, String keyword, String categoryCode,
      Boolean reviewPending, String sortBy, Pageable pageable) throws Exception {
    return this.solrActiveProductCollectionRepository
        .getProductCodesListFromSolr(storeId, keyword, categoryCode, reviewPending, sortBy, pageable);
  }

  /**
   * convert to ProductCollectionResponse list from productCollection List
   *
   * @param productCollections
   * @return
   */
  private void convertToProductCollectionResponses(List<ProductCollection> productCollections,
      List<ProductCollectionResponse> productCollectionResponses) {
    for (ProductCollection productCollection : productCollections) {
      ProductCollectionResponse productCollectionResponse = new ProductCollectionResponse();
      BeanUtils.copyProperties(productCollection, productCollectionResponse);
      productCollectionResponses.add(productCollectionResponse);
    }
  }


  /**
   * State of product collection response are updated
   *
   * @param productWorkflowMap must not null
   * @param productCollectionResponses must not null
   * @return ProductCollectionResponse
   */
  private void updateProductCollectionState(List<ProductCollectionResponse> productCollectionResponses,
      final Map<String, ProductWorkflowStatus> productWorkflowMap) {
    if (MapUtils.isNotEmpty(productWorkflowMap)) {
      productCollectionResponses.stream().forEach(productCollectionResponse -> {
        ProductWorkflowStatus productWorkflowStatus =
            productWorkflowMap.get(productCollectionResponse.getProductCode());
        if (!productWorkflowStatus.getStates().contains(WorkflowStates.CONTENT_APPROVED.getValue())) {
          productCollectionResponse.setContentApproved(Boolean.FALSE);
        }
        if (!productWorkflowStatus.getStates().contains(WorkflowStates.IMAGE_APPROVED.getValue())) {
          productCollectionResponse.setImageApproved(Boolean.FALSE);
        }
        if (productWorkflowStatus.getStates().contains(WorkflowStates.PROCESS_IMAGE.getValue())) {
          productCollectionResponse.setProcessImage(Boolean.TRUE);
        }
      });
    }
  }

  /**
   * Get PDTDomainEventModel from PDT by productCode
   *
   * @param productWfStateResponse
   */
  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public StuckProductEventPublishDto getPDTEventDomainModel(ProductWfStateResponse productWfStateResponse) {
    try {
      LOGGER.info("Retrying product activation for productCode : {} ", productWfStateResponse.getProductCode());
      ProductCollection productCollection =
          productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
              productWfStateResponse.getProductCode());
      productOutbound.clearProductCacheSyncByProductIdAndProductCode(productCollection.getProductId(),
          productCollection.getProductCode());
      PDTProductDomainEventModel pdtProductDomainEventModel = null;
      try {
        pdtProductDomainEventModel = productDistributionTaskRepositoryBean
            .getPDTDomainModelResponseByCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
                productWfStateResponse.getProductCode());
      } catch (Exception e) {
        LOGGER.info("Product not found in PDT productCode : {} ", productWfStateResponse.getProductCode());
      }
      boolean trustedSeller = Optional.ofNullable(
          businessPartnerRepository.filterDetailByBusinessPartnerCode(
            productCollection.getBusinessPartnerCode())).map(ProfileResponse::isTrustedSeller)
        .orElse(false);
      if (productCollection.isNeedRevision() && Objects.nonNull(pdtProductDomainEventModel)) {
        log.info("[Retry-product-activation] PDT status update for revised productCode : {}", productCollection.getProductCode());
        if (!WorkflowState.NEED_CORRECTION.name().equalsIgnoreCase(pdtProductDomainEventModel.getState().name())) {
          productDistributionTaskRepositoryBean.productRetryStatusUpdate(pdtProductDomainEventModel.getProductCode(),
              ProductRetryStatusUpdate.builder().state(WorkflowState.NEED_CORRECTION.name()).markForDelete(true)
                  .reviewType(Objects.nonNull(pdtProductDomainEventModel.getReviewType()) ?
                      pdtProductDomainEventModel.getReviewType().name() :
                      ReviewType.CONTENT.name())
                  .revised(pdtProductDomainEventModel.isRevised()).edited(pdtProductDomainEventModel.isEdited())
                  .build());
          log.info("[Retry-product-activation] PDT status update for productCode : {}", productCollection.getProductCode());
        }
        ProductBusinessPartner productBusinessPartner = productBusinessPartnerService.findFirstByStoreIdAndProductId(
            productCollection.getStoreId(), productCollection.getProductId());
        return StuckProductEventPublishDto.builder().addRevisedProductToPDTEvent(
            ConverterUtil.toAddRevisedProductToPDTEvent(productCollection,
                Constants.DEFAULT_USERNAME, trustedSeller, productBusinessPartner, false, null,
                priceInfoVendorRevisedEnabled, priceInfoMaxVariantLimit)).build();
      } else if (productCollection.isEdited() && Objects.nonNull(pdtProductDomainEventModel)) {
        log.info("[Retry-product-activation] PDT status update for edited productCode : {}", productCollection.getProductCode());
        if (Objects.isNull(pdtProductDomainEventModel.getReviewType())) {
          productDistributionTaskRepositoryBean.productRetryStatusUpdate(pdtProductDomainEventModel.getProductCode(),
              ProductRetryStatusUpdate.builder().state(WorkflowState.IN_REVIEW.name()).markForDelete(false)
                  .reviewType(ReviewType.CONTENT.name()).revised(pdtProductDomainEventModel.isRevised())
                  .edited(pdtProductDomainEventModel.isEdited()).build());
          log.info("[Retry-product-activation] PDT status update for productCode : {}", productCollection.getProductCode());
        }
        return StuckProductEventPublishDto.builder().editedPublishProductCollection(productCollection).build();
      } else if (!productCollection.isPostLive() && Objects.isNull(pdtProductDomainEventModel)) {
        log.info("[Retry-product-activation] PDT no entry for productCode : {}", productCollection.getProductCode());
        this.productWfService.deleteAllExistingWorkFlowAndCreateNewState(productCollection.getStoreId(),
            productCollection.getProductCode(), WorkflowStates.IN_VENDOR.getValue());
        setViewableFlagToFalseInPCB(productCollection.getProductCode());
        log.info("[Retry-product-activation] PCB viewable flag update for productCode : {}", productCollection.getProductCode());
        return StuckProductEventPublishDto.builder().productCollection(productCollection).trustedSeller(trustedSeller).build();
      } else {
        if (Objects.nonNull(pdtProductDomainEventModel) && !pdtProductDomainEventModel.isMarkForDelete()) {
          log.info("[Retry-product-activation] PDT entry present for productCode : {}", productCollection.getProductCode());
          this.productWfService.deleteAllExistingWorkFlowAndCreateNewState(productCollection.getStoreId(),
              productCollection.getProductCode(), WorkflowStates.IN_VENDOR.getValue());
          setViewableFlagToFalseInPCB(productCollection.getProductCode());
          log.info("[Retry-product-activation] PCB viewable flag update for productCode : {}", productCollection.getProductCode());
          if (WorkflowState.PASSED.name().equalsIgnoreCase(pdtProductDomainEventModel.getState().name())) {
            productDistributionTaskRepositoryBean.productRetryStatusUpdate(pdtProductDomainEventModel.getProductCode(),
                ProductRetryStatusUpdate.builder().state(WorkflowState.IN_REVIEW.name()).markForDelete(false)
                    .reviewType(Objects.nonNull(pdtProductDomainEventModel.getReviewType()) ?
                        pdtProductDomainEventModel.getReviewType().name() :
                        ReviewType.CONTENT.name()).revised(pdtProductDomainEventModel.isRevised())
                    .edited(pdtProductDomainEventModel.isEdited()).build());
            log.info("[Retry-product-activation] PDT status update for productCode : {}", productCollection.getProductCode());
          }
          return StuckProductEventPublishDto.builder().vendorApprovalProductCollection(productCollection).build();
        } else {
          log.info("[Retry-product-activation] default block for productCode : {}", productCollection.getProductCode());
          // check for two time screening approval event
          this.productWfService.deleteAllExistingWorkFlowAndCreateNewState(productCollection.getStoreId(),
              productCollection.getProductCode(), WorkflowStates.IN_VENDOR.getValue());
          setViewableFlagToFalseInPCB(productCollection.getProductCode());
          log.info("[Retry-product-activation] PCB viewable flag update for productCode : {}", productCollection.getProductCode());
          return StuckProductEventPublishDto.builder().productCollection(productCollection).trustedSeller(trustedSeller)
              .build();
        }
      }
    } catch (Exception e) {
      LOGGER.error("Error: getProductDomainEventModel failed for productCode : {} ",
          productWfStateResponse.getProductCode(), e);
    }
    return new StuckProductEventPublishDto();
  }

  private void setViewableFlagToFalseInPCB(String productCode) throws Exception {
    if (unsetViewableFlagInPCB) {
      productRepository.updateViewable(productCode, false);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void saveProductHistory(String storeId, String productCode, String username, String activity,
      String notes) {
    final Calendar now = Calendar.getInstance();
    final ProductHistory productHistory = new ProductHistory();
    productHistory.setDescription(activity);
    productHistory.setNotes(notes);
    productHistory.setState(5);
    productHistory.setStoreId(storeId);
    productHistory.setCreatedBy(username);
    try {
      this.saveProductHistory(productCode, productHistory);
    } catch (Exception e) {
      LOGGER.error("error while saveProductHistory with productCode: {} ", productCode, e);
    }
  }

  @Override
  public Page<ProductCollection> getProductsByStoreIdAndActivatedAndViewable(String storeId, boolean activated,
      boolean viewable, Pageable pageable) {
    return productCollectionRepository
        .findByStoreIdAndActivatedAndViewableAndMarkForDeleteFalseAndStateAndImageResizedTrueOrderByProductCodeAsc(storeId, activated,
            viewable, WorkflowStates.DRAFT.getValue(), pageable);
  }


  @Override
  public Page<ProductCollection> getProductsByStoreIdAndActivatedAndReviewPending(String storeId, boolean activated,
      boolean reviewPending, Pageable pageable) {
    return productCollectionRepository
        .findByStoreIdAndActivatedAndReviewPendingAndMarkForDeleteFalseAndImageResizedTrueOrderByProductCodeAsc(storeId,
            activated, reviewPending, pageable);
  }

  @Override
  public Page<ProductCollection> getProductsByStoreIdAndUpdatedDateBetween(String storeId, Date startDate, Date endDate,
      Pageable pageable) {
    return productCollectionRepository.findByStoreIdAndImageResizedTrueAndUpdatedDateBetweenOrderByProductCodeAsc(
        storeId, startDate, endDate, pageable);
  }

  @Override
  public Page<ProductCollection> getProductByStoreIdAndProductCode(String storeId, String productCode,
      Pageable pageable) {
    ProductCollection productCollection = productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    return new PageImpl<ProductCollection>(Arrays.asList(productCollection), pageable, 1);
  }

  @Override
  public void reindexActiveProductCollectionByStoreIdAndProductCode(String storeId, String productCode) {
    ProductCollection productCollection = productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    updateSolrProductCollection(productCollection);
  }

  @Override
  @Transactional(readOnly = false)
  public void assignProducts(String storeId, List<String> productCodes, String assignedTo, String assignedBy) {
    int success = productCollectionRepository.updateAssignmentStatus(storeId, productCodes, assignedTo, assignedBy);
    if (success == 0) {
      if (SolrConstants.ASSIGNED_TO_PREFIX.equalsIgnoreCase(assignedTo)) {
        throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, FAILED_TO_UN_ASSIGN_PRODUCTS);
      } else {
        throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, FAILED_TO_ASSIGN_PRODUCTS);
      }
    }
  }

  @Override
  public ProductCollection updateImagePathsAndFlagAfterResizingImage(
      ProductAndItemImageRequest productAndItemImageRequest, ProductCollection productCollection, boolean resize)
      throws Exception {
    productCollection.setImageResized(resize);
    productRepository.updateProductAndItemImages(productAndItemImageRequest);
    return productCollection;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public boolean updateImagePathsAndFlagOnResizingImageFailure(BulkImageProcessResponse bulkImageProcessResponse)
      throws Exception {
    ProductDetailResponse productDetailResponse =
        this.findProductDetailByProductCode(bulkImageProcessResponse.getGroupCode(), false);
    if (Objects.nonNull(productDetailResponse)) {
      try {
        ProductAndItemImageRequest productAndItemImageRequest =
            ConverterUtil.toProductAndItemImageRequestOnImageResizingFailure(productDetailResponse);
        ProductCollection productCollection = productCollectionRepository
            .findByStoreIdAndProductCodeAndMarkForDeleteFalse(bulkImageProcessResponse.getStoreId(),
                bulkImageProcessResponse.getGroupCode());
        ProfileResponse profileResponse =
          businessPartnerRepository.filterDetailByBusinessPartnerCode(
            productCollection.getBusinessPartnerCode());
        Boolean trustedSeller =
          Optional.ofNullable(profileResponse).map(ProfileResponse::isTrustedSeller).orElse(false);
        if (!productCollection.isImageResized()) {
        productRepository.updateProductAndItemImages(productAndItemImageRequest);
        ImageQcEnableAndSyncResponse imageQcEnableAndSyncResponse =
            getImageQcStatus(bulkImageProcessResponse.getStoreId(), productDetailResponse.getProductCode(),
                productDetailResponse.getCategoryCodes().get(0));
          List<RestrictedKeywordsByField> restrictedKeywordsByFieldList = new ArrayList<>();
        if(!imageQcEnableAndSyncResponse.isSync()) {
          restrictedKeywordsByFieldList = productLevel3Helper
              .getRestrictedKeywordsInProductDetails(productDetailResponse, productCollection.getCategoryCode());
          productCollection.setRestrictedKeywordsPresent(!CollectionUtils.isEmpty(restrictedKeywordsByFieldList));
          productCollection.setRestrictedKeywordsDetected(objectMapper.writeValueAsString(restrictedKeywordsByFieldList));
          sendNotificationForRestrictedPostLiveProduct(productCollection, !CollectionUtils.isEmpty(
              restrictedKeywordsByFieldList), profileResponse);
        }
        productCollection.setImageResized(!imageQcEnableAndSyncResponse.isSync());
        if (productCollection.isPostLive()) {
          productCollection.setPostLive(false);
          updateHistoryOnConfigChanges(productCollection);
        }
        if (imageQcEnableAndSyncResponse.isEnable()) {
          publishImageQcEvent(bulkImageProcessResponse, productDetailResponse,
              new RestrictedKeywordsByFieldAndActionType(restrictedKeywordsByFieldList));
          productCollection.setImageQcState(1);
        }
        productCollectionRepository.saveAndFlush(productCollection);
        if (!imageQcEnableAndSyncResponse.isSync()) {
          this.updateSolrOrPublishEvent(productCollection, trustedSeller);
        }
        this.publishProductStatusEvent(productDetailResponse, productCollection, ProductStatus.CREATED,
            StringUtils.EMPTY);
        }
      } catch (Exception e) {
        LOGGER.error("Exception caught while updating image location paths after resizing, productCode :{}",
            bulkImageProcessResponse.getGroupCode(), e);
        return false;
      }
    }
    return true;
  }

  @Override
  public ImageQcEnableAndSyncResponse getImageQcStatus(String storeId, String productCode, String parentCategoryCode) {
    ImageQcEnableAndSyncResponse imageQcEnableAndSyncResponse = new ImageQcEnableAndSyncResponse();
    try {
      boolean imageQcEnable = imageQcEnabledForThisProductCategory(storeId, parentCategoryCode);
      imageQcEnableAndSyncResponse.setEnable(imageQcEnable);
      imageQcEnableAndSyncResponse.setSync(imageQcEnable && Boolean.parseBoolean(productSystemParameterService
          .findByStoreIdAndVariable(storeId, SystemParameterConstants.PRODUCT_IMAGE_QC_SYNC).getValue()));
    } catch (Exception e) {
      LOGGER.error("Exception caught while trying to get values for imageQc for productCode : {} ", productCode, e);
    }
    return imageQcEnableAndSyncResponse;
  }

  private boolean imageQcEnabledForThisProductCategory(String storeId, String parentCategoryCode) {
    ProductSystemParameter imageQcEnabled = productSystemParameterService
        .findByStoreIdAndVariable(storeId, SystemParameterConstants.PRODUCT_IMAGE_QC_ENABLED);
    if (Boolean.valueOf(imageQcEnabled.getValue())) {
      String imageQcSkipCategoryCodeResponse = productSystemParameterService
          .findByStoreIdAndVariable(storeId, SystemParameterConstants.PRODUCT_IMAGE_QC_CATEGORY_LIST).getValue();
      if (imageQcSkipCategoryCodeResponse.equalsIgnoreCase(SystemParameterConstants.CATEGORY_CODE_ALL)) {
        return true;
      }
      return Arrays.asList(imageQcSkipCategoryCodeResponse.split(Constants.COMMA)).contains(parentCategoryCode);
    }
    return false;
  }

  @Override
  @Transactional(readOnly = false)
  public AutoNeedRevisionAndForceReviewResponse processImageQcResponse(String storeId,
    ImageQcResponseDomainEvent imageQcResponseDomainEvent, Map<String, Long> imageCountMap,
    ProductCollection productCollection, ProductDetailResponse productDetailResponse,
    boolean trustedSeller, ProfileResponse profileResponse) {
    try {
      if (!imageQcResponseDomainEvent.isSuccess()) {
        LOGGER
            .error("Image qc processing failure for productCode : {} , {}", imageQcResponseDomainEvent.getProductCode(),
                imageQcResponseDomainEvent.getErrorMessage());
        return new AutoNeedRevisionAndForceReviewResponse();
      }
      Map<String, ProductImagePrediction> productImagePredictionMap =
          getProductImagePredictionMap(storeId, imageQcResponseDomainEvent);
      updateNeedRevisionThresholdWithSellerTypeThreshold(storeId, productCollection, productImagePredictionMap);
      updateTakeDownRulesBasedOnCategory(productDetailResponse, productImagePredictionMap);
      BrandAndCategoryTakeDownResponse takeDownProductBasedOnBrandAndCategory =
          isTakeDownProductBasedOnBrandAndCategory(storeId, imageQcResponseDomainEvent, productCollection, profileResponse);
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
          restrictedKeywordsValidationByDs(imageQcResponseDomainEvent, productCollection);
      if (ConverterUtil.skipAllActionsUnlessCategoryChange(trustedSeller, restrictedKeywordsByFieldAndActionType)) {
        restrictedKeywordsByFieldAndActionType.setAction(SKIP_ALL_ACTIONS);
      }
      if (
        RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType()
          == restrictedKeywordsByFieldAndActionType.getAction()) {
        log.info("Overriding category take down flag if the resultant action is category change "
          + "for productCode = {} ", productCollection.getProductCode());
        takeDownProductBasedOnBrandAndCategory.setCategoryTakeDown(false);
      }
      AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse =
          computeImageQcResponse(storeId, imageQcResponseDomainEvent, productImagePredictionMap,
              productDetailResponse.getCategoryCodes().get(0), imageCountMap, productCollection.isPostLive(),
              productCollection.isAutoNeedRevision(), takeDownProductBasedOnBrandAndCategory, trustedSeller,
              restrictedKeywordsByFieldAndActionType.getAction());
      autoNeedRevisionAndForceReviewResponse.setRestrictedKeywordsByFieldAndActionType(
          restrictedKeywordsByFieldAndActionType);
      autoNeedRevisionAndForceReviewResponse.setBrandTakeDown(takeDownProductBasedOnBrandAndCategory.isTakeDown());
      autoNeedRevisionAndForceReviewResponse.setCategoryTakeDown(
        takeDownProductBasedOnBrandAndCategory.isCategoryTakeDown());
      return autoNeedRevisionAndForceReviewResponse;
    } catch (Exception e) {
      LOGGER.error("Exception caught while processing imageQcResponse for productCode : {} ",
          imageQcResponseDomainEvent.getProductCode(), e);
      if (throwExceptionOnImageQcFailure) {
        throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
            ErrorMessages.ERROR_PROCESSING_IMAGE_QC_RESPONSE);
      } else {
        return new AutoNeedRevisionAndForceReviewResponse();
      }
    }
  }

  private RestrictedKeywordsByFieldAndActionType restrictedKeywordsValidationByDs(
      ImageQcResponseDomainEvent imageQcResponseDomainEvent, ProductCollection productCollection) throws Exception {
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    restrictedKeywordsByFieldAndActionType.setAction(SKIP_ALL_ACTIONS);
    if (enableKeywordsActionInImageQcRequest) {
      log.info("Image-qc-ds-model computation for productCode : {} ", productCollection.getProductCode());
      if (CollectionUtils.isNotEmpty(imageQcResponseDomainEvent.getKeywordRestrictionModels())) {
        KeywordRestrictionModelsResponse keywordRestrictionModelsResponse =
            imageQcResponseDomainEvent.getKeywordRestrictionModels().get(0);
        if (CollectionUtils.isNotEmpty(keywordRestrictionModelsResponse.getKeywordRecommendations())) {
          Map<String, KeywordRecommendationsResponse> keywordRecommendationsResponseMap =
              getKeywordRecommendationsResponseMap(keywordRestrictionModelsResponse);
          List<CategoryRestrictedKeywordResponse> categoryRestrictedKeywordDetailList =
              getCategoryRestrictedKeywordResponses(productCollection, keywordRecommendationsResponseMap);
          Map<String, CategoryRestrictedKeywordResponse> categoryRestrictedKeywordResponseMap =
              getCategoryRestrictedKeywordResponseMap(categoryRestrictedKeywordDetailList);
          Map<String, CategoryRestrictedKeywordResponse> categoryRestrictedKeywordIdAndResponseMap =
              getCategoryRestrictedIdKeywordResponseMap(categoryRestrictedKeywordDetailList);
          Map<String, RestrictedKeywordsMappedToCategoryResponse> restrictedKeywordToActionTypeMap =
              filterKeywordsBasedOnDsModelFlagAndDsResponse(categoryRestrictedKeywordResponseMap,
                  keywordRecommendationsResponseMap, imageQcResponseDomainEvent, restrictedKeywordNotSureEnabled);
          if (keywordResetOnMultipleDestinationCategoriesEnabled) {
            CommonUtils.resetRestrictedKeywordToActionTypeMapForMultipleCategoryChangeKeywords(
              restrictedKeywordToActionTypeMap);
          }
          restrictedKeywordsByFieldAndActionType =
              computeResultantAction(restrictedKeywordToActionTypeMap, restrictedKeywordsByFieldAndActionType,
                  categoryRestrictedKeywordIdAndResponseMap);
        }
      }
    }
    log.info("Image-qc-ds-model output : {} productCode : {} ", restrictedKeywordsByFieldAndActionType,
        productCollection.getProductCode());
    if (skipDefinitiveAction && CommonUtils.isDefinitiveActionToBeSkipped(
      restrictedKeywordsByFieldAndActionType.getAction())) {
      validateSkipDefinitiveAction(productCollection.getStoreId(), productCollection,
        restrictedKeywordsByFieldAndActionType);
    }
    return restrictedKeywordsByFieldAndActionType;
  }

  private static Map<String, KeywordRecommendationsResponse> getKeywordRecommendationsResponseMap(
      KeywordRestrictionModelsResponse keywordRestrictionModelsResponse) {
    return keywordRestrictionModelsResponse.getKeywordRecommendations().stream().collect(
            Collectors.toMap(KeywordRecommendationsResponse::getKeywordId,
                keywordRecommendationsResponse -> keywordRecommendationsResponse, (a, b) -> b));
  }

  private List<CategoryRestrictedKeywordResponse> getCategoryRestrictedKeywordResponses(
      ProductCollection productCollection,
      Map<String, KeywordRecommendationsResponse> keywordRecommendationsResponseMap) throws Exception {
    List<CategoryRestrictedKeywordResponse> categoryRestrictedKeywordDetailList =
        productRepository.getCategoryRestrictedKeywordDetailList(productCollection.getCategoryCode(),
            new ArrayList<>(keywordRecommendationsResponseMap.keySet()));
    log.info("Image-qc-ds-model category restricted keyword request : {} productCode : {} ",
        keywordRecommendationsResponseMap.keySet(), productCollection.getProductCode());
    return categoryRestrictedKeywordDetailList;
  }

  private static Map<String, CategoryRestrictedKeywordResponse> getCategoryRestrictedKeywordResponseMap(
      List<CategoryRestrictedKeywordResponse> categoryRestrictedKeywordDetailList) {
    Map<String, CategoryRestrictedKeywordResponse> categoryRestrictedKeywordResponseMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(categoryRestrictedKeywordDetailList)) {
      categoryRestrictedKeywordResponseMap = categoryRestrictedKeywordDetailList.stream().collect(
          Collectors.toMap(CategoryRestrictedKeywordResponse::getRestrictedKeywordId,
              categoryRestrictedKeywordResponse -> categoryRestrictedKeywordResponse, (a, b) -> b));
    }
    return categoryRestrictedKeywordResponseMap;
  }

  private static Map<String, CategoryRestrictedKeywordResponse> getCategoryRestrictedIdKeywordResponseMap(
      List<CategoryRestrictedKeywordResponse> categoryRestrictedKeywordDetailList) {
    Map<String, CategoryRestrictedKeywordResponse> categoryRestrictedKeywordIdAndResponseMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(categoryRestrictedKeywordDetailList)) {
      categoryRestrictedKeywordIdAndResponseMap = categoryRestrictedKeywordDetailList.stream().collect(
          Collectors.toMap(CategoryRestrictedKeywordResponse::getId,
              categoryRestrictedKeywordResponse -> categoryRestrictedKeywordResponse, (a, b) -> b));
    }
    return categoryRestrictedKeywordIdAndResponseMap;
  }

  private static Map<String, RestrictedKeywordsMappedToCategoryResponse> filterKeywordsBasedOnDsModelFlagAndDsResponse(
    Map<String, CategoryRestrictedKeywordResponse> categoryRestrictedKeywordResponseMap,
    Map<String, KeywordRecommendationsResponse> keywordRecommendationsResponseMap,
    ImageQcResponseDomainEvent imageQcResponseDomainEvent,
    boolean restrictedKeywordNotSureEnabled) {
    List<KeywordRecommendationsResponse> keywordRecommendationsResponseList = new ArrayList<>();
    Map<String, RestrictedKeywordsMappedToCategoryResponse> restrictedKeywordToActionTypeMap = new HashMap<>();
    for (Map.Entry<String, CategoryRestrictedKeywordResponse> categoryRestrictedKeywordResponse : categoryRestrictedKeywordResponseMap.entrySet()) {
      RestrictedKeywordsMappedToCategoryResponse response = new RestrictedKeywordsMappedToCategoryResponse();
      response.setKeywordId(categoryRestrictedKeywordResponse.getValue().getRestrictedKeywordId());
      response.setAction(categoryRestrictedKeywordResponse.getValue().getAction());
      response.setKeyword(keywordRecommendationsResponseMap.get(
          categoryRestrictedKeywordResponse.getValue().getRestrictedKeywordId()).getKeyword());
      response.setCategoryRestrictedKeywordId(categoryRestrictedKeywordResponse.getValue().getId());
      getRestrictedKeywordToActionTypeMap(keywordRecommendationsResponseMap,
        categoryRestrictedKeywordResponse, restrictedKeywordToActionTypeMap, response,
        keywordRecommendationsResponseList, restrictedKeywordNotSureEnabled);
    }
    imageQcResponseDomainEvent.getKeywordRestrictionModels().get(0)
        .setKeywordRecommendations(keywordRecommendationsResponseList);
    log.info("Keyword response model productCode : {} response : {} ", imageQcResponseDomainEvent.getProductCode(),
        keywordRecommendationsResponseList);
    return restrictedKeywordToActionTypeMap;
  }

  private static void getRestrictedKeywordToActionTypeMap(
    Map<String, KeywordRecommendationsResponse> keywordRecommendationsResponseMap,
    Map.Entry<String, CategoryRestrictedKeywordResponse> categoryRestrictedKeywordResponse,
    Map<String, RestrictedKeywordsMappedToCategoryResponse> restrictedKeywordToActionTypeMap,
    RestrictedKeywordsMappedToCategoryResponse response,
    List<KeywordRecommendationsResponse> keywordRecommendationsResponseList,
    boolean restrictedKeywordNotSureEnabled) {
    if (Boolean.TRUE.equals(categoryRestrictedKeywordResponse.getValue().getValidateByDs())) {
      // If keyword is validateByDs then we will check response from ds model
      KeywordRecommendationsResponse keywordRecommendationsResponse = keywordRecommendationsResponseMap.getOrDefault(
          categoryRestrictedKeywordResponse.getValue().getRestrictedKeywordId(), new KeywordRecommendationsResponse());
      if (restrictedKeywordNotSureEnabled && NOT_SURE.equalsIgnoreCase(
        keywordRecommendationsResponse.getRecommendation())) {
        log.info("Overriding action to manual review for keyword {} ", response.getKeyword());
        response.setAction(1);
      }
       if (!INVALID_DETECTION.equals(keywordRecommendationsResponse.getRecommendation())) {
        restrictedKeywordToActionTypeMap.put(response.getKeyword().toLowerCase(), response);
      }
      keywordRecommendationsResponse.setSkipKeyword(false);
      if (RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType()
          == response.getAction()) {
        keywordRecommendationsResponse.setSkipKeyword(true);
      }
      keywordRecommendationsResponse.setValidateByDs(true);
      keywordRecommendationsResponse.setAction(response.getAction());
      keywordRecommendationsResponseList.add(keywordRecommendationsResponse);
    } else {
      // or else by default we will consider the restricted keyword
      restrictedKeywordToActionTypeMap.put(response.getKeyword().toLowerCase(), response);
      KeywordRecommendationsResponse keywordRecommendationsResponse = keywordRecommendationsResponseMap.getOrDefault(
          categoryRestrictedKeywordResponse.getValue().getRestrictedKeywordId(), new KeywordRecommendationsResponse());
      keywordRecommendationsResponse.setRecommendation(null);
      keywordRecommendationsResponse.setValidateByDs(false);
      keywordRecommendationsResponse.setSkipKeyword(false);
      keywordRecommendationsResponse.setAction(keywordRecommendationsResponse.getAction());
      keywordRecommendationsResponseList.add(keywordRecommendationsResponse);
    }
  }

  private RestrictedKeywordsByFieldAndActionType computeResultantAction(
      Map<String, RestrictedKeywordsMappedToCategoryResponse> restrictedKeywordToActionTypeMap,
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType,
      Map<String, CategoryRestrictedKeywordResponse> categoryRestrictedKeywordIdAndResponseMap) {
    if (MapUtils.isNotEmpty(restrictedKeywordToActionTypeMap)) {
      RestrictedKeywordsByField restrictedKeywordsByField = new RestrictedKeywordsByField();
      restrictedKeywordsByField.setKeywords(new ArrayList<>(restrictedKeywordToActionTypeMap.keySet()));
      restrictedKeywordsByFieldAndActionType =
          productLevel3Helper.getResultantActionType(restrictedKeywordToActionTypeMap,
              Collections.singletonList(restrictedKeywordsByField), restrictedKeywordsByFieldAndActionType);
      if (Objects.nonNull(restrictedKeywordsByFieldAndActionType)) {
        if (StringUtils.isNotEmpty(restrictedKeywordsByFieldAndActionType.getCategoryRestrictedKeywordId())) {
          CategoryRestrictedKeywordResponse categoryRestrictedKeywordResponse =
              categoryRestrictedKeywordIdAndResponseMap.get(
                  restrictedKeywordsByFieldAndActionType.getCategoryRestrictedKeywordId());
          if (Objects.nonNull(categoryRestrictedKeywordResponse)) {
            restrictedKeywordsByFieldAndActionType.setDestinationCategory(
                categoryRestrictedKeywordResponse.getDestinationCategory());
            restrictedKeywordsByFieldAndActionType.setKeyword(categoryRestrictedKeywordResponse.getKeyword());
            restrictedKeywordsByFieldAndActionType.setMessage(categoryRestrictedKeywordResponse.getMessage());
          }
        }
      }
    }
    return restrictedKeywordsByFieldAndActionType;
  }

  private BrandAndCategoryTakeDownResponse isTakeDownProductBasedOnBrandAndCategory(String storeId,
      ImageQcResponseDomainEvent imageQcResponseDomainEvent, ProductCollection productCollection,
    ProfileResponse profileResponse) throws Exception {
    boolean takeDownProductBasedOnBrand = false;
    boolean takeDownProductBasedOnCategory = false;
    String predictedBrand = StringUtils.EMPTY;
    if (CollectionUtils.isNotEmpty(imageQcResponseDomainEvent.getBrandModels())
      && CommonUtils.skipBrandModelForMerchantType(profileResponse, merchantTypeToSkipBrandModelCheck)) {
      Integer brandTakeDownThreshold = Integer.valueOf(String.valueOf(productSystemParameterService
          .findByStoreIdAndVariable(storeId, SystemParameterConstants.BRAND_TAKE_DOWN_THRESHOLD).getValue()));
      BrandPredictionResponse protectedBrandPredictionResponse = new BrandPredictionResponse();
      protectedBrandPredictionResponse =
        getBrandPredictionResponse(imageQcResponseDomainEvent, brandTakeDownThreshold,
          protectedBrandPredictionResponse);
      predictedBrand = protectedBrandPredictionResponse.getBrand();
      takeDownProductBasedOnBrand = productRepository.takeDownProductBasedOnBrand(
          new ProductBrandValidationRequest(productCollection.getBrandCode(),
              protectedBrandPredictionResponse.getBrand(), productCollection.getBusinessPartnerCode(), productCollection.getProductCode()));
      log.info("Brand take down response for productCode : {} with brandCode : {} and brand : {} is : {} ",
          productCollection.getProductCode(), productCollection.getBrandCode(),
          protectedBrandPredictionResponse.getBrand(), takeDownProductBasedOnBrand);
    }
    if (categoryPredictionEnabled && CollectionUtils.isNotEmpty(
      imageQcResponseDomainEvent.getCategoryModels())) {
      takeDownProductBasedOnCategory = isTakeDownProductBasedOnCategory(imageQcResponseDomainEvent,
        takeDownProductBasedOnCategory);
    }
    return new BrandAndCategoryTakeDownResponse(predictedBrand, takeDownProductBasedOnBrand,
      takeDownProductBasedOnCategory);
  }

  private static boolean isTakeDownProductBasedOnCategory(
    ImageQcResponseDomainEvent imageQcResponseDomainEvent, boolean takeDownProductBasedOnCategory) {
    for (CategoryModelPredictionResponse categoryModelPredictionResponse : imageQcResponseDomainEvent.getCategoryModels()) {
      if (Constants.CATEGORY_MISMATCH_PREDICTION_TYPE.equals(
        categoryModelPredictionResponse.getPredictionType())
        && categoryModelPredictionResponse.isMismatchCombined()) {
        takeDownProductBasedOnCategory = true;
        break;
      }
    }
    return takeDownProductBasedOnCategory;
  }

  private static BrandPredictionResponse getBrandPredictionResponse(
    ImageQcResponseDomainEvent imageQcResponseDomainEvent, Integer brandTakeDownThreshold,
    BrandPredictionResponse protectedBrandPredictionResponse) {
    for (BrandRestrictedModelsResponse brandRestrictedModelsResponse : imageQcResponseDomainEvent.getBrandModels()) {
      if (Constants.PROTECTED_BRAND_PREDICTION_TYPE.equals(brandRestrictedModelsResponse.getPredictionType())) {
        for (BrandPredictionResponse brandPredictionResponse : brandRestrictedModelsResponse.getPredictions()) {
          if (brandPredictionResponse.getConfidence() > brandTakeDownThreshold) {
            protectedBrandPredictionResponse = brandPredictionResponse;
          }
        }
      }
    }
    return protectedBrandPredictionResponse;
  }

  private void updateTakeDownRulesBasedOnCategory(ProductDetailResponse productDetailResponse,
      Map<String, ProductImagePrediction> productImagePredictionMap) throws Exception {
    boolean compareCategory = productImagePredictionMap.entrySet().stream()
        .anyMatch(productImagePredictionEntry -> productImagePredictionEntry.getValue().isCompareCategory());
    if (compareCategory) {
      List<ProductPredictionCategoryMappingResponse> predictionListByCategoryCode = productRepository
          .getPredictionListByCategoryCode(
              productDetailResponse.getCategoryCodes().get(productDetailResponse.getCategoryCodes().size() - 1));
      List<String> takeDownPredictionIdOverrideList = predictionListByCategoryCode.stream()
          .map(ProductPredictionCategoryMappingResponse::getPredictionId)
          .collect(Collectors.toList());
      for (Map.Entry<String, ProductImagePrediction> productImagePredictionEntry :
          productImagePredictionMap.entrySet()) {
        if(takeDownPredictionIdOverrideList.contains(productImagePredictionEntry.getValue().getId())) {
          productImagePredictionEntry.getValue().setForceReview(false);
          productImagePredictionEntry.getValue().setNeedRevisionEnabled(false);
        }
      }
    }
  }

  private void updateNeedRevisionThresholdWithSellerTypeThreshold(String storeId, ProductCollection productCollection,
      Map<String, ProductImagePrediction> productImagePredictionMap) {
    try {
      log.info("Fetching seller Detail from product analytics for productCode : {} ",
          productCollection.getProductCode());
      fetchSellerDetailAndNeedRevisionConfig(storeId, productCollection, productImagePredictionMap);
    } catch (Exception e) {
      log.error("Error when trying to fetch sellerType based auto need revision rules for productCode : {} ",
          productCollection.getProductCode(), e);
    }
  }

  private void fetchSellerDetailAndNeedRevisionConfig(String storeId, ProductCollection productCollection,
      Map<String, ProductImagePrediction> productImagePredictionMap) throws IOException {
    SellerDetailResponse sellerDetail =
        productAnalyticsOutbound.getSellerDetail(productCollection.getBusinessPartnerCode());
    log.info("Fetching seller wise need revision config for auto need revision for productCode : {} ",
        productCollection.getProductCode());
    if (Objects.nonNull(sellerDetail)) {
      AutoApprovalRules autoApprovalRules =
          autoApprovalService.findByStoreIdAndRuleName(storeId, sellerDetail.getSellerType());
      if (Objects.nonNull(autoApprovalRules)) {
        setSellerWiseNeedRevisionRule(productImagePredictionMap, autoApprovalRules);
      }
    }
  }

  private void setSellerWiseNeedRevisionRule(Map<String, ProductImagePrediction> productImagePredictionMap,
      AutoApprovalRules autoApprovalRules) throws IOException {
    List<AutoApprovalRuleDetailsDto> imageQcConfigNeedRevision = objectMapper
        .readValue(autoApprovalRules.getNeedRevisionConfig(), new TypeReference<List<AutoApprovalRuleDetailsDto>>() {
        });
    Map<String, AutoApprovalRuleDetailsDto> autoApprovalRuleDetailsDtoMap = imageQcConfigNeedRevision.stream()
        .collect(Collectors.toMap(AutoApprovalRuleDetailsDto::getKeyName, Function.identity()));
    for (Map.Entry<String, ProductImagePrediction> productImagePredictionEntry : productImagePredictionMap.entrySet()) {
      AutoApprovalRuleDetailsDto autoApprovalRuleDetailsDto =
          autoApprovalRuleDetailsDtoMap.get(productImagePredictionEntry.getValue().getDisplayName());
      if (Objects.nonNull(autoApprovalRuleDetailsDto)) {
        overrideGlobalValuesForAutoNeedRevision(autoApprovalRules, productImagePredictionEntry,
            autoApprovalRuleDetailsDto);
      }
    }
  }

  private void overrideGlobalValuesForAutoNeedRevision(AutoApprovalRules autoApprovalRules,
      Map.Entry<String, ProductImagePrediction> productImagePredictionEntry,
      AutoApprovalRuleDetailsDto autoApprovalRuleDetailsDto) {
    if (autoApprovalRules.isNeedRevisionEnabled()) {
      // Changing auto need revision threshold for the seller
      productImagePredictionEntry.getValue()
          .setNeedRevisionConfidenceThreshold(Integer.parseInt(autoApprovalRuleDetailsDto.getValue()));
    }
  }

  private Map<String, ProductImagePrediction> getProductImagePredictionMap(String storeId,
      ImageQcResponseDomainEvent imageQcResponseDomainEvent) {
    Map<String, ProductImagePrediction> map = new HashMap<>();
    ProductImagePrediction copyProductImagePrediction;
    if (CollectionUtils.isNotEmpty(imageQcResponseDomainEvent.getImages()) && CollectionUtils.isNotEmpty(
        imageQcResponseDomainEvent.getImages().get(0).getPredictions())) {
      for (ImageQcPredictionResponse prediction : imageQcResponseDomainEvent.getImages().get(0).getPredictions()) {
        ProductImagePrediction productImagePrediction =
            productImagePredictionService.findByStoreIdAndPredictionType(storeId, prediction.getPredictionType());
        if (Objects.nonNull(productImagePrediction)) {
          if (productImagePredictionDirtyCheckingFixEnabled) {
            copyProductImagePrediction = new ProductImagePrediction();
            BeanUtils.copyProperties(productImagePrediction, copyProductImagePrediction);
            map.putIfAbsent(prediction.getPredictionType(), copyProductImagePrediction);
          } else {
            map.putIfAbsent(prediction.getPredictionType(), productImagePrediction);
          }
        }
      }
    }

    // Text based predictions
    if (CollectionUtils.isNotEmpty(imageQcResponseDomainEvent.getRestrictionModels())) {
      for (RestrictionModelsResponse restrictionModelsResponse : imageQcResponseDomainEvent.getRestrictionModels()) {
        ProductImagePrediction productImagePrediction =
            productImagePredictionService.findByStoreIdAndPredictionType(storeId, restrictionModelsResponse.getPredictionType());
        if (Objects.nonNull(productImagePrediction)) {
          if (productImagePredictionDirtyCheckingFixEnabled) {
            copyProductImagePrediction = new ProductImagePrediction();
            BeanUtils.copyProperties(productImagePrediction, copyProductImagePrediction);
            map.putIfAbsent(restrictionModelsResponse.getPredictionType(),
                copyProductImagePrediction);
          } else {
            map.putIfAbsent(restrictionModelsResponse.getPredictionType(), productImagePrediction);
          }
        }
      }
    }
    return map;
  }

  private AutoNeedRevisionAndForceReviewResponse computeImageQcResponse(String storeId,
      ImageQcResponseDomainEvent imageQcResponseDomainEvent,
      Map<String, ProductImagePrediction> productImagePredictionMap, String parentCategoryCode,
      Map<String, Long> imageCountMap, boolean postLive, boolean productAlreadyInAutoNeedRevision,
      BrandAndCategoryTakeDownResponse takeDownProductBasedOnBrand, boolean trustedSeller, int restrictedKeywordAction)
      throws JsonProcessingException {
    int productPredictionScore = 0;
    String imageViolations = StringUtils.EMPTY;
    String textViolations = StringUtils.EMPTY;
    AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse =
        new AutoNeedRevisionAndForceReviewResponse();
    HashMap<String, String> violationMap = new HashMap<>();
    for (ImageQcResponse imageQcResponse : imageQcResponseDomainEvent.getImages()) {
      for (ImageQcPredictionResponse imageQcPredictionResponse : imageQcResponse.getPredictions()) {
        ProductImagePrediction productImagePrediction =
            productImagePredictionMap.get(imageQcPredictionResponse.getPredictionType());
        if (Objects.nonNull(productImagePrediction)) {
          if (imageQcPredictionResponse.getConfidence() > productImagePrediction.getConfidenceThreshold()) {
            imageViolations = populateImageQcWhenTrue(imageViolations, violationMap, imageQcPredictionResponse,
                productImagePrediction);
            productPredictionScore +=
              productImagePrediction.getPredictionWeightage() * imageCountMap.getOrDefault(
                imageQcResponse.getHashCode(), Constants.DEFAULT_LONG_VALUE);
            checkForAutoNeedRevisionAndForceReview(postLive, autoNeedRevisionAndForceReviewResponse,
                imageQcPredictionResponse, productImagePrediction,
              productAlreadyInAutoNeedRevision, trustedSeller);
          } else {
            populateImageQcWhenFalse(imageQcPredictionResponse, productImagePrediction);
          }
        }
      }
    }
    PredictionScoreAndViolationsDto textRestrictionModelResponse =
        getTextRestrictionModelResponse(imageQcResponseDomainEvent, productImagePredictionMap, postLive,
            productAlreadyInAutoNeedRevision, productPredictionScore, textViolations,
            autoNeedRevisionAndForceReviewResponse, violationMap, trustedSeller);
    imageViolations = takeDownProductBasedOnBrandAndCategory(takeDownProductBasedOnBrand, imageViolations,
        autoNeedRevisionAndForceReviewResponse, violationMap, postLive);
    productPredictionScore = textRestrictionModelResponse.getProductPredictionScore();
    textViolations = textRestrictionModelResponse.getViolations();
    ImageQcEnableAndSyncResponse imageQcEnableAndSyncResponse =
        getImageQcStatus(storeId, imageQcResponseDomainEvent.getProductCode(), parentCategoryCode);
    if (!imageQcEnableAndSyncResponse.isSync()) {
      autoNeedRevisionAndForceReviewResponse.setSendProductToReview(false);
    }
    setAutoNeedRevisionFlagsIfApplicable(restrictedKeywordAction, autoNeedRevisionAndForceReviewResponse);
    setForceReviewFlagsIfApplicable(postLive, takeDownProductBasedOnBrand, restrictedKeywordAction,
        autoNeedRevisionAndForceReviewResponse);
    LOGGER.info("processImageQcResponse done productCode : {} with response : {} ",
        imageQcResponseDomainEvent.getProductCode(), autoNeedRevisionAndForceReviewResponse);
    productImageQcProcessingResponseService.save(
        getProductImageQcProcessingResponse(imageQcResponseDomainEvent, productPredictionScore, imageViolations,
            autoNeedRevisionAndForceReviewResponse.isForceReview(), textViolations, takeDownProductBasedOnBrand));
    return autoNeedRevisionAndForceReviewResponse;
  }

  private static void setForceReviewFlagsIfApplicable(boolean postLive, BrandAndCategoryTakeDownResponse takeDownProductBasedOnBrand,
      int restrictedKeywordAction, AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse) {
    if ((Boolean.TRUE.equals(takeDownProductBasedOnBrand.isTakeDown() || Boolean.TRUE.equals(
      takeDownProductBasedOnBrand.isCategoryTakeDown()))
      || RestrictedKeywordActionType.MANUAL_REVIEW_DEFAULT.getRestrictedKeywordActionType()
      == restrictedKeywordAction) && Boolean.TRUE.equals(postLive)) {
      autoNeedRevisionAndForceReviewResponse.setForceReview(true);
      autoNeedRevisionAndForceReviewResponse.setSendProductToReview(true);
    }
  }

  private static void setAutoNeedRevisionFlagsIfApplicable(int restrictedKeywordAction,
      AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse) {
    if (autoNeedRevisionAndForceReviewResponse.isAutoNeedRevision()
        || RestrictedKeywordActionType.AUTO_NEED_REVISION.getRestrictedKeywordActionType() == restrictedKeywordAction) {
      autoNeedRevisionAndForceReviewResponse.setForceReview(false);
      autoNeedRevisionAndForceReviewResponse.setAutoNeedRevision(true);
      autoNeedRevisionAndForceReviewResponse.setSendProductToReview(false);
    }
  }

  private String takeDownProductBasedOnBrandAndCategory(
    BrandAndCategoryTakeDownResponse takeDownProductBasedOnBrand, String imageViolations,
    AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse,
    HashMap<String, String> violationMap, boolean postLive) {
    if (takeDownProductBasedOnBrand.isTakeDown()) {
      imageViolations =
        updateImageViolations(imageViolations, violationMap, autoNeedRevisionAndForceReviewResponse,
          Constants.SUSPICIOUS_BRAND, Constants.SUSPICIOUS_BRAND, postLive);
    }
    if (takeDownProductBasedOnBrand.isCategoryTakeDown()) {
      imageViolations =
        updateImageViolations(imageViolations, violationMap, autoNeedRevisionAndForceReviewResponse,
          Constants.CATEGORY_MISMATCH, Constants.CATEGORY_MISMATCH, postLive);
    }

    return imageViolations;
  }

  private String updateImageViolations(String imageViolations, HashMap<String, String> violationMap,
    AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse,
    String predictionType, String displayName, boolean postLive) {
    ProductImagePrediction productImagePrediction = new ProductImagePrediction();
    productImagePrediction.setPredictionType(predictionType);
    productImagePrediction.setDisplayName(displayName);
    imageViolations =
      addImageViolationIfNotPresent(imageViolations, violationMap, productImagePrediction);
    // Brand And category take Down will be unAltered regardless of Seller Type
    if (postLive) {
      autoNeedRevisionAndForceReviewResponse.setForceReview(true);
    }
    return imageViolations;
  }

  private PredictionScoreAndViolationsDto getTextRestrictionModelResponse(
      ImageQcResponseDomainEvent imageQcResponseDomainEvent,
      Map<String, ProductImagePrediction> productImagePredictionMap, boolean postLive,
      boolean productAlreadyInAutoNeedRevision, int productPredictionScore, String textViolations,
      AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse,
      HashMap<String, String> violationMap, boolean trustedSeller) {
    for (RestrictionModelsResponse restrictionModelsResponse : imageQcResponseDomainEvent.getRestrictionModels()) {
      ProductImagePrediction productImagePrediction =
          productImagePredictionMap.get(restrictionModelsResponse.getPredictionType());
      if (Objects.nonNull(productImagePrediction)) {
        ImageQcPredictionResponse restrictionModelResponse = restrictionModelsResponse.getPredictions().get(0);
        // Checking if confidence from Image qc is > TextConfidenceThreshold in our system
        if (restrictionModelResponse.getConfidence() > productImagePrediction.getTextConfidenceThreshold()) {
          textViolations =
              populateImageQcWhenTrue(textViolations, violationMap, restrictionModelResponse, productImagePrediction);
          productPredictionScore += productImagePrediction.getPredictionWeightage();
          checkForAutoNeedRevisionAndForceReview(postLive, autoNeedRevisionAndForceReviewResponse,
            restrictionModelResponse, productImagePrediction, productAlreadyInAutoNeedRevision,
            trustedSeller);
        } else {
          populateImageQcWhenFalse(restrictionModelResponse, productImagePrediction);
        }
      }
    }
    return new PredictionScoreAndViolationsDto(textViolations, productPredictionScore);
  }

  private void checkForAutoNeedRevisionAndForceReview(boolean postLive,
      AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse,
      ImageQcPredictionResponse imageQcPredictionResponse, ProductImagePrediction productImagePrediction,
      boolean productAlreadyInAutoNeedRevision, boolean trustedSeller) {
    boolean takeActionOnTrustedSeller;
    //Added column in table to relax trusted seller from checks on the basis of different violations
    // Force Review and Need Revision will be False for products from Trusted Sellers
    if (enable21PlusModel) {
      takeActionOnTrustedSeller = trustedSeller && productImagePrediction.isRelaxTrustedSeller();
    } else {
      takeActionOnTrustedSeller = trustedSeller;
    }
    if (!productAlreadyInAutoNeedRevision && productImagePrediction.isNeedRevisionEnabled() && !productImagePrediction
        .isMarkForDelete() && imageQcPredictionResponse.getConfidence() >= productImagePrediction
        .getNeedRevisionConfidenceThreshold()) {
      autoNeedRevisionAndForceReviewResponse.setAutoNeedRevision(!takeActionOnTrustedSeller);
      autoNeedRevisionAndForceReviewResponse.getPredictionTypeSet().add(productImagePrediction.getPredictionType());
    } else if (productImagePrediction.isForceReview() && postLive && !productImagePrediction.isMarkForDelete()) {
      autoNeedRevisionAndForceReviewResponse.setForceReview(!takeActionOnTrustedSeller);
      autoNeedRevisionAndForceReviewResponse.getPredictionTypeSet().add(productImagePrediction.getPredictionType());
    }
  }

  private String addImageViolationIfNotPresent(String imageViolations, HashMap<String, String> imageViolationMap,
      ProductImagePrediction productImagePrediction) {
    if (!imageViolationMap.containsKey(productImagePrediction.getPredictionType())) {
      imageViolationMap
          .put(productImagePrediction.getPredictionType(), productImagePrediction.getDisplayName());
      if (StringUtils.isNotBlank(imageViolations)) {
        imageViolations = imageViolations.concat(Constants.COMMA);
      }
      imageViolations = imageViolations.concat(productImagePrediction.getDisplayName());
    }
    return imageViolations;
  }

  private ProductImageQcProcessingResponse getProductImageQcProcessingResponse(
      ImageQcResponseDomainEvent imageQcResponseDomainEvent, int productPredictionScore, String imageViolations,
      boolean sendProductToReview, String textViolations, BrandAndCategoryTakeDownResponse brandTakeDownResponse)
      throws JsonProcessingException {
    ObjectMapper mapper = new ObjectMapper();
    ProductImageQcProcessingResponse response = new ProductImageQcProcessingResponse();
    response.setProductCode(imageQcResponseDomainEvent.getProductCode());
    response.setProductPredictionScore(productPredictionScore);
    response.setImageViolations(imageViolations);
    response.setForceReview(sendProductToReview);
    response.setImageQcResponse(mapper.writeValueAsString(imageQcResponseDomainEvent));
    response.setStoreId(MDC.get(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER));
    response.setTextViolations(textViolations);
    if (brandTakeDownResponse.isTakeDown()) {
      response.setPredictedBrand(brandTakeDownResponse.getBrand());
    }
    log.info("Product Image QC Response for product : {} was {} ",
      imageQcResponseDomainEvent.getProductCode(), response);
    return response;
  }

  private void sendNotificationForRestrictedPostLiveProduct(ProductCollection productCollection,
      boolean isKeywordPresent, ProfileResponse profileResponse) throws Exception {
    if (isKeywordPresent && productCollection.isPostLive()) {
      LOGGER.info("Sending BliBli standardization notification to business partner :{} for product : {}",
          productCollection.getBusinessPartnerCode(), productCollection.getProductCode());
      productNotificationService
          .sendNotificationForProductWithRestrictedKeyword(profileResponse.getBusinessPartnerCode(),
              productCollection.getProductName(), profileResponse.getCompany().isInternationalFlag());
    }
  }

  @Override
  public void updateSolrOrPublishEvent(ProductCollection productCollection, boolean trustedSeller) throws Exception {
    solrReviewProductCollectionService.addProductToReviewProductCollection(productCollection);
    if (productCollection.getState().equalsIgnoreCase(STATE_IN_PROGRESS)) {
      productPublisherService.publish(productCollection.getProductCode(), productCollection.getBusinessPartnerCode(),
          productCollection.getBusinessPartnerName(), productCollection.getUpdatedBy(), productCollection.isPostLive(),
          productCollection.isRestrictedKeywordsPresent(), productCollection.getRestrictedKeywordsDetected(),
          productCollection.getPrioritySeller(), trustedSeller, productCollection.getProductId(),
          productCollection);
    }
  }

  private void updateHistoryOnConfigChanges(ProductCollection productCollection) {
    ProductHistory productHistory = new ProductHistory();
    productHistory.setStoreId(productCollection.getStoreId());
    productHistory.setProductId(productCollection.getProductId());
    productHistory.setState(ProductWorkflowLookup.STATE_EDIT);
    productHistory.setDescription(SaveHistoryConstants.REVIEW_CONFIG_CHANGE);
    productHistory.setNotes(REVIEW_CONFIG_CHANGE_DESCRIPTION);
    productHistoryRepository.saveAndFlush(productHistory);
  }

  @Override
  public Page<ProductCollection> getDraftProducts(String storeId, Pageable pageable) {
    checkArgument(StringUtils.isNoneBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    List<String> states = Collections.singletonList(STATE_DRAFT);
    return productCollectionRepository
        .findByStoreIdAndStateInAndMarkForDeleteFalseOrderByProductCode(storeId, states, pageable);
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public void updateProductAsPostLiveTrue(String storeId, String productCode) throws Exception{
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    checkArgument(Objects.nonNull(productCollection), ErrorMessages.PRODUCT_COLLECTION_NOT_FOUND);
    updateProductCollectionPostLiveFlag(productCollection, true);
  }

  @Override
  public Page<ProductFilterResponse> getProductListFilter(String storeId, ProductFilterRequest productFilterRequest,
      int page, int size) throws Exception {
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage = this.solrActiveProductCollectionRepository
        .getProductCollectionListForFilterRequest(storeId, productFilterRequest, PageRequest.of(page, size));
    List<ProductFilterResponse> productFilterResponseList =
        Optional.of(solrProductCollectionDTOPage.getContent()).orElse(new ArrayList<>()).stream()
            .map(ConverterUtil::toProductFilterResponse).collect(Collectors.toList());
    return new PageImpl<>(productFilterResponseList, PageRequest.of(page, size),
        solrProductCollectionDTOPage.getTotalElements());
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public void updateProductCollectionPostLiveFlag(ProductCollection productCollection, boolean postLive) throws Exception{
    productCollection.setPostLive(postLive);
    productCollectionRepository.saveAndFlush(productCollection);
    solrReviewProductCollectionService.addProductToReviewProductCollection(productCollection);
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public ProductCollection updatePostLiveStatus(ProductCollection productCollection, boolean postLive) {
    productCollection.setPostLive(postLive);
    return productCollectionRepository.saveAndFlush(productCollection);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateBuyableAndDiscoverableFlagAndSendMailOnMarginChange(String storeId, String existingCategoryCode,
      String existingCategoryName, String productCode, boolean postLive, String username) throws Exception {
    ProductCollection productCollection = productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    if (postLive) {
      List<String> itemSkus =
          productBusinessPartnerRepository.getAllItemSkusByProductId(productCollection.getProductId());
      for (String itemSku : itemSkus) {
        ItemViewConfigRequest itemViewConfigRequest = new ItemViewConfigRequest();
        itemViewConfigRequest.setDiscoverable(false);
        itemViewConfigRequest.setBuyable(false);
        itemViewConfigRequest.setChannel(ChannelName.DEFAULT.toString());
        productLevel3Service.updateItemViewConfig(itemViewConfigRequest, itemSku, username);
        if (cncForWarehouseFeatureSwitch) {
          itemViewConfigRequest.setChannel(ChannelName.CNC.name());
          productLevel3Service.updateItemViewConfig(itemViewConfigRequest, itemSku, username);
        }
      }
    } else {
      productBusinessPartnerRepository.markItemsAsUnBuyableAndUnViewable(productCollection.getProductId());
    }
    CategoryChangeMailEvent categoryChangeMailEvent =
        CategoryChangeMailEvent.builder().existingCategoryCode(existingCategoryCode)
            .existingCategoryName(existingCategoryName).build();
    productMailEventService.createAndSaveCategoryChangeMailEvent(productCollection, categoryChangeMailEvent);
  }

  @Override
  public boolean isForceReview(String storeId, String productCode) {
    ProductImageQcProcessingResponse response =
        productImageQcProcessingResponseService.findByStoreIdAndProductCode(storeId, productCode);
    if (Objects.nonNull(response)) {
      return response.isForceReview();
    }
    return false;
  }

  @Override
  public ImageQcRequestDomainEvent publishImageQcEvent(BulkImageProcessResponse bulkImageProcessResponse,
      ProductDetailResponse productDetailResponse,
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType) {
    ImageQcRequestDomainEvent imageQcRequestDomainEvent =
        ConverterUtil.toImageQcRequest(bulkImageProcessResponse, imageSourceDirectory, productDetailResponse,
            fileStorageService.getImagePathPrefix(), fileStorageService.getCompleteSourceUrlPrefix(),
            restrictedKeywordsByFieldAndActionType);
    updateMinMaxPriceInImageQcRequest(productDetailResponse.getStoreId(), productDetailResponse.getId(),
        imageQcRequestDomainEvent);
    LOGGER.info("Publishing com.gdn.image.qc.prediction.request event for productCode : {}, imageQcRequestDomainEvent : {}",
        imageQcRequestDomainEvent.getProductCode(), imageQcRequestDomainEvent);
    kafkaProducer
        .send(DomainEventName.IMAGE_QC_PREDICTION_REQUEST, imageQcRequestDomainEvent.getProductCode(), imageQcRequestDomainEvent);
    return imageQcRequestDomainEvent;
  }

  private void updateMinMaxPriceInImageQcRequest(String storeId, String productId,
      ImageQcRequestDomainEvent imageQcRequestDomainEvent) {
    if (enablePriceUpdateInImageQcRequest) {
      List<ProductBusinessPartner> productBusinessPartnerList =
          productBusinessPartnerService.findByStoreIdAndProductId(storeId, productId);
      if (CollectionUtils.isNotEmpty(productBusinessPartnerList)) {
        String productBusinessId = productBusinessPartnerList.get(0).getId();
        imageQcRequestDomainEvent.setSellerCode(productBusinessPartnerList.get(0).getBusinessPartnerId());
        List<ProductItemBusinessPartner> productItemBusinessPartnerList =
        productItemBusinessPartnerRepository.findByStoreIdAndProductBusinessPartnerIdAndMarkForDeleteFalse(storeId, productBusinessId);
        List<Double> prices =
            productItemBusinessPartnerList.stream().filter(Objects::nonNull).map(ProductItemBusinessPartner::getSalePrice)
                .collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(prices)) {
          imageQcRequestDomainEvent.setMinPrice(Collections.min(prices));
          imageQcRequestDomainEvent.setMaxPrice(Collections.max(prices));
        }
      }
    }
  }

  private void updateMinMaxPriceInImageQcRequestForEditedProducts(ImageQcRequestDomainEvent imageQcRequestDomainEvent) {
    if (enablePriceUpdateInImageQcRequest) {
      MinMaxItemPriceResponse maxItemPriceResponse = xProductOutbound
          .getMinAndMaxOfferPrice(imageQcRequestDomainEvent.getProductCode());
      imageQcRequestDomainEvent.setMinPrice(maxItemPriceResponse.getMinPrice());
      imageQcRequestDomainEvent.setMaxPrice(maxItemPriceResponse.getMaxPrice());
    }
  }

  @Override
  public ImageQcRequestDomainEvent publishImageQcEventForEditedImages(String productCode, List<Image> images,
      ProductDetailResponse productDetailResponse, boolean isRevised,
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType, String businessPartnerCode) {
    ImageQcRequestDomainEvent imageQcRequestDomainEvent =
        ConverterUtil.toImageQcRequestForEditedImages(productCode, images, imageSourceDirectory, productDetailResponse,
            fileStorageService.getImagePathPrefix(), fileStorageService.getCompleteSourceUrlPrefix(),
            restrictedKeywordsByFieldAndActionType);
    imageQcRequestDomainEvent.setSellerCode(businessPartnerCode);
    if (isRevised) {
      updateMinMaxPriceInImageQcRequest(productDetailResponse.getStoreId(),productDetailResponse.getId(),imageQcRequestDomainEvent);
    } else {
      updateMinMaxPriceInImageQcRequestForEditedProducts(imageQcRequestDomainEvent);
    }
    LOGGER.info("Publishing com.gdn.image.qc.prediction.request event for productCode : {}, imageQcRequestDomainEvent : {}",
        imageQcRequestDomainEvent.getProductCode(), imageQcRequestDomainEvent);
    kafkaProducer
        .send(DomainEventName.IMAGE_QC_PREDICTION_REQUEST, imageQcRequestDomainEvent.getProductCode(), imageQcRequestDomainEvent);
    return imageQcRequestDomainEvent;
  }

  @Override
  public ImageQcRequestDomainEvent publishImageQcEventForContentEdit(List<CategoryResponse> categoryResponses,
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType, ProductLevel3 productLevel3, boolean needRevisionSubmit) {
    ImageQcRequestDomainEvent imageQcRequestDomainEvent =
        ConverterUtil.toImageQcRequestForContentEdit(categoryResponses, restrictedKeywordsByFieldAndActionType,
            productLevel3);
    imageQcRequestDomainEvent.setSellerCode(productLevel3.getBusinessPartnerCode());
    if (needRevisionSubmit) {
      updateMinMaxPriceInImageQcRequest(productLevel3.getStoreId(), productLevel3.getId(), imageQcRequestDomainEvent);
    } else {
      updateMinMaxPriceInImageQcRequestForEditedProducts(imageQcRequestDomainEvent);
    }
    LOGGER.info("Publishing com.gdn.image.qc.prediction.request event for productCode : {}, imageQcRequestDomainEvent : {}",
        imageQcRequestDomainEvent.getProductCode(), imageQcRequestDomainEvent);
    kafkaProducer.send(DomainEventName.IMAGE_QC_PREDICTION_REQUEST, imageQcRequestDomainEvent.getProductCode(),
        imageQcRequestDomainEvent);
    return imageQcRequestDomainEvent;
  }

  @Override
  @Transactional(readOnly = false)
  public ProductCollection updateProductCollectionResizedFlag(ProductCollection productCollection, boolean resize,
      int imageQcState) {
    productCollection.setImageResized(resize);
    productCollection.setImageQcState(imageQcState);
    return productCollectionRepository.saveAndFlush(productCollection);
  }

  @Override
  public void publishAutoNeedRevisionEvent(String storeId, String productCode, Set<String> predictionTypeList,
      boolean contentNeedRevision, String notes) {
    AutoNeedRevisionDomainEvent autoNeedRevisionDomainEvent =
        new AutoNeedRevisionDomainEvent(storeId, productCode, new ArrayList<>(predictionTypeList), contentNeedRevision);
    autoNeedRevisionDomainEvent.setNotes(notes);
    kafkaProducer.send(DomainEventName.PRODUCT_AUTO_NEED_REVISION_EVENT, productCode, autoNeedRevisionDomainEvent);
    log.info("Publish event com.gdn.pbp.product.auto.need.revision for productCode : {} ", productCode);
  }

  @Override
  public void publishProductActionRetryEvent(ProductActionRetryEvent productActionRetryEvent) {
    kafkaProducer.send(DomainEventName.ADD_PRODUCT_TO_PDT_RETRY, productActionRetryEvent.getProductCode(),
        productActionRetryEvent);
    log.info("Publish event com.gdn.pbp.product.pdt.retry for productCode : {} ",
        productActionRetryEvent.getProductCode());
  }

  @Override
  public ImageQcProcessedResponseDomainEvent publishImageQcProcessedResponseEvent(String storeId, String productCode,
      ProductCollection productCollection, ProductDetailResponse productDetailResponse,
      AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse,
    ProfileResponse profileResponse, ImageQcResponseDomainEvent imageQcResponseDomainEvent) throws Exception {
    ImageQcProcessedResponse imageQcProcessedResponse = new ImageQcProcessedResponse();
    ProductImageQcProcessingResponse imageQcProcessingResponse =
        productImageQcProcessingResponseService.findByStoreIdAndProductCode(storeId, productCode);
    if (imageQcProcessingResponse.isForceReview() && profileResponse.isTrustedSeller()
      && !autoNeedRevisionAndForceReviewResponse.isBrandTakeDown()
      && !autoNeedRevisionAndForceReviewResponse.isCategoryTakeDown()) {
      imageQcProcessedResponse.setForceReview(Boolean.FALSE);
    }
    //Set Force review False for Trusted Seller
    ConverterUtil.getImageQcResponse(productCode, imageQcProcessedResponse, imageQcProcessingResponse,
        productImagePredictionService.getListOfActivePredictionTypes(storeId));
    ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent =
        ConverterUtil.toImageQcProcessedResponseDomainEventFromProcessedResponse(imageQcProcessedResponse);
    boolean eligibleForKeywordAction =
      isEligibleForKeywordAction(productCode, autoNeedRevisionAndForceReviewResponse,
        imageQcProcessedResponseDomainEvent,
        autoNeedRevisionAndForceReviewResponse.isBrandTakeDown(),
        autoNeedRevisionAndForceReviewResponse.isCategoryTakeDown());
    if (autoNeedRevisionAndForceReviewResponse.isAutoNeedRevision() && !productCollection.isAutoNeedRevision()) {
      imageQcProcessedResponseDomainEvent
          .setAutoNeedRevisionAndForceReviewResponse(autoNeedRevisionAndForceReviewResponse);
    } else if (!autoNeedRevisionAndForceReviewResponse.isAutoNeedRevision() && !eligibleForKeywordAction) {
      AutoApprovalsDetailDto autoApprovalsDetailDto =
          generateAutoApprovalDetailsDTO(storeId, productCode, productDetailResponse, imageQcProcessingResponse,
              productCollection.isEdited(), true, productCollection.isNeedRevision(), profileResponse.isTrustedSeller(),
              productCollection);
      ConverterUtil.overrideRestrictedKeywordPresentFlagBasedOnDsResponse(imageQcResponseDomainEvent,
          autoApprovalsDetailDto, null);
      imageQcProcessedResponseDomainEvent.setAutoApprovalType(this.autoApprovalService.verifyAutoApprovalRules(
          autoApprovalsDetailDto));
    }
    ProductBusinessPartner productBusinessPartner = productBusinessPartnerService
        .findFirstByStoreIdAndProductId(productCollection.getStoreId(), productCollection.getProductId());
    AddEditedProductToPDTEvent addEditedProductToPDTEvent = new AddEditedProductToPDTEvent();
    addEditedProductToPDTEvent.setOnlyImageQcDataUpdate(true);
    addEditedProductToPDTEvent.setTrustedSeller(profileResponse.isTrustedSeller());
    addEditedProductToPDTEvent.setB2cActivated(productBusinessPartner.isB2cActivated());
    addEditedProductToPDTEvent.setB2bActivated(productBusinessPartner.isB2bActivated());
    addEditedProductToPDTEvent.setImageQcProcessedResponseDomainEvent(imageQcProcessedResponseDomainEvent);
    SellerDetailResponse sellerDetailResponse = null;
    try {
      sellerDetailResponse = productAnalyticsOutbound.getSellerDetail(productCollection.getBusinessPartnerCode());
    } catch (Exception e) {
      log.error("Exception while getting sellerDetail Response from ProductAnalytics for businessPartnerCode : {} ",
          productCollection.getProductCode(), e);
    }
    if (Objects.nonNull(sellerDetailResponse)) {
      addEditedProductToPDTEvent.setSellerBadge(sellerDetailResponse.getSellerBadge());
    }
    kafkaProducer.send(kafkaTopicProperties.getVendorCombinedEventNoPriority(),
        imageQcProcessedResponseDomainEvent.getProductCode(),
        AddProductToVendorCombinedEventModel.builder().addEditedProductToPDTEvent(addEditedProductToPDTEvent).build());
    log.info("Publish image qc processed event productCode : {} ", addEditedProductToPDTEvent.getProductCode());
    return imageQcProcessedResponseDomainEvent;
  }

  private boolean isEligibleForKeywordAction(String productCode,
    AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse,
    ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent, boolean brandTakeDown,
    boolean categoryTakeDown) {
    boolean eligibleForKeywordAction = false;
    if (Objects.nonNull(autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType())) {
      if (RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType()
          == autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType().getAction()) {
        eligibleForKeywordAction = true;
        log.info("Product eligible for auto reject action : {} ", productCode);
        kafkaProducer.send(DomainEventName.ADD_PRODUCT_TO_PDT_RETRY, productCode,
            new ProductActionRetryEvent(Constants.DEFAULT_STORE_ID, productCode, Constants.PDT_AUTO_REJECTION,
                autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType().getMessage()));
      } else if (RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType()
          == autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType().getAction()) {
        eligibleForKeywordAction = true;
        log.info("Product eligible for auto category change : {} categoryCode : {} ", productCode,
            autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType()
                .getDestinationCategory());
        imageQcProcessedResponseDomainEvent.setCategoryCode(
            autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType()
                .getDestinationCategory());
      } else if (RestrictedKeywordActionType.AUTO_NEED_REVISION.getRestrictedKeywordActionType()
          == autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType().getAction()) {
        log.info("Product eligible for auto need revision : {}  ", productCode);
        autoNeedRevisionAndForceReviewResponse.setContentNeedRevision(true);
        autoNeedRevisionAndForceReviewResponse.setNotes(
            autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType().getMessage());
      } else if(brandTakeDown) {
        log.info("Product eligible for brand take down : {} ", productCode);
        eligibleForKeywordAction = true;
      } else if (categoryTakeDown) {
        log.info("Product eligible for category take down : {} ", productCode);
        eligibleForKeywordAction = true;
      }
    }
    return eligibleForKeywordAction;
  }

  private AutoApprovalsDetailDto generateAutoApprovalDetailsDTO(String storeId, String productCode,
      ProductDetailResponse productDetailResponse, ProductImageQcProcessingResponse productImageQcProcessingResponse,
      Boolean edited, boolean setImageConfidenceDto, boolean revised, boolean trustedSeller,
    ProductCollection productCollection) throws Exception {
    List<CategoryResponse> categoryHierarchy =
        this.categoryRepository.findHierarchyByCategoryCode(productCollection.getCategoryCode());
    List<String> predictionsToBeConsideredForAutoApproval = getPredictionsToBeConsidered(storeId);
    String c1CategoryCode = categoryHierarchy.get(categoryHierarchy.size() - 1).getCategoryCode();
    AutoApprovalsDetailDto autoApprovalsDetailDto;
    if (setImageConfidenceDto) {
      ImageQcResponseDomainEvent imageQcResponseDomainEvent =
          objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class);
      filterPredictionsForAutoApprovalCheck(predictionsToBeConsideredForAutoApproval, imageQcResponseDomainEvent);
      getImageQcListForRevisedProducts(revised, imageQcResponseDomainEvent);
      Map<String, ImageQcResponse> mapOfHashCodeAndImageQcResponse = imageQcResponseDomainEvent.getImages().stream()
          .collect(Collectors.toMap(ImageQcResponse::getHashCode, Function.identity(), (value1, value2) -> value2));
      autoApprovalsDetailDto = ConverterUtil.setAutoApprovalDetailDTO(storeId, productCollection, c1CategoryCode,
          productDetailResponse.getImages().stream()
              .filter(image -> ((Objects.equals(edited, image.isEdited()) || revised) && !image.isMarkForDelete()))
              .collect(Collectors.toList()), mapOfHashCodeAndImageQcResponse);
      // TODO
      autoApprovalsDetailDto.setForceReview(
        trustedSeller ? Boolean.FALSE : productImageQcProcessingResponse.isForceReview());
      if (overrideRestrictedKeywordsFlagByDsResponse) {
        ConverterUtil.overrideRestrictedKeywordPresentFlagBasedOnDsResponse(imageQcResponseDomainEvent,
            new AutoApprovalsDetailDto(), productCollection);
        if (productImageQcProcessingResponse.isForceReview()
          && ConverterUtil.checkBrandCategoryTwentyOnePlusViolation(
          productImageQcProcessingResponse)) {
          autoApprovalsDetailDto.setForceReview(true);
        }
      }
    } else {
      autoApprovalsDetailDto = ConverterUtil.setAutoApprovalDetailDTOForNoImages(storeId, productCollection,
          c1CategoryCode);
    }
    autoApprovalsDetailDto.setRevised(productCollection.isNeedRevision());
    autoApprovalsDetailDto.setRestrictedKeywordPresent(autoApprovalContentImageViolation ?
        StringUtils.isNotBlank(Optional.ofNullable(productImageQcProcessingResponse)
            .map(ProductImageQcProcessingResponse::getTextViolations).orElse(StringUtils.EMPTY))
            || StringUtils.isNotBlank(Optional.ofNullable(productImageQcProcessingResponse)
            .map(ProductImageQcProcessingResponse::getImageViolations).orElse(StringUtils.EMPTY))
            || productCollection.isRestrictedKeywordsPresent() :
        productCollection.isRestrictedKeywordsPresent());
    autoApprovalsDetailDto.setProductCreationType(productCollection.getProductCreationType());
    return autoApprovalsDetailDto;
  }

  private void filterPredictionsForAutoApprovalCheck(List<String> predictionsToBeConsideredForAutoApproval,
      ImageQcResponseDomainEvent imageQcResponseDomainEvent) {
    for (ImageQcResponse imageQcResponse : imageQcResponseDomainEvent.getImages()) {
      imageQcResponse.getPredictions().removeIf(imageQcPredictionResponse -> !predictionsToBeConsideredForAutoApproval
          .contains(imageQcPredictionResponse.getPredictionType()));
    }
  }

  private List<String> getPredictionsToBeConsidered(String storeId) {
    List<ProductImagePrediction> productImagePredictions =
        productImagePredictionService.findByStoreIdAndMarkForDeleteFalse(storeId);
    return productImagePredictions.stream().filter(ProductImagePrediction::isPredictionConsidered)
        .map(ProductImagePrediction::getPredictionType).collect(Collectors.toList());
  }

  private void getImageQcListForRevisedProducts(boolean revised,
      ImageQcResponseDomainEvent imageQcResponseDomainEvent) {
    if (revised) {
      List<ImageQcResponse> imageQcResponseList = imageQcResponseDomainEvent.getImages().stream()
          .filter(imageQcResponse -> !imageQcResponse.isMarkForDelete()).collect(Collectors.toList());
      imageQcResponseDomainEvent.setImages(imageQcResponseList);
    }
  }

  @Trace(dispatcher=true)
  @Async
  @Override
  public void publishProductStatusEvent(ProductDetailResponse productDetailResponse,
      ProductCollection productCollection, ProductStatus productStatus, String reason) throws Exception {
    if (productStatusEventEnabled) {
      ProductStatusDomainEvent productStatusDomainEvent =
          ProductStatusDomainEvent.builder().productStatus(productStatus.getProductStatus())
              .businessPartnerCode(productCollection.getBusinessPartnerCode()).postLive(productCollection.isPostLive())
              .reason(reason).product(new BasicProductDetail()).build();
      productStatusDomainEvent.setPostLive(productCollection.isPostLive());
      productStatusDomainEvent.setProductStatus(productStatus.getProductStatus());
      BasicProductDetail basicProductDetail =
          BasicProductDetail.builder().productCode(productDetailResponse.getProductCode())
              .productName(productDetailResponse.getName()).brand(productCollection.getBrand())
              .productItems(new ArrayList<>()).build();
      Map<String, ProductItemBusinessPartner> productItemIdToItemMap = new HashMap<>();
      productItemIdToItemMap =
          getProductItemBusinessPartnerMap(productCollection, basicProductDetail, productItemIdToItemMap);
      productStatusDomainEvent = ConverterUtil
          .toProductStatusDomainEvent(productDetailResponse, productStatusDomainEvent, basicProductDetail,
              productItemIdToItemMap);
      productPublisherService.publishProductStatusDomainEvent(productStatusDomainEvent);
    }
  }

  @Trace(dispatcher=true)
  @Async
  @Override
  public void publishProductStatusEventByProductCode(String productCode, ProductStatus productStatus, String reason)
      throws Exception {
    if (productStatusEventEnabled) {
      ProductDetailResponse productDetailResponse = this.findProductDetailByProductCode(productCode, false);
      ProductCollection productCollection =
          this.productCollectionRepository.findByStoreIdAndProductCode(productDetailResponse.getStoreId(), productCode);
      ProductStatusDomainEvent productStatusDomainEvent =
          ProductStatusDomainEvent.builder().productStatus(productStatus.getProductStatus())
              .businessPartnerCode(productCollection.getBusinessPartnerCode()).postLive(productCollection.isPostLive())
              .reason(reason).product(new BasicProductDetail()).build();
      productStatusDomainEvent.setPostLive(productCollection.isPostLive());
      productStatusDomainEvent.setProductStatus(productStatus.getProductStatus());
      if (productStatus.equals(ProductStatus.NEED_CORRECTION) && StringUtils.isBlank(reason)) {
        ProductHistory history = this.productHistoryRepository
            .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(productCollection.getStoreId(),
                productCollection.getProductId(), WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc());
        if(Objects.nonNull(history)) {
          productStatusDomainEvent.setReason(history.getNotes());
        }
      }
      BasicProductDetail basicProductDetail =
          BasicProductDetail.builder().productCode(productDetailResponse.getProductCode())
              .productName(productDetailResponse.getName()).brand(productCollection.getBrand())
              .productItems(new ArrayList<>()).build();
      Map<String, ProductItemBusinessPartner> productItemIdToItemMap = new HashMap<>();
      productItemIdToItemMap =
          getProductItemBusinessPartnerMap(productCollection, basicProductDetail, productItemIdToItemMap);
      productStatusDomainEvent = ConverterUtil
          .toProductStatusDomainEvent(productDetailResponse, productStatusDomainEvent, basicProductDetail,
              productItemIdToItemMap);
      productPublisherService.publishProductStatusDomainEvent(productStatusDomainEvent);
    }
  }

  private void setDistributionMappingStatus(List<ProductItemBusinessPartner> productItemBusinessPartnerList,
      BasicProductDetail basicProductDetail) {
    if (ranchIntegrationEnabled) {
      long countOfDistributionL5s = productItemBusinessPartnerList.stream()
          .filter(ProductItemBusinessPartner::isDistribution).count();
      long totalNumberOfL5s = productItemBusinessPartnerList.size();
      if (countOfDistributionL5s == 0) {
        basicProductDetail.setDistributionMappingStatus(Constants.NON_DISTRIBUTION);
      } else {
        if (countOfDistributionL5s == totalNumberOfL5s) {
          basicProductDetail.setDistributionMappingStatus(Constants.PURE_DISTRIBUTION);
        } else {
          basicProductDetail.setDistributionMappingStatus(Constants.DISTRIBUTION);
        }
      }
    }
  }

  private Map<String, ProductItemBusinessPartner> getProductItemBusinessPartnerMap(ProductCollection productCollection,
      BasicProductDetail basicProductDetail, Map<String, ProductItemBusinessPartner> productItemIdToItemMap) {
    if (!DEFAULT_BUSINESS_PARTNER_CODE.equalsIgnoreCase(productCollection.getBusinessPartnerCode())) {
      List<ProductBusinessPartner> productBusinessPartners = productBusinessPartnerRepository
          .findByStoreIdAndProductId(productCollection.getStoreId(), productCollection.getProductId());
      if (productBusinessPartners.size() == 1) {
        basicProductDetail.setProductSku(productBusinessPartners.get(0).getGdnProductSku());
        if (CollectionUtils.isNotEmpty(productBusinessPartners.get(0).getProductItemBusinessPartners())) {
          productItemIdToItemMap = productBusinessPartners.get(0).getProductItemBusinessPartners().stream().collect(
              Collectors.toMap(ProductItemBusinessPartner::getProductItemId,
                  Function.identity(),
                  (productItemBusinessPartner1, productItemBusinessPartner2) -> productItemBusinessPartner1));
          setDistributionMappingStatus(productBusinessPartners.get(0).getProductItemBusinessPartners(),
              basicProductDetail);
        }
      }
    }
    return productItemIdToItemMap;
  }

  @Override
  @Transactional(readOnly = false)
  public void checkIfProductWasTakeDown(ProductDetailResponse productDetailResponse, String productId) {
    ImageQcProcessedResponse response = productImagePredictionService
        .findProductImagePredictionResponseByStoreIdAndProductCode(productDetailResponse.getStoreId(),
            productDetailResponse.getProductCode());
    if (Objects.isNull(response) || !response.isForceReview()) {
      return;
    }
    List<ItemFlagDetails> itemSkuFlags =
        productBusinessPartnerService.getAllItemSkusViewConfigByProductId(productDetailResponse.getId());
    xProductOutbound.updateItemViewConfigAndForceReview(false,  getItemViewConfigAndItemSkuRequest(itemSkuFlags), false);
    List<ProductBusinessPartner> productBusinessPartnerList =
        productBusinessPartnerRepository.findByStoreIdAndProductId(Constants.DEFAULT_STORE_ID, productId);
    productBusinessPartnerList.forEach(
        productBusinessPartner -> updateProductBusinessPartnerStatus(productBusinessPartner, true,
            ProductLevel1State.ACTIVE));
  }

  @Override
  public Integer getMinimumPrice(String storeId) {
    ProductSystemParameter minimumPrice =
        productSystemParameterService.findByStoreIdAndVariable(storeId, Constants.MINIMUM_PRICE);
    if (Objects.nonNull(minimumPrice)) {
      return Integer.parseInt(minimumPrice.getValue());
    }
    return null;
  }

  @Override
  public boolean checkIfMPPIsAllowed(String storeId, String businessPartnerCode, ProfileResponse profileResponse) {
    return ifMppIsAllowed(profileResponse);
  }

  private boolean ifMppIsAllowed(ProfileResponse profileResponse) {
    return Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany()) && (
        (profileResponse.getCompany().isCncActivated()) || (
            Boolean.TRUE.equals(profileResponse.getMultiDefaultAddressFlag()) && mppAllowedSellers
                .contains(profileResponse.getCompany().getMerchantType())));
  }

  @Override
  public boolean checkIfMPPIsAllowed(ProfileResponse profileResponse) {
    return ifMppIsAllowed(profileResponse);
  }

  private List<ItemViewConfigAndItemSkuRequest> getItemViewConfigAndItemSkuRequest(
      List<ItemFlagDetails> itemFlagDetails) {
    List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests = new ArrayList<>();
    for (ItemFlagDetails flagDetails : itemFlagDetails) {
      if (cncForWarehouseFeatureSwitch) {
        ItemViewConfigAndItemSkuRequest defaultViewConfigAndItemSkuRequest = new ItemViewConfigAndItemSkuRequest();
        defaultViewConfigAndItemSkuRequest.setItemSku(flagDetails.getItemSku());
        defaultViewConfigAndItemSkuRequest.setPickupPointCode(flagDetails.getPickupPointCode());
        defaultViewConfigAndItemSkuRequest.setBuyable(flagDetails.isBuyable());
        defaultViewConfigAndItemSkuRequest.setDiscoverable(flagDetails.isDisplayable());
        defaultViewConfigAndItemSkuRequest.setChannel(Constants.DEFAULT);
        itemViewConfigAndItemSkuRequests.add(defaultViewConfigAndItemSkuRequest);

        ItemViewConfigAndItemSkuRequest cncViewConfigAndItemSkuRequest = new ItemViewConfigAndItemSkuRequest();
        cncViewConfigAndItemSkuRequest.setItemSku(flagDetails.getItemSku());
        cncViewConfigAndItemSkuRequest.setPickupPointCode(flagDetails.getPickupPointCode());
        cncViewConfigAndItemSkuRequest.setBuyable(flagDetails.isCncBuyable());
        cncViewConfigAndItemSkuRequest.setDiscoverable(flagDetails.isCncDisplayable());
        cncViewConfigAndItemSkuRequest.setChannel(Constants.CNC_CHANNEL);
        itemViewConfigAndItemSkuRequests.add(cncViewConfigAndItemSkuRequest);
      } else {
        ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest = new ItemViewConfigAndItemSkuRequest();
        itemViewConfigAndItemSkuRequest.setItemSku(flagDetails.getItemSku());
        itemViewConfigAndItemSkuRequest.setPickupPointCode(flagDetails.getPickupPointCode());
        itemViewConfigAndItemSkuRequest.setBuyable(flagDetails.isBuyable());
        itemViewConfigAndItemSkuRequest.setDiscoverable(flagDetails.isDisplayable());
        itemViewConfigAndItemSkuRequests.add(itemViewConfigAndItemSkuRequest);
      }
    }
    return itemViewConfigAndItemSkuRequests;
  }

  @Trace(dispatcher = true)
  @Async
  @Override
  @Transactional(readOnly = false)
  public void saveHistoryForUrlImage(String productCode, List<ItemFlagDetails> itemFlagDetails,
      ProductCreationRequest productCreationRequest) throws Exception {
    try {
      Map<String, Set<String>> itemImagesList = new HashMap<>();
      Map<String, String> productItemIdAndGdnItemSkuMap;
      Map<String, Set<String>> gdnItemSkuAndUrlHistory = new HashMap<>();
      ProductDetailResponse productDetailByProductCode =
          this.productRepository.findProductDetailByProductCode(productCode);
      productItemIdAndGdnItemSkuMap = getItemImageDetails(itemFlagDetails, itemImagesList, productDetailByProductCode);
      generateHistoryForItems(productCreationRequest, itemImagesList, productItemIdAndGdnItemSkuMap,
          gdnItemSkuAndUrlHistory);
      saveItemHistory(productCreationRequest, gdnItemSkuAndUrlHistory);
    } catch (Exception e) {
      LOGGER.error("Error while saving url history for productCode : {} ", productCreationRequest.getProductCode(), e);
    }
  }

  private Map<String, String> getItemImageDetails(List<ItemFlagDetails> itemFlagDetails,
      Map<String, Set<String>> itemImagesList, ProductDetailResponse productDetailByProductCode) {
    Map<String, String> productItemIdAndGdnItemSkuMap;
    productItemIdAndGdnItemSkuMap = itemFlagDetails.stream().collect(
        Collectors.toMap(ItemFlagDetails::getProductItemId, ItemFlagDetails::getItemSku, (itemId1, itemId2) -> itemId1));
    productDetailByProductCode.getProductItemResponses().forEach(productItemResponse -> {
      Set<String> imagesList =
          productItemResponse.getImages().stream().map(Image::getLocationPath).collect(Collectors.toSet());
      itemImagesList.putIfAbsent(productItemResponse.getId(), imagesList);
    });
    return productItemIdAndGdnItemSkuMap;
  }

  private void generateHistoryForItems(ProductCreationRequest productCreationRequest,
      Map<String, Set<String>> itemImagesList, Map<String, String> productItemIdAndGdnItemSkuMap,
      Map<String, Set<String>> gdnItemSkuAndUrlHistory) {
    Map<String, String> imageLocationPathAndUrlMap;
    imageLocationPathAndUrlMap =
        productCreationRequest.getImages().stream().filter(image -> StringUtils.isNotBlank(image.getUrlPath()))
            .collect(Collectors.toMap(Image::getLocationPath, Image::getUrlPath, (a, b) -> a));
    for (Map.Entry<String, Set<String>> itemAndImages : itemImagesList.entrySet()) {
      Set<String> imageList = new HashSet<>();
      for (String image : itemAndImages.getValue()) {
        if (imageLocationPathAndUrlMap.containsKey(image)) {
          imageList.add(imageLocationPathAndUrlMap.get(image));
        }
      }
      if (!imageList.isEmpty()) {
        gdnItemSkuAndUrlHistory.putIfAbsent(productItemIdAndGdnItemSkuMap.get(itemAndImages.getKey()), imageList);
      }
    }
  }

  private void saveItemHistory(ProductCreationRequest productCreationRequest,
      Map<String, Set<String>> gdnItemSkuAndUrlHistory) throws JsonProcessingException {
    List<UpdatedProductHistory> updatedProductHistoryList = new ArrayList<>();
    if (MapUtils.isNotEmpty(gdnItemSkuAndUrlHistory)) {
      updatedProductHistoryList = gdnItemSkuAndUrlHistory.entrySet().stream().map(
          gdnItemSkuAndUrl -> generateLogAuditTrailUpdatedProduct(gdnItemSkuAndUrl.getKey(),
              SaveHistoryConstants.IMAGE_LINK, new Date(), productCreationRequest.getBusinessPartnerCode(),
              productCreationRequest.getUpdatedBy(), productCreationRequest.getUpdatedBy(), new Date(), null,
              StringUtils.join(gdnItemSkuAndUrl.getValue(), Constants.COMMA), productCreationRequest.getGdnProductSku(),
              productCreationRequest.getName())).collect(Collectors.toList());
    }
    if (CollectionUtils.isNotEmpty(updatedProductHistoryList)) {
      GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus =
          this.xProductOutbound.getItemPickupPointCodeByItemSkus(
              ConverterUtil.toSimpleListStringRequest(updatedProductHistoryList));
      Map<String, String> itemSkuAndPickupPointCodeMap =
          ConverterUtil.toItemSkuAndPickupPointCodeMap(itemPickupPointCodeByItemSkus.getContent());
      updatedProductHistoryList =
          CommonUtils.setOnlineStatusAndPickupPointCode(updatedProductHistoryList,
              itemSkuAndPickupPointCodeMap);
    }
    if (productHistoryUpdateThroughEvent) {
      updatedProductHistoryList.forEach(updatedProductHistory -> {
        AuditTrailListRequest auditTrailListRequest = CommonUtils
            .getAuditTrailRequestForL3History(updatedProductHistory.getBusinessPartnerCode(),
                updatedProductHistory.getProductSku(), updatedProductHistory.getGdnName(),
                updatedProductHistory.getActivity(), StringUtils.EMPTY, updatedProductHistory.getOldValues(),
                updatedProductHistory.getNewValues(), updatedProductHistory.getChangedBy(),
                updatedProductHistory.getRequestId(), updatedProductHistory.getClientHost(),
                updatedProductHistory.getGdnSku(), updatedProductHistory.getPickupPointCode());
        auditTrailListRequest.setUpdateDirectlyToDB(true);
        log.info("Publishing the event {} for updating history of productSku {} ", PRODUCT_SKU_UPDATE_HISTORY,
            updatedProductHistory.getProductSku());
        kafkaProducer.send(PRODUCT_SKU_UPDATE_HISTORY, updatedProductHistory.getProductSku(), auditTrailListRequest);
      });
    } else {
      updatedProductHistoryService.createAudit(updatedProductHistoryList, false);
    }
    LOGGER.info("Saving url history for productCode : {} done", productCreationRequest.getProductCode());
  }

  private void updateProductBusinessPartnerStatus(ProductBusinessPartner productBusinessPartner, boolean markForDelete,
      String state) {
    productBusinessPartner.setMarkForDelete(markForDelete);
    productBusinessPartner.setActivated(markForDelete);
    productBusinessPartner.setState(state);
    productBusinessPartnerRepository.save(productBusinessPartner);
  }

  private UpdatedProductHistory generateLogAuditTrailUpdatedProduct(String gdnSku, String activity,
      Date accessTime, String businessPartnerCode, String requestId, String username, Date activatedDate,
      String oldValues, String newValues, String productSku, String name) {
    UpdatedProductHistory updatedProductHistory = new UpdatedProductHistory();
    updatedProductHistory.setGdnSku(gdnSku);
    updatedProductHistory.setActivity(activity);
    updatedProductHistory.setAccessTime(accessTime);
    updatedProductHistory.setBusinessPartnerCode(businessPartnerCode);
    updatedProductHistory.setRequestId(requestId);
    updatedProductHistory.setChangedBy(username);
    updatedProductHistory.setActivatedDate(activatedDate);
    updatedProductHistory.setOldValues(oldValues);
    updatedProductHistory.setNewValues(newValues);
    updatedProductHistory.setProductSku(productSku);
    updatedProductHistory.setGdnName(name);
    return updatedProductHistory;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void saveMasterProductMigrationHistory(ProductMigration productMigration) {
    String note = "Old master product (" + productMigration.getProductCode() + ")";
    if (StringUtils.isBlank(productMigration.getProductCode())) {
      note = "New product code created";
    }
    ProductHistory productHistory =
        new ProductHistory(productMigration.getMigratedProductId(), ProductWorkflowLookup.STATE_MIGRATE,
            ProductWorkflowLookup.STATE_MIGRATED_PRODUCT, note, Constants.SYSTEM, new Date(),
            Constants.DEFAULT_STORE_ID);
    this.productHistoryRepository.save(productHistory);

    saveSyncHistoryForItems(productMigration);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void saveSyncHistoryForItems(ProductMigration productMigration) {
    if (!productMigration.isSyncStatus()) {
      try {
        List<String> itemSkus =
            productBusinessPartnerRepository.findListOfItemSkusbyProductSku(productMigration.getGdnProductSku());
        if (CollectionUtils.isNotEmpty(itemSkus)) {
          for (String itemSku : itemSkus) {
            this.updatedProductHistoryService.saveUpdateProductLevel3AuditForMigration(productMigration.getBusinessPartnerId(), itemSku);
          }
        }
      } catch (Exception e) {
        log.warn("Unable to save the audit history for gdn sku : {} ", productMigration.getGdnProductSku(), e);
      }
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateProductCollectionForMigratedProduct(ProductMigration productMigration,
      ProductAndItemsResponse productAndItemsResponse) throws Exception {
    if (StringUtils.isBlank(productMigration.getProductCode())) {
      createdProductBusinessPartnerDetails(productMigration, productAndItemsResponse);
    } else {
      ProductCollection productCollection = this.productCollectionRepository
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
              productMigration.getProductCode());
      ProductCollection newProductCollection = generateProductCollection(productMigration, productCollection);
      updateProductBusinessPartnerInformation(productMigration);
      newProductCollection = this.productCollectionRepository.save(newProductCollection);
      updateSolrProductCollectionDocument(newProductCollection);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public ProductCollection updateReviewType(String storeId, String productCode, String reviewType) {
    ProductCollection productCollection = productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    if (Objects.nonNull(productCollection)) {
      productCollection.setAutoNeedRevision(false);
      productCollection.setNeedRevision(false);
      productCollection.setAutoNeedRevisionCount(0);
      if (StringUtils.isNotBlank(reviewType) && StringUtils.isNotBlank(productCollection.getReviewType())) {
        String[] reviewTypes = productCollection.getReviewType().split(Constants.COMMA);
        String updatedReviewTypes =
            Arrays.stream(reviewTypes).filter(type -> !type.toLowerCase().contains(reviewType.toLowerCase()))
                .collect(Collectors.joining(Constants.COMMA));
        productCollection.setReviewType(StringUtils.isNotBlank(updatedReviewTypes) ? updatedReviewTypes : null);
      } else if (StringUtils.isBlank(reviewType)) {
        productCollection.setReviewType(null);
        productCollection.setReviewPending(false);
        productCollection.setRestrictedKeywordsPresent(false);
        productCollection.setAutoApprovalType(AutoApprovalType.NA);
        productCollection.setEdited(false);
      } else {
        return productCollection;
      }
      return productCollectionRepository.save(productCollection);
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorCategory.INVALID_STATE.getMessage());
    }
  }

  @Override
  public String isProductActivationNeeded(String storeId, String productId) throws Exception {
    List<ProductBusinessPartner> businessPartners = productBusinessPartnerRepository.findByStoreIdAndProductId(storeId, productId);
    List<ProductBusinessPartner> inProgressProducts = Optional.ofNullable(businessPartners)
        .orElse(new ArrayList<>()).stream().filter(l3 -> STATE_IN_PROGRESS.equalsIgnoreCase(l3.getState())
            && Boolean.FALSE.equals(l3.isMarkForDelete())).collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(inProgressProducts) && inProgressProducts.size() == 1) {
      return businessPartners.get(0).getGdnProductSku();
    } else if (CollectionUtils.isNotEmpty(inProgressProducts)) {
      log.error("Error on activation , product id mapped to more than one L3, productId = {} ", productId);
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT, ApiErrorCode.PRODUCT_ID_MAPPED_MORE_THAN_1_L3.getDesc());
    } else if (CollectionUtils.isNotEmpty(businessPartners)) {
      String productSku = StringUtils.EMPTY;
      try {
        BasicProductResponse basicProductResponse =
            xProductOutbound.getBasicProductInfo(businessPartners.get(0).getGdnProductSku());
        if (basicProductResponse.isMarkForDelete() && basicProductResponse.isForceReview()) {
          productSku = basicProductResponse.getProductSku();
        }
      } finally {
        return productSku;
      }
    } else {
      return StringUtils.EMPTY;
    }
  }

  private ProductCollection generateProductCollection(ProductMigration productMigration, ProductCollection productCollection)
      throws Exception {
    ProductCollection newProductCollection = new ProductCollection();
    BeanUtils.copyProperties(productCollection, newProductCollection, "id", "productCode", "productId",
        "businessPartnerCode", "businessPartnerName");
    newProductCollection.setProductId(productMigration.getMigratedProductId());
    newProductCollection.setProductCode(productMigration.getMigratedProductCode());
    newProductCollection.setBusinessPartnerCode(productMigration.getBusinessPartnerId());
    newProductCollection.setBusinessPartnerName(productMigration.getBusinessPartnerName());
    newProductCollection.setProductCreationType(ProductCreationType.MIGRATION.getProductCreationType());
    return newProductCollection;
  }

  private void updateProductBusinessPartnerInformation(ProductMigration productMigration) throws Exception {
    this.productBusinessPartnerRepository
        .updateProductIdForMigration(productMigration.getMigratedProductId(), productMigration.getProductId(),
            productMigration.getGdnProductSku());
  }

  private void createdProductBusinessPartnerDetails(ProductMigration productMigration,
      ProductAndItemsResponse productAndItemsResponse) throws Exception {
    GdnRestListResponse<ProductDetailResponse> productDetailResponseGdnRestListResponse = productRepository
        .getAllProductDetailsByProductCodes(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            Arrays.asList(productMigration.getMigratedProductCode()));
    ProductDetailResponse productDetailResponse = productDetailResponseGdnRestListResponse.getContent().get(0);
    ProductCollection productCollection = saveProductCollection(productMigration, productDetailResponse);
    addProductBusinessPartnerDetails(productMigration, productAndItemsResponse, productDetailResponse,
        productCollection);
  }

  private ProductCollection saveProductCollection(ProductMigration productMigration,
      ProductDetailResponse productDetailResponse) {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(productMigration.getMigratedProductCode());
    productCollection.setProductId(productMigration.getMigratedProductId());
    productCollection.setBusinessPartnerCode(productMigration.getBusinessPartnerId());
    productCollection.setBusinessPartnerName(productMigration.getBusinessPartnerName());
    productCollection.setState("ACTIVE");
    productCollection.setImageResized(true);
    productCollection.setActivated(true);
    productCollection.setViewable(true);
    productCollection.setStoreId(Constants.DEFAULT_STORE_ID);
    productCollection.setBrand(productDetailResponse.getBrand());
    productCollection.setProductName(productDetailResponse.getName());
    productCollection.setProductCreationType(ProductCreationType.MIGRATION.getProductCreationType());
    ProductCategoryResponse productCategoryResponse = productDetailResponse.getProductCategoryResponses().get(0);
    productCollection.setCategoryCode(productCategoryResponse.getCategory().getCategoryCode());
    productCollection.setCategoryName(productCategoryResponse.getCategory().getName());
    productCollection = this.productCollectionRepository.save(productCollection);
    updateSolrProductCollectionDocument(productCollection);
    return productCollection;
  }

  private void addProductBusinessPartnerDetails(ProductMigration productMigration,
      ProductAndItemsResponse productAndItemsResponse, ProductDetailResponse productDetailResponse,
      ProductCollection productCollection) {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setProductId(productMigration.getMigratedProductId());
    productBusinessPartner.setBusinessPartnerId(productMigration.getBusinessPartnerId());
    productBusinessPartner.setGdnProductSku(productMigration.getGdnProductSku());
    productBusinessPartner.setState("ACTIVE");
    productBusinessPartner.setActivated(true);
    productBusinessPartner.setBrand(productCollection.getBrand());
    productBusinessPartner.setCategoryCode(productCollection.getCategoryCode());
    productBusinessPartner.setCategoryName(productCollection.getCategoryName());
    productBusinessPartner.setCreatedBy(Constants.SYSTEM);
    productBusinessPartner.setCreatedDate(new Date());
    productBusinessPartner.setStoreId(Constants.DEFAULT_STORE_ID);
    productBusinessPartner.setProductName(productCollection.getProductName());
    Map<String, ProductItemResponse> itemsMap = productDetailResponse.getProductItemResponses().stream().collect(
        Collectors.toMap(productItemResponse -> productItemResponse.getGeneratedItemName(), Function.identity()));
    addProductBusinessPartnerItems(productAndItemsResponse, productBusinessPartner, itemsMap);
    addProductBusinessPartnerAttributes(productMigration, productAndItemsResponse, productBusinessPartner);
    productBusinessPartnerRepository.save(productBusinessPartner);
  }

  private void addProductBusinessPartnerItems(ProductAndItemsResponse productAndItemsResponse,
      ProductBusinessPartner productBusinessPartner, Map<String, ProductItemResponse> itemsMap) {
    List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList<>();
    for (ItemResponse itemResponse : productAndItemsResponse.getItems()) {
      ProductItemResponse productItemResponse = itemsMap.get(itemResponse.getMasterDataItem().getGeneratedItemName());
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
      if (CollectionUtils.isNotEmpty(itemResponse.getItemViewConfigs())) {
        productItemBusinessPartner.setBuyable(itemResponse.getItemViewConfigs().stream().findFirst().get().isBuyable());
        productItemBusinessPartner
            .setDisplay(itemResponse.getItemViewConfigs().stream().findFirst().get().isDiscoverable());
      }
      productItemBusinessPartner.setGdnProductItemSku(itemResponse.getItemSku());
      productItemBusinessPartner.setMerchantSku(itemResponse.getMerchantSku());
      productItemBusinessPartner.setPickupPointId(itemResponse.getPickupPointCode());
      productItemBusinessPartner.setProductItemId(productItemResponse.getId());
      productItemBusinessPartner.setGdnProductItemSku(itemResponse.getItemSku());
      productItemBusinessPartner.setMinimumStock(Constants.MINIMUM_STOCK);
      productItemBusinessPartner.setStock(Constants.MINIMUM_STOCK);
      List<PriceDTO> price = new ArrayList<>(itemResponse.getPrice());
      productItemBusinessPartner.setPrice(price.get(0).getListPrice());
      productItemBusinessPartner.setSalePrice(price.get(0).getOfferPrice());
      productItemBusinessPartner.setCreatedBy(Constants.SYSTEM);
      productItemBusinessPartner.setCreatedDate(new Date());
      productItemBusinessPartner.setStoreId(Constants.DEFAULT_STORE_ID);
      productItemBusinessPartner.setProductType(productAndItemsResponse.getProduct().getProductType().getCode());
      productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
      productItemBusinessPartners.add(productItemBusinessPartner);
    }
    productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartners);
  }

  private void addProductBusinessPartnerAttributes(ProductMigration productMigration,
      ProductAndItemsResponse productAndItemsResponse, ProductBusinessPartner productBusinessPartner) {
    List<ProductSpecialAttributeDTO> specialAttributeDTOList =
        productAndItemsResponse.getProduct().getProductSpecialAttributes();
    if (CollectionUtils.isNotEmpty(specialAttributeDTOList)) {
      List<ProductBusinessPartnerAttribute> productBusinessPartnerAttributes = new ArrayList<>();
      for (ProductSpecialAttributeDTO productSpecialAttributeDTO : specialAttributeDTOList) {
        ProductBusinessPartnerAttribute productBusinessPartnerAttribute = new ProductBusinessPartnerAttribute();
        try {
          AttributeResponse attributeResponse =
              productOutbound.getAttributeDetailByAttributeCode(productSpecialAttributeDTO.getAttributeCode());
          productBusinessPartnerAttribute.setAttributeId(attributeResponse.getId());
          productBusinessPartnerAttribute.setValue(productSpecialAttributeDTO.getAttributeValue());
          productBusinessPartnerAttribute.setProductBusinessPartner(productBusinessPartner);
          productBusinessPartnerAttribute.setCreatedBy(Constants.SYSTEM);
          productBusinessPartnerAttribute.setCreatedDate(new Date());
          productBusinessPartnerAttribute.setStoreId(Constants.DEFAULT_STORE_ID);
          productBusinessPartnerAttributes.add(productBusinessPartnerAttribute);
        } catch (Exception e) {
          log.warn("Special attribute not found during migration for attribute code : {} and product sku : {} ",
              productSpecialAttributeDTO.getAttributeCode(), productMigration.getGdnProductSku(), e);
          continue;
        }
      }
      productBusinessPartner.setProductBusinessPartnerAttributes(productBusinessPartnerAttributes);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateEditedImagePathsAndFlagAfterResizingImage(ProductAndItemImageRequest productAndItemImageRequest,
      ProductCollection productCollection,
      boolean resize, boolean setDgLevel) throws Exception {
    productCollection.setImageResized(resize);
    productOutbound.updateProductAndItemImagesByProductCode(setDgLevel, productAndItemImageRequest);
    productCollectionRepository.saveAndFlush(productCollection);
  }

  @Override
  public AddEditedProductToPDTEvent publishAddEditedProductToPDTEvent(String storeId, String reviewTypes,
      ProductCollection productCollection, List<String> allModifiedFields) throws Exception {
    ProfileResponse profileResponse = businessPartnerRepository.filterDetailByBusinessPartnerCode(
      productCollection.getBusinessPartnerCode());
    ProductBusinessPartner productBusinessPartner = productBusinessPartnerService
        .findFirstByStoreIdAndProductId(productCollection.getStoreId(), productCollection.getProductId());
    AddEditedProductToPDTEvent addEditedProductToPDTEvent =
        ConverterUtil.toAddEditedProductToPDTEvent(storeId, reviewTypes, productCollection,
          allModifiedFields, profileResponse, productBusinessPartner, priceInfoVendorEditedEnabled,
            priceInfoMaxVariantLimit);
    SellerDetailResponse sellerDetailResponse = null;
    try {
      sellerDetailResponse = productAnalyticsOutbound.getSellerDetail(productCollection.getBusinessPartnerCode());
    }catch (Exception e)
    {
      log.error("Exception while getting sellerDetail Response from ProductAnalytics for businessPartnerCode : {} ",
          productCollection.getProductCode(), e);
    }
    if (Objects.nonNull(sellerDetailResponse)) {
      addEditedProductToPDTEvent.setSellerBadge(sellerDetailResponse.getSellerBadge());
    }
    if(brandCategoryEditEnabledForExternal){
      addEditedProductToPDTEvent.setBrand(productCollection.getBrand());
      addEditedProductToPDTEvent.setBrandCode(productCollection.getBrandCode());
    }
    kafkaProducer.send(kafkaTopicProperties.getVendorCombinedEventNoPriority(), productCollection.getProductCode(),
        AddProductToVendorCombinedEventModel.builder().addEditedProductToPDTEvent(addEditedProductToPDTEvent).build());
    return addEditedProductToPDTEvent;
  }

  @Override
  public ProductImageQcProcessingResponse getProductImageQcProcessingResponse(String storeId, String productCode) {
    return productImageQcProcessingResponseService.findByStoreIdAndProductCode(storeId, productCode);
  }

  @Override
  public ProductImageQcFeedbackResponse getImageQcResponseFromPDT(String storeId, String productCode) {
    return productDistributionTaskRepositoryBean.getProductImageQcFeedback(storeId, productCode);
  }

  @Override
  public PDTProductDomainEventModel getPDTDomainModelResponseByCode(String productCode) throws Exception {
    return productDistributionTaskRepositoryBean
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCode);
  }

  @Override
  @Transactional(readOnly = false)
  public AutoNeedRevisionAndForceReviewResponse updateImageQcResponse(String storeId,
      ImageQcResponseDomainEvent imageQcResponseDomainEvent,
      ProductImageQcProcessingResponse productImageQcProcessingResponse, ProductCollection productCollection,
      Map<String, Long> imageCountMap, ProductImageQcFeedbackResponse imageQcResponseFromPDT,
      PDTProductDomainEventModel pdtDomainModelResponseByCode, ProductDetailResponse productDetailResponse,
    ProfileResponse profileResponse) throws Exception {
    boolean existingForceReviewFlag = productImageQcProcessingResponse.isForceReview();
    String existingImageViolation = productImageQcProcessingResponse.getImageViolations();
    String existingTextViolation = productImageQcProcessingResponse.getTextViolations();
    AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse =
        new AutoNeedRevisionAndForceReviewResponse();
    Map<String, ProductImagePrediction> productImagePredictionMap =
        getProductImagePredictionMap(storeId, imageQcResponseDomainEvent);
    updateNeedRevisionThresholdWithSellerTypeThreshold(storeId, productCollection, productImagePredictionMap);
    updateTakeDownRulesBasedOnCategory(productDetailResponse, productImagePredictionMap);
    BrandAndCategoryTakeDownResponse takeDownProductBasedOnBrandAndCategory =
      isTakeDownProductBasedOnBrandAndCategory(storeId, imageQcResponseDomainEvent,
        productCollection, profileResponse);
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
        restrictedKeywordsValidationByDs(imageQcResponseDomainEvent, productCollection);
    if (ConverterUtil.skipAllActionsUnlessCategoryChange(profileResponse.isTrustedSeller(),
        restrictedKeywordsByFieldAndActionType)) {
      restrictedKeywordsByFieldAndActionType.setAction(SKIP_ALL_ACTIONS);
    }
    if (
      RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType()
        == restrictedKeywordsByFieldAndActionType.getAction()) {
      log.info("Overriding category take down flag if the resultant action is category change for"
        + " productCode = {} ", productCollection.getProductCode());
      takeDownProductBasedOnBrandAndCategory.setCategoryTakeDown(false);
    }
    autoNeedRevisionAndForceReviewResponse.setRestrictedKeywordsByFieldAndActionType(
        restrictedKeywordsByFieldAndActionType);
    ImageQcResponseDomainEvent storedImageQcResponseInDb =
        objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class);
    boolean updateImageQcData = false;
    if (!productCollection.isPostLive()) {
      updateImageQcData = true;
    } else {
      updateImageQcData = updateImageQcData(storedImageQcResponseInDb, imageQcResponseFromPDT);
    }
    log.info("Image-qc-update productCode : {}, updateImageQcData : {}", updateImageQcData,
        productCollection.getProductCode());
    int productPredictionScore = updateImageQcData ? productImageQcProcessingResponse.getProductPredictionScore() : 0;
    String imageViolations = StringUtils.EMPTY;
    String textViolations = StringUtils.EMPTY;
    boolean trustedSeller = profileResponse.isTrustedSeller();
    HashMap<String, String> violationMap = new HashMap<>();
    for (ImageQcResponse imageQcResponse : imageQcResponseDomainEvent.getImages()) {
      imageQcResponse.setEdited(true);
      for (ImageQcPredictionResponse imageQcPredictionResponse : imageQcResponse.getPredictions()) {
        ProductImagePrediction productImagePrediction =
            productImagePredictionMap.get(imageQcPredictionResponse.getPredictionType());
        if (Objects.nonNull(productImagePrediction)) {
          if (imageQcPredictionResponse.getConfidence() > productImagePrediction.getConfidenceThreshold()) {
            imageViolations = populateImageQcWhenTrue(imageViolations, violationMap, imageQcPredictionResponse,
                productImagePrediction);
            productPredictionScore +=
              productImagePrediction.getPredictionWeightage() * imageCountMap.getOrDefault(
                imageQcResponse.getHashCode(), Constants.DEFAULT_LONG_VALUE);
            checkForAutoNeedRevisionAndForceReview(productCollection.isPostLive(),
              autoNeedRevisionAndForceReviewResponse, imageQcPredictionResponse,
              productImagePrediction, productCollection.isAutoNeedRevision(), trustedSeller);
          } else {
            populateImageQcWhenFalse(imageQcPredictionResponse, productImagePrediction);
          }
        }
      }
    }
    PredictionScoreAndViolationsDto textRestrictionModelResponse =
        getTextRestrictionModelResponse(imageQcResponseDomainEvent, productImagePredictionMap,
            productCollection.isPostLive(), productCollection.isAutoNeedRevision(), productPredictionScore,
            textViolations, autoNeedRevisionAndForceReviewResponse, violationMap, trustedSeller);
    productPredictionScore = textRestrictionModelResponse.getProductPredictionScore();
    textViolations = textRestrictionModelResponse.getViolations();
    imageViolations = takeDownProductBasedOnBrandAndCategory(takeDownProductBasedOnBrandAndCategory, imageViolations,
        autoNeedRevisionAndForceReviewResponse, violationMap, productCollection.isPostLive());
    storedImageQcResponseInDb.getImages().addAll(imageQcResponseDomainEvent.getImages());
    storedImageQcResponseInDb.setRestrictionModels(imageQcResponseDomainEvent.getRestrictionModels());
    storedImageQcResponseInDb.setKeywordRestrictionModels(imageQcResponseDomainEvent.getKeywordRestrictionModels());
    storedImageQcResponseInDb.setCategoryModels(imageQcResponseDomainEvent.getCategoryModels());
    storedImageQcResponseInDb.setBrandModels(imageQcResponseDomainEvent.getBrandModels());

    if (productCollection.isPostLive()) {
      setExistingImagesAsEditedInImageQcResponse(pdtDomainModelResponseByCode, storedImageQcResponseInDb);
    }
    setAutoNeedRevisionFlagsIfApplicable(restrictedKeywordsByFieldAndActionType.getAction(),
        autoNeedRevisionAndForceReviewResponse);
    setForceReviewFlagsIfApplicable(productCollection.isPostLive(), takeDownProductBasedOnBrandAndCategory,
        restrictedKeywordsByFieldAndActionType.getAction(), autoNeedRevisionAndForceReviewResponse);
    imageViolations = updateImageViolations(productImageQcProcessingResponse, updateImageQcData, imageViolations);
    log.info("Image-qc-update productCode : {}, images : {}, productViolations : {}",
        productCollection.getProductCode(), storedImageQcResponseInDb, imageViolations);
    updateImageQcProcessingData(productImageQcProcessingResponse,
        autoNeedRevisionAndForceReviewResponse.isForceReview(), storedImageQcResponseInDb, productPredictionScore,
        imageViolations, textViolations, takeDownProductBasedOnBrandAndCategory);
    boolean updatedForceReview = productImageQcProcessingResponse.isForceReview();
    String updatedImageViolation = productImageQcProcessingResponse.getImageViolations();
    String updatedTextViolation = productImageQcProcessingResponse.getTextViolations();
    if (overrideForceReview && productCollection.isReviewPending()) {
      productImageQcProcessingResponse.setForceReview(
        CommonUtils.setForceReviewFlag(existingForceReviewFlag, updatedForceReview));
      productImageQcProcessingResponse.setImageViolations(
        CommonUtils.setImageQcViolation(existingImageViolation, updatedImageViolation));
      productImageQcProcessingResponse.setTextViolations(
        CommonUtils.setImageQcViolation(existingTextViolation, updatedTextViolation));
    }
    productImageQcProcessingResponseService.save(productImageQcProcessingResponse);
    autoNeedRevisionAndForceReviewResponse.setBrandTakeDown(takeDownProductBasedOnBrandAndCategory.isTakeDown());
    autoNeedRevisionAndForceReviewResponse.setCategoryTakeDown(
      takeDownProductBasedOnBrandAndCategory.isCategoryTakeDown());
    return autoNeedRevisionAndForceReviewResponse;
  }

  private void setExistingImagesAsEditedInImageQcResponse(PDTProductDomainEventModel pdtDomainModelResponseByCode,
      ImageQcResponseDomainEvent storedImageQcResponseInDb) {
    Map<String, String> locationAndImageNameMapPDTData = new HashMap<>();
    Map<String, String> locationAndImageNameMapImageQcEventData = new HashMap<>();
    List<String> imageList;
    for(ImageDomainEventModel imageDomainEventModel : pdtDomainModelResponseByCode.getImages()) {
      String imageName = imageDomainEventModel.getLocationPath()
          .substring(imageDomainEventModel.getLocationPath().lastIndexOf(Constants.DELIMITER_SLASH) + 1);
      locationAndImageNameMapPDTData.putIfAbsent(imageDomainEventModel.getLocationPath(), imageName);
    }
    for(ImageQcResponse imageQcResponse : storedImageQcResponseInDb.getImages()) {
      String imageName = imageQcResponse.getLocationPath()
          .substring(imageQcResponse.getLocationPath().lastIndexOf(Constants.DELIMITER_SLASH) + 1);
      locationAndImageNameMapImageQcEventData.putIfAbsent(imageQcResponse.getLocationPath(), imageName);
    }
    imageList = locationAndImageNameMapImageQcEventData.entrySet().stream()
        .filter(imageQcEventData -> locationAndImageNameMapPDTData.containsValue(imageQcEventData.getValue()))
        .map(Map.Entry::getKey).collect(Collectors.toList());
    // Here existing images which are mapped to product are made as edited, so that image review step 1 these images appear
    for (ImageQcResponse imageQcResponse : storedImageQcResponseInDb.getImages()) {
      if (imageList.contains(imageQcResponse.getLocationPath())) {
        imageQcResponse.setEdited(true);
      } else {
        imageQcResponse.setEdited(false);
      }
    }
  }

  private void updateImageQcProcessingData(ProductImageQcProcessingResponse productImageQcProcessingResponse,
      boolean takeDownProduct, ImageQcResponseDomainEvent storedImageQcResponseInDb, int productPredictionScore,
      String imageViolations, String textViolations, BrandAndCategoryTakeDownResponse takeDownProductBasedOnBrand) throws JsonProcessingException {
    ProductImageQcProcessingResponse imageQcProcessingResponse =
        getProductImageQcProcessingResponse(storedImageQcResponseInDb, productPredictionScore, imageViolations,
            takeDownProduct, textViolations, takeDownProductBasedOnBrand);
    productImageQcProcessingResponse.setProductPredictionScore(imageQcProcessingResponse.getProductPredictionScore());
    productImageQcProcessingResponse.setImageViolations(imageQcProcessingResponse.getImageViolations());
    productImageQcProcessingResponse.setTextViolations(imageQcProcessingResponse.getTextViolations());
    productImageQcProcessingResponse.setForceReview(imageQcProcessingResponse.isForceReview());
    productImageQcProcessingResponse.setImageQcResponse(imageQcProcessingResponse.getImageQcResponse());
  }

  private String updateImageViolations(ProductImageQcProcessingResponse productImageQcProcessingResponse,
      boolean updateImageQcData, String imageViolations) {
    if (updateImageQcData) {
      List<String> imageViolationsString = new ArrayList<>();
      if (StringUtils.isNotEmpty(imageViolations)) {
        imageViolationsString = Arrays.asList(imageViolations.split(Constants.COMMA));
      }
      List<String> existingImageViolations = new ArrayList<>();
      if (StringUtils.isNotEmpty(productImageQcProcessingResponse.getImageViolations())) {
        existingImageViolations =
            Arrays.asList(productImageQcProcessingResponse.getImageViolations().split(Constants.COMMA));
      }
      return Stream.concat(imageViolationsString.stream(), existingImageViolations.stream()).distinct()
          .collect(Collectors.joining(Constants.COMMA));
    }
    return imageViolations;
  }

  private boolean updateImageQcData(ImageQcResponseDomainEvent storedImageQcResponseInDb,
      ProductImageQcFeedbackResponse imageQcResponseFromPDT) throws IOException {
    List<String> imagesPresentInDb;
    imagesPresentInDb = storedImageQcResponseInDb.getImages().stream().map(ImageQcResponse::getLocationPath)
        .collect(Collectors.toList());
    if (Objects.isNull(imageQcResponseFromPDT) || Objects.isNull(imageQcResponseFromPDT.getUserFeedback())) {
      return true;
    }
    UserFeedbackImageQcListResponse userFeedbackImageFromPDT =
        objectMapper.readValue(imageQcResponseFromPDT.getUserFeedback(), UserFeedbackImageQcListResponse.class);
    return imagesPresentInDb.size() > userFeedbackImageFromPDT.getUserFeedback().size();
  }

  @Override
  public void takeDownOrActivateProductByProductCode(String storeId, String productCode, boolean takeDown)
      throws Exception {
    List<String> productSkuByProductCode = productBusinessPartnerService.getProductSkusByProductCode(productCode);
    productLevel3Service.takeDownOrReactivateProduct(storeId, productSkuByProductCode.get(0), takeDown, null, null);
  }

  @Override
  public void publishEditedProduct(String storeId, String productCode, String reviewType)
    throws Exception {
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    publishAddEditedProductToPDTEvent(storeId, reviewType, productCollection, null);
  }

  @Override
  @Transactional(readOnly = false)
  public void updateImageQcDataAfterVendorApproval(String storeId, String productCode) throws IOException {
    ImageQcResponseDomainEvent imageQcResponseDomainEvent;
    ProductImageQcProcessingResponse productImageQcProcessingResponse =
        productImageQcProcessingResponseService.findByStoreIdAndProductCodeDb(storeId, productCode);
    if (Objects.nonNull(productImageQcProcessingResponse) && StringUtils
        .isNotBlank(productImageQcProcessingResponse.getImageQcResponse())) {
      imageQcResponseDomainEvent = objectMapper
          .readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class);
      for (ImageQcResponse imageQcResponse : imageQcResponseDomainEvent.getImages()) {
        imageQcResponse.setMarkForDelete(true);
        imageQcResponse.setEdited(false);
      }
      productImageQcProcessingResponse.setImageQcResponse(objectMapper.writeValueAsString(imageQcResponseDomainEvent));
      productImageQcProcessingResponse.setForceReview(false);
      productImageQcProcessingResponse.setPredictedBrand(StringUtils.EMPTY);
      productImageQcProcessingResponseService.save(productImageQcProcessingResponse);
    }
  }

  @Override
  public ActivateImageResponse updateActiveImagesAndGetActivateImageResponse(String productCode,
      List<ScaleImageResponse> imageResponses) throws Exception {
    ProductActivateImageRequest productActivateImageRequest = new ProductActivateImageRequest();
    productActivateImageRequest.setProductCode(productCode);
    Set<ActivateImageRequest> activateImageRequests =
        imageResponses.stream().map(imageResponse -> ConverterUtil.getActivateImageRequest(productCode, imageResponse))
            .collect(Collectors.toSet());
    productActivateImageRequest.setImageRequests(activateImageRequests);
    LOGGER.info("Update product images request sending to PCB for productCode : {}", productCode);
    return this.updateProductImagesName(productActivateImageRequest, false);
  }

  @Override
  public AutoApprovalType findAutoApprovalTypeByRequest(String storeId, String username, String productCode,
      AutoApprovalTypeRequest autoApprovalTypeRequest, ProfileResponse profileResponse,
      ProductCollection productCollection) throws Exception {
    ProductDetailResponse productDetailResponse =
        this.productOutbound.getProductDetailByProductCode(productCode, false, true);
    if (Constants.CONTENT_AUTO_APPROVAL_CHECK.equals(autoApprovalTypeRequest.getReviewType())) {
      return this.autoApprovalService.verifyAutoApprovalRules(
          generateAutoApprovalDetailsDTO(storeId, productCode, productDetailResponse, null, autoApprovalTypeRequest.isEdited(),
              false, autoApprovalTypeRequest.isRevised(), profileResponse.isTrustedSeller(), productCollection));
    } else {
      ProductImageQcProcessingResponse imageQcProcessingResponse =
          productImageQcProcessingResponseService.findByStoreIdAndProductCode(storeId, productCode);
      if (Objects.isNull(imageQcProcessingResponse)) {
        return AutoApprovalType.NA;
      }
      return this.autoApprovalService.verifyAutoApprovalRules(
          generateAutoApprovalDetailsDTO(storeId, productCode, productDetailResponse, imageQcProcessingResponse,
              autoApprovalTypeRequest.isEdited(), true, autoApprovalTypeRequest.isRevised(),
            profileResponse.isTrustedSeller(),productCollection));
    }
  }

  @Override
  @Transactional(readOnly = false)
  public ProductCollection saveProductCollection(ProductCollection productCollection) {
    return productCollectionRepository.save(productCollection);
  }

  @Override
  public void removeProductFromPDT(String productCode) throws Exception {
    RemoveProductRequest removeProductRequest = new RemoveProductRequest(productCode);
    productDistributionService
        .removeProductFromPDT(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, removeProductRequest);
  }

  @Override
  public void deleteFromReviewProductCollection(List<String> productIds) {
    SolrReviewProductCollectionDeleteEvent solrReviewProductCollectionDeleteEvent =
        new SolrReviewProductCollectionDeleteEvent();
    solrReviewProductCollectionDeleteEvent.setIds(productIds);
    kafkaProducer.send(DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST, solrReviewProductCollectionDeleteEvent);
  }

  @Override
  public boolean checkIfProductExistsInPDT(String productCode, boolean allProducts) {
    return productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(productCode, allProducts);
  }

  @Override
  public void addProductToReviewCollection(ProductCollection productCollection) throws Exception {
    this.solrReviewProductCollectionService.publishKafkaEventToAddProductToReviewProductCollection(productCollection);
  }

  @Override
  public void updateAutoApprovalTypeByProductCode(AutoApprovalType autoApprovalType, String productCode) {
    this.productCollectionRepository.updateAutoApprovalTypeByProductCode(autoApprovalType, productCode);
  }

  @Override
  public InProgressProductResponsePageResponse findInProgressProductsByMerchantCode(String storeId,
      String merchantCode, int page, int size) {
    List<InProgressProductResponse> inProgressProductResponseList =
        this.productBusinessPartnerService.findByStoreIdAndBusinessPartnerIdAndStateAndMarkForDeleteFalse(
            storeId, merchantCode, IN_PROGRESS_STATE);
    return new InProgressProductResponsePageResponse(inProgressProductResponseList.stream()
        .filter(CommonUtils.distinctByKey(InProgressProductResponse::getProductSku))
        .collect(Collectors.toList()), inProgressProductResponseList.size());
  }

  @Override
  public String getProductStatus(String storeId, String productCode) {
    ProductCollection productCollection = productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    return Optional.ofNullable(productCollection).map(ProductCollection::getState).orElse(StringUtils.EMPTY);
  }

  @Override
  public void publishDimensionRefreshEventForReviewPendingProducts(String storeId, String productCode,
      DimensionRefreshRequest dimensionRefreshRequest) {
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    // process only if product is review pending true
    if (Objects.nonNull(productCollection) && productCollection.isReviewPending()) {
      productPublisherService.publishProductDimensionRefreshEvent(
          ConverterUtil.toPDTDimensionRefreshEventModel(storeId, productCode, dimensionRefreshRequest));
    }
  }

  @Override
  public CogsValueResponse fetchCogsValueByMaterialCode(String storeId, String materialCode) {
    return productLevel3Service.fetchCogsValueByMaterialCode(materialCode);
  }

  @Override
  public String processImageQcForBacklogProducts(String storeId,
      ImageQcResponseDomainEvent imageQcResponseDomainEvent) {
    Map<String, ProductImagePrediction> productImagePredictionMap =
        getProductImagePredictionMap(storeId, imageQcResponseDomainEvent);
    String imageViolations = StringUtils.EMPTY;
    HashMap<String, String> imageViolationMap = new HashMap<>();
    for (ImageQcResponse imageQcResponse : imageQcResponseDomainEvent.getImages()) {
      for (ImageQcPredictionResponse imageQcPredictionResponse : imageQcResponse.getPredictions()) {
        ProductImagePrediction productImagePrediction =
            productImagePredictionMap.get(imageQcPredictionResponse.getPredictionType());
        if (Objects.nonNull(productImagePrediction) && !productImagePrediction.isMarkForDelete()) {
          if (imageQcPredictionResponse.getConfidence() > productImagePrediction.getConfidenceThreshold()) {
            imageViolations = populateImageQcWhenTrue(imageViolations, imageViolationMap, imageQcPredictionResponse,
                productImagePrediction);
          } else {
            populateImageQcWhenFalse(imageQcPredictionResponse, productImagePrediction);
          }
        }
      }
    }
    return imageViolations;
  }

  private String populateImageQcWhenTrue(String imageViolations, HashMap<String, String> imageViolationMap,
      ImageQcPredictionResponse imageQcPredictionResponse, ProductImagePrediction productImagePrediction) {
    imageQcPredictionResponse.setPresent(true);
    imageViolations = addImageViolationIfNotPresent(imageViolations, imageViolationMap, productImagePrediction);
    imageQcPredictionResponse.setPredictionType(productImagePrediction.getPredictionType());
    imageQcPredictionResponse.setDisplayName(productImagePrediction.getDisplayName());
    return imageViolations;
  }

  private void populateImageQcWhenFalse(ImageQcPredictionResponse imageQcPredictionResponse,
      ProductImagePrediction productImagePrediction) {
    imageQcPredictionResponse.setPresent(false);
    imageQcPredictionResponse.setPredictionType(productImagePrediction.getPredictionType());
    imageQcPredictionResponse.setDisplayName(productImagePrediction.getDisplayName());
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updatePbpProductWorkflowState(String storeId, String productCode, String state) {
    this.productWfService.deleteAllExistingWorkFlowAndCreateNewState(storeId, productCode, state);
  }

  @Override
  public void publishImageQcBacklogRequestEvent(ImageQcResponseDomainEvent imageQcResponseDomainEvent) {
    kafkaProducer.send(DomainEventName.IMAGE_QC_PREDICTION_BACKLOG_REQUEST, imageQcResponseDomainEvent.getProductCode(),
        imageQcResponseDomainEvent);
    log.info("Publish event com.gdn.image.qc.backlog.prediction.request with message {} ", imageQcResponseDomainEvent);
  }

  @Override
  public ItemsPriceStockImagesUpdateResponse editPriceStockVariantsInfo(String storeId, ProductLevel3 productLevel3,
      ProductVariantUpdateRequest productVariantUpdateRequest, EditProductResponse editResponse, boolean productDetailEdit) throws Exception {
    fileStorageService.editImageNameIfGcsEnabled(productVariantUpdateRequest);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
        productLevel3Service.editProductItemsPriceStockImagesL5(storeId, productLevel3.getBusinessPartnerCode(),
            productVariantUpdateRequest, editResponse, productLevel3, productDetailEdit);
    Optional.ofNullable(itemsPriceStockImagesUpdateResponse).ifPresent(response -> response.setScheduleRemovedForStatusUpdate(
        editResponse.getScheduleRemovedForStatusUpdate()));
    if (editResponse.isPublishImageQcForContentChange() && !itemsPriceStockImagesUpdateResponse.isNewImagesAdded()) {
      log.info("Publishing image qc event as content is changed but no new images added : {} ",
          productVariantUpdateRequest.getProductSku());
      CategoryResponse categoryResponse = new CategoryResponse();
      categoryResponse.setCategoryCode(productLevel3.getCategoryCode());
      categoryResponse.setName(productLevel3.getCategoryName());
      publishImageQcEventForContentEdit(CollectionUtils.isNotEmpty(editResponse.getCategoryResponses()) ?
              editResponse.getCategoryResponses() :
              Arrays.asList(categoryResponse), editResponse.getRestrictedKeywordsByFieldAndActionType(), productLevel3, false);
    }
    return itemsPriceStockImagesUpdateResponse;
  }

  @Override
  public ProfileResponse getProfileResponse(String businessPartnerCode) throws Exception {
    return this.businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public ProductCollection saveProductCollectionForUPC(ProductCollection productCollection) {
    return productCollectionRepository.save(productCollection);
  }

  @Override
  public void validatePickupPointsAndFbb(String requestId, ProductCreationRequest request, ProfileResponse profileResponse,
      Map<String, OmniChannelSkuResponse> existingSellerSkusAndProductDetailsMap)
      throws Exception {
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    Set<String> pickupPointCodesUsedInCreation = new HashSet<>();
    Set<String> omniChannelSkusInRequest = new HashSet<>();
    Map<String, String> omniChannelSkuAndItemNameInRequestMap = new HashMap<>();
    List<ProductItemDistributionInfoRequest> productItemDistributionInfoRequests = new ArrayList<>();
    pickupPointResponseList =
        getPickupPointResponses(requestId, request, pickupPointCodesUsedInCreation, pickupPointResponseList,
            productItemDistributionInfoRequests, omniChannelSkusInRequest, omniChannelSkuAndItemNameInRequestMap);
    if (ranchIntegrationEnabled && Optional.ofNullable(distributionSellerList).orElse(new HashSet<>())
        .contains(request.getBusinessPartnerCode())) {
      ranchRelatedValidationAndRequestFormation(request, productItemDistributionInfoRequests, omniChannelSkusInRequest,
          omniChannelSkuAndItemNameInRequestMap, pickupPointResponseList, existingSellerSkusAndProductDetailsMap);
    }
    validatePickupPoints(request, profileResponse, pickupPointCodesUsedInCreation, pickupPointResponseList);
    if (validateCreateRequestForFbb) {
      log.info("Validating fbb for productCode : {} ", request.getProductCode());
      CommonUtils.validateCreateRequestForFbb(request, pickupPointResponseList, profileResponse, mppAllowedSellers,
          mppForWhEnabled);
    }
  }

  private void ranchRelatedValidationAndRequestFormation(ProductCreationRequest request,
      List<ProductItemDistributionInfoRequest> productItemDistributionInfoRequests,
      Set<String> omniChannelSkusInRequest, Map<String, String> omniChannelSkuAndItemNameInRequestMap,
      List<PickupPointResponse> pickupPointResponseList,
      Map<String, OmniChannelSkuResponse> existingSellerSkusAndProductDetailsMap) throws Exception {
    List<String> distributionTruePickupPointCodes = pickupPointResponseList.stream().filter(
        pickupPointResponse -> Boolean.TRUE.equals(
            Optional.ofNullable(pickupPointResponse.getFlags()).orElse(new HashMap<>())
                .getOrDefault(Constants.DISTRIBUTION_FLAG_KEY, false))).map(PickupPointResponse::getCode).toList();
    // Validate distribution info not being empty when distribution pp is sent
    Map<String, ProductL1AndL2CodeResponse> omniChannelSkuToResponseMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(distributionTruePickupPointCodes)) {
    validateIncompleteDistributionRelatedData(request, productItemDistributionInfoRequests, distributionTruePickupPointCodes);
    DistributionInfoRequest distributionInfoRequest =
        getDistributionInfoRequest(request, productItemDistributionInfoRequests);
    if (CollectionUtils.isNotEmpty(omniChannelSkusInRequest)) {
      omniChannelSkuToResponseMap = productOutbound.getOmniChannelSkuToItemCode(distributionInfoRequest.getSellerCode(),
          new ArrayList<>(omniChannelSkusInRequest));
      existingSellerSkusAndProductDetailsMap.putAll(
          CommonUtils.convertToOmniChannelSkuResponse(new ValidOmniChannelSkuResponse(omniChannelSkuToResponseMap),
              omniChannelSkuAndItemNameInRequestMap));
    }
    distributionInfoServiceBean.validateDistributionInfoUpdateRequest(distributionInfoRequest, new HashMap<>(),
        omniChannelSkuToResponseMap);
    } else {
      if (CollectionUtils.isNotEmpty(omniChannelSkusInRequest)) {
        omniChannelSkuToResponseMap = productOutbound.getOmniChannelSkuToItemCode(request.getBusinessPartnerCode(),
            new ArrayList<>(omniChannelSkusInRequest));
        Map<String, String> omniChannelSkuToItemCodeMap = new HashMap<>();
        for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
          if (StringUtils.isNotBlank(productItemCreationRequest.getMerchantSku())) {
            distributionInfoServiceBean.validateDuplicateOmniChannelSku(omniChannelSkuToResponseMap,
                omniChannelSkuToItemCodeMap, productItemCreationRequest.getMerchantSku(), StringUtils.EMPTY);
          }
        }
      }
    }
    CommonUtils.overrideFlagsBasedOnDistributionPP(request, pickupPointResponseList);
  }

  private static void validateIncompleteDistributionRelatedData(ProductCreationRequest request,
      List<ProductItemDistributionInfoRequest> productItemDistributionInfoRequests,
      List<String> distributionTruePickupPointCodes) {
    if (CollectionUtils.isNotEmpty(distributionTruePickupPointCodes)) {
      CommonUtils.validateIfDistributionInfoIsMissing(productItemDistributionInfoRequests,
          request.getDistributionInfoRequest(), request.getProductCode());
    }
  }

  private static DistributionInfoRequest getDistributionInfoRequest(ProductCreationRequest request,
      List<ProductItemDistributionInfoRequest> productItemDistributionInfoRequests) {
    DistributionInfoRequest distributionInfoRequest = new DistributionInfoRequest();
    distributionInfoRequest.setSellerCode(request.getBusinessPartnerCode());
    distributionInfoRequest.setProductItems(productItemDistributionInfoRequests);
    distributionInfoRequest.setDistributionInfoRequest(request.getDistributionInfoRequest());
    return distributionInfoRequest;
  }

  private void validatePickupPoints(ProductCreationRequest request, ProfileResponse profileResponse,
      Set<String> pickupPointCodesUsedInCreation, List<PickupPointResponse> pickupPointResponseList) {
    if (validatePickupPoints) {
      log.info("Validating pickup point for productCode : {} ", request.getProductCode());
      if (StringUtils.isNotEmpty(request.getBusinessPartnerCode()) && !Constants.INTERNAL
          .equals(request.getBusinessPartnerCode())) {
        if (setWaitingDeletionForDeletePickupPoint) {
          CommonUtils.validatePickupPointsAndWaitingDeletion(request.getProductCode(), pickupPointCodesUsedInCreation,
              pickupPointResponseList, new SimpleStringResponse(), profileResponse.getBusinessPartnerCode());
        } else {
          CommonUtils.validatePickupPoints(request.getProductCode(), pickupPointCodesUsedInCreation,
              pickupPointResponseList, new SimpleStringResponse(), profileResponse.getBusinessPartnerCode());
        }
      }
    }
  }

  private List<PickupPointResponse> getPickupPointResponses(String requestId, ProductCreationRequest request,
      Set<String> pickupPointCodesUsedInCreation, List<PickupPointResponse> pickupPointResponseList,
      List<ProductItemDistributionInfoRequest> productItemDistributionInfoRequests,
      Set<String> omniChannelSkusInRequest, Map<String, String> omniChannelSkuAndItemNameInRequestMap) throws Exception {
    if (validatePickupPoints || validateCreateRequestForFbb) {
      for (ProductItemCreationRequest itemCreationRequest : request.getProductItemRequests()) {
        if (StringUtils.isNotBlank(itemCreationRequest.getMerchantSku())) {
          omniChannelSkusInRequest.add(itemCreationRequest.getMerchantSku());
          omniChannelSkuAndItemNameInRequestMap.put(itemCreationRequest.getItemGeneratedName(),
              itemCreationRequest.getMerchantSku());
        }
        itemCreationRequest.getPickupPoints().stream().map(PickupPointCreateRequest::getPickupPointId)
            .forEach(pickupPointCodesUsedInCreation::add);
        if (ranchIntegrationEnabled && Optional.ofNullable(distributionSellerList).orElse(new HashSet<>())
            .contains(request.getBusinessPartnerCode())) {
          DistributionItemRequest distributionItemInfoRequest = itemCreationRequest.getDistributionItemInfoRequest();
          if (Objects.nonNull(distributionItemInfoRequest)) {
            distributionItemInfoRequest.setOmniChannelSku(itemCreationRequest.getMerchantSku());
          }
          productItemDistributionInfoRequests.add(new ProductItemDistributionInfoRequest(request.getProductCode(),
              distributionItemInfoRequest, itemCreationRequest.getDimensionsAndUOMRequest(),
              itemCreationRequest.getItemGeneratedName()));
        }
      }
      pickupPointResponseList =
          pickupPointOutbound.getByPickupPointCodes(requestId, new ArrayList<>(pickupPointCodesUsedInCreation));
    }
    return pickupPointResponseList;
  }

  @Override
  public ProductCollection getProductCollectionByProductSku(String productSku) {
    return productCollectionRepository.findProductByGdnSku(productSku);
  }

  @Override
  public List<ProductCollection> fetchProductsToSync(String storeId, Date startUpdatedDate,
    Date endUpdatedDate) {
    checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(Objects.nonNull(startUpdatedDate), ErrorMessages.DATE_MUST_NOT_BE_NULL);
    checkArgument(Objects.nonNull(endUpdatedDate), ErrorMessages.DATE_MUST_NOT_BE_NULL);
    return productCollectionRepository
      .findByStoreIdAndUpdatedDateBetweenAndMarkForDeleteFalse(storeId, startUpdatedDate,
        endUpdatedDate);
  }

  @Override
  public Page<ProductCollection> fetchActiveProductsToSync(String storeId, Date startUpdatedDate, Date endUpdatedDate,
      Pageable pageable) {
    checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(Objects.nonNull(startUpdatedDate), ErrorMessages.DATE_MUST_NOT_BE_NULL);
    checkArgument(Objects.nonNull(endUpdatedDate), ErrorMessages.DATE_MUST_NOT_BE_NULL);
    return productCollectionRepository.findByStoreIdAndStateAndReviewPendingFalseAndUpdatedDateBetweenAndMarkForDeleteFalse(
        storeId, WorkflowStates.ACTIVE.getValue(), startUpdatedDate, endUpdatedDate, pageable);
  }

  @Override
  public Page<ProductCollection> fetchPreLiveProductsToSync(String storeId, String state, boolean postLive,
      boolean edited, int resubmitCount, Date startUpdatedDate, Date endUpdatedDate, Pageable pageable) {
    checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(Objects.nonNull(startUpdatedDate), ErrorMessages.DATE_MUST_NOT_BE_NULL);
    checkArgument(Objects.nonNull(endUpdatedDate), ErrorMessages.DATE_MUST_NOT_BE_NULL);
    return productCollectionRepository
        .findByStoreIdAndStateAndPostLiveAndEditedAndResubmitCountAndImageResizedTrueAndUpdatedDateBetweenAndMarkForDeleteFalse(
            storeId, state, postLive, edited, resubmitCount, startUpdatedDate, endUpdatedDate, pageable);
  }

  @Override
  public ApiErrorCode checkBpBopisEligibility(Integer productType, ProfileResponse profileResponse,
      CategoryDetailResponse categoryDetailResponse, ProductL3UpdateRequest productL3UpdateRequest,
      boolean isPureExternalUser) {
    ApiErrorCode apiErrorCode = checkBpBopisEligibility(productType, profileResponse);
    if (Objects.nonNull(apiErrorCode)) {
      return apiErrorCode;
    }

    if (Objects.nonNull(profileResponse) && Objects.nonNull(productType) && productType.equals(
        ProductType.BOPIS.getCode())) {
      if (isPureExternalUser && !categoryDetailResponse.isBopisEligible() && Arrays.asList(
              bopisUnsupportedMerchantTypes.split(Constants.COMMA))
          .contains(profileResponse.getCompany().getMerchantType())) {
        log.error("Seller {} is not eligible for the Product Type: {} ", profileResponse.getBusinessPartnerCode(),
            productType);
        apiErrorCode = ApiErrorCode.BP_BOPIS_ELIGIBILITY_ERROR;
      }

      if (Boolean.FALSE.equals(productL3UpdateRequest.getCnc())) {
        Stream.concat(Optional.ofNullable(productL3UpdateRequest.getProductItems()).orElse(new ArrayList<>()).stream()
                    .map(ProductVariantPriceStockAndImagesRequest::getModifiedItemPickupPoints).flatMap(List::stream),
                Optional.ofNullable(productL3UpdateRequest.getAddPickupPoints()).orElse(new ArrayList<>()).stream())
            .forEach(itemPickupPointRequest -> itemPickupPointRequest.setCncActive(false));
      }

      boolean isL5CncForBopisProduct = Stream.concat(
              Optional.ofNullable(productL3UpdateRequest.getProductItems()).orElse(new ArrayList<>()).stream()
                  .map(ProductVariantPriceStockAndImagesRequest::getModifiedItemPickupPoints).flatMap(List::stream),
              Optional.ofNullable(productL3UpdateRequest.getAddPickupPoints()).orElse(new ArrayList<>()).stream())
          .anyMatch(ItemPickupPointRequest::isCncActive);

      if (bopisCNCRestrictionEnabled && isL5CncForBopisProduct) {
        log.error("Seller {} is not eligible for the Product Type: {} and cnc item pickup point",
            profileResponse.getBusinessPartnerCode(), productType);
        apiErrorCode = ApiErrorCode.BOPIS_CNC_CHANGE_ERROR;
      }
    }
    return apiErrorCode;
  }

  @Override
  public ApiErrorCode checkBpBopisEligibility(Integer productType, ProfileResponse profileResponse) {
    ApiErrorCode apiErrorCode = null;
    if (bpBopisRestrictionEnabled && productType != null && Objects.nonNull(profileResponse)) {
      log.info("Validating seller eligibility for the Product Type : {}, Business Partner Code {} ",
          productType, profileResponse.getBusinessPartnerCode());
      boolean isBigProduct = productType.equals(ProductType.BIG_PRODUCT.getCode());
      boolean isBopisProduct = productType.equals(ProductType.BOPIS.getCode());
      boolean sellerBPFlag = Objects.nonNull(profileResponse.getBigProductFlag()) ?
          profileResponse.getBigProductFlag() : true;
      boolean sellerBopisFlag =
          Objects.nonNull(profileResponse.getBopisFlag()) ? profileResponse.getBopisFlag() : true;
      if ((isBigProduct && !sellerBPFlag) || (isBopisProduct && !sellerBopisFlag)) {
        log.error("Seller {} is not eligible for the Product Type: {} ",
            profileResponse.getBusinessPartnerCode(), productType);
        apiErrorCode = ApiErrorCode.BP_BOPIS_ELIGIBILITY_ERROR;
      }
    }
    return apiErrorCode;
  }

  @Override
  public ApiErrorCode checkProductLimitExceeded(String businessPartnerCode, String storeId) throws Exception {
    ProductSystemParameter productSystemParameter =
        productSystemParameterService.findByStoreIdAndVariable(storeId, Constants.PRODUCT_LIMIT_SYSTEM_PARAMETER);
    long currentProductCount = productLevel3V2Service.getProductCount(storeId, businessPartnerCode, true);
    if (Objects.nonNull(productSystemParameter) && currentProductCount > Long
        .parseLong(productSystemParameter.getValue())) {
      log.error("Product Limit Exceeded for businessPartnerCode {} and currentProductCount {} and limit is {} ",
          businessPartnerCode, currentProductCount, productSystemParameter.getValue());
      return ApiErrorCode.PRODUCT_LIMIT_REACHED;
    }
    return null;
  }

  @Override
  public ApiErrorCode validateSizeChart(String sizeChartCode, String storeId) {
    GdnBaseRestResponse sizeChartValidityResponse =
        pcbFeign.validateSizeChartCode(mandatoryParameterHelper.getStoreId(), mandatoryParameterHelper.getChannelId(),
            mandatoryParameterHelper.getClientId(), mandatoryParameterHelper.getRequestId(),
            mandatoryParameterHelper.getUsername(), sizeChartCode);
    if (!sizeChartValidityResponse.isSuccess()) {
      return ApiErrorCode.SIZE_CHART_CODE_INVALID;
    }
    return null;
  }

  @Override
  public void updateStatusInPCBForBackFillAttributes(String productCode, String updatedStatus, String errorMessage) {
    ProductMigrationRequest productMigrationRequest = new ProductMigrationRequest();
    productMigrationRequest.setProductCode(productCode);
    productMigrationRequest.setUpdatedStatus(updatedStatus);
    productMigrationRequest.setMigrationType(PRODUCT_ATTRIBUTE_UPDATE);
    productMigrationRequest.setErrorMessage(errorMessage);
    GdnBaseRestResponse updateProductMigrationStatus =
        pcbFeign.updateProductMigrationStatus(mandatoryParameterHelper.getStoreId(),
            mandatoryParameterHelper.getRequestId(), productMigrationRequest);
    if (!updateProductMigrationStatus.isSuccess()) {
      log.error("Failed to update the status in pcb for product code {}: with error {} : ", productCode,
          updateProductMigrationStatus.getErrorMessage());
    }
  }

  @Override
  public void validateSkipDefinitiveAction(String storeId, ProductCollection productCollection,
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsWithActionTypeInProductDetails) {
    ProductBusinessPartner productBusinessPartner =
      productBusinessPartnerService.findFirstByStoreIdAndProductId(storeId,
        productCollection.getProductId());
    AgpSimpleQueryResponse agpResponse =
      getQueryResponseForActiveOrderData(productCollection.getProductCode(),
        productBusinessPartner.getGdnProductSku());
    if (Objects.isNull(agpResponse)){
      return;
      }
    //If orders count is more than 0 then override action to default manual review
    if (agpResponse.getHits().getTotal() > 0) {
      overRideRestrictedKeywordActionOnActiveOrders(productCollection.getProductCode(),
        restrictedKeywordsWithActionTypeInProductDetails);
    }
  }

  @Override
  public void checkActiveOrderDataToSkipRestrictedKeywordAction(
    ProductMasterDataEditRequest productMasterDataEditRequest,
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsWithActionTypeInProductDetails) {
    AgpSimpleQueryResponse queryResponseForActiveOrderData =
      getQueryResponseForActiveOrderData(productMasterDataEditRequest.getProductCode(),
        productMasterDataEditRequest.getProductSku());
    if (Objects.isNull(queryResponseForActiveOrderData)) {
      return;
    }
    if (queryResponseForActiveOrderData.getHits().getTotal() > 0) {
      overRideRestrictedKeywordActionOnActiveOrders(productMasterDataEditRequest.getProductCode(),
        restrictedKeywordsWithActionTypeInProductDetails);
    }
  }

  @Override
  public void  overRideRestrictedKeywordActionOnActiveOrders(String productCode,
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsWithActionTypeInProductDetails) {
    log.info("Over-riding action {} for product code {}",
      restrictedKeywordsWithActionTypeInProductDetails.getAction(), productCode);
    restrictedKeywordsWithActionTypeInProductDetails.setAction(
      RestrictedKeywordActionType.MANUAL_REVIEW_DEFAULT.getRestrictedKeywordActionType());
    InternalProductHistoryEventModel internalProductHistoryEventModel =
      ConverterUtil.generateInternalProductHistoryEventModel(productCode,
        Constants.SKIP_DEFINITIVE_ACTIVITY, Constants.SYSTEM,
        Constants.SKIP_DEFINITIVE_DESCRIPTION);
    kafkaProducer.send(DomainEventName.PRODUCT_INTERNAL_HISTORY_SAVE, productCode,
      internalProductHistoryEventModel);
  }

  @Override
  public  AgpSimpleQueryResponse getQueryResponseForActiveOrderData(String productCode,
    String productSku) {
    AgpSimpleQueryResponse agpResponse;
    try {
      agpResponse =
        agpQueryFeign.findNumberOfOrder(productSku, String.valueOf(0), String.valueOf(0),
          Constants.AGP_ITEM_STATUS);
      log.info("Order found {} for product code {}", agpResponse.getHits().getTotal(), productCode);
    } catch (Exception ex) {
      log.error("Agp call to fetch order failed for product code {} ", productCode);
      return null;
    }
    return agpResponse;
  }

  @Override
  public void deleteProductCollectionByStoreIdAndProductCode(String storeId, String productCode){
    productCollectionRepository.deleteByStoreIdAndProductCode(storeId, productCode);
  }


  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public List<String> findProductsByAddDeleteVariantByPendingStatus(String storeId, String productCode, String requestId,
      String userName) throws Exception {
    if (StringUtils.isNotBlank(productCode)) {
      ProductCollection productCollection =
          productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
      GdnRestListResponse<ProductDetailResponse> productDetailResponseGdnRestListResponse =
          getProductDetailsByProductCodes(requestId, userName, Collections.singletonList(productCode));
      boolean newlyAddedItem =
          Optional.ofNullable(productDetailResponseGdnRestListResponse.getContent()).orElse(new ArrayList<>()).stream()
              .flatMap(productDetailResponse -> Optional.ofNullable(productDetailResponse.getProductItemResponses())
                  .orElse(new HashSet<>()).stream()).anyMatch(ProductItemResponse::isNewlyAddedItem);
      if (Objects.nonNull(productCollection) && newlyAddedItem) {
        productCollection.setAddDeleteVariantStatus(AddDeleteVariantStatus.PUBLISHED);
        productCollectionRepository.save(productCollection);
        return Collections.singletonList(productCode);
      }
    } else {
      int batchSize = Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
          SystemParameterConstants.FETCH_ADD_DELETE_VARIANT_PENDING_STATUS_BATCH_SIZE).getValue());
      int beforeHours = Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
          SystemParameterConstants.FETCH_ADD_DELETE_VARIANT_PENDING_STATUS_IN_HOURS).getValue());
      Page<ProductCollection> productCollectionList =
          productCollectionRepository.findByAddDeleteVariantStatusAndMarkForDeleteFalseAndCreatedDateBefore(
              AddDeleteVariantStatus.PENDING, calculateTimestampBefore(beforeHours),
              PageRequest.of(DEFAULT_PAGE, batchSize));
      if (CollectionUtils.isNotEmpty(productCollectionList.getContent())) {
        List<String> productCodesList =
            productCollectionList.getContent().stream().filter(Objects::nonNull).map(ProductCollection::getProductCode)
                .collect(Collectors.toList());
        return setProductsAsPublished(productCollectionList, productCodesList);
      }
    }
    return new ArrayList<>();
  }

  private List<String> setProductsAsPublished(Page<ProductCollection> productCollectionList,
      List<String> productCodesList) {
    productCollectionList.getContent().stream().filter(Objects::nonNull)
        .forEach(productCollection -> productCollection.setAddDeleteVariantStatus(AddDeleteVariantStatus.PUBLISHED));
    productCollectionRepository.saveAll(productCollectionList.getContent());
    return productCodesList;
  }


  private Date calculateTimestampBefore(int hours) {
    Calendar cal = Calendar.getInstance();
    cal.add(Calendar.HOUR_OF_DAY, -hours);
    return cal.getTime();
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  @Override
  public void updateAddDeleteVariantStatus(String productCode, AddDeleteVariantStatus addDeleteVariantStatus){
    productCollectionRepository.updateAddDeleteVariantStatus(productCode, addDeleteVariantStatus);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public ProductCollection updateAddDeleteVariantStatusForListener(String productCode,
      AddDeleteVariantStatus addDeleteVariantStatus) {
    ProductCollection productCollection = productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        mandatoryParameterHelper.getStoreId(), productCode);
    if(Objects.nonNull(productCollection)) {
      productCollection.setAddDeleteVariantStatus(addDeleteVariantStatus);
      productCollectionRepository.save(productCollection);
    }
    return productCollection;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void migrateProductAndL5DetailsByProductSku(String storeId,
    String productBusinessPartnerId, ProductAndL5MigrationRequest productAndL5MigrationRequest,
    String username, boolean b2bActivated, boolean b2cActivated) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId),
      ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productBusinessPartnerId),
      ErrorMessages.PRODUCT_BUSINESS_PARTNER_NOT_FOUND);
    int pageNumber = 0;
    Page<ProductItemBusinessPartner> productItemBusinessPartners;
    do {
      productItemBusinessPartners =
        productItemBusinessPartnerRepository.findByStoreIdAndProductBusinessPartnerIdAndMarkForDeleteFalseOrderByGdnProductItemSku(
          storeId, productBusinessPartnerId, PageRequest.of(pageNumber, l4FetchSizeForMigration));
      CommonUtils.migrateProductAndL5DetailsByProductSku(productItemBusinessPartners.getContent(),
        productAndL5MigrationRequest, b2cActivated, b2bActivated, username);
      productItemBusinessPartnerRepository.saveAll(productItemBusinessPartners.getContent());
      pageNumber++;
    } while (productItemBusinessPartners.hasNext());
  }


  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void takeActionsOnCategoryChangeFromVendor(String storeId, String existingCategoryCode,
    String existingCategoryName, String productCode, boolean eligibleForShippingMigration,
    boolean marginExceeded) throws Exception {
    ProductCollection productCollection =
      productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    List<String> productSkus = productBusinessPartnerRepository.getGdnSkuByProductCode(productCode);
    if (marginExceeded || eligibleForShippingMigration) {
      migrateActiveAndInActiveProducts(storeId, eligibleForShippingMigration, productSkus);
    }
    if (marginExceeded) {
      // send mail for category change
      CategoryChangeMailEvent categoryChangeMailEvent =
        CategoryChangeMailEvent.builder().existingCategoryCode(existingCategoryCode)
          .existingCategoryName(existingCategoryName).build();
      productMailEventService.createAndSaveCategoryChangeMailEvent(productCollection,
        categoryChangeMailEvent);
    }
  }

  private void migrateActiveAndInActiveProducts(String storeId,
    boolean eligibleForShippingMigration, List<String> productSkus) throws Exception {
    for (String productSku : productSkus) {
      ProductAndL5MigrationRequest productAndL5MigrationRequest =
        new ProductAndL5MigrationRequest();
      productAndL5MigrationRequest.setProductSku(productSku);
      productAndL5MigrationRequest.setBuyable(false);
      productAndL5MigrationRequest.setDiscoverable(false);
      productAndL5MigrationRequest.setL5Updated(true);
      productAndL5MigrationRequest.setDimensionsMissing(true);
      productAndL5MigrationRequest.setProductType(
        eligibleForShippingMigration ? com.gdn.mta.product.enums.ProductType.REGULAR : null);
      if(Objects.nonNull(productAndL5MigrationRequest.getProductType())){
      migrateProductAndL5Details(storeId, productAndL5MigrationRequest, SYSTEM_REVIEW);
      }
    }
  }

  @Override
  public BasicSizeChartDetailMapResponse getSizeChartBasicDetailBySizeChartCode(List<String> sizeChartCodes) {
    checkArgument(CollectionUtils.isNotEmpty(sizeChartCodes), ErrorMessages.SIZE_CHART_CODE_MUST_NOT_BE_BLANK);
    GdnRestSingleResponse<BasicSizeChartDetailMapResponse> sizeChartBasicDetails =
        pcbFeign.getSizeChartBasicDetailBySizeChartCode(mandatoryParameterHelper.getStoreId(),
            mandatoryParameterHelper.getChannelId(), mandatoryParameterHelper.getClientId(),
            mandatoryParameterHelper.getRequestId(), mandatoryParameterHelper.getUsername(), sizeChartCodes);
    if (sizeChartBasicDetails.isSuccess() && Objects.nonNull(sizeChartBasicDetails.getValue())) {
      return sizeChartBasicDetails.getValue();
    }
    return new BasicSizeChartDetailMapResponse();
  }

  @Override
  public void migrateProductAndL5Details(String storeId,
    ProductAndL5MigrationRequest productAndL5MigrationRequest, String username) throws Exception {
    ProductBusinessPartner productBusinessPartner =
      productBusinessPartnerService.findFirstByProductSku(
        productAndL5MigrationRequest.getProductSku());
    if (ProductLevel1State.ACTIVE.equals(productBusinessPartner.getState())) {
      com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest request =
        CommonUtils.fetchRequestForActiveProductMigration(productAndL5MigrationRequest);
      xProductOutbound.migrateProductAndL5DetailByProductSku(request, storeId, username);
    } else if (Set.of(Constants.IN_PROGRESS_STATE, Constants.NEED_CORRECTION)
      .contains(productBusinessPartner.getState())) {
      migrateProductAndL5DetailsByProductSku(storeId, productBusinessPartner.getId(),
        productAndL5MigrationRequest, username, productBusinessPartner.isB2bActivated(),
        productBusinessPartner.isB2cActivated());
    }
  }

  @Override
  public Page<String> getDistinctBusinessPartnerCodesForNeedRevisionProduct(String storeId,
      NeedRevisionProductsRequest needRevisionProductsRequest, Pageable pageable) {
    checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(Objects.nonNull(needRevisionProductsRequest), ErrorMessages.INVALID_REQUEST);
    checkArgument(StringUtils.isNotEmpty(needRevisionProductsRequest.getState()),
        ErrorMessages.STATE_MUST_NOT_BE_EMPTY);
    checkArgument(ObjectUtils.isNotEmpty(needRevisionProductsRequest.getStartUpdatedDate()),
        ErrorMessages.DATE_MUST_NOT_BE_NULL);
    checkArgument(ObjectUtils.isNotEmpty(needRevisionProductsRequest.getEndUpdatedDate()),
        ErrorMessages.DATE_MUST_NOT_BE_NULL);
      return productBusinessPartnerRepository.findDistinctBusinessPartnerCodesByStateAndUpdatedStepDate(storeId,
          needRevisionProductsRequest.getState(), needRevisionProductsRequest.getStartUpdatedDate(),
          needRevisionProductsRequest.getEndUpdatedDate(), pageable);
  }

  @Override
  public Page<ProductCodeAndNameDetails> fetchProductsByBusinessPartnerCodeForLastXDays(String storeId,
      String businessPartnerCode, Pageable pageable) {
    checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotEmpty(businessPartnerCode), ErrorMessages.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_YEAR, -fetchNRProductsFromLastXDays);
    Date updatedDate = calendar.getTime();
    return productCollectionRepository.findNeedRevisionProductsDetailsByBusinessPartnerCodeUpdatedDate(
        businessPartnerCode, updatedDate, pageable);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void populateProductBusinessPartnerAttribute(String productCode, String attributeCode, String attributeId,
      String attributeValue, boolean skuValue, String attributeName) {
    List<ProductBusinessPartner> productBusinessPartnerList =
        productBusinessPartnerService.findByProductCode(productCode);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productBusinessPartnerList),
        ErrorMessages.PRODUCT_NOT_FOUND);
    for (ProductBusinessPartner productBusinessPartner : productBusinessPartnerList) {
      BasicProductResponse validateProductActivated =
          xProductOutbound.getBasicProductInfoV2(productBusinessPartner.getGdnProductSku());
      if (Objects.nonNull(validateProductActivated)) {
        XProdAttributeMigrationEventModel xProdAttributeMigrationModel =
          RequestHelper.getXProdAttributeMigrationModel(productCode, attributeCode, attributeName,
            attributeValue, skuValue, productBusinessPartner.getGdnProductSku());
        kafkaProducer.send(kafkaTopicProperties.getXProductAttributeMigrationEvent(),
          xProdAttributeMigrationModel);
      } else {
        setProductBusinessPartnerAttributeAndSaveBusinessPartner(attributeId, attributeValue,
          productBusinessPartner);
      }
    }
  }

  private void setProductBusinessPartnerAttributeAndSaveBusinessPartner(String attributeId, String attributeValue,
      ProductBusinessPartner productBusinessPartner) {
    Optional<ProductBusinessPartnerAttribute> existingAttribute =
        productBusinessPartner.getProductBusinessPartnerAttributes().stream()
            .filter(attr -> attr.getAttributeId().equals(attributeId)).findFirst();
    if (existingAttribute.isEmpty()) {
      RequestHelper.setProductBusinessPartnerAttribute(mandatoryParameterHelper.getStoreId(), attributeId,
          attributeValue, productBusinessPartner);
      productBusinessPartnerRepository.save(productBusinessPartner);
    }
  }

  @Override
  public AutoApprovalType verifyAutoApprovalRules(AutoApprovalsDetailDto autoApprovalsDetailDto) throws Exception {
    return autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetailDto);
  }

  @Override
  @Transactional(readOnly = false)
  public ApiErrorCode performMasterDataUpdate(
    ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO, String storeId, String requestId, String username)
    throws Exception {
    // perform dimensions recomputation at L3 and generate shipping weight
    productLevel3Service.updateDimensionsAtL3(productMasterDataEditRequest,
      productMasterDataEditRequest.getMasterDataEditChangeTypes()
        .contains(L3InfoUpdateChangeType.DIMENSIONS_UPDATE));
    if(eligibleForPCBUpdate(productMasterDataEditRequest)) {
      setCommonImageUpdateRequest(productMasterDataEditRequest, masterProductEditDTO);
      productOutbound.updateProductMasterDataInPCB(storeId, requestId, username,
        productMasterDataEditRequest, masterProductEditDTO);
    }
    saveInternalHistoryOnAutoApproval(productMasterDataEditRequest, masterProductEditDTO);
    updateProductBusinessPartner(productMasterDataEditRequest, masterProductEditDTO);
    masterProductEditDTO.setPublishImageQcForContentChange(masterProductEditDTO.isContentChanged());
    masterProductEditDTO.setPublishImageQcForContentChange(!masterProductEditDTO.isTakenDownProduct());
    //Update ProductCollection And Solr And Publish History Event
    productLevel3Service.updateProductCollectionAndSolrAndPublishHistoryEvent(
      productMasterDataEditRequest.getProductName(), masterProductEditDTO.getProductCollection(),
      masterProductEditDTO.getReviewTypeList(), masterProductEditDTO, StringUtils.EMPTY);
    // Update Product Master Data Info in x-product and call generate product score to sync master data
    if (!masterProductEditDTO.isTakenDownProduct()) {
      ProductBasicMasterFieldsRequest productBasicMasterFieldsRequest =
        getProductBasicMasterFieldsRequest(productMasterDataEditRequest);
      xProductOutbound.updateProductMasterDataInfo(productMasterDataEditRequest.getUpdatedBy(),
        productBasicMasterFieldsRequest);
    } else {
      return null;
    }
    // Generate modified fields for master data edit
    publishEditedEventToVendorAndPublishDimensionsRefreshEvent(productMasterDataEditRequest, masterProductEditDTO, storeId);
    return null;
  }

  @Override
  public void publishEditedEventToVendorAndPublishDimensionsRefreshEvent(
    ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO, String storeId) throws Exception {
    CommonUtils.generateModifiedFieldsForMasterDataEdit(
      productMasterDataEditRequest.getMasterDataEditChangeTypes(), masterProductEditDTO);
    // Publish add edited event to vendor
    publishAddEditedEventToVendor(productMasterDataEditRequest, masterProductEditDTO, storeId);
    // Publish dimensions refresh event
    publishDimensionsRefreshEvent(productMasterDataEditRequest, masterProductEditDTO, storeId);
  }

  private static ProductBasicMasterFieldsRequest getProductBasicMasterFieldsRequest(
    ProductMasterDataEditRequest productMasterDataEditRequest) {
    ProductBasicMasterFieldsRequest productBasicMasterFieldsRequest =
      RequestHelper.getProductMasterDataEditRequest(productMasterDataEditRequest);
    Boolean lateFulfillmentOnShippingTypeChange = CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(
      productMasterDataEditRequest.getProductType(),
      productMasterDataEditRequest.getMasterDataEditChangeTypes()
        .contains(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE), null);
    productBasicMasterFieldsRequest.setLateFulfillment(lateFulfillmentOnShippingTypeChange);
    return productBasicMasterFieldsRequest;
  }


  private void publishDimensionsRefreshEvent(ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO, String storeId) {
    if(masterProductEditDTO.getProductCollection().isReviewPending()) {
      DimensionRefreshRequest dimensionRefreshRequest =
        ConverterUtil.toDimensionRefreshRequest(productMasterDataEditRequest.getLength(),
          productMasterDataEditRequest.getWidth(), productMasterDataEditRequest.getHeight(),
          productMasterDataEditRequest.getWeight(),
          productMasterDataEditRequest.getShippingWeight(),
          productMasterDataEditRequest.getDangerousGoodsLevel(),
          productMasterDataEditRequest.getProductType());
      productPublisherService.publishProductDimensionRefreshEvent(
        ConverterUtil.toPDTDimensionRefreshEventModel(storeId,
          productMasterDataEditRequest.getProductCode(), dimensionRefreshRequest));
    }
  }

  private void publishAddEditedEventToVendor(ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO, String storeId) throws Exception {
    if (StringUtils.isNotEmpty(masterProductEditDTO.getContentType())) {
      if (ResponseHelper.isProductEligibleForVendorPublish(masterProductEditDTO.getAction(), false,
        productMasterDataEditRequest.isTrustedSeller())) {
        List<String> allModifiedFields = List.copyOf(masterProductEditDTO.getModifiedFields());
        publishAddEditedProductToPDTEvent(storeId, masterProductEditDTO.getContentType(),
          masterProductEditDTO.getProductCollection(), allModifiedFields);
      }
    }
  }

  private void updateProductBusinessPartner(ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO) {
    if (POST_LIVE.equals(
      masterProductEditDTO.getConfigurationStatusResponseList().getFirst().getReviewConfig())) {
      productBusinessPartnerService.updateProductMasterData(
        productMasterDataEditRequest.getProductSku(), productMasterDataEditRequest.getProductName(),
        productMasterDataEditRequest.getCategoryCode(), productMasterDataEditRequest.getCategoryName(),
        productMasterDataEditRequest.getSizeChartCode(), productMasterDataEditRequest.getUpdatedBy(),
        productMasterDataEditRequest.getMasterDataEditChangeTypes().contains(L3InfoUpdateChangeType.SIZE_CHART_UPDATE),
          StringUtils.EMPTY);
    } else {
      productBusinessPartnerService.updateSizeChartDetailsAndBrandDetails(
        productMasterDataEditRequest.getProductSku(), productMasterDataEditRequest.getSizeChartCode(),
        productMasterDataEditRequest.getUpdatedBy(), productMasterDataEditRequest.getMasterDataEditChangeTypes()
          .contains(L3InfoUpdateChangeType.SIZE_CHART_UPDATE), false, StringUtils.EMPTY);
    }
  }

  private void setCommonImageUpdateRequest(ProductMasterDataEditRequest productMasterDataEditRequest, MasterProductEditDTO masterProductEditDTO) {
    if (productMasterDataEditRequest.getMasterDataEditChangeTypes()
      .contains(L3InfoUpdateChangeType.COMMON_IMAGE_UPDATE)) {
      List<ProductLevel3SummaryDetailsImageRequest> copyAllVariantImageRequest =
        productMasterDataEditRequest.getProductLevel3SummaryDetailsImageRequests();
      if (CollectionUtils.isNotEmpty(copyAllVariantImageRequest)) {
        List<ProductImageEditRequest> productImageEditRequestList =
          ConverterUtil.toProductImageEditRequestList(copyAllVariantImageRequest, false,
            CommonUtils.isProductActivatedBefore(masterProductEditDTO.getProductCollection()),
            productMasterDataEditRequest.getProductCode());
        masterProductEditDTO.setProductImageEditRequestList(productImageEditRequestList);
      }
    }
  }

  private void saveInternalHistoryOnAutoApproval(ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO) {
    if(masterProductEditDTO.isSaveInternalHistory()){
      productLevel3Service.publishInternalProductHistoryEvent(
        masterProductEditDTO.getProductCollection(), productMasterDataEditRequest.getUpdatedBy());
    }
  }

  private boolean eligibleForPCBUpdate(ProductMasterDataEditRequest productMasterDataEditRequest) {
    Set<L3InfoUpdateChangeType> eligibleChangeTypesForPCBUpdate =
      Set.of(L3InfoUpdateChangeType.SIZE_CHART_UPDATE, L3InfoUpdateChangeType.DESCRIPTION_UPDATE,
        L3InfoUpdateChangeType.DIMENSIONS_UPDATE, L3InfoUpdateChangeType.COMMON_IMAGE_UPDATE,
        L3InfoUpdateChangeType.YOUTUBE_URL_UPDATE, L3InfoUpdateChangeType.VIDEO_UPDATE,
        L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE);
    return productMasterDataEditRequest.getMasterDataEditChangeTypes().stream()
      .anyMatch(eligibleChangeTypesForPCBUpdate::contains);
  }

  @Override
  public Page<ProductAndBrandResponse> getProductsByBrandName(String storeId, String brandName,
      Pageable pageable) throws Exception {
    Page<ProductCodeResponse> pcbResponse =
        productRepository.getProductsByBrandName(brandName, pageable);
    List<ProductAndBrandResponse> response = new ArrayList<>();
    for (ProductCodeResponse productCodeResponse : pcbResponse.getContent()) {
      List<ProductBusinessPartner> productBusinessPartners =
          productBusinessPartnerService.findByStoreIdAndProductId(storeId,
              productCodeResponse.getId());
      Set<String> businessPartnerCodes =
          Optional.ofNullable(productBusinessPartners).orElse(new ArrayList<>()).stream()
              .map(ProductBusinessPartner::getBusinessPartnerId)
              .collect(Collectors.toSet());
      response.add(ProductAndBrandResponse.builder().productId(productCodeResponse.getId())
          .productCode(productCodeResponse.getProductCode()).brandName(brandName)
          .businessPartnerCodes(businessPartnerCodes).build());
    }
    return new PageImpl<>(response, pageable, pcbResponse.getTotalElements());
  }

  @Override
  @Transactional
  public Pair<ProductCollection, String> updateBrandData(String storeId,
      ProductBrandUpdateRequest productBrandUpdateRequest,
      List<ProductBusinessPartner> productBusinessPartners) {
    ProductCollection savedProduct =
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId,
            productBrandUpdateRequest.getProductCode());
    GdnPreconditions.checkArgument(Objects.nonNull(savedProduct), ErrorMessages.PRODUCT_NOT_FOUND);
    String oldBrand = savedProduct.getBrand();
    if (!productBrandUpdateRequest.isOnlyBrandNameUpdate()) {
      savedProduct.setBrandCode(productBrandUpdateRequest.getNewBrandCode());
      savedProduct.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    }
    productBusinessPartnerService.updateBrand(productBusinessPartners,
        productBrandUpdateRequest.getNewBrandName());
    savedProduct.setBrand(productBrandUpdateRequest.getNewBrandName());
    ProductCollection updatedProduct = productCollectionRepository.save(savedProduct);
    return Pair.of(updatedProduct, oldBrand);
  }
}
