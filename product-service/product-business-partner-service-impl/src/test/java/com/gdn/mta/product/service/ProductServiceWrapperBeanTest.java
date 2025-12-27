package com.gdn.mta.product.service;

import com.gda.mta.product.dto.ProductBrandUpdateRequest;
import com.gda.mta.product.dto.response.AgpSimpleQueryResponse;
import com.gda.mta.product.dto.response.HitsResponse;
import com.gdn.mta.domain.event.modal.VendorPublishEventModel;

import static com.gdn.mta.domain.event.config.DomainEventName.PRODUCT_INTERNAL_HISTORY_SAVE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.gda.mta.product.dto.response.UpdatedProductHistoryRequest;
import com.gdn.mta.product.entity.ProductBusinessPartnerCounter;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.ProductType;
import com.gdn.mta.product.service.solr.SolrActiveProductCollectionService;
import com.gdn.mta.product.util.BeanUtils;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutbound;

import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.slf4j.MDC;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.AutoNeedRevisionAndForceReviewResponse;
import com.gda.mta.product.dto.BulkMasterProductUpdateResponse;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.EditedResizeAndImagesUpdateStatusResponse;
import com.gda.mta.product.dto.ItemNeedRevisionNotes;
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
import com.gda.mta.product.dto.response.ImageQcResponse;
import com.gda.mta.product.dto.response.InternalProductHistoryEventModel;
import com.gda.mta.product.dto.response.RetryAutoNeedRevisionResponse;
import com.gda.mta.product.dto.response.VendorNotesResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AutoNeedRevisionDomainEvent;
import com.gdn.mta.domain.event.modal.EditedImageResizeEvent;
import com.gdn.mta.domain.event.modal.ImageQcProcessedResponseDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcRequestDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcResponseDomainEvent;
import com.gdn.mta.domain.event.modal.ImageResizeEvent;
import com.gdn.mta.domain.event.modal.ProductActionRetryEvent;
import com.gdn.mta.domain.event.modal.ProductQCRetryEvent;
import com.gdn.mta.domain.event.modal.ScreeningProductApprovalEvent;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionDeleteEvent;
import com.gdn.mta.domain.event.modal.StuckProductEventPublishDto;
import com.gdn.mta.domain.event.modal.TerminatedSellerSkuCleanupStatusEventModel;
import com.gdn.mta.product.commons.constant.TerminatedSellerSkuStatus;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductImagePrediction;
import com.gdn.mta.product.entity.ProductImageQcBacklog;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.mta.product.enums.BulkActionType;
import com.gdn.mta.product.enums.ImageQcStatus;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductDistributionTaskRepositoryBean;
import com.gdn.mta.product.repository.ProductImageQcProcessingResponseRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.SolrHistoryCollectionRepository;
import com.gdn.mta.product.service.config.GcsProperties;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.util.ProductWorkflowLookup;
import com.gdn.mta.product.valueobject.BulkMasterProductUpdateRequestDTO;
import com.gdn.mta.product.valueobject.SimpleMasterProductUpdateRequestDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.EditedReviewTypeConstants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEventsEnum;
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
import com.gdn.partners.product.pricing.web.model.dto.WholeSalePriceSkuStatusDto;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceRequest;
import com.gdn.partners.product.pricing.web.model.response.BulkActivateDeactivateResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceBulkUpdateResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.dto.BusinessPartnerCodesRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.rest.web.model.L3VersionResponse;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.google.common.collect.ImmutableMap;

public class ProductServiceWrapperBeanTest {

  private ProductImageQcProcessingResponse productImageQcProcessingResponse;

  @Mock
  private ProductService productService;

  @Mock
  private GcsProperties gcsProperties;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Mock
  private ProductDistributionTaskRepositoryBean productDistributionTaskRepositoryBean;

  @Mock
  private ProductImageQcProcessingResponseRepository productImageQcProcessingResponseRepository;

  @Mock
  private ProductWfService productWfService;

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Mock
  private ProductMailEventService productMailEventService;

  @InjectMocks
  private ProductServiceWrapperBean productServiceWrapper;

  @Mock
  private ProductImageQcProcessingResponseService productImageQcProcessingResponseService;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductPublisherService productPublisherService;

  @Mock
  private ImageProcessorService imageProcessorService;

  @Mock
  private ProductNotificationService productNotificationService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private ProductSystemParameterService productSystemParameterService;

  @Mock
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private ItemService itemService;

  @Mock
  private ProductLevel1WipService productLevel1WipService;

  @Mock
  private ApproveProductService approveProductService;

  @Mock
  private SolrHistoryCollectionRepository solrHistoryCollectionRepository;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductItemWholesalePriceService productItemWholesalePriceService;

  @Mock
  private ProductPricingOutbound productPricingOutbound;

  @Mock
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Mock
  private ProductLevel3Helper productLevel3Helper;

  @Mock
  private ProductImagePredictionService productImagePredictionService;

  @Mock
  private NeedCorrectionServiceBean needCorrectionService;

  @Mock
  private ProductImageQcBacklogService productImageQcBacklogService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ProductWorkflowServiceWrapper productWorkflowServiceWrapper;

  @Mock
  private ProductDistributionService productDistributionService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private ProductLevel3Service productLevel3Service;

  @Mock
  private ProductStatusPublisherService productStatusPublisherService;

  @Mock
  private com.gdn.mta.product.service.ProductLevel3Service productLevel3ServiceV1;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private TerminatedSellerSkuCleanupServiceBean terminatedSellerSkuCleanupServiceBean;

  @Mock
  private SolrActiveProductCollectionService solrActiveProductCollectionService;

  @Mock
  private ProductAppealService productAppealService;

  @Mock
  private InventoryOutbound inventoryOutbound;

  @Captor
  private ArgumentCaptor<ProductAndItemImageRequest> productAndItemImageRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductDetailResponse> productDetailResponseArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductCollection> productCollectionArgumentCaptor;

  @Captor
  private ArgumentCaptor<SimpleMasterProductUpdateRequestDTO> simpleMasterProductUpdateRequestDTOArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductCollection> productCollectionArgumentCaptorNew;

  @Captor
  private ArgumentCaptor<Boolean> booleanArgumentCaptor;

  @Captor
  private ArgumentCaptor<Map<String, Long>> stringIntegerMapArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductImageQcFeedbackResponse> productImageQcFeedbackResponseArgumentCaptor;

  @Captor
  private ArgumentCaptor<PDTProductDomainEventModel> pdtProductDomainEventModelArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductHistory> productHistoryArgumentCaptor;

  @Captor
  private ArgumentCaptor<AutoNeedRevisionRequest> autoNeedRevisionRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<AddRevisedProductToPDTEvent> addRevisedProductToPDTEventArgumentCaptor;

  @Captor
  private ArgumentCaptor<ImageQcRequestDomainEvent> imageQcRequestDomainEventArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductImageQcBacklog> productImageQcBacklogArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductRequest> productRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<RemoveProductRequest> removeProductRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<TerminatedSellerSkuCleanupStatusEventModel>
      terminatedSellerSkuCleanupStatusEventModelArgumentCaptor;

  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_CODE = "productCode";
  private static final String SELLER_CODE = "sellerCode";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_NAME = "productName";
  private static final String PRODUCT_NAME_2 = "productName2";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String ASSIGNED_BY = "assignedBy";
  private static final String REASON = "reason";
  private static final String REASON_1 = "reason1";
  private static final String ID = "id";
  private static final String BRAND = "brand";
  private static final String USP = "usp";
  private static final String DESCRIPTION = "description";
  private static final String CATEGORY_CODE = "CAT-0000001";
  private static final String CATEGORY_NAME = "catName";
  private static final String CATEGORY_CODE_NEW = "CAT-0000001 new";
  private static final String CATEGORY_NAME_NEW = "catName new";
  private static final Double LENGTH = 1.0;
  private static final Double WIDTH = 1.0;
  private static final Double HEIGHT = 1.0;
  private static final Double WEIGHT = 1.0;
  private static final Integer DANGEROUS_GOODS_LEVEL = 1;
  private static final String PRODUCT_CODE_1 = "productCode1";
  private static final String PRODUCT_ID_1 = "productId1";
  private static final String ITEM_CODE_1 = "itemCode1";
  private static final String ITEM_CODE_2 = "itemCode2";
  private static final String CORRECTION_REASON = "correctionReason";
  private static final String CORRECTION_REASON_1 = "correctionReason1";
  private static final String REJECTION_REASON = "rejectionReason";
  private static final String ADDITIONAL_NOTES = "additionalNotes";
  private static final String ADDITIONAL_NOTES_1 = "additionalNotes1";
  private static final String CREATED_BY = "createdBy";
  private static final String NOTES = "notes";
  private static final String DRAFT_STATE = "DRAFT";
  private static final Date CREATED_DATE = new Date();
  private static final String USER_NAME = "userName";
  private static final String BUSINESS_PARTNER_NAME = "bpn";
  private static final String BUSINESS_PARTNER_CODE = "bpc";
  private static final String INTERNAL = "INTERNAL";
  private static final String VALUE = "value";
  private static final String ATTRIBUTE_ID = "attributeId";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_APPROVAL_STATUS = "APPROVED";
  private static final String BRAND_NAME = "brandName";
  private static final String IMAGE_LOCATION_PATH_1 = "path1";
  private static final String IMAGE_LOCATION_PATH_2 = "path2";
  private static final String IMAGE_LOCATION_PATH_3 = "path3";
  private static final String IMAGE_LOCATION_PATH_4 = "path4";
  private static final String IMAGE_LOCATION_PATH_5 = "path5";
  private static final String IMAGE_LOCATION_PATH_6 ="/filestore/path6";
  private static final String IMAGE_HASH_CODE_1 = "hashCode1";
  private static final String IMAGE_HASH_CODE_2 = "hashCode2";
  private static final String IMAGE_HASH_CODE_3 = "hashCode3";
  private static final String IMAGE_HASH_CODE_4 = "hashCode4";
  private static final String IMAGE_HASH_CODE_5 = "hashCode5";

  private static final String HASH_CODE = "HASH_CODE";
  private static final String RESIZED_IMAGE_HASH_CODE_1 = "d5470db95eafb48ab9921fa12a28fac9";
  private static final String RESIZED_IMAGE_HASH_CODE_3 = "fbed65517a520997aa14714e8d0742e2";
  private static final String WARNA = "Warna";
  private static final String KEYWORD = "keyword";
  private static final String FAMILY_COLOUR = "Family Colour";
  private static final String REVIEW_CONFIG_CHANGE_DESCRIPTION =
      "Auto screening approval failed, so product is made pre-live";
  private static final String STATE_IN_PROGRESS = "IN_PROGRESS";
  private static final int PAGE = 0;
  private static final int SIZE = 100;
  private static final String NAME_WITH_PHONE_NUMBER = "0873676NAME +628.173-4567--890 NAME-6289672";
  private static final String HISTORY_NOTES = "{field: 'Category', oldValue: catName, newValue: catName new}";
  private static final String PREDICTION_NAME_1 = "watermark";
  private static final String PREDICTION_DISPLAY_NAME_1 = "water mark present";
  private static final String PREDICTION_DISPLAY_NAME_1_IN = "water mark In";
  private static final String PREDICTION_NAME_2 = "nsfw";
  private static final String PREDICTION_DISPLAY_NAME_2 = "nsfw present";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String ITEM_SKU = "itemSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String REVIEW_TYPE_IMAGE = "IMAGE";
  private static final String DEFAULT_ACCESS_CHANNEL = "Access channel";
  private static final String DEFAULT_ACCESS_KEY = "PICK_POINT_CODE";
  private static final String DEFAULT_OLD_VALUE = "old value";
  private static final String DEFAULT_NEW_VALUE = "new value";
  private static final List<String> ERROR_FIELDS = Arrays.asList("Description");
  private static final String VENDOR_NOTES = "vendor notes";
  private static final String SKU_CODE = "skuCode";
  private static final String SKU_CODE_1 = "skuCode1";
  private static final String ITEM_SKU_1 = "ABC-10001-10001-00001";
  private static final String ITEM_SKU_2 = "ABC-10001-10001-00002";
  private static final String IMAGE_SOURCE = "/image-source";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String PREDICTION_NAME_3 = "blur";
  private static final String PREDICTION_DISPLAY_NAME_3 = "blur present";
  private static final String PREDICTION_DISPLAY_NAME_3_IN = "blur";
  private static final String PATH_2 = "/image-sourcepath3";
  private static final String GCS_PATH_PREFIX = "path4";
  private static final String GCS_COMPLETE_SOURCE_URL = "complete-soucre";
  private static final String IMAGE_PREDICTION_RESPONSE_1 =
      "[{\"locationPath\":\"path1\",\"hashCode\":\"hashCode1\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"water mark present\",\"present\":false,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfw present\",\"present\":true,\"confidence\":80}]},{\"locationPath\":\"path2\",\"hashCode\":\"hashCode2\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"water mark present\",\"present\":false,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfw present\",\"present\":true,\"confidence\":80}]}]";
  private static final String DESTINATION_CATEGORY = "DESTINATION_CATEGORY";
  public static final String DEFAULT_STORE_ID = "10001";
  public static final String DEFAULT_USERNAME = "username";
  public static final int RETRY_BATCH_SIZE_COUNT = 1;
  public static final int RETRY_TIME_SPAN = 1;
  private ImageQcResponseDomainEvent imageQcResponseDomainEvent;
  private SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO;
  private BulkMasterProductUpdateRequestDTO bulkMasterProductUpdateRequestDTO;
  private ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest;
  private NeedRevisionNotes revisionNotesWebRequest;
  private List<String> productCodes;
  private ProductCollection productCollection;
  private ProductHistory productHistory1;
  private ProductHistory productHistory2;
  private ProductHistory productHistory3;
  private Product product;
  private ProductCollection productCollection1;
  private List<ProductAttribute> productAttributeList;
  private ProductAttribute productAttribute;
  private Attribute attribute;
  private ProductAttributeValue productAttributeValue;
  private Pageable pageable;
  private BulkImageProcessResponse bulkImageProcessResponse;
  private BulkImageProcessResponse editImageProcessResponse;
  private CategorySummaryResponse categorySummaryResponse;
  private ProfileResponse profileResponse;
  private CompanyDTO companyDTO;
  private ProductSystemParameter productSystemParameterImageQcSwitch;
  private ProductSystemParameter productSystemParameterCategoryResponse;
  private List<ImageQcResponse> imageQcResponseList;
  private ImageQcResponse imageQcResponse;
  private ImageQcPredictionResponse imageQcPredictionResponse;
  private Map<String, Long> imageCountMap;
  private AuditTrailDto auditTrailRequest;
  private ItemSummaryRequest itemSummaryRequest;
  private VendorNotesResponse vendorNotesResponseData;
  private ItemNotesDto itemNotesDto;
  private ItemNotesDto itemNotesDto1;
  private Map<String, String> skuCodesAndItemSkuMap;
  private List<ProductItemWholesalePrice> productItemWholesalePrices;
  private AutoNeedRevisionDomainEvent autoNeedRevisionDomainEvent;
  private ProductImagePrediction productImagePrediction;
  private ProductImagePrediction productImagePredictionEn;
  private ProductImagePrediction productImagePredictionBlur;
  private ProductImagePrediction productImagePredictionBlurEn;
  private RetryNeedRevisionRequest retryNeedRevisionRequest;
  private NeedRevisionReasonRequest needRevisionReasonRequest;
  private AutoNeedRevisionRequest autoNeedRevisionRequest;
  private ProductImageQcBacklog productImageQcBacklog;
  private ObjectMapper mapper = new ObjectMapper();
  private CategoryRestrictedKeywordResponse categoryRestrictedKeywordResponse = new CategoryRestrictedKeywordResponse();
  RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
      new RestrictedKeywordsByFieldAndActionType();
  private AutoApprovalTypeRequest autoApprovalTypeRequest;
  private AutoApprovalType autoApprovalType;
  private ProductBusinessPartner productBusinessPartner;
  private ProductBusinessPartnerCounter productBusinessPartnerCounter;
  private UpdatedProductHistory updatedProductHistory;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    simpleMasterProductUpdateRequestDTO = new SimpleMasterProductUpdateRequestDTO();
    simpleMasterProductUpdateRequestDTO.setProductCode(PRODUCT_CODE);
    simpleMasterProductUpdateRequestDTO.setName(PRODUCT_NAME);
    simpleMasterProductUpdateRequestDTO.setBrand(BRAND);
    simpleMasterProductUpdateRequestDTO.setLength(LENGTH);
    simpleMasterProductUpdateRequestDTO.setWidth(WIDTH);
    simpleMasterProductUpdateRequestDTO.setHeight(HEIGHT);
    simpleMasterProductUpdateRequestDTO.setWeight(WEIGHT);
    simpleMasterProductUpdateRequestDTO.setDangerousGoodsLevel(DANGEROUS_GOODS_LEVEL);
    bulkMasterProductUpdateRequestDTO =
        new BulkMasterProductUpdateRequestDTO(Collections.singletonList(simpleMasterProductUpdateRequestDTO));
    screeningProductBulkActionsRequest = new ScreeningProductBulkActionsRequest();
    revisionNotesWebRequest = new NeedRevisionNotes();
    productCodes = Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1);
    screeningProductBulkActionsRequest.setProductCodes(productCodes);
    screeningProductBulkActionsRequest.setAllVariants(true);
    screeningProductBulkActionsRequest.setVendorErrorFields(ERROR_FIELDS);
    screeningProductBulkActionsRequest.setVendorNotes(Arrays.asList(CORRECTION_REASON));
    screeningProductBulkActionsRequest.setImageReason(Arrays.asList(CORRECTION_REASON_1));
    screeningProductBulkActionsRequest.setCommonImageReason(Arrays.asList(CORRECTION_REASON_1));
    screeningProductBulkActionsRequest.setContentAdditionalNotes(ADDITIONAL_NOTES);
    screeningProductBulkActionsRequest.setImagesAdditionalNotes(ADDITIONAL_NOTES_1);
    screeningProductBulkActionsRequest.setVendorNotes(productCodes);
    screeningProductBulkActionsRequest.setContentAdditionalNotes(NOTES);
    screeningProductBulkActionsRequest.setAllVariants(true);
    screeningProductBulkActionsRequest.setItemNotes(
        Arrays.asList(ItemNeedRevisionNotes.builder().itemName(ITEM_SKU).vendorErrorFields(productCodes).build()));
    BeanUtils.copyProperties(screeningProductBulkActionsRequest, revisionNotesWebRequest);
    productCollection = new ProductCollection();
    productCollection.setId(ID);
    productCollection.setProductId(PRODUCT_ID);
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setProductName(PRODUCT_NAME);
    productCollection.setCategoryName(CATEGORY_NAME);
    productCollection.setCategoryCode(CATEGORY_CODE);
    productCollection.setNeedCorrectionNotes(VENDOR_NOTES);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productHistory1 = new ProductHistory();
    productHistory1.setDescription(WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc());
    productHistory1.setProductId(PRODUCT_ID);
    productHistory1.setCreatedBy(CREATED_BY);
    productHistory1.setCreatedDate(CREATED_DATE);
    productHistory1.setNotes(REASON + "-" + ADDITIONAL_NOTES);
    productHistory2 = new ProductHistory();
    productHistory2.setDescription(WorkflowProcessCode.ACTIVATE.getDesc());
    productHistory2.setProductId(PRODUCT_ID);
    productHistory3 = new ProductHistory();
    productHistory3.setDescription(WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc());
    productHistory3.setProductId(PRODUCT_ID);
    productHistory3.setCreatedBy(CREATED_BY);
    productHistory3.setCreatedDate(CREATED_DATE);
    productHistory3.setNotes(REASON_1 + "-" + ADDITIONAL_NOTES_1);
    pageable = PageRequest.of(0, 100);

    this.product = new Product.Builder().productCode(PRODUCT_CODE).name(PRODUCT_NAME).length(1.0).width(1.0).height(1.0)
        .weight(1.0).shippingWeight(1.0).description(DESCRIPTION.getBytes())
        .brand(BRAND).uniqueSellingPoint(USP).uom(null).storeId(null).promoSKU(false)
        .productCategories(new ArrayList<>()).productAttributes(new ArrayList<>()).productItems(new ArrayList<>())
        .build();
    productCollection1 =
        new ProductCollection(PRODUCT_ID, PRODUCT_CODE, PRODUCT_NAME, BRAND, CATEGORY_CODE, CATEGORY_NAME,
            BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, false, false, DRAFT_STATE, USER_NAME,
            Calendar.getInstance().getTime(), STORE_ID);

    productAttributeList = new ArrayList<>();
    productAttribute = new ProductAttribute();
    productAttributeValue = new ProductAttributeValue();
    attribute = new Attribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setId(ATTRIBUTE_ID);
    productAttribute.setAttribute(attribute);
    productAttributeValue.setDescriptiveAttributeValue(VALUE);
    productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttribute.setProductAttributeValues(Collections.singletonList(productAttributeValue));
    productAttribute.setAttribute(attribute);
    productAttributeList.add(productAttribute);
    bulkImageProcessResponse = new BulkImageProcessResponse();
    bulkImageProcessResponse.setGroupCode(PRODUCT_CODE);
    bulkImageProcessResponse.setStoreId(STORE_ID);
    ImageResponse imageResponse = new ImageResponse();
    imageResponse.setImagePathLocation(IMAGE_LOCATION_PATH_1);
    imageResponse.setHashCode(IMAGE_HASH_CODE_1);
    imageResponse.setSuccess(true);
    ImageResponse imageResponse1 = new ImageResponse();
    imageResponse1.setImagePathLocation(IMAGE_LOCATION_PATH_2);
    imageResponse1.setHashCode(IMAGE_HASH_CODE_2);
    imageResponse1.setSuccess(true);
    ImageResponse imageResponse2 = new ImageResponse();
    imageResponse2.setImagePathLocation(IMAGE_LOCATION_PATH_3);
    imageResponse2.setHashCode(IMAGE_HASH_CODE_3);
    imageResponse2.setSuccess(true);
    ImageResponse imageResponse3 = new ImageResponse();
    imageResponse3.setImagePathLocation(IMAGE_LOCATION_PATH_4);
    imageResponse3.setHashCode(IMAGE_HASH_CODE_4);
    imageResponse3.setSuccess(true);
    bulkImageProcessResponse
        .setImageResponses(Arrays.asList(imageResponse, imageResponse1, imageResponse2, imageResponse3));
    editImageProcessResponse = new BulkImageProcessResponse();
    editImageProcessResponse.setGroupCode(PRODUCT_CODE);
    editImageProcessResponse.setStoreId(STORE_ID);
    editImageProcessResponse.setImageResponses(Arrays.asList(imageResponse, imageResponse2));
    categorySummaryResponse = new CategorySummaryResponse();
    categorySummaryResponse.setCategoryCode(CATEGORY_CODE_NEW);
    categorySummaryResponse.setCategoryName(CATEGORY_NAME_NEW);

    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USER_NAME);

    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(Boolean.TRUE);
    profileResponse.setCompany(companyDTO);

    productSystemParameterImageQcSwitch = new ProductSystemParameter();
    productSystemParameterCategoryResponse = new ProductSystemParameter();
    productSystemParameterImageQcSwitch.setValue(String.valueOf(true));
    productSystemParameterCategoryResponse.setValue(CATEGORY_CODE);

    imageQcResponseDomainEvent = new ImageQcResponseDomainEvent();
    imageQcResponseDomainEvent.setProductCode(PRODUCT_CODE);
    imageQcResponseDomainEvent.setImages(imageQcResponseList);

    imageQcResponseList = new ArrayList<>();
    imageQcResponse = new ImageQcResponse();
    imageQcResponse.setLocationPath(IMAGE_LOCATION_PATH_1);
    ImageQcResponse imageQcResponse2 = new ImageQcResponse();
    imageQcResponse2.setLocationPath(IMAGE_LOCATION_PATH_6);
    imageQcResponse2.setHashCode(HASH_CODE);
    imageQcResponseList.add(imageQcResponse);
    imageQcResponseList.add(imageQcResponse2);
    List<ImageQcPredictionResponse> imageQcPredictionResponseList = new ArrayList<>();
    imageQcPredictionResponse = new ImageQcPredictionResponse();
    imageQcPredictionResponse.setPredictionType(PREDICTION_NAME_1);
    imageQcPredictionResponse.setDisplayName(PREDICTION_DISPLAY_NAME_1);
    imageQcPredictionResponse.setPresent(true);
    imageQcPredictionResponse.setConfidence(40);
    ImageQcPredictionResponse imageQcPredictionResponse1 = new ImageQcPredictionResponse();
    imageQcPredictionResponse1.setPredictionType(PREDICTION_NAME_2);
    imageQcPredictionResponse1.setDisplayName(PREDICTION_DISPLAY_NAME_2);
    imageQcPredictionResponse1.setPresent(false);
    imageQcPredictionResponse1.setConfidence(80);
    imageQcPredictionResponseList.add(imageQcPredictionResponse);
    imageQcPredictionResponseList.add(imageQcPredictionResponse1);
    imageQcResponse.setPredictions(imageQcPredictionResponseList);
    imageQcResponse.setHashCode(IMAGE_HASH_CODE_1);
    imageQcResponse.setLocationPath(IMAGE_LOCATION_PATH_1);

    ImageQcResponse imageQcResponse1 = new ImageQcResponse();
    imageQcResponse1.setLocationPath(IMAGE_LOCATION_PATH_2);
    imageQcResponse1.setHashCode(IMAGE_HASH_CODE_2);
    imageQcResponse.setPredictions(imageQcPredictionResponseList);

    imageQcResponseDomainEvent.setImages(imageQcResponseList);

    categoryRestrictedKeywordResponse.setMessage(NOTES);

    imageCountMap = new HashMap<>();
    imageCountMap.put(IMAGE_HASH_CODE_1, 1L);
    imageCountMap.put(IMAGE_HASH_CODE_2, 1L);
    imageCountMap.put(IMAGE_HASH_CODE_3, 1L);

    auditTrailRequest = new AuditTrailDto();
    auditTrailRequest.setBusinessPartnerCode(MERCHANT_CODE);
    auditTrailRequest.setGdnSku(ITEM_SKU);
    auditTrailRequest.setActionKey(DEFAULT_ACCESS_KEY);
    auditTrailRequest.setOldValue(DEFAULT_OLD_VALUE);
    auditTrailRequest.setNewValue(DEFAULT_NEW_VALUE);
    auditTrailRequest.setAttributeName(ATTRIBUTE_ID);
    auditTrailRequest.setProductSku(PRODUCT_ID);
    auditTrailRequest.setName(BUSINESS_PARTNER_NAME);
    itemSummaryRequest = new ItemSummaryRequest();
    itemSummaryRequest.setItemSkus(Arrays.asList());
    itemSummaryRequest.setMerchantCode(MERCHANT_CODE);

    vendorNotesResponseData = new VendorNotesResponse();
    List<String> vendorNotes = new ArrayList<>();
    vendorNotes.add(VENDOR_NOTES);
    vendorNotesResponseData.setVendorNotes(vendorNotes);
    vendorNotesResponseData.setItemNotes(new ArrayList<>());
    vendorNotesResponseData.setVendorErrorFields(new ArrayList<>());
    vendorNotesResponseData.setMerchantModifiedFields(new ArrayList<>());
    vendorNotesResponseData.setImageReason(new ArrayList<>());

    itemNotesDto = new ItemNotesDto();
    itemNotesDto.setSkuCode(SKU_CODE);
    itemNotesDto1 = new ItemNotesDto();
    itemNotesDto1.setSkuCode(SKU_CODE_1);

    skuCodesAndItemSkuMap = new HashMap<>();
    skuCodesAndItemSkuMap.put(SKU_CODE_1, ITEM_SKU_1);

    ProductItemWholesalePrice productItemWholesalePrice1 = new ProductItemWholesalePrice();
    productItemWholesalePrice1.setUpdatePending(true);
    productItemWholesalePrice1.setWholesalePriceActivated(true);
    productItemWholesalePrice1.setItemSku(ITEM_SKU);

    ProductItemWholesalePrice productItemWholesalePrice2 = new ProductItemWholesalePrice();
    productItemWholesalePrice2.setUpdatePending(true);
    productItemWholesalePrice2.setWholesalePriceActivated(false);
    productItemWholesalePrice2.setItemSku(ITEM_SKU_1);

    ProductItemWholesalePrice productItemWholesalePrice3 = new ProductItemWholesalePrice();
    productItemWholesalePrice3.setUpdatePending(false);
    productItemWholesalePrice3.setWholesalePriceActivated(false);
    when(productService
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), EditedReviewTypeConstants.CONTENT_REFRESH,
            productCollection, null)).thenReturn(new AddEditedProductToPDTEvent());
    when(productDistributionTaskRepositoryBean
        .checkIfProductExistsInPDT(Mockito.any(), Mockito.anyBoolean())).thenReturn(false);
    productItemWholesalePrices = Arrays.asList(productItemWholesalePrice1, productItemWholesalePrice2, productItemWholesalePrice3);

    when(productLevel3Helper
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(Arrays.asList(new RestrictedKeywordsByField()));

    autoNeedRevisionDomainEvent = new AutoNeedRevisionDomainEvent();
    autoNeedRevisionDomainEvent.setProductCode(PRODUCT_CODE);
    autoNeedRevisionDomainEvent.setPredictionTypeList(Collections.singletonList(PREDICTION_NAME_1));
    autoNeedRevisionDomainEvent.setStoreId(Constants.DEFAULT_STORE_ID);

    productImagePrediction = new ProductImagePrediction();
    productImagePrediction.setDisplayName(PREDICTION_DISPLAY_NAME_1);
    productImagePrediction.setDisplayNameIn(PREDICTION_DISPLAY_NAME_1_IN);
    productImagePrediction.setPredictionType(PREDICTION_NAME_1);

    productImagePredictionBlur = new ProductImagePrediction();
    productImagePredictionBlur.setDisplayName(PREDICTION_DISPLAY_NAME_3);
    productImagePredictionBlur.setDisplayNameIn(PREDICTION_DISPLAY_NAME_3_IN);
    productImagePredictionBlur.setPredictionType(PREDICTION_NAME_3);

    productImagePredictionEn = new ProductImagePrediction();
    productImagePredictionEn.setDisplayName(PREDICTION_DISPLAY_NAME_1);
    productImagePredictionEn.setDisplayNameIn(PREDICTION_DISPLAY_NAME_1_IN);
    productImagePredictionEn.setPredictionType(PREDICTION_NAME_1);

    productImagePredictionBlurEn = new ProductImagePrediction();
    productImagePredictionBlurEn.setDisplayName(PREDICTION_DISPLAY_NAME_3_IN);
    productImagePredictionBlurEn.setDisplayNameIn(PREDICTION_DISPLAY_NAME_3_IN);
    productImagePredictionBlurEn.setPredictionType(PREDICTION_NAME_3);

    retryNeedRevisionRequest = new RetryNeedRevisionRequest();
    retryNeedRevisionRequest.setProductCode(PRODUCT_CODE);
    retryNeedRevisionRequest.setStoreId(STORE_ID);
    retryNeedRevisionRequest.setNotes(NOTES);
    ReflectionTestUtils.setField(productServiceWrapper, "isSkipScreeningSwitch", false);
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);

    needRevisionReasonRequest = new NeedRevisionReasonRequest();

    autoNeedRevisionRequest = new AutoNeedRevisionRequest();
    autoNeedRevisionRequest.setProductCode(PRODUCT_CODE);
    ReflectionTestUtils.setField(productServiceWrapper, "isSkipScreeningSwitch", false);
    ReflectionTestUtils.setField(productServiceWrapper, "publishAutoNeedRevisionEvent", true);

    productImageQcBacklog = new ProductImageQcBacklog();
    productImageQcBacklog.setProductCode(PRODUCT_CODE);

    productImageQcProcessingResponse = new ProductImageQcProcessingResponse();
    productImageQcProcessingResponse.setProductCode(PRODUCT_CODE);
    productImageQcProcessingResponse.setForceReview(true);
    productImageQcProcessingResponse.setImageQcResponse(IMAGE_PREDICTION_RESPONSE_1);

    autoApprovalTypeRequest = new AutoApprovalTypeRequest();
    autoApprovalTypeRequest.setDestinationCategoryCode(CATEGORY_CODE);
    autoApprovalType = AutoApprovalType.CONTENT_AND_IMAGE;

    productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setStoreId(STORE_ID);
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);

    productBusinessPartnerCounter = new ProductBusinessPartnerCounter();
    productBusinessPartnerCounter.setAppealedProductCount(10);
    productBusinessPartnerCounter.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    updatedProductHistory = new UpdatedProductHistory();
    updatedProductHistory.setProductSku(PRODUCT_SKU);

    when(productService.publishImageQcProcessedResponseEvent(any(), any(), any(ProductCollection.class),
        any(ProductDetailResponse.class), any(AutoNeedRevisionAndForceReviewResponse.class), any(ProfileResponse.class),
        any())).thenReturn(ImageQcProcessedResponseDomainEvent.builder().productCode(PRODUCT_CODE).build());
    Mockito.when(fileStorageService.getRevisedImageRequests(Mockito.anyList(), Mockito.any()))
        .thenReturn(new ArrayList<>());
    Mockito.when(productService.saveProductCollection(productCollection)).thenReturn(productCollection);
    ReflectionTestUtils.setField(productServiceWrapper, "retryTimeSpan", RETRY_TIME_SPAN);
    ReflectionTestUtils.setField(productServiceWrapper, "warehouseMerchantCommissionTypes",
        "CC,TD,TC");
    ReflectionTestUtils.setField(productServiceWrapper, "sendProductToAutoNROnBrandOrCategoryTakeDown", true);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productService);
    verifyNoMoreInteractions(productCollectionRepository);
    verifyNoMoreInteractions(solrReviewProductCollectionService);
    verifyNoMoreInteractions(productWfService);
    verifyNoMoreInteractions(productLevel1HistoryService);
    verifyNoMoreInteractions(productBusinessPartnerService);
    verifyNoMoreInteractions(productMailEventService);
    verifyNoMoreInteractions(productRepository, productPublisherService);
    verifyNoMoreInteractions(imageProcessorService);
    verifyNoMoreInteractions(productOutbound);
    verifyNoMoreInteractions(productNotificationService);
    verifyNoMoreInteractions(businessPartnerRepository);
    verifyNoMoreInteractions(productSystemParameterService);
    verifyNoMoreInteractions(updatedProductHistoryService);
    verifyNoMoreInteractions(itemService);
    verifyNoMoreInteractions(productLevel1WipService);
    verifyNoMoreInteractions(approveProductService, xProductOutbound);
    verifyNoMoreInteractions(productImagePredictionService);
    verifyNoMoreInteractions(needCorrectionService);
    verifyNoMoreInteractions(productImageQcBacklogService);
    verifyNoMoreInteractions(fileStorageService);
    verifyNoMoreInteractions(kafkaProducer);
    verifyNoMoreInteractions(productImageQcProcessingResponseService);
    verifyNoMoreInteractions(productWorkflowServiceWrapper);
    verifyNoMoreInteractions(productDistributionService);
    verifyNoMoreInteractions(mandatoryParameterHelper);
  }

  private Product generateProduct() {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    return product;
  }

  private ProductCollection generateProductCollection() {
    ProductCollection productCollection1 = new ProductCollection();
    productCollection1.setStoreId(STORE_ID);
    productCollection1.setProductCode(PRODUCT_CODE);
    productCollection1.setActivated(false);
    productCollection1.setViewable(false);
    productCollection1.setState(DRAFT_STATE);
    productCollection1.setProductId(PRODUCT_ID);
    productCollection1.setImageResized(true);
    return productCollection1;
  }

  @Test
  public void bulkUpdateActivatedProductsWithoutProductCollectionChangeTest() throws Exception {
    SimpleMasterProductUpdateResponse simpleMasterProductUpdateResponse = new SimpleMasterProductUpdateResponse();
    simpleMasterProductUpdateResponse.setProductCode(PRODUCT_CODE);
    simpleMasterProductUpdateResponse.setUpdateSuccess(Boolean.TRUE);
    productCollection.setActivated(Boolean.TRUE);
    productCollection.setViewable(Boolean.TRUE);
    productCollection.setProductCode(PRODUCT_CODE);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productRepository.findOne(productCollection.getProductId())).thenReturn(product);
    when(productService.updateForBulk(simpleMasterProductUpdateRequestDTO, STORE_ID, productCollection))
        .thenReturn(simpleMasterProductUpdateResponse);
    BulkMasterProductUpdateResponse response =
        productServiceWrapper.bulkUpdateActivatedProducts(bulkMasterProductUpdateRequestDTO, STORE_ID);
    Mockito.verify(productService)
        .checkAndUpdateSolr(STORE_ID, simpleMasterProductUpdateRequestDTO, product, productCollection);
    verify(productService).updateForBulk(simpleMasterProductUpdateRequestDTO, STORE_ID, productCollection);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productRepository).findOne(productCollection.getProductId());
    assertTrue(response.getSimpleMasterProductUpdateResponses().get(0).getUpdateSuccess());
  }

  @Test
  public void bulkUpdateActivatedProductsTest() throws Exception {
    SimpleMasterProductUpdateResponse simpleMasterProductUpdateResponse = new SimpleMasterProductUpdateResponse();
    simpleMasterProductUpdateResponse.setProductCode(PRODUCT_CODE);
    simpleMasterProductUpdateResponse.setUpdateSuccess(Boolean.TRUE);
    productCollection.setActivated(Boolean.TRUE);
    productCollection.setViewable(Boolean.TRUE);
    productCollection.setPostLive(Boolean.TRUE);
    productCollection.setProductCode(PRODUCT_CODE);
    Map<Boolean, ProductCollection> productCollectionMap = new HashMap<>();
    productCollectionMap.put(Boolean.TRUE, productCollection);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productRepository.findOne(productCollection.getProductId())).thenReturn(product);
    when(productService.updateForBulk(simpleMasterProductUpdateRequestDTO, STORE_ID, productCollection))
        .thenReturn(simpleMasterProductUpdateResponse);
    when(productService.checkAndUpdateSolr(STORE_ID, simpleMasterProductUpdateRequestDTO, product, productCollection))
        .thenReturn(productCollectionMap);
    BulkMasterProductUpdateResponse response =
        productServiceWrapper.bulkUpdateActivatedProducts(bulkMasterProductUpdateRequestDTO, STORE_ID);
    Mockito.verify(productService)
        .checkAndUpdateSolr(STORE_ID, simpleMasterProductUpdateRequestDTO, product, productCollection);
    verify(productService)
        .updateForBulk(simpleMasterProductUpdateRequestDTOArgumentCaptor.capture(), Mockito.eq(STORE_ID),
            Mockito.eq(productCollection));
    verify(productService).updateSolrProductCollection(productCollection);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productRepository).findOne(productCollection.getProductId());
    assertTrue(simpleMasterProductUpdateRequestDTOArgumentCaptor.getValue().isPostLive());
    assertTrue(response.getSimpleMasterProductUpdateResponses().get(0).getUpdateSuccess());
    assertEquals(PRODUCT_CODE, response.getSimpleMasterProductUpdateResponses().get(0).getProductCode());
  }

  @Test
  public void bulkUpdateActivatedProducts_BulkUpdateExceptionTest() throws Exception {
    productCollection.setActivated(Boolean.TRUE);
    productCollection.setViewable(Boolean.TRUE);
    productCollection.setProductCode(PRODUCT_CODE);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productRepository.findOne(productCollection.getProductId())).thenReturn(product);
    doThrow(Exception.class).when(productService)
        .updateForBulk(simpleMasterProductUpdateRequestDTO, STORE_ID, productCollection);
    BulkMasterProductUpdateResponse response =
        productServiceWrapper.bulkUpdateActivatedProducts(bulkMasterProductUpdateRequestDTO, STORE_ID);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productRepository).findOne(productCollection.getProductId());
    Mockito.verify(productService).updateForBulk(simpleMasterProductUpdateRequestDTO, STORE_ID, productCollection);
    assertEquals(PRODUCT_CODE, response.getSimpleMasterProductUpdateResponses().get(0).getProductCode());
    assertFalse(response.getSimpleMasterProductUpdateResponses().get(0).getUpdateSuccess());
  }

  @Test
  public void doScreeningProductsBulkActions_BulkAssignTest() throws Exception {
    screeningProductBulkActionsRequest.setAssignTo(ASSIGNED_TO);
    screeningProductBulkActionsRequest.setAssignedBy(ASSIGNED_BY);
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT_ID);
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1)).thenReturn(PRODUCT_ID_1);
    productServiceWrapper
        .doScreeningProductsBulkActions(STORE_ID, BulkActionType.ASSIGN, screeningProductBulkActionsRequest);
    verify(productService).assignProducts(STORE_ID, productCodes, ASSIGNED_TO, ASSIGNED_BY);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1);
    verify(solrReviewProductCollectionService).updateAssignedToInReviewProductCollection(PRODUCT_ID, ASSIGNED_TO);
    verify(solrReviewProductCollectionService).updateAssignedToInReviewProductCollection(PRODUCT_ID_1, ASSIGNED_TO);
    verify(productLevel1HistoryService)
        .addProductHistoryForProductAssignment(screeningProductBulkActionsRequest.getProductCodes(),
            screeningProductBulkActionsRequest.getAssignTo(), screeningProductBulkActionsRequest.getAssignedBy());
  }

  @Test
  public void doScreeningProductsBulkActions_BulkAssign_WithoutAssigneeTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productServiceWrapper
          .doScreeningProductsBulkActions(STORE_ID, BulkActionType.ASSIGN, screeningProductBulkActionsRequest);
    });
  }

  @Test
  public void doScreeningProductsBulkActions_BulkUnAssignTest() throws Exception {
    screeningProductBulkActionsRequest.setAssignedBy(ASSIGNED_BY);
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT_ID);
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1)).thenReturn(PRODUCT_ID_1);
    productServiceWrapper
        .doScreeningProductsBulkActions(STORE_ID, BulkActionType.UN_ASSIGN, screeningProductBulkActionsRequest);
    verify(productService).assignProducts(STORE_ID, productCodes, SolrConstants.ASSIGNED_TO_PREFIX, ASSIGNED_BY);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1);
    verify(solrReviewProductCollectionService)
        .updateAssignedToInReviewProductCollection(PRODUCT_ID, SolrConstants.ASSIGNED_TO_PREFIX);
    verify(solrReviewProductCollectionService)
        .updateAssignedToInReviewProductCollection(PRODUCT_ID_1, SolrConstants.ASSIGNED_TO_PREFIX);
    verify(productLevel1HistoryService)
        .addProductHistoryForProductAssignment(screeningProductBulkActionsRequest.getProductCodes(),
            screeningProductBulkActionsRequest.getAssignTo(), screeningProductBulkActionsRequest.getAssignedBy());
  }

  @Test
  public void doScreeningProductsBulkActions_BulkNeedCorrectionTest() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.name());
    revisionNotesWebRequest.setAllVariants(false);
    screeningProductBulkActionsRequest.setCorrectionReason(CORRECTION_REASON);
    screeningProductBulkActionsRequest.setAdditionalNotes(ADDITIONAL_NOTES);
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT_ID);
    when(productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, screeningProductBulkActionsRequest.getProductCodes()))
        .thenReturn(Arrays.asList(productCollection));
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1)).thenReturn(PRODUCT_ID_1);
    productServiceWrapper
        .doScreeningProductsBulkActions(STORE_ID, BulkActionType.SEND_FOR_REVISION, screeningProductBulkActionsRequest);
    verify(productWfService).returnForCorrection(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1),
        PRODUCT_CODE + Constants.COMMA + PRODUCT_CODE_1 + Constants.COMMA + CORRECTION_REASON_1 +
        Constants.COMMA + CORRECTION_REASON_1 + Constants.DASH_DELIMITER + NOTES + Constants.COMMA + ADDITIONAL_NOTES_1,
        revisionNotesWebRequest, false);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID_1);
  }

  @Test
  public void doScreeningProductsBulkActions_BulkNeedCorrectionTest_InvalidStateTest() throws Exception {
    productCollection.setState(WorkflowStates.IN_VENDOR.name());
    screeningProductBulkActionsRequest.setRejectionReason(CORRECTION_REASON);
    screeningProductBulkActionsRequest.setAdditionalNotes(ADDITIONAL_NOTES);
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT_ID);
    when(productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, screeningProductBulkActionsRequest.getProductCodes()))
        .thenReturn(Arrays.asList(productCollection));
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1)).thenReturn(PRODUCT_ID_1);
    try {
      productServiceWrapper
          .doScreeningProductsBulkActions(STORE_ID, BulkActionType.REJECT, screeningProductBulkActionsRequest);
    } catch (Exception e) {
      Assertions.assertEquals(ApplicationRuntimeException.class, e.getClass());
    } finally {
      verify(productCollectionRepository).findByStoreIdAndProductCodeIn(STORE_ID, screeningProductBulkActionsRequest.getProductCodes());
    }
  }

  @Test
  public void doScreeningProductsBulkActions_BulkNeedCorrectionVendorNotesTest() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.name());
    screeningProductBulkActionsRequest.setImageReason(null);
    screeningProductBulkActionsRequest.setCommonImageReason(null);
    screeningProductBulkActionsRequest.setImagesAdditionalNotes(null);
    revisionNotesWebRequest.setImageReason(null);
    revisionNotesWebRequest.setCommonImageReason(null);
    revisionNotesWebRequest.setImagesAdditionalNotes(null);
    revisionNotesWebRequest.setAllVariants(false);
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT_ID);
    when(productCollectionRepository
        .findByStoreIdAndProductCodeIn(STORE_ID, screeningProductBulkActionsRequest.getProductCodes()))
        .thenReturn(Arrays.asList(productCollection));
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1)).thenReturn(PRODUCT_ID_1);
    productServiceWrapper
        .doScreeningProductsBulkActions(STORE_ID, BulkActionType.SEND_FOR_REVISION, screeningProductBulkActionsRequest);
    verify(productWfService).returnForCorrection(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1),
        PRODUCT_CODE + Constants.COMMA + PRODUCT_CODE_1 + Constants.DASH_DELIMITER + NOTES, revisionNotesWebRequest,
        false);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID_1);
  }

  @Test
  public void doScreeningProductsBulkActions_BulkNeedCorrectionImageNotesTest() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.name());
    screeningProductBulkActionsRequest.setVendorNotes(null);
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT_ID);
    when(productCollectionRepository
        .findByStoreIdAndProductCodeIn(STORE_ID, screeningProductBulkActionsRequest.getProductCodes()))
        .thenReturn(Arrays.asList(productCollection));
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1)).thenReturn(PRODUCT_ID_1);
    productServiceWrapper
        .doScreeningProductsBulkActions(STORE_ID, BulkActionType.SEND_FOR_REVISION, screeningProductBulkActionsRequest);
    verify(productWfService).returnForCorrection(Mockito.eq(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1)),
        Mockito.eq(CORRECTION_REASON_1 + Constants.COMMA + CORRECTION_REASON_1 + Constants.DELIMITER_DASH + ADDITIONAL_NOTES_1),
        Mockito.any(NeedRevisionNotes.class), Mockito.eq(false));
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID_1);
  }

  @Test
  public void doScreeningProductsBulkActions_BulkNeedCorrectionCommonImageNotesTest() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.name());
    screeningProductBulkActionsRequest.setVendorNotes(null);
    screeningProductBulkActionsRequest.setImageReason(null);
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT_ID);
    when(productCollectionRepository
        .findByStoreIdAndProductCodeIn(STORE_ID, screeningProductBulkActionsRequest.getProductCodes()))
        .thenReturn(Arrays.asList(productCollection));
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1)).thenReturn(PRODUCT_ID_1);
    productServiceWrapper
        .doScreeningProductsBulkActions(STORE_ID, BulkActionType.SEND_FOR_REVISION, screeningProductBulkActionsRequest);
    verify(productWfService).returnForCorrection(Mockito.eq(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1)),
        Mockito.eq(CORRECTION_REASON_1 + Constants.DELIMITER_DASH + ADDITIONAL_NOTES_1),
        Mockito.any(NeedRevisionNotes.class), Mockito.eq(false));
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID_1);
  }


  @Test
  public void doScreeningProductsBulkActions_BulkNeedCorrectionImageNotes1Test() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.name());
    screeningProductBulkActionsRequest.setVendorNotes(null);
    screeningProductBulkActionsRequest.setCommonImageReason(null);
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT_ID);
    when(productCollectionRepository
        .findByStoreIdAndProductCodeIn(STORE_ID, screeningProductBulkActionsRequest.getProductCodes()))
        .thenReturn(Arrays.asList(productCollection));
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1)).thenReturn(PRODUCT_ID_1);
    productServiceWrapper
        .doScreeningProductsBulkActions(STORE_ID, BulkActionType.SEND_FOR_REVISION, screeningProductBulkActionsRequest);
    verify(productWfService).returnForCorrection(Mockito.eq(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1)),
        Mockito.eq(CORRECTION_REASON_1 + Constants.DELIMITER_DASH + ADDITIONAL_NOTES_1),
        Mockito.any(NeedRevisionNotes.class), Mockito.eq(false));
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID_1);
  }

  @Test
  public void doScreeningProductsBulkActions_BulkNeedCorrectionImageReasonTest() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.name());
    screeningProductBulkActionsRequest.setImageReason(new ArrayList<>());
    screeningProductBulkActionsRequest.setCommonImageReason(new ArrayList<>());
    revisionNotesWebRequest.setAllVariants(false);
    revisionNotesWebRequest.setImageReason(new ArrayList<>());
    revisionNotesWebRequest.setCommonImageReason(new ArrayList<>());
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT_ID);
    when(productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID,
        screeningProductBulkActionsRequest.getProductCodes())).thenReturn(Arrays.asList(productCollection));
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1)).thenReturn(PRODUCT_ID_1);
    productServiceWrapper.doScreeningProductsBulkActions(STORE_ID, BulkActionType.SEND_FOR_REVISION,
        screeningProductBulkActionsRequest);
    verify(productWfService).returnForCorrection(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1),
        PRODUCT_CODE + Constants.COMMA + PRODUCT_CODE_1 + Constants.DASH_DELIMITER + NOTES + Constants.COMMA
            + ADDITIONAL_NOTES_1, revisionNotesWebRequest, false);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID_1);
  }

  @Test
  public void doScreeningProductsBulkActions_BulkNeedCorrectionCommonImageReasonTest() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.name());
    screeningProductBulkActionsRequest.setCommonImageReason(new ArrayList<>());
    revisionNotesWebRequest.setAllVariants(false);
    revisionNotesWebRequest.setCommonImageReason(new ArrayList<>());
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT_ID);
    when(productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID,
        screeningProductBulkActionsRequest.getProductCodes())).thenReturn(Arrays.asList(productCollection));
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1)).thenReturn(PRODUCT_ID_1);
    productServiceWrapper.doScreeningProductsBulkActions(STORE_ID, BulkActionType.SEND_FOR_REVISION,
        screeningProductBulkActionsRequest);
    verify(productWfService).returnForCorrection(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1),
        PRODUCT_CODE + Constants.COMMA + PRODUCT_CODE_1 + Constants.COMMA + CORRECTION_REASON_1 +  Constants.DASH_DELIMITER + NOTES + Constants.COMMA
            + ADDITIONAL_NOTES_1, revisionNotesWebRequest, false);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID_1);
  }

  @Test
  public void doScreeningProductsBulkActions_BulkNeedCorrectionCommonImageReason1Test() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.name());
    screeningProductBulkActionsRequest.setImageReason(new ArrayList<>());
    screeningProductBulkActionsRequest.setCommonImageReason(Arrays.asList("ABCD"));
    revisionNotesWebRequest.setAllVariants(false);
    revisionNotesWebRequest.setImageReason(new ArrayList<>());
    revisionNotesWebRequest.setCommonImageReason(Arrays.asList("ABCD"));
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT_ID);
    when(productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID,
        screeningProductBulkActionsRequest.getProductCodes())).thenReturn(Arrays.asList(productCollection));
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1)).thenReturn(PRODUCT_ID_1);
    productServiceWrapper.doScreeningProductsBulkActions(STORE_ID, BulkActionType.SEND_FOR_REVISION,
        screeningProductBulkActionsRequest);
    verify(productWfService).returnForCorrection(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1),
        PRODUCT_CODE + Constants.COMMA + PRODUCT_CODE_1 + Constants.COMMA +"ABCD" + Constants.DASH_DELIMITER + NOTES + Constants.COMMA
            + ADDITIONAL_NOTES_1, revisionNotesWebRequest, false);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID_1);
  }

  @Test
  public void updateImagePathsForEditedResizeImagesSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "checkRestrictedKeywordsInEditedImage", true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_CODE)).thenReturn(productCollection);
    when(productService.publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE,
      productCollection, null)).thenReturn(new AddEditedProductToPDTEvent());
    when(productService.publishImageQcEventForEditedImages(PRODUCT_CODE, eq(anyList()), eq(new ProductDetailResponse()),
      eq(false), eq(new RestrictedKeywordsByFieldAndActionType()), eq(BUSINESS_PARTNER_CODE))).thenReturn(
      new ImageQcRequestDomainEvent());
    doNothing().when(productService)
      .updateEditedImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class),
        eq(productCollection), eq(true), eq(false));
    productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, false);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(1)).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_CODE);
    verify(productService).updateEditedImagePathsAndFlagAfterResizingImage(
      productAndItemImageRequestArgumentCaptor.capture(), productCollectionArgumentCaptor.capture(), eq(true),
      eq(false));
    verify(productService).publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE,
      productCollection, null);
    verify(productService).publishAddEditedProductToPDTEvent(productCollection.getStoreId(),
      EditedReviewTypeConstants.CONTENT_REFRESH, productCollection, null);
    verify(productService).publishImageQcEventForEditedImages(PRODUCT_CODE,
      productAndItemImageRequestArgumentCaptor.getValue().getProductImages(), productDetailResponse, false,
      new RestrictedKeywordsByFieldAndActionType(), BUSINESS_PARTNER_CODE);
    verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(
      Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals(0,
      productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
        .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
      .filter(image -> !image.getOriginalImage()).count());
  }
  @Test
  public void updateImagePathsForEditedResizeImagesSwitchOnKeywordsTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "checkRestrictedKeywordsInEditedImage", true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_CODE)).thenReturn(productCollection);
    when(productService.publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE,
      productCollection, null)).thenReturn(new AddEditedProductToPDTEvent());
    when(productService.publishImageQcEventForEditedImages(PRODUCT_CODE, eq(anyList()), eq(new ProductDetailResponse()),
      eq(false), eq(new RestrictedKeywordsByFieldAndActionType()), eq(BUSINESS_PARTNER_CODE))).thenReturn(
      new ImageQcRequestDomainEvent());
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(
      Mockito.any(ProductDetailResponse.class), Mockito.any())).thenReturn(restrictedKeywordsByFieldAndActionType);
    doNothing().when(productService)
      .updateEditedImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class),
        eq(productCollection), eq(true), eq(false));
    productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, false);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(1)).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_CODE);
    verify(productService).updateEditedImagePathsAndFlagAfterResizingImage(
      productAndItemImageRequestArgumentCaptor.capture(), productCollectionArgumentCaptor.capture(), eq(true),
      eq(false));
    verify(productService).publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE,
      productCollection, null);
    verify(productService).publishAddEditedProductToPDTEvent(productCollection.getStoreId(),
      EditedReviewTypeConstants.CONTENT_REFRESH, productCollection, null);
    verify(productService).publishImageQcEventForEditedImages(PRODUCT_CODE,
      productAndItemImageRequestArgumentCaptor.getValue().getProductImages(), productDetailResponse, false,
      new RestrictedKeywordsByFieldAndActionType(), BUSINESS_PARTNER_CODE);
    verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(
      Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals(0,
      productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
        .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
      .filter(image -> !image.getOriginalImage()).count());
  }


  @Test
  public void doScreeningProductsBulkAvtions_BulkNeedCorrectionExceptionTestTest() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.name());
    screeningProductBulkActionsRequest.setVendorNotes(null);
    screeningProductBulkActionsRequest.setImageReason(null);
    screeningProductBulkActionsRequest.setCommonImageReason(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productServiceWrapper.doScreeningProductsBulkActions(STORE_ID, BulkActionType.SEND_FOR_REVISION, screeningProductBulkActionsRequest);
    });
  }

  @Test
  public void doScreeningProductsBulkActions_BulkRejectionTest_DeletedProduct() throws Exception {
    productCollection.setMarkForDelete(true);
    screeningProductBulkActionsRequest.setRejectionReason(CORRECTION_REASON);
    screeningProductBulkActionsRequest.setAdditionalNotes(ADDITIONAL_NOTES);
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT_ID);
    when(productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, screeningProductBulkActionsRequest.getProductCodes()))
        .thenReturn(Arrays.asList(productCollection));
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1)).thenReturn(PRODUCT_ID_1);
    try {
      productServiceWrapper
          .doScreeningProductsBulkActions(STORE_ID, BulkActionType.REJECT, screeningProductBulkActionsRequest);
    } catch (Exception e) {
      Assertions.assertEquals(ApplicationRuntimeException.class, e.getClass());
    } finally {
      verify(productCollectionRepository).findByStoreIdAndProductCodeIn(STORE_ID, screeningProductBulkActionsRequest.getProductCodes());
    }
  }

  @Test
  public void doScreeningProductsBulkActions_BulkRejectionTest() throws Exception {
    productCollection.setState(WorkflowStates.DRAFT.name());
    screeningProductBulkActionsRequest.setRejectionReason(REJECTION_REASON);
    screeningProductBulkActionsRequest.setAdditionalNotes(ADDITIONAL_NOTES);
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT_ID);
    when(productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1)).thenReturn(PRODUCT_ID_1);
    when(productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, screeningProductBulkActionsRequest.getProductCodes()))
        .thenReturn(Arrays.asList(productCollection));
    productServiceWrapper
        .doScreeningProductsBulkActions(STORE_ID, BulkActionType.REJECT, screeningProductBulkActionsRequest);
    verify(productWfService).delete(productCodes, REJECTION_REASON + Constants.DASH_DELIMITER + ADDITIONAL_NOTES);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE_1);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(PRODUCT_ID_1);
    ArgumentCaptor<String> productCodesCaptor = ArgumentCaptor.forClass(String.class);
    verify(productMailEventService, times(2)).createAndSaveMailEvent(productCodesCaptor.capture(),
        eq(REJECTION_REASON + Constants.DASH_DELIMITER + ADDITIONAL_NOTES), eq(ProductMailEventsEnum.REJECTED));
    verify(itemService).publishItemStatusEvent(PRODUCT_CODE, ProductStatus.REJECTED);
    verify(itemService).publishItemStatusEvent(PRODUCT_CODE_1, ProductStatus.REJECTED);
    verify(productCollectionRepository).findByStoreIdAndProductCodeIn(STORE_ID, screeningProductBulkActionsRequest.getProductCodes());
    List<String> productCodesCaptured = productCodesCaptor.getAllValues();
    assertTrue(productCodesCaptured.contains(PRODUCT_CODE));
    assertTrue(productCodesCaptured.contains(PRODUCT_CODE_1));
  }

  @Test
  public void doScreeningProductsBulkActions_BulkNeedRejection_WithoutReasonTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productServiceWrapper
          .doScreeningProductsBulkActions(STORE_ID, BulkActionType.REJECT, screeningProductBulkActionsRequest);
    });
  }

  @Test
  public void deleteTest() throws Exception {
    Mockito.doNothing().when(this.productService).delete(PRODUCT_ID);
    Mockito.when(this.productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(ID);
    Mockito.doNothing().when(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
    this.productServiceWrapper.delete(STORE_ID, PRODUCT_ID, PRODUCT_CODE);
    Mockito.verify(this.productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productService).delete(PRODUCT_ID);
    Mockito.verify(this.solrReviewProductCollectionService).deleteProductFromReviewProductCollection(ID);
  }

  @Test
  public void updateProductAssignmentStatusTest() throws Exception {
    Mockito.doNothing().when(this.productService)
        .assignProducts(STORE_ID, Collections.singletonList(PRODUCT_CODE), ASSIGNED_TO, ASSIGNED_BY);
    Mockito.when(this.productCollectionRepository.findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(ID);
    Mockito.doNothing().when(this.solrReviewProductCollectionService)
        .updateAssignedToInReviewProductCollection(ID, ASSIGNED_TO);
    this.productServiceWrapper.updateProductAssignmentStatus(STORE_ID, PRODUCT_CODE, ASSIGNED_TO, ASSIGNED_BY);
    Mockito.verify(this.productCollectionRepository).findIdByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productService)
        .assignProducts(STORE_ID, Collections.singletonList(PRODUCT_CODE), ASSIGNED_TO, ASSIGNED_BY);
    Mockito.verify(this.solrReviewProductCollectionService).updateAssignedToInReviewProductCollection(ID, ASSIGNED_TO);
    Mockito.verify(this.productLevel1HistoryService)
        .addProductHistoryForProductAssignment(Collections.singletonList(PRODUCT_CODE), ASSIGNED_TO, ASSIGNED_BY);
  }

  @Test
  public void getProductRevisionInfoTest() {
    productHistory3.setCreatedDate(new Date());
    productCollection.setResubmitCount(1);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productLevel1HistoryService.findByStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productHistory1, productHistory2, productHistory3));
    List<ProductRevisionInfoResponse> result = productServiceWrapper.getProductRevisionInfo(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productLevel1HistoryService).findByStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID);
    assertEquals(2, result.size());
    assertEquals(REASON_1, result.get(0).getCorrectionReason());
    assertEquals(ADDITIONAL_NOTES_1, result.get(0).getAdditionalNotes());
    assertEquals(CREATED_BY, result.get(0).getCreatedBy());
    assertEquals(REASON, result.get(1).getCorrectionReason());
    assertEquals(ADDITIONAL_NOTES, result.get(1).getAdditionalNotes());
    assertEquals(CREATED_BY, result.get(1).getCreatedBy());
    assertEquals(CREATED_DATE, result.get(1).getCreatedDate());
  }

  @Test
  public void getProductRevisionInfoProductCollectionNotExistTest() {
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(null);
    List<ProductRevisionInfoResponse> result = productServiceWrapper.getProductRevisionInfo(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    assertEquals(0, result.size());
  }

  @Test
  public void updateTest() throws Exception {
    Product generateProduct = generateProduct();
    ProductCollection generatedProductCollection = generateProductCollection();
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, generateProduct.getProductCode()))
        .thenReturn(generatedProductCollection);
    productServiceWrapper
        .update(STORE_ID, generateProduct, NOTES, true, BRAND_CODE, false, StringUtils.EMPTY, BRAND_APPROVAL_STATUS, false);
    verify(productService)
        .update(generateProduct, false, NOTES, BRAND_CODE, BRAND_APPROVAL_STATUS, true, false, StringUtils.EMPTY, false);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, generateProduct.getProductCode());
    verify(productBusinessPartnerService).markItemsAsUnBuyableAndUnViewable(PRODUCT_ID);
    verify(solrReviewProductCollectionService).addProductToReviewProductCollection(generatedProductCollection);
  }

  @Test
  public void updateProductNeedForCorrectionTest() throws Exception {
    Product generatedProduct = generateProduct();
    ProductCollection generatedProductCollection = generateProductCollection();
    generatedProductCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode()))
        .thenReturn(generatedProductCollection);
    productServiceWrapper
        .update(STORE_ID, generatedProduct, NOTES, true, BRAND_CODE, false, StringUtils.EMPTY, BRAND_APPROVAL_STATUS, false);
    verify(productService)
        .update(generatedProduct, false, NOTES, BRAND_CODE, BRAND_APPROVAL_STATUS, true, false, StringUtils.EMPTY, false);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode());
    verify(productBusinessPartnerService).markItemsAsUnBuyableAndUnViewable(PRODUCT_ID);
  }

  @Test
  public void updateDeletedProductTest() throws Exception {
    Product generatedProduct = generateProduct();
    ProductCollection generatedProductCollection = generateProductCollection();
    generatedProductCollection.setMarkForDelete(true);
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode()))
        .thenReturn(generatedProductCollection);
    productServiceWrapper
        .update(STORE_ID, generatedProduct, NOTES, true, BRAND_CODE, false, StringUtils.EMPTY, BRAND_APPROVAL_STATUS, false);
    verify(productService)
        .update(generatedProduct, false, NOTES, BRAND_CODE, BRAND_APPROVAL_STATUS, true, false, StringUtils.EMPTY, false);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode());
    verify(productBusinessPartnerService).markItemsAsUnBuyableAndUnViewable(PRODUCT_ID);
  }

  @Test
  public void updateViewableProductTest() throws Exception {
    Product generatedProduct = generateProduct();
    ProductCollection generatedProductCollection = generateProductCollection();
    generatedProductCollection.setViewable(true);
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode()))
        .thenReturn(generatedProductCollection);
    productServiceWrapper
        .update(STORE_ID, generatedProduct, NOTES, true, BRAND_CODE, false, StringUtils.EMPTY, BRAND_APPROVAL_STATUS, false);
    verify(productService)
        .update(generatedProduct, false, NOTES, BRAND_CODE, BRAND_APPROVAL_STATUS, true, false, StringUtils.EMPTY, false);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode());
    verify(productBusinessPartnerService).markItemsAsUnBuyableAndUnViewable(PRODUCT_ID);
  }

  @Test
  public void updateActivatedProductTest() throws Exception {
    Product generatedProduct = generateProduct();
    ProductCollection generatedProductCollection = generateProductCollection();
    generatedProductCollection.setActivated(true);
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode()))
        .thenReturn(generatedProductCollection);
    productServiceWrapper
        .update(STORE_ID, generatedProduct, NOTES, true, BRAND_CODE, false, StringUtils.EMPTY, BRAND_APPROVAL_STATUS, false);
    verify(productService)
        .update(generatedProduct, false, NOTES, BRAND_CODE, BRAND_APPROVAL_STATUS, true, false, StringUtils.EMPTY, false);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode());
    verify(productBusinessPartnerService).markItemsAsUnBuyableAndUnViewable(PRODUCT_ID);
  }

  @Test
  public void updateWithImageResizedFalseTest() throws Exception {
    Product generatedProduct = generateProduct();
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode()))
        .thenReturn(productCollection);
    when(productPublisherService
        .publishProductImageResizeEvent(productCollection.getProductCode(), productCollection.getStoreId()))
        .thenReturn(new ImageResizeEvent());
    productServiceWrapper
        .update(STORE_ID, generatedProduct, NOTES, true, BRAND_CODE, false, StringUtils.EMPTY, BRAND_APPROVAL_STATUS, false);
    verify(productService)
        .update(generatedProduct, false, NOTES, BRAND_CODE, BRAND_APPROVAL_STATUS, true, false, StringUtils.EMPTY, false);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode());
    verify(productPublisherService)
        .publishProductImageResizeEvent(productCollection.getProductCode(), productCollection.getStoreId());
    verify(productBusinessPartnerService).markItemsAsUnBuyableAndUnViewable(PRODUCT_ID);
  }

  @Test
  public void updateWithMarkForDeleteTrueTest() throws Exception {
    Product generatedProduct = generateProduct();
    ProductCollection generatedProductCollection = generateProductCollection();
    generatedProductCollection.setMarkForDelete(Boolean.TRUE);
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode()))
        .thenReturn(generatedProductCollection);
    productServiceWrapper
        .update(STORE_ID, generatedProduct, NOTES, true, BRAND_CODE, false, StringUtils.EMPTY, BRAND_APPROVAL_STATUS, false);
    verify(productService)
        .update(generatedProduct, false, NOTES, BRAND_CODE, BRAND_APPROVAL_STATUS, true, false, StringUtils.EMPTY, false);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode());
    verify(productBusinessPartnerService).markItemsAsUnBuyableAndUnViewable(PRODUCT_ID);
  }

  @Test
  public void updateTestMarginNoExceed() throws Exception {
    Product generatedProduct = generateProduct();
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(generateProductCollection());
    productServiceWrapper
        .update(STORE_ID, generatedProduct, NOTES, false, BRAND_CODE, false, StringUtils.EMPTY, BRAND_APPROVAL_STATUS, false);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productService)
        .update(generatedProduct, false, NOTES, BRAND_CODE, BRAND_APPROVAL_STATUS, false, false, StringUtils.EMPTY, false);
    verify(solrReviewProductCollectionService).addProductToReviewProductCollection(generateProductCollection());
  }

  @Test
  public void updateTestDescriptiveAttributeWithSkuTrue() throws Exception {
    Product generatedProduct = generateProduct();
    productAttributeList.get(0).getAttribute().setSkuValue(true);
    generatedProduct.setProductAttributes(productAttributeList);
    ProductCollection generatedProductCollection = generateProductCollection();
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode()))
        .thenReturn(generatedProductCollection);
    productServiceWrapper
        .update(STORE_ID, generatedProduct, NOTES, false, BRAND_CODE, false, StringUtils.EMPTY, BRAND_APPROVAL_STATUS, false);
    verify(productService)
        .update(generatedProduct, false, NOTES, BRAND_CODE, BRAND_APPROVAL_STATUS, false, false, StringUtils.EMPTY, false);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode());
    verify(productBusinessPartnerService)
        .updateSkuValueTrueInProductBusinessPartnerAttribute(productAttributeList, PRODUCT_ID);
    verify(solrReviewProductCollectionService).addProductToReviewProductCollection(generatedProductCollection);
  }

  @Test
  public void updateTestDescriptiveAttributeWithSkuFalse() throws Exception {
    Product generatedProduct = generateProduct();
    productAttributeList.get(0).getAttribute().setSkuValue(false);
    generatedProduct.setProductAttributes(productAttributeList);
    ProductCollection generatedProductCollection = generateProductCollection();
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode()))
        .thenReturn(generatedProductCollection);
    productServiceWrapper
        .update(STORE_ID, generatedProduct, NOTES, false, BRAND_CODE, false, StringUtils.EMPTY, BRAND_APPROVAL_STATUS, false);
    verify(productService)
        .update(generatedProduct, false, NOTES, BRAND_CODE, BRAND_APPROVAL_STATUS, false, false, StringUtils.EMPTY, false);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, generatedProduct.getProductCode());
    verify(solrReviewProductCollectionService).addProductToReviewProductCollection(generatedProductCollection);
  }

  @Test
  public void changeBrandCodeAndBrandApprovalStatusForApprovedBrandTest() throws Exception {
    productCollection.setBrandCode(BRAND_CODE);
    productCollection1.setBrandCode(BRAND_CODE);
    productCollection1.setId(ID);
    productCollection1.setProductCode(PRODUCT_CODE_1);
    when(this.productCollectionRepository
        .findByBrandCodeAndBrandApprovalStatusAndMarkForDeleteFalse(BRAND_CODE, BrandApprovalStatus.DRAFT, pageable))
        .thenReturn(new PageImpl<>(Arrays.asList(productCollection, productCollection1)));
    when(this.productCollectionRepository
        .updateBrandCodeAndBrandApprovalStatus(eq(BRAND_CODE), eq(BrandApprovalStatus.APPROVED), eq(BRAND_NAME),
            Mockito.any())).thenReturn(1);
    doNothing().when(this.solrReviewProductCollectionService)
        .updateBrandApprovedInReviewProductCollection(ID, Boolean.TRUE, BRAND_NAME);
    Mockito.doNothing().when(productOutbound).clearProductCacheByProductCodes(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
    this.productServiceWrapper.changeBrandCodeAndBrandApprovalStatusInScreeningProducts(BRAND_CODE, BRAND_CODE,
        BrandApprovalStatus.APPROVED.name(), BRAND_NAME);
    verify(this.solrReviewProductCollectionService, times(2))
        .updateBrandApprovedInReviewProductCollection(ID, Boolean.TRUE, BRAND_NAME);
    verify(this.productCollectionRepository)
        .findByBrandCodeAndBrandApprovalStatusAndMarkForDeleteFalse(BRAND_CODE, BrandApprovalStatus.DRAFT, pageable);
    verify(this.productCollectionRepository, times(2))
        .updateBrandCodeAndBrandApprovalStatus(eq(BRAND_CODE), eq(BrandApprovalStatus.APPROVED), eq(BRAND_NAME),
            Mockito.any());
    verify(productOutbound).clearProductCacheByProductCodes(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
  }

  @Test
  public void changeBrandCodeAndBrandApprovalStatusForApprovedBrandExceptionTest() throws Exception {
    productCollection.setBrandCode(BRAND_CODE);
    productCollection1.setBrandCode(BRAND_CODE);
    productCollection1.setId(ID);
    productCollection1.setProductCode(PRODUCT_CODE_1);
    when(this.productCollectionRepository
        .findByBrandCodeAndBrandApprovalStatusAndMarkForDeleteFalse(BRAND_CODE, BrandApprovalStatus.DRAFT, pageable))
        .thenReturn(new PageImpl<>(Arrays.asList(productCollection, productCollection1)));
    when(this.productCollectionRepository
        .updateBrandCodeAndBrandApprovalStatus(eq(BRAND_CODE), eq(BrandApprovalStatus.APPROVED), eq(BRAND_NAME),
            Mockito.any())).thenReturn(1);
    doNothing().when(this.solrReviewProductCollectionService)
        .updateBrandApprovedInReviewProductCollection(ID, Boolean.TRUE, BRAND_NAME);
    Mockito.doThrow(ApplicationRuntimeException.class).when(productOutbound)
        .clearProductCacheByProductCodes(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
    this.productServiceWrapper.changeBrandCodeAndBrandApprovalStatusInScreeningProducts(BRAND_CODE, BRAND_CODE,
        BrandApprovalStatus.APPROVED.name(), BRAND_NAME);
    verify(this.solrReviewProductCollectionService, times(2))
        .updateBrandApprovedInReviewProductCollection(ID, Boolean.TRUE, BRAND_NAME);
    verify(this.productCollectionRepository)
        .findByBrandCodeAndBrandApprovalStatusAndMarkForDeleteFalse(BRAND_CODE, BrandApprovalStatus.DRAFT, pageable);
    verify(this.productCollectionRepository, times(2))
        .updateBrandCodeAndBrandApprovalStatus(eq(BRAND_CODE), eq(BrandApprovalStatus.APPROVED), eq(BRAND_NAME),
            Mockito.any());
    verify(productOutbound).clearProductCacheByProductCodes(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
  }

  @Test
  public void changeBrandCodeAndBrandApprovalStatusForRejectedBrandTest() throws Exception {
    productCollection.setBrandCode(BRAND_CODE);
    productCollection1.setBrandCode(BRAND_CODE);
    productCollection1.setId(ID);
    productCollection1.setProductCode(PRODUCT_CODE_1);
    when(this.productCollectionRepository
        .findByBrandCodeAndBrandApprovalStatusAndMarkForDeleteFalse(BRAND_CODE, BrandApprovalStatus.DRAFT, pageable))
        .thenReturn(new PageImpl<>(Arrays.asList(productCollection, productCollection1)));
    when(this.productCollectionRepository
        .updateBrandApprovalStatus(Mockito.any(), eq(BrandApprovalStatus.REJECTED))).thenReturn(1);
    this.productServiceWrapper.changeBrandCodeAndBrandApprovalStatusInScreeningProducts(BRAND_CODE, BRAND_CODE,
        BrandApprovalStatus.REJECTED.name(), BRAND_NAME);
    Mockito.doNothing().when(productOutbound).clearProductCacheByProductCodes(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
    verify(this.productCollectionRepository)
        .findByBrandCodeAndBrandApprovalStatusAndMarkForDeleteFalse(BRAND_CODE, BrandApprovalStatus.DRAFT, pageable);
    verify(this.productCollectionRepository, times(2))
        .updateBrandApprovalStatus(Mockito.any(), eq(BrandApprovalStatus.REJECTED));
    verify(productOutbound).clearProductCacheByProductCodes(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
  }

  @Test
  public void getScreenerNotesTest() {
    when(productCollectionRepository.getReviewerNotesByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(NOTES);
    String response = productServiceWrapper.getScreeningNotes(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).getReviewerNotesByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    assertEquals(NOTES, response);
  }


  @Test
  public void bulkUpdateActivatedProductsTest_nullProductCollection() {
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(null);
    BulkMasterProductUpdateResponse response =
        productServiceWrapper.bulkUpdateActivatedProducts(bulkMasterProductUpdateRequestDTO, STORE_ID);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    assertFalse(response.getSimpleMasterProductUpdateResponses().get(0).getUpdateSuccess());
  }

  @Test
  public void resizeImagesProductCodeTest() throws Exception {
    when(this.imageProcessorService.resizeImage(STORE_ID, PRODUCT_CODE, 0)).thenReturn(false);
    this.productServiceWrapper.resizeImages(STORE_ID, PRODUCT_CODE, false, PAGE, SIZE);
    verify(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE, 0);
  }

  @Test
  public void resizeImagesProductCodeEmptyTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      this.productServiceWrapper.resizeImages(STORE_ID, StringUtils.EMPTY, false, PAGE, SIZE);
    });
  }

  @Test
  public void resizeImagesDeltaIndexTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "batchTime", "10");
    when(
        this.productCollectionRepository.findByStoreIdAndStateInAndViewableAndImageResizedAndImageResizeRetryLessThanAndMarkForDeleteFalseAndUpdatedDateLessThanOrderByUpdatedDateDesc(
            eq(STORE_ID), eq(Arrays.asList(WorkflowStates.DRAFT.getValue(), Constants.IN_PROGRESS_STATE)), eq(false),
            eq(false), Mockito.anyInt(), Mockito.any(), eq(pageable))).thenReturn(
        new PageImpl<>(Arrays.asList(productCollection), pageable, SIZE));
    this.productServiceWrapper.resizeImages(STORE_ID, StringUtils.EMPTY, true, PAGE, SIZE);
    verify(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE, 0);
    verify(
        productCollectionRepository).findByStoreIdAndStateInAndViewableAndImageResizedAndImageResizeRetryLessThanAndMarkForDeleteFalseAndUpdatedDateLessThanOrderByUpdatedDateDesc(
        eq(STORE_ID), eq(Arrays.asList(WorkflowStates.DRAFT.getValue(), Constants.IN_PROGRESS_STATE)), eq(false),
        eq(false), Mockito.anyInt(), Mockito.any(), eq(pageable));
    verify(businessPartnerRepository).filterDetailsByBusinessPartnerCodeList(any(
      BusinessPartnerCodesRequest.class));
    verify(productService).saveProductCollection(Mockito.any());
  }

  @Test
  public void resizeImagesDeltaIndexPublishEventTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "batchTime", "10");
    when(this.productCollectionRepository
        .findByStoreIdAndStateInAndViewableAndImageResizedAndImageResizeRetryLessThanAndMarkForDeleteFalseAndUpdatedDateLessThanOrderByUpdatedDateDesc(
            eq(STORE_ID), eq(Arrays.asList(WorkflowStates.DRAFT.getValue(), Constants.IN_PROGRESS_STATE)), eq(false),
            eq(false), Mockito.anyInt(), Mockito.any(), eq(pageable)))
        .thenReturn(new PageImpl<>(Arrays.asList(productCollection), pageable, SIZE));
    profileResponse.setTrustedSeller(false);
    List<ProfileResponse> profileResponses = Collections.singletonList(profileResponse);
    when(businessPartnerRepository.filterDetailsByBusinessPartnerCodeList(any(
      BusinessPartnerCodesRequest.class))).thenReturn(profileResponses);
    when(imageProcessorService.resizeImage(STORE_ID, PRODUCT_CODE, 0)).thenReturn(true);
    this.productServiceWrapper.resizeImages(STORE_ID, StringUtils.EMPTY, true, PAGE, SIZE);
    verify(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE, 0);
    verify(productCollectionRepository)
        .findByStoreIdAndStateInAndViewableAndImageResizedAndImageResizeRetryLessThanAndMarkForDeleteFalseAndUpdatedDateLessThanOrderByUpdatedDateDesc(
            eq(STORE_ID), eq(Arrays.asList(WorkflowStates.DRAFT.getValue(), Constants.IN_PROGRESS_STATE)), eq(false),
            eq(false), Mockito.anyInt(), Mockito.any(), eq(pageable));
    verify(productService).saveProductCollection(productCollectionArgumentCaptor.capture());
    verify(productPublisherService)
        .publish(productCollection.getProductCode(), productCollection.getBusinessPartnerCode(),
            productCollection.getBusinessPartnerName(), GdnMandatoryRequestParameterUtil.getUsername(),
            productCollection.isPostLive(), productCollection.isRestrictedKeywordsPresent(),
            productCollection.getRestrictedKeywordsDetected(),productCollection.getPrioritySeller(),
          false, productCollection.getProductId(), productCollection);
    verify(businessPartnerRepository).filterDetailsByBusinessPartnerCodeList(any(
      BusinessPartnerCodesRequest.class));
  }

  @Test
  public void resizeImagesDeltaIndexPublishEvent_forTrustedSellersTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "batchTime", "10");
    when(this.productCollectionRepository
      .findByStoreIdAndStateInAndViewableAndImageResizedAndImageResizeRetryLessThanAndMarkForDeleteFalseAndUpdatedDateLessThanOrderByUpdatedDateDesc(
        eq(STORE_ID), eq(Arrays.asList(WorkflowStates.DRAFT.getValue(), Constants.IN_PROGRESS_STATE)), eq(false),
        eq(false), Mockito.anyInt(), Mockito.any(), eq(pageable)))
      .thenReturn(new PageImpl<>(Arrays.asList(productCollection), pageable, SIZE));
    profileResponse.setTrustedSeller(true);
    ProfileResponse profileResponse2 =
      ProfileResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).markForDelete(true)
        .trustedSeller(true).build();
    List<ProfileResponse> profileResponses = Arrays.asList(profileResponse2,profileResponse);
    when(businessPartnerRepository.filterDetailsByBusinessPartnerCodeList(any(
      BusinessPartnerCodesRequest.class))).thenReturn(profileResponses);
    when(imageProcessorService.resizeImage(STORE_ID, PRODUCT_CODE, 0)).thenReturn(true);
    this.productServiceWrapper.resizeImages(STORE_ID, StringUtils.EMPTY, true, PAGE, SIZE);
    verify(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE, 0);
    verify(productCollectionRepository)
      .findByStoreIdAndStateInAndViewableAndImageResizedAndImageResizeRetryLessThanAndMarkForDeleteFalseAndUpdatedDateLessThanOrderByUpdatedDateDesc(
        eq(STORE_ID), eq(Arrays.asList(WorkflowStates.DRAFT.getValue(), Constants.IN_PROGRESS_STATE)), eq(false),
        eq(false), Mockito.anyInt(), Mockito.any(), eq(pageable));
    verify(productService).saveProductCollection(productCollectionArgumentCaptor.capture());
    verify(productPublisherService)
      .publish(productCollection.getProductCode(), productCollection.getBusinessPartnerCode(),
        productCollection.getBusinessPartnerName(), GdnMandatoryRequestParameterUtil.getUsername(),
        productCollection.isPostLive(), productCollection.isRestrictedKeywordsPresent(),
        productCollection.getRestrictedKeywordsDetected(),productCollection.getPrioritySeller(),
        true, productCollection.getProductId(), productCollection);
    verify(businessPartnerRepository).filterDetailsByBusinessPartnerCodeList(any(
      BusinessPartnerCodesRequest.class));
  }

  @Test
  public void resizeImagesDeltaIndexExceptionTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "batchTime", "10");
    doThrow(Exception.class).when(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE, 0);
    when(
        this.productCollectionRepository.findByStoreIdAndStateInAndViewableAndImageResizedAndImageResizeRetryLessThanAndMarkForDeleteFalseAndUpdatedDateLessThanOrderByUpdatedDateDesc(
            eq(STORE_ID), eq(Arrays.asList(WorkflowStates.DRAFT.getValue(), Constants.IN_PROGRESS_STATE)), eq(false),
            eq(false), Mockito.anyInt(), Mockito.any(), eq(pageable))).thenReturn(
        new PageImpl<>(Arrays.asList(productCollection), pageable, SIZE));
    this.productServiceWrapper.resizeImages(STORE_ID, StringUtils.EMPTY, true, PAGE, SIZE);
    verify(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE, 0);
    verify(productCollectionRepository)
        .findByStoreIdAndStateInAndViewableAndImageResizedAndImageResizeRetryLessThanAndMarkForDeleteFalseAndUpdatedDateLessThanOrderByUpdatedDateDesc(
            eq(STORE_ID), eq(Arrays.asList(WorkflowStates.DRAFT.getValue(), Constants.IN_PROGRESS_STATE)), eq(false),
            eq(false), Mockito.anyInt(), Mockito.any(), eq(pageable));
    verify(productService).saveProductCollection(Mockito.any());
    verify(businessPartnerRepository).filterDetailsByBusinessPartnerCodeList(any(
      BusinessPartnerCodesRequest.class));
  }

  @Test
  public void bulkUpdateActivatedProductsTest_inactiveProduct() {
    productCollection.setActivated(Boolean.FALSE);
    productCollection.setViewable(Boolean.TRUE);
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setProductId(PRODUCT_ID);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    BulkMasterProductUpdateResponse response =
        productServiceWrapper.bulkUpdateActivatedProducts(bulkMasterProductUpdateRequestDTO, STORE_ID);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    assertFalse(response.getSimpleMasterProductUpdateResponses().get(0).getUpdateSuccess());
  }

  @Test
  public void bulkUpdateActivatedProducts_unviewableProduct() {
    productCollection.setActivated(Boolean.TRUE);
    productCollection.setViewable(Boolean.FALSE);
    productCollection.setProductCode(PRODUCT_CODE);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    BulkMasterProductUpdateResponse response =
        productServiceWrapper.bulkUpdateActivatedProducts(bulkMasterProductUpdateRequestDTO, STORE_ID);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    assertFalse(response.getSimpleMasterProductUpdateResponses().get(0).getUpdateSuccess());
  }

  @Test
  public void bulkUpdateActivatedProductsTest_reviewPendingProduct() throws Exception {
    SimpleMasterProductUpdateResponse simpleMasterProductUpdateResponse = new SimpleMasterProductUpdateResponse();
    simpleMasterProductUpdateResponse.setProductCode(PRODUCT_CODE);
    simpleMasterProductUpdateResponse.setUpdateSuccess(Boolean.TRUE);
    bulkMasterProductUpdateRequestDTO.getSimpleMasterProductUpdateRequestDTOS().get(0).setBrand(BRAND_NAME);
    bulkMasterProductUpdateRequestDTO.getSimpleMasterProductUpdateRequestDTOS().get(0).setName(PRODUCT_NAME_2);
    productCollection.setActivated(Boolean.TRUE);
    productCollection.setViewable(Boolean.TRUE);
    productCollection.setReviewPending(true);
    productCollection.setBrand(BRAND);
    productCollection.setProductName(PRODUCT_NAME);
    productCollection.setProductCode(PRODUCT_CODE);
    Map<Boolean, ProductCollection> productCollectionMap = new HashMap<>();
    productCollectionMap.put(Boolean.TRUE, productCollection);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productRepository.findOne(productCollection.getProductId())).thenReturn(product);
    when(productService.updateForBulk(simpleMasterProductUpdateRequestDTO, STORE_ID, productCollection))
        .thenReturn(simpleMasterProductUpdateResponse);
    when(productService.checkAndUpdateSolr(STORE_ID, simpleMasterProductUpdateRequestDTO, product, productCollection))
        .thenReturn(productCollectionMap);
    BulkMasterProductUpdateResponse response =
        productServiceWrapper.bulkUpdateActivatedProducts(bulkMasterProductUpdateRequestDTO, STORE_ID);
    Mockito.verify(productService)
        .checkAndUpdateSolr(STORE_ID, simpleMasterProductUpdateRequestDTO, product, productCollection);
    verify(productService)
        .updateForBulk(simpleMasterProductUpdateRequestDTOArgumentCaptor.capture(), Mockito.eq(STORE_ID),
            Mockito.eq(productCollection));
    verify(productService).updateSolrProductCollection(productCollection);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productRepository).findOne(productCollection.getProductId());
    assertTrue(response.getSimpleMasterProductUpdateResponses().get(0).getUpdateSuccess());
    assertEquals(PRODUCT_CODE, response.getSimpleMasterProductUpdateResponses().get(0).getProductCode());
    assertEquals(BRAND, simpleMasterProductUpdateRequestDTOArgumentCaptor.getValue().getBrand());
    assertEquals(PRODUCT_NAME, simpleMasterProductUpdateRequestDTOArgumentCaptor.getValue().getName());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPreLiveProductsTest() throws Exception {
    productCollection.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(1);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPreLiveProductsAttributeAutoFillTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "productAttributeAutoFillEnabled", true);
    productCollection.setPostLive(false);
    productCollection.setProductId(PRODUCT_ID);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE)).thenReturn(productCollection);
    when(productService.updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
        eq(true))).thenReturn(productCollection);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(1);
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any())).thenReturn(
        keywordsByFieldAndActionType);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE)).thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    when(productRepository.autoFillProductAttribute(STORE_ID, PRODUCT_CODE)).thenReturn(Arrays.asList(new AttributeHistoryResponse()));
    doNothing().when(productService).updateHistoryOnAttributeAutoFill(STORE_ID, PRODUCT_ID, Arrays.asList(new AttributeHistoryResponse()));
    Assertions.assertEquals(1, keywordsByFieldAndActionType.getAction());
  }

  @Test
  public void updateImagePathsAndSkipScreeningAlreadyImageResizeTest() throws Exception {
    productCollection.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    productCollection.setImageResized(true);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPreLiveProductsAttributeAutoFillNoUpdateTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "productAttributeAutoFillEnabled", true);
    productCollection.setPostLive(false);
    productCollection.setProductId(PRODUCT_ID);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(1);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    when(productRepository.autoFillProductAttribute(STORE_ID, PRODUCT_CODE)).thenReturn(new ArrayList<>());
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productRepository).autoFillProductAttribute(STORE_ID, PRODUCT_CODE);
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPreLiveProductsAttributeAutoFillErrorTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "productAttributeAutoFillEnabled", true);
    productCollection.setPostLive(false);
    productCollection.setProductId(PRODUCT_ID);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(1);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productRepository.autoFillProductAttribute(STORE_ID, PRODUCT_CODE)).thenThrow(ApplicationRuntimeException.class);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productRepository).autoFillProductAttribute(STORE_ID, PRODUCT_CODE);
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsNullProductDetailResponse() throws Exception {
    when(productService.findProductDetailByProductCode(bulkImageProcessResponse.getGroupCode(), false))
        .thenReturn(null);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(bulkImageProcessResponse.getGroupCode(), false);
  }

  @Test
  public void updateImagePathsAndSkipScreeningForInternalProducts() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setBusinessPartnerCode(Constants.INTERNAL);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(1);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForFlow3Products() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setBusinessPartnerCode(INTERNAL);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setBusinessPartnerCode(INTERNAL);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE))
        .thenReturn(Collections.singletonList(KEYWORD));
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForFlow3RestrictedKeywordsProducts() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setBusinessPartnerCode(INTERNAL);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setBusinessPartnerCode(INTERNAL);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE))
        .thenReturn(Collections.singletonList(KEYWORD));
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveItemAttributeEmptyProducts() throws Exception {
    productCollection.setPostLive(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColour(productDetailResponse, StringUtils.EMPTY);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE))
        .thenReturn(Collections.singletonList(KEYWORD));
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(3);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productWorkflowServiceWrapper)
        .deleteProductCollection(STORE_ID, PRODUCT_CODE, categoryRestrictedKeywordResponse.getMessage(), true, true);
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }


  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsFamilyColourEmptyTest() throws Exception {
    productCollection.setPostLive(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, StringUtils.EMPTY);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE))
        .thenReturn(Collections.singletonList(KEYWORD));
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(1);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }


  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsFamilyColourEmptyForSingleItemTest() throws Exception {
    productCollection.setPostLive(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    productDetailResponse.getProductItemResponses().iterator().next().getProductItemAttributeValueResponses().get(0)
        .setValue(StringUtils.EMPTY);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(1);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }


  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsFamilyColourNotEmptyTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productWfService).approveDraft(PRODUCT_CODE);
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(true, productCollectionArgumentCaptor.getValue().isPostLive());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }


  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsWithoutWarnaTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productWfService).approveDraft(PRODUCT_CODE);
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals(true, productCollectionArgumentCaptor.getValue().isPostLive());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsFamilyColourNotEmptyExceptionCaseTest()
      throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(DRAFT_STATE);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE))
        .thenReturn(Collections.singletonList(KEYWORD));
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection).thenReturn(null);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    when(productService.updatePostLiveStatus(productCollection, false)).thenReturn(productCollection);
    doThrow(Exception.class).when(productWfService).approveDraft(PRODUCT_CODE);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(2);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    RestrictedKeywordsByField restrictedKeywordsByField = new RestrictedKeywordsByField();
    keywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(
        Collections.singletonList(restrictedKeywordsByField));
    keywordsByFieldAndActionType.setSkipAllActions(true);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productWfService).approveDraft(PRODUCT_CODE);
    verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, Constants.DEFAULT_USERNAME, SaveHistoryConstants.REVIEW_CONFIG_CHANGE,
            REVIEW_CONFIG_CHANGE_DESCRIPTION);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).updatePostLiveStatus(productCollectionArgumentCaptor.capture(), eq(false));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsNoSkipTest()
      throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(DRAFT_STATE);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE))
        .thenReturn(Collections.singletonList(KEYWORD));
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection).thenReturn(null);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    when(productService.updatePostLiveStatus(productCollection, false)).thenReturn(productCollection);
    doThrow(Exception.class).when(productWfService).approveDraft(PRODUCT_CODE);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(2);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    RestrictedKeywordsByField restrictedKeywordsByField = new RestrictedKeywordsByField();
    keywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(
        Collections.singletonList(restrictedKeywordsByField));
    keywordsByFieldAndActionType.setSkipAllActions(false);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productNotificationService)
        .sendNotificationForProductWithRestrictedKeyword(Mockito.any(), Mockito.any(),
            Mockito.anyBoolean());
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsNewBrandTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.DRAFT);
    productCollection.setState(DRAFT_STATE);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productWfService).approveDraft(productCollection.getProductCode());
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsStateIPTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }


    @Test
    public void updateImagePathsAndSkipScreeningAlreadyImageResize_forTrustedSellersTest() throws Exception {
      productCollection.setPostLive(false);
      ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
      when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
      productCollection.setImageResized(true);
      when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      profileResponse.setTrustedSeller(true);
      when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    }

    @Test
    public void updateImagePathsAndSkipScreeningForPreLiveProductsAttributeAutoFillNoUpdate_forTrustedSellersTest() throws Exception {
      ReflectionTestUtils.setField(productServiceWrapper, "productAttributeAutoFillEnabled", true);
      productCollection.setPostLive(false);
      productCollection.setProductId(PRODUCT_ID);
      ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
      when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
      when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
      when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      profileResponse.setTrustedSeller(true);
      when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
          eq(true))).thenReturn(productCollection);
      RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
      keywordsByFieldAndActionType.setAction(1);
      when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
      when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
      when(productRepository.autoFillProductAttribute(STORE_ID, PRODUCT_CODE)).thenReturn(new ArrayList<>());
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
      verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
      verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
        anyBoolean());
      verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
      verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
      verify(productRepository).autoFillProductAttribute(STORE_ID, PRODUCT_CODE);
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
          .count());
      verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
      Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    }

    @Test
    public void updateImagePathsAndSkipScreeningForPreLiveProductsAttributeAutoFillError_forTrustedSellersTest() throws Exception {
      ReflectionTestUtils.setField(productServiceWrapper, "productAttributeAutoFillEnabled", true);
      productCollection.setPostLive(false);
      productCollection.setProductId(PRODUCT_ID);
      ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
      when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
      when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
      when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
          eq(true))).thenReturn(productCollection);
      RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
      keywordsByFieldAndActionType.setAction(1);
      when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
      when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
      when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      profileResponse.setTrustedSeller(true);
      when(productRepository.autoFillProductAttribute(STORE_ID, PRODUCT_CODE)).thenThrow(ApplicationRuntimeException.class);
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
      verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
        anyBoolean());
      verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
      verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
      verify(productRepository).autoFillProductAttribute(STORE_ID, PRODUCT_CODE);
      Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
          .count());
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
      Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    }

    @Test
    public void updateImagePathsAndSkipScreeningForPostLiveProductsNullProductDetailResponse_forTrustedSellersTest() throws Exception {
      when(productService.findProductDetailByProductCode(bulkImageProcessResponse.getGroupCode(), false))
        .thenReturn(null);
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(bulkImageProcessResponse.getGroupCode(), false);
    }

    @Test
    public void updateImagePathsAndSkipScreeningForInternalProducts_forTrustedSellersTest() throws Exception {
      productCollection.setPostLive(false);
      productCollection.setBusinessPartnerCode(Constants.INTERNAL);
      ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
      when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
      when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
      when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
          eq(true))).thenReturn(productCollection);
      RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
      keywordsByFieldAndActionType.setAction(1);
      when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
      when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
      when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      profileResponse.setTrustedSeller(true);
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
      verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
      verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
        anyBoolean());
      verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
          .count());
      verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
      Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    }

    @Test
    public void updateImagePathsAndSkipScreeningForFlow3Products_forTrustedSellersTest() throws Exception {
      productCollection.setPostLive(false);
      productCollection.setState(STATE_IN_PROGRESS);
      productCollection.setBusinessPartnerCode(INTERNAL);
      ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
      productDetailResponse.setBusinessPartnerCode(INTERNAL);
      when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
      when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE))
        .thenReturn(Collections.singletonList(KEYWORD));
      when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
      when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
          eq(true))).thenReturn(productCollection);
      RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
      when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
      when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      profileResponse.setTrustedSeller(true);
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
      verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
      verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
        anyBoolean());
      verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
      verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
          .count());
      Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    }

    @Test
    public void updateImagePathsAndSkipScreeningForFlow3RestrictedKeywordsProducts_forTrustedSellersTest() throws Exception {
      productCollection.setPostLive(false);
      productCollection.setState(STATE_IN_PROGRESS);
      productCollection.setBusinessPartnerCode(INTERNAL);
      ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
      productDetailResponse.setBusinessPartnerCode(INTERNAL);
      when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
      when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE))
        .thenReturn(Collections.singletonList(KEYWORD));
      when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
      when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
          eq(true))).thenReturn(productCollection);
      when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      profileResponse.setTrustedSeller(true);
      RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
      when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
      verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
      verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
        anyBoolean());
      verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
      verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
          .count());
      Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    }

    @Test
    public void updateImagePathsAndSkipScreeningForPostLiveItemAttributeEmptyProducts_forTrustedSellersTest() throws Exception {
      productCollection.setPostLive(true);
      ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
      setFamilyColour(productDetailResponse, StringUtils.EMPTY);
      when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
      when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE))
        .thenReturn(Collections.singletonList(KEYWORD));
      when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
      when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
          eq(true))).thenReturn(productCollection);
      when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
      when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      profileResponse.setTrustedSeller(true);
      RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
      keywordsByFieldAndActionType.setAction(3);
      keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
      when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
      when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
      verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
      verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
      verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
      verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
      Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
          .count());
      verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
        anyBoolean());
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    }


    @Test
    public void updateImagePathsAndSkipScreeningForPostLiveProductsFamilyColourEmpty_forTrustedSellersTest() throws Exception {
      productCollection.setPostLive(true);
      ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
      setFamilyColourForAllItems(productDetailResponse, StringUtils.EMPTY);
      when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
      when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE))
        .thenReturn(Collections.singletonList(KEYWORD));
      when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
      when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
          eq(true))).thenReturn(productCollection);
      when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
      when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      profileResponse.setTrustedSeller(true);
      RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
      keywordsByFieldAndActionType.setAction(1);
      when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
      verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
      verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
        anyBoolean());
      verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
      verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
      verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
          .count());
      Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    }


    @Test
    public void updateImagePathsAndSkipScreeningForPostLiveProductsFamilyColourEmptyForSingleItem_forTrustedSellersTest() throws Exception {
      productCollection.setPostLive(true);
      ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
      setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
      productDetailResponse.getProductItemResponses().iterator().next().getProductItemAttributeValueResponses().get(0)
        .setValue(StringUtils.EMPTY);
      when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
      when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
      when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
          eq(true))).thenReturn(productCollection);
      when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      profileResponse.setTrustedSeller(true);
      RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
      keywordsByFieldAndActionType.setAction(1);
      when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
      when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
      verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
      verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
      verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
        anyBoolean());
      verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
      verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
          .count());
      Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    }


    @Test
    public void updateImagePathsAndSkipScreeningForPostLiveProductsFamilyColourNotEmpty_forTrustedSellersTest() throws Exception {
      productCollection.setPostLive(true);
      productCollection.setState(DRAFT_STATE);
      productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
      ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
      setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
      when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
      when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
      RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
      when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
      when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      profileResponse.setTrustedSeller(true);
      when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
          eq(true))).thenReturn(productCollection);
      when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
      verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
      verify(productWfService).approveDraft(PRODUCT_CODE);
      verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
      verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
      verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
          .count());
      Assertions.assertEquals(true, productCollectionArgumentCaptor.getValue().isPostLive());
      Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    }


    @Test
    public void updateImagePathsAndSkipScreeningForPostLiveProductsWithoutWarna_forTrustedSellersTest() throws Exception {
      productCollection.setPostLive(true);
      productCollection.setState(DRAFT_STATE);
      productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
      ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
      productDetailResponse.setProductAttributeResponses(new ArrayList<>());
      when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
      when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
      when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      profileResponse.setTrustedSeller(true);
      RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
      when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
      when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
          eq(true))).thenReturn(productCollection);
      when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
      verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
      verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
      verify(productWfService).approveDraft(PRODUCT_CODE);
      verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
      verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
      Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
          .count());
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      Assertions.assertEquals(true, productCollectionArgumentCaptor.getValue().isPostLive());
      Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    }


    @Test
    public void updateImagePathsAndSkipScreeningForPostLiveProductsFamilyColourNotEmptyExceptionCase_forTrustedSellersTest()
      throws Exception {
      productCollection.setPostLive(true);
      productCollection.setState(DRAFT_STATE);
      ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
      setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
      when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
      when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE))
        .thenReturn(Collections.singletonList(KEYWORD));
      when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection).thenReturn(null);
      when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
          eq(true))).thenReturn(productCollection);
      when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      profileResponse.setTrustedSeller(true);
      when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
      when(productService.updatePostLiveStatus(productCollection, false)).thenReturn(productCollection);
      doThrow(Exception.class).when(productWfService).approveDraft(PRODUCT_CODE);
      RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
      keywordsByFieldAndActionType.setAction(2);
      keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
      Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
      when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
      verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
      verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
      verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
        anyBoolean());
      verify(productWfService).approveDraft(PRODUCT_CODE);
      verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, Constants.DEFAULT_USERNAME, SaveHistoryConstants.REVIEW_CONFIG_CHANGE,
          REVIEW_CONFIG_CHANGE_DESCRIPTION);
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      verify(productService).updatePostLiveStatus(productCollectionArgumentCaptor.capture(), eq(false));
      verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
      verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
      Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
          .count());
      Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    }

    @Test
    public void updateImagePathsAndSkipScreeningForPostLiveProductsNewBrand_forTrustedSellersTest() throws Exception {
      productCollection.setPostLive(true);
      productCollection.setBrandApprovalStatus(BrandApprovalStatus.DRAFT);
      productCollection.setState(DRAFT_STATE);
      ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
      when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
      when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
      when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
          eq(true))).thenReturn(productCollection);
      when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
      when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      profileResponse.setTrustedSeller(true);
      RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
      when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
      verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
      verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
      verify(productWfService).approveDraft(productCollection.getProductCode());
      verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
      Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
          .count());
      Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    }

    @Test
    public void updateImagePathsAndSkipScreeningForPostLiveProductsStateIP_forTrustedSellers() throws Exception {
      productCollection.setPostLive(true);
      productCollection.setState(STATE_IN_PROGRESS);
      ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
      when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
      when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
      when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
      profileResponse.setTrustedSeller(true);
      when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
          eq(true))).thenReturn(productCollection);
      when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
      RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
      when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
      verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
      verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
      verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
      verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
        anyBoolean());
      verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
          .count());
      Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsIPStateWithRestrictedKeywords() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(1);
    keywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(Arrays.asList(new RestrictedKeywordsByField()));
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(
        productCollection.getBusinessPartnerCode())).thenReturn(profileResponse);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productCollection.getBusinessPartnerCode());
    verify(productNotificationService)
        .sendNotificationForProductWithRestrictedKeyword(Mockito.any(), Mockito.any(),
            Mockito.anyBoolean());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsAutoRejectTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(3);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    keywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(Arrays.asList(new RestrictedKeywordsByField()));
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(
        productCollection.getBusinessPartnerCode())).thenReturn(profileResponse);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productCollection.getBusinessPartnerCode());
    verify(productNotificationService)
        .sendNotificationForProductWithRestrictedKeyword(Mockito.any(), Mockito.any(),
            Mockito.anyBoolean());
    verify(productWorkflowServiceWrapper)
        .deleteProductCollection(STORE_ID, PRODUCT_CODE, categoryRestrictedKeywordResponse.getMessage(), true, true);
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsCategoryChangeTest() throws Exception {
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<>();
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setBusinessPartnerCode(Constants.CM_MERCHANT);
    categoryRestrictedKeywordResponse.setDestinationCategory(DESTINATION_CATEGORY);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(0);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    keywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(Arrays.asList(new RestrictedKeywordsByField()));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any()))
        .thenReturn(productCollection);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    businessPartner.setActivated(true);
    businessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartners.add(businessPartner);
    when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
        .thenReturn(Collections.singletonList(businessPartner));
    doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    when(productOutbound.updateProductCategory(any(),any(),anyBoolean(),anyBoolean())).thenReturn(categorySummaryResponse);
    doNothing().when(productService).updateSolrProductCollection(any(ProductCollection.class));
    doNothing().when(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    doNothing().when(productLevel1HistoryService).saveProductHistory(any(),any(),any(),any());
    when(productBusinessPartnerService.findByStoreIdAndProductId(any(), any()))
        .thenReturn(productBusinessPartners);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository,times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any());
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productBusinessPartnerService, times(2)).findByStoreIdAndProductId(Mockito.any(),
      Mockito.any());
    verify(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    verify(productOutbound).updateProductCategory(any(),any(),anyBoolean(),anyBoolean());
    verify(productService).updateSolrProductCollection(any(ProductCollection.class));
    verify(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    verify(productCollectionRepository).save(any(ProductCollection.class));
    Mockito.verify(this.kafkaProducer).send(eq(PRODUCT_INTERNAL_HISTORY_SAVE), any(),
        any(InternalProductHistoryEventModel.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY), any(),
        any(AuditTrailListRequest.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productService).getProfileResponse(anyString());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsCategoryChangeTestForBopisCategoryUpdate() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "bopisCategoryActionOnCategoryChangeSwitch",
      true);
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<>();
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setBusinessPartnerCode(Constants.CM_MERCHANT);
    categoryRestrictedKeywordResponse.setDestinationCategory(DESTINATION_CATEGORY);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(ProductType.BOPIS.getProductType());
    productItemBusinessPartner.setProductBusinessPartnerId(PRODUCT_ID);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(true);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryCode(DESTINATION_CATEGORY);
    categoryDetailResponse.setBopisEligible(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    ProfileResponse profileResponse1 = new ProfileResponse();
    profileResponse1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    CompanyDTO companyDTO1 = new CompanyDTO();
    List<String> salesChannels= new ArrayList<>();
    salesChannels.add("BLIBLI");
    companyDTO1.setSalesChannel(salesChannels);
    profileResponse1.setCompany(companyDTO1);
    when(productService.getProfileResponse(anyString())).thenReturn(profileResponse1);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(0);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    keywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(Arrays.asList(new RestrictedKeywordsByField()));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any()))
        .thenReturn(productCollection);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    businessPartner.setActivated(true);
    businessPartner.setGdnProductSku(PRODUCT_SKU);
    businessPartner.setProductItemBusinessPartners(Collections.singletonList(productItemBusinessPartner));
    productBusinessPartners.add(businessPartner);
    when(productBusinessPartnerService
        .findByStoreIdAndProductId(Mockito.any(), Mockito.any()))
        .thenReturn(Collections.singletonList(businessPartner));
    doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    when(productOutbound.updateProductCategory(any(),any(),anyBoolean(),anyBoolean())).thenReturn(categorySummaryResponse);
    doNothing().when(productService).updateSolrProductCollection(any(ProductCollection.class));
    doNothing().when(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    doNothing().when(productLevel1HistoryService).saveProductHistory(any(),any(),any(),any());
    when(productBusinessPartnerService.findByStoreIdAndProductId(any(), any()))
        .thenReturn(productBusinessPartners);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE_NEW)).thenReturn(categoryDetailResponse);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository,times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any());
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productBusinessPartnerService, times(2)).findByStoreIdAndProductId(Mockito.any(),Mockito.any());
    verify(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    verify(productOutbound).updateProductCategory(any(),any(),anyBoolean(),anyBoolean());
    verify(productService).updateSolrProductCollection(any(ProductCollection.class));
    verify(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    verify(productCollectionRepository).save(any(ProductCollection.class));
    Mockito.verify(this.kafkaProducer).send(eq(PRODUCT_INTERNAL_HISTORY_SAVE), any(),
        any(InternalProductHistoryEventModel.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY), any(),
        any(AuditTrailListRequest.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productService).getProfileResponse(anyString());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(CATEGORY_CODE_NEW);
    Assertions.assertTrue(productItemBusinessPartner.getProductType().equals(ProductType.REGULAR.getProductType()));
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsCategoryChangeTestForBopisCategorySwitchOffUpdate() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "bopisCategoryActionOnCategoryChangeSwitch",
      false);
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<>();
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setBusinessPartnerCode(Constants.CM_MERCHANT);
    categoryRestrictedKeywordResponse.setDestinationCategory(DESTINATION_CATEGORY);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(ProductType.BOPIS.getProductType());
    productItemBusinessPartner.setProductBusinessPartnerId(PRODUCT_ID);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(true);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryCode(DESTINATION_CATEGORY);
    categoryDetailResponse.setBopisEligible(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    ProfileResponse profileResponse1 = new ProfileResponse();
    profileResponse1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    CompanyDTO companyDTO1 = new CompanyDTO();
    List<String> salesChannels= new ArrayList<>();
    salesChannels.add("BLIBLI");
    companyDTO1.setSalesChannel(salesChannels);
    profileResponse1.setCompany(companyDTO1);
    when(productService.getProfileResponse(anyString())).thenReturn(profileResponse1);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productService
      .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
        eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
      .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
      new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(0);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    keywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(Arrays.asList(new RestrictedKeywordsByField()));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    when(productLevel3Helper
      .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
      .thenReturn(keywordsByFieldAndActionType);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any()))
      .thenReturn(productCollection);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    businessPartner.setActivated(true);
    businessPartner.setGdnProductSku(PRODUCT_SKU);
    businessPartner.setProductItemBusinessPartners(Collections.singletonList(productItemBusinessPartner));
    productBusinessPartners.add(businessPartner);
    when(productBusinessPartnerService
      .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
      .thenReturn(Collections.singletonList(businessPartner));
    doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    when(productOutbound.updateProductCategory(any(),any(),anyBoolean(),anyBoolean())).thenReturn(categorySummaryResponse);
    doNothing().when(productService).updateSolrProductCollection(any(ProductCollection.class));
    doNothing().when(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    doNothing().when(productLevel1HistoryService).saveProductHistory(any(),any(),any(),any());
    when(productBusinessPartnerService.findByStoreIdAndProductId(any(), any()))
      .thenReturn(productBusinessPartners);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE_NEW)).thenReturn(categoryDetailResponse);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
      .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository,times(2))
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any());
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
      productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
      productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productBusinessPartnerService, times(2)).findByStoreIdAndProductId(Mockito.any(),
      Mockito.any());
    verify(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    verify(productOutbound).updateProductCategory(any(),any(),anyBoolean(),anyBoolean());
    verify(productService).updateSolrProductCollection(any(ProductCollection.class));
    verify(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    verify(productCollectionRepository).save(any(ProductCollection.class));
    Mockito.verify(this.kafkaProducer).send(eq(PRODUCT_INTERNAL_HISTORY_SAVE), any(),
      any(InternalProductHistoryEventModel.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY), any(),
      any(AuditTrailListRequest.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productService).getProfileResponse(anyString());
  }


  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsCategoryChangeTestForSalesChannelisNotB2B() throws Exception {
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<>();
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setBusinessPartnerCode(Constants.CM_MERCHANT);
    categoryRestrictedKeywordResponse.setDestinationCategory(DESTINATION_CATEGORY);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    ProfileResponse profileResponse1 = new ProfileResponse();
    profileResponse1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    CompanyDTO companyDTO1 = new CompanyDTO();
    List<String> salesChannels= new ArrayList<>();
    salesChannels.add("BLIBLI");
    companyDTO1.setSalesChannel(salesChannels);
    profileResponse1.setCompany(companyDTO1);
    when(productService.getProfileResponse(anyString())).thenReturn(profileResponse1);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productService
      .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
        eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
      .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
      new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(0);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    keywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(Arrays.asList(new RestrictedKeywordsByField()));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    when(productLevel3Helper
      .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
      .thenReturn(keywordsByFieldAndActionType);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any()))
      .thenReturn(productCollection);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    businessPartner.setActivated(true);
    businessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartners.add(businessPartner);
    when(productBusinessPartnerService
      .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
      .thenReturn(Collections.singletonList(businessPartner));
    doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    when(productOutbound.updateProductCategory(any(),any(),anyBoolean(),anyBoolean())).thenReturn(categorySummaryResponse);
    doNothing().when(productService).updateSolrProductCollection(any(ProductCollection.class));
    doNothing().when(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    doNothing().when(productLevel1HistoryService).saveProductHistory(any(),any(),any(),any());
    when(productBusinessPartnerService.findByStoreIdAndProductId(any(), any()))
      .thenReturn(productBusinessPartners);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
      .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository,times(2))
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any());
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
      productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
      productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productBusinessPartnerService, times(2)).findByStoreIdAndProductId(Mockito.any(),Mockito.any());
    verify(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    verify(productOutbound).updateProductCategory(any(),any(),anyBoolean(),anyBoolean());
    verify(productService).updateSolrProductCollection(any(ProductCollection.class));
    verify(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    verify(productCollectionRepository).save(any(ProductCollection.class));
    Mockito.verify(this.kafkaProducer).send(eq(PRODUCT_INTERNAL_HISTORY_SAVE), any(),
      any(InternalProductHistoryEventModel.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY), any(),
      any(AuditTrailListRequest.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productService).getProfileResponse(anyString());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsCategoryChangeTestForBopisCategoryUpdateNonBopisProduct() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "bopisCategoryActionOnCategoryChangeSwitch",
      true);
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<>();
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setBusinessPartnerCode(Constants.CM_MERCHANT);
    categoryRestrictedKeywordResponse.setDestinationCategory(DESTINATION_CATEGORY);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(ProductType.REGULAR.getProductType());
    productItemBusinessPartner.setProductBusinessPartnerId(PRODUCT_ID);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(true);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryCode(DESTINATION_CATEGORY);
    categoryDetailResponse.setBopisEligible(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    ProfileResponse profileResponse1 = new ProfileResponse();
    profileResponse1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    CompanyDTO companyDTO1 = new CompanyDTO();
    List<String> salesChannels= new ArrayList<>();
    salesChannels.add("BLIBLI");
    companyDTO1.setSalesChannel(salesChannels);
    profileResponse1.setCompany(companyDTO1);
    when(productService.getProfileResponse(anyString())).thenReturn(profileResponse1);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productService
      .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
        eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
      .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
      new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(0);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    keywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(Arrays.asList(new RestrictedKeywordsByField()));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    when(productLevel3Helper
      .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
      .thenReturn(keywordsByFieldAndActionType);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any()))
      .thenReturn(productCollection);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    businessPartner.setActivated(true);
    businessPartner.setGdnProductSku(PRODUCT_SKU);
    businessPartner.setProductItemBusinessPartners(Collections.singletonList(productItemBusinessPartner));
    productBusinessPartners.add(businessPartner);
    when(productBusinessPartnerService
      .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
      .thenReturn(Collections.singletonList(businessPartner));
    doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    when(productOutbound.updateProductCategory(any(),any(),anyBoolean(),anyBoolean())).thenReturn(categorySummaryResponse);
    doNothing().when(productService).updateSolrProductCollection(any(ProductCollection.class));
    doNothing().when(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    doNothing().when(productLevel1HistoryService).saveProductHistory(any(),any(),any(),any());
    when(productBusinessPartnerService.findByStoreIdAndProductId(any(), any()))
      .thenReturn(productBusinessPartners);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE_NEW)).thenReturn(categoryDetailResponse);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
      .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository,times(2))
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any());
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
      productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
      productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productBusinessPartnerService, times(2)).findByStoreIdAndProductId(Mockito.any(),Mockito.any());
    verify(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    verify(productOutbound).updateProductCategory(any(),any(),anyBoolean(),anyBoolean());
    verify(productService).updateSolrProductCollection(any(ProductCollection.class));
    verify(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    verify(productCollectionRepository).save(any(ProductCollection.class));
    Mockito.verify(this.kafkaProducer).send(eq(PRODUCT_INTERNAL_HISTORY_SAVE), any(),
      any(InternalProductHistoryEventModel.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY), any(),
      any(AuditTrailListRequest.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productService).getProfileResponse(anyString());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsCategoryChangeTestForBopisCategoryEligibleUpdate() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "bopisCategoryActionOnCategoryChangeSwitch",
      true);
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<>();
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setBusinessPartnerCode(Constants.CM_MERCHANT);
    categoryRestrictedKeywordResponse.setDestinationCategory(DESTINATION_CATEGORY);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(ProductType.BOPIS.getProductType());
    productItemBusinessPartner.setProductBusinessPartnerId(PRODUCT_ID);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(true);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryCode(DESTINATION_CATEGORY);
    categoryDetailResponse.setBopisEligible(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    ProfileResponse profileResponse1 = new ProfileResponse();
    profileResponse1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    CompanyDTO companyDTO1 = new CompanyDTO();
    List<String> salesChannels= new ArrayList<>();
    salesChannels.add("BLIBLI");
    companyDTO1.setSalesChannel(salesChannels);
    profileResponse1.setCompany(companyDTO1);
    when(productService.getProfileResponse(anyString())).thenReturn(profileResponse1);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productService
      .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
        eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
      .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
      new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(0);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    keywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(Arrays.asList(new RestrictedKeywordsByField()));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    when(productLevel3Helper
      .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
      .thenReturn(keywordsByFieldAndActionType);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any()))
      .thenReturn(productCollection);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    businessPartner.setActivated(true);
    businessPartner.setGdnProductSku(PRODUCT_SKU);
    businessPartner.setProductItemBusinessPartners(Collections.singletonList(productItemBusinessPartner));
    productBusinessPartners.add(businessPartner);
    when(productBusinessPartnerService
      .findByStoreIdAndProductId(Mockito.any(), Mockito.any()))
      .thenReturn(Collections.singletonList(businessPartner));
    doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    when(productOutbound.updateProductCategory(any(),any(),anyBoolean(),anyBoolean())).thenReturn(categorySummaryResponse);
    doNothing().when(productService).updateSolrProductCollection(any(ProductCollection.class));
    doNothing().when(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    doNothing().when(productLevel1HistoryService).saveProductHistory(any(),any(),any(),any());
    when(productBusinessPartnerService.findByStoreIdAndProductId(any(), any()))
      .thenReturn(productBusinessPartners);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE_NEW)).thenReturn(categoryDetailResponse);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
      .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository,times(2))
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any());
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
      productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
      productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productBusinessPartnerService, times(2)).findByStoreIdAndProductId(Mockito.any(),Mockito.any());
    verify(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    verify(productOutbound).updateProductCategory(any(),any(),anyBoolean(),anyBoolean());
    verify(productService).updateSolrProductCollection(any(ProductCollection.class));
    verify(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    verify(productCollectionRepository).save(any(ProductCollection.class));
    Mockito.verify(this.kafkaProducer).send(eq(PRODUCT_INTERNAL_HISTORY_SAVE), any(),
      any(InternalProductHistoryEventModel.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY), any(),
      any(AuditTrailListRequest.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productService).getProfileResponse(anyString());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(CATEGORY_CODE_NEW);
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsCategoryChangeTestForSalesChannelisNotB2BSizeisOne() throws Exception {
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<>();
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setBusinessPartnerCode(Constants.CM_MERCHANT);
    categoryRestrictedKeywordResponse.setDestinationCategory(DESTINATION_CATEGORY);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    ProfileResponse profileResponse1 = new ProfileResponse();
    profileResponse1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    CompanyDTO companyDTO1 = new CompanyDTO();
    List<String> salesChannels= new ArrayList<>();
    salesChannels.add("B2B");
    companyDTO1.setSalesChannel(salesChannels);
    profileResponse1.setCompany(companyDTO1);
    when(productService.getProfileResponse(anyString())).thenReturn(profileResponse1);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(0);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    keywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(Arrays.asList(new RestrictedKeywordsByField()));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any()))
        .thenReturn(productCollection);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    businessPartner.setActivated(true);
    businessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartners.add(businessPartner);
    when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
        .thenReturn(Collections.singletonList(businessPartner));
    doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    when(productOutbound.updateProductCategory(any(),any(),anyBoolean(),anyBoolean())).thenReturn(categorySummaryResponse);
    doNothing().when(productService).updateSolrProductCollection(any(ProductCollection.class));
    doNothing().when(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    doNothing().when(productLevel1HistoryService).saveProductHistory(any(),any(),any(),any());
    when(productBusinessPartnerService.findByStoreIdAndProductId(any(), any()))
        .thenReturn(productBusinessPartners);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository,times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any());
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productBusinessPartnerService, times(2)).findByStoreIdAndProductId(Mockito.any(),
      Mockito.any());
    verify(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    verify(productOutbound).updateProductCategory(any(),any(),anyBoolean(),anyBoolean());
    verify(productService).updateSolrProductCollection(any(ProductCollection.class));
    verify(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    verify(productCollectionRepository).save(any(ProductCollection.class));
    Mockito.verify(this.kafkaProducer).send(eq(PRODUCT_INTERNAL_HISTORY_SAVE), any(),
        any(InternalProductHistoryEventModel.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY), any(),
        any(AuditTrailListRequest.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productService).getProfileResponse(anyString());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsCategoryChangeTestForSalesChannelistBothSizeANDSales()
      throws Exception {
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<>();
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setBusinessPartnerCode(Constants.CM_MERCHANT);
    categoryRestrictedKeywordResponse.setDestinationCategory(DESTINATION_CATEGORY);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    ProfileResponse profileResponse1 = new ProfileResponse();
    profileResponse1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    CompanyDTO companyDTO1 = new CompanyDTO();
    List<String> salesChannels = new ArrayList<>();
    salesChannels.add("b2c");
    salesChannels.add("l2c");
    companyDTO1.setSalesChannel(salesChannels);
    profileResponse1.setCompany(companyDTO1);
    when(productService.getProfileResponse(anyString())).thenReturn(profileResponse1);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productService.updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class),
        eq(productCollection), eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE)).thenReturn(
        new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(0);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    keywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(
        Arrays.asList(new RestrictedKeywordsByField()));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(
        Mockito.any(ProductDetailResponse.class), Mockito.any())).thenReturn(keywordsByFieldAndActionType);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any())).thenReturn(
        productCollection);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    businessPartner.setActivated(true);
    businessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartners.add(businessPartner);
    when(productBusinessPartnerService.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
        Mockito.any())).thenReturn(Collections.singletonList(businessPartner));
    doNothing().when(productBusinessPartnerService)
        .saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    when(productOutbound.updateProductCategory(any(), any(), anyBoolean(), anyBoolean())).thenReturn(
        categorySummaryResponse);
    doNothing().when(productService).updateSolrProductCollection(any(ProductCollection.class));
    doNothing().when(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    doNothing().when(productLevel1HistoryService).saveProductHistory(any(), any(), any(), any());
    when(productBusinessPartnerService.findByStoreIdAndProductId(any(), any())).thenReturn(productBusinessPartners);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(
        Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2)).findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any());
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productBusinessPartnerService, times(2)).findByStoreIdAndProductId(Mockito.any(),
      Mockito.any());
    verify(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    verify(productOutbound).updateProductCategory(any(), any(), anyBoolean(), anyBoolean());
    verify(productService).updateSolrProductCollection(any(ProductCollection.class));
    verify(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    verify(productCollectionRepository).save(any(ProductCollection.class));
    Mockito.verify(this.kafkaProducer)
        .send(eq(PRODUCT_INTERNAL_HISTORY_SAVE), any(), any(InternalProductHistoryEventModel.class));
    Mockito.verify(this.kafkaProducer)
        .send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY), any(), any(AuditTrailListRequest.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productService).getProfileResponse(anyString());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsCategoryChangeResponseNullTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    categoryRestrictedKeywordResponse.setDestinationCategory(DESTINATION_CATEGORY);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(0);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    keywordsByFieldAndActionType
        .setRestrictedKeywordsByFieldList(Arrays.asList(new RestrictedKeywordsByField()));
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class),
            Mockito.any())).thenReturn(keywordsByFieldAndActionType);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any()))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    when(productOutbound.updateProductCategory(any(), any(), anyBoolean(),anyBoolean())).thenReturn(null);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(
        Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2)).findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any());
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productOutbound).updateProductCategory(any(), any(), anyBoolean(),anyBoolean());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productNotificationService)
        .sendNotificationForProductWithRestrictedKeyword(Mockito.any(), Mockito.any(),
            Mockito.anyBoolean());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productCollectionRepository).saveAndFlush(any(ProductCollection.class));
    Mockito.verify(productService).getProfileResponse(anyString());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsCategoryChangeProductCollectionNullTest()
      throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    categoryRestrictedKeywordResponse.setDestinationCategory(DESTINATION_CATEGORY);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(0);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    keywordsByFieldAndActionType
        .setRestrictedKeywordsByFieldList(Arrays.asList(new RestrictedKeywordsByField()));
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class),
            Mockito.any())).thenReturn(keywordsByFieldAndActionType);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(bulkImageProcessResponse.getStoreId(),
            bulkImageProcessResponse.getGroupCode())).thenReturn(productCollection).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      });
    } finally {
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productLevel3Helper)
          .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class),
              Mockito.any());
      verify(productCollectionRepository, times(2))
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any());
      verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      verify(productService)
          .updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
              productCollectionArgumentCaptor.capture(), eq(true));
      verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    }
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsCategoryChangeWithSameCategoryTest()
      throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    categoryRestrictedKeywordResponse.setDestinationCategory(productCollection.getCategoryCode());
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(0);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    keywordsByFieldAndActionType
        .setRestrictedKeywordsByFieldList(Arrays.asList(new RestrictedKeywordsByField()));
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class),
            Mockito.any())).thenReturn(keywordsByFieldAndActionType);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(bulkImageProcessResponse.getStoreId(),
            bulkImageProcessResponse.getGroupCode())).thenReturn(productCollection);
    doNothing().when(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    when(productCollectionRepository.save(productCollection)).thenReturn(productCollection);
    doNothing().when(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
        anyBoolean());
    try {
      productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    } finally {
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productLevel3Helper)
          .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class),
              Mockito.any());
      verify(productCollectionRepository, times(1))
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any());
      verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
      verify(productService)
          .updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
              productCollectionArgumentCaptor.capture(), eq(true));
      verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
      verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
          productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
          anyBoolean());
      verify(productCollectionRepository).save(productCollection);
    }
  }
  @Test
  public void updateImagePathsAndSkipScreeningForInternalProductsCategoryChange() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setBusinessPartnerCode(Constants.INTERNAL);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(0);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    keywordsByFieldAndActionType
        .setRestrictedKeywordsByFieldList(Arrays.asList(new RestrictedKeywordsByField()));
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }
  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsAutoNeedRevision() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(2);
    keywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    keywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(Arrays.asList(new RestrictedKeywordsByField()));
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(
        productCollection.getBusinessPartnerCode())).thenReturn(profileResponse);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productCollection.getBusinessPartnerCode());
    verify(productNotificationService)
        .sendNotificationForProductWithRestrictedKeyword(Mockito.any(), Mockito.any(),
            Mockito.anyBoolean());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForSkipAllActions() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(STATE_IN_PROGRESS);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    restrictedKeywordsByFieldAndActionType.setAction(2);
    restrictedKeywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(
        Arrays.asList(new RestrictedKeywordsByField()));
    restrictedKeywordsByFieldAndActionType.setSkipAllActions(true);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(
        productCollection.getBusinessPartnerCode())).thenReturn(profileResponse);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
        anyBoolean());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productCollection.getBusinessPartnerCode());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPreLiveProductsNewBrandTest() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.DRAFT);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(1);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true));
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsAndFlagOnResizingImageFailure() throws Exception {
    productServiceWrapper.updateImagePathsAndFlagOnResizingImageFailure(bulkImageProcessResponse);
    Mockito.verify(productService).updateImagePathsAndFlagOnResizingImageFailure(bulkImageProcessResponse);
  }

  private ProductDetailResponse getProductDetailResponseWithImages() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductCode(PRODUCT_CODE);
    productDetailResponse.setName(PRODUCT_NAME);
    productDetailResponse.setUniqueSellingPoint(USP);
    productDetailResponse.setDescription(DESCRIPTION.getBytes());
    productDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productDetailResponse.setCategoryCodes(Collections.singletonList(CATEGORY_CODE));

    Image image = new Image();
    image.setLocationPath(IMAGE_LOCATION_PATH_1);
    image.setHashCode(IMAGE_HASH_CODE_1);
    image.setOriginalImage(true);
    Image image1 = new Image();
    image1.setLocationPath(IMAGE_LOCATION_PATH_2);
    image1.setHashCode(IMAGE_HASH_CODE_2);
    image1.setOriginalImage(true);
    Image image2 = new Image();
    image2.setLocationPath(IMAGE_LOCATION_PATH_3);
    image2.setHashCode(IMAGE_HASH_CODE_3);
    image2.setOriginalImage(true);
    Image image3 = new Image();
    image3.setLocationPath(IMAGE_LOCATION_PATH_4);
    image3.setHashCode(IMAGE_HASH_CODE_4);
    image3.setOriginalImage(true);
    Image image4 = new Image();
    image4.setLocationPath(IMAGE_LOCATION_PATH_5);
    image4.setHashCode(IMAGE_HASH_CODE_5);
    image4.setOriginalImage(true);
    image4.setMarkForDelete(true);
    productDetailResponse.setImages(Arrays.asList(image, image1, image2, image3, image4));
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setImages(Arrays.asList(image, image1));
    productItemResponse.setId(ITEM_SKU);
    productItemResponse.setGeneratedItemName("name1");
    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setImages(Arrays.asList(image2, image1));
    productItemResponse1.setId(ITEM_SKU_1);
    productItemResponse1.setGeneratedItemName("name2");
    productDetailResponse
        .setProductItemResponses(new HashSet<>(Arrays.asList(productItemResponse, productItemResponse1)));
    return productDetailResponse;
  }

  private ProductDetailResponse getProductDetailResponseWithImagesAddItemImage() {
    Image image1 = new Image();
    image1.setLocationPath(IMAGE_LOCATION_PATH_4);
    image1.setHashCode(IMAGE_HASH_CODE_4);
    image1.setOriginalImage(true);
    Image image2 = new Image();
    image2.setLocationPath(IMAGE_LOCATION_PATH_5);
    image2.setHashCode(IMAGE_HASH_CODE_5);
    image2.setOriginalImage(true);
    image2.setMarkForDelete(true);
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(UUID.randomUUID().toString());
    productItemResponse.setSkuCode(SKU_CODE);
    productItemResponse.setImages(Arrays.asList(image1, image2));
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setProductItemResponses(new HashSet<>(Arrays.asList(productItemResponse)));
    return productDetailResponse;
  }

  private ProductDetailResponse getProductDetailResponseWithResizedImages() {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    Image image1 = new Image();
    image1.setLocationPath(IMAGE_LOCATION_PATH_1);
    image1.setHashCode(RESIZED_IMAGE_HASH_CODE_1);
    Image image2 = new Image();
    image2.setLocationPath(IMAGE_LOCATION_PATH_3);
    image2.setHashCode(RESIZED_IMAGE_HASH_CODE_3);
    List<Image> images = productDetailResponse.getImages();
    List<Image> newImages = new ArrayList<>();
    for (Image img : images) {
      newImages.add(img);
    }
    newImages.add(image1);
    newImages.add(image2);
    productDetailResponse.setImages(newImages);
    return productDetailResponse;
  }

  private ProductDetailResponse setFamilyColour(ProductDetailResponse productDetailResponse, String value) {
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setProductAttributeName(WARNA);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeType(String.valueOf(AttributeType.DEFINING_ATTRIBUTE));
    productAttributeResponse.setAttribute(attributeResponse1);
    productDetailResponse.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    productItemAttributeValueResponse.setValue(value);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setName(FAMILY_COLOUR);
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productDetailResponse.getProductItemResponses().iterator().next()
        .setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse));
    return productDetailResponse;
  }

  private ProductDetailResponse setFamilyColourForAllItems(ProductDetailResponse productDetailResponse, String value) {
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setProductAttributeName(WARNA);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeType(String.valueOf(AttributeType.DEFINING_ATTRIBUTE));
    productAttributeResponse.setAttribute(attributeResponse1);
    productDetailResponse.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    Iterator<ProductItemResponse> iterator = productDetailResponse.getProductItemResponses().iterator();
    while (iterator.hasNext()) {
      ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
      productItemAttributeValueResponse.setValue(value);
      AttributeResponse attributeResponse = new AttributeResponse();
      attributeResponse.setName(FAMILY_COLOUR);
      productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
      iterator.next().setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse));
    }
    return productDetailResponse;
  }

  @Test
  public void updateProductCategoryTest() throws Exception {
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    productCollection.setViewable(true);
    productCollection.setActivated(true);
    productCollection.setReviewPending(false);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productOutbound.updateProductCategory(PRODUCT_CODE, CATEGORY_CODE, true,true))
        .thenReturn(categorySummaryResponse);
    productServiceWrapper.updateProductCategory(STORE_ID, PRODUCT_CODE, CATEGORY_CODE, true);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productOutbound).updateProductCategory(PRODUCT_CODE, CATEGORY_CODE, true,true);
    Mockito.verify(productCollectionRepository).save(productCollectionArgumentCaptor.capture());
    Mockito.verify(productService).updateSolrProductCollection(productCollectionArgumentCaptorNew.capture());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(productCollection.getProductCode(), USER_NAME, ProductWorkflowLookup.STATE_EDIT_DESCRIPTION,
            HISTORY_NOTES);
    Assertions.assertEquals(CATEGORY_CODE_NEW, productCollectionArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME_NEW, productCollectionArgumentCaptor.getValue().getCategoryName());
    Assertions.assertEquals(CATEGORY_CODE_NEW, productCollectionArgumentCaptorNew.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME_NEW, productCollectionArgumentCaptorNew.getValue().getCategoryName());
    Mockito.verify(productService).getProfileResponse(anyString());
  }

  @Test
  public void updateProductCategoryProductControllerExceptionTest() throws Exception {
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    productCollection.setViewable(true);
    productCollection.setActivated(true);
    productCollection.setReviewPending(true);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    try {
      Assertions.assertThrows(Exception.class, () -> {
        productServiceWrapper.updateProductCategory(STORE_ID, PRODUCT_CODE, CATEGORY_CODE, true);
      });
    } finally {
      Mockito.verify(productCollectionRepository)
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void updateProductCategoryProductControllerActivatedFalseTest() throws Exception {
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    productCollection.setViewable(true);
    productCollection.setActivated(false);
    productCollection.setReviewPending(true);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    try {
      Assertions.assertThrows(Exception.class, () -> {
        productServiceWrapper.updateProductCategory(STORE_ID, PRODUCT_CODE, CATEGORY_CODE, true);
      });
    } finally {
      Mockito.verify(productCollectionRepository)
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void updateProductCategoryProductControllerViewableFalseTest() throws Exception {
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    productCollection.setViewable(false);
    productCollection.setActivated(true);
    productCollection.setReviewPending(true);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    try {
      Assertions.assertThrows(Exception.class, () -> {
        productServiceWrapper.updateProductCategory(STORE_ID, PRODUCT_CODE, CATEGORY_CODE, true);
      });
    } finally {
      Mockito.verify(productCollectionRepository)
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void updateProductCategoryCatgeoryExceptionTest() throws Exception {
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    productCollection.setViewable(true);
    productCollection.setActivated(true);
    productCollection.setReviewPending(false);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productOutbound.updateProductCategory(PRODUCT_CODE, CATEGORY_CODE, true,true)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productServiceWrapper.updateProductCategory(STORE_ID, PRODUCT_CODE, CATEGORY_CODE, true);
      });
    } finally {
      Mockito.verify(productCollectionRepository)
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      Mockito.verify(productOutbound).updateProductCategory(PRODUCT_CODE, CATEGORY_CODE, true,true);
      Mockito.verify(productService).getProfileResponse(anyString());

    }
  }

  @Test
  public void updateProductCategoryCatgeoryProductCollectionNullExceptionTest() throws Exception {
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    productCollection.setViewable(true);
    productCollection.setActivated(true);
    productCollection.setReviewPending(false);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productServiceWrapper.updateProductCategory(STORE_ID, PRODUCT_CODE, CATEGORY_CODE, true);
      });
    } finally {
      Mockito.verify(productCollectionRepository)
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void updatePostLiveFlagAndSkipScreeningTest() throws Exception {
    when(productService.updatePostLiveStatus(productCollection, true)).thenReturn(productCollection);
    productServiceWrapper.updatePostLiveFlagAndSkipScreening(productCollection);
    Mockito.verify(productService).updatePostLiveStatus(productCollectionArgumentCaptor.capture(), eq(true));
    Mockito.verify(productWfService).approveDraft(productCollection.getProductCode());
    Mockito.verify(solrReviewProductCollectionService)
        .deleteProductFromReviewProductCollection(productCollection.getId());
    Assertions.assertEquals(productCollection.getProductCode(), productCollectionArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void updatePostLiveFlagAndSkipScreeningExceptionTest() throws Exception {
    when(productService.updatePostLiveStatus(productCollection, true)).thenReturn(productCollection);
    when(productService.updatePostLiveStatus(productCollection, false)).thenReturn(productCollection);
    doThrow(Exception.class).when(productWfService).approveDraft(productCollection.getProductCode());
    productServiceWrapper.updatePostLiveFlagAndSkipScreening(productCollection);
    verify(productService, times(2))
        .updatePostLiveStatus(productCollectionArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    verify(productWfService).approveDraft(productCollection.getProductCode());
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productLevel1HistoryService)
        .saveProductHistory(productCollection.getProductCode(), Constants.DEFAULT_USERNAME,
            SaveHistoryConstants.REVIEW_CONFIG_CHANGE, REVIEW_CONFIG_CHANGE_DESCRIPTION);
    Assertions.assertEquals(productCollection.getProductCode(), productCollectionArgumentCaptor.getValue().getProductCode());
    Assertions.assertTrue(booleanArgumentCaptor.getAllValues().get(0));
    Assertions.assertFalse(booleanArgumentCaptor.getAllValues().get(1));
  }

  @Test
  public void checkIfProductIsEligibleForScreeningSkipTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    productCollection.setImageResized(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productLevel3Helper
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(new ArrayList<>());
    boolean response = productServiceWrapper.checkIfProductIsEligibleForScreeningSkip(productCollection);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    assertTrue(response);
  }

  @Test
  public void checkIfProductIsEligibleForScreeningSkipEmptyFamilyColourTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, StringUtils.EMPTY);
    productCollection.setImageResized(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productLevel3Helper
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(new ArrayList<>());
    boolean response = productServiceWrapper.checkIfProductIsEligibleForScreeningSkip(productCollection);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());;
    assertTrue(response);
  }

  @Test
  public void checkIfProductIsEligibleForScreeningSkipWithoutWarnaTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, StringUtils.EMPTY);
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    productCollection.setImageResized(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productLevel3Helper
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(new ArrayList<>());
    boolean response = productServiceWrapper.checkIfProductIsEligibleForScreeningSkip(productCollection);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    assertTrue(response);
  }

  @Test
  public void checkIfProductIsEligibleForScreeningSkipNotResizedTest() throws Exception {
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    boolean response = productServiceWrapper.checkIfProductIsEligibleForScreeningSkip(productCollection);
    assertFalse(response);
  }

  @Test
  public void checkIfProductIsEligibleForScreeningSkipBrandNotActiveTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productCollection.setImageResized(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.DRAFT);
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    productCollection.setImageResized(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productLevel3Helper
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(new ArrayList<>());
    boolean response = productServiceWrapper.checkIfProductIsEligibleForScreeningSkip(productCollection);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    assertTrue(response);
  }

  @Test
  public void checkIfProductIsEligibleForScreeningSkipAssignedTest() throws Exception {
    productCollection.setImageResized(true);
    productCollection.setAssignedTo(BrandApprovalStatus.REJECTED.name());
    boolean response = productServiceWrapper.checkIfProductIsEligibleForScreeningSkip(productCollection);
    assertFalse(response);
  }

  @Test
  public void checkIfProductIsEligibleForScreeningSkipRestrictedKeywordTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    productCollection.setImageResized(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    boolean response = productServiceWrapper.checkIfProductIsEligibleForScreeningSkip(productCollection);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    assertFalse(response);
  }

  @Test
  public void checkIfProductIsEligibleForScreeningSkipRestrictedKeywordFalseTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    productCollection.setImageResized(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productLevel3Helper
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(new ArrayList<>());
    boolean response = productServiceWrapper.checkIfProductIsEligibleForScreeningSkip(productCollection);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    assertTrue(response);
  }

  @Test
  public void checkIfProductIsEligibleForScreeningSkipRestrictedKeywordUSPTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    productCollection.setImageResized(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    boolean response = productServiceWrapper.checkIfProductIsEligibleForScreeningSkip(productCollection);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    assertFalse(response);
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPostLiveProductsNotificationTest() throws Exception {
    productCollection1.setPostLive(false);
    productCollection1.setProductCode(PRODUCT_CODE);
    List<String> restrictedKeywordList = Arrays.asList(KEYWORD, USP, NOTES);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    RestrictedKeywordsByFieldAndActionType keywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    keywordsByFieldAndActionType.setAction(1);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    Mockito.when(
            productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(), Mockito.any()))
        .thenReturn(keywordsByFieldAndActionType);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE)).thenReturn(restrictedKeywordList);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection1);
    when(productService.updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class),
        eq(productCollection1), Mockito.eq(true))).thenReturn(productCollection1);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(false, false));
    doNothing().when(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    verify(productService).updateSolrOrPublishEvent(any(ProductCollection.class), anyBoolean());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    Assertions.assertTrue(booleanArgumentCaptor.getValue());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPreLiveImageQcRestrictedCategoryProducts() throws Exception {
    productCollection.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(false))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(true, true));
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(false));
    verify(productService).publishImageQcEvent(bulkImageProcessResponse, productDetailResponse,
        new RestrictedKeywordsByFieldAndActionType());
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
    Assertions.assertEquals(1, productCollection.getImageQcState());
  }

  @Test
  public void updateImagePathsAndSkipScreeningForPreLiveImageQcProducts() throws Exception {
    productCollection.setPostLive(false);
    productSystemParameterCategoryResponse.setValue(CATEGORY_CODE_NEW);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService
        .updateImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(false))).thenReturn(productCollection);
    when(productService.getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new ImageQcEnableAndSyncResponse(true, true));
    productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(false));
    verify(productService).publishImageQcEvent(bulkImageProcessResponse, productDetailResponse,
        new RestrictedKeywordsByFieldAndActionType());
    verify(productService).getImageQcStatus(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productCollectionRepository).saveAndFlush(Mockito.any(ProductCollection.class));
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void checkIfProductIsEligibleForScreeningSkipPhoneNumberTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setName(NAME_WITH_PHONE_NUMBER);
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    productCollection.setImageResized(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    boolean response = productServiceWrapper.checkIfProductIsEligibleForScreeningSkip(productCollection);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    assertFalse(response);
  }

  @Test
  public void processImageQcResponsePostLiveProductTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap,
      productCollection, productDetailResponse, false, profileResponse)).thenReturn(
      new AutoNeedRevisionAndForceReviewResponse(false, false, false, null));
    when(productLevel3Helper
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(new ArrayList<>());
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).processImageQcResponse(eq(STORE_ID), eq(imageQcResponseDomainEvent),
        stringIntegerMapArgumentCaptor.capture(), eq(productCollection), eq(productDetailResponse),
      eq(false), eq(profileResponse));
    verify(productWfService).approveDraft(PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals(1, stringIntegerMapArgumentCaptor.getValue().get(IMAGE_HASH_CODE_1), 0);
    Assertions.assertEquals(2, stringIntegerMapArgumentCaptor.getValue().get(IMAGE_HASH_CODE_2), 0);
    Assertions.assertEquals(1, stringIntegerMapArgumentCaptor.getValue().get(IMAGE_HASH_CODE_3), 0);
  }

  @Test
  public void processImageQcResponsePostLiveProductAutoNeedRevisionTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollection.setReviewPending(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse)).thenReturn(new AutoNeedRevisionAndForceReviewResponse(true,
      false, false, null));
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productLevel3Helper
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(new ArrayList<>());
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).processImageQcResponse(eq(STORE_ID), eq(imageQcResponseDomainEvent),
        stringIntegerMapArgumentCaptor.capture(), eq(productCollection), eq(productDetailResponse),
      eq(false), eq(profileResponse));
    verify(productWfService).approveDraft(PRODUCT_CODE);
    verify(productService).publishAutoNeedRevisionEvent(STORE_ID, PRODUCT_CODE, null, false, StringUtils.EMPTY);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals(1, stringIntegerMapArgumentCaptor.getValue().get(IMAGE_HASH_CODE_1), 0);
    Assertions.assertEquals(2, stringIntegerMapArgumentCaptor.getValue().get(IMAGE_HASH_CODE_2), 0);
    Assertions.assertEquals(1, stringIntegerMapArgumentCaptor.getValue().get(IMAGE_HASH_CODE_3), 0);
  }

  @Test
  public void processImageQcResponsePostLiveProductAutoNeedRevision_ForTrustedSellersTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
      productDetailResponse, true, profileResponse)).thenReturn(new AutoNeedRevisionAndForceReviewResponse(true,
      false, false, null));
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(true);
    when(productLevel3Helper
      .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
      .thenReturn(new ArrayList<>());
    productCollection.setImageResized(true);
    productCollection.setReviewPending(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
      .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).processImageQcResponse(eq(STORE_ID), eq(imageQcResponseDomainEvent),
      stringIntegerMapArgumentCaptor.capture(), eq(productCollection), eq(productDetailResponse),
      eq(true), eq(profileResponse));
    verify(productWfService).approveDraft(PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).publishAutoNeedRevisionEvent(STORE_ID, PRODUCT_CODE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(1, stringIntegerMapArgumentCaptor.getValue().get(IMAGE_HASH_CODE_1), 0);
    Assertions.assertEquals(2, stringIntegerMapArgumentCaptor.getValue().get(IMAGE_HASH_CODE_2), 0);
    Assertions.assertEquals(1, stringIntegerMapArgumentCaptor.getValue().get(IMAGE_HASH_CODE_3), 0);
  }


  @Test
  public void processImageQcResponsePostLiveProductAutoNeedRevision2ndTimeTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollection.setAutoNeedRevision(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse)).thenReturn(new AutoNeedRevisionAndForceReviewResponse(true,
      false, false, null));
    when(productLevel3Helper
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(new ArrayList<>());
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).processImageQcResponse(eq(STORE_ID), eq(imageQcResponseDomainEvent),
        stringIntegerMapArgumentCaptor.capture(), eq(productCollection), eq(productDetailResponse),
      eq(false), eq(profileResponse));
    verify(businessPartnerRepository)
      .filterDetailByBusinessPartnerCode(any());
    verify(productWfService).approveDraft(PRODUCT_CODE);
    Assertions.assertEquals(1, stringIntegerMapArgumentCaptor.getValue().get(IMAGE_HASH_CODE_1), 0);
    Assertions.assertEquals(2, stringIntegerMapArgumentCaptor.getValue().get(IMAGE_HASH_CODE_2), 0);
    Assertions.assertEquals(1, stringIntegerMapArgumentCaptor.getValue().get(IMAGE_HASH_CODE_3), 0);
  }

  @Test
  public void processImageQcResponsePostLiveExceptionWhileApprovingTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    Image imageWithoutHashCode = new Image();
    imageWithoutHashCode.setLocationPath(IMAGE_LOCATION_PATH_3);
    imageWithoutHashCode.setOriginalImage(true);
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setImages(Arrays.asList(imageWithoutHashCode));
    productItemResponse.setId(ITEM_SKU);
    productItemResponse.setGeneratedItemName("name1");
    productDetailResponse.getProductItemResponses().add(productItemResponse);
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE))
        .thenReturn(Collections.singletonList(KEYWORD));
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse))
        .thenReturn(new AutoNeedRevisionAndForceReviewResponse(false, false, false, null));
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    when(productService.updatePostLiveStatus(productCollection, false)).thenReturn(productCollection);
    when(productLevel3Helper
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(new ArrayList<>());
    doThrow(Exception.class).when(productWfService).approveDraft(PRODUCT_CODE);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
        .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
            productDetailResponse, false, profileResponse);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productWfService).approveDraft(PRODUCT_CODE);
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productService).updatePostLiveStatus(productCollectionArgumentCaptor.capture(), eq(false));
    verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, Constants.DEFAULT_USERNAME, SaveHistoryConstants.REVIEW_CONFIG_CHANGE,
            REVIEW_CONFIG_CHANGE_DESCRIPTION);
    Assertions.assertTrue(productCollectionArgumentCaptor.getAllValues().get(0).isPostLive());
    Assertions.assertFalse(productCollectionArgumentCaptor.getAllValues().get(0).isReviewPending());
    Assertions.assertFalse(productCollectionArgumentCaptor.getAllValues().get(1).isReviewPending());
    Assertions.assertFalse(productCollectionArgumentCaptor.getAllValues().get(1).isReviewPending());
  }

  @Test
  public void processImageQcResponseRestrictedKeywordTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse))
        .thenReturn(new AutoNeedRevisionAndForceReviewResponse(false, false, false, null));
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
        .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
            productDetailResponse, false, profileResponse);
    verify(productService).updatePostLiveStatus(productCollectionArgumentCaptor.capture(), eq(false));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productNotificationService)
        .sendNotificationForProductWithRestrictedKeyword(BUSINESS_PARTNER_CODE, PRODUCT_NAME,
            profileResponse.getCompany().isInternationalFlag());
    Assertions.assertFalse(productCollectionArgumentCaptor.getValue().isReviewPending());
  }

  @Test
  public void processImageQcResponseRestrictedKeyword_ForTrustedSellersTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(true);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
      productDetailResponse, true, profileResponse))
      .thenReturn(new AutoNeedRevisionAndForceReviewResponse(false, false, false, null));
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
      .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
      .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, true, profileResponse);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).updatePostLiveStatus(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productNotificationService)
      .sendNotificationForProductWithRestrictedKeyword(BUSINESS_PARTNER_CODE, PRODUCT_NAME,
        profileResponse.getCompany().isInternationalFlag());
    Assertions.assertFalse(productCollectionArgumentCaptor.getValue().isReviewPending());
  }

  @Test
  public void processImageQcResponseImageViolationTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse))
        .thenReturn(new AutoNeedRevisionAndForceReviewResponse(false, true, true, null));
    when(productLevel3Helper
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(new ArrayList<>());
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
        .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
            productDetailResponse, false, profileResponse);
    verify(productService).updatePostLiveStatus(productCollectionArgumentCaptor.capture(), eq(false));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(productCollectionArgumentCaptor.getValue().isReviewPending());
  }

  @Test
  public void processImageQcResponseImageViolation_ForTrustedSellerTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
      productDetailResponse, false, profileResponse))
      .thenReturn(new AutoNeedRevisionAndForceReviewResponse(false, true, true, null));
    when(productLevel3Helper
      .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
      .thenReturn(new ArrayList<>());
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
      .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
      .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse);
    verify(productService).updatePostLiveStatus(productCollectionArgumentCaptor.capture(), eq(false));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(productCollectionArgumentCaptor.getValue().isReviewPending());
  }

  @Test
  public void processImageQcResponsePublishNeedRevisionEventTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "publishAutoNeedRevisionEvent", false);
    productCollection.setPostLive(false);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse))
        .thenReturn(
            new AutoNeedRevisionAndForceReviewResponse(true, false, false, Collections.singleton(PREDICTION_NAME_1)));
    when(productLevel3Helper
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(new ArrayList<>());
    productCollection.setImageResized(true);
    productCollection.setReviewPending(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
        .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
            productDetailResponse, false, profileResponse);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).updatePostLiveStatus(productCollectionArgumentCaptor.capture(), eq(false));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    Assertions.assertTrue(productCollectionArgumentCaptor.getValue().isReviewPending());
  }

  @Test
  public void processImageQcResponsePublishNeedRevisionEvent_ForTrustedSellersTest() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(true);
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
      productDetailResponse, true, profileResponse))
      .thenReturn(
        new AutoNeedRevisionAndForceReviewResponse(true, false, false, Collections.singleton(PREDICTION_NAME_1)));
    when(productLevel3Helper
      .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
      .thenReturn(new ArrayList<>());
    productCollection.setImageResized(true);
    productCollection.setReviewPending(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
      .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
      .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, true, profileResponse);
    verify(productService).updatePostLiveStatus(productCollectionArgumentCaptor.capture(),
      eq(true));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(productService).publishAutoNeedRevisionEvent(STORE_ID, PRODUCT_CODE, Collections.singleton(PREDICTION_NAME_1),
        false, StringUtils.EMPTY);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertTrue(productCollectionArgumentCaptor.getValue().isReviewPending());
  }

  @Test
  public void processImageQcResponseAutoNeedRevisionResubmitProduct_ForTrustedSellersTest() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollection.setAutoNeedRevision(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(true);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
      productDetailResponse, true, profileResponse)).thenReturn(
      new AutoNeedRevisionAndForceReviewResponse(true, false, false, Collections.singleton(PREDICTION_NAME_1)));
    when(productLevel3Helper
      .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
      .thenReturn(new ArrayList<>());
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
      .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
      .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, true, profileResponse);
    verify(productService).updatePostLiveStatus(productCollectionArgumentCaptor.capture(),
      eq(true));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertFalse(productCollectionArgumentCaptor.getValue().isReviewPending());
  }

  @Test
  public void processImageQcResponse_nonDraftFor_trustedSellers() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setReviewPending(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(true);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
      productDetailResponse, true, profileResponse)).thenReturn(new AutoNeedRevisionAndForceReviewResponse(false,
      true, true, null));
    when(productService.isForceReview(STORE_ID, PRODUCT_CODE)).thenReturn(false);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository, times(2))
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
      .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, true, profileResponse);
    verify(productService)
      .publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE, productCollection, productDetailResponse,
        new AutoNeedRevisionAndForceReviewResponse(false, true, true, null), profileResponse,
          imageQcResponseDomainEvent);
    verify(productService).isForceReview(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
  }


  @Test
  public void processImageQcResponseAutoNeedRevisionResubmitProductTest() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollection.setAutoNeedRevision(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse)).thenReturn(
        new AutoNeedRevisionAndForceReviewResponse(true, false, false, Collections.singleton(PREDICTION_NAME_1)));
    when(productLevel3Helper
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(new ArrayList<>());
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
        .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
            productDetailResponse, false, profileResponse);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).updatePostLiveStatus(productCollectionArgumentCaptor.capture(), eq(false));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    Assertions.assertFalse(productCollectionArgumentCaptor.getValue().isReviewPending());
  }

  @Test
  public void processImageQcResponseNotInDraftStateTest() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setReviewPending(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse)).thenReturn(new AutoNeedRevisionAndForceReviewResponse(false,
      true, true, null));
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.isForceReview(STORE_ID, PRODUCT_CODE)).thenReturn(false);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
        .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
            productDetailResponse, false, profileResponse);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService)
        .publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE, productCollection, productDetailResponse,
            new AutoNeedRevisionAndForceReviewResponse(false, true, true, null), profileResponse,
            imageQcResponseDomainEvent);
    verify(productService).isForceReview(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void processImageQcResponseProductCollectionNullTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_CODE)).thenReturn(null);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, null)).thenReturn(new AutoNeedRevisionAndForceReviewResponse(false, true, true, null));
    when(productService.getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE)).thenReturn(
        new ProductImageQcProcessingResponse());
    profileResponse.setTrustedSeller(false);
    when(productService.isForceReview(STORE_ID, PRODUCT_CODE)).thenReturn(false);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void processImageQcResponseProductCollectionBusinessPartnerCodeInternalTest()
    throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(
      productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_CODE)).thenReturn(productCollection);
    productCollection.setBusinessPartnerCode(GdnBaseLookup.INTERNAL_BUSINESS_PARTNER_CODE);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap,
      productCollection, productDetailResponse, false, null)).thenReturn(
      new AutoNeedRevisionAndForceReviewResponse(false, true, true, null));
    when(productService.getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE)).thenReturn(
      new ProductImageQcProcessingResponse());
    profileResponse.setTrustedSeller(false);
    when(productService.isForceReview(STORE_ID, PRODUCT_CODE)).thenReturn(false);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(
      productCollection);
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(
      productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_CODE);
  }

  @Test
  public void processImageQcResponseNotInDraftStateTestAutoNeedRevision() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setReviewPending(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollection.setAutoNeedRevision(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse)).thenReturn(new AutoNeedRevisionAndForceReviewResponse(true,
      true, true, null));
    when(productService.isForceReview(STORE_ID, PRODUCT_CODE)).thenReturn(false);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
        .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
            productDetailResponse, false, profileResponse);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService)
        .publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE, productCollection, productDetailResponse,
            new AutoNeedRevisionAndForceReviewResponse(true, true, true, null), profileResponse,
            imageQcResponseDomainEvent);
    verify(productService).isForceReview(STORE_ID, PRODUCT_CODE);
    verify(productService).publishAutoNeedRevisionEvent(STORE_ID, PRODUCT_CODE, null, false, StringUtils.EMPTY);
  }

  @Test
  public void processImageQcResponseNotInDraftStateTestAutoNeedRevisionKeyword() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setReviewPending(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollection.setAutoNeedRevision(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_CODE)).thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse =
        new AutoNeedRevisionAndForceReviewResponse(true, true, true, null);
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType1 =
        new RestrictedKeywordsByFieldAndActionType();
    autoNeedRevisionAndForceReviewResponse.setRestrictedKeywordsByFieldAndActionType(
        restrictedKeywordsByFieldAndActionType1);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse)).thenReturn(autoNeedRevisionAndForceReviewResponse);
    when(productService.isForceReview(STORE_ID, PRODUCT_CODE)).thenReturn(false);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository, times(2)).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_CODE);
    verify(productService).processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap,
        productCollection, productDetailResponse, false, profileResponse);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE, productCollection,
        productDetailResponse, autoNeedRevisionAndForceReviewResponse, profileResponse, imageQcResponseDomainEvent);
    verify(productService).isForceReview(STORE_ID, PRODUCT_CODE);
    verify(productService).publishAutoNeedRevisionEvent(STORE_ID, PRODUCT_CODE, null, false, StringUtils.EMPTY);
  }

  @Test
  public void processImageQcResponseNotInDraftStateTestAutoNeedRevisionKeyword2() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setReviewPending(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollection.setAutoNeedRevision(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_CODE)).thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse =
        new AutoNeedRevisionAndForceReviewResponse(true, true, true, null);
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType1 =
        new RestrictedKeywordsByFieldAndActionType();
    restrictedKeywordsByFieldAndActionType1.setAction(2);
    autoNeedRevisionAndForceReviewResponse.setRestrictedKeywordsByFieldAndActionType(
        restrictedKeywordsByFieldAndActionType1);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse)).thenReturn(autoNeedRevisionAndForceReviewResponse);
    when(productService.isForceReview(STORE_ID, PRODUCT_CODE)).thenReturn(false);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository, times(2)).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_CODE);
    verify(productService).processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap,
        productCollection, productDetailResponse, false, profileResponse);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE, productCollection,
        productDetailResponse, autoNeedRevisionAndForceReviewResponse, profileResponse, imageQcResponseDomainEvent);
    verify(productService).isForceReview(STORE_ID, PRODUCT_CODE);
    verify(productService).publishAutoNeedRevisionEvent(STORE_ID, PRODUCT_CODE, null, true, null);
  }

  @Test
  public void processImageQcResponseNotInDraftStateTestAutoNeedRevision_forTrustedSellers() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setReviewPending(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollection.setAutoNeedRevision(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
      productDetailResponse, false, profileResponse)).thenReturn(new AutoNeedRevisionAndForceReviewResponse(true,
      true, true, null));
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.isForceReview(STORE_ID, PRODUCT_CODE)).thenReturn(false);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository, times(2))
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
      .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService)
      .publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE, productCollection, productDetailResponse,
        new AutoNeedRevisionAndForceReviewResponse(true, true, true, null), profileResponse, imageQcResponseDomainEvent);
    verify(productService).isForceReview(STORE_ID, PRODUCT_CODE);
    verify(productService).publishAutoNeedRevisionEvent(STORE_ID, PRODUCT_CODE, null, false, StringUtils.EMPTY);
  }

  @Test
  public void processImageQcResponseNotInDraftStateTestAutoNeedRevision_On_CategoryOrBrandTakeDown() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setReviewPending(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollection.setAutoNeedRevision(false);
    productCollection.setStoreId(STORE_ID);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    AgpSimpleQueryResponse agpSimpleQueryResponse = new AgpSimpleQueryResponse();
    HitsResponse hitsResponse = new HitsResponse(10, 10);
    agpSimpleQueryResponse.setHits(hitsResponse);
    Mockito.when(productService.getQueryResponseForActiveOrderData(PRODUCT_CODE, PRODUCT_SKU))
        .thenReturn(agpSimpleQueryResponse);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(productBusinessPartner);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_CODE)).thenReturn(productCollection);
    AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse =
        new AutoNeedRevisionAndForceReviewResponse(true, true, true, null);
    autoNeedRevisionAndForceReviewResponse.setBrandTakeDown(true);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse)).thenReturn(autoNeedRevisionAndForceReviewResponse);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.isForceReview(STORE_ID, PRODUCT_CODE)).thenReturn(false);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository, times(2)).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_CODE);
    verify(productService).processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap,
        productCollection, productDetailResponse, false, profileResponse);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).overRideRestrictedKeywordActionOnActiveOrders(productCollection.getProductCode(),
        autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType());
    verify(productService).publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE, productCollection,
        productDetailResponse, autoNeedRevisionAndForceReviewResponse, profileResponse, imageQcResponseDomainEvent);
    verify(productService).isForceReview(STORE_ID, PRODUCT_CODE);
    verify(productService).publishAutoNeedRevisionEvent(STORE_ID, PRODUCT_CODE, null, false, StringUtils.EMPTY);
    verify(productService).getQueryResponseForActiveOrderData(PRODUCT_CODE, PRODUCT_SKU);
    verify(productBusinessPartnerService).findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void processImageQcResponseNotInDraftStateTestAutoNeedRevision_On_CategoryOrBrandTakeDown_No_Order() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setReviewPending(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollection.setAutoNeedRevision(false);
    productCollection.setStoreId(STORE_ID);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    AgpSimpleQueryResponse agpSimpleQueryResponse = new AgpSimpleQueryResponse();
    agpSimpleQueryResponse.setHits(new HitsResponse());
    Mockito.when(productService.getQueryResponseForActiveOrderData(PRODUCT_CODE, PRODUCT_SKU))
        .thenReturn(agpSimpleQueryResponse);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(productBusinessPartner);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_CODE)).thenReturn(productCollection);
    AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse =
        new AutoNeedRevisionAndForceReviewResponse(true, true, true, null);
    autoNeedRevisionAndForceReviewResponse.setBrandTakeDown(true);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse)).thenReturn(autoNeedRevisionAndForceReviewResponse);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.isForceReview(STORE_ID, PRODUCT_CODE)).thenReturn(false);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository, times(2)).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_CODE);
    verify(productService).processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap,
        productCollection, productDetailResponse, false, profileResponse);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE, productCollection,
        productDetailResponse, autoNeedRevisionAndForceReviewResponse, profileResponse, imageQcResponseDomainEvent);
    verify(productService).isForceReview(STORE_ID, PRODUCT_CODE);
    verify(productService).publishAutoNeedRevisionEvent(STORE_ID, PRODUCT_CODE, null, false, StringUtils.EMPTY);
    verify(productService).getQueryResponseForActiveOrderData(PRODUCT_CODE, PRODUCT_SKU);
    verify(productBusinessPartnerService).findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }


  @Test
  public void processImageQcResponseNotInDraftStateForceReviewTest() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(STATE_IN_PROGRESS);
    productCollection.setReviewPending(true);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse))
        .thenReturn(new AutoNeedRevisionAndForceReviewResponse(false, true, true, null));
    when(productService.isForceReview(STORE_ID, PRODUCT_CODE)).thenReturn(true);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productCollection.setImageResized(true);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository, times(2))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
        .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
            productDetailResponse, false, profileResponse);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService)
        .publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE, productCollection, productDetailResponse,
            new AutoNeedRevisionAndForceReviewResponse(false, true, true, null), profileResponse,
            imageQcResponseDomainEvent);
    verify(productService).isForceReview(STORE_ID, PRODUCT_CODE);
    verify(productService).takeDownOrActivateProductByProductCode(STORE_ID, productCollection.getProductCode(), true);
  }

  @Test
  public void updateProductHistoryOnWholesaleChangesBySchedulerTest() throws Exception {
    when(xProductOutbound.findSummaryDetailsByFilter(any(), any())).thenReturn(new PageImpl<>(Arrays.asList()));
    productServiceWrapper.updateProductHistoryOnWholesaleChangesByScheduler(MERCHANT_CODE, ITEM_SKU, true);
    verify(xProductOutbound).findSummaryDetailsByFilter(any(), any());
  }

  @Test
  public void updateProductHistoryOnWholesaleChangesBySchedulerActivatedFalseTest() throws Exception {
    when(xProductOutbound.findSummaryDetailsByFilter(any(), any()))
        .thenReturn(new PageImpl<>(Arrays.asList(new ItemSummaryDetailResponse())));
    productServiceWrapper.updateProductHistoryOnWholesaleChangesByScheduler(MERCHANT_CODE, ITEM_SKU, false);
    verify(xProductOutbound).findSummaryDetailsByFilter(any(), any());
    Mockito.verify(updatedProductHistoryService).saveUpdateProductLevel3AuditForWholeSale(MERCHANT_CODE, ITEM_SKU,
        UpdateProductActivity.WHOLE_PRICE_FLAG.getDesc(), String.valueOf(false), String.valueOf(true), null, null);
  }

  @Test
  public void updateProductHistoryOnWholesaleChangesBySchedulerActivatedTrueTest() throws Exception {
    when(xProductOutbound.findSummaryDetailsByFilter(any(), any()))
        .thenReturn(new PageImpl<>(Arrays.asList(new ItemSummaryDetailResponse())));
    productServiceWrapper.updateProductHistoryOnWholesaleChangesByScheduler(MERCHANT_CODE, ITEM_SKU, true);
    verify(xProductOutbound).findSummaryDetailsByFilter(any(), any());
    Mockito.verify(updatedProductHistoryService).saveUpdateProductLevel3AuditForWholeSale(MERCHANT_CODE, ITEM_SKU,
        UpdateProductActivity.WHOLE_PRICE_FLAG.getDesc(), String.valueOf(true), String.valueOf(false), null, null);
  }

  @Test
  public void updateProductHistoryLevel3AuditTest() throws Exception {
    List<AuditTrailDto> auditTrailRequests = new ArrayList<>();
    auditTrailRequests.add(auditTrailRequest);
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(ITEM_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);
    productServiceWrapper.updateProductHistoryLevel3Audit(auditTrailRequests, DEFAULT_ACCESS_CHANNEL, false, false);
    Mockito.verify(updatedProductHistoryService)
        .addAuditLogsForProductHistoryUpdate(auditTrailRequest, new ArrayList<>(), DEFAULT_ACCESS_CHANNEL);
    Mockito.verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    Mockito.verify(updatedProductHistoryService).createAudit(anyList(), eq(false));
  }

  @Test
  public void updateProductHistoryLevel3AuditDirectUpdateTrueTest() throws Exception {
    List<AuditTrailDto> auditTrailRequests = new ArrayList<>();
    auditTrailRequests.add(auditTrailRequest);
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(ITEM_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    productServiceWrapper.updateProductHistoryLevel3Audit(auditTrailRequests, DEFAULT_ACCESS_CHANNEL, true, false);
    Mockito.verify(updatedProductHistoryService)
        .addAuditLogsForProductHistoryUpdate(auditTrailRequest, new ArrayList<>(), DEFAULT_ACCESS_CHANNEL);
    Mockito.verify(updatedProductHistoryService).createAudit(anyList(), eq(false));
  }

  @Test
  public void updateProductHistoryLevel3AuditNewSolrUpdateTest() throws Exception {
    List<AuditTrailDto> auditTrailRequests = new ArrayList<>();
    auditTrailRequests.add(auditTrailRequest);
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(ITEM_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);
    Mockito.when(kafkaTopicProperties.getProductSkuSolrUpdateEvent()).thenReturn(PRODUCT_ID);
    Mockito.when(updatedProductHistoryService.createAudit(anyList(), eq(true)))
        .thenReturn(Collections.singletonList(updatedProductHistory));
    productServiceWrapper.updateProductHistoryLevel3Audit(auditTrailRequests, DEFAULT_ACCESS_CHANNEL, false, true);
    Mockito.verify(updatedProductHistoryService)
        .addAuditLogsForProductHistoryUpdate(auditTrailRequest, new ArrayList<>(), DEFAULT_ACCESS_CHANNEL);
    Mockito.verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    Mockito.verify(updatedProductHistoryService).createAudit(anyList(), eq(true));
    Mockito.verify(kafkaTopicProperties).getProductSkuSolrUpdateEvent();
    Mockito.verify(kafkaProducer).send(PRODUCT_ID, PRODUCT_SKU,
        new UpdatedProductHistoryRequest(Collections.singletonList(updatedProductHistory)));
  }

  @Test
  public void updateProductHistoryLevel3AuditDirectUpdateTrueNewSolrUpdateTest() throws Exception {
    List<AuditTrailDto> auditTrailRequests = new ArrayList<>();
    auditTrailRequests.add(auditTrailRequest);
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(ITEM_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(kafkaTopicProperties.getProductSkuSolrUpdateEvent()).thenReturn(PRODUCT_ID);
    Mockito.when(updatedProductHistoryService.createAudit(anyList(), eq(true)))
        .thenReturn(Collections.singletonList(updatedProductHistory));
    productServiceWrapper.updateProductHistoryLevel3Audit(auditTrailRequests, DEFAULT_ACCESS_CHANNEL, true, true);
    Mockito.verify(updatedProductHistoryService)
        .addAuditLogsForProductHistoryUpdate(auditTrailRequest, new ArrayList<>(), DEFAULT_ACCESS_CHANNEL);
    Mockito.verify(updatedProductHistoryService).createAudit(anyList(), eq(true));
    Mockito.verify(kafkaTopicProperties).getProductSkuSolrUpdateEvent();
    Mockito.verify(kafkaProducer).send(PRODUCT_ID, PRODUCT_SKU,
        new UpdatedProductHistoryRequest(Collections.singletonList(updatedProductHistory)));
  }

  @Test
  public void skipScreeningForSkipReviewProductTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productLevel1WipService.approveDraft(PRODUCT_CODE, null)).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel1HistoryService).create(PRODUCT_CODE, "APPROVE_DRAFT", null);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    Mockito.doNothing().when(approveProductService).processImage(productDetailResponse);
    Mockito.doNothing().when(productService)
        .publishProductStatusEvent(productDetailResponse, productCollection, ProductStatus.CREATED, StringUtils.EMPTY);
    productServiceWrapper.skipScreeningForSkipReviewProduct(PRODUCT_CODE, null);
    verify(productLevel1WipService).approveDraft(PRODUCT_CODE, null);
    verify(productLevel1HistoryService).create(PRODUCT_CODE, "APPROVE_DRAFT", null);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(approveProductService).processImage(productDetailResponse);
    verify(productService).publishProductStatusEvent(productDetailResponseArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(ProductStatus.CREATED), eq(StringUtils.EMPTY));
  }

  @Test
  public void updateImagePathsForEditedResizeImagesTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE, productCollection, null))
        .thenReturn(new AddEditedProductToPDTEvent());
    when(productService.publishImageQcEventForEditedImages(PRODUCT_CODE, eq(anyList()), eq(new ProductDetailResponse()),
        eq(false), eq(new RestrictedKeywordsByFieldAndActionType()), eq(BUSINESS_PARTNER_CODE))).thenReturn(new ImageQcRequestDomainEvent());
    doNothing().when(productService)
        .updateEditedImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true), eq(false));
    productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, false);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(1))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateEditedImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true), eq(false));
    verify(productService)
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE, productCollection, null);
    verify(productService)
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), EditedReviewTypeConstants.CONTENT_REFRESH,
            productCollection, null);
    verify(productService).publishImageQcEventForEditedImages(PRODUCT_CODE,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages(), productDetailResponse,false,
        new RestrictedKeywordsByFieldAndActionType(), BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsForEditedResizeImagesAddedItemImagesTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImagesAddItemImage();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE, productCollection, null))
        .thenReturn(new AddEditedProductToPDTEvent());
    when(productService.publishImageQcEventForEditedImages(PRODUCT_CODE, eq(anyList()), eq(new ProductDetailResponse()),
        eq(false), eq(new RestrictedKeywordsByFieldAndActionType()), eq(BUSINESS_PARTNER_CODE))).thenReturn(new ImageQcRequestDomainEvent());
    doNothing().when(productService)
        .updateEditedImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true), eq(false));
    productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, false);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(1))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateEditedImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true), eq(false));
    verify(productService)
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE, productCollection, null);
    verify(productService)
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), EditedReviewTypeConstants.CONTENT_REFRESH,
            productCollection, null);
    verify(productService).publishImageQcEventForEditedImages(PRODUCT_CODE,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages(), productDetailResponse,false,
        new RestrictedKeywordsByFieldAndActionType(), BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsForEditedResizeImagesAddedItemImagesTest2() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "preventEditedEventPublishForNeedRevisionProducts", true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImagesAddItemImage();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE, productCollection, null))
        .thenReturn(new AddEditedProductToPDTEvent());
    when(productService.publishImageQcEventForEditedImages(PRODUCT_CODE, eq(anyList()), eq(new ProductDetailResponse()),
        eq(false), eq(new RestrictedKeywordsByFieldAndActionType()), eq(BUSINESS_PARTNER_CODE))).thenReturn(new ImageQcRequestDomainEvent());
    doNothing().when(productService)
        .updateEditedImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true), eq(false));
    when(productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(PRODUCT_CODE, false)).thenReturn(true);
    productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, false);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(1))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateEditedImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true), eq(false));
    verify(productService)
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE, productCollection, null);
    verify(productService).publishImageQcEventForEditedImages(PRODUCT_CODE,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages(), productDetailResponse,false,
        new RestrictedKeywordsByFieldAndActionType(), BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsForEditedResizeImagesAddedItemImagesTest3() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "preventEditedEventPublishForNeedRevisionProducts", true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImagesAddItemImage();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    productCollection.setState(Constants.NEED_CORRECTION);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE, productCollection, null))
        .thenReturn(new AddEditedProductToPDTEvent());
    when(productService.publishImageQcEventForEditedImages(PRODUCT_CODE, eq(anyList()), eq(new ProductDetailResponse()),
        eq(false), eq(new RestrictedKeywordsByFieldAndActionType()), eq(BUSINESS_PARTNER_CODE))).thenReturn(new ImageQcRequestDomainEvent());
    doNothing().when(productService)
        .updateEditedImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true), eq(false));
    productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, false);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(1))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateEditedImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true), eq(false));
    verify(productService).publishImageQcEventForEditedImages(PRODUCT_CODE,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages(), productDetailResponse,false,
        new RestrictedKeywordsByFieldAndActionType(), BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsForEditedResizeImagesWithNoImagesTest() throws Exception {
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(null);
    productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, false);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
  }

  @Test
  public void updateImagePathsForEditedResizeImagesWithFewEditProductsTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE, productCollection, null))
        .thenReturn(new AddEditedProductToPDTEvent());
    when(productService.publishImageQcEventForEditedImages(PRODUCT_CODE, eq(anyList()), eq(new ProductDetailResponse()),
        eq(false), eq(new RestrictedKeywordsByFieldAndActionType()), eq(BUSINESS_PARTNER_CODE))).thenReturn(new ImageQcRequestDomainEvent());
    doNothing().when(productService)
        .updateEditedImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true), eq(false));
    productServiceWrapper.updateImagePathsForEditedResizeImages(editImageProcessResponse, false);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(1))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateEditedImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true), eq(false));
    verify(productService)
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE, productCollection, null);
    verify(productService)
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), EditedReviewTypeConstants.CONTENT_REFRESH,
            productCollection, null);
    verify(productService).publishImageQcEventForEditedImages(PRODUCT_CODE,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages(), productDetailResponse,false,
        new RestrictedKeywordsByFieldAndActionType(), BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(2, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsForEditedResizeImagesWithFewExistingProductsTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithResizedImages();
    List<Image> images = productDetailResponse.getImages();
    productDetailResponse.setImages(images);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE, productCollection, null))
        .thenReturn(new AddEditedProductToPDTEvent());
    when(productService.publishImageQcEventForEditedImages(PRODUCT_CODE, eq(anyList()), eq(new ProductDetailResponse()),
        eq(false), eq(new RestrictedKeywordsByFieldAndActionType()), eq(BUSINESS_PARTNER_CODE))).thenReturn(new ImageQcRequestDomainEvent());
    doNothing().when(productService)
        .updateEditedImagePathsAndFlagAfterResizingImage(Mockito.any(ProductAndItemImageRequest.class), eq(productCollection),
            eq(true), eq(false));
    productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, false);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(1))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateEditedImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true), eq(false));
    verify(productService)
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), REVIEW_TYPE_IMAGE, productCollection, null);
    verify(productService)
        .publishAddEditedProductToPDTEvent(productCollection.getStoreId(), EditedReviewTypeConstants.CONTENT_REFRESH,
            productCollection, null);
    verify(productService).publishImageQcEventForEditedImages(PRODUCT_CODE,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages(), productDetailResponse,false,
        new RestrictedKeywordsByFieldAndActionType(), BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(2, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsForEditedResizeImagesWithFewEditProductsAndWithExistingProductsTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithResizedImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        productServiceWrapper.updateImagePathsForEditedResizeImages(editImageProcessResponse, false);
      });
    } finally {
      verify(productService, times(1)).
          findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productCollectionRepository, times(1)).
          findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void updateImagePathsForEditedResizeImagesRevisedTrueTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "skipDefinitiveAction", true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    productCollection.setNeedCorrectionNotes(null);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, DRAFT_STATE, ITEM_SKU_1, DRAFT_STATE);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.publishImageQcEventForEditedImages(PRODUCT_CODE, eq(anyList()), eq(new ProductDetailResponse()),
        eq(true), eq(new RestrictedKeywordsByFieldAndActionType()), eq(BUSINESS_PARTNER_CODE))).thenReturn(new ImageQcRequestDomainEvent());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(
        productPricingOutbound.bulkActivateOrDeactivateSku(Mockito.anyList()))
        .thenReturn(new BulkActivateDeactivateResponse());
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class)))
        .thenReturn(WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatusMap(statusMap).build());
    Mockito.when(productPricingOutbound.getWholesalePriceList(Mockito.anySet(), Mockito.anyMap())).thenReturn(Arrays
        .asList(WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.INACTIVE_STATUS).build(),
            WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_1).skuStatus(Constants.ACTIVE_STATUS).build()));
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    businessPartner.setActivated(true);
    when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
        .thenReturn(Collections.singletonList(businessPartner));
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, true);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(1))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateEditedImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true), eq(false));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    verify(productService).publishImageQcEventForEditedImages(PRODUCT_CODE,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages(), productDetailResponse,true,
        new RestrictedKeywordsByFieldAndActionType(), BUSINESS_PARTNER_CODE);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsForEditedResizeImagesRevisedTrueAutoRejectTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    restrictedKeywordsByFieldAndActionType.setAction(3);
    restrictedKeywordsByFieldAndActionType.setCategoryRestrictedKeywordId(KEYWORD);
    productCollection.setNeedCorrectionNotes(null);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, DRAFT_STATE, ITEM_SKU_1, DRAFT_STATE);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.publishImageQcEventForEditedImages(PRODUCT_CODE, eq(anyList()), eq(new ProductDetailResponse()),
        eq(true), eq(new RestrictedKeywordsByFieldAndActionType()), eq(BUSINESS_PARTNER_CODE))).thenReturn(new ImageQcRequestDomainEvent());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
            .findByStoreIdAndProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(
            productPricingOutbound.bulkActivateOrDeactivateSku(Mockito.anyList()))
        .thenReturn(new BulkActivateDeactivateResponse());
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class)))
        .thenReturn(WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatusMap(statusMap).build());
    Mockito.when(productPricingOutbound.getWholesalePriceList(Mockito.anySet(), Mockito.anyMap())).thenReturn(Arrays
        .asList(WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.INACTIVE_STATUS).build(),
            WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_1).skuStatus(Constants.ACTIVE_STATUS).build()));
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
            .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    Mockito.when(productRepository.getCategoryRestrictedKeywordDetail(KEYWORD)).thenReturn(categoryRestrictedKeywordResponse);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    businessPartner.setActivated(true);
    when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
        .thenReturn(Collections.singletonList(businessPartner));
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, true);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(1))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateEditedImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true), eq(false));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    verify(productService).publishImageQcEventForEditedImages(PRODUCT_CODE,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages(), productDetailResponse,true,
        restrictedKeywordsByFieldAndActionType, BUSINESS_PARTNER_CODE);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productRepository).getCategoryRestrictedKeywordDetail(KEYWORD);
    Mockito.verify(productWorkflowServiceWrapper).deleteProductCollection(STORE_ID, PRODUCT_CODE, categoryRestrictedKeywordResponse.getMessage(),
        true, true);
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsForEditedResizeImagesRevisedTrueAutoRejectTrustedSellerTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    restrictedKeywordsByFieldAndActionType.setAction(3);
    restrictedKeywordsByFieldAndActionType.setCategoryRestrictedKeywordId(KEYWORD);
    productCollection.setNeedCorrectionNotes(null);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, DRAFT_STATE, ITEM_SKU_1, DRAFT_STATE);
    profileResponse.setTrustedSeller(true);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.publishImageQcEventForEditedImages(PRODUCT_CODE, eq(anyList()), eq(new ProductDetailResponse()),
        eq(true), eq(new RestrictedKeywordsByFieldAndActionType()), eq(BUSINESS_PARTNER_CODE))).thenReturn(new ImageQcRequestDomainEvent());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
            .findByStoreIdAndProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(
            productPricingOutbound.bulkActivateOrDeactivateSku(Mockito.anyList()))
        .thenReturn(new BulkActivateDeactivateResponse());
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class)))
        .thenReturn(WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatusMap(statusMap).build());
    Mockito.when(productPricingOutbound.getWholesalePriceList(Mockito.anySet(), Mockito.anyMap())).thenReturn(Arrays
        .asList(WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.INACTIVE_STATUS).build(),
            WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_1).skuStatus(Constants.ACTIVE_STATUS).build()));
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
            .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    Mockito.when(productRepository.getCategoryRestrictedKeywordDetail(KEYWORD)).thenReturn(categoryRestrictedKeywordResponse);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    businessPartner.setActivated(true);
    when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
        .thenReturn(Collections.singletonList(businessPartner));
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, true);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(1))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateEditedImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true), eq(false));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    verify(productService).publishImageQcEventForEditedImages(PRODUCT_CODE,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages(), productDetailResponse,true,
        restrictedKeywordsByFieldAndActionType, BUSINESS_PARTNER_CODE);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsForEditedResizeImagesRevisedTrueMppTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    productCollection.setNeedCorrectionNotes(null);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, DRAFT_STATE, ITEM_SKU_1, DRAFT_STATE);

    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.publishImageQcEventForEditedImages(PRODUCT_CODE, eq(anyList()), eq(new ProductDetailResponse()),
        eq(true), eq(new RestrictedKeywordsByFieldAndActionType()), eq(BUSINESS_PARTNER_CODE))).thenReturn(new ImageQcRequestDomainEvent());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(
        productPricingOutbound.bulkActivateOrDeactivateSku(Mockito.anyList()))
        .thenReturn(new BulkActivateDeactivateResponse());
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class)))
        .thenReturn(WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatusMap(statusMap).build());
    Mockito.when(productPricingOutbound.getWholesalePriceList(Mockito.anySet(), Mockito.anyMap())).thenReturn(Arrays
        .asList(WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.INACTIVE_STATUS).build(),
            WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_1).skuStatus(Constants.ACTIVE_STATUS).build()));
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    businessPartner.setActivated(true);
    when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
        .thenReturn(Collections.singletonList(businessPartner));
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, true);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(1))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateEditedImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true), eq(false));
    verify(productService).publishImageQcEventForEditedImages(PRODUCT_CODE,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages(), productDetailResponse,true,
        new RestrictedKeywordsByFieldAndActionType(), BUSINESS_PARTNER_CODE);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }

  @Test
  public void updateImagePathsForEditedResizeImagesRevisedTrueStatusInactiveTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    productCollection.setNeedCorrectionNotes(null);
    WholeSalePriceSkuStatusDto wholeSalePriceSkuStatusDto1 = new WholeSalePriceSkuStatusDto();
    wholeSalePriceSkuStatusDto1.setItemSku(ITEM_SKU);
    wholeSalePriceSkuStatusDto1.setSkuStatus(DRAFT_STATE);
    WholeSalePriceSkuStatusDto wholeSalePriceSkuStatusDto2 = new WholeSalePriceSkuStatusDto();
    wholeSalePriceSkuStatusDto2.setItemSku(ITEM_SKU_1);
    wholeSalePriceSkuStatusDto2.setSkuStatus(Constants.ACTIVE);

    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.publishImageQcEventForEditedImages(PRODUCT_CODE, eq(anyList()), eq(new ProductDetailResponse()),
        eq(true), eq(new RestrictedKeywordsByFieldAndActionType()), eq(BUSINESS_PARTNER_CODE))).thenReturn(new ImageQcRequestDomainEvent());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
            .findByStoreIdAndProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(
            productPricingOutbound.bulkActivateOrDeactivateSku(Mockito.anyList()))
        .thenReturn(new BulkActivateDeactivateResponse());
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class))).thenReturn(
        WholesalePriceBulkUpdateResponse.builder()
            .wholesalePriceSkuStatus(Arrays.asList(wholeSalePriceSkuStatusDto1, wholeSalePriceSkuStatusDto2)).build());
    Mockito.when(productPricingOutbound.getWholesalePriceList(Mockito.anySet(), Mockito.anyMap())).thenReturn(Arrays
        .asList(WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.INACTIVE_STATUS).build(),
            WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_1).skuStatus(Constants.ACTIVE_STATUS).build()));
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
            .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    businessPartner.setActivated(true);
    when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
        .thenReturn(Collections.singletonList(businessPartner));
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    Mockito.doNothing().when(updatedProductHistoryService).createProductL3AuditLog(Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.anyBoolean(), Mockito.any());
    productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, true);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(productCollectionRepository, times(1))
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateEditedImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
        productCollectionArgumentCaptor.capture(), eq(true), eq(false));
    verify(productService).publishImageQcEventForEditedImages(PRODUCT_CODE,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages(), productDetailResponse,true,
        new RestrictedKeywordsByFieldAndActionType(), BUSINESS_PARTNER_CODE);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(updatedProductHistoryService, times(2)).createProductL3AuditLog(Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.anyBoolean(), Mockito.any());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals(0,
        productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream().filter(Image::getOriginalImage)
            .count());
    Assertions.assertEquals(4, productAndItemImageRequestArgumentCaptor.getValue().getProductImages().stream()
        .filter(image -> !image.getOriginalImage()).count());
  }


  @Test
  public void updateImagePathsEmptyTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    productCollection.setNeedCorrectionNotes(null);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productService.publishImageQcEventForEditedImages(PRODUCT_CODE, eq(anyList()), eq(new ProductDetailResponse()),
        eq(false), eq(new RestrictedKeywordsByFieldAndActionType()), eq(BUSINESS_PARTNER_CODE))).thenReturn(new ImageQcRequestDomainEvent());
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(productPricingOutbound.setWholesaleActivated(Mockito.any(String.class), Mockito.any(String.class),
        Mockito.any()))
        .thenReturn(true);
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class)))
        .thenReturn(new WholesalePriceBulkUpdateResponse());
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
        .thenReturn(null);
    Mockito.when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse,
        productCollection.getCategoryCode())).thenReturn(restrictedKeywordsByFieldAndActionType);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, true);
      });
    } finally {
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
      verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      verify(productCollectionRepository, times(1))
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
      verify(productService).updateEditedImagePathsAndFlagAfterResizingImage(productAndItemImageRequestArgumentCaptor.capture(),
          productCollectionArgumentCaptor.capture(), eq(true), eq(false));

      Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
      Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
      Mockito.verify(productBusinessPartnerService)
          .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any());
      Mockito.verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse,
          productCollection.getCategoryCode());
    }
  }

  @Test
  public void processImageQcResponseEditedProductTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setEdited(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE))
        .thenReturn(new ProductImageQcProcessingResponse());
    when(productService.updateImageQcResponse(Mockito.eq(STORE_ID), Mockito.eq(imageQcResponseDomainEvent),
        Mockito.eq(new ProductImageQcProcessingResponse()), Mockito.eq(productCollection), Mockito.eq(imageCountMap),
        Mockito.any(), Mockito.any(), eq(productDetailResponse), eq(profileResponse))).thenReturn(new AutoNeedRevisionAndForceReviewResponse());
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImageQcResponse(Mockito.eq(STORE_ID), Mockito.eq(imageQcResponseDomainEvent),
        Mockito.eq(new ProductImageQcProcessingResponse()), Mockito.eq(productCollection), Mockito.eq(imageCountMap),
        productImageQcFeedbackResponseArgumentCaptor.capture(), pdtProductDomainEventModelArgumentCaptor.capture(),
        eq(productDetailResponse), eq(profileResponse));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService)
        .publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE, productCollection, productDetailResponse,
            new AutoNeedRevisionAndForceReviewResponse(), profileResponse, imageQcResponseDomainEvent);
    verify(productService).getImageQcResponseFromPDT(STORE_ID, PRODUCT_CODE);
    verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE);
  }

  @Test
  public void processImageQcResponseEditedProductTakeDownTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setEdited(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollection.setReviewPending(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE))
        .thenReturn(new ProductImageQcProcessingResponse());
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService
        .updateImageQcResponse(eq(STORE_ID), eq(imageQcResponseDomainEvent), eq(new ProductImageQcProcessingResponse()),
            eq(productCollection), eq(imageCountMap), Mockito.any(), Mockito.any(), eq(productDetailResponse),
          eq(profileResponse)))
        .thenReturn(new AutoNeedRevisionAndForceReviewResponse(false, true, false, null));
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImageQcResponse(Mockito.eq(STORE_ID), Mockito.eq(imageQcResponseDomainEvent),
        Mockito.eq(new ProductImageQcProcessingResponse()), Mockito.eq(productCollection), Mockito.eq(imageCountMap),
        productImageQcFeedbackResponseArgumentCaptor.capture(), pdtProductDomainEventModelArgumentCaptor.capture(),
        eq(productDetailResponse), eq(profileResponse));
    verify(productService)
        .publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE, productCollection, productDetailResponse,
            new AutoNeedRevisionAndForceReviewResponse(false, true, false, null), profileResponse,
            imageQcResponseDomainEvent);
    verify(productService)
        .takeDownOrActivateProductByProductCode(STORE_ID, imageQcResponseDomainEvent.getProductCode(), true);
    verify(productService).getImageQcResponseFromPDT(STORE_ID, PRODUCT_CODE);
    verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
  }

  @Test
  public void processImageQcResponseEditedProductTakeDownNeedRevisionTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setEdited(true);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.name());
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollection.setReviewPending(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE))
      .thenReturn(new ProductImageQcProcessingResponse());
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService
      .updateImageQcResponse(eq(STORE_ID), eq(imageQcResponseDomainEvent), eq(new ProductImageQcProcessingResponse()),
        eq(productCollection), eq(imageCountMap), Mockito.any(), Mockito.any(), eq(productDetailResponse),
        eq(profileResponse)))
      .thenReturn(new AutoNeedRevisionAndForceReviewResponse(false, true, false, null));
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImageQcResponse(Mockito.eq(STORE_ID), Mockito.eq(imageQcResponseDomainEvent),
      Mockito.eq(new ProductImageQcProcessingResponse()), Mockito.eq(productCollection), Mockito.eq(imageCountMap),
      productImageQcFeedbackResponseArgumentCaptor.capture(), pdtProductDomainEventModelArgumentCaptor.capture(),
      eq(productDetailResponse), eq(profileResponse));
    verify(productService)
      .publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE, productCollection, productDetailResponse,
        new AutoNeedRevisionAndForceReviewResponse(false, true, false, null), profileResponse,
        imageQcResponseDomainEvent);
    verify(productService).getImageQcResponseFromPDT(STORE_ID, PRODUCT_CODE);
    verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
  }

  @Test
  public void processImageQcResponseEditedProductTakeDownForTrustedSellersTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setEdited(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE))
      .thenReturn(new ProductImageQcProcessingResponse());
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(true);
    when(productService
      .updateImageQcResponse(eq(STORE_ID), eq(imageQcResponseDomainEvent), eq(new ProductImageQcProcessingResponse()),
        eq(productCollection), eq(imageCountMap), Mockito.any(), Mockito.any(), eq(productDetailResponse),
        eq(profileResponse)))
      .thenReturn(new AutoNeedRevisionAndForceReviewResponse(false, false, true, null));
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImageQcResponse(Mockito.eq(STORE_ID), Mockito.eq(imageQcResponseDomainEvent),
      Mockito.eq(new ProductImageQcProcessingResponse()), Mockito.eq(productCollection), Mockito.eq(imageCountMap),
      productImageQcFeedbackResponseArgumentCaptor.capture(), pdtProductDomainEventModelArgumentCaptor.capture(),
      eq(productDetailResponse), eq(profileResponse));
    verify(productService)
      .publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE, productCollection, productDetailResponse,
        new AutoNeedRevisionAndForceReviewResponse(false, false, true, null), profileResponse,
          imageQcResponseDomainEvent);
    verify(productService).getImageQcResponseFromPDT(STORE_ID, PRODUCT_CODE);
    verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
  }

  @Test
  public void processImageQcResponseEditedProductTakeDownPreLiveTest() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE))
        .thenReturn(new ProductImageQcProcessingResponse());
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService
        .updateImageQcResponse(eq(STORE_ID), eq(imageQcResponseDomainEvent), eq(new ProductImageQcProcessingResponse()),
            eq(productCollection), eq(imageCountMap), Mockito.any(), Mockito.any(), eq(productDetailResponse),
          eq(profileResponse)))
        .thenReturn(new AutoNeedRevisionAndForceReviewResponse(false, true, false, null));
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService)
        .publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE, productCollection, productDetailResponse,
            new AutoNeedRevisionAndForceReviewResponse(false, true, false, null), profileResponse,
            imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService).updateImageQcResponse(Mockito.eq(STORE_ID), Mockito.eq(imageQcResponseDomainEvent),
        Mockito.eq(new ProductImageQcProcessingResponse()), Mockito.eq(productCollection), Mockito.eq(imageCountMap),
        productImageQcFeedbackResponseArgumentCaptor.capture(), pdtProductDomainEventModelArgumentCaptor.capture(),
        eq(productDetailResponse), eq(profileResponse));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
  }

  @Test
  public void getVendorNotesTest() throws Exception {
    productHistory1.setCreatedDate(new Date());
    this.productHistory3.setNotes(REASON_1);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection1);
    when(productLevel1HistoryService.findByStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productHistory1, productHistory2, productHistory3));
    VendorNotesResponse vendorNotesResponse = productServiceWrapper.getVendorNotes(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productLevel1HistoryService).findByStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID);
    assertEquals(1, vendorNotesResponse.getVendorNotes().size());
    assertEquals(2, vendorNotesResponse.getVendorErrorFields().size());
    assertEquals(ADDITIONAL_NOTES, vendorNotesResponse.getContentAdditionalNotes());
  }

  @Test
  public void getVendorNotesWithEmptyHistoryListTest() throws Exception {
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection1);
    when(productLevel1HistoryService.findByStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productHistory2));
    VendorNotesResponse vendorNotesResponse = productServiceWrapper.getVendorNotes(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productLevel1HistoryService).findByStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID);
    Assertions.assertEquals(vendorNotesResponse.getVendorNotes(), new ArrayList<>());
  }

  @Test
  public void getVendorNotesWithProductCollectionNullTest() throws Exception {
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(null);
    VendorNotesResponse vendorNotesResponse = productServiceWrapper.getVendorNotes(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Assertions.assertEquals(vendorNotesResponse.getVendorNotes(), new ArrayList<>());
  }

  @Test
  public void getVendorNotesWithNeedCorrectionTest() throws Exception {
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(objectMapper.readValue((String) any(), eq(VendorNotesResponse.class))).thenReturn(vendorNotesResponseData);
    VendorNotesResponse vendorNotesResponse = productServiceWrapper.getVendorNotes(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(objectMapper).readValue((String)  any(), eq(VendorNotesResponse.class));
    assertEquals(VENDOR_NOTES, vendorNotesResponse.getVendorNotes().get(0));
  }

  @Test
  public void getVendorNotesWithNeedCorrectionNullItemNotesTest() throws Exception {
    vendorNotesResponseData.setItemNotes(null);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(objectMapper.readValue((String) any(), eq(VendorNotesResponse.class))).thenReturn(vendorNotesResponseData);
    VendorNotesResponse vendorNotesResponse = productServiceWrapper.getVendorNotes(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(objectMapper).readValue((String) any(), eq(VendorNotesResponse.class));
    assertEquals(VENDOR_NOTES, vendorNotesResponse.getVendorNotes().get(0));
  }

  @Test
  public void getVendorNotesWithNeedCorrectionWithItemSkusTest() throws Exception {
    itemNotesDto.setItemSku(ITEM_SKU_1);
    this.vendorNotesResponseData.setItemNotes(Arrays.asList(itemNotesDto, itemNotesDto1));
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productOutbound.getProductItemIdsBySkuCodes(anyList())).thenReturn(new HashMap());
    when(productItemBusinessPartnerService.getSkuCodesAndItemSkusMap(any(), anyMap()))
        .thenReturn(skuCodesAndItemSkuMap);
    when(objectMapper.readValue((String) any(), eq(VendorNotesResponse.class))).thenReturn(vendorNotesResponseData);
    VendorNotesResponse vendorNotesResponse = productServiceWrapper.getVendorNotes(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(objectMapper).readValue((String) any(), eq(VendorNotesResponse.class));
    verify(productOutbound).getProductItemIdsBySkuCodes(anyList());
    verify(productItemBusinessPartnerService).getSkuCodesAndItemSkusMap(any(), anyMap());
    assertEquals(VENDOR_NOTES, vendorNotesResponse.getVendorNotes().get(0));
  }

  @Test
  public void getVendorNotesWithNeedCorrectionWithItemSkusSortingTest() throws Exception {
    itemNotesDto.setItemSku(ITEM_SKU_2);
    itemNotesDto1.setItemSku(ITEM_SKU_1);
    this.vendorNotesResponseData.setItemNotes(Arrays.asList(itemNotesDto, itemNotesDto1));
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productOutbound.getProductItemIdsBySkuCodes(anyList())).thenReturn(new HashMap());
    when(productItemBusinessPartnerService.getSkuCodesAndItemSkusMap(any(), anyMap()))
        .thenReturn(skuCodesAndItemSkuMap);
    when(objectMapper.readValue((String) any(), eq(VendorNotesResponse.class))).thenReturn(vendorNotesResponseData);
    VendorNotesResponse vendorNotesResponse = productServiceWrapper.getVendorNotes(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(objectMapper).readValue((String)  any(), eq(VendorNotesResponse.class));
    assertEquals(VENDOR_NOTES, vendorNotesResponse.getVendorNotes().get(0));
  }

  @Test
  public void updateVendorNotesTest() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(objectMapper.writeValueAsString(any())).thenReturn(VENDOR_NOTES);
    productServiceWrapper.updateVendorNotes(STORE_ID, PRODUCT_CODE, new VendorNotesRequest());
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(objectMapper).writeValueAsString(any());
    verify(productCollectionRepository).save(any(ProductCollection.class));
  }

  @Test
  public void updateVendorNotesWithNeedCorrectionNotesTest() throws Exception {
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(objectMapper.readValue(productCollection.getNeedCorrectionNotes(),
        VendorNotesResponse.class)).thenReturn(new VendorNotesResponse());
    when(objectMapper.writeValueAsString(any())).thenReturn(VENDOR_NOTES);
    productServiceWrapper.updateVendorNotes(STORE_ID, PRODUCT_CODE, new VendorNotesRequest());
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(objectMapper).writeValueAsString(any());
    verify(productCollectionRepository).save(any(ProductCollection.class));
  }

  @Test
  public void updateVendorNotesExceptionTest() throws Exception {
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productServiceWrapper.updateVendorNotes(STORE_ID, PRODUCT_CODE, new VendorNotesRequest());
      });
    }finally {
      verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void needRevisionSubmitPreliveTestWSNotChanged() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    productItemWholesalePrices.forEach(
        productItemWholesalePrice -> productItemWholesalePrice.setWholesalePriceActivated(true));
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, Constants.ACTIVE, ITEM_SKU_1, Constants.ACTIVE);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
            .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(
            productPricingOutbound.bulkActivateOrDeactivateSku(Mockito.anyList()))
        .thenReturn(new BulkActivateDeactivateResponse());
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class))).thenReturn(
        WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatus(
            Arrays.asList(new WholeSalePriceSkuStatusDto(ITEM_SKU, null, Constants.ACTIVE),
                new WholeSalePriceSkuStatusDto(ITEM_SKU_1, null, Constants.ACTIVE))).build());
    Mockito.when(productPricingOutbound.getWholesalePriceList(Mockito.anySet(), Mockito.anyMap())).thenReturn(Arrays
        .asList(WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.ACTIVE_STATUS).build(),
            WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_1).skuStatus(Constants.INACTIVE_STATUS).build()));
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
            .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals("Pre-live", editProductResponse.getReviewType());
  }

  @Test
  public void needRevisionSubmitPreliveTestWSNotChangedNoPath() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    productItemWholesalePrices.forEach(
        productItemWholesalePrice -> productItemWholesalePrice.setWholesalePriceActivated(true));
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, Constants.ACTIVE, ITEM_SKU_1, Constants.ACTIVE);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(
        productPricingOutbound.bulkActivateOrDeactivateSku(Mockito.anyList()))
        .thenReturn(new BulkActivateDeactivateResponse());
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class))).thenReturn(
        WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatus(
            Arrays.asList(new WholeSalePriceSkuStatusDto(ITEM_SKU, null, Constants.ACTIVE),
                new WholeSalePriceSkuStatusDto(ITEM_SKU_1, null, Constants.ACTIVE))).build());
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productPricingOutbound.getWholesalePriceList(Mockito.anySet(), Mockito.anyMap())).thenReturn(Arrays
        .asList(WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.ACTIVE_STATUS).build(),
            WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_1).skuStatus(Constants.INACTIVE_STATUS).build()));
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals("Pre-live", editProductResponse.getReviewType());
  }

  @Test
  public void needRevisionSubmitPreliveTest() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, DRAFT_STATE, ITEM_SKU_1, DRAFT_STATE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(
        productPricingOutbound.bulkActivateOrDeactivateSku(Mockito.anyList()))
        .thenReturn(new BulkActivateDeactivateResponse());
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class)))
        .thenReturn(WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatusMap(statusMap).build());
    Mockito.when(productPricingOutbound.getWholesalePriceList(Mockito.anySet(), Mockito.anyMap())).thenReturn(Arrays
        .asList(WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.ACTIVE_STATUS).build(),
            WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_1).skuStatus(Constants.INACTIVE_STATUS).build()));
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals("Pre-live", editProductResponse.getReviewType());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
  }

  @Test
  public void needRevisionSubmitPreliveTestForTrustedSellers() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, DRAFT_STATE, ITEM_SKU_1, DRAFT_STATE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
      .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
      .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
      .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
      .thenReturn(productItemWholesalePrices);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(true);
    Mockito.when(
        productPricingOutbound.bulkActivateOrDeactivateSku(Mockito.anyList()))
      .thenReturn(new BulkActivateDeactivateResponse());
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class)))
      .thenReturn(WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatusMap(statusMap).build());
    Mockito.when(productPricingOutbound.getWholesalePriceList(Mockito.anySet(), Mockito.anyMap())).thenReturn(Arrays
      .asList(WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.ACTIVE_STATUS).build(),
        WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_1).skuStatus(Constants.INACTIVE_STATUS).build()));
    Mockito.doNothing().when(productItemWholesalePriceService)
      .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
      .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
      .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    when(productLevel3Helper
      .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
      .thenReturn(restrictedKeywordsByFieldAndActionType);
    EditProductResponse editProductResponse = productServiceWrapper
      .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
      .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productItemWholesalePriceService)
      .saveWholesalePrice(Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
      .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
      .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
      .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
      .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals("Pre-live", editProductResponse.getReviewType());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
  }

  @Test
  public void performResultantActionBasedOnRestrictedKeywordsTest() throws Exception {
    this.profileResponse.setTrustedSeller(true);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    this.productServiceWrapper.performResultantActionBasedOnRestrictedKeywords(STORE_ID,
      PRODUCT_CODE,0,null,true,true,false, Collections.emptyList(),false,BUSINESS_PARTNER_CODE,
      null, null, null);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  public void performResultantActionBasedOnRestrictedKeywordsTestRejection_warehouseStockValidationTrueAndAvailableTrueEmptyItemCodes()
      throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper,
        "skipStraightforwardRejectionWarehouseStockValidation", true);
    this.profileResponse.setTrustedSeller(false);
    profileResponse.setCompany(CompanyDTO.builder().merchantType("TD").build());
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(productRepository.getCategoryRestrictedKeywordDetail(null))
        .thenReturn(categoryRestrictedKeywordResponse);

    this.productServiceWrapper.performResultantActionBasedOnRestrictedKeywords(STORE_ID,
        PRODUCT_CODE, 3, null, true, true, false, Collections.emptyList(), false,
        BUSINESS_PARTNER_CODE, null, null, null);

    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productRepository).getCategoryRestrictedKeywordDetail(null);
    verify(productWorkflowServiceWrapper).deleteProductCollection(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),  Mockito.anyBoolean());
  }

  @Test
  public void performResultantActionBasedOnRestrictedKeywordsTestRejection_warehouseStockValidationTrueAndCMSeller()
      throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper,
        "skipStraightforwardRejectionWarehouseStockValidation", true);
    this.profileResponse.setTrustedSeller(false);
    profileResponse.setCompany(CompanyDTO.builder().merchantType("CM").build());
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(productRepository.getCategoryRestrictedKeywordDetail(null))
        .thenReturn(categoryRestrictedKeywordResponse);

    this.productServiceWrapper.performResultantActionBasedOnRestrictedKeywords(STORE_ID,
        PRODUCT_CODE, 3, null, true, true, false, Collections.emptyList(), false,
        BUSINESS_PARTNER_CODE, null, null, null);

    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productRepository).getCategoryRestrictedKeywordDetail(null);
    verify(productWorkflowServiceWrapper).deleteProductCollection(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),  Mockito.anyBoolean());
  }

  @Test
  public void performResultantActionBasedOnRestrictedKeywordsTestRejection_warehouseStockValidationTrueAndAvailableTrue()
      throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper,
        "skipStraightforwardRejectionWarehouseStockValidation", true);
    this.profileResponse.setTrustedSeller(false);
    profileResponse.setCompany(CompanyDTO.builder().merchantType("TD").build());
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            inventoryOutbound.isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_1)))
        .thenReturn(false);
    Mockito.when(
            inventoryOutbound.isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_2)))
        .thenReturn(true);
    this.productServiceWrapper.performResultantActionBasedOnRestrictedKeywords(STORE_ID,
        PRODUCT_CODE, 3, null, true, true, false, Collections.emptyList(), false,
        BUSINESS_PARTNER_CODE, null, new LinkedHashSet<>(List.of(ITEM_CODE_1, ITEM_CODE_2)),
        EditProductResponse.builder().postLive(false).build());

    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(inventoryOutbound).isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_1));
    verify(inventoryOutbound).isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_2));
    verify(kafkaProducer).send(Mockito.eq(PRODUCT_INTERNAL_HISTORY_SAVE), Mockito.eq(PRODUCT_CODE),
        Mockito.any(InternalProductHistoryEventModel.class));
  }

  @Test
  public void performResultantActionBasedOnRestrictedKeywordsTestRejection_warehouseStockValidationTrueAndAvailableTruePostLiveTrue()
      throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper,
        "skipStraightforwardRejectionWarehouseStockValidation", true);
    this.profileResponse.setTrustedSeller(false);
    profileResponse.setCompany(CompanyDTO.builder().merchantType("TD").build());
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            inventoryOutbound.isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_1)))
        .thenReturn(false);
    Mockito.when(
            inventoryOutbound.isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_2)))
        .thenReturn(true);
    EditProductResponse editResponse =
        EditProductResponse.builder().postLive(true).reviewType(Constants.POST_LIVE_REVIEW_TYPE)
            .build();
    this.productServiceWrapper.performResultantActionBasedOnRestrictedKeywords(STORE_ID,
        PRODUCT_CODE, 3, null, true, true, false, Collections.emptyList(), false,
        BUSINESS_PARTNER_CODE, null, new LinkedHashSet<>(List.of(ITEM_CODE_1, ITEM_CODE_2)),
        editResponse);

    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(inventoryOutbound).isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_1));
    verify(inventoryOutbound).isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_2));
    verify(kafkaProducer).send(Mockito.eq(PRODUCT_INTERNAL_HISTORY_SAVE), Mockito.eq(PRODUCT_CODE),
        Mockito.any(InternalProductHistoryEventModel.class));
    assertTrue(editResponse.isPostLive());
  }

  @Test
  public void performResultantActionBasedOnRestrictedKeywordsTestRejection_warehouseStockValidationTrueAndAvailableTrueNullEditResponse()
      throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper,
        "skipStraightforwardRejectionWarehouseStockValidation", true);
    this.profileResponse.setTrustedSeller(false);
    profileResponse.setCompany(CompanyDTO.builder().merchantType("TD").build());
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            inventoryOutbound.isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_1)))
        .thenReturn(false);
    Mockito.when(
            inventoryOutbound.isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_2)))
        .thenReturn(true);

    this.productServiceWrapper.performResultantActionBasedOnRestrictedKeywords(STORE_ID,
        PRODUCT_CODE, 3, null, true, true, false, Collections.emptyList(), false,
        BUSINESS_PARTNER_CODE, null, new LinkedHashSet<>(List.of(ITEM_CODE_1, ITEM_CODE_2)),
        null);

    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(inventoryOutbound).isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_1));
    verify(inventoryOutbound).isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_2));
    verify(kafkaProducer).send(Mockito.eq(PRODUCT_INTERNAL_HISTORY_SAVE), Mockito.eq(PRODUCT_CODE),
        Mockito.any(InternalProductHistoryEventModel.class));
  }

  @Test
  public void performResultantActionBasedOnRestrictedKeywordsTestRejection_warehouseStockValidationTrueAndAvailableFalse()
      throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper,
        "skipStraightforwardRejectionWarehouseStockValidation", true);
    this.profileResponse.setTrustedSeller(false);
    profileResponse.setCompany(CompanyDTO.builder().merchantType("TD").build());
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            inventoryOutbound.isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_1)))
        .thenReturn(false);
    Mockito.when(
            inventoryOutbound.isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_2)))
        .thenReturn(false);
    Mockito.when(productRepository.getCategoryRestrictedKeywordDetail(null))
        .thenReturn(categoryRestrictedKeywordResponse);
    this.productServiceWrapper.performResultantActionBasedOnRestrictedKeywords(STORE_ID,
        PRODUCT_CODE, 3, null, true, true, false, Collections.emptyList(), false,
        BUSINESS_PARTNER_CODE, null, Set.of(ITEM_CODE_1, ITEM_CODE_2), EditProductResponse.builder().postLive(true).build());

    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(inventoryOutbound).isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_1));
    verify(inventoryOutbound).isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_2));
    verify(productRepository).getCategoryRestrictedKeywordDetail(null);
    verify(productWorkflowServiceWrapper).deleteProductCollection(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),  Mockito.anyBoolean());
  }

  @Test
  public void performResultantActionBasedOnRestrictedKeywordsTestRejection_warehouseStockValidationTrueAndInventoryFailure()
      throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper,
        "skipStraightforwardRejectionWarehouseStockValidation", true);
    this.profileResponse.setTrustedSeller(false);
    profileResponse.setCompany(CompanyDTO.builder().merchantType("TD").build());
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            inventoryOutbound.isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_1)))
        .thenThrow(new ApplicationRuntimeException());
    Mockito.when(productRepository.getCategoryRestrictedKeywordDetail(null))
        .thenReturn(categoryRestrictedKeywordResponse);
    this.productServiceWrapper.performResultantActionBasedOnRestrictedKeywords(STORE_ID,
        PRODUCT_CODE, 3, null, true, true, false, Collections.emptyList(), false,
        BUSINESS_PARTNER_CODE, null, new LinkedHashSet<>(List.of(ITEM_CODE_1, ITEM_CODE_2)), null);

    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(inventoryOutbound).isWarehouseStockPresent(Mockito.any(), Mockito.eq(ITEM_CODE_1));
    verify(productRepository).getCategoryRestrictedKeywordDetail(null);
    verify(productWorkflowServiceWrapper).deleteProductCollection(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),  Mockito.anyBoolean());
  }

  @Test
  public void needRevisionBusinessPartnerTest() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, DRAFT_STATE, ITEM_SKU_1, DRAFT_STATE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(
        productPricingOutbound.bulkActivateOrDeactivateSku(Mockito.anyList()))
        .thenReturn(new BulkActivateDeactivateResponse());
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productPricingOutbound.getWholesalePriceList(Mockito.anySet(), Mockito.anyMap())).thenReturn(Arrays
        .asList(WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.INACTIVE_STATUS).build(),
            WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_1).skuStatus(Constants.ACTIVE_STATUS).build()));
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class)))
        .thenReturn(WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatusMap(statusMap)
            .failedItemSkuToFailedReasonMap(new HashMap<>()).build());
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals("Pre-live", editProductResponse.getReviewType());
  }

  @Test
  public void needRevisionBusinessPartnerAutoRejectTest() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, DRAFT_STATE, ITEM_SKU_1, DRAFT_STATE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(
        productPricingOutbound.bulkActivateOrDeactivateSku(Mockito.anyList()))
        .thenReturn(new BulkActivateDeactivateResponse());
    restrictedKeywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    restrictedKeywordsByFieldAndActionType.setAction(3);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productPricingOutbound.getWholesalePriceList(Mockito.anySet(), Mockito.anyMap())).thenReturn(Arrays
        .asList(WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.INACTIVE_STATUS).build(),
            WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_1).skuStatus(Constants.ACTIVE_STATUS).build()));
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class)))
        .thenReturn(WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatusMap(statusMap)
            .failedItemSkuToFailedReasonMap(new HashMap<>()).build());
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productWorkflowServiceWrapper)
        .deleteProductCollection(STORE_ID, PRODUCT_CODE, categoryRestrictedKeywordResponse.getMessage(), true, true);
    Assertions.assertEquals("Pre-live", editProductResponse.getReviewType());
  }

  @Test
  public void needRevisionBusinessPartnerAutoReject1Test() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, DRAFT_STATE, ITEM_SKU_1, DRAFT_STATE);
    profileResponse.setTrustedSeller(true);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
            .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(
            productPricingOutbound.bulkActivateOrDeactivateSku(Mockito.anyList()))
        .thenReturn(new BulkActivateDeactivateResponse());
    restrictedKeywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    restrictedKeywordsByFieldAndActionType.setAction(3);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productPricingOutbound.getWholesalePriceList(Mockito.anySet(), Mockito.anyMap())).thenReturn(Arrays
        .asList(WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.INACTIVE_STATUS).build(),
            WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_1).skuStatus(Constants.ACTIVE_STATUS).build()));
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class)))
        .thenReturn(WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatusMap(statusMap)
            .failedItemSkuToFailedReasonMap(new HashMap<>()).build());
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
            .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals("Pre-live", editProductResponse.getReviewType());
  }

  @Test
  public void needRevisionSubmitScreeningPreliveTest() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    restrictedKeywordsByFieldAndActionType.setAction(1);
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class)))
        .thenReturn(new WholesalePriceBulkUpdateResponse());
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(false);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class),
            Mockito.any())).thenReturn(restrictedKeywordsByFieldAndActionType);
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).addProductToReviewCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productWfService)
        .deleteAllExistingWorkFlowAndCreateNewState(STORE_ID, PRODUCT_CODE, WorkflowStates.DRAFT.getValue());
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class),
            Mockito.any());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals("Pre-live", editProductResponse.getReviewType());
    Assertions.assertEquals(WorkflowStates.DRAFT.getValue(), productCollectionArgumentCaptor.getValue().getState());
    Assertions.assertFalse(productCollectionArgumentCaptor.getValue().isViewable());
    Assertions.assertFalse(productCollectionArgumentCaptor.getValue().isActivated());
  }

  @Test
  public void needRevisionSubmitScreeningPreliveWholesaleFalseTest() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    restrictedKeywordsByFieldAndActionType.setAction(1);
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, DRAFT_STATE, ITEM_SKU_1, DRAFT_STATE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
            .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class)))
        .thenReturn(WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatusMap(statusMap).build());
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
            .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(false);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class),
            Mockito.any())).thenReturn(restrictedKeywordsByFieldAndActionType);
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).addProductToReviewCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productWfService)
        .deleteAllExistingWorkFlowAndCreateNewState(STORE_ID, PRODUCT_CODE, WorkflowStates.DRAFT.getValue());
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class),
            Mockito.any());
  }

  @Test
  public void needRevisionSubmitPreliveEmptyWholesaleConfigTest() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setRestrictedKeywordsPresent(true);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Post-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(new ArrayList<>());
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(Mockito.any(ProductBusinessPartner.class));
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals("Pre-live", editProductResponse.getReviewType());
  }

  @Test
  public void needRevisionSubmitPostLiveTest() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    restrictedKeywordsByFieldAndActionType.setSkipAllActions(true);
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Post-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals("Post-live", editProductResponse.getReviewType());
  }

  @Test
  public void needRevisionSubmitPostLiveTestPublishContentImageQcEvent() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    productCollection.setRestrictedKeywordsDetected("[]");
    restrictedKeywordsByFieldAndActionType.setSkipAllActions(true);
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Post-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
            .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productBusinessPartnerService
            .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productService).publishImageQcEventForContentEdit(Mockito.anyList(), Mockito.any(),Mockito.any(), Mockito.anyBoolean());
    Assertions.assertEquals("Post-live", editProductResponse.getReviewType());
  }

  @Test
  public void needRevisionSubmitPostLiveTest_forTrustedSellers() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Post-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
      .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
      .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
      .thenReturn(Arrays.asList(config));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(true);
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
      .thenReturn(new ArrayList<>());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
      .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    when(productLevel3Helper
      .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
      .thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
      .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    EditProductResponse editProductResponse = productServiceWrapper
      .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService)
      .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
      .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
      .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
      .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productLevel3Helper)
      .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals("Post-live", editProductResponse.getReviewType());
  }

  @Test
  public void needRevisionSubmitPostLiveCategoryChangeTest() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    restrictedKeywordsByFieldAndActionType.setAction(0);
    restrictedKeywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    categoryRestrictedKeywordResponse.setDestinationCategory(DESTINATION_CATEGORY);
    restrictedKeywordsByFieldAndActionType.setSkipAllActions(true);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    businessPartner.setCategoryName(CATEGORY_NAME);
    businessPartner.setCategoryCode(DESTINATION_CATEGORY);
    businessPartner.setState(STATE_IN_PROGRESS);
    businessPartner.setExpectedActivationDate(new Date());
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Post-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(
        objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class),
            Mockito.any())).thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(businessPartner));
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    when(productOutbound.updateProductCategory(any(), any(), anyBoolean(),anyBoolean()))
        .thenReturn(categorySummaryResponse);
    doNothing().when(productLevel1HistoryService)
        .saveProductHistory(any(), eq("perubahan kategori otomatis"), any(), any());
    Mockito.when(xProductOutbound.generateProductScoreByProductSkuOrProductCode(any(), any(), eq(true)))
        .thenReturn(new L3VersionResponse());
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(businessPartner);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class),
            Mockito.any());
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productOutbound).updateProductCategory(any(), any(), anyBoolean(),anyBoolean());
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(any(), any(), eq(true));
    Mockito.verify(this.kafkaProducer).send(eq(PRODUCT_INTERNAL_HISTORY_SAVE), any(),
        any(InternalProductHistoryEventModel.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY), any(),
        any(AuditTrailListRequest.class));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productService).getProfileResponse(anyString());
    Assertions.assertEquals("Post-live", editProductResponse.getReviewType());
    Assertions.assertEquals(categoryRestrictedKeywordResponse.getDestinationCategory(), productCollection.getCategoryCode());
  }

  @Test
  public void needRevisionSubmitPostLiveCategoryChangeWithSameDestinationCategoryTest() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    restrictedKeywordsByFieldAndActionType.setAction(0);
    restrictedKeywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    restrictedKeywordsByFieldAndActionType.setSkipAllActions(true);
    categoryRestrictedKeywordResponse.setDestinationCategory(CATEGORY_CODE);
    ProductBusinessPartner businessPartner = new ProductBusinessPartner();
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Post-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    Mockito.when(
        objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class),
            Mockito.any())).thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(businessPartner));
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(businessPartner);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class),
            Mockito.any());
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals("Post-live", editProductResponse.getReviewType());
    Assertions.assertEquals(categoryRestrictedKeywordResponse.getDestinationCategory(), productCollection.getCategoryCode());
  }

  @Test
  public void needRevisionSubmitPostLiveAlreadyInAutoNeedRevisionTest() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setAutoNeedRevision(true);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    restrictedKeywordsByFieldAndActionType.setSkipAllActions(true);
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Post-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    restrictedKeywordsByFieldAndActionType.setAction(2);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals("Post-live", editProductResponse.getReviewType());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());

  }

  @Test
  public void needRevisionSubmitImageRevisedTest() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    restrictedKeywordsByFieldAndActionType.setSkipAllActions(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.getImages().get(0).setRevised(true);
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Post-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(productDetailResponse);
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productPublisherService.publishReviseImageResizeEvent(Mockito.any(EditedImageResizeEvent.class)))
        .thenReturn(new EditedImageResizeEvent());
    Mockito.when(fileStorageService.getRevisedImageRequests(Mockito.anyList(), Mockito.any())).thenReturn(Arrays.asList(new ImageRequest()));
    Mockito.when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse,
        productCollection.getCategoryCode())).thenReturn(restrictedKeywordsByFieldAndActionType);
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));

    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productPublisherService).publishReviseImageResizeEvent(Mockito.any(EditedImageResizeEvent.class));
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse,
        productCollection.getCategoryCode());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Assertions.assertEquals("Post-live", editProductResponse.getReviewType());
  }

  @Test
  public void needRevisionSubmitNotNeedRevisionTest() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.DRAFT.getValue());
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.getImages().get(0).setRevised(true);
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Post-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(productDetailResponse);
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productPublisherService.publishReviseImageResizeEvent(Mockito.any(EditedImageResizeEvent.class))).thenReturn(new EditedImageResizeEvent());

    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Assertions.assertEquals("Pre-live", editProductResponse.getReviewType());
  }

  @Test
  public void autoApproveProductNullTest() throws Exception {
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(null);
    productServiceWrapper.autoApproveProduct(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void autoApproveProductReviewPendingTest() throws Exception {
    productCollection.setReviewPending(false);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productServiceWrapper.autoApproveProduct(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void autoApproveProductEditedTest() throws Exception {
    productCollection.setReviewPending(true);
    productCollection.setEdited(true);
    productCollection.setPostLive(true);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productServiceWrapper.autoApproveProduct(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void autoApprovePostLiveTest() throws Exception {
    productCollection.setReviewPending(true);
    productCollection.setPostLive(false);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productServiceWrapper.autoApproveProduct(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void autoApproveProductNotActiveTest() throws Exception {
    productCollection.setReviewPending(true);
    productCollection.setEdited(false);
    productCollection.setPostLive(true);
    productCollection.setState(WorkflowStates.DRAFT.getValue());
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productServiceWrapper.autoApproveProduct(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productPublisherService)
        .publishProductQCRetryEvent(new ProductQCRetryEvent(STORE_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE));
    Mockito.verify(productService).saveProductHistory(Mockito.eq(PRODUCT_CODE), productHistoryArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskRepositoryBean).sendProductBackToVendor(STORE_ID, PRODUCT_CODE);
    Assertions.assertEquals(SaveHistoryConstants.CANNOT_AUTO_APPROVED, productHistoryArgumentCaptor.getValue().getDescription());
    Assertions.assertEquals(SaveHistoryConstants.NOT_ACTIVE_ERROR_MESSAGE, productHistoryArgumentCaptor.getValue().getNotes());
  }

  @Test
  public void autoApproveProductAutoNeedRevisionTest() throws Exception {
    productCollection.setReviewPending(true);
    productCollection.setEdited(false);
    productCollection.setPostLive(true);
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    productCollection.setAutoNeedRevision(true);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productServiceWrapper.autoApproveProduct(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void autoApproveProductTest() throws Exception {
    productCollection.setReviewPending(true);
    productCollection.setEdited(false);
    productCollection.setPostLive(true);
    productCollection.setRestrictedKeywordsPresent(true);
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productServiceWrapper.autoApproveProduct(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productOutbound).deleteOriginalImages(PRODUCT_CODE);
    Mockito.verify(productService).saveProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productService).updateSolrProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productService)
        .deleteFromReviewProductCollection(Collections.singletonList(productCollection.getId()));
    Mockito.verify(productService).removeProductFromPDT(PRODUCT_CODE);
    Mockito.verify(productService).saveProductHistory(Mockito.eq(PRODUCT_CODE), productHistoryArgumentCaptor.capture());
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(STORE_ID, PRODUCT_CODE);
    Assertions.assertFalse(productCollectionArgumentCaptor.getValue().isReviewPending());
    Assertions.assertFalse(productCollectionArgumentCaptor.getValue().isRestrictedKeywordsPresent());
    Assertions.assertEquals(SaveHistoryConstants.PRODUCT_AUTO_APPROVAL_SUCCESSFUL,
        productHistoryArgumentCaptor.getValue().getDescription());
  }

  @Test
  public void retrySkipReviewProductActivationTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, false)).thenReturn(productDetailResponse);
    Mockito.doNothing().when(approveProductService).processImage(productDetailResponse);
    productServiceWrapper.retrySkipReviewProductActivation(PRODUCT_CODE);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    verify(approveProductService).processImage(productDetailResponse);
  }

  @Test
  public void autoNeedRevisionProductNullTest() throws Exception {
    productCollection.setReviewPending(true);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, true, true, true, false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void retryAutoNeedRevisionNullTest() throws Exception {
    RetryAutoNeedRevisionResponse response =
        productServiceWrapper.retryAutoNeedRevision(retryNeedRevisionRequest, false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void retryAutoNeedRevisionReviewPendingFalseTest() throws Exception {
    productCollection.setReviewPending(false);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    RetryAutoNeedRevisionResponse response =
        productServiceWrapper.retryAutoNeedRevision(retryNeedRevisionRequest, false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void retryAutoNeedRevisionPostLiveNotActiveTest() throws Exception {
    productCollection.setReviewPending(true);
    productCollection.setPostLive(true);
    productCollection.setState(Constants.IN_PROGRESS_STATE);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    RetryAutoNeedRevisionResponse response =
        productServiceWrapper.retryAutoNeedRevision(retryNeedRevisionRequest, false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertFalse(response.isProductActive());
  }

  @Test
  public void retryAutoNeedRevisionPostLiveTest() throws Exception {
    autoNeedRevisionRequest.setState(WorkflowWebState.NEED_CORRECTION.name());
    autoNeedRevisionRequest.setReason(NOTES);
    autoNeedRevisionRequest.setImageReason(Constants.OTHERS_NOTES_REASON);
    productCollection.setReviewPending(true);
    productCollection.setPostLive(true);
    productCollection.setState(Constants.ACTIVE);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(
        productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(productCollection.getProductCode(), false))
        .thenReturn(true);
    RetryAutoNeedRevisionResponse response =
        productServiceWrapper.retryAutoNeedRevision(retryNeedRevisionRequest, false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productDistributionTaskRepositoryBean)
        .sendProductToAutoNeedRevision(autoNeedRevisionRequestArgumentCaptor.capture(), anyBoolean());
    verify(needCorrectionService)
        .sendForNeedCorrectionByProductCode(autoNeedRevisionDomainEvent.getProductCode(), true, autoNeedRevisionRequest,
            true);
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(response.isProductActive());
  }

  @Test
  public void retryAutoNeedRevisionPostLiveNotInPDTTest() throws Exception {
    autoNeedRevisionRequest.setState(WorkflowWebState.NEED_CORRECTION.name());
    autoNeedRevisionRequest.setReason(NOTES);
    autoNeedRevisionRequest.setImageReason(Constants.OTHERS_NOTES_REASON);
    productCollection.setReviewPending(true);
    productCollection.setPostLive(true);
    productCollection.setState(Constants.ACTIVE);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(
        productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(productCollection.getProductCode(), false))
        .thenReturn(false);
    RetryAutoNeedRevisionResponse response =
        productServiceWrapper.retryAutoNeedRevision(retryNeedRevisionRequest, false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(needCorrectionService)
        .sendForNeedCorrectionByProductCode(autoNeedRevisionDomainEvent.getProductCode(), true, autoNeedRevisionRequest,
            true);
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(response.isProductActive());
  }

  @Test
  public void retryAutoNeedRevisionPreLiveActiveTest() throws Exception {
    productCollection.setReviewPending(true);
    productCollection.setPostLive(false);
    productCollection.setState(Constants.ACTIVE);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    RetryAutoNeedRevisionResponse response =
        productServiceWrapper.retryAutoNeedRevision(retryNeedRevisionRequest, false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(response.isProductActive());
  }

  @Test
  public void retryAutoNeedRevisionPreLiveTest() throws Exception {
    productCollection.setReviewPending(true);
    productCollection.setPostLive(false);
    productCollection.setState(Constants.IN_PROGRESS_STATE);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    RetryAutoNeedRevisionResponse response =
        productServiceWrapper.retryAutoNeedRevision(retryNeedRevisionRequest, false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    NeedRevisionNotes needRevisionNotes = new NeedRevisionNotes();
    needRevisionNotes.setVendorErrorFields(new ArrayList<>());
    needRevisionNotes.setAllVariants(true);
    needRevisionNotes.setImagesAdditionalNotes(NOTES);
    needRevisionNotes.setImageReason(Collections.singletonList(Constants.OTHERS_NOTES_REASON));
    needRevisionNotes.setItemNotes(new ArrayList<>());
    verify(productWfService)
        .returnForCorrection(autoNeedRevisionDomainEvent.getProductCode(), NOTES, needRevisionNotes, true, true, true);
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(response.isProductActive());
  }

  @Test
  public void autoNeedRevisionProductReviewPendingFalseTest() throws Exception {
    productCollection.setReviewPending(false);
    Mockito.when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, true, true, true, false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void autoNeedRevisionProductPostLiveNotActiveTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setReviewPending(true);
    productCollection.setState(WorkflowStates.IN_VENDOR.getValue());
    when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    when(productImagePredictionService.findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1))
        .thenReturn(productImagePrediction);
    when(objectMapper.writeValueAsString(any())).thenReturn(
      SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE_EN + PREDICTION_DISPLAY_NAME_1
        + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE_EN);
    when(productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(productCollection.getProductCode(), false))
        .thenReturn(true);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, true, true, true, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService).findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1);
    verify(productService).publishProductActionRetryEvent(
        new ProductActionRetryEvent(autoNeedRevisionDomainEvent.getStoreId(),
            autoNeedRevisionDomainEvent.getProductCode(), Constants.AUTO_NEED_REVISION_ACTION,
            SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE_EN + PREDICTION_DISPLAY_NAME_1
                + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE_EN));
    verify(productDistributionTaskRepositoryBean).checkIfProductExistsInPDT(productCollection.getProductCode(), false);
  }

  @Test
  public void autoNeedRevisionProductPostLiveNotActiveInMerchantTest() throws Exception {
    productCollection.setReviewPending(true);
    profileResponse.getCompany().setInternationalFlag(false);
    productCollection.setPostLive(true);
    productCollection.setState(WorkflowStates.IN_VENDOR.getValue());
    when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    when(productImagePredictionService.findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1))
        .thenReturn(productImagePrediction);
    when(objectMapper.writeValueAsString(any())).thenReturn(
      SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
        + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE);
    when(productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(productCollection.getProductCode(), false))
        .thenReturn(true);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, true, true, true, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService).findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1);
    verify(productDistributionTaskRepositoryBean).checkIfProductExistsInPDT(productCollection.getProductCode(), false);
    verify(productService).publishProductActionRetryEvent(
        new ProductActionRetryEvent(autoNeedRevisionDomainEvent.getStoreId(),
            autoNeedRevisionDomainEvent.getProductCode(), Constants.AUTO_NEED_REVISION_ACTION,
            SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
                + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE));
  }

  @Test
  public void autoNeedRevisionProductPostLiveActiveTest() throws Exception {
    autoNeedRevisionRequest.setReason(SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE);
    autoNeedRevisionRequest.setState(WorkflowWebState.NEED_CORRECTION.name());
    autoNeedRevisionRequest.setImageReason(SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_IMAGE_IN + PREDICTION_DISPLAY_NAME_1_IN);
    autoNeedRevisionRequest.setEdited(true);
    autoNeedRevisionRequest.setEditedState(REVIEW_TYPE_IMAGE);
    profileResponse.getCompany().setInternationalFlag(false);
    productCollection.setPostLive(true);
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    productCollection.setReviewType(REVIEW_TYPE_IMAGE);
    productCollection.setEdited(true);
    productCollection.setReviewPending(true);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
        PRODUCT_CODE)).thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    when(productImagePredictionService.findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1)).thenReturn(productImagePrediction);
    when(productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(productCollection.getProductCode(), false)).thenReturn(true);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, true, true, true, false);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
        PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService).findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1);
    verify(productDistributionTaskRepositoryBean).sendProductToAutoNeedRevision(autoNeedRevisionRequestArgumentCaptor.capture(), anyBoolean());
    verify(needCorrectionService).sendForNeedCorrectionByProductCode(autoNeedRevisionDomainEvent.getProductCode(), true,
        autoNeedRevisionRequest, true);
    Assertions.assertEquals(productCollection.getProductCode(),
        autoNeedRevisionRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(WorkflowWebState.NEED_CORRECTION.name(),
        autoNeedRevisionRequestArgumentCaptor.getValue().getState());
    Assertions.assertEquals(REVIEW_TYPE_IMAGE, autoNeedRevisionRequestArgumentCaptor.getValue().getEditedState());
    Assertions.assertTrue(autoNeedRevisionRequestArgumentCaptor.getValue().isEdited());
    Assertions.assertEquals(SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
            + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE,
        autoNeedRevisionRequestArgumentCaptor.getValue().getReason());
  }

  @Test
  public void autoNeedRevisionProductEditedTest() throws Exception {
    autoNeedRevisionRequest.setReason(SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
      + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE);
    autoNeedRevisionRequest.setState(WorkflowWebState.NEED_CORRECTION.name());
    autoNeedRevisionRequest.setImageReason(SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_IMAGE_IN + PREDICTION_DISPLAY_NAME_1_IN);
    autoNeedRevisionRequest.setEdited(true);
    autoNeedRevisionRequest.setEditedState(REVIEW_TYPE_IMAGE);
    profileResponse.getCompany().setInternationalFlag(false);
    productCollection.setPostLive(false);
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    productCollection.setReviewType(REVIEW_TYPE_IMAGE);
    productCollection.setEdited(true);
    productCollection.setReviewPending(true);
    when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    when(productImagePredictionService.findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1))
        .thenReturn(productImagePrediction);
    Mockito.when(productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(productCollection.getProductCode(), false)).thenReturn(true);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, true, true, true, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService).findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1);
    verify(productDistributionTaskRepositoryBean)
        .sendProductToAutoNeedRevision(autoNeedRevisionRequestArgumentCaptor.capture(),anyBoolean());
    verify(needCorrectionService).sendForNeedCorrectionByProductCode(autoNeedRevisionDomainEvent.getProductCode(), true,
      autoNeedRevisionRequest, true);
    verify(productDistributionTaskRepositoryBean).checkIfProductExistsInPDT(productCollection.getProductCode(), false);
    Assertions.assertEquals(productCollection.getProductCode(),
        autoNeedRevisionRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(WorkflowWebState.NEED_CORRECTION.name(),
        autoNeedRevisionRequestArgumentCaptor.getValue().getState());
    Assertions.assertEquals(REVIEW_TYPE_IMAGE, autoNeedRevisionRequestArgumentCaptor.getValue().getEditedState());
    Assertions.assertTrue(autoNeedRevisionRequestArgumentCaptor.getValue().isEdited());
    Assertions.assertEquals(SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
            + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE,
        autoNeedRevisionRequestArgumentCaptor.getValue().getReason());
  }

  @Test
  public void autoNeedRevisionProductEditedFullyApprovedTest() throws Exception {
    autoNeedRevisionRequest.setReason(SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
        + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE);
    autoNeedRevisionRequest.setState(WorkflowWebState.NEED_CORRECTION.name());
    autoNeedRevisionRequest
        .setImageReason(SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_IMAGE_IN + PREDICTION_DISPLAY_NAME_1_IN);
    autoNeedRevisionRequest.setEdited(true);
    autoNeedRevisionRequest.setEditedState(REVIEW_TYPE_IMAGE);
    profileResponse.getCompany().setInternationalFlag(false);
    productCollection.setPostLive(false);
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    productCollection.setReviewType(REVIEW_TYPE_IMAGE);
    productCollection.setEdited(true);
    productCollection.setReviewPending(true);
    when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    when(productImagePredictionService.findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1))
        .thenReturn(productImagePrediction);
    Mockito.when(
        productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(productCollection.getProductCode(), false))
        .thenReturn(false);
    autoNeedRevisionDomainEvent.setNotes(NOTES);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, true, true, true, true, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService).findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1);
    verify(needCorrectionService)
        .takeDownNeedForCorrectionProduct(autoNeedRevisionDomainEvent.getProductCode(), new HashMap<>(), null);
    verify(productDistributionTaskRepositoryBean).checkIfProductExistsInPDT(productCollection.getProductCode(), false);
    NeedRevisionNotes needRevisionNotes = new NeedRevisionNotes();
    needRevisionNotes.setAllVariants(true);
    needRevisionNotes.setContentAdditionalNotes(NOTES);
    needRevisionNotes.setItemNotes(new ArrayList<>());
    needRevisionNotes.setVendorErrorFields(new ArrayList<>());
    needRevisionNotes.setVendorNotes(Collections.singletonList(Constants.OTHERS_PRE_LIVE_NOTES_REASON));
    verify(needCorrectionService)
        .updateStateInPBPAndSendProductToNeedRevision(autoNeedRevisionDomainEvent.getProductCode(), NOTES, needRevisionNotes, true, true, true,
            productCollection);
  }

  @Test
  public void autoNeedRevisionProductEditedFullyApprovedMigratedProductTest() throws Exception {
    autoNeedRevisionRequest.setReason(SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
        + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE);
    autoNeedRevisionRequest.setState(WorkflowWebState.NEED_CORRECTION.name());
    autoNeedRevisionRequest
        .setImageReason(SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_IMAGE_IN + PREDICTION_DISPLAY_NAME_1_IN);
    autoNeedRevisionRequest.setEdited(true);
    autoNeedRevisionRequest.setEditedState(REVIEW_TYPE_IMAGE);
    profileResponse.getCompany().setInternationalFlag(false);
    productCollection.setPostLive(false);
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    productCollection.setReviewType(REVIEW_TYPE_IMAGE);
    productCollection.setEdited(true);
    productCollection.setReviewPending(true);
    productCollection.setProductCreationType(ProductCreationType.MIGRATION.getProductCreationType());
    when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    when(productImagePredictionService.findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1))
        .thenReturn(productImagePrediction);
    Mockito.when(
            productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(productCollection.getProductCode(), false))
        .thenReturn(false);
    when(productOutbound.getProductDetailByProductCode(productCollection.getProductCode(), false, false)).thenReturn(getProductDetailResponseWithImagesAddItemImage());
    autoNeedRevisionDomainEvent.setNotes(NOTES);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, true, true, true, true, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService).findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1);
    verify(needCorrectionService)
        .takeDownNeedForCorrectionProduct(Mockito.eq(autoNeedRevisionDomainEvent.getProductCode()), Mockito.anyMap(),
            Mockito.any());
    verify(productDistributionTaskRepositoryBean).checkIfProductExistsInPDT(productCollection.getProductCode(), false);
    NeedRevisionNotes needRevisionNotes = new NeedRevisionNotes();
    needRevisionNotes.setAllVariants(true);
    needRevisionNotes.setContentAdditionalNotes(NOTES);
    needRevisionNotes.setItemNotes(new ArrayList<>());
    needRevisionNotes.setVendorErrorFields(new ArrayList<>());
    needRevisionNotes.setVendorNotes(Collections.singletonList(Constants.OTHERS_PRE_LIVE_NOTES_REASON));
    verify(needCorrectionService)
        .updateStateInPBPAndSendProductToNeedRevision(autoNeedRevisionDomainEvent.getProductCode(), NOTES, needRevisionNotes, true, true, true,
            productCollection);
    verify(productOutbound).getProductDetailByProductCode(productCollection.getProductCode(), false, false);
  }

  @Test
  public void autoNeedRevisionProductPreLiveTest() throws Exception {
    productCollection.setReviewPending(true);
    profileResponse.getCompany().setInternationalFlag(false);
    productCollection.setPostLive(false);
    productCollection.setState(WorkflowStates.IN_VENDOR.getValue());
    productCollection.setReviewType(REVIEW_TYPE_IMAGE);
    when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    when(productImagePredictionService.findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1))
        .thenReturn(productImagePrediction);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, true, true, true, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService).findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1);
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    NeedRevisionNotes needRevisionNotes = new NeedRevisionNotes();
    needRevisionNotes.setAllVariants(true);
    needRevisionNotes.setItemNotes(new ArrayList<>());
    needRevisionNotes.setVendorErrorFields(new ArrayList<>());
    needRevisionNotes.setImagesAdditionalNotes(
        SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
            + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE);
    needRevisionNotes.setImageReason(Collections.singletonList(
      SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_IMAGE_IN + PREDICTION_DISPLAY_NAME_1_IN));
    verify(needCorrectionService)
        .updateStateInPBPAndSendProductToNeedRevision(autoNeedRevisionDomainEvent.getProductCode(),
      SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
        + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE, needRevisionNotes,
        true, true, true, productCollection);
  }

  @Test
  public void autoNeedRevisionProductPreLiveSkipScreeningTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "isSkipScreeningSwitch", true);
    productCollection.setReviewPending(true);
    profileResponse.getCompany().setInternationalFlag(false);
    productCollection.setPostLive(false);
    productCollection.setState(WorkflowStates.IN_VENDOR.getValue());
    when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    when(productImagePredictionService.findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1))
        .thenReturn(productImagePrediction);
    autoNeedRevisionRequest.setState(WorkflowWebState.NEED_CORRECTION.name());
    autoNeedRevisionRequest.setReason(SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
        + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE);
    autoNeedRevisionRequest
        .setImageReason(SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_IMAGE_IN + PREDICTION_DISPLAY_NAME_1_IN);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, false, true, true, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService).findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1);
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    NeedRevisionNotes needRevisionNotes = new NeedRevisionNotes();
    needRevisionNotes.setAllVariants(true);
    needRevisionNotes.setImagesAdditionalNotes(
        SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
            + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE);
    needRevisionNotes.setImageReason(Collections.singletonList(
        SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_IMAGE_IN + PREDICTION_DISPLAY_NAME_1_IN));
    verify(needCorrectionService)
        .sendForNeedCorrectionByProductCode(autoNeedRevisionDomainEvent.getProductCode(), true,
            autoNeedRevisionRequest, true);
    verify(productDistributionTaskRepositoryBean).sendProductToAutoNeedRevision(autoNeedRevisionRequest, false);
  }

  @Test
  public void autoNeedRevisionProductPreLiveSkipScreeningActionFalseTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "isSkipScreeningSwitch", true);
    productCollection.setReviewPending(true);
    profileResponse.getCompany().setInternationalFlag(false);
    productCollection.setPostLive(false);
    productCollection.setState(WorkflowStates.IN_VENDOR.getValue());
    when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    when(productImagePredictionService.findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1))
        .thenReturn(productImagePrediction);
    autoNeedRevisionRequest.setState(WorkflowWebState.NEED_CORRECTION.name());
    autoNeedRevisionRequest.setReason(SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
        + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE);
    autoNeedRevisionRequest
        .setImageReason(SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_IMAGE_IN + PREDICTION_DISPLAY_NAME_1_IN);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, true, true, true, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService).findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1);
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    NeedRevisionNotes needRevisionNotes = new NeedRevisionNotes();
    needRevisionNotes.setAllVariants(true);
    needRevisionNotes.setVendorErrorFields(new ArrayList<>());
    needRevisionNotes.setItemNotes(new ArrayList<>());
    needRevisionNotes.setImagesAdditionalNotes(
        SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
            + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE);
    needRevisionNotes.setImageReason(Collections.singletonList(
        SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_IMAGE_IN + PREDICTION_DISPLAY_NAME_1_IN));
    verify(needCorrectionService)
        .updateStateInPBPAndSendProductToNeedRevision(autoNeedRevisionDomainEvent.getProductCode(),
        SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
            + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE, needRevisionNotes, true, true, true,
            productCollection);
  }

  @Test
  public void autoNeedRevisionProductPreLiveActiveTest() throws Exception {
    profileResponse.getCompany().setInternationalFlag(false);
    productCollection.setPostLive(false);
    productCollection.setReviewPending(true);
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    productCollection.setReviewType(REVIEW_TYPE_IMAGE);
    when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    when(productImagePredictionService.findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1))
        .thenReturn(productImagePrediction);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, true, true, true, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService).findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1);
  }

  @Test
  public void retryAutoNeedRevisionWithImageReasonPostLiveTest() throws Exception {
    autoNeedRevisionRequest.setState(WorkflowWebState.NEED_CORRECTION.name());
    autoNeedRevisionRequest.setReason(NOTES);
    autoNeedRevisionRequest.setImageReason(PREDICTION_DISPLAY_NAME_1_IN);
    productCollection.setReviewPending(true);
    productCollection.setPostLive(true);
    productCollection.setState(Constants.ACTIVE);
    needRevisionReasonRequest.setNotes(NOTES);
    needRevisionReasonRequest.setImageNotes(PREDICTION_DISPLAY_NAME_1_IN);
    retryNeedRevisionRequest.setNotes(NOTES);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(objectMapper.readValue((String)  any(), eq(NeedRevisionReasonRequest.class)))
        .thenReturn(needRevisionReasonRequest);
    Mockito.when(productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(productCollection.getProductCode(), false)).thenReturn(true);
    RetryAutoNeedRevisionResponse response = productServiceWrapper.retryAutoNeedRevision(retryNeedRevisionRequest, false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productDistributionTaskRepositoryBean)
        .sendProductToAutoNeedRevision(autoNeedRevisionRequestArgumentCaptor.capture(), anyBoolean());
    verify(needCorrectionService).sendForNeedCorrectionByProductCode(autoNeedRevisionDomainEvent.getProductCode(), true,
        autoNeedRevisionRequest, true);
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(response.isProductActive());
  }

  @Test
  public void autoNeedRevisionProductPreLiveMultiplePredictionTest() throws Exception {
    productCollection.setReviewPending(true);
    profileResponse.getCompany().setInternationalFlag(false);
    productCollection.setPostLive(false);
    productCollection.setState(WorkflowStates.IN_VENDOR.getValue());
    productCollection.setReviewType(REVIEW_TYPE_IMAGE);
    List<String> predictionList = new ArrayList<>();
    predictionList.add(PREDICTION_NAME_1);
    predictionList.add(PREDICTION_NAME_3);
    autoNeedRevisionDomainEvent.setPredictionTypeList(predictionList);
    when(productCollectionRepository
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    when(productImagePredictionService
      .findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1))
      .thenReturn(productImagePrediction);
    when(productImagePredictionService
      .findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_3))
      .thenReturn(productImagePredictionBlur);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, true, true, true, false);
    verify(productCollectionRepository)
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService)
      .findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1);
    verify(productImagePredictionService)
      .findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_3);
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    NeedRevisionNotes needRevisionNotes = new NeedRevisionNotes();
    needRevisionNotes.setAllVariants(true);
    needRevisionNotes.setVendorErrorFields(new ArrayList<>());
    needRevisionNotes.setItemNotes(new ArrayList<>());
    needRevisionNotes.setImagesAdditionalNotes(
      SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
        + Constants.COMMA + PREDICTION_DISPLAY_NAME_3_IN
        + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE);
    needRevisionNotes.setImageReason(Collections.singletonList(
      SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_BLUR_IMAGE_IN
        + SaveHistoryConstants.AUTO_QC_NEED_REVISION_POST_MESSAGE_FOR_MULTIPLE_PREDICTION_IN
        + PREDICTION_DISPLAY_NAME_1_IN));
    verify(needCorrectionService)
        .updateStateInPBPAndSendProductToNeedRevision(autoNeedRevisionDomainEvent.getProductCode(),
      SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_1_IN
        + Constants.COMMA + PREDICTION_DISPLAY_NAME_3_IN
        + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE, needRevisionNotes, true, true, true,
            productCollection);
  }

  @Test
  public void autoNeedRevisionProductPreLiveBlurPredictionTest() throws Exception {
    productCollection.setReviewPending(true);
    profileResponse.getCompany().setInternationalFlag(false);
    productCollection.setPostLive(false);
    productCollection.setState(WorkflowStates.IN_VENDOR.getValue());
    productCollection.setReviewType(REVIEW_TYPE_IMAGE);
    List<String> predictionList = new ArrayList<>();
    predictionList.add(PREDICTION_NAME_3);
    autoNeedRevisionDomainEvent.setPredictionTypeList(predictionList);
    when(productCollectionRepository
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    when(productImagePredictionService
      .findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_3))
      .thenReturn(productImagePredictionBlur);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, true, true, true,false);
    verify(productCollectionRepository)
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService)
      .findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_3);
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    NeedRevisionNotes needRevisionNotes = new NeedRevisionNotes();
    needRevisionNotes.setAllVariants(true);
    needRevisionNotes.setVendorErrorFields(new ArrayList<>());
    needRevisionNotes.setImagesAdditionalNotes(
      SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_3_IN
        + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE);
    needRevisionNotes.setImageReason(Collections
      .singletonList(SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_BLUR_IMAGE_IN));
    needRevisionNotes.setItemNotes(new ArrayList<>());
    verify(needCorrectionService)
        .updateStateInPBPAndSendProductToNeedRevision(autoNeedRevisionDomainEvent.getProductCode(),
            SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE + PREDICTION_DISPLAY_NAME_3_IN
                + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE, needRevisionNotes, true, true, true,
            productCollection);
  }

  @Test
  public void autoNeedRevisionProductContentTest() throws Exception {
    productCollection.setReviewPending(true);
    profileResponse.getCompany().setInternationalFlag(false);
    productCollection.setPostLive(false);
    productCollection.setState(WorkflowStates.IN_VENDOR.getValue());
    productCollection.setReviewType(REVIEW_TYPE_IMAGE);
    List<String> predictionList = new ArrayList<>();
    predictionList.add(PREDICTION_NAME_3);
    autoNeedRevisionDomainEvent.setPredictionTypeList(predictionList);
    when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    when(productImagePredictionService
        .findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_3))
        .thenReturn(productImagePredictionBlur);
    autoNeedRevisionDomainEvent.setNotes(NOTES);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, true, true, true, true, false);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService)
        .findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_3);
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    NeedRevisionNotes needRevisionNotes = new NeedRevisionNotes();
    needRevisionNotes.setVendorErrorFields(new ArrayList<>());
    needRevisionNotes.setAllVariants(true);
    needRevisionNotes.setContentAdditionalNotes(NOTES);
    needRevisionNotes.setVendorNotes(Collections.singletonList(Constants.OTHERS_PRE_LIVE_NOTES_REASON));
    needRevisionNotes.setItemNotes(new ArrayList<>());
    verify(needCorrectionService)
        .updateStateInPBPAndSendProductToNeedRevision(autoNeedRevisionDomainEvent.getProductCode(), NOTES, needRevisionNotes, true, true, true,
            productCollection);
  }

  @Test
  public void autoNeedRevisionProductPreLiveBlurPrediction_InternationalSellerTest() throws Exception {
    productCollection.setReviewPending(true);
    profileResponse.getCompany().setInternationalFlag(true);
    productCollection.setPostLive(false);
    productCollection.setState(WorkflowStates.IN_VENDOR.getValue());
    productCollection.setReviewType(REVIEW_TYPE_IMAGE);
    List<String> predictionList = new ArrayList<>();
    predictionList.add(PREDICTION_NAME_3);
    autoNeedRevisionDomainEvent.setPredictionTypeList(predictionList);
    when(productCollectionRepository
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    when(productImagePredictionService
      .findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_3))
      .thenReturn(productImagePredictionBlurEn);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, true, true, true, false);
    verify(productCollectionRepository)
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService)
      .findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_3);
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    NeedRevisionNotes needRevisionNotes = new NeedRevisionNotes();
    needRevisionNotes.setAllVariants(true);
    needRevisionNotes.setItemNotes(new ArrayList<>());
    needRevisionNotes.setVendorErrorFields(new ArrayList<>());
    needRevisionNotes.setImagesAdditionalNotes(
      SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE_EN + PREDICTION_DISPLAY_NAME_3_IN
        + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE_EN);
    needRevisionNotes.setImageReason(Collections
      .singletonList(SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_BLUR_IMAGE_EN));
    verify(needCorrectionService)
        .updateStateInPBPAndSendProductToNeedRevision(autoNeedRevisionDomainEvent.getProductCode(),
      SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE_EN + PREDICTION_DISPLAY_NAME_3_IN
        + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE_EN, needRevisionNotes, true, true, true,
            productCollection);
  }

  @Test
  public void autoNeedRevisionProductPreLiveMultiplePrediction_InternationalSellerTest() throws Exception {
    productCollection.setReviewPending(true);
    profileResponse.getCompany().setInternationalFlag(true);
    productCollection.setPostLive(false);
    productCollection.setState(WorkflowStates.IN_VENDOR.getValue());
    productCollection.setReviewType(REVIEW_TYPE_IMAGE);
    List<String> predictionList = new ArrayList<>();
    predictionList.add(PREDICTION_NAME_1);
    predictionList.add(PREDICTION_NAME_3);
    autoNeedRevisionDomainEvent.setPredictionTypeList(predictionList);
    when(productCollectionRepository
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    when(productImagePredictionService
      .findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1))
      .thenReturn(productImagePredictionEn);
    when(productImagePredictionService
      .findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_3))
      .thenReturn(productImagePredictionBlurEn);
    productServiceWrapper.autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, true, true, true, false);
    verify(productCollectionRepository)
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productImagePredictionService)
      .findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_1);
    verify(productImagePredictionService)
      .findByStoreIdAndPredictionType(Constants.DEFAULT_STORE_ID, PREDICTION_NAME_3);
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    NeedRevisionNotes needRevisionNotes = new NeedRevisionNotes();
    needRevisionNotes.setAllVariants(true);
    needRevisionNotes.setVendorErrorFields(new ArrayList<>());
    needRevisionNotes.setItemNotes(new ArrayList<>());
    needRevisionNotes.setImagesAdditionalNotes(
      SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE_EN + PREDICTION_DISPLAY_NAME_1
        + Constants.COMMA + PREDICTION_DISPLAY_NAME_3_IN
        + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE_EN);
    needRevisionNotes.setImageReason(Collections.singletonList(
      SaveHistoryConstants.AUTO_QC_NEED_REVISION_MESSAGE_FOR_BLUR_IMAGE_EN
        + SaveHistoryConstants.AUTO_QC_NEED_REVISION_POST_MESSAGE_FOR_MULTIPLE_PREDICTION_EN
        + PREDICTION_DISPLAY_NAME_1));
    verify(needCorrectionService)
        .updateStateInPBPAndSendProductToNeedRevision(autoNeedRevisionDomainEvent.getProductCode(),
      SaveHistoryConstants.AUTO_NEED_REVISION_MESSAGE_EN + PREDICTION_DISPLAY_NAME_1
        + Constants.COMMA + PREDICTION_DISPLAY_NAME_3_IN
        + SaveHistoryConstants.AUTO_NEED_REVISION_IMAGE_MESSAGE_EN, needRevisionNotes, true, true, true,
            productCollection);
  }

  @Test
  public void needRevisionSubmitScreeningPreliveTest_switchtrue() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "isSkipScreeningSwitch", true);
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, DRAFT_STATE, ITEM_SKU_1, DRAFT_STATE, ITEM_SKU_2, DRAFT_STATE);
    Map<String, String> failedMap = ImmutableMap.of(ITEM_SKU_2, DRAFT_STATE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class)))
        .thenReturn(WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatusMap(statusMap)
            .failedItemSkuToFailedReasonMap(failedMap).build());
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(false);
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals("Pre-live", editProductResponse.getReviewType());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
  }

  @Test
  public void needRevisionSubmitScreeningPreliveTest_switchtrue_trustedSellers() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "isSkipScreeningSwitch", true);
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, DRAFT_STATE, ITEM_SKU_1, DRAFT_STATE, ITEM_SKU_2, DRAFT_STATE);
    Map<String, String> failedMap = ImmutableMap.of(ITEM_SKU_2, DRAFT_STATE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(productImageQcProcessingResponse);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(true);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
      .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
      .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
      .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
      .thenReturn(productItemWholesalePrices);
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class)))
      .thenReturn(WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatusMap(statusMap)
        .failedItemSkuToFailedReasonMap(failedMap).build());
    Mockito.doNothing().when(productItemWholesalePriceService)
      .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
      .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
      .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    when(productLevel3Helper
      .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
      .thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(false);
    EditProductResponse editProductResponse = productServiceWrapper
      .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
      .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productItemWholesalePriceService)
      .saveWholesalePrice(Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
      .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productLevel1HistoryService)
      .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
      .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
      .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals("Pre-live", editProductResponse.getReviewType());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
  }

  @Test
  public void publishRevisedEventTest() throws Exception {
    productCollection.setNeedCorrectionNotes(null);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    productServiceWrapper.publishRevisedEvent(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productPublisherService)
        .publishRevisedProductToPDT(addRevisedProductToPDTEventArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, addRevisedProductToPDTEventArgumentCaptor.getValue().getProductCode());
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productBusinessPartnerService).findFirstByStoreIdAndProductId(any(), any());
  }

  @Test
  public void updateReviewPendingTest() {
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productServiceWrapper.updateReviewPending(STORE_ID, PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).saveProductCollection(productCollection);
    Mockito.verify(productService).updateSolrProductCollection(productCollection);
  }

  @Test
  public void updateReviewPendingProductNotFoundTest() {
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productServiceWrapper.updateReviewPending(STORE_ID, PRODUCT_CODE, true);
      });
    } finally {
      Mockito.verify(productCollectionRepository)
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void updateViewableTest() {
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productServiceWrapper.updateActivatedAndViewable(STORE_ID, PRODUCT_CODE, false, true);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).saveProductCollection(productCollection);
    Mockito.verify(productService).updateSolrProductCollection(productCollection);
  }

  @Test
  public void updateViewablePendingProductNotFoundTest() {
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productServiceWrapper.updateActivatedAndViewable(STORE_ID, PRODUCT_CODE, false, true);
      });
    } finally {
      Mockito.verify(productCollectionRepository)
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void processImageQcForBacklogProductsNullTest() throws Exception {
    productServiceWrapper.processImageQcForBacklogProducts(STORE_ID, null);
  }

  @Test
  public void processImageQcForBacklogProductsImageQcImageViolationsTest() throws Exception {
    Mockito.when(productImageQcBacklogService
        .findByStoreIdAndProductCodeAndStatus(STORE_ID, PRODUCT_CODE, ImageQcStatus.IN_PROGRESS.getImageQcStatus()))
        .thenReturn(productImageQcBacklog);
    ProductDetailResponse productDetailResponseWithImages = getProductDetailResponseWithImages();
    productDetailResponseWithImages.getImages().get(0).setMarkForDelete(true);
    productDetailResponseWithImages.getImages().get(1).setActive(true);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(productDetailResponseWithImages);
    Mockito.when(productService.processImageQcForBacklogProducts(STORE_ID, new ImageQcResponseDomainEvent()))
        .thenReturn(PREDICTION_NAME_1);
    Mockito.when(objectMapper.writeValueAsString(new ImageQcResponseDomainEvent()))
        .thenReturn(mapper.writeValueAsString(new ImageQcResponseDomainEvent()));
    imageQcResponseDomainEvent.setProductCode(Constants.BACKLOG + PRODUCT_CODE);
    productServiceWrapper.processImageQcForBacklogProducts(STORE_ID, imageQcResponseDomainEvent);
    Mockito.verify(productImageQcBacklogService)
        .findByStoreIdAndProductCodeAndStatus(STORE_ID, PRODUCT_CODE, ImageQcStatus.IN_PROGRESS.getImageQcStatus());
    Mockito.verify(productImageQcBacklogService)
        .saveProductImageQcBacklog(productImageQcBacklogArgumentCaptor.capture());
    Mockito.verify(productService).processImageQcForBacklogProducts(STORE_ID, imageQcResponseDomainEvent);
    Assertions.assertEquals(ImageQcStatus.COMPLETED.getImageQcStatus(),
        productImageQcBacklogArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(PRODUCT_CODE, productImageQcBacklogArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void getImageQcRequestDomainEventTest() throws Exception {
    ProductDetailResponse productDetailResponseWithImages = getProductDetailResponseWithImages();
    productDetailResponseWithImages.getImages().get(0).setActive(true);
    productDetailResponseWithImages.getImages().get(1).setLocationPath(PATH_2);
    productDetailResponseWithImages.getImages().get(1).setMarkForDelete(true);
    productDetailResponseWithImages.getImages().get(1).setLocationPath(PATH_2);
    Mockito.when(fileStorageService.getImagePathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(fileStorageService.getCompleteSourceUrlPrefix()).thenReturn(GCS_COMPLETE_SOURCE_URL);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(productDetailResponseWithImages);
    Mockito.when(fileStorageService.generateFinalImageFullPath(IMAGE_LOCATION_PATH_1)).thenReturn(GCS_COMPLETE_SOURCE_URL);
    productServiceWrapper.getImageQcRequestDomainEvent(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(fileStorageService, times(2)).getImagePathPrefix();
    Mockito.verify(fileStorageService).getCompleteSourceUrlPrefix();
    Mockito.verify(fileStorageService).generateFinalImageFullPath(IMAGE_LOCATION_PATH_1);
  }

  @Test
  public void getImageQcRequestDomainEventTest1() throws Exception {
    ProductDetailResponse productDetailResponseWithImages = getProductDetailResponseWithImages();
    productDetailResponseWithImages.getImages().get(0).setActive(true);
    productDetailResponseWithImages.getImages().get(0).setLocationPath(PATH_2);
    productDetailResponseWithImages.getImages().get(1).setMarkForDelete(true);
    productDetailResponseWithImages.getImages().get(1).setLocationPath(PATH_2);
    Mockito.when(fileStorageService.getImagePathPrefix()).thenReturn(GCS_PATH_PREFIX);
    Mockito.when(fileStorageService.getCompleteSourceUrlPrefix()).thenReturn(GCS_COMPLETE_SOURCE_URL);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(productDetailResponseWithImages);
    Mockito.when(fileStorageService.generateFinalImageFullPath(PATH_2)).thenReturn(GCS_COMPLETE_SOURCE_URL);
    productServiceWrapper.getImageQcRequestDomainEvent(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(fileStorageService, times(2)).getImagePathPrefix();
    Mockito.verify(fileStorageService).getCompleteSourceUrlPrefix();
    Mockito.verify(fileStorageService).generateFinalImageFullPath(PATH_2);
  }

  @Test
  public void getImageQcRequestDomainEventNullTest() throws Exception {
    productServiceWrapper.getImageQcRequestDomainEvent(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
  }

  @Test
  public void deleteTerminatedSellerNonSharedProductsTest() throws Exception {
    productCollection.setCreatedBy(CREATED_BY);
    Mockito.when(productService.getProductCollectionByProductSku(PRODUCT_SKU)).thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productRepository).discardProduct(productRequestArgumentCaptor.capture());

    productServiceWrapper.deleteTerminatedSellerNonSharedProducts(STORE_ID, PRODUCT_SKU);

    Mockito.verify(productService).getProductCollectionByProductSku(PRODUCT_SKU);
    Mockito.verify(productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productRepository).discardProduct(productRequestArgumentCaptor.capture());

    Assertions.assertEquals(PRODUCT_ID, productRequestArgumentCaptor.getValue().getId());
    Assertions.assertEquals(CREATED_BY, productRequestArgumentCaptor.getValue().getCreatedBy());
  }

  @Test
  public void deleteTerminatedSellerNonSharedProductsReviewPendingTrueTest() throws Exception {
    productCollection.setCreatedBy(CREATED_BY);
    productCollection.setReviewPending(true);
    Mockito.when(productService.getProductCollectionByProductSku(PRODUCT_SKU)).thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.doNothing().when(productDistributionService)
        .removeProductFromPDT(Mockito.any(), Mockito.any(), removeProductRequestArgumentCaptor.capture());
    Mockito.doNothing().when(productRepository).discardProduct(productRequestArgumentCaptor.capture());

    productServiceWrapper.deleteTerminatedSellerNonSharedProducts(STORE_ID, PRODUCT_SKU);

    Mockito.verify(productService).getProductCollectionByProductSku(PRODUCT_SKU);
    Mockito.verify(productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productRepository).discardProduct(productRequestArgumentCaptor.capture());
    Mockito.verify(productDistributionService)
        .removeProductFromPDT(Mockito.any(), Mockito.any(), removeProductRequestArgumentCaptor.capture());

    Assertions.assertEquals(PRODUCT_ID, productRequestArgumentCaptor.getValue().getId());
    Assertions.assertEquals(CREATED_BY, productRequestArgumentCaptor.getValue().getCreatedBy());
  }

  @Test
  public void deleteTerminatedSellerNonSharedProductsSharedProductTest() throws Exception {
    Mockito.when(productService.getProductCollectionByProductSku(PRODUCT_SKU)).thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(new ProductBusinessPartner(), new ProductBusinessPartner()));

    productServiceWrapper.deleteTerminatedSellerNonSharedProducts(STORE_ID, PRODUCT_SKU);

    Mockito.verify(productService).getProductCollectionByProductSku(PRODUCT_SKU);
    Mockito.verify(productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void deleteTerminatedSellerNonSharedProductProductBusinessPartnerNotFoundTest() throws Exception {
    Mockito.when(productService.getProductCollectionByProductSku(PRODUCT_SKU)).thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(new ArrayList<>());
    productServiceWrapper.deleteTerminatedSellerNonSharedProducts(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productService).getProductCollectionByProductSku(PRODUCT_SKU);
    Mockito.verify(productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void deleteTerminatedSellerNonSharedProductProductCollectionNotFoundTest() {
    try {
      Mockito.when(productService.getProductCollectionByProductSku(PRODUCT_SKU)).thenReturn(null);
      productServiceWrapper.deleteTerminatedSellerNonSharedProducts(STORE_ID, PRODUCT_SKU);
    } finally {
      Mockito.verify(productService).getProductCollectionByProductSku(PRODUCT_SKU);
    }
  }

  @Test
  public void processImageQcResponseEditedProductReviewPendingFalseTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollection.setReviewPending(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(
      productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_CODE)).thenReturn(productCollection);
    productCollection.setImageResized(true);
    when(productService.getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE)).thenReturn(new ProductImageQcProcessingResponse());
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(
      profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.updateImageQcResponse(eq(STORE_ID), eq(imageQcResponseDomainEvent), eq(new ProductImageQcProcessingResponse()),
      eq(productCollection), eq(imageCountMap), Mockito.any(), Mockito.any(), eq(productDetailResponse),
      eq(profileResponse))).thenReturn(
      new AutoNeedRevisionAndForceReviewResponse(false, true, false, null));
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_CODE);
    verify(productService).updateImageQcResponse(Mockito.eq(STORE_ID), Mockito.eq(imageQcResponseDomainEvent),
      Mockito.eq(new ProductImageQcProcessingResponse()), Mockito.eq(productCollection), Mockito.eq(imageCountMap),
      productImageQcFeedbackResponseArgumentCaptor.capture(),
      pdtProductDomainEventModelArgumentCaptor.capture(), eq(productDetailResponse), eq(profileResponse));
    verify(productService).publishImageQcProcessedResponseEvent(STORE_ID, PRODUCT_CODE,
      productCollection, productDetailResponse, new AutoNeedRevisionAndForceReviewResponse(false, true, false, null), profileResponse,
        imageQcResponseDomainEvent);
    verify(productService).getImageQcResponseFromPDT(STORE_ID, PRODUCT_CODE);
    verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
  }

  @Test
  public void processImageQcResponsePublishNeedRevisionReviewPendingFalseEventTest() throws Exception {
    productCollection.setPostLive(false);
    productCollection.setState(DRAFT_STATE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    imageCountMap.replace(IMAGE_HASH_CODE_2, 2L);
    setFamilyColourForAllItems(productDetailResponse, FAMILY_COLOUR);
    when(productService.findProductDetailByProductCode(PRODUCT_CODE, true)).thenReturn(productDetailResponse);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(false);
    when(productService.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
      productDetailResponse, false, profileResponse))
      .thenReturn(
        new AutoNeedRevisionAndForceReviewResponse(true, false, false, Collections.singleton(PREDICTION_NAME_1)));
    when(productLevel3Helper
      .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
      .thenReturn(new ArrayList<>());
    productCollection.setImageResized(true);
    productCollection.setReviewPending(false);
    when(productService.updateProductCollectionResizedFlag(productCollection, true, 2)).thenReturn(productCollection);
    productServiceWrapper.processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    verify(productService).findProductDetailByProductCode(PRODUCT_CODE, true);
    verify(productService).getProductImageQcProcessingResponse(STORE_ID, PRODUCT_CODE);
    verify(productLevel3Helper)
      .getRestrictedKeywordsInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productCollectionRepository, times(2))
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productService)
      .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent, imageCountMap, productCollection,
        productDetailResponse, false, profileResponse);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productService).updatePostLiveStatus(productCollectionArgumentCaptor.capture(), eq(false));
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
      anyBoolean());
    Assertions.assertFalse(productCollectionArgumentCaptor.getValue().isReviewPending());
  }

  @Test
  public void getStuckProductCodeAndStateTest() throws Exception {
    ProductWfStateResponse productWfStateResponse = new ProductWfStateResponse();
    StuckProductResponse stuckProductResponse = new StuckProductResponse();
    stuckProductResponse.setProductWfStateResponseList(Arrays.asList(productWfStateResponse));
    Mockito.when(productWfService.getStuckProducts(RETRY_BATCH_SIZE_COUNT, RETRY_TIME_SPAN))
        .thenReturn(stuckProductResponse);

    Mockito.when(productService.getPDTEventDomainModel(productWfStateResponse)).thenReturn(
        StuckProductEventPublishDto.builder().addRevisedProductToPDTEvent(new AddRevisedProductToPDTEvent()).build());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    productServiceWrapper.getStuckProductCodeAndState(RETRY_BATCH_SIZE_COUNT);

    Mockito.when(productService.getPDTEventDomainModel(productWfStateResponse)).thenReturn(
        StuckProductEventPublishDto.builder().editedPublishProductCollection(new ProductCollection()).build());
    Mockito.when(productService.publishAddEditedProductToPDTEvent(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(ProductCollection.class), Mockito.any())).thenReturn(new AddEditedProductToPDTEvent());
    productServiceWrapper.getStuckProductCodeAndState(RETRY_BATCH_SIZE_COUNT);

    Mockito.when(productService.getPDTEventDomainModel(productWfStateResponse)).thenReturn(
        StuckProductEventPublishDto.builder().productCollection(new ProductCollection()).build());
    Mockito.when(productPublisherService.publish(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean(), Mockito.anyString(), any())).thenReturn(new ScreeningProductApprovalEvent());
    productServiceWrapper.getStuckProductCodeAndState(RETRY_BATCH_SIZE_COUNT);

    Mockito.when(productService.getPDTEventDomainModel(productWfStateResponse)).thenReturn(
        StuckProductEventPublishDto.builder().vendorApprovalProductCollection(new ProductCollection()).build());
    Mockito.when(productStatusPublisherService.publishVendorApprovedEventToPBP(Mockito.any(ProductCollection.class)))
        .thenReturn(new PDTProductVendorApprovedEventModel());
    productServiceWrapper.getStuckProductCodeAndState(RETRY_BATCH_SIZE_COUNT);

    Mockito.when(productService.getPDTEventDomainModel(productWfStateResponse)).thenReturn(
        StuckProductEventPublishDto.builder().build());
    productServiceWrapper.getStuckProductCodeAndState(RETRY_BATCH_SIZE_COUNT);

    stuckProductResponse.setProductWfStateResponseList(new ArrayList<>());
    Mockito.when(productWfService.getStuckProducts(RETRY_BATCH_SIZE_COUNT, RETRY_TIME_SPAN))
        .thenReturn(stuckProductResponse);
    productServiceWrapper.getStuckProductCodeAndState(RETRY_BATCH_SIZE_COUNT);

    Mockito.when(productWfService.getStuckProducts(RETRY_BATCH_SIZE_COUNT, RETRY_TIME_SPAN))
        .thenReturn(null);
    productServiceWrapper.getStuckProductCodeAndState(RETRY_BATCH_SIZE_COUNT);

    Mockito.verify(productWfService, times(7)).getStuckProducts(RETRY_BATCH_SIZE_COUNT, RETRY_TIME_SPAN);
    Mockito.verify(productService, times(5)).getPDTEventDomainModel(productWfStateResponse);
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).publishAddEditedProductToPDTEvent(Mockito.any(), Mockito.any(),
        Mockito.any(ProductCollection.class), Mockito.any());
    Mockito.verify(productPublisherService).publish(Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.anyInt(),
        Mockito.anyBoolean(), Mockito.any(), any());
    Mockito.verify(productStatusPublisherService).publishVendorApprovedEventToPBP(Mockito.any(ProductCollection.class));
  }

  @Test
  public void findAutoApprovalTypeByRequestDestinationCategoryEmptyTest() throws Exception {
    autoApprovalTypeRequest.setDestinationCategoryCode(StringUtils.EMPTY);
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(
        profileResponse);
    when(productService.findAutoApprovalTypeByRequest(STORE_ID, USER_NAME, PRODUCT_CODE, autoApprovalTypeRequest,
        profileResponse, productCollection)).thenReturn(autoApprovalType);
    AutoApprovalTypeResponse response =
        productServiceWrapper.findAutoApprovalTypeByRequest(STORE_ID, USER_NAME, PRODUCT_CODE, false,
            autoApprovalTypeRequest);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productService).findAutoApprovalTypeByRequest(STORE_ID, USER_NAME, PRODUCT_CODE, autoApprovalTypeRequest,
        profileResponse, productCollection);
    Assertions.assertEquals(AutoApprovalType.CONTENT_AND_IMAGE.name(), response.getAutoApprovalType());
  }

  @Test
  public void findAutoApprovalTypeByRequestDestinationCategoryEmptyOnlyCatTrueTest() throws Exception {
    autoApprovalTypeRequest.setDestinationCategoryCode(StringUtils.EMPTY);
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(
        profileResponse);
    AutoApprovalTypeResponse response =
        productServiceWrapper.findAutoApprovalTypeByRequest(STORE_ID, USER_NAME, PRODUCT_CODE, true,
            autoApprovalTypeRequest);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(AutoApprovalType.NA.name(), response.getAutoApprovalType());
  }

  @Test
  public void findAutoApprovalTypeByRequestDestinationCategoryNotEmptyTest() throws Exception {
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productCollection);
    when(productCollectionRepository.save(productCollection)).thenReturn(productCollection);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(
        profileResponse);
    when(productService.findAutoApprovalTypeByRequest(STORE_ID, USER_NAME, PRODUCT_CODE, autoApprovalTypeRequest,
        profileResponse, productCollection)).thenReturn(autoApprovalType);
    doNothing().when(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
        anyBoolean());
    AutoApprovalTypeResponse response =
        productServiceWrapper.findAutoApprovalTypeByRequest(STORE_ID, USER_NAME, PRODUCT_CODE, false,
            autoApprovalTypeRequest);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productService).findAutoApprovalTypeByRequest(STORE_ID, USER_NAME, PRODUCT_CODE, autoApprovalTypeRequest,
        profileResponse, productCollection);
    verify(productService).updateSolrOrPublishEvent(productCollectionArgumentCaptor.capture(),
        anyBoolean());
    verify(productCollectionRepository).save(productCollection);
    Assertions.assertEquals(AutoApprovalType.CONTENT_AND_IMAGE.name(), response.getAutoApprovalType());
    Assertions.assertEquals(productCollection.getCategoryCode(), response.getCategoryCode());
    Assertions.assertEquals(productCollection.getCategoryName(), response.getCategoryName());
  }

  @Test
  public void publishEditedImageResizeEventNullTest() {
    EditedResizeAndImagesUpdateStatusResponse response = new EditedResizeAndImagesUpdateStatusResponse();
    productServiceWrapper.publishEditedImageResizeEvent(PRODUCT_CODE, null);
    productServiceWrapper.publishEditedImageResizeEvent(PRODUCT_CODE, response);
    Assertions.assertNull(response.getProductItemImageHistoryDTO());
  }

  @Test
  public void publishEditedImageResizeEventTest() {
    EditedResizeAndImagesUpdateStatusResponse editedResizeAndImagesUpdateStatusResponse =
        new EditedResizeAndImagesUpdateStatusResponse();
    editedResizeAndImagesUpdateStatusResponse.setEditedImages(Collections.singletonList(new ImageRequest()));
    productServiceWrapper.publishEditedImageResizeEvent(PRODUCT_CODE, editedResizeAndImagesUpdateStatusResponse);
    Mockito.verify(productPublisherService).publishEditImageResizeEvent(
        new EditedImageResizeEvent(PRODUCT_CODE, DEFAULT_STORE_ID,
            editedResizeAndImagesUpdateStatusResponse.getEditedImages()));
  }

  @Test
  public void processProductVendorSearchAutoHealTest_processProductEligibleToBeAddedToVendor() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "productStateEligibleToAddToVendor", Constants.ACTIVE);
    productCollection.setReviewPending(true);
    productCollection.setState(Constants.ACTIVE);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.doNothing().when(productService).publishToPDTByProductCollection(productCollection);
    productServiceWrapper.processProductVendorSearchAutoHeal(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).publishToPDTByProductCollection(productCollection);
  }

  @Test
  public void processProductVendorSearchAutoHealTest_processProductNotEligibleToBeAddedToVendor() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "productStateEligibleToAddToVendor", Constants.IN_PROGRESS_STATE);
    productCollection.setReviewPending(true);
    productCollection.setState(Constants.ACTIVE);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productServiceWrapper.processProductVendorSearchAutoHeal(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void processProductVendorSearchAutoHealTest_processProductEligibleToBeActivatedL3Creation() throws Exception {
    productCollection.setStoreId(STORE_ID);
    productCollection.setReviewPending(false);
    productCollection.setState(Constants.ACTIVE);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findByStoreIdAndProductId(productCollection.getStoreId(),
            productCollection.getProductId())).thenReturn(Arrays.asList(productBusinessPartner));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(PRODUCT_SKU)).thenReturn(null);
    Mockito.doNothing().when(productBusinessPartnerService).retryCreate(productBusinessPartner.getStoreId(),
        productBusinessPartner.getId(), null);

    productServiceWrapper.processProductVendorSearchAutoHeal(STORE_ID, PRODUCT_CODE);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.any(), Mockito.any());
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService).findByStoreIdAndProductId(productCollection.getStoreId(),
        productCollection.getProductId());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(PRODUCT_SKU);
    Mockito.verify(productBusinessPartnerService).retryCreate(productBusinessPartner.getStoreId(),
        productBusinessPartner.getId(), null);
  }

  @Test
  public void processProductVendorSearchAutoHealTest_processProductEligibleToBeActivatedForceReviewTrue()
      throws Exception {
    productCollection.setStoreId(STORE_ID);
    productCollection.setReviewPending(false);
    productCollection.setState(Constants.ACTIVE);

    BasicProductResponse basicProductDetail = new BasicProductResponse();
    basicProductDetail.setForceReview(true);

    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findByStoreIdAndProductId(productCollection.getStoreId(),
        productCollection.getProductId())).thenReturn(Arrays.asList(productBusinessPartner));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(PRODUCT_SKU)).thenReturn(basicProductDetail);
    Mockito.when(
            businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
        .thenReturn(profileResponse);
    Mockito.doNothing().when(productLevel3ServiceV1)
        .activateProductOnNeedCorrection(productBusinessPartner.getStoreId(), productBusinessPartner.getGdnProductSku(),
            profileResponse, new ArrayList<>());

    productServiceWrapper.processProductVendorSearchAutoHeal(STORE_ID, PRODUCT_CODE);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.any(), Mockito.any());
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductId(productCollection.getStoreId(), productCollection.getProductId());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(PRODUCT_SKU);
    Mockito.verify(businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
    Mockito.verify(productLevel3ServiceV1)
        .activateProductOnNeedCorrection(productBusinessPartner.getStoreId(), productBusinessPartner.getGdnProductSku(),
            profileResponse, new ArrayList<>());
  }

  @Test
  public void processProductVendorSearchAutoHealTest_processProductEligibleToBeActivatedTakenDownTrue()
      throws Exception {
    productCollection.setStoreId(STORE_ID);
    productCollection.setReviewPending(false);
    productCollection.setState(Constants.ACTIVE);

    BasicProductResponse basicProductDetail = new BasicProductResponse();
    basicProductDetail.setTakenDown(true);

    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findByStoreIdAndProductId(productCollection.getStoreId(),
        productCollection.getProductId())).thenReturn(Arrays.asList(productBusinessPartner));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(PRODUCT_SKU)).thenReturn(basicProductDetail);
    Mockito.when(
            businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
        .thenReturn(profileResponse);
    Mockito.doNothing().when(productLevel3ServiceV1)
        .activateProductOnNeedCorrection(productBusinessPartner.getStoreId(), productBusinessPartner.getGdnProductSku(),
            profileResponse, new ArrayList<>());

    productServiceWrapper.processProductVendorSearchAutoHeal(STORE_ID, PRODUCT_CODE);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.any(), Mockito.any());
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductId(productCollection.getStoreId(), productCollection.getProductId());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(PRODUCT_SKU);
    Mockito.verify(businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
    Mockito.verify(productLevel3ServiceV1)
        .activateProductOnNeedCorrection(productBusinessPartner.getStoreId(), productBusinessPartner.getGdnProductSku(),
            profileResponse, new ArrayList<>());
  }

  @Test
  public void processProductVendorSearchAutoHealTest_processProductEligibleToBeActivated()
      throws Exception {
    productCollection.setStoreId(STORE_ID);
    productCollection.setReviewPending(false);
    productCollection.setState(Constants.ACTIVE);

    BasicProductResponse basicProductDetail = new BasicProductResponse();

    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findByStoreIdAndProductId(productCollection.getStoreId(),
        productCollection.getProductId())).thenReturn(Arrays.asList(productBusinessPartner));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(PRODUCT_SKU)).thenReturn(basicProductDetail);

    productServiceWrapper.processProductVendorSearchAutoHeal(STORE_ID, PRODUCT_CODE);

    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductId(productCollection.getStoreId(), productCollection.getProductId());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(PRODUCT_SKU);
  }

  @Test
  public void processProductVendorSearchAutoHealTest_noAction()
      throws Exception {
    productCollection.setStoreId(STORE_ID);
    productCollection.setReviewPending(false);
    productCollection.setState(Constants.IN_PROGRESS_STATE);

    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);

    productServiceWrapper.processProductVendorSearchAutoHeal(STORE_ID, PRODUCT_CODE);

    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void needRevisionSubmitPostLiveSkipActionsTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "skipDefinitiveAction", true);
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    restrictedKeywordsByFieldAndActionType.setSkipAllActions(true);
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Post-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
            .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productBusinessPartnerService
            .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    EditProductResponse editProductResponse = productServiceWrapper
        .needRevisionSubmit(STORE_ID, USER_NAME, new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Assertions.assertEquals("Post-live", editProductResponse.getReviewType());
  }

  @Test
  public void needRevisionBusinessPartnerAutoRejectSkipActionTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "skipDefinitiveAction", true);
    productCollection.setProductId(PRODUCT_ID);
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Pre-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Map<String, String> statusMap = ImmutableMap.of(ITEM_SKU, DRAFT_STATE, ITEM_SKU_1, DRAFT_STATE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(
            objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService.findByStoreIdAndProductItemId(Mockito.eq(STORE_ID),
        Mockito.anyList())).thenReturn(productItemWholesalePrices);
    Mockito.when(
            productPricingOutbound.bulkActivateOrDeactivateSku(Mockito.anyList()))
        .thenReturn(new BulkActivateDeactivateResponse());
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(productBusinessPartner);
    restrictedKeywordsByFieldAndActionType.setCategoryRestrictedKeywordId(ID);
    restrictedKeywordsByFieldAndActionType.setAction(3);
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(
        Mockito.any(ProductDetailResponse.class), Mockito.any())).thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productPricingOutbound.getWholesalePriceList(Mockito.anySet(), Mockito.anyMap()))
        .thenReturn(Arrays.asList(
            WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.INACTIVE_STATUS).build(),
            WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_1).skuStatus(Constants.ACTIVE_STATUS).build()));
    Mockito.when(productPricingOutbound.upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class))).thenReturn(
        WholesalePriceBulkUpdateResponse.builder().wholesalePriceSkuStatusMap(statusMap)
            .failedItemSkuToFailedReasonMap(new HashMap<>()).build());
    Mockito.doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.when(productBusinessPartnerService.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID),
        Mockito.any())).thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    when(productRepository.getCategoryRestrictedKeywordDetail(ID)).thenReturn(categoryRestrictedKeywordResponse);
    EditProductResponse editProductResponse = productServiceWrapper.needRevisionSubmit(STORE_ID, USER_NAME,
        new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productService).validateSkipDefinitiveAction(STORE_ID, productCollection,
      restrictedKeywordsByFieldAndActionType);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).upsertWholesalePrice(Mockito.any(WholesalePriceRequest.class));
    Mockito.verify(productItemWholesalePriceService)
        .saveWholesalePrice(Mockito.anyList());
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productRepository).getCategoryRestrictedKeywordDetail(ID);
    verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(
        Mockito.any(ProductDetailResponse.class), Mockito.any());
    verify(productWorkflowServiceWrapper).deleteProductCollection(STORE_ID, PRODUCT_CODE,
        categoryRestrictedKeywordResponse.getMessage(), true, true);
    Assertions.assertEquals("Pre-live", editProductResponse.getReviewType());
  }

  @Test
  public void appealFromNeedRevisionTest_thresholdBreached() throws Exception {
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(
            businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(productAppealService.fetchThresholdAndCounterForAppealProduct(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(Pair.of(10, productBusinessPartnerCounter));
    EditProductResponse editProductResponse =
        productServiceWrapper.needRevisionSubmit(STORE_ID, USER_NAME,
            new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU, true, "appeal"));
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productAppealService).fetchThresholdAndCounterForAppealProduct(STORE_ID, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(ApiErrorCode.APPEAL_LIMIT_CROSSED.getCode(),
        editProductResponse.getApiErrorCode().getCode());
  }

  @Test
  public void appealFromNeedRevisionTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "skipDefinitiveAction", true);
    productCollection.setNeedCorrectionNotes(null);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    restrictedKeywordsByFieldAndActionType.setSkipAllActions(true);
    ConfigurationStatusResponse config = ConfigurationStatusResponse.builder().reviewConfig("Post-live").build();
    ReflectionTestUtils.setField(productServiceWrapper, "imageSourceDirectory", IMAGE_SOURCE);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    Mockito.when(objectMapper.readValue(productImageQcProcessingResponse.getImageQcResponse(), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productService.findProductDetailByProductCode(PRODUCT_CODE, false))
        .thenReturn(getProductDetailResponseWithImages());
    Mockito.when(productOutbound.getReviewConfiguration(Mockito.anyList()))
        .thenReturn(Arrays.asList(config));
    Mockito.when(productItemWholesalePriceService
            .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(productAppealService.fetchThresholdAndCounterForAppealProduct(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(Pair.of(11, productBusinessPartnerCounter));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(productPublisherService.publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class)))
        .thenReturn(new AddRevisedProductToPDTEvent());
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(productCollection);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, true)).thenReturn(true);
    when(productLevel3Helper
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any()))
        .thenReturn(restrictedKeywordsByFieldAndActionType);
    Mockito.when(productBusinessPartnerService
            .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    EditProductResponse editProductResponse =
        productServiceWrapper.needRevisionSubmit(STORE_ID, USER_NAME,
            new NeedRevisionSubmitRequest(PRODUCT_CODE, PRODUCT_SKU, true, "appeal"));
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productBusinessPartnerService).saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndProductItemId(Mockito.eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPublisherService).publishRevisedProductToPDT(Mockito.any(AddRevisedProductToPDTEvent.class));
    Mockito.verify(productService).saveProductCollection(Mockito.any(ProductCollection.class));
    Mockito.verify(productBusinessPartnerService).getExpectedActivationDateByCategoryCode(Mockito.any(), Mockito.any());
    Mockito.verify(productLevel1HistoryService)
        .saveProductHistory(PRODUCT_CODE, USER_NAME, SaveHistoryConstants.PRODUCT_RESUBMITTED, null);
    Mockito.verify(fileStorageService).getRevisedImageRequests(Mockito.anyList(), Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService).save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(productLevel3Helper)
        .getRestrictedKeywordsWithActionTypeInProductDetails(Mockito.any(ProductDetailResponse.class), Mockito.any());
    Mockito.verify(productAppealService).incrementCounterForProductAppeal(productBusinessPartnerCounter);
    Assertions.assertEquals("Post-live", editProductResponse.getReviewType());
  }

  @Test
  public void terminatedSellerSkuCleanupProductCollectionNullTest() throws Exception {
    when(productCollectionRepository.findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        PRODUCT_CODE)).thenReturn(null);
    productServiceWrapper.terminatedSellerSkuCleanup(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(this.kafkaProducer)
        .send(eq(kafkaTopicProperties.getTerminatedSellerSkuCleanupStatus()), eq(PRODUCT_CODE),
            terminatedSellerSkuCleanupStatusEventModelArgumentCaptor.capture());
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(), PRODUCT_CODE);
    Assertions.assertEquals(terminatedSellerSkuCleanupStatusEventModelArgumentCaptor.getValue().getResult(),
        TerminatedSellerSkuStatus.SUCCESS.name());
  }

  @Test
  public void terminatedSellerSkuCleanupDeletionInProgressTest() throws Exception{
    ReflectionTestUtils.setField(productServiceWrapper, "terminatedSellerSkuPickedForDeletionThresholdInMinutes", 30);
    productCollection.setUpdatedDate(new Date());
    productCollection.setPickedForDeletion(true);
    when(productCollectionRepository.findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        PRODUCT_CODE)).thenReturn(productCollection);
    productServiceWrapper.terminatedSellerSkuCleanup(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(), PRODUCT_CODE);
  }

  @Test
  public void terminatedSellerSkuCleanupTest() throws Exception{
    ReflectionTestUtils.setField(productServiceWrapper, "terminatedSellerSkuPickedForDeletionThresholdInMinutes", 30);
    productCollection.setUpdatedDate(new Date(3600000));
    productCollection.setPickedForDeletion(true);
    when(productCollectionRepository.findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        PRODUCT_CODE)).thenReturn(productCollection);
    when(terminatedSellerSkuCleanupServiceBean.terminatedSellerSkuCleanup(productCollection)).thenReturn(false);
    productServiceWrapper.terminatedSellerSkuCleanup(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(), PRODUCT_CODE);
    Mockito.verify(this.kafkaProducer)
        .send(eq(kafkaTopicProperties.getTerminatedSellerSkuCleanupStatus()), eq(PRODUCT_CODE),
            terminatedSellerSkuCleanupStatusEventModelArgumentCaptor.capture());
    Mockito.verify(productService).saveProductCollection(productCollection);
    Assertions.assertEquals(terminatedSellerSkuCleanupStatusEventModelArgumentCaptor.getValue().getResult(),
        TerminatedSellerSkuStatus.FAILED.name());
  }

  @Test
  public void terminatedSellerSkuCleanupProductDeletedFromDbTest() throws Exception{
    ReflectionTestUtils.setField(productServiceWrapper, "terminatedSellerSkuPickedForDeletionThresholdInMinutes", 30);
    productCollection.setUpdatedDate(new Date(3600000));
    productCollection.setPickedForDeletion(false);
    when(productCollectionRepository.findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        PRODUCT_CODE)).thenReturn(productCollection);
    when(terminatedSellerSkuCleanupServiceBean.terminatedSellerSkuCleanup(productCollection)).thenReturn(true);
    productServiceWrapper.terminatedSellerSkuCleanup(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(), PRODUCT_CODE);
    Mockito.verify(solrActiveProductCollectionService)
        .deleteSolrProductCollectionDocument(productCollection.getId());
    Mockito.verify(solrReviewProductCollectionService)
        .deleteProductFromReviewProductCollection(productCollection.getId());
    Mockito.verify(productService).saveProductCollection(productCollection);
    Mockito.verify(this.kafkaProducer)
        .send(eq(kafkaTopicProperties.getTerminatedSellerSkuCleanupStatus()), eq(PRODUCT_CODE),
            terminatedSellerSkuCleanupStatusEventModelArgumentCaptor.capture());
    Assertions.assertEquals(terminatedSellerSkuCleanupStatusEventModelArgumentCaptor.getValue().getResult(),
        TerminatedSellerSkuStatus.SUCCESS.name());
  }

  @Test
  public void terminatedSellerSkuCleanupProductNotDeletedFromDbTest() throws Exception{
    ReflectionTestUtils.setField(productServiceWrapper, "terminatedSellerSkuPickedForDeletionThresholdInMinutes", 30);
    productCollection.setUpdatedDate(new Date(3600000));
    productCollection.setPickedForDeletion(false);
    when(productCollectionRepository.findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        PRODUCT_CODE)).thenReturn(productCollection);
    when(terminatedSellerSkuCleanupServiceBean.terminatedSellerSkuCleanup(productCollection)).thenReturn(false);
    productServiceWrapper.terminatedSellerSkuCleanup(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(), PRODUCT_CODE);
    Mockito.verify(productService).saveProductCollection(productCollection);
    Mockito.verify(this.kafkaProducer)
        .send(eq(kafkaTopicProperties.getTerminatedSellerSkuCleanupStatus()), eq(PRODUCT_CODE),
            terminatedSellerSkuCleanupStatusEventModelArgumentCaptor.capture());
    Assertions.assertEquals(terminatedSellerSkuCleanupStatusEventModelArgumentCaptor.getValue().getResult(),
        TerminatedSellerSkuStatus.FAILED.name());
  }

  @Test
  public void terminatedSellerSkuCleanupSolrProductDeletionSuccessTest() throws Exception {
    ReflectionTestUtils.setField(productServiceWrapper, "terminatedSellerSkuPickedForDeletionThresholdInMinutes", 30);
    productCollection.setUpdatedDate(new Date(3600000));
    productCollection.setPickedForDeletion(false);
    when(productCollectionRepository.findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        PRODUCT_CODE)).thenReturn(productCollection);
    when(terminatedSellerSkuCleanupServiceBean.terminatedSellerSkuCleanup(productCollection)).thenReturn(true);
    doNothing().when(solrReviewProductCollectionService)
        .deleteProductFromReviewProductCollection(anyString());
    productServiceWrapper.terminatedSellerSkuCleanup(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productService).saveProductCollection(productCollection);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(), PRODUCT_CODE);
    Mockito.verify(solrActiveProductCollectionService)
        .deleteSolrProductCollectionDocument(productCollection.getId());
    Mockito.verify(solrReviewProductCollectionService)
        .deleteProductFromReviewProductCollection(productCollection.getId());
    Mockito.verify(this.kafkaProducer)
        .send(eq(kafkaTopicProperties.getTerminatedSellerSkuCleanupStatus()), eq(PRODUCT_CODE),
            terminatedSellerSkuCleanupStatusEventModelArgumentCaptor.capture());
    Assertions.assertEquals(terminatedSellerSkuCleanupStatusEventModelArgumentCaptor.getValue().getResult(),
        TerminatedSellerSkuStatus.SUCCESS.name());
  }

  @Test
  public void terminatedSellerSkuCleanupSolrProductDeletionFailureTest() throws Exception{
    ReflectionTestUtils.setField(productServiceWrapper, "terminatedSellerSkuPickedForDeletionThresholdInMinutes", 30);
    productCollection.setUpdatedDate(new Date(3600000));
    productCollection.setPickedForDeletion(false);
    when(productCollectionRepository.findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        PRODUCT_CODE)).thenReturn(productCollection);
    when(terminatedSellerSkuCleanupServiceBean.terminatedSellerSkuCleanup(productCollection)).thenReturn(true);
    doThrow(ApplicationRuntimeException.class).when(solrReviewProductCollectionService)
        .deleteProductFromReviewProductCollection(anyString());
    try {
      productServiceWrapper.terminatedSellerSkuCleanup(PRODUCT_CODE, SELLER_CODE);
    } finally {
      Mockito.verify(productCollectionRepository)
          .findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(), PRODUCT_CODE);
      Mockito.verify(productService).saveProductCollection(productCollection);
      Mockito.verify(solrActiveProductCollectionService)
          .deleteSolrProductCollectionDocument(productCollection.getId());
      Mockito.verify(solrReviewProductCollectionService)
          .deleteProductFromReviewProductCollection(productCollection.getId());
      Mockito.verify(this.kafkaProducer)
          .send(eq(kafkaTopicProperties.getTerminatedSellerSkuCleanupStatus()), eq(PRODUCT_CODE),
              terminatedSellerSkuCleanupStatusEventModelArgumentCaptor.capture());
      Assertions.assertEquals(terminatedSellerSkuCleanupStatusEventModelArgumentCaptor.getValue().getResult(),
          TerminatedSellerSkuStatus.FAILED.name());
    }
  }

  @Test
  public void terminatedSellerSkuCleanupExceptionTest() throws Exception{
    ReflectionTestUtils.setField(productServiceWrapper, "terminatedSellerSkuPickedForDeletionThresholdInMinutes", 30);
    productCollection.setUpdatedDate(new Date(3600000));
    productCollection.setPickedForDeletion(false);
    when(productCollectionRepository.findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        PRODUCT_CODE)).thenReturn(productCollection);
    when(terminatedSellerSkuCleanupServiceBean.terminatedSellerSkuCleanup(productCollection)).thenThrow(
        ApplicationRuntimeException.class);
    try {
      productServiceWrapper.terminatedSellerSkuCleanup(PRODUCT_CODE, SELLER_CODE);
    } finally {
      Mockito.verify(productCollectionRepository)
          .findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(), PRODUCT_CODE);
      Mockito.verify(productService).saveProductCollection(productCollection);
      Mockito.verify(this.kafkaProducer)
          .send(eq(kafkaTopicProperties.getTerminatedSellerSkuCleanupStatus()), eq(PRODUCT_CODE),
              terminatedSellerSkuCleanupStatusEventModelArgumentCaptor.capture());
      Assertions.assertEquals(terminatedSellerSkuCleanupStatusEventModelArgumentCaptor.getValue().getResult(),
          TerminatedSellerSkuStatus.FAILED.name());
    }
  }

  @Test
  public void publishSolrHistoryUpdateEventTest() {
    productServiceWrapper.publishSolrHistoryUpdateEvent(new ArrayList<>());
  }

  @Test
  public void testProcessPcbVendorPublishEvent() throws Exception {
    VendorPublishEventModel vendorPublishEventModel = new VendorPublishEventModel();
    vendorPublishEventModel.setStoreId(STORE_ID);
    vendorPublishEventModel.setProductCode(PRODUCT_CODE);
    ProductCollection productCollection = new ProductCollection();
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any())).thenReturn(
        productCollection);
    when(productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(PRODUCT_CODE, false)).thenReturn(true);
    productServiceWrapper.processPcbVendorPublishEvent(vendorPublishEventModel);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productDistributionTaskRepositoryBean).checkIfProductExistsInPDT(productCollection.getProductCode(), false);
  }

  @Test
  public void testProcessPcbVendorPublishExistInPdtEvent() throws Exception {
    VendorPublishEventModel vendorPublishEventModel = new VendorPublishEventModel();
    vendorPublishEventModel.setStoreId(STORE_ID);
    vendorPublishEventModel.setProductCode(PRODUCT_CODE);
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any())).thenReturn(
        productCollection);
    when(productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(PRODUCT_CODE, false)).thenReturn(true);
    productServiceWrapper.processPcbVendorPublishEvent(vendorPublishEventModel);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService)
        .publishAddEditedProductToPDTEvent(Mockito.any(), Mockito.any(), Mockito.any(ProductCollection.class),
            Mockito.any());
    verify(productDistributionTaskRepositoryBean).checkIfProductExistsInPDT(productCollection.getProductCode(), false);
  }

  @Test
  public void testProcessPcbVendorPublishEventNullProduct() throws Exception {
    VendorPublishEventModel vendorPublishEventModel = new VendorPublishEventModel();
    vendorPublishEventModel.setStoreId(STORE_ID);
    vendorPublishEventModel.setProductCode(PRODUCT_CODE);
    ProductCollection productCollection = new ProductCollection();
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any())).thenReturn(
        null);
    productServiceWrapper.processPcbVendorPublishEvent(vendorPublishEventModel);
    verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void updateProductBrandValue_success() throws Exception {
    ProductBrandUpdateRequest productBrandUpdateRequest =
        ProductBrandUpdateRequest.builder().productCode(PRODUCT_CODE).newBrandCode(BRAND_CODE+"2")
            .newBrandName(BRAND_NAME+"2").build();
    productCollection.setBrandCode(BRAND_CODE+"2");
    productCollection.setBrand(BRAND_NAME+"2");
    ProductBrandUpdateResponse productBrandUpdateResponse = new ProductBrandUpdateResponse();
    productBrandUpdateResponse.setBrandCode(BRAND_CODE+"2");
    productBrandUpdateResponse.setBrandName(BRAND_NAME+"2");
    InternalProductHistoryEventModel internalProductHistoryEventModel =
        new InternalProductHistoryEventModel();
    internalProductHistoryEventModel.setStoreId(DEFAULT_STORE_ID);
    internalProductHistoryEventModel.setProductCode(PRODUCT_CODE);
    internalProductHistoryEventModel.setActivity(ProductWorkflowLookup.STATE_EDIT_DESCRIPTION);
    internalProductHistoryEventModel.setUsername(DEFAULT_USERNAME);
    internalProductHistoryEventModel.setNotes("[{field: 'Brand', oldValue: brandName, newValue: brandName2}]");
    when(productService.updateBrandData(anyString(), any(), any())).thenReturn(
        Pair.of(productCollection, BRAND_NAME));
    when(mandatoryParameterHelper.getUsername()).thenReturn(DEFAULT_USERNAME);
    when(productOutbound.updateProductBrandValue(any())).thenReturn(productBrandUpdateResponse);
    productServiceWrapper.updateProductBrandValue(DEFAULT_STORE_ID, productBrandUpdateRequest);
    verify(productBusinessPartnerService).findByProductCode(PRODUCT_CODE);
    verify(productService).updateBrandData(DEFAULT_STORE_ID, productBrandUpdateRequest, Collections.EMPTY_LIST);
    verify(productOutbound).updateProductBrandValue(productBrandUpdateRequest);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null,
        productBrandUpdateRequest.getProductCode(), false);
    verify(productService).updateSolrProductCollection(productCollection);
    verify(mandatoryParameterHelper).getUsername();
    verify(kafkaProducer).send("com.gdn.pbp.internal.product.history.save", PRODUCT_CODE,
        internalProductHistoryEventModel);
  }

  @Test
  public void updateProductBrandValue_successSameBrandCode() throws Exception {
    ProductBrandUpdateRequest productBrandUpdateRequest =
        ProductBrandUpdateRequest.builder().productCode(PRODUCT_CODE).newBrandCode(BRAND_CODE)
            .newBrandName(BRAND_NAME).build();
    ProductBrandUpdateResponse productBrandUpdateResponse = new ProductBrandUpdateResponse();
    productBrandUpdateResponse.setBrandCode(BRAND_CODE);
    productBrandUpdateResponse.setBrandName(BRAND_NAME);
    productCollection.setBrandCode(BRAND_CODE);
    productCollection.setBrand(BRAND_NAME);
    when(productService.updateBrandData(anyString(), any(), anyList())).thenReturn(
        Pair.of(productCollection, BRAND_NAME));
    when(mandatoryParameterHelper.getUsername()).thenReturn(DEFAULT_USERNAME);
    when(productOutbound.updateProductBrandValue(any())).thenReturn(productBrandUpdateResponse);
    productServiceWrapper.updateProductBrandValue(DEFAULT_STORE_ID, productBrandUpdateRequest);
    verify(productBusinessPartnerService).findByProductCode(PRODUCT_CODE);
    verify(productService).updateBrandData(anyString(), any(), anyList());
    verify(productOutbound).updateProductBrandValue(productBrandUpdateRequest);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null,
        productBrandUpdateRequest.getProductCode(), false);
    verify(productService).updateSolrProductCollection(productCollection);
  }

  @Test
  public void updateProductBrandValue_successReviewPendingTrue() throws Exception {
    ProductBrandUpdateRequest productBrandUpdateRequest =
        ProductBrandUpdateRequest.builder().productCode(PRODUCT_CODE).newBrandCode(BRAND_CODE+"2")
            .newBrandName(BRAND_NAME+"2").build();
    productCollection.setBrandCode(BRAND_CODE+"2");
    productCollection.setBrand(BRAND_NAME+"2");
    productCollection.setReviewPending(true);
    ProductBrandUpdateResponse productBrandUpdateResponse = new ProductBrandUpdateResponse();
    productBrandUpdateResponse.setBrandCode(BRAND_CODE+"2");
    productBrandUpdateResponse.setBrandName(BRAND_NAME+"2");
    InternalProductHistoryEventModel internalProductHistoryEventModel =
        new InternalProductHistoryEventModel();
    internalProductHistoryEventModel.setStoreId(DEFAULT_STORE_ID);
    internalProductHistoryEventModel.setProductCode(PRODUCT_CODE);
    internalProductHistoryEventModel.setActivity(ProductWorkflowLookup.STATE_EDIT_DESCRIPTION);
    internalProductHistoryEventModel.setUsername(DEFAULT_USERNAME);
    internalProductHistoryEventModel.setNotes("[{field: 'Brand', oldValue: brandName, newValue: brandName2}]");
    when(productService.updateBrandData(anyString(), any(), any())).thenReturn(
        Pair.of(productCollection, BRAND_NAME));
    when(mandatoryParameterHelper.getUsername()).thenReturn(DEFAULT_USERNAME);
    when(productOutbound.updateProductBrandValue(any())).thenReturn(productBrandUpdateResponse);
    productServiceWrapper.updateProductBrandValue(DEFAULT_STORE_ID, productBrandUpdateRequest);
    verify(productBusinessPartnerService).findByProductCode(PRODUCT_CODE);
    verify(productService).updateBrandData(DEFAULT_STORE_ID, productBrandUpdateRequest,
        Collections.EMPTY_LIST);
    verify(productOutbound).updateProductBrandValue(productBrandUpdateRequest);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null,
        productBrandUpdateRequest.getProductCode(), false);
    verify(productDistributionTaskRepositoryBean).updateProductBrand(
        ChangeBrandRequest.builder().brandCode(BRAND_CODE+"2").brandName(BRAND_NAME+"2")
            .productCode(PRODUCT_CODE).build());
    verify(productService).updateSolrProductCollection(productCollection);
    verify(mandatoryParameterHelper).getUsername();
    verify(kafkaProducer).send("com.gdn.pbp.internal.product.history.save", PRODUCT_CODE,
        internalProductHistoryEventModel);
  }
}
