package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.BulkMasterProductUpdateRequest;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gdn.common.base.entity.GdnBaseEntity;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.ValidationException;
import com.gdn.mta.bulk.dto.product.ProductAndBrandResponse;
import com.gdn.mta.bulk.request.InternalBrandUpdateNotes;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkBrandAuthUploadModel;
import com.gdn.mta.bulk.dto.BulkInternalProcessPendingDataDTO;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;
import com.gdn.mta.bulk.dto.BulkInternalProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkRestrictedKeywordUploadModel;
import com.gdn.mta.bulk.dto.BulkReviewUploadModel;
import com.gdn.mta.bulk.dto.BulkUpdateErrorDTO;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.BulkUpdateSuccessDTO;
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.dto.product.UserResponse;
import com.gdn.mta.bulk.entity.BulkAddCampaignProductQueue;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.BrandAuthAddRequestData;
import com.gdn.mta.bulk.models.BrandAuthDeleteRequestData;
import com.gdn.mta.bulk.models.BulkPriceUpdateRequestData;
import com.gdn.mta.bulk.models.CampaignProductUpdateDto;
import com.gdn.mta.bulk.models.CampaignUpdateResponse;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.models.InternalProcessDataDomainEventModel;
import com.gdn.mta.bulk.models.IprActionRequest;
import com.gdn.mta.bulk.models.RestrictedKeywordRequestData;
import com.gdn.mta.bulk.models.VendorBulkAssignmentRequest;
import com.gdn.mta.bulk.models.download.BrandAuthorisationRequest;
import com.gdn.mta.bulk.models.download.ChangeAssigneeRequest;
import com.gdn.mta.bulk.models.download.VendorAutoAssignmentFilterRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkAddReviewIPRProductsRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkApprovalRejectionRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkAssignAutoApprovedProductsRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkAssigneeMasterSkuReviewRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterSkuReviewRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkPriceProductTypeTaggingRequest;
import com.gdn.mta.bulk.models.InternalBrandUpdateEventModel;
import com.gdn.mta.bulk.models.download.responsedata.BulkPriceRebateRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkSkuLevelRebateRequestData;
import com.gdn.mta.bulk.models.download.responsedata.ClusterActionResponse;
import com.gdn.mta.bulk.models.download.responsedata.ClusterItemErrorListResponse;
import com.gdn.mta.bulk.repository.AttributeRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.CategoryRepository;
import com.gdn.mta.bulk.repository.MasterSkuItemsRepository;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.repository.campaign.CampaignRepository;
import com.gdn.mta.bulk.repository.download.ProductDistributionTaskRepository;
import com.gdn.mta.bulk.repository.pcb.ProductAttributeRepository;
import com.gdn.mta.bulk.service.download.BulkProcessDownloadService;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.bulk.util.BulkMasterSkuUploadParameters;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.VendorProductDataBulkParameters;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.campaign.dto.ItemDetailsDto;
import com.gdn.x.campaign.response.CampaignProductDetailResponse;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.response.ProductCodeResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.BoostedProductFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.product.rest.web.model.dto.ProductSpecialAttributeDTO;
import com.gdn.x.product.rest.web.model.request.GetProductInfoRequestV2;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.ItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductInfoResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.FAILED;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.HYPHEN;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.MASTER_PRODUCT_BULK_UPDATE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.MASTER_PRODUCT_UPDATE;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class InternalProcessServiceWrapperImplTest {

  private static final String STORE_ID = "10001";
  private static final String UPDATED_BY = "updatedBy";
  private static final String INTERNAL_PROCESS_REQUEST_CODE = "request-code";
  private static final String SYSTEM = "System";
  private static final String ID = "request-id";
  private static final String INTERNAL_PROCESS_REQUEST_ID = "request-id";
  private static final String FILE_NAME = "Copy_product_template.xlsx";
  private static final String FILE_NAME1 = "Copy_product_template1.xlsx";
  private static final String FILE_NAME_EMPTY = "Copy_product_template_empty.xlsx";
  private static final String FILE_NAME_DELETED_HEADER = "Copy_product_template_deleted_Header.xlsx";
  private static final String FILE_NAME_HEADER_NAME_CHANGED = "Copy_product_template_Header_Name_changed.xlsx";
  private static final String FILE_NAME_PICKUP_POINT_SHIPPING_TYPE_EMPTY =
      "Copy_product_template_PickupPointCode_ShippingType_empty.xlsx";
  private static final String FILE_NAME_VALIDATION_FAILURE = "Copy_product_template_validation_failure.xlsx";
  private static final int BATCH_SIZE = 10;
  public static final String SELLER_CODE = "seller-code";
  public static final String BULK_MINUTE_VALUE = "120";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  public static final int TOTAL_COUNT = 1;
  public static final String DATA_VALUE =
      "{\n" + "\"productCode\" : \"productCode\",\n" + "\"productName\" : \"productName\",\n"
          + "\"productSku\" : \"productSku\",\n" + "\"itemSku\" : \"RAM-70107-00025-00001\",\n"
          + "\"itemCode\" : \"MTA-21446744-00001\",\n" + "\"copyProductName\" : \"copyProductName\",\n"
          + "\"sellerSku\" : \"30301684\",\n" + "\"listPrice\" : 157500.00,\n" + "\"offerPrice\" : 157500.00,\n"
          + "\"stock\" : 10,\n" + "\"shippingType\" : \"Big/Regular/Bopis\",\n" + "\"minimumStock\" : 10,\n"
          + "\"pickupPointCode\" : \"PP-3239816\",\n" + "\"status\" : 0\n" + "}";
  private static final String REQUEST_ID = "request-id";
  private static final String USER_NAME = "user-name";
  private static final String FETCH_BATCH_SIZE = "10";
  private static final String TOTAL_BATCH_SIZE = "10";
  private static final String CATALOG_TYPE = "SALES_CATALOG";
  private static final String MASTER_TYPE = "MASTER_CATALOG";
  private static final String PRODUCT_CODE = "productCode";
  private static final String ITEM_SKU = "itemSku";
  private static final String MASTER_SKU = "masterSku";
  private static final String FILE_BATCH_SIZE = "10";
  private static final String STORE_COPY_DIRECTORY = "StoreCopy";
  private static final String SALES_CATEGORY_UPDATE_DIRECTORY = "salesCategoryUpdate";
  private static final String DELETE_BRAND_AUTH_DIRECTORY = "deleteBrandAuth";
  private static final String SALES_CATEGORY_UPDATE_FILE_NAME_1 = "Halal_updates_v1 (2).xlsx";
  private static final String SALES_CATEGORY_UPDATE_FILE_NAME_2 = "Halal_updates_v1 (3).xlsx";
  private static final String SALES_CATEGORY_UPDATE_FILE_NAME_3 = "Halal_updates_v1 (4).xlsx";
  private static final String SALES_CATEGORY_UPDATE_FILE_NAME_4 = "Halal_updates_v1 (5).xlsx";
  private static final String SALES_CATEGORY_UPDATE_FILE_NAME_5 = "Halal_updates_v1 (6).xlsx";
  private static final String DELETE_BRAND_AUTH_FILE_NAME_1 = "DeleteBrandBulk.xlsx";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PICKUP_POINT = "PP-3239816";
  private static final String DATA_DEACTIVATE = "{\"operationType\":\"0\",\"cnCategoryCode\":\"345678\"}";
  private static final String DATA_ACTIVATE = "{\"operationType\":\"1\",\"cnCategoryCode\":\"345678\"}";
  private static final String INVALID_DATA = "{\"operationType\":\"2\",\"cnCategoryCode\":\"345678\"}";
  private static final String CATEGORY_CODE = "345678";
  private static final String ATTRIBUTE_CODE = "345678";
  private static final String ATTRIBUTE_NAME = "attributeName";
  private static final String ATTRIBUTE_VALUE = "attributeValue";
  private static final String CATALOG_CODE = "catalogCode";
  private static final String DATA = "{\"operationType\":\"0\",\"cnCategoryCode\":\"345678\"}";
  private static final String INTERNAL_BULK_UPLOAD_DATA =
      "{\"productCode\":\"PRODUCT_CODE\",\"productName\":\"NAME\",\"brand\":\"BRAND\",\"length\":\"1.2\","
          + "\"width\":\"3.4\",\"height\":\"5.6\",\"weight\":\"7.8\",\"dangerousGoodsLevel\":\"1\"}";
  public static final String MINUTE_VALUE = "60";
  public static final String PROCESS_TYPE_STORE_COPY = "STORE_COPY";
  public static final String PROCESS_TYPE_DELETE_BRAND_AUTH = "DELETE_BRAND_AUTHORISATION";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String BUSINESS_PARTNER_NAME = "bpName";
  private static final String NAME = "name";
  private static final String BULK_UPDATE_FOLDER = "BulkUpdate";
  private static final String INTERNAL_UPLOAD_FILE_NAME = "InternalBulkUpdateNewFlow.xlsx";
  private static final String INTERNAL_UPLOAD_FILE_NAME_DUMMY = "InternalBulkUpdateNewFlowDummy.xlsx";
  private static final String VENDOR_BULK_ASSIGNMENT_FILE_NAME = "VendorBulkAssignment.xlsx";
  private static final String VENDOR_BULK_ALL_SUCCESS = "VendorBulkAssignmentSuccess.xlsx";
  private static final String VENDOR_BULK_WRONG_ASSIGNEE = "VendorBulkAssignmentException.xlsx";
  private static final String VENDOR_CODE = "vendorCode";
  private static final String CREATED_DATE = "createdDate";
  private static final String BRAND_CODE = "brand-code";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANNEL_ID = "channelId";
  private static final String REQUESTID = "requestId";
  private static final String USERNAME = "username";
  private static final String FIRST_ANCHOR = "firstAnchor";
  private static final String SECOND_ANCHOR = "secondAnchor";
  private static final String DELETE_BRAND_AUTHORISATION = "DELETE_BRAND_AUTHORISATION";

  private static final String filterAssignmentRequest = "{\"timeFilterWebType"
    + "\":\"string\",\"contentPending\":true,\"imagePending\":true,\"businessPartnerCode\":\"string\",\"postLive\":true,\"faultyType\":\"string\",\"brandPending\":true,\"edited\":true,\"revised\":true,\"restrictedKeyword\":true}";
  private static final String RESTRICTED_KEYWORD_UPSERT_DIRECTORY = "restrictedKeywordUpsert";
  private static final String RESTRICTED_KEYWORD_DELETE_DIRECTORY = "restrictedKeywordDelete";
  private static final String BRAND_AUTH_ADD_DIRECTORY = "brandAuthAdd";
  private static final String BRAND_AUTH_DELETE_DIRECTORY = "brandAuthDelete";
  private static final String BULK_APPROVAL_DIRECTORY = "bulkApproval";
  private static final String BULK_REJECTION_DIRECTORY = "bulkRejection";
  private static final String BULK_ASSIGNEE_DIRECTORY = "bulkAssignee";
  private static final String AUTO_APPROVED_BULK_ASSIGN = "autoApprovedBulkAssign";
  private static final String IPR_PRODUCT_BULK_ADD_REVIEW = "iprProductBulkAddReview";
  private static final String BULK_PRICE_UPDATE = "bulkPriceUpdate";
  private static final String BULK_MASTER_SKU_REVIEW_DIRECTORY = "bulkMasterSkuReview";
  private static final String ALL_PERFECT_FILE = "AllPerfect.xlsx";
  private static final String APPLICABLE_FOR_ALL_NO = "AllCatNoSelectedCat.xlsx";
  private static final String BRAND_AUTH_ADD_FILE = "brandAuthAdd.xlsx";
  private static final String BRAND_AUTH_ADD_2_FILE = "brandAuthAdd2.xlsx";
  private static final String BRAND_AUTH_DELETE_FILE = "brandAuthDelete.xlsx";
  private static final String BRAND_AUTH_DELETE_2_FILE = "brandAuthDelete2.xlsx";
  private static final String BULK_APPROVAL_FAILED = "testApprovalFailed.xlsx";
  private static final String BULK_APPROVAL_2_FILE = "testapproval.xlsx";
  private static final String BULK_REJECTION_FILE="testRejectionSuccess.xlsx";
  private static final String BULK_REJECTION_2_FILE="testRejectionSuccess2.xlsx";
  private static final String BULK_ASSIGNEE_FILE = "testAssigneeSuccess.xlsx";
  private static final String BULK_ASSIGN_FILE = "bulkAssignSuccess.xlsx";
  private static final String BULK_ADD_REVIEW_IPR_PRODUCTS_FILE = "bulkIprProducts.xlsx";
  private static final String BULK_PRICE_UPDATE_FILE = "priceUpdateTool.xlsx";
  private static final String BULK_PRICE_UPDATE_FILE_1 = "priceUpdateTool1.xlsx";
  private static final String BULK_MASTER_SKU_REVIEW_FILE = "testMasterSkuReview.xlsx";
  private static final String MESSAGE =
      "{\"keyword\":\"Beer\",\"keywordType\":\"Competitor's name\",\"keywordAction\":\"No action\",\"message\":\"dfn\",\"destinationCategory\":\"\",\"categoryCode\":null,\"exclusionList\":[\"\"],\"applicableCategoryList\":[\"AU-1010272\",\"TE-1000220\",\"UI-1000001 :  PA-1000366\",\"AL-1000041\"],\"excelRowNumber\":1}";
  private static final String ACTION_EMPTY = "ActionEmpty.xlsx";
  private static final String DELETE_APPLICABLE_NO= "Del_AllPerfect_yes.xlsx";
  private static final String ERROR_MESSAGE = "Can not process invalid input data :ProductSku productCode invalid";
  private static final Set<String> IPR_REVIEWER_CODES = new HashSet<>(
      Arrays.asList(new String[] {"INTERNAL_IPRREVIEWTEAM", "INTERNAL_IPRSUPERVISORTEAM"}));
  private static final String CAMPAIGN_CODE = "CAMP-123456";
  private static final String CAMPAIGN_PRICE = "9000";
  private static final String SALES_PRICE = "9500";
  private static final String LIST_PRICE = "10000";

  private CategoryAttributeResponse categoryAttributeResponse=new CategoryAttributeResponse();
  private BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
  private ProductDetailResponse productDetailResponse = new ProductDetailResponse();
  private ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
  private ProductAndItemInfoResponseV2 productAndItemInfoResponseV2;
  private ProfileResponse profileResponse = new ProfileResponse();
  private CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
  private CategoryResponse categoryResponse = new CategoryResponse();
  private BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
  private SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
  private Pageable pageable = PageRequest.of(PAGE, BATCH_SIZE);
  private byte[] fileContent = new byte[]{-1, -40, -20, -10};
  private BulkVendorProductAssignRequest bulkVendorProductAssignRequest;
  private ListBaseResponse<UserResponse> userResponseListBaseResponse;
  private ObjectMapper mapper = new ObjectMapper();
  private BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();
  private BrandAuthorisationRequest brandAuthorisationRequest = new BrandAuthorisationRequest();
  private BulkInternalUploadRequestDTO bulkInternalUploadRequestDTO;
  private BulkRestrictedKeywordUploadModel bulkRestrictedKeywordUploadModel;
  private BulkBrandAuthUploadModel bulkBrandAuthUploadModel;
  private BulkReviewUploadModel bulkReviewUploadModel;
  private InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel;
  private BulkPriceUpdateRequestData bulkPriceUpdateRequestData = new BulkPriceUpdateRequestData();
  private ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
  private ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();

  private PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
  private PickupPointResponse pickupPointResponse = new PickupPointResponse();

  @InjectMocks
  private InternalProcessServiceWrapperImpl internalProcessServiceWrapper;

  @Mock
  private InternalProcessService internalProcessService;

  @Mock
  private MailDeliveryService mailDeliveryService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private BulkProductSuspensionService bulkProductSuspensionService;

  @Mock
  private BulkConfigurationUpdateService bulkConfigurationUpdateService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private PCBOutboundService pcbOutboundService;

  @Mock
  private PriceAnalyticsOutboundService priceAnalyticsOutboundService;

  @Mock
  private VendorProductBulkAssignService vendorProductBulkAssignService;

  @Mock
  private MasterSkuItemsRepository masterSkuItemsRepository;

  @Mock
  private ProductLevel3BulkUpdateServiceBean productLevel3BulkUpdateServiceBean;

  @Mock
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Captor
  private ArgumentCaptor<List<BulkInternalProcess>> bulkInternalProcessListArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkInternalProcessData> bulkInternalProcessDataArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<BulkInternalProcessData>> internalProcessDataArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<Map<String, String>>> listMapArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkInternalProcess> bulkInternalProcessArgumentCaptor;

  @Mock
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Mock
  private BulkProcessDownloadService bulkProcessDownloadService;

  @Captor
  private ArgumentCaptor<InternalBulkUploadDataDomainEventModel> internalBulkUploadDataDomainEventModelArgumentCaptor;

  @Mock
  private AttributeRepository attributeRepository;

  @Mock
  private CategoryRepository categoryRepository;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private MasterDataBulkUpdateService masterDataBulkUpdateService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private TrackerService trackerService;

  @Mock
  private PartnersEngineOutboundService partnersEngineOutboundService;

  @Mock
  private BrandAuthorisationService brandAuthorisationService;

  @Mock
  private FileStorageServiceBean fileStorageService;

  @Mock
  private FbbConsignmentService fbbConsignmentService;

  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock
  private RestrictedKeywordService restrictedKeywordService;

  @Mock
  private BulkVendorActionService bulkVendorActionService;

  @Mock
  private BulkIPRProductService bulkIPRProductService;

  @Mock
  private BulkMasterSkuReviewService bulkMasterSkuReviewService;

  @Mock
  private BulkAutoApprovedProductsService bulkAutoApprovedProductsService;

  @Mock
  private BulkRebateUpdateService bulkRebateUpdateService;

  @Mock
  private BrandUpdateService brandUpdateService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private MasterSkuReviewOutboundService masterSkuReviewOutboundService;

  @Mock
  private ProductAnalyticsOutboundService productAnalyticsOutboundService;

  @Mock
  private BulkPriceUpdateService bulkPriceUpdateService;

  @Mock
  private PickupPointService pickupPointService;

  @Mock
  private BulkProductTypeTaggingUpdateService bulkProductTypeTaggingUpdateService;

  @Mock
  private BulkSkuLevelRebateService bulkSkuLevelRebateService;

  @Mock
  private ProductAttributeRepository productAttributeRepository;

  @Mock
  private CampaignRepository campaignRepository;

  @Mock
  private BulkPriceUpdateNewService bulkPriceUpdateNewService;

  @Mock
  private PBPOutboundService pbpOutboundService;

  private GdnRestListResponse<ProductCodeResponse> productCodeResponse;

  @BeforeEach
  public void initialize() throws Exception {
    MockitoAnnotations.initMocks(this);

    ReflectionTestUtils.setField(internalProcessServiceWrapper, "salesCatalogCode", CATALOG_CODE);
    ReflectionTestUtils.setField(internalProcessServiceWrapper,  "partnerEngineSize", 1);
    ReflectionTestUtils.setField(internalProcessServiceWrapper,  "childCategoryMaxSize", 1);
    ReflectionTestUtils.setField(internalProcessServiceWrapper,  "brandAuthEndYear", 5);
    ReflectionTestUtils.setField(internalProcessServiceWrapper,  "vendorCode",VENDOR_CODE );
    ReflectionTestUtils.setField(internalProcessServiceWrapper,  "mskuRoleCode", VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE);
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "bulkPriceUpdateAllowedSellers", SELLER_CODE);
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "bulkSkuLevelRebateMaxRows", 100);
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "iprRoleCodeReviewer", "INTERNAL_IPRREVIEWTEAM,INTERNAL_IPRSUPERVISORTEAM");

    bulkInternalProcess.setId(ID);
    bulkInternalProcess.setCreatedBy(CREATED_DATE);
    bulkInternalProcess.setStatus(ProcessStatus.PENDING.name());
    bulkInternalProcess.setSellerCode(SELLER_CODE);
    bulkInternalProcess.setFileName(FILE_NAME);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
    bulkInternalProcessData.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setId(ID);
    bulkInternalProcessData.setParentCode(PRODUCT_CODE);
    bulkInternalProcessData.setSellerCode(SELLER_CODE);
    bulkInternalProcessData.setData(DATA_VALUE);

    ProductResponse productResponse = new ProductResponse();
    productResponse.setProductSpecialAttributes(new ArrayList<>());
    productAndItemsResponse.setProduct(productResponse);

    ProductInfoResponse productInfoResponse = new ProductInfoResponse();
    productInfoResponse.setArchived(false);
    ItemInfoResponseV2 itemInfoResponse = new ItemInfoResponseV2();
    productAndItemInfoResponseV2 = new ProductAndItemInfoResponseV2();
    productAndItemInfoResponseV2.setProduct(productInfoResponse);
    productAndItemInfoResponseV2.setItem(itemInfoResponse);

    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setArchived(false);
    productAndItemsResponse.setItems(Arrays.asList(itemResponse));

    productDetailResponse.setStoreId(STORE_ID);
    productDetailResponse.setName(NAME);
    productDetailResponse.setUrl("url");
    productDetailResponse.setLength(10.0);
    productDetailResponse.setWidth(10.0);
    productDetailResponse.setHeight(10.0);
    productDetailResponse.setWeight(10.0);
    productDetailResponse.setShippingWeight(10.0);
    productDetailResponse.setCategoryCodes(Arrays.asList(CATEGORY_CODE));

    categoryAttributeResponse.setAttribute(new AttributeResponse());
    categoryDetailResponse.setNameEnglish(NAME);
    categoryDetailResponse.setCategoryAttributes(Arrays.asList(categoryAttributeResponse));
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CATALOG_TYPE);
    categoryDetailResponse.setCatalog(catalogResponse);


    CompanyDTO companyDTO=new CompanyDTO();
    companyDTO.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    companyDTO.setMerchantType(SELLER_CODE);
    profileResponse.setCompany(companyDTO);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    bulkVendorProductAssignRequest = new BulkVendorProductAssignRequest();
    bulkVendorProductAssignRequest.setAssignmentType(VendorProductDataBulkParameters.CONTENT_ASSIGNMENT);
    bulkRestrictedKeywordUploadModel = new BulkRestrictedKeywordUploadModel();
    bulkRestrictedKeywordUploadModel.setBulkProcessCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkRestrictedKeywordUploadModel.setBulkProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());
    bulkRestrictedKeywordUploadModel.setFilePath(FILE_NAME);
    bulkBrandAuthUploadModel = new BulkBrandAuthUploadModel();
    bulkBrandAuthUploadModel.setBulkProcessCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkBrandAuthUploadModel.setBulkProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());
    bulkBrandAuthUploadModel.setFilePath(FILE_NAME);

    bulkReviewUploadModel = new BulkReviewUploadModel();
    bulkReviewUploadModel.setBulkProcessCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkReviewUploadModel.setBulkProcessType(BulkInternalProcessType.BULK_APPROVAL.name());
    bulkReviewUploadModel.setFilePath(FILE_NAME);

    userResponseListBaseResponse = new ListBaseResponse<>();
    userResponseListBaseResponse.setMetadata(new Metadata(0, 1, (long) 1));
    userResponseListBaseResponse.setContent(new ArrayList<>());

    bulkInternalProcessData1.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData1.setData(INTERNAL_BULK_UPLOAD_DATA);
    productCodeResponse = getProductCodeSuccessResponse();

//    PowerMockito.mockStatic(BulkCreationCommonUtil.class);

    internalBulkUploadDataDomainEventModel = new InternalBulkUploadDataDomainEventModel();
    internalBulkUploadDataDomainEventModel.setStoreId(STORE_ID);
    internalBulkUploadDataDomainEventModel.setInternalProcessDataRequestId(INTERNAL_PROCESS_REQUEST_CODE);
    internalBulkUploadDataDomainEventModel.setProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE.name());
    internalBulkUploadDataDomainEventModel.setInternalProcessDataRequestIdList(Collections.singletonList(ID));

    bulkPriceUpdateRequestData.setItemSku(ITEM_SKU);
    bulkPriceUpdateRequestData.setProductSku(PRODUCT_SKU);
    bulkPriceUpdateRequestData.setPickupPointCode(PICKUP_POINT);
    bulkPriceUpdateRequestData.setSellerCode(SELLER_CODE);

    itemPickupPointListingL3Request.setBusinessPartnerCode(SELLER_CODE);
    itemPickupPointListingL3Request.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Request.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Request.setPickupPointCodes(Collections.singleton(PICKUP_POINT));

    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PRODUCT_CODE);
    itemPickupPointListingL3Response.setMerchantCode(SELLER_CODE);

    pickupPointFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setCodes(new HashSet<>(Collections.singleton(PICKUP_POINT)));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRODUCT_TYPE_TAGGING_PUBLISH_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    pickupPointResponse.setCode(PICKUP_POINT);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(internalProcessService);
    Mockito.verifyNoMoreInteractions(mailDeliveryService);
    Mockito.verifyNoMoreInteractions(partnersEngineOutboundService);
    Mockito.verifyNoMoreInteractions(vendorProductBulkAssignService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(masterSkuReviewOutboundService);
    Mockito.verifyNoMoreInteractions(bulkMasterSkuReviewService);
    Mockito.verifyNoMoreInteractions(productAnalyticsOutboundService);
    Mockito.verifyNoMoreInteractions(bulkAutoApprovedProductsService);
    ProcessorUtils.deleteFile(ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS);
    ProcessorUtils.deleteFile(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR);
    ProcessorUtils.deleteFile(ProcessorUtils.BULK_APPROVAL);
    ProcessorUtils.deleteFile(ProcessorUtils.BULK_REJECTION);
    Mockito.verifyNoMoreInteractions(productLevel3BulkUpdateServiceBean);
    Mockito.verifyNoMoreInteractions(bulkUpdateServiceUtil);
    Mockito.verifyNoMoreInteractions(pickupPointService);
    Mockito.verifyNoMoreInteractions(bulkPriceUpdateNewService);
    Mockito.verifyNoMoreInteractions(campaignRepository);
  }

  private GdnRestListResponse<ProductCodeResponse> getProductCodeSuccessResponse(){
    List<ProductCodeResponse> productCodeResponses = new ArrayList<>();
    ProductCodeResponse productCodeResponse = new ProductCodeResponse();
    productCodeResponse.setProductCodes(PRODUCT_CODE);
    productCodeResponses.add(productCodeResponse);
    return new GdnRestListResponse<>(null, null, true,productCodeResponses,
      new PageMetaData(PAGE, SIZE, productCodeResponses.size()), REQUEST_ID);
  }

  @Test
  public void processNewStoreCopyRequestTest() throws IOException {
    getStoreCopyFiles(STORE_COPY_DIRECTORY, FILE_NAME1);
    bulkInternalProcess.setFileName(FILE_NAME1);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.STORE_COPY.name());
    XSSFWorkbook workBook;
    try (InputStream is = new FileInputStream(
        ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS + File.separator + INTERNAL_PROCESS_REQUEST_CODE
            + File.separator + FILE_NAME1)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.SUSPEND)
          .relativePath(bulkInternalProcess.getFileName()).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
          SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
              ProcessStatus.PENDING.name(), pageable, BulkInternalProcessType.STORE_COPY.name()))
          .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.STORE_COPY.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.STORE_COPY.name());
      Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
      Mockito.verify(internalProcessService)
          .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processDeleteBrandAuthRequestTest() throws IOException {
    getStoreCopyFiles(DELETE_BRAND_AUTH_DIRECTORY, DELETE_BRAND_AUTH_FILE_NAME_1);
    bulkInternalProcess.setFileName(DELETE_BRAND_AUTH_FILE_NAME_1);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name());
    XSSFWorkbook workBook;
    try (InputStream is = new FileInputStream(
        ProcessorUtils.DELETE_BRAND_AUTHORISATION_DIR_UPLOADS + File.separator
            + INTERNAL_PROCESS_REQUEST_CODE + File.separator + DELETE_BRAND_AUTH_FILE_NAME_1)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.SUSPEND)
          .relativePath(bulkInternalProcess.getFileName()).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterConfigNames.DELETE_BRAND_AUTH_FILE_BATCH_SIZE))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
              ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name()))
          .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name());
      Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
      Mockito.verify(internalProcessService)
          .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  private String getInternalBulkUploadFiles(String directory, String fileName) throws IOException {
    ClassLoader classLoader = getClass().getClassLoader();
    String filePathPrefix = classLoader.getResource(directory).getPath();
    File file = new File(filePathPrefix + File.separator + fileName);
    return file.getAbsolutePath();
  }

  @Test
  public void processInternalBulkRequestTest() throws IOException {
    bulkInternalProcess.setFileName(getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, INTERNAL_UPLOAD_FILE_NAME));
    bulkInternalProcess.setProcessType(BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name());
    bulkInternalProcess.setUpdatedBy(USER_NAME);
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    XSSFWorkbook workBook;
    try (InputStream is = new FileInputStream(
        getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, INTERNAL_UPLOAD_FILE_NAME))) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.SUSPEND).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterConfigNames.INTERNAL_BLUK_UPLOAD_FILE_BATCH_SIZE))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
              ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name()))
          .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name());
      Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
      Mockito.verify(internalProcessService)
          .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
      verify(trackerService, times(8)).sendTracker(MASTER_PRODUCT_BULK_UPDATE,
          MASTER_PRODUCT_UPDATE, HYPHEN, FAILED, bulkInternalProcess.getUpdatedBy());
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processVendorBulkAssignmentFailureTest() throws IOException {
    bulkInternalProcess.setFileName(getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_ASSIGNMENT_FILE_NAME));
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    bulkInternalProcess.setUpdatedBy(USER_NAME);
    XSSFWorkbook workBook;
    try (InputStream is = new FileInputStream(
        getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_ASSIGNMENT_FILE_NAME))) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.SUSPEND).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FILE_BATCH_SIZE))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
          ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name())).thenReturn(
          new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
      UserResponse userResponse = new UserResponse();
      userResponse.setUsername(SYSTEM);
      userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
      Mockito.when(partnersEngineOutboundService.userFilter(
              VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
              VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null))
          .thenReturn(userResponseListBaseResponse);
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService)
          .saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
      Mockito.verify(partnersEngineOutboundService)
          .userFilter(VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE,
              Constant.CREATED_DATE, VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null);
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      Assertions.assertEquals(6, internalProcessDataArgumentCaptor.getAllValues().get(0).size());
      Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(),
          bulkInternalProcessArgumentCaptor.getValue().getStatus());
      Assertions.assertEquals(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name(),
          bulkInternalProcessArgumentCaptor.getValue().getProcessType());
      Assertions.assertEquals(4, internalProcessDataArgumentCaptor.getAllValues().get(0).stream()
          .filter(bulkInternalProcessData1 -> bulkInternalProcessData1.getStatus()
              .equals(ProcessStatus.PENDING.name())).collect(Collectors.toList()).size());
    }
  }

  @Test
  public void processVendorBulkAssignmentImageAssignmentTest() throws IOException {
    bulkInternalProcess.setFileName(getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_ASSIGNMENT_FILE_NAME));
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    bulkInternalProcess.setNotes(VendorProductDataBulkParameters.IMAGE_ASSIGNMENT);
    bulkInternalProcess.setUpdatedBy(USER_NAME);
    XSSFWorkbook workBook;
    try (InputStream is = new FileInputStream(
        getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_ASSIGNMENT_FILE_NAME))) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.SUSPEND).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      UserResponse userResponse = new UserResponse();
      userResponse.setUsername(SYSTEM);
      userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FILE_BATCH_SIZE))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
          ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name())).thenReturn(
          new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(partnersEngineOutboundService.userFilter(
              VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
              VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null))
          .thenReturn(userResponseListBaseResponse);
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService)
          .saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
      Mockito.verify(partnersEngineOutboundService)
          .userFilter(VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE,
              Constant.CREATED_DATE, VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null);
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      Assertions.assertEquals(6, internalProcessDataArgumentCaptor.getAllValues().get(0).size());
      Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(),
          bulkInternalProcessArgumentCaptor.getValue().getStatus());
      Assertions.assertEquals(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name(),
          bulkInternalProcessArgumentCaptor.getValue().getProcessType());
      Assertions.assertEquals(SELLER_CODE,
          bulkInternalProcessArgumentCaptor.getValue().getSellerCode());
      Assertions.assertEquals(2, internalProcessDataArgumentCaptor.getAllValues().get(0).stream()
          .filter(bulkInternalProcessData1 -> bulkInternalProcessData1.getStatus()
              .equals(ProcessStatus.FAILED.name())).collect(Collectors.toList()).size());
      Assertions.assertEquals(4, internalProcessDataArgumentCaptor.getAllValues().get(0).stream()
          .filter(bulkInternalProcessData1 -> bulkInternalProcessData1.getStatus()
              .equals(ProcessStatus.PENDING.name())).collect(Collectors.toList()).size());
    }
  }

  @Test
  public void processVendorBulkAssignmentContentAssignmentTest() throws IOException {
    bulkInternalProcess.setFileName(getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_ASSIGNMENT_FILE_NAME));
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    XSSFWorkbook workBook;
    try (InputStream is = new FileInputStream(
        getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_ASSIGNMENT_FILE_NAME))) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.SUSPEND).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      bulkInternalProcess.setNotes(VendorProductDataBulkParameters.CONTENT_ASSIGNMENT);
      bulkInternalProcess.setUpdatedBy(USER_NAME);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      UserResponse userResponse = new UserResponse();
      userResponse.setUsername(SYSTEM);
      userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FILE_BATCH_SIZE))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
          ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name())).thenReturn(
          new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(partnersEngineOutboundService.userFilter(
              VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
              VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null))
          .thenReturn(userResponseListBaseResponse);
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService)
          .saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
      Mockito.verify(partnersEngineOutboundService)
          .userFilter(VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE,
              Constant.CREATED_DATE, VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null);
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      Assertions.assertEquals(6, internalProcessDataArgumentCaptor.getAllValues().get(0).size());
      Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(),
          bulkInternalProcessArgumentCaptor.getValue().getStatus());
      Assertions.assertEquals(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name(),
          bulkInternalProcessArgumentCaptor.getValue().getProcessType());
      Assertions.assertEquals(SELLER_CODE,
          bulkInternalProcessArgumentCaptor.getValue().getSellerCode());
      Assertions.assertEquals(2, internalProcessDataArgumentCaptor.getAllValues().get(0).stream()
          .filter(bulkInternalProcessData1 -> bulkInternalProcessData1.getStatus()
              .equals(ProcessStatus.FAILED.name())).collect(Collectors.toList()).size());
      Assertions.assertEquals(4, internalProcessDataArgumentCaptor.getAllValues().get(0).stream()
          .filter(bulkInternalProcessData1 -> bulkInternalProcessData1.getStatus()
              .equals(ProcessStatus.PENDING.name())).collect(Collectors.toList()).size());
    }
  }

  @Test
  public void processVendorBulkAssignmentContentAssignmentAllSuccessTest() throws IOException {
    bulkInternalProcess.setFileName(getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_ALL_SUCCESS));
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    bulkInternalProcess.setNotes(VendorProductDataBulkParameters.CONTENT_ASSIGNMENT);
    XSSFWorkbook workBook;
    try (InputStream is = new FileInputStream(
        getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_ALL_SUCCESS))) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.SUSPEND).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      bulkInternalProcess.setUpdatedBy(USER_NAME);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      UserResponse userResponse = new UserResponse();
      userResponse.setUsername(SYSTEM);
      userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FILE_BATCH_SIZE))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
          ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name())).thenReturn(
          new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(partnersEngineOutboundService.userFilter(
              VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
              VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null))
          .thenReturn(userResponseListBaseResponse);
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService)
          .saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
      Mockito.verify(partnersEngineOutboundService)
          .userFilter(VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE,
              Constant.CREATED_DATE, VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null);
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      Assertions.assertEquals(4, internalProcessDataArgumentCaptor.getAllValues().get(0).size());
      Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(),
          bulkInternalProcessArgumentCaptor.getValue().getStatus());
      Assertions.assertEquals(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name(),
          bulkInternalProcessArgumentCaptor.getValue().getProcessType());
      Assertions.assertEquals(SELLER_CODE,
          bulkInternalProcessArgumentCaptor.getValue().getSellerCode());
      Assertions.assertEquals(0, internalProcessDataArgumentCaptor.getAllValues().get(0).stream()
          .filter(bulkInternalProcessData1 -> bulkInternalProcessData1.getStatus()
              .equals(ProcessStatus.FAILED.name())).collect(Collectors.toList()).size());
      Assertions.assertEquals(4, internalProcessDataArgumentCaptor.getAllValues().get(0).stream()
          .filter(bulkInternalProcessData1 -> bulkInternalProcessData1.getStatus()
              .equals(ProcessStatus.PENDING.name())).collect(Collectors.toList()).size());
    }
  }

  @Test
  public void processVendorBulkAssignmentContentAssignmentInvalidTest() throws IOException {
    bulkInternalProcess.setFileName(getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_WRONG_ASSIGNEE));
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    bulkInternalProcess.setNotes(VendorProductDataBulkParameters.CONTENT_ASSIGNMENT);
    bulkInternalProcess.setUpdatedBy(USER_NAME);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_WRONG_ASSIGNEE))) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      UserResponse userResponse = new UserResponse();
      userResponse.setUsername(SYSTEM);
      userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FILE_BATCH_SIZE))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
          ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name())).thenReturn(
          new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(partnersEngineOutboundService.userFilter(
              VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
              VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null))
          .thenReturn(userResponseListBaseResponse);
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService)
          .saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
      Mockito.verify(partnersEngineOutboundService)
          .userFilter(VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE,
              Constant.CREATED_DATE, VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null);
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Assertions.assertEquals(6, internalProcessDataArgumentCaptor.getAllValues().get(0).size());
      Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(),
          bulkInternalProcessArgumentCaptor.getValue().getStatus());
      Assertions.assertEquals(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name(),
          bulkInternalProcessArgumentCaptor.getValue().getProcessType());
      Assertions.assertEquals(SELLER_CODE,
          bulkInternalProcessArgumentCaptor.getValue().getSellerCode());
      Assertions.assertEquals(6, internalProcessDataArgumentCaptor.getAllValues().get(0).stream()
          .filter(bulkInternalProcessData1 -> bulkInternalProcessData1.getStatus()
              .equals(ProcessStatus.FAILED.name())).collect(Collectors.toList()).size());
      Assertions.assertEquals(0, internalProcessDataArgumentCaptor.getAllValues().get(0).stream()
          .filter(bulkInternalProcessData1 -> bulkInternalProcessData1.getStatus()
              .equals(ProcessStatus.PENDING.name())).collect(Collectors.toList()).size());
      for (BulkInternalProcessData bulkInternalProcessData : internalProcessDataArgumentCaptor.getAllValues()
          .get(0)) {
        if (ProcessStatus.PENDING.name().equals(bulkInternalProcessData.getStatus())) {
          VendorBulkAssignmentRequest vendorBulkAssignmentRequest =
              mapper.readValue(bulkInternalProcessData.getData(),
                  VendorBulkAssignmentRequest.class);
          Assertions.assertEquals(VendorProductDataBulkParameters.CONTENT_ASSIGNMENT,
              vendorBulkAssignmentRequest.getAssignmentType());
        }
      }
    }
  }

  @Test
  public void processVendorBulkAssignmentImageAssignmentInvalidTest() throws IOException {
    bulkInternalProcess.setFileName(getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_WRONG_ASSIGNEE));
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    bulkInternalProcess.setNotes(VendorProductDataBulkParameters.IMAGE_ASSIGNMENT);
    bulkInternalProcess.setUpdatedBy(USER_NAME);
    XSSFWorkbook workBook;
    try (InputStream is = new FileInputStream(
        getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_WRONG_ASSIGNEE))) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      UserResponse userResponse = new UserResponse();
      userResponse.setUsername(SYSTEM);
      userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FILE_BATCH_SIZE))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
          ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name())).thenReturn(
          new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(partnersEngineOutboundService.userFilter(
              VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
              VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null))
          .thenReturn(userResponseListBaseResponse);
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService)
          .saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
      Mockito.verify(partnersEngineOutboundService)
          .userFilter(VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE,
              Constant.CREATED_DATE, VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null);
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      Assertions.assertEquals(6, internalProcessDataArgumentCaptor.getAllValues().get(0).size());
      Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(),
          bulkInternalProcessArgumentCaptor.getValue().getStatus());
      Assertions.assertEquals(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name(),
          bulkInternalProcessArgumentCaptor.getValue().getProcessType());
      Assertions.assertEquals(SELLER_CODE,
          bulkInternalProcessArgumentCaptor.getValue().getSellerCode());
      Assertions.assertEquals(6, internalProcessDataArgumentCaptor.getAllValues().get(0).stream()
          .filter(bulkInternalProcessData1 -> bulkInternalProcessData1.getStatus()
              .equals(ProcessStatus.FAILED.name())).collect(Collectors.toList()).size());
      Assertions.assertEquals(0, internalProcessDataArgumentCaptor.getAllValues().get(0).stream()
          .filter(bulkInternalProcessData1 -> bulkInternalProcessData1.getStatus()
              .equals(ProcessStatus.PENDING.name())).collect(Collectors.toList()).size());
      for (BulkInternalProcessData bulkInternalProcessData : internalProcessDataArgumentCaptor.getAllValues()
          .get(0)) {
        if (ProcessStatus.PENDING.name().equals(bulkInternalProcessData.getStatus())) {
          VendorBulkAssignmentRequest vendorBulkAssignmentRequest =
              mapper.readValue(bulkInternalProcessData.getData(),
                  VendorBulkAssignmentRequest.class);
          Assertions.assertEquals(VendorProductDataBulkParameters.IMAGE_ASSIGNMENT,
              vendorBulkAssignmentRequest.getAssignmentType());
        }
      }
    }
  }

  @Test
  public void processVendorBulkAssignmentPartnersPaginationTest() throws IOException {
    ReflectionTestUtils.setField(internalProcessServiceWrapper,  "partnerEngineSize", 100);
    bulkInternalProcess.setFileName(getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_WRONG_ASSIGNEE));
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    bulkInternalProcess.setNotes(VendorProductDataBulkParameters.IMAGE_ASSIGNMENT);
    XSSFWorkbook workBook;
    try (InputStream is = new FileInputStream(
        getInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_WRONG_ASSIGNEE))) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      bulkInternalProcess.setUpdatedBy(USER_NAME);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      UserResponse userResponse = new UserResponse();
      userResponse.setUsername(SYSTEM);
      userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
      userResponseListBaseResponse.setMetadata(new Metadata(0, 100, (long) 178));
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FILE_BATCH_SIZE))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
          ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name())).thenReturn(
          new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(partnersEngineOutboundService.userFilter(
              VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
              VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null))
          .thenReturn(userResponseListBaseResponse);
      userResponseListBaseResponse.setMetadata(new Metadata(1, 100, (long) 178));
      Mockito.when(partnersEngineOutboundService.userFilter(
              VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
              VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 1, null))
          .thenReturn(userResponseListBaseResponse);
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService)
          .saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
      Mockito.verify(partnersEngineOutboundService)
          .userFilter(VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE,
              Constant.CREATED_DATE, VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null);
      Mockito.verify(partnersEngineOutboundService)
          .userFilter(VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE,
              Constant.CREATED_DATE, VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 1, null);
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      Assertions.assertEquals(6, internalProcessDataArgumentCaptor.getAllValues().get(0).size());
      Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(),
          bulkInternalProcessArgumentCaptor.getValue().getStatus());
      Assertions.assertEquals(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name(),
          bulkInternalProcessArgumentCaptor.getValue().getProcessType());
      Assertions.assertEquals(SELLER_CODE,
          bulkInternalProcessArgumentCaptor.getValue().getSellerCode());
      Assertions.assertEquals(6, internalProcessDataArgumentCaptor.getAllValues().get(0).stream()
          .filter(bulkInternalProcessData1 -> bulkInternalProcessData1.getStatus()
              .equals(ProcessStatus.FAILED.name())).collect(Collectors.toList()).size());
      Assertions.assertEquals(0, internalProcessDataArgumentCaptor.getAllValues().get(0).stream()
          .filter(bulkInternalProcessData1 -> bulkInternalProcessData1.getStatus()
              .equals(ProcessStatus.PENDING.name())).collect(Collectors.toList()).size());
    }
  }

  @Test
  public void processNewStoreCopyRequestEmptyFileTest() throws IOException {
    getStoreCopyFiles(STORE_COPY_DIRECTORY, FILE_NAME_EMPTY);
    bulkInternalProcess.setFileName(FILE_NAME_EMPTY);
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable, BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
  }

  @Test
  public void processNewStoreCopyRequestHeaderDeletedValidationTest() throws IOException {
    getStoreCopyFiles(STORE_COPY_DIRECTORY, FILE_NAME_DELETED_HEADER);
    bulkInternalProcess.setFileName(FILE_NAME_DELETED_HEADER);
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable, BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
  }

  @Test
  public void processNewStoreCopyRequestHeaderModifiedValidationTest() throws IOException {
    getStoreCopyFiles(STORE_COPY_DIRECTORY, FILE_NAME_HEADER_NAME_CHANGED);
    bulkInternalProcess.setFileName(FILE_NAME_HEADER_NAME_CHANGED);
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable, BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
  }

  @Test
  public void processNewStoreCopyRequestPickupPointShippingTypeEmptyTest() throws IOException {
    getStoreCopyFiles(STORE_COPY_DIRECTORY, FILE_NAME_PICKUP_POINT_SHIPPING_TYPE_EMPTY);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.STORE_COPY.name());
    bulkInternalProcess.setFileName(FILE_NAME_PICKUP_POINT_SHIPPING_TYPE_EMPTY);
    XSSFWorkbook workBook;
    try (InputStream is = new FileInputStream(
        ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS + File.separator + INTERNAL_PROCESS_REQUEST_CODE
            + File.separator + FILE_NAME_PICKUP_POINT_SHIPPING_TYPE_EMPTY)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.STORE_COPY)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
          SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
              ProcessStatus.PENDING.name(), pageable, BulkInternalProcessType.STORE_COPY.name()))
          .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.STORE_COPY.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.STORE_COPY.name());
      ;
      Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
      verify(fileStorageService).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processNewStoreCopyRequestMultiVariantFailedValidationTest() throws IOException {
    getStoreCopyFiles(STORE_COPY_DIRECTORY, FILE_NAME_VALIDATION_FAILURE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.STORE_COPY.name());
    bulkInternalProcess.setFileName(FILE_NAME_VALIDATION_FAILURE);
    XSSFWorkbook workBook;
    try (InputStream is = new FileInputStream(
        ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS + File.separator + INTERNAL_PROCESS_REQUEST_CODE
            + File.separator + FILE_NAME_VALIDATION_FAILURE)) {
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    bulkInternalUploadRequestDTO =
      BulkInternalUploadRequestDTO.builder().internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode()).fileName(bulkInternalProcess.getFileName())
        .bulkInternalProcessType(BulkInternalProcessType.STORE_COPY).build();
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(sheet);
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
                BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
        BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name());
    ;
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  @Disabled
  public void processNewStoreCopyRequestExceptionTest() throws IOException {
    getStoreCopyFiles(STORE_COPY_DIRECTORY, FILE_NAME1);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(STORE_COPY_DIRECTORY + File.separator + INTERNAL_PROCESS_REQUEST_CODE + File.separator
      + FILE_NAME1)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.SUSPEND).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      bulkInternalProcess.setFileName(FILE_NAME1);
      bulkInternalProcess.setProcessType(BulkInternalProcessType.STORE_COPY.name());
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
          SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
              ProcessStatus.PENDING.name(), pageable, BulkInternalProcessType.STORE_COPY.name()))
          .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.doThrow(Exception.class).when(internalProcessService)
          .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
      try {
        Assertions.assertThrows(RuntimeException.class,
            () -> internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID,
                USER_NAME, BulkInternalProcessType.STORE_COPY.name()));
      } finally {
        Mockito.verify(internalProcessService)
            .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
                BulkInternalProcessType.STORE_COPY.name());
        Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
        Mockito.verify(internalProcessService, Mockito.times(2))
            .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
      }
    }
  }

  private void getStoreCopyFiles(String directory, String fileName) throws IOException {
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(
        Thread.currentThread().getContextClassLoader().getResourceAsStream(directory + File.separator + fileName))),
        "UTF-8");
    byte[] excelFile = Base64.decodeBase64(excelData);
    if (STORE_COPY_DIRECTORY.equals(directory)) {
      ProcessorUtils
          .createDirectories(ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS + File.separator + INTERNAL_PROCESS_REQUEST_CODE);
      ProcessorUtils.createFile(
          ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS + File.separator + INTERNAL_PROCESS_REQUEST_CODE + File.separator
              + fileName, excelFile);
    } else if (SALES_CATEGORY_UPDATE_DIRECTORY.equals(directory)) {
      ProcessorUtils.createDirectories(
          ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR + File.separator + INTERNAL_PROCESS_REQUEST_CODE);
      ProcessorUtils.createFile(
          ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR + File.separator + INTERNAL_PROCESS_REQUEST_CODE
              + File.separator + fileName, excelFile);
    } else if (DELETE_BRAND_AUTH_DIRECTORY.equals(directory)) {
      ProcessorUtils.createDirectories(
          ProcessorUtils.DELETE_BRAND_AUTHORISATION_DIR_UPLOADS + File.separator + INTERNAL_PROCESS_REQUEST_CODE);
      ProcessorUtils.createFile(
          ProcessorUtils.DELETE_BRAND_AUTHORISATION_DIR_UPLOADS + File.separator + INTERNAL_PROCESS_REQUEST_CODE
              + File.separator + fileName, excelFile);
    } else if (RESTRICTED_KEYWORD_UPSERT_DIRECTORY.equals(directory)) {
      ProcessorUtils.createDirectories(
          ProcessorUtils.DATA_BASE_DIR + RESTRICTED_KEYWORD_UPSERT_DIRECTORY + "/" + File.separator
              + INTERNAL_PROCESS_REQUEST_CODE);
      ProcessorUtils.createFile(
          ProcessorUtils.DATA_BASE_DIR + RESTRICTED_KEYWORD_UPSERT_DIRECTORY + "/" + File.separator
              + INTERNAL_PROCESS_REQUEST_CODE + File.separator + fileName, excelFile);
    } else if (RESTRICTED_KEYWORD_DELETE_DIRECTORY.equals(directory)) {
      ProcessorUtils.createDirectories(
          ProcessorUtils.DATA_BASE_DIR + RESTRICTED_KEYWORD_DELETE_DIRECTORY + "/" + File.separator
              + INTERNAL_PROCESS_REQUEST_CODE);
      ProcessorUtils.createFile(
          ProcessorUtils.DATA_BASE_DIR + RESTRICTED_KEYWORD_DELETE_DIRECTORY + "/" + File.separator
              + INTERNAL_PROCESS_REQUEST_CODE + File.separator + fileName, excelFile);
    } else if (BRAND_AUTH_ADD_DIRECTORY.equals(directory)) {
      ProcessorUtils.createDirectories(
          ProcessorUtils.DATA_BASE_DIR + BRAND_AUTH_ADD_DIRECTORY + "/" + File.separator
              + INTERNAL_PROCESS_REQUEST_CODE);
      ProcessorUtils.createFile(
          ProcessorUtils.DATA_BASE_DIR + BRAND_AUTH_ADD_DIRECTORY + "/" + File.separator
              + INTERNAL_PROCESS_REQUEST_CODE + File.separator + fileName, excelFile);
    } else if (BULK_APPROVAL_DIRECTORY.equals(directory)) {
      ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + BULK_APPROVAL_DIRECTORY + "/" + File.separator
          + INTERNAL_PROCESS_REQUEST_CODE);
      ProcessorUtils.createFile(
          ProcessorUtils.DATA_BASE_DIR + BULK_APPROVAL_DIRECTORY + "/" + File.separator + INTERNAL_PROCESS_REQUEST_CODE
              + File.separator + fileName, excelFile);

    } else if (BULK_REJECTION_DIRECTORY.equals(directory)) {
      ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + BULK_REJECTION_DIRECTORY + "/" + File.separator
          + INTERNAL_PROCESS_REQUEST_CODE);
      ProcessorUtils.createFile(
          ProcessorUtils.DATA_BASE_DIR + BULK_REJECTION_DIRECTORY + "/" + File.separator + INTERNAL_PROCESS_REQUEST_CODE
              + File.separator + fileName, excelFile);
    } else if (BRAND_AUTH_DELETE_DIRECTORY.equals(directory)) {
      ProcessorUtils.createDirectories(
          ProcessorUtils.DATA_BASE_DIR + BRAND_AUTH_DELETE_DIRECTORY + "/" + File.separator
              + INTERNAL_PROCESS_REQUEST_CODE);
      ProcessorUtils.createFile(
          ProcessorUtils.DATA_BASE_DIR + BRAND_AUTH_DELETE_DIRECTORY + "/" + File.separator
              + INTERNAL_PROCESS_REQUEST_CODE + File.separator + fileName, excelFile);
    } else if (BULK_MASTER_SKU_REVIEW_DIRECTORY.equals(directory)) {
      ProcessorUtils.createDirectories(
          ProcessorUtils.DATA_BASE_DIR + BULK_MASTER_SKU_REVIEW_DIRECTORY + "/" + File.separator
              + INTERNAL_PROCESS_REQUEST_CODE);
      ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + BULK_MASTER_SKU_REVIEW_DIRECTORY + "/" + File.separator
          + INTERNAL_PROCESS_REQUEST_CODE + File.separator + fileName, excelFile);
    }
    else if (BULK_ASSIGNEE_DIRECTORY.equals(directory)) {
      ProcessorUtils.createDirectories(
          ProcessorUtils.DATA_BASE_DIR + BULK_ASSIGNEE_DIRECTORY + "/" + File.separator
              + INTERNAL_PROCESS_REQUEST_CODE);
      ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + BULK_ASSIGNEE_DIRECTORY + "/" + File.separator
          + INTERNAL_PROCESS_REQUEST_CODE + File.separator + fileName, excelFile);
    }
    else if (AUTO_APPROVED_BULK_ASSIGN.equals(directory)) {
      ProcessorUtils.createDirectories(
          ProcessorUtils.DATA_BASE_DIR + AUTO_APPROVED_BULK_ASSIGN + "/" + File.separator
              + INTERNAL_PROCESS_REQUEST_CODE);
      ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + AUTO_APPROVED_BULK_ASSIGN + "/" + File.separator
          + INTERNAL_PROCESS_REQUEST_CODE + File.separator + fileName, excelFile);
    } else if (BULK_PRICE_UPDATE.equals(directory)) {
      ProcessorUtils.createDirectories(
          ProcessorUtils.DATA_BASE_DIR + BULK_PRICE_UPDATE + "/" + File.separator + INTERNAL_PROCESS_REQUEST_CODE);
      ProcessorUtils.createFile(
          ProcessorUtils.DATA_BASE_DIR + BULK_PRICE_UPDATE + "/" + File.separator + INTERNAL_PROCESS_REQUEST_CODE
              + File.separator + fileName, excelFile);
    } else if (IPR_PRODUCT_BULK_ADD_REVIEW.equals(directory)) {
      ProcessorUtils.createDirectories(
          ProcessorUtils.DATA_BASE_DIR + IPR_PRODUCT_BULK_ADD_REVIEW + "/" + File.separator
              + INTERNAL_PROCESS_REQUEST_CODE);
      ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + IPR_PRODUCT_BULK_ADD_REVIEW + "/" + File.separator
          + INTERNAL_PROCESS_REQUEST_CODE + File.separator + fileName, excelFile);
    }
  }

  @Test
  public void processStoreCopyDataProductCreationTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_PRODUCT_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_FILE_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
            internalProcessService
                .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStatusAndBatchSize(ProcessStatus.PENDING.name(),
            BATCH_SIZE, BulkInternalProcessType.STORE_COPY.name(), STORE_ID, Arrays.asList(bulkInternalProcess)))
        .thenReturn(bulkInternalProcessMap);
    Mockito.when(
            internalProcessService.getAllProductByParentCodeAndProcessTypeAndInternalProcessRequestId(PRODUCT_CODE,
                bulkInternalProcess.getProcessType(), bulkInternalProcess.getId()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(0);
    internalProcessServiceWrapper
        .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_PRODUCT_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_FILE_TOTAL_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStatusAndBatchSize(ProcessStatus.PENDING.name(), BATCH_SIZE,
            BulkInternalProcessType.STORE_COPY.name(), STORE_ID, Arrays.asList(bulkInternalProcess));
    Mockito.verify(internalProcessService)
        .getAllProductByParentCodeAndProcessTypeAndInternalProcessRequestId(Mockito.isNull(),
            Mockito.isNull(), Mockito.anyString());
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).internalProcessRequestId(REQUEST_ID).build();
    Mockito.verify(kafkaProducer)
        .send(kafkaTopicProperties.getStoreCopyProductCreationDetails(),
            internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(kafkaTopicProperties, times(2)).getStoreCopyProductCreationDetails();
  }



  @Test
  public void processStoreCopyDataProductCreationNumberOfPagesGreaterThanOneTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue("20");
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_PRODUCT_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_FILE_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
            internalProcessService
                .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                    BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStatusAndBatchSize(ProcessStatus.PENDING.name(),
            BATCH_SIZE, BulkInternalProcessType.STORE_COPY.name(), STORE_ID, Arrays.asList(bulkInternalProcess)))
        .thenReturn(bulkInternalProcessMap);
    Mockito.when(
            internalProcessService.getAllProductByParentCodeAndProcessTypeAndInternalProcessRequestId(PRODUCT_CODE,
                bulkInternalProcess.getProcessType(), bulkInternalProcess.getId()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(0);
    internalProcessServiceWrapper
        .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_PRODUCT_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_FILE_TOTAL_BATCH_SIZE);
    Mockito.verify(internalProcessService, times(2))
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService, times(2))
        .getAllBulkInternalProcessDataByStatusAndBatchSize(ProcessStatus.PENDING.name(), BATCH_SIZE,
            BulkInternalProcessType.STORE_COPY.name(), STORE_ID, Arrays.asList(bulkInternalProcess));
    Mockito.verify(internalProcessService, times(2))
        .getAllProductByParentCodeAndProcessTypeAndInternalProcessRequestId(Mockito.isNull(),
            Mockito.isNull(), Mockito.anyString());
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).internalProcessRequestId(REQUEST_ID).build();
    Mockito.verify(kafkaProducer, times(2))
        .send(kafkaTopicProperties.getStoreCopyProductCreationDetails(),
            internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(internalProcessService, times(2)).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService, times(2))
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(kafkaTopicProperties, times(3)).getStoreCopyProductCreationDetails();
  }

  @Test
  public void processExceptionTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_PRODUCT_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_FILE_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
        internalProcessService
            .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStatusAndBatchSize(ProcessStatus.PENDING.name(),
        BATCH_SIZE, BulkInternalProcessType.STORE_COPY.name(), STORE_ID, Arrays.asList(bulkInternalProcess)))
        .thenReturn(bulkInternalProcessMap);
    Mockito.when(
        internalProcessService.getAllProductByParentCodeAndProcessTypeAndInternalProcessRequestId(PRODUCT_CODE,
            bulkInternalProcess.getProcessType(), bulkInternalProcess.getId()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(0);
    Mockito.doThrow(ApplicationRuntimeException.class).when(internalProcessService)
        .saveInternalProcesses(Mockito.anyList());
    internalProcessServiceWrapper
        .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_PRODUCT_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_FILE_TOTAL_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStatusAndBatchSize(ProcessStatus.PENDING.name(), BATCH_SIZE,
            BulkInternalProcessType.STORE_COPY.name(), STORE_ID, Arrays.asList(bulkInternalProcess));
    Mockito.verify(internalProcessService)
        .getAllProductByParentCodeAndProcessTypeAndInternalProcessRequestId(Mockito.isNull(),
            Mockito.isNull(), Mockito.anyString());
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).internalProcessRequestId(REQUEST_ID).build();
    Mockito.verify(kafkaProducer)
        .send(kafkaTopicProperties.getStoreCopyProductCreationDetails(),
            internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(kafkaTopicProperties, times(2)).getStoreCopyProductCreationDetails();
  }

  @Test
  public void processStoreCopyDataProductCreationNoProductTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_PRODUCT_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_FILE_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
            internalProcessService
                .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStatusAndBatchSize(ProcessStatus.PENDING.name(),
            BATCH_SIZE, BulkInternalProcessType.STORE_COPY.name(), STORE_ID, Arrays.asList(bulkInternalProcess)))
        .thenReturn(new HashMap<>());
    Mockito.when(internalProcessService
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(1);
    internalProcessServiceWrapper
        .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_PRODUCT_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_FILE_TOTAL_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStatusAndBatchSize(ProcessStatus.PENDING.name(), BATCH_SIZE,
            BulkInternalProcessType.STORE_COPY.name(), STORE_ID, Arrays.asList(bulkInternalProcess));
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
  }

  @Test
  @Disabled
  public void process_Suspension() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.SUSPENSION_FILE_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.SUSPEND.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(0);
    internalProcessServiceWrapper
        .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.SUSPEND.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.SUSPENSION_FILE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.SUSPEND.name());
    Mockito.verify(bulkProductSuspensionService).processToPublishForSuspension(eq(STORE_ID), eq(REQUEST_ID), any());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
  }

  @Disabled
  @Test
  public void process_ConfigUpdate() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.CONFIG_UPDATE_FILE_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.CONFIGURATION.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(0);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.CONFIGURATION.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.CONFIG_UPDATE_FILE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.CONFIGURATION.name());
    Mockito.verify(bulkConfigurationUpdateService).processToPublishForConfigUpdate(eq(STORE_ID), eq(REQUEST_ID), any());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
  }

  @Test
  @Disabled
  public void processStoreCopyDataProductCreationExceptionTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Mockito.doThrow(new Exception()).when(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_PRODUCT_FETCH_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.STORE_COPY_FILE_TOTAL_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    try {
      internalProcessServiceWrapper
          .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.STORE_COPY.name());
    } finally {
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_PRODUCT_FETCH_BATCH_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_FILE_TOTAL_BATCH_SIZE);
    }
  }

  @Test
  public void processStoreCopyDataProductCreationNoFileToProcessTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.STORE_COPY_FILE_TOTAL_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
            internalProcessService
                .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(), pageable, TOTAL_COUNT));
    internalProcessServiceWrapper
        .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.STORE_COPY_FILE_TOTAL_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
  }

  @Test
  public void processNewSalesCategoryUpdateRequestTest() throws IOException {
    getStoreCopyFiles(SALES_CATEGORY_UPDATE_DIRECTORY, SALES_CATEGORY_UPDATE_FILE_NAME_1);
    bulkInternalProcess.setFileName(SALES_CATEGORY_UPDATE_FILE_NAME_1);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    XSSFWorkbook workBook;
    try(InputStream is =
      new FileInputStream(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR + File.separator + INTERNAL_PROCESS_REQUEST_CODE + File.separator
      + SALES_CATEGORY_UPDATE_FILE_NAME_1)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FILE_BATCH_SIZE))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
              ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.SALES_CATEGORY_UPDATE.name()))
          .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService)
          .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      Assertions.assertEquals(11, internalProcessDataArgumentCaptor.getValue().size());
      Assertions.assertEquals(DATA_DEACTIVATE,
          internalProcessDataArgumentCaptor.getValue().get(0).getData());
    }
  }

  @Test
  public void processNewInternalProcessRequestRestrictedKeywordUpsertApplicableYes() throws IOException {
    List<CategoryDTO> categoryDTOS = new ArrayList<>();
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setCategoryCode(CATEGORY_CODE);
    categoryDTOS.add(categoryDTO);
    getStoreCopyFiles(RESTRICTED_KEYWORD_UPSERT_DIRECTORY, ALL_PERFECT_FILE);
    bulkInternalProcess.setFileName(ALL_PERFECT_FILE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + RESTRICTED_KEYWORD_UPSERT_DIRECTORY + "/" + File.separator
            + INTERNAL_PROCESS_REQUEST_CODE + File.separator + ALL_PERFECT_FILE)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      Mockito.when(internalProcessService.saveInternalProcess(any(BulkInternalProcess.class)))
          .thenReturn(bulkInternalProcess);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(
          pcbOutboundService.getChildFromParentByCatalogIdWithChildCount(STORE_ID, 0, 1, "ALL",
              "ALL", USER_NAME)).thenReturn(categoryDTOS);
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(pcbOutboundService)
          .getChildFromParentByCatalogIdWithChildCount(anyString(), anyInt(), anyInt(), anyString(),
              anyString(), anyString());
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(anyString(), anyString());
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService).saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      File file = new File(ProcessorUtils.DATA_BASE_DIR + RESTRICTED_KEYWORD_UPSERT_DIRECTORY);
      if (file.exists()) {
        FileUtils.deleteDirectory(file);
      }
    }
  }

  @Test
  public void processNewInternalProcessRequestRestrictedKeywordDeleteApplicableYes() throws IOException {
    List<CategoryDTO> categoryDTOS = new ArrayList<>();
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setCategoryCode(CATEGORY_CODE);
    categoryDTOS.add(categoryDTO);
    getStoreCopyFiles(RESTRICTED_KEYWORD_DELETE_DIRECTORY, DELETE_APPLICABLE_NO);
    bulkInternalProcess.setFileName(DELETE_APPLICABLE_NO);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + RESTRICTED_KEYWORD_DELETE_DIRECTORY + "/" + File.separator
            + INTERNAL_PROCESS_REQUEST_CODE + File.separator + DELETE_APPLICABLE_NO)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      Mockito.when(internalProcessService.saveInternalProcess(any(BulkInternalProcess.class)))
          .thenReturn(bulkInternalProcess);
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(
          pcbOutboundService.getChildFromParentByCatalogIdWithChildCount(STORE_ID, 0, 1, "ALL",
              "ALL", USER_NAME)).thenReturn(categoryDTOS);
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));

      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(anyString(), anyString());
      Mockito.verify(pcbOutboundService)
          .getChildFromParentByCatalogIdWithChildCount(anyString(), anyInt(), anyInt(), anyString(),
              anyString(), anyString());
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService).saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processNewInternalProcessRequestRestrictedKeywordUpsertApplicableNo() throws IOException {
    RestrictedKeywordRequestData restrictedKeywordRequestData = new RestrictedKeywordRequestData();
    restrictedKeywordRequestData.setApplicableCategoryList(Collections.singleton(CATEGORY_CODE));
    getStoreCopyFiles(RESTRICTED_KEYWORD_UPSERT_DIRECTORY, APPLICABLE_FOR_ALL_NO);
    bulkInternalProcess.setFileName(APPLICABLE_FOR_ALL_NO);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + RESTRICTED_KEYWORD_UPSERT_DIRECTORY + "/" + File.separator
            + INTERNAL_PROCESS_REQUEST_CODE + File.separator + APPLICABLE_FOR_ALL_NO)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      Mockito.when(internalProcessService.saveInternalProcess(any(BulkInternalProcess.class)))
          .thenReturn(bulkInternalProcess);
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(objectMapper.readValue(MESSAGE, RestrictedKeywordRequestData.class))
          .thenReturn(restrictedKeywordRequestData);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(anyString(), anyString());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService).saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processNewInternalProcessRequestRestrictedKeywordUpsertFailed() throws IOException {
    getStoreCopyFiles(RESTRICTED_KEYWORD_UPSERT_DIRECTORY, ACTION_EMPTY);
    bulkInternalProcess.setFileName(ACTION_EMPTY);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + RESTRICTED_KEYWORD_UPSERT_DIRECTORY + "/" + File.separator
            + INTERNAL_PROCESS_REQUEST_CODE + File.separator + ACTION_EMPTY)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      Mockito.when(internalProcessService.saveInternalProcess(any(BulkInternalProcess.class)))
          .thenReturn(bulkInternalProcess);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(anyString(), anyString());
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService).saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processNewInternalProcessRequestBrandAuthTest() throws IOException {
    getStoreCopyFiles(BRAND_AUTH_ADD_DIRECTORY, BRAND_AUTH_ADD_FILE);
    bulkInternalProcess.setFileName(BRAND_AUTH_ADD_FILE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + BRAND_AUTH_ADD_DIRECTORY + "/" + File.separator
            + INTERNAL_PROCESS_REQUEST_CODE + File.separator + BRAND_AUTH_ADD_FILE)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.BRAND_AUTH_ADD)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.BRAND_AUTH_ADD.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.BRAND_AUTH_ADD.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.BRAND_AUTH_ADD.name());
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(anyString(), anyString());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService, times(1))
          .saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processNewInternalProcessRequestBrandAuthDeleteElseCaseTest() throws IOException {
    getStoreCopyFiles(BRAND_AUTH_DELETE_DIRECTORY, BRAND_AUTH_DELETE_FILE);
    bulkInternalProcess.setFileName(BRAND_AUTH_DELETE_FILE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + BRAND_AUTH_DELETE_DIRECTORY + "/" + File.separator
            + INTERNAL_PROCESS_REQUEST_CODE + File.separator + BRAND_AUTH_DELETE_FILE)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.BRAND_AUTH_DELETE.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.BRAND_AUTH_DELETE.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.BRAND_AUTH_DELETE.name());
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(anyString(), anyString());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService, times(1))
          .saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }


  @Test
  public void processNewInternalProcessRequestBrandAuthDeleteTest() throws IOException {
    getStoreCopyFiles(BRAND_AUTH_DELETE_DIRECTORY, BRAND_AUTH_DELETE_2_FILE);
    bulkInternalProcess.setFileName(BRAND_AUTH_DELETE_2_FILE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + BRAND_AUTH_DELETE_DIRECTORY + "/" + File.separator
            + INTERNAL_PROCESS_REQUEST_CODE + File.separator + BRAND_AUTH_DELETE_2_FILE)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.BRAND_AUTH_DELETE.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.BRAND_AUTH_DELETE.name());
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.BRAND_AUTH_DELETE.name());
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(anyString(), anyString());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService, times(1))
          .saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processNewInternalProcessRequestBrandAuthElseCaseTest() throws IOException {
    getStoreCopyFiles(BRAND_AUTH_ADD_DIRECTORY, BRAND_AUTH_ADD_2_FILE);
    bulkInternalProcess.setFileName(BRAND_AUTH_ADD_2_FILE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + BRAND_AUTH_ADD_DIRECTORY + "/" + File.separator
            + INTERNAL_PROCESS_REQUEST_CODE + File.separator + BRAND_AUTH_ADD_2_FILE)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.BRAND_AUTH_ADD)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.BRAND_AUTH_ADD.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.BRAND_AUTH_ADD.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.BRAND_AUTH_ADD.name());
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(anyString(), anyString());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService, times(1))
          .saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processNewInternalProcessRequestBulkPriceUpdateTest() throws Exception {
    getStoreCopyFiles(BULK_PRICE_UPDATE, BULK_PRICE_UPDATE_FILE);
    bulkInternalProcess.setFileName(BULK_PRICE_UPDATE_FILE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + BULK_PRICE_UPDATE + "/" + File.separator + INTERNAL_PROCESS_REQUEST_CODE
            + File.separator + BULK_PRICE_UPDATE_FILE)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.BULK_PRICE_UPDATE.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.BULK_PRICE_UPDATE.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.BULK_PRICE_UPDATE.name());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(bulkPriceUpdateService)
          .sendEmailNotification(anyString(), anyString(), anyString(), anyString(), anyString(),
              anyString());
      Mockito.verify(internalProcessService, times(1))
          .saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processNewInternalProcessRequestBulkPriceUpdateMaxRowsTest() throws Exception {
    getStoreCopyFiles(BULK_PRICE_UPDATE, BULK_PRICE_UPDATE_FILE_1);
    bulkInternalProcess.setFileName(BULK_PRICE_UPDATE_FILE_1);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + BULK_PRICE_UPDATE + "/" + File.separator + INTERNAL_PROCESS_REQUEST_CODE
            + File.separator + BULK_PRICE_UPDATE_FILE_1)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.BULK_PRICE_UPDATE.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.BULK_PRICE_UPDATE.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.BULK_PRICE_UPDATE.name());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(bulkPriceUpdateService)
          .sendEmailNotification(anyString(), anyString(), anyString(), anyString(), anyString(),
              anyString());
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService, times(1))
          .saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }


  @Test
  public void processNewInternalProcessRequestBulkPriceUpdateAllFailedTest() throws IOException {
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "bulkPriceUpdateMaxRows", 2);
    getStoreCopyFiles(BULK_PRICE_UPDATE, BULK_PRICE_UPDATE_FILE_1);
    bulkInternalProcess.setFileName(BULK_PRICE_UPDATE_FILE_1);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + BULK_PRICE_UPDATE + "/" + File.separator + INTERNAL_PROCESS_REQUEST_CODE
            + File.separator + BULK_PRICE_UPDATE_FILE_1)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.BULK_PRICE_UPDATE.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.BULK_PRICE_UPDATE.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.BULK_PRICE_UPDATE.name());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService, times(1))
          .saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processNewInternalProcessRequestBulkApproval() throws IOException {
    getStoreCopyFiles(BULK_APPROVAL_DIRECTORY, BULK_APPROVAL_2_FILE);
    bulkInternalProcess.setFileName(BULK_APPROVAL_2_FILE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_APPROVAL.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + BULK_APPROVAL_DIRECTORY + "/" + File.separator
            + INTERNAL_PROCESS_REQUEST_CODE + File.separator + BULK_APPROVAL_2_FILE)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.BULK_APPROVAL)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.BULK_APPROVAL.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.BULK_APPROVAL.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.BULK_APPROVAL.name());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService, times(1))
          .saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processNewInternalProcessRequestBulkApprovalForAllFailed() throws IOException {
    getStoreCopyFiles(BULK_APPROVAL_DIRECTORY, BULK_APPROVAL_FAILED);
    bulkInternalProcess.setFileName(BULK_APPROVAL_FAILED);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_APPROVAL.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
            ProcessorUtils.DATA_BASE_DIR + BULK_APPROVAL_DIRECTORY + "/" + File.separator
                    + INTERNAL_PROCESS_REQUEST_CODE + File.separator + BULK_APPROVAL_FAILED)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.BULK_APPROVAL)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.BULK_APPROVAL.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.BULK_APPROVAL.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.BULK_APPROVAL.name());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService, times(1))
          .saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processNewInternalProcessRequestRejection() throws IOException {
    getStoreCopyFiles(BULK_REJECTION_DIRECTORY, BULK_REJECTION_FILE);
    bulkInternalProcess.setFileName(BULK_REJECTION_FILE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_REJECTION.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
            ProcessorUtils.DATA_BASE_DIR + BULK_REJECTION_DIRECTORY + "/" + File.separator
                    + INTERNAL_PROCESS_REQUEST_CODE + File.separator + BULK_REJECTION_FILE)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.BULK_REJECTION)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.BULK_REJECTION.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.BULK_REJECTION.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.BULK_REJECTION.name());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService, times(1))
          .saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processNewInternalProcessRequestBulkAssigneeMasterSkuReview() throws IOException {
    getStoreCopyFiles(BULK_ASSIGNEE_DIRECTORY, BULK_ASSIGNEE_FILE);
    bulkInternalProcess.setFileName(BULK_ASSIGNEE_FILE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
      ProcessorUtils.DATA_BASE_DIR + BULK_ASSIGNEE_DIRECTORY + "/" + File.separator
        + INTERNAL_PROCESS_REQUEST_CODE + File.separator + BULK_ASSIGNEE_FILE)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService, times(1))
          .saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    }
  }

  @Test
  public void processNewInternalProcessRequestBulkAssigneeAutoApprovedProducts() throws IOException {
    getStoreCopyFiles(AUTO_APPROVED_BULK_ASSIGN, BULK_ASSIGN_FILE);
    bulkInternalProcess.setFileName(BULK_ASSIGN_FILE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + AUTO_APPROVED_BULK_ASSIGN + "/" + File.separator
            + INTERNAL_PROCESS_REQUEST_CODE + File.separator + BULK_ASSIGN_FILE)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      UserResponse userResponse = new UserResponse();
      userResponse.setUsername(SYSTEM);
      userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
      Mockito.when(partnersEngineOutboundService.userFilter(
              VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
              VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null))
          .thenReturn(userResponseListBaseResponse);
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1),
              BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService, times(1))
          .saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      verify(partnersEngineOutboundService).userFilter(
          VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
          VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null);
    }
  }

  @Test
  public void processNewInternalProcessRequestBulkMasterSkuReviewTest() throws IOException {
    getStoreCopyFiles(BULK_MASTER_SKU_REVIEW_DIRECTORY, BULK_MASTER_SKU_REVIEW_FILE);
    bulkInternalProcess.setFileName(BULK_MASTER_SKU_REVIEW_FILE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + BULK_MASTER_SKU_REVIEW_DIRECTORY + "/" + File.separator
            + INTERNAL_PROCESS_REQUEST_CODE + File.separator + BULK_MASTER_SKU_REVIEW_FILE)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.MASTER_SKU_BULK_REVIEW)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      Mockito.verify(fileStorageService)
          .getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService, times(1))
          .saveInternalProcess(any(BulkInternalProcess.class));
    }
  }

  @Test
  public void processNewSalesCategoryUpdateRequestInvalidInputTest() throws IOException {
    getStoreCopyFiles(SALES_CATEGORY_UPDATE_DIRECTORY, SALES_CATEGORY_UPDATE_FILE_NAME_2);
    bulkInternalProcess.setFileName(SALES_CATEGORY_UPDATE_FILE_NAME_2);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR + File.separator + INTERNAL_PROCESS_REQUEST_CODE
      + File.separator + SALES_CATEGORY_UPDATE_FILE_NAME_2)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FILE_BATCH_SIZE))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
              ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.SALES_CATEGORY_UPDATE.name()))
          .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
      verify(fileStorageService).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService)
          .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      Assertions.assertEquals(11, internalProcessDataArgumentCaptor.getValue().size());
      Assertions.assertEquals(ProcessStatus.FAILED.name(),
          internalProcessDataArgumentCaptor.getValue().get(8).getStatus());
      Assertions.assertEquals(ProcessStatus.FAILED.name(),
          internalProcessDataArgumentCaptor.getValue().get(9).getStatus());
      Assertions.assertEquals(ProcessStatus.FAILED.name(),
          internalProcessDataArgumentCaptor.getValue().get(10).getStatus());
    }
  }

  @Test
  public void processNewSalesCategoryUpdateRequestEmptyFileTest() throws IOException {
    getStoreCopyFiles(SALES_CATEGORY_UPDATE_DIRECTORY, SALES_CATEGORY_UPDATE_FILE_NAME_3);
    bulkInternalProcess.setFileName(SALES_CATEGORY_UPDATE_FILE_NAME_3);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    XSSFWorkbook workBook;
    try(InputStream is =
      new FileInputStream(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR + File.separator + INTERNAL_PROCESS_REQUEST_CODE + File.separator
      + SALES_CATEGORY_UPDATE_FILE_NAME_3)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FILE_BATCH_SIZE))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
              ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.SALES_CATEGORY_UPDATE.name()))
          .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
      verify(fileStorageService).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
      Mockito.verify(internalProcessService)
          .saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
      Mockito.verify(mailDeliveryService)
          .sendEmailByForInternalBulkProcess(bulkInternalProcessArgumentCaptor.capture(),
              eq(ProcessStatus.FAILED.name()));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      Assertions.assertEquals(ProcessStatus.FAILED.name(),
          bulkInternalProcessArgumentCaptor.getValue().getStatus());
    }
  }

  @Test
  public void processNewSalesCategoryUpdateRequestHeaderCountWrongTest() throws IOException {
    getStoreCopyFiles(SALES_CATEGORY_UPDATE_DIRECTORY, SALES_CATEGORY_UPDATE_FILE_NAME_4);
    bulkInternalProcess.setFileName(SALES_CATEGORY_UPDATE_FILE_NAME_4);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    XSSFWorkbook workBook;
    try(InputStream is =
      new FileInputStream(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR + File.separator + INTERNAL_PROCESS_REQUEST_CODE + File.separator
        + SALES_CATEGORY_UPDATE_FILE_NAME_4)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FILE_BATCH_SIZE))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
              ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.SALES_CATEGORY_UPDATE.name()))
          .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
      verify(fileStorageService).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(internalProcessService)
          .saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
      Mockito.verify(mailDeliveryService)
          .sendEmailByForInternalBulkProcess(bulkInternalProcessArgumentCaptor.capture(),
              eq(ProcessStatus.FAILED.name()));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      Assertions.assertEquals(ProcessStatus.FAILED.name(),
          bulkInternalProcessArgumentCaptor.getValue().getStatus());
    }
  }

  @Test
  public void processNewSalesCategoryUpdateRequestHeaderNameWrongTest() throws IOException {
    getStoreCopyFiles(SALES_CATEGORY_UPDATE_DIRECTORY, SALES_CATEGORY_UPDATE_FILE_NAME_5);
    bulkInternalProcess.setFileName(SALES_CATEGORY_UPDATE_FILE_NAME_5);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    XSSFWorkbook workBook;
    try(InputStream is =
      new FileInputStream(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR + File.separator + INTERNAL_PROCESS_REQUEST_CODE + File.separator
      + SALES_CATEGORY_UPDATE_FILE_NAME_5)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE)
          .relativePath(bulkInternalProcess.getFileName())
          .createdBy(bulkInternalProcess.getCreatedBy()).build();
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      systemParameterConfig.setValue(FILE_BATCH_SIZE);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FILE_BATCH_SIZE))
          .thenReturn(systemParameterConfig);
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
              ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.SALES_CATEGORY_UPDATE.name()))
          .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
              BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
      Mockito.verify(internalProcessService)
          .saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
      verify(fileStorageService).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
      Mockito.verify(mailDeliveryService)
          .sendEmailByForInternalBulkProcess(bulkInternalProcessArgumentCaptor.capture(),
              eq(ProcessStatus.FAILED.name()));
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      Assertions.assertEquals(ProcessStatus.FAILED.name(),
          bulkInternalProcessArgumentCaptor.getValue().getStatus());
    }
  }

  @Test
  public void processUpdateSalesCategoryTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE).internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);

    Mockito.when(internalProcessService
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FILE_TOTAL_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
        internalProcessService
            .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.SALES_CATEGORY_UPDATE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, Arrays.asList(INTERNAL_PROCESS_REQUEST_ID),
            ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE))).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(
        internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);

    internalProcessServiceWrapper
        .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FILE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FETCH_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FILE_TOTAL_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, Arrays.asList(INTERNAL_PROCESS_REQUEST_ID),
            ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE));
    Mockito.verify(
        internalProcessService).saveBulkInternalProcessData(bulkInternalProcessData);
    Mockito.verify(kafkaProducer)
        .send(kafkaTopicProperties.getUpdateSalesCategoryDetails(),
            internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(kafkaTopicProperties, times(2)).getUpdateSalesCategoryDetails();
  }

  @Test
  public void processUpdateRestrictedCategoryUpsertTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE).internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);

    Mockito.when(internalProcessService
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
            internalProcessService
                .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                    BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, Arrays.asList(INTERNAL_PROCESS_REQUEST_ID),
            ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE))).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(
            internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);

    internalProcessServiceWrapper
        .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_TOTAL_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
  }

  @Test
  public void processUpdateRestrictedCategoryDeleteTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE).internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);

    Mockito.when(internalProcessService
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
            internalProcessService
                .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                    BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, Arrays.asList(INTERNAL_PROCESS_REQUEST_ID),
            ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE))).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(
            internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);

    internalProcessServiceWrapper
        .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_TOTAL_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
  }

  @Test
  public void processInternalBulkUploadTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel =
        InternalBulkUploadDataDomainEventModel.builder().storeId(STORE_ID)
            .updatedBy(CREATED_DATE).internalProcessDataRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    bulkInternalProcessData.setId(REQUEST_ID);
    Mockito.when(internalProcessService
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_BLUK_UPLOAD_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.INTERNAL_BLUK_UPLOAD_FILE_TOTAL_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
        internalProcessService
            .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, Arrays.asList(INTERNAL_PROCESS_REQUEST_ID),
            ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE))).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(
        internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);

    internalProcessServiceWrapper
        .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BLUK_UPLOAD_FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.INTERNAL_BLUK_UPLOAD_FILE_TOTAL_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name());
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, Arrays.asList(INTERNAL_PROCESS_REQUEST_ID),
            ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE));
    Mockito.verify(
        internalProcessService).saveBulkInternalProcessData(bulkInternalProcessData);
    Mockito.verify(kafkaProducer)
        .send(kafkaTopicProperties.getInternalBulkUploadDetails(),
            internalBulkUploadDataDomainEventModel);
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(kafkaTopicProperties, times(2)).getInternalBulkUploadDetails();
  }

  @Test
  public void processInternalVendorBulkAssignmentTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Collections.singletonList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel =
        InternalBulkUploadDataDomainEventModel.builder().storeId(STORE_ID).internalProcessDataRequestId(REQUEST_ID)
            .build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    bulkInternalProcessData.setId(REQUEST_ID);
    Mockito.when(internalProcessService
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FILE_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FILE_TOTAL_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, Collections.singletonList(INTERNAL_PROCESS_REQUEST_ID),
            ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE))).thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FILE_TOTAL_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, Collections.singletonList(INTERNAL_PROCESS_REQUEST_ID),
            ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkInternalProcessData);
    Mockito.verify(kafkaProducer)
        .send(kafkaTopicProperties.getBulkVendorAssignmentEvent(),
            internalBulkUploadDataDomainEventModel);
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkVendorAssignmentEvent();
  }

  @Test
  public void processInternalVendorAutoAssignmentTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Collections.singletonList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel =
        InternalBulkUploadDataDomainEventModel.builder().storeId(STORE_ID).internalProcessDataRequestId(REQUEST_ID)
            .build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    bulkInternalProcessData.setId(REQUEST_ID);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_TOTAL_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Collections.singletonList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(),
        Integer.parseInt(FETCH_BATCH_SIZE))).thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_TOTAL_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(internalProcessService).getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Collections.singletonList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(),
        Integer.parseInt(FETCH_BATCH_SIZE));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkInternalProcessData);
    Mockito.verify(kafkaProducer)
        .send(kafkaTopicProperties.getBulkVendorAssignmentEvent(),
          internalBulkUploadDataDomainEventModel);
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkVendorAssignmentEvent();
  }

  @Test
  public void processDeleteBulkAuthorisationTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE).internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);

    Mockito.when(internalProcessService
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(eq(STORE_ID), any())).thenReturn(systemParameterConfig);
    Mockito.when(
        internalProcessService
            .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, Arrays.asList(INTERNAL_PROCESS_REQUEST_ID),
            ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE))).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(
        internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);

    internalProcessServiceWrapper
        .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DELETE_BRAND_AUTH_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DELETE_BRAND_AUTH_FILE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DELETE_BRAND_AUTH_FILE_TOTAL_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name());
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, Arrays.asList(INTERNAL_PROCESS_REQUEST_ID),
            ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE));
    Mockito.verify(
        internalProcessService).saveBulkInternalProcessData(bulkInternalProcessData);
    Mockito.verify(kafkaProducer)
        .send(kafkaTopicProperties.getDeleteBrandAuthorisation(),
            internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(kafkaTopicProperties, times(2)).getDeleteBrandAuthorisation();
  }

  @Test
  public void processDeleteBulkAuthorisationNullTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE).internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);

    Mockito.when(internalProcessService
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(eq(STORE_ID), any())).thenReturn(systemParameterConfig);
    Mockito.when(
        internalProcessService
            .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, Arrays.asList(INTERNAL_PROCESS_REQUEST_ID),
            ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE))).thenReturn(null);
    Mockito.when(
        internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);

    internalProcessServiceWrapper
        .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DELETE_BRAND_AUTH_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DELETE_BRAND_AUTH_FILE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name());
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, Arrays.asList(INTERNAL_PROCESS_REQUEST_ID),
            ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE));
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
  }

  @Test
  public void processInternalBulkUploadEventTest() throws Exception {
    Mockito.when(internalProcessService.getBulkInternalProcessDataById(STORE_ID, REQUEST_ID))
        .thenReturn(bulkInternalProcessData1);
    internalProcessServiceWrapper
        .processInternalBulkUploadEvent(STORE_ID, UPDATED_BY, BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name(),
            REQUEST_ID);
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataById(STORE_ID, REQUEST_ID);
    Mockito.verify(productRepository).updateMasterProducts(any(BulkMasterProductUpdateRequest.class));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkInternalProcessDataArgumentCaptor.capture());
    Assertions.assertEquals(ProcessStatus.COMPLETED.name(), bulkInternalProcessDataArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processInternalBulkUploadEventExceptionTest() throws Exception {
    BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData1.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData1.setData(INTERNAL_BULK_UPLOAD_DATA);
    Mockito.when(internalProcessService.getBulkInternalProcessDataById(STORE_ID, REQUEST_ID))
        .thenReturn(bulkInternalProcessData1);
    Mockito.doThrow(ApplicationRuntimeException.class).when(productRepository)
        .updateMasterProducts(any(BulkMasterProductUpdateRequest.class));
    internalProcessServiceWrapper
        .processInternalBulkUploadEvent(STORE_ID, UPDATED_BY, BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name(),
            REQUEST_ID);
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataById(STORE_ID, REQUEST_ID);
    Mockito.verify(productRepository)
        .updateMasterProducts(any(BulkMasterProductUpdateRequest.class));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkInternalProcessDataArgumentCaptor.capture());
    Mockito.verify(trackerService).sendTracker(MASTER_PRODUCT_BULK_UPDATE, MASTER_PRODUCT_UPDATE, HYPHEN, FAILED, UPDATED_BY);
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(Constant.SYSTEM_ERROR, bulkInternalProcessDataArgumentCaptor.getValue().getErrorMessage());
  }


  @Test
  public void processUpdateSalesCategoryEventTest() {
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CATALOG_TYPE);
    categoryResponse.setCatalog(catalogResponse);
    BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData1.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData1.setData(DATA_ACTIVATE);
    BulkInternalProcessData bulkInternalProcessData2 = new BulkInternalProcessData();
    bulkInternalProcessData2.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData2.setData(DATA_DEACTIVATE);
    BulkInternalProcessData bulkInternalProcessData3 = new BulkInternalProcessData();
    bulkInternalProcessData3.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData3.setData(INVALID_DATA);

    Mockito.when(
      internalProcessService.getAllProductByParentCodeAndProcessTypeAndStatusAndInternalProcessRequestId(
        PRODUCT_SKU, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(),
        ProcessStatus.IN_PROGRESS.name(), REQUEST_ID)).thenReturn(
      Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2, bulkInternalProcessData3));
    Mockito.when(this.internalProcessService.saveInternalProcessData(
      Mockito.anyList())).thenReturn(
      Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2, bulkInternalProcessData3));
    Mockito.doNothing().when(xProductOutboundService)
        .addSalesCategory(STORE_ID, CATALOG_CODE, CATEGORY_CODE, Arrays.asList(PRODUCT_SKU));
    Mockito.when(this.pcbOutboundService.getBasicCategoryInfoAndCatalogInfo(Mockito.anyString()))
        .thenReturn(categoryResponse);
    Mockito.doNothing().when(xProductOutboundService)
        .deleteSalesCategory(STORE_ID, CATALOG_CODE, CATEGORY_CODE, Arrays.asList(PRODUCT_SKU));

    internalProcessServiceWrapper
        .processUpdateSalesCategoryEvent(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(), PRODUCT_SKU,
            REQUEST_ID);

    Mockito.verify(internalProcessService)
        .getAllProductByParentCodeAndProcessTypeAndStatusAndInternalProcessRequestId(PRODUCT_SKU,
            BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(), ProcessStatus.IN_PROGRESS.name(),
          REQUEST_ID);
    Mockito.verify(pcbOutboundService, times(3))
        .getBasicCategoryInfoAndCatalogInfo(CATEGORY_CODE);
    Mockito.verify(xProductOutboundService)
        .addSalesCategory(STORE_ID, CATALOG_CODE, CATEGORY_CODE, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(xProductOutboundService)
        .deleteSalesCategory(STORE_ID, CATALOG_CODE, CATEGORY_CODE, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(internalProcessService,
      times(2)).saveInternalProcessData(Mockito.anyList());
  }

  @Test
  public void processDeleteBrandAuthorisationEventTest() throws JsonProcessingException {
    brandAuthorisationRequest.setBrandCode(BRAND_CODE);
    brandAuthorisationRequest.setSellerCode(SELLER_CODE);
    bulkInternalProcessData1.setData(mapper.writeValueAsString(brandAuthorisationRequest));
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
        ID, ProcessStatus.IN_PROGRESS.name())).thenReturn(bulkInternalProcessData1);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData1);
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.any())).thenReturn(bulkInternalProcessDataList);
    internalProcessServiceWrapper.processDeleteBrandAuthorisationEvent(STORE_ID, PROCESS_TYPE_DELETE_BRAND_AUTH, ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(STORE_ID, ID, ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(pcbOutboundService).deleteBrandAuthorisation(STORE_ID, SELLER_CODE, BRAND_CODE);
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.any());
  }

  @Test
  public void processDeleteBrandAuthorisationEventNullTest() throws JsonProcessingException {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
        ID, ProcessStatus.IN_PROGRESS.name())).thenReturn(null);
    internalProcessServiceWrapper.processDeleteBrandAuthorisationEvent(STORE_ID, PROCESS_TYPE_DELETE_BRAND_AUTH, ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(STORE_ID, ID, ProcessStatus.IN_PROGRESS.name());
  }

  @Test
  public void processDeleteBrandAuthorisationEventErrorTest() throws JsonProcessingException {
    brandAuthorisationRequest.setBrandCode(BRAND_CODE);
    brandAuthorisationRequest.setSellerCode(SELLER_CODE);
    bulkInternalProcessData1.setData(mapper.writeValueAsString(brandAuthorisationRequest));
    Mockito.doThrow(ApplicationRuntimeException.class).when(pcbOutboundService).deleteBrandAuthorisation(STORE_ID, SELLER_CODE, BRAND_CODE);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
        ID, ProcessStatus.IN_PROGRESS.name())).thenReturn(bulkInternalProcessData1);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData1);
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(bulkInternalProcessDataList);
    try {
      internalProcessServiceWrapper.processDeleteBrandAuthorisationEvent(STORE_ID, PROCESS_TYPE_DELETE_BRAND_AUTH, ID);
    } finally {
      Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(STORE_ID, ID, ProcessStatus.IN_PROGRESS.name());
      Mockito.verify(pcbOutboundService).deleteBrandAuthorisation(STORE_ID, SELLER_CODE, BRAND_CODE);
      Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
      Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    }
  }

  @Test
  public void getCategoryDetailResponseNullTest() {
    BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData1.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData1.setData(DATA_ACTIVATE);
    BulkInternalProcessData bulkInternalProcessData2 = new BulkInternalProcessData();
    bulkInternalProcessData2.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData2.setData(DATA_DEACTIVATE);
    BulkInternalProcessData bulkInternalProcessData3 = new BulkInternalProcessData();
    bulkInternalProcessData3.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData3.setData(INVALID_DATA);

    Mockito.when(
        internalProcessService.getAllProductByParentCodeAndProcessTypeAndStatusAndInternalProcessRequestId(
            PRODUCT_SKU, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(),
            ProcessStatus.IN_PROGRESS.name(), REQUEST_ID)).thenReturn(
        Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2, bulkInternalProcessData3));
    Mockito.when(this.internalProcessService.saveInternalProcessData(
        Mockito.anyList())).thenReturn(
        Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2, bulkInternalProcessData3));
    Mockito.doNothing().when(xProductOutboundService)
        .addSalesCategory(STORE_ID, CATALOG_CODE, CATEGORY_CODE, Arrays.asList(PRODUCT_SKU));
    Mockito.when(this.pcbOutboundService.getBasicCategoryInfoAndCatalogInfo(Mockito.anyString()))
        .thenReturn(null);
    Mockito.doNothing().when(xProductOutboundService)
        .deleteSalesCategory(STORE_ID, CATALOG_CODE, CATEGORY_CODE, Arrays.asList(PRODUCT_SKU));

    internalProcessServiceWrapper
        .processUpdateSalesCategoryEvent(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(), PRODUCT_SKU,
            REQUEST_ID);

    Mockito.verify(internalProcessService)
        .getAllProductByParentCodeAndProcessTypeAndStatusAndInternalProcessRequestId(PRODUCT_SKU,
            BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(), ProcessStatus.IN_PROGRESS.name(),
            REQUEST_ID);
    Mockito.verify(pcbOutboundService, times(3))
        .getBasicCategoryInfoAndCatalogInfo(CATEGORY_CODE);
    Mockito.verify(internalProcessService,
        times(2)).saveInternalProcessData(Mockito.anyList());
  }

  @Test
  public void getCategoryDetailResponseNotSalesCatalogTest() {
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(MASTER_TYPE);
    categoryResponse.setCatalog(catalogResponse);
    BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData1.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData1.setData(DATA_DEACTIVATE);
    Mockito.when(
        internalProcessService.getAllProductByParentCodeAndProcessTypeAndStatusAndInternalProcessRequestId(
            PRODUCT_SKU, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(),
            ProcessStatus.IN_PROGRESS.name(), REQUEST_ID)).thenReturn(
        Arrays.asList(bulkInternalProcessData1));
    Mockito.when(this.internalProcessService.saveInternalProcessData(
        Mockito.anyList())).thenReturn(
        Arrays.asList(bulkInternalProcessData1));
    categoryDetailResponse.getCatalog().setCatalogType(MASTER_TYPE);
    Mockito.when(this.pcbOutboundService.getBasicCategoryInfoAndCatalogInfo(Mockito.anyString()))
        .thenReturn(categoryResponse);
    Mockito.doThrow(ApplicationRuntimeException.class).when(xProductOutboundService)
        .deleteSalesCategory(STORE_ID, CATALOG_CODE, CATEGORY_CODE, Arrays.asList(PRODUCT_SKU));

    internalProcessServiceWrapper
        .processUpdateSalesCategoryEvent(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(), PRODUCT_SKU,
            REQUEST_ID);

    Mockito.verify(internalProcessService)
        .getAllProductByParentCodeAndProcessTypeAndStatusAndInternalProcessRequestId(PRODUCT_SKU,
            BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(),
            ProcessStatus.IN_PROGRESS.name(), REQUEST_ID);
    Mockito.verify(pcbOutboundService)
        .getBasicCategoryInfoAndCatalogInfo(CATEGORY_CODE);
    Mockito.verify(internalProcessService, times(2))
        .saveInternalProcessData(Mockito.anyList());
  }
  @Test
  public void processUpdateSalesCategoryEventExceptionTest() {
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CATALOG_TYPE);
    categoryResponse.setCatalog(catalogResponse);
    BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData1.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData1.setData(DATA_DEACTIVATE);
    Mockito.when(
      internalProcessService.getAllProductByParentCodeAndProcessTypeAndStatusAndInternalProcessRequestId(
        PRODUCT_SKU, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(),
        ProcessStatus.IN_PROGRESS.name(), REQUEST_ID)).thenReturn(
      Arrays.asList(bulkInternalProcessData1));
    Mockito.when(this.internalProcessService.saveInternalProcessData(
      Mockito.anyList())).thenReturn(
      Arrays.asList(bulkInternalProcessData1));
    Mockito.when(this.pcbOutboundService.getBasicCategoryInfoAndCatalogInfo(Mockito.anyString()))
        .thenReturn(categoryResponse);
    Mockito.doThrow(ApplicationRuntimeException.class).when(xProductOutboundService)
        .deleteSalesCategory(STORE_ID, CATALOG_CODE, CATEGORY_CODE, Arrays.asList(PRODUCT_SKU));

    internalProcessServiceWrapper
        .processUpdateSalesCategoryEvent(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(), PRODUCT_SKU,
            REQUEST_ID);

    Mockito.verify(internalProcessService)
        .getAllProductByParentCodeAndProcessTypeAndStatusAndInternalProcessRequestId(PRODUCT_SKU,
            BulkInternalProcessType.SALES_CATEGORY_UPDATE.name(),
          ProcessStatus.IN_PROGRESS.name(), REQUEST_ID);
    Mockito.verify(xProductOutboundService)
        .deleteSalesCategory(STORE_ID, CATALOG_CODE, CATEGORY_CODE, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(pcbOutboundService)
      .getBasicCategoryInfoAndCatalogInfo(CATEGORY_CODE);
    Mockito.verify(internalProcessService, times(2))
      .saveInternalProcessData(Mockito.anyList());
  }

  @Test
  public void deleteOldBulkInternalProcessRequestStoreCopyTest() throws IOException {
    getStoreCopyFiles(STORE_COPY_DIRECTORY, FILE_NAME);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_DELETE_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.STORE_COPY_DELETE_BATCH_SIZE, "1",
            SystemParameterConfigNames.STORE_COPY_DELETE_BATCH_SIZE));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_DELETE_DAYS_BEFORE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.STORE_COPY_DELETE_DAYS_BEFORE, "10",
            SystemParameterConfigNames.STORE_COPY_DELETE_DAYS_BEFORE));
    Mockito.when(internalProcessService
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(new PageImpl(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), 2));
    Mockito.doNothing().when(internalProcessService)
        .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);

    internalProcessServiceWrapper
        .deleteOldBulkInternalProcessRequest(STORE_ID, BulkInternalProcessType.STORE_COPY.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_DELETE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_DELETE_DAYS_BEFORE);
    Mockito.verify(internalProcessService, Mockito.times(2))
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class));
    Mockito.verify(internalProcessService,  Mockito.times(2))
        .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);
  }

  @Test
  public void deleteOldBulkInternalProcessRequestRestrictedKeywordUpsertTest() throws IOException {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_DATA_DELETE_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_DATA_DELETE_BATCH_SIZE, "1",
            SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_DATA_DELETE_BATCH_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_DATA_DELETE_BEFORE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_DATA_DELETE_BEFORE, "10",
            SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_DATA_DELETE_BEFORE));
    Mockito.when(internalProcessService
            .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
                Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(new PageImpl(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), 2));
    Mockito.doNothing().when(internalProcessService)
        .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);

    internalProcessServiceWrapper
        .deleteOldBulkInternalProcessRequest(STORE_ID, BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_DATA_DELETE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_DATA_DELETE_BEFORE);
    Mockito.verify(internalProcessService, Mockito.times(2))
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class));
    Mockito.verify(internalProcessService,  Mockito.times(2))
        .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);
  }

  @Test
  public void deleteOldBulkInternalProcessRequestRestrictedKeywordDeleteTest() throws IOException {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_DATA_DELETE_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_DATA_DELETE_BATCH_SIZE, "1",
            SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_DATA_DELETE_BATCH_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_DATA_DELETE_BEFORE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_DATA_DELETE_BEFORE, "10",
            SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_DATA_DELETE_BEFORE));
    Mockito.when(internalProcessService
            .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
                Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(new PageImpl(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), 2));
    Mockito.doNothing().when(internalProcessService)
        .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);

    internalProcessServiceWrapper
        .deleteOldBulkInternalProcessRequest(STORE_ID, BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_DATA_DELETE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_DATA_DELETE_BEFORE);
    Mockito.verify(internalProcessService, Mockito.times(2))
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class));
    Mockito.verify(internalProcessService,  Mockito.times(2))
        .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);
  }

  @Test
  public void deleteOldBulkInternalProcessRequestStoreCopyEmptyTest() throws IOException {
    getStoreCopyFiles(STORE_COPY_DIRECTORY, FILE_NAME);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_DELETE_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.STORE_COPY_DELETE_BATCH_SIZE, "1",
            SystemParameterConfigNames.STORE_COPY_DELETE_BATCH_SIZE));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_DELETE_DAYS_BEFORE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.STORE_COPY_DELETE_DAYS_BEFORE, "10",
            SystemParameterConfigNames.STORE_COPY_DELETE_DAYS_BEFORE));
    Mockito.when(internalProcessService
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(new PageImpl(new ArrayList(), PageRequest.of(0, 1), 1));
    Mockito.doNothing().when(internalProcessService)
        .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);

    internalProcessServiceWrapper
        .deleteOldBulkInternalProcessRequest(STORE_ID, BulkInternalProcessType.STORE_COPY.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_DELETE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_DELETE_DAYS_BEFORE);
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class));
  }

  @Test
  public void deleteOldBulkInternalProcessRequestSalesCatgeoryUpdateTest() throws IOException {
    getStoreCopyFiles(SALES_CATEGORY_UPDATE_DIRECTORY, SALES_CATEGORY_UPDATE_FILE_NAME_1);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_SALES_CATEGORY_DELETE_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_SALES_CATEGORY_DELETE_BATCH_SIZE, "1",
            SystemParameterConfigNames.UPDATE_SALES_CATEGORY_DELETE_BATCH_SIZE));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_SALES_CATEGORY_DELETE_DAYS_BEFORE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_SALES_CATEGORY_DELETE_DAYS_BEFORE, "10",
            SystemParameterConfigNames.UPDATE_SALES_CATEGORY_DELETE_DAYS_BEFORE));
    Mockito.when(internalProcessService
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(new PageImpl(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), 2));
    Mockito.doNothing().when(internalProcessService)
        .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);

    internalProcessServiceWrapper
        .deleteOldBulkInternalProcessRequest(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_SALES_CATEGORY_DELETE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_SALES_CATEGORY_DELETE_DAYS_BEFORE);
    Mockito.verify(internalProcessService, Mockito.times(2))
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class));
    Mockito.verify(internalProcessService,  Mockito.times(2))
        .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);
  }

  private String getDuplicateInternalBulkUploadFiles(String directory, String fileName) throws IOException {
    ClassLoader classLoader = getClass().getClassLoader();
    String filePathPrefix = classLoader.getResource(directory).getPath();
    FileUtils.copyFile(new File(filePathPrefix+ File.separator + fileName),
        new File( filePathPrefix+ File.separator + INTERNAL_UPLOAD_FILE_NAME_DUMMY));
    File fileDummy = new File(filePathPrefix + File.separator + INTERNAL_UPLOAD_FILE_NAME_DUMMY);
    return fileDummy.getAbsolutePath();
  }

  @Test
  public void deleteOldBulkInternalProcessRequestVendotAutoAssignmentTest() throws IOException {
    getStoreCopyFiles(STORE_COPY_DIRECTORY, FILE_NAME);
    Mockito.when(systemParameterConfigService
      .findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_DELETE_BATCH_SIZE)).thenReturn(
      new SystemParameterConfig(SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_DELETE_BATCH_SIZE, "1",
        SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_DELETE_BATCH_SIZE));
    Mockito.when(systemParameterConfigService
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_DELETE_DAYS_BEFORE)).thenReturn(
      new SystemParameterConfig(SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_DELETE_DAYS_BEFORE, "10",
        SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_DELETE_DAYS_BEFORE));
    Mockito.when(internalProcessService
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
          Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class)))
      .thenReturn(new PageImpl(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), 2));
    Mockito.doNothing().when(internalProcessService)
      .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);

    internalProcessServiceWrapper
      .deleteOldBulkInternalProcessRequest(STORE_ID, BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());

    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_DELETE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_DELETE_DAYS_BEFORE);
    Mockito.verify(internalProcessService, Mockito.times(2))
      .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class));
    Mockito.verify(internalProcessService,  Mockito.times(2))
      .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);
  }



  @Test
  public void deleteOldBulkInternalProcessRequestInternalBulkUploadTest() throws IOException {
    bulkInternalProcess.setFileName(getDuplicateInternalBulkUploadFiles(BULK_UPDATE_FOLDER, INTERNAL_UPLOAD_FILE_NAME));
    bulkInternalProcess.setProcessType(BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name());
    bulkInternalProcess.setUpdatedBy(USER_NAME);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_DELETE_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_DELETE_BATCH_SIZE, "1",
            SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_DELETE_BATCH_SIZE));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_DELETE_DAYS_BEFORE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_DELETE_DAYS_BEFORE, "10",
            SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_DELETE_DAYS_BEFORE));
    Mockito.when(internalProcessService
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(new PageImpl(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), 2));
    Mockito.doNothing().when(internalProcessService)
        .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);

    internalProcessServiceWrapper
        .deleteOldBulkInternalProcessRequest(STORE_ID, BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_DELETE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_DELETE_DAYS_BEFORE);
    Mockito.verify(internalProcessService, Mockito.times(2))
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class));
    Mockito.verify(internalProcessService,  Mockito.times(2))
        .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);
  }

  @Test
  public void deleteOldBulkInternalProcessRequestVendorBulkAssignmentTest() throws IOException {
    bulkInternalProcess.setFileName(getDuplicateInternalBulkUploadFiles(BULK_UPDATE_FOLDER, VENDOR_BULK_ALL_SUCCESS));
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    bulkInternalProcess.setUpdatedBy(USER_NAME);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_DELETE_BATCH_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_DELETE_BATCH_SIZE, "1",
            SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_DELETE_BATCH_SIZE));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_DELETE_DAYS_BEFORE))
        .thenReturn(
            new SystemParameterConfig(SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_DELETE_DAYS_BEFORE, "10",
                SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_DELETE_DAYS_BEFORE));
    Mockito.when(internalProcessService
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(new PageImpl(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), 2));
    Mockito.doNothing().when(internalProcessService)
        .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);
    internalProcessServiceWrapper
        .deleteOldBulkInternalProcessRequest(STORE_ID, BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_DELETE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_DELETE_DAYS_BEFORE);
    Mockito.verify(internalProcessService, Mockito.times(2))
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class));
    Mockito.verify(internalProcessService, Mockito.times(2))
        .deleteBulkInternalProcessDataByInternalRequestId(INTERNAL_PROCESS_REQUEST_ID);
  }

  @Test
  public void deleteOldBulkInternalProcessRequestSuspendTest() throws IOException {
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.SUSPENSION_DELETE_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.SUSPENSION_DELETE_BATCH_SIZE, "1",
            SystemParameterConfigNames.SUSPENSION_DELETE_BATCH_SIZE));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.SUSPENSION_DELETE_DAYS_BEFORE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.SUSPENSION_DELETE_DAYS_BEFORE, "10",
            SystemParameterConfigNames.SUSPENSION_DELETE_DAYS_BEFORE));
    Mockito.when(internalProcessService
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(new PageImpl(Arrays.asList(), PageRequest.of(0, 1), 2));
    internalProcessServiceWrapper.deleteOldBulkInternalProcessRequest(STORE_ID, BulkInternalProcessType.SUSPEND.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.SUSPENSION_DELETE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.SUSPENSION_DELETE_DAYS_BEFORE);
    Mockito.verify(internalProcessService, Mockito.times(2))
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class));
  }

  @Test
  public void deleteOldBulkInternalProcessRequestConfigUpdateTest() throws IOException {
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.CONFIG_UPDATE_DELETE_BATCH_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.CONFIG_UPDATE_DELETE_BATCH_SIZE, "1",
            SystemParameterConfigNames.CONFIG_UPDATE_DELETE_BATCH_SIZE));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.CONFIG_UPDATE_DELETE_DAYS_BEFORE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.CONFIG_UPDATE_DELETE_DAYS_BEFORE, "10",
            SystemParameterConfigNames.CONFIG_UPDATE_DELETE_DAYS_BEFORE));
    Mockito.when(internalProcessService
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(new PageImpl(Arrays.asList(), PageRequest.of(0, 1), 2));
    internalProcessServiceWrapper
        .deleteOldBulkInternalProcessRequest(STORE_ID, BulkInternalProcessType.CONFIGURATION.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.CONFIG_UPDATE_DELETE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.CONFIG_UPDATE_DELETE_DAYS_BEFORE);
    Mockito.verify(internalProcessService, Mockito.times(2))
        .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(Date.class), Mockito.anyList(), Mockito.any(Pageable.class));
  }

  @Test
  @Disabled
  public void deleteOldBulkInternalProcessRequestDefaultTest() throws IOException {
    internalProcessServiceWrapper
          .deleteOldBulkInternalProcessRequest(STORE_ID, "");
  }

  @Test
  public void bulkInternalProcessSummaryTest() throws Exception {
    bulkInternalProcess.setTotalCount(TOTAL_COUNT);
    Mockito.when(internalProcessService.bulkInternalProcessSummary(STORE_ID, new BulkInternalProcessSummaryRequest(), PAGE, SIZE))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess)));
    internalProcessServiceWrapper.bulkInternalProcessSummary(STORE_ID, new BulkInternalProcessSummaryRequest(), PAGE, SIZE);
    Mockito.verify(internalProcessService).bulkInternalProcessSummary(STORE_ID, new BulkInternalProcessSummaryRequest(), PAGE, SIZE);
  }

  @Test
  public void bulkInternalProcessCancelRequestTest() throws Exception {
    internalProcessServiceWrapper.bulkInternalProcessCancelRequest(STORE_ID, USER_NAME, INTERNAL_PROCESS_REQUEST_CODE);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessCancelRequest(STORE_ID, USER_NAME, INTERNAL_PROCESS_REQUEST_CODE);
  }

  @Test
  public void bulkInternalProcessUploadTest() throws IOException {
    setMdcParameters();
    BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest = new BulkInternalProcessUploadRequest();
    bulkInternalProcessUploadRequest.setProcessType(BulkInternalProcessType.STORE_COPY.name());
    bulkInternalProcessUploadRequest.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessUploadRequest.setFileName(FILE_NAME);
    BulkInternalUploadRequestDTO bulkUploadRequest =
      BulkInternalUploadRequestDTO.builder().fileName(FILE_NAME)
        .bulkInternalProcessType(BulkInternalProcessType.STORE_COPY)
        .internalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE)
        .createdBy(USERNAME).build();
    Mockito.when(fileStorageService.isFileExists(bulkUploadRequest)).thenReturn(Boolean.TRUE);
    mockFile(ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS + INTERNAL_PROCESS_REQUEST_CODE + Constant.SLASH + FILE_NAME);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any(BulkInternalProcess.class))).thenReturn(any());
    internalProcessServiceWrapper.uploadBulkInternalProcess(STORE_ID, bulkInternalProcessUploadRequest);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Mockito.verify(fileStorageService).isFileExists(bulkUploadRequest);
    Mockito.verify(mailDeliveryService)
        .sendEmailByForInternalBulkProcess(bulkInternalProcessArgumentCaptor.capture(), eq(ProcessStatus.PENDING.name()));
  }

  @Test
  public void bulkInternalProcessUploadSalesCategoryTest() throws IOException {
    setMdcParameters();
    BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest = new BulkInternalProcessUploadRequest();
    bulkInternalProcessUploadRequest.setProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    bulkInternalProcessUploadRequest.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessUploadRequest.setFileName(FILE_NAME);
    mockFile(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR + INTERNAL_PROCESS_REQUEST_CODE + Constant.SLASH + FILE_NAME);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any(BulkInternalProcess.class))).thenReturn(any());
    BulkInternalUploadRequestDTO bulkUploadRequest =
      BulkInternalUploadRequestDTO.builder().fileName(FILE_NAME)
      .bulkInternalProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE)
      .internalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE)
      .createdBy(USERNAME).build();
    Mockito.when(fileStorageService.isFileExists(bulkUploadRequest)).thenReturn(Boolean.TRUE);
    internalProcessServiceWrapper.uploadBulkInternalProcess(STORE_ID, bulkInternalProcessUploadRequest);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Mockito.verify(mailDeliveryService)
        .sendEmailByForInternalBulkProcess(bulkInternalProcessArgumentCaptor.capture(), eq(ProcessStatus.PENDING.name()));
    Mockito.verify(fileStorageService).isFileExists(bulkUploadRequest);
  }

  @Test
  public void bulkInternalProcessDeleteBrandAuthTest() throws IOException {
    setMdcParameters();
    BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest = new BulkInternalProcessUploadRequest();
    bulkInternalProcessUploadRequest.setProcessType(BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name());
    bulkInternalProcessUploadRequest.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessUploadRequest.setFileName(FILE_NAME);
    BulkInternalUploadRequestDTO bulkUploadRequest =
      BulkInternalUploadRequestDTO.builder().fileName(FILE_NAME)
        .bulkInternalProcessType(BulkInternalProcessType.DELETE_BRAND_AUTHORISATION)
        .internalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE)
        .createdBy(USERNAME).build();
    Mockito.when(fileStorageService.isFileExists(bulkUploadRequest)).thenReturn(Boolean.TRUE);
    mockFile(ProcessorUtils.DELETE_BRAND_AUTHORISATION_DIR_UPLOADS + INTERNAL_PROCESS_REQUEST_CODE + Constant.SLASH + FILE_NAME);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any(BulkInternalProcess.class))).thenReturn(any());
    internalProcessServiceWrapper.uploadBulkInternalProcess(STORE_ID, bulkInternalProcessUploadRequest);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Mockito.verify(fileStorageService).isFileExists(bulkUploadRequest);
    Mockito.verify(mailDeliveryService)
        .sendEmailByForInternalBulkProcess(bulkInternalProcessArgumentCaptor.capture(), eq(ProcessStatus.PENDING.name()));
  }

  @Test
  @Disabled
  public void bulkInternalProcessUploadExceptionTest() throws IOException {
    BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest = new BulkInternalProcessUploadRequest();
    bulkInternalProcessUploadRequest.setProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    bulkInternalProcessUploadRequest.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessUploadRequest.setFileName(FILE_NAME);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any(BulkInternalProcess.class)))
        .thenReturn(new BulkInternalProcess());
    internalProcessServiceWrapper.uploadBulkInternalProcess(STORE_ID, bulkInternalProcessUploadRequest);
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @Test
  public void abortPendingBulkInternalProcessBeforeTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_ABORT_PENDING_TASK_IN_MINUTES)).thenReturn(systemParameterConfig);
    internalProcessServiceWrapper .abortPendingBulkInternalProcessBefore(STORE_ID, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService).abortPendingBulkInternalProcess(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_ABORT_PENDING_TASK_IN_MINUTES);
  }

  @Test
  public void abortPendingBulkInternalProcessBeforeRestrictedKeywordUpsertTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_ABORT_BEFORE)).thenReturn(systemParameterConfig);
    internalProcessServiceWrapper .abortPendingBulkInternalProcessBefore(STORE_ID, BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
    Mockito.verify(internalProcessService).abortPendingBulkInternalProcess(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_ABORT_BEFORE);
  }

  @Test
  public void abortPendingBulkInternalProcessBeforeRestrictedKeywordDeleteTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_ABORT_BEFORE)).thenReturn(systemParameterConfig);
    internalProcessServiceWrapper .abortPendingBulkInternalProcessBefore(STORE_ID, BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());
    Mockito.verify(internalProcessService).abortPendingBulkInternalProcess(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_ABORT_BEFORE);
  }

  @Test
  public void abortPendingBulkInternalProcessBeforeUpdateSalesCategoryTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_SALES_CATEGORY_ABORT_PENDING_TASK_IN_MINUTES))
        .thenReturn(systemParameterConfig);
    internalProcessServiceWrapper
        .abortPendingBulkInternalProcessBefore(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    Mockito.verify(internalProcessService).abortPendingBulkInternalProcess(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_SALES_CATEGORY_ABORT_PENDING_TASK_IN_MINUTES);
  }

  @Test
  public void abortPendingBulkInternalProcessBeforeSuspend() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.SUSPENSION_ABORT_PENDING_TASK_IN_MINUTES))
        .thenReturn(systemParameterConfig);
    internalProcessServiceWrapper
        .abortPendingBulkInternalProcessBefore(STORE_ID, BulkInternalProcessType.SUSPEND.name());
    Mockito.verify(internalProcessService).abortPendingBulkInternalProcess(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.SUSPENSION_ABORT_PENDING_TASK_IN_MINUTES);
  }

  @Test
  public void abortPendingBulkInternalProcessBeforeConfigUpdate() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.CONFIG_UPDATE_ABORT_PENDING_TASK_IN_MINUTES)).thenReturn(systemParameterConfig);
    internalProcessServiceWrapper
        .abortPendingBulkInternalProcessBefore(STORE_ID, BulkInternalProcessType.CONFIGURATION.name());
    Mockito.verify(internalProcessService).abortPendingBulkInternalProcess(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.CONFIG_UPDATE_ABORT_PENDING_TASK_IN_MINUTES);
  }

  @Test
  public void abortPendingBulkInternalProcessBeforeInternalBulkProcessTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_ABORT_PENDING_TASK_IN_MINUTES))
        .thenReturn(systemParameterConfig);
    internalProcessServiceWrapper
        .abortPendingBulkInternalProcessBefore(STORE_ID, BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name());
    Mockito.verify(internalProcessService).abortPendingBulkInternalProcess(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_ABORT_PENDING_TASK_IN_MINUTES);
  }

  @Test
  public void abortPendingBulkInternalProcessBefore_forAutoAssignment() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_ABORT_PENDING_TASK_IN_MINUTES))
      .thenReturn(systemParameterConfig);
    internalProcessServiceWrapper
      .abortPendingBulkInternalProcessBefore(STORE_ID, BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(internalProcessService).abortPendingBulkInternalProcess(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_ABORT_PENDING_TASK_IN_MINUTES);
  }


  @Test
  public void abortPendingBulkInternalProcessBeforeDeafultTest() {
    internalProcessServiceWrapper .abortPendingBulkInternalProcessBefore(STORE_ID, "");
    Mockito.verify(internalProcessService).abortPendingBulkInternalProcess(Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void failPendingBulkInternalProcessDataBeforeTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_FAIL_PENDING_TASK_IN_MINUTES)).thenReturn(systemParameterConfig);
    internalProcessServiceWrapper.failPendingBulkInternalProcessDataBefore(STORE_ID, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService).failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_FAIL_PENDING_TASK_IN_MINUTES);
  }

  @Test
  public void failPendingBulkInternalProcessDataBeforeForAutoAssignmentTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FAIL_PENDING_TASK_IN_MINUTES))
        .thenReturn(systemParameterConfig);
    internalProcessServiceWrapper.failPendingBulkInternalProcessDataBefore(STORE_ID, BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(internalProcessService).failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FAIL_PENDING_TASK_IN_MINUTES);

  }

  @Test
  public void failPendingBulkInternalProcessDataRestrictedKeywordUpsertBeforeTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_FAIL_BEFORE)).thenReturn(systemParameterConfig);
    internalProcessServiceWrapper.failPendingBulkInternalProcessDataBefore(STORE_ID, BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
    Mockito.verify(internalProcessService).failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_FAIL_BEFORE);
  }

  @Test
  public void failPendingBulkInternalProcessDataRestrictedKeywordDeleteBeforeTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_FAIL_BEFORE)).thenReturn(systemParameterConfig);
    internalProcessServiceWrapper.failPendingBulkInternalProcessDataBefore(STORE_ID, BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());
    Mockito.verify(internalProcessService).failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_FAIL_BEFORE);
  }

  @Test
  public void failPendingBulkInternalProcessDataBeforepdateSalesCategoryTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FAIL_PENDING_TASK_IN_MINUTES))
        .thenReturn(systemParameterConfig);
    internalProcessServiceWrapper
        .failPendingBulkInternalProcessDataBefore(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    Mockito.verify(internalProcessService).failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FAIL_PENDING_TASK_IN_MINUTES);
  }

  @Test
  public void failPendingBulkInternalProcessDataBeforSuspend() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.SUSPENSION_FAIL_PENDING_TASK_IN_MINUTES))
        .thenReturn(systemParameterConfig);
    internalProcessServiceWrapper
        .failPendingBulkInternalProcessDataBefore(STORE_ID, BulkInternalProcessType.SUSPEND.name());
    Mockito.verify(internalProcessService)
        .failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.SUSPENSION_FAIL_PENDING_TASK_IN_MINUTES);
  }

  @Test
  public void failPendingBulkInternalProcessDataBeforeConfig() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.CONFIG_UPDATE_FAIL_PENDING_TASK_IN_MINUTES))
        .thenReturn(systemParameterConfig);
    internalProcessServiceWrapper
        .failPendingBulkInternalProcessDataBefore(STORE_ID, BulkInternalProcessType.CONFIGURATION.name());
    Mockito.verify(internalProcessService)
        .failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.CONFIG_UPDATE_FAIL_PENDING_TASK_IN_MINUTES);
  }

  @Test
  public void failPendingBulkInternalProcessDataBeforepdateInternalBulkUploadTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_FAIL_PENDING_TASK_IN_MINUTES))
        .thenReturn(systemParameterConfig);
    internalProcessServiceWrapper
        .failPendingBulkInternalProcessDataBefore(STORE_ID, BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name());
    Mockito.verify(internalProcessService).failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_FAIL_PENDING_TASK_IN_MINUTES);
  }

  @Test
  public void failPendingBulkInternalProcessDataBeforeDefaultTest() {
    internalProcessServiceWrapper.failPendingBulkInternalProcessDataBefore(STORE_ID, "");
    Mockito.verify(internalProcessService).failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void processStatusUpdateNoProductDataPendingTest() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
  }

  @Test
  @Disabled
  public void processStatusUpdateNoProductDataPendingTest_None() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME, "ELSE");
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
  }

  @Test
  public void processStatusUpdateErrorTest() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId()))
        .thenThrow(ApplicationRuntimeException.class);
    internalProcessServiceWrapper
        .processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
  }

  @Test
  public void processStatusUpdateEmptyResultTest() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(new ArrayList<>(), pageable, TOTAL_COUNT));
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name());
  }

  @Test
  public void processStatusUpdatePendingProductsTest() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId())).thenReturn(Arrays.asList(bulkInternalProcessData));
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
  }

  @Test
  public void processStatusUpdateInProgressProductsTest() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId())).thenReturn(Arrays.asList(bulkInternalProcessData));
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
  }

  @Test
  public void processStatusUpdateSuccessProductsTest() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId())).thenReturn(Arrays.asList(bulkInternalProcessData));
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(mailDeliveryService)
        .sendEmailByForInternalBulkProcess(bulkInternalProcessArgumentCaptor.capture(), eq(ProcessStatus.COMPLETED.name()));
  }

  @Test
  public void processStatusUpdateFailedProductsTest() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setTotalCount(10);
    bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId())).thenReturn(Arrays.asList(bulkInternalProcessData));
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(mailDeliveryService)
        .sendEmailByForInternalBulkProcess(bulkInternalProcessArgumentCaptor.capture(), eq(ProcessStatus.FAILED.name()));
  }

  @Test
  public void processStatusUpdatePartialCompletedProductsTest() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setTotalCount(10);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    BulkInternalProcessData bulkInternalProcessData1= new BulkInternalProcessData();
    bulkInternalProcessData1.setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessDataList.add(bulkInternalProcessData1);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.STORE_COPY.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId())).thenReturn(bulkInternalProcessDataList);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.STORE_COPY.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(mailDeliveryService)
        .sendEmailByForInternalBulkProcess(bulkInternalProcessArgumentCaptor.capture(), eq(ProcessStatus.PARTIAL_COMPLETED.name()));
  }

  @Test
  public void processStatusUpdateDalesCategoryTest() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.UPDATE_SALES_CATEGORY_STATUS_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.SALES_CATEGORY_UPDATE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    internalProcessServiceWrapper
        .processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.UPDATE_SALES_CATEGORY_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(mailDeliveryService)
        .sendEmailByForInternalBulkProcess(bulkInternalProcessArgumentCaptor.capture(), eq(ProcessStatus.COMPLETED.name()));
  }

  @Test
  public void processStatusUpdateDalesInternalBulkUpdateTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name());
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_STATUS_UPDATE_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    internalProcessServiceWrapper
        .processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name());
    Mockito.verify(masterDataBulkUpdateService)
        .setFinalStatusAndGenerateFailedExcel(bulkInternalProcess, STORE_ID);
  }

  @Test
  public void processStatusDeleteBrandAuthorisationTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(DELETE_BRAND_AUTHORISATION);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name());
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DELETE_BRAND_AUTH_STATUS_UPDATE_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    internalProcessServiceWrapper
        .processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DELETE_BRAND_AUTH_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name());
    Mockito.verify(brandAuthorisationService)
        .setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
  }

  @Test
  public void processStatusUpdateVendorBulkAssignmentTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_STATUS_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    internalProcessServiceWrapper
        .processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    Mockito.verify(vendorProductBulkAssignService).setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
  }

  @Test
  public void processStatusUpdateVendorAutoAssignmentTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_STATUS_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
            .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    internalProcessServiceWrapper
        .processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(vendorProductBulkAssignService).setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
  }

  @Test
  @Disabled
  public void processEventTest() throws Exception {
    setMdcParameters();
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(
            internalProcessService.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
                Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.findByInternalProcessRequestCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new BulkInternalProcess());
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any(GetProductInfoRequestV2.class))).thenReturn(
        new GdnRestListResponse<ProductAndItemInfoResponseV2>(null, null, true,
            Arrays.asList(productAndItemInfoResponseV2), new PageMetaData(0L, 0L, 0L), REQUEST_ID));
    Mockito.when(pcbOutboundService.getProductDetailByProductCode(PRODUCT_CODE, false, false))
        .thenReturn(productDetailResponse);
    Mockito.when(
            categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(categoryDetailResponse);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    Mockito.when(attributeRepository.findOne(Mockito.any(), Mockito.any())).thenReturn(attributeResponse);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setStoreId(STORE_ID);
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setBrandCode(BRAND_CODE);
    Mockito.when(BulkCreationCommonUtil.toProductCreationRequestForCopyStore(Mockito.anyMap(), Mockito.any(),
            Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyMap(),
        Mockito.anyBoolean(), Mockito.anyBoolean()))
        .thenReturn(productCreationRequest);
    Mockito.when(productRepository.generateProductCode(Constant.REQUEST_ID, Constant.USER_NAME))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(productRepository.createProduct(Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(new GdnBaseRestResponse());
    Mockito.when(pcbOutboundService
      .getBrandAuthorisation(STORE_ID, GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, BUSINESS_PARTNER_CODE, BRAND_CODE))
      .thenReturn(new SimpleBooleanResponse(true));
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any());
    Mockito.verify(pcbOutboundService).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE);
    Mockito.verify(productRepository).generateProductCode(Constant.REQUEST_ID, Constant.USER_NAME);
    Mockito.verify(productRepository)
        .createProduct(INTERNAL_PROCESS_REQUEST_CODE, Constant.SYSTEM, productCreationRequest);
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
    Mockito.verify(internalProcessService).findByInternalProcessRequestCode(Mockito.anyString(), Mockito.anyString());
  }


  @Test
  @Disabled
  public void processEventTest2() throws Exception {
    setMdcParameters();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setName(Constants.BRAND);
    attributeResponse.setAttributeCode(ID);
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(attributeResponse);
    productDetailResponse.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(productAttributeRepository.getAttributeDetailByAttributeCodes(Mockito.eq(Constants.DEFAULT_REQUEST_ID),
            Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.anyList(), Mockito.eq(true)))
        .thenReturn(Arrays.asList(attributeResponse));
    Mockito.when(
            internalProcessService.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
                Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.findByInternalProcessRequestCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new BulkInternalProcess());
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any(GetProductInfoRequestV2.class))).thenReturn(
        new GdnRestListResponse<ProductAndItemInfoResponseV2>(null, null, true,
            Arrays.asList(productAndItemInfoResponseV2), new PageMetaData(0L, 0L, 0L), REQUEST_ID));
    Mockito.when(pcbOutboundService.getProductDetailByProductCode(PRODUCT_CODE, false, false))
        .thenReturn(productDetailResponse);
    Mockito.when(
            categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(categoryDetailResponse);
    Mockito.when(attributeRepository.findOne(Mockito.any(), Mockito.any())).thenReturn(attributeResponse);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setStoreId(STORE_ID);
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setBrandCode(BRAND_CODE);
    Mockito.when(BulkCreationCommonUtil.toProductCreationRequestForCopyStore(Mockito.anyMap(), Mockito.any(),
            Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyMap(),
            Mockito.anyBoolean(), Mockito.anyBoolean()))
        .thenReturn(productCreationRequest);
    try (MockedStatic<BulkCreationCommonUtil> mockedStatic = Mockito.mockStatic(BulkCreationCommonUtil.class)) {
      mockedStatic.when(() ->
          BulkCreationCommonUtil.generateProductBusinessPartnerRequest(
              Mockito.anyList(),
              Mockito.any(),
              Mockito.any()
          )
      ).thenAnswer(invocation -> null);
    }

    Mockito.when(productRepository.generateProductCode(Constant.REQUEST_ID, Constant.USER_NAME))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(productRepository.createProduct(Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(new GdnBaseRestResponse());
    Mockito.when(pcbOutboundService
            .getBrandAuthorisation(STORE_ID, GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
                GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
                GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
                GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, BUSINESS_PARTNER_CODE, BRAND_CODE))
        .thenReturn(new SimpleBooleanResponse(true));
      try (MockedStatic<BulkCreationCommonUtil> mockedStatic2 = Mockito.mockStatic(BulkCreationCommonUtil.class)) {
        mockedStatic2.when(() ->
            BulkCreationCommonUtil.getAttributeIdAndValuesMap(
                Mockito.anyList(),
                Mockito.anyMap()
            )
        ).thenReturn(Map.of(ID, ATTRIBUTE_VALUE));
      }

      Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID,  ID,
            ATTRIBUTE_VALUE)).thenReturn(attributeResponse);
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any());
    Mockito.verify(pcbOutboundService).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE);
    Mockito.verify(productRepository).generateProductCode(Constant.REQUEST_ID, Constant.USER_NAME);
    Mockito.verify(productRepository)
        .createProduct(INTERNAL_PROCESS_REQUEST_CODE, Constant.SYSTEM, productCreationRequest);
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
    Mockito.verify(internalProcessService).findByInternalProcessRequestCode(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void processEventEmptyRowsTest() throws Exception {
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(new ArrayList<>());
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE, PROCESS_TYPE_STORE_COPY,
            INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
  }

  @Test
  public void processEventExceptionTest() throws Exception {
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(
            internalProcessService.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
                Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any())).thenThrow(ApplicationRuntimeException.class);
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(internalProcessService, times(2))
        .saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
  }

  @Test
  public void processEventNullProductAndItemResponseTest() throws Exception {
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any())).thenReturn(
        new GdnRestListResponse<ProductAndItemInfoResponseV2>(null, null, true,
            Arrays.asList(productAndItemInfoResponseV2), new PageMetaData(0L, 0L, 0L), REQUEST_ID));
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
  }

  @Test
  public void processEventNullProductAndItemResponseSuspendedTrueTest() throws Exception {
    productAndItemInfoResponseV2.getProduct().setSuspended(true);
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any())).thenReturn(
        new GdnRestListResponse<ProductAndItemInfoResponseV2>(null, null, true,
            Arrays.asList(productAndItemInfoResponseV2), new PageMetaData(0L, 0L, 0L), REQUEST_ID));
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
  }

  @Test
  public void processEventNullProductAndItemResponseForceReviewTrueTest() throws Exception {
    productAndItemInfoResponseV2.getProduct().setForceReview(true);
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any())).thenReturn(
        new GdnRestListResponse<ProductAndItemInfoResponseV2>(null, null, true,
            Arrays.asList(productAndItemInfoResponseV2), new PageMetaData(0L, 0L, 0L), REQUEST_ID));
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
  }

  @Test
  public void processEventNullProductAndItemResponsemfdTrueTest() throws Exception {
    productAndItemInfoResponseV2.getProduct().setMarkForDelete(true);
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any())).thenReturn(
        new GdnRestListResponse<ProductAndItemInfoResponseV2>(null, null, true,
            Arrays.asList(productAndItemInfoResponseV2), new PageMetaData(0L, 0L, 0L), REQUEST_ID));
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
  }

  @Test
  public void processEventNullProductAndItemResponseArchivedTrueTest() throws Exception {
    productAndItemInfoResponseV2.getItem().setArchived(true);
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any())).thenReturn(
        new GdnRestListResponse<ProductAndItemInfoResponseV2>(null, null, true,
            Arrays.asList(productAndItemInfoResponseV2), new PageMetaData(0L, 0L, 0L), REQUEST_ID));
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
  }

  @Test
  public void processEventIsSuspendedTest() throws Exception {
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    productAndItemsResponse.getProduct().setSuspended(true);
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any())).thenReturn(
        new GdnRestListResponse<ProductAndItemInfoResponseV2>(null, null, true,
            Arrays.asList(productAndItemInfoResponseV2), new PageMetaData(0L, 0L, 0L), REQUEST_ID));
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
  }

  @Test
  public void processEventIsArchivedTest() throws Exception {
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    productAndItemsResponse.getItems().get(0).setArchived(true);
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any())).thenReturn(
        new GdnRestListResponse<ProductAndItemInfoResponseV2>(null, null, true,
            Arrays.asList(productAndItemInfoResponseV2), new PageMetaData(0L, 0L, 0L), REQUEST_ID));
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(internalProcessService, times(2))
        .saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
  }

  @Test
  public void processEventIsForceReviewTest() throws Exception {
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    productAndItemsResponse.getProduct().setForceReview(true);
    Mockito.when(xProductOutboundService.getProductAndItemsWithProductData(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
  }

  @Test
  public void processEventIsMarkForDeleteTest() throws Exception {
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    productAndItemsResponse.getProduct().setMarkForDelete(true);
    Mockito.when(xProductOutboundService.getProductAndItemsWithProductData(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
  }

  @Test
  @Disabled
  public void processEventSpecialAttributesTest() throws Exception {
    setMdcParameters();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setProductSpecialAttributes(Arrays.asList(new ProductSpecialAttributeDTO()));
    productAndItemsResponse.setProduct(productResponse);
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any())).thenReturn(
        new GdnRestListResponse<ProductAndItemInfoResponseV2>(null, null, true,
            Arrays.asList(productAndItemInfoResponseV2), new PageMetaData(0L, 0L, 0L), REQUEST_ID));
    Mockito.when(pcbOutboundService.getProductDetailByProductCode(PRODUCT_CODE, false, false))
        .thenReturn(productDetailResponse);
    Mockito.when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(categoryDetailResponse);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    Mockito.when(attributeRepository.findOne(Mockito.any(), Mockito.any())).thenReturn(attributeResponse);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setStoreId(STORE_ID);
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setBrandCode(BRAND_CODE);
    try (MockedStatic<BulkCreationCommonUtil> mockedStatic = Mockito.mockStatic(
        BulkCreationCommonUtil.class)) {
      mockedStatic.when(
              () -> BulkCreationCommonUtil.toProductCreationRequestForCopyStore(Mockito.anyMap(),
                  Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
                  Mockito.anyMap(), Mockito.anyBoolean(), Mockito.anyBoolean()))
          .thenReturn(productCreationRequest);
    }

    try (MockedStatic<BulkCreationCommonUtil> mockedStatic = Mockito.mockStatic(
        BulkCreationCommonUtil.class)) {
      mockedStatic.when(
          () -> BulkCreationCommonUtil.generateProductBusinessPartnerRequest(Mockito.anyList(),
              Mockito.any(), Mockito.any())).thenAnswer(invocation -> null);
    }

    Mockito.when(pcbOutboundService
      .getBrandAuthorisation(STORE_ID, GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, BUSINESS_PARTNER_CODE, BRAND_CODE))
      .thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.generateProductCode(Constant.REQUEST_ID, Constant.USER_NAME))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(productRepository.createProduct(Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(new GdnBaseRestResponse());
    Mockito.when(internalProcessService.findByInternalProcessRequestCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new BulkInternalProcess());
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
    Mockito.verify(pcbOutboundService).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE);
    Mockito.verify(productRepository).generateProductCode(Constant.REQUEST_ID, Constant.USER_NAME);
    Mockito.verify(productRepository)
        .createProduct(INTERNAL_PROCESS_REQUEST_CODE, Constant.SYSTEM, productCreationRequest);
    Mockito.verify(internalProcessService).findByInternalProcessRequestCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
  }

  @Test
  @Disabled
  public void processEventwithNoSpecialAttributesTest() throws Exception {
    setMdcParameters();
    ProductSpecialAttributeDTO productSpecialAttributeDTO = new ProductSpecialAttributeDTO();
    productSpecialAttributeDTO.setAttributeCode(ATTRIBUTE_CODE);
    productSpecialAttributeDTO.setAttributeName(ATTRIBUTE_NAME);
    productSpecialAttributeDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAndItemInfoResponseV2.getProduct().setProductSpecialAttributes(Arrays.asList(productSpecialAttributeDTO));
    ProductResponse productResponse = new ProductResponse();
    productAndItemsResponse.setProduct(productResponse);
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any())).thenReturn(
        new GdnRestListResponse<ProductAndItemInfoResponseV2>(null, null, true,
            Arrays.asList(productAndItemInfoResponseV2), new PageMetaData(0L, 0L, 0L), REQUEST_ID));
    Mockito.when(pcbOutboundService.getProductDetailByProductCode(PRODUCT_CODE, false, false))
        .thenReturn(productDetailResponse);
    Mockito.when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(categoryDetailResponse);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    Mockito.when(attributeRepository.findOne(Mockito.any(), Mockito.any())).thenReturn(attributeResponse);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setStoreId(STORE_ID);
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setBrandCode(BRAND_CODE);
    try (MockedStatic<BulkCreationCommonUtil> mockedStatic = Mockito.mockStatic(
        BulkCreationCommonUtil.class)) {
      mockedStatic.when(
              () -> BulkCreationCommonUtil.toProductCreationRequestForCopyStore(Mockito.anyMap(),
                  Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
                  Mockito.anyMap(), Mockito.anyBoolean(), Mockito.anyBoolean()))
          .thenReturn(productCreationRequest);
      mockedStatic.when(
          () -> BulkCreationCommonUtil.generateProductBusinessPartnerRequest(Mockito.anyList(),
              Mockito.any(), Mockito.any())).thenAnswer(invocation -> null);
    }

    Mockito.when(pcbOutboundService
        .getBrandAuthorisation(STORE_ID, GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, BUSINESS_PARTNER_CODE, BRAND_CODE))
        .thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.generateProductCode(Constant.REQUEST_ID, Constant.USER_NAME))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(productRepository.createProduct(Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(new GdnBaseRestResponse());
    Mockito.when(internalProcessService.findByInternalProcessRequestCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new BulkInternalProcess());
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
    Mockito.verify(pcbOutboundService).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE);
    Mockito.verify(productRepository).generateProductCode(Constant.REQUEST_ID, Constant.USER_NAME);
    Mockito.verify(productRepository)
        .createProduct(INTERNAL_PROCESS_REQUEST_CODE, Constant.SYSTEM, productCreationRequest);
    Mockito.verify(internalProcessService).findByInternalProcessRequestCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
  }

  @Test
  @Disabled
  public void processEventFailedCopyExceptionTest() throws Exception {
    setMdcParameters();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setProductSpecialAttributes(Arrays.asList(new ProductSpecialAttributeDTO()));
    productAndItemsResponse.setProduct(productResponse);
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any())).thenReturn(
        new GdnRestListResponse<ProductAndItemInfoResponseV2>(null, null, true,
            Arrays.asList(productAndItemInfoResponseV2), new PageMetaData(0L, 0L, 0L), REQUEST_ID));
    Mockito.when(pcbOutboundService.getProductDetailByProductCode(PRODUCT_CODE, false, false))
        .thenReturn(productDetailResponse);
    Mockito.when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(categoryDetailResponse);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    Mockito.when(attributeRepository.findOne(Mockito.any(), Mockito.any())).thenReturn(attributeResponse);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setStoreId(STORE_ID);
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setBrandCode(BRAND_CODE);
    Mockito.when(pcbOutboundService
      .getBrandAuthorisation(STORE_ID, GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, BUSINESS_PARTNER_CODE, BRAND_CODE))
      .thenReturn(new SimpleBooleanResponse(true));
    try (MockedStatic<BulkCreationCommonUtil> mockedStatic = Mockito.mockStatic(
        BulkCreationCommonUtil.class)) {
      mockedStatic.when(
              () -> BulkCreationCommonUtil.toProductCreationRequestForCopyStore(Mockito.anyMap(),
                  Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
                  Mockito.anyMap(), Mockito.anyBoolean(), Mockito.anyBoolean()))
          .thenReturn(productCreationRequest);

      mockedStatic.when(
          () -> BulkCreationCommonUtil.generateProductBusinessPartnerRequest(Mockito.anyList(),
              Mockito.any(), Mockito.any())).thenAnswer(invocation -> null);
    }

    Mockito.when(productRepository.generateProductCode(Constant.REQUEST_ID, Constant.USER_NAME))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(productRepository.createProduct(Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenThrow(Exception.class);
    Mockito.when(internalProcessService.findByInternalProcessRequestCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new BulkInternalProcess());
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
    Mockito.verify(pcbOutboundService).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE);
    Mockito.verify(productRepository).generateProductCode(Constant.REQUEST_ID, Constant.USER_NAME);
    Mockito.verify(productRepository)
        .createProduct(INTERNAL_PROCESS_REQUEST_CODE, Constant.SYSTEM, productCreationRequest);
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
    Mockito.verify(internalProcessService).findByInternalProcessRequestCode(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  @Disabled
  public void processEventCategoryIsMarkForDeleteTest() throws Exception {
    setMdcParameters();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setProductSpecialAttributes(Arrays.asList(new ProductSpecialAttributeDTO()));
    productAndItemsResponse.setProduct(productResponse);
    categoryAttributeResponse.setMarkForDelete(true);
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
            .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any())).thenReturn(
        new GdnRestListResponse<ProductAndItemInfoResponseV2>(null, null, true,
            Arrays.asList(productAndItemInfoResponseV2), new PageMetaData(0L, 0L, 0L), REQUEST_ID));
    Mockito.when(pcbOutboundService.getProductDetailByProductCode(PRODUCT_CODE, false, false))
        .thenReturn(productDetailResponse);
    Mockito.when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(categoryDetailResponse);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setStoreId(STORE_ID);
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setBrandCode(BRAND_CODE);
    Mockito.when(pcbOutboundService
      .getBrandAuthorisation(STORE_ID, GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, BUSINESS_PARTNER_CODE, BRAND_CODE))
      .thenReturn(new SimpleBooleanResponse(true));
    try (MockedStatic<BulkCreationCommonUtil> utilities = Mockito.mockStatic(
        BulkCreationCommonUtil.class)) {
      utilities.when(
              () -> BulkCreationCommonUtil.toProductCreationRequestForCopyStore(Mockito.anyMap(),
                  Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
                  Mockito.anyMap(), Mockito.anyBoolean(), Mockito.anyBoolean()))
          .thenReturn(productCreationRequest);

      utilities.when(
          () -> BulkCreationCommonUtil.generateProductBusinessPartnerRequest(Mockito.anyList(),
              Mockito.any(), Mockito.any())).thenAnswer(invocation -> null);

    }

    Mockito.when(productRepository.generateProductCode(Constant.REQUEST_ID, Constant.USER_NAME))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(productRepository.createProduct(Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenThrow(Exception.class);
    Mockito.when(internalProcessService.findByInternalProcessRequestCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new BulkInternalProcess());
    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
            PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(xProductOutboundService).getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
    Mockito.verify(pcbOutboundService).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE);
    Mockito.verify(productRepository).generateProductCode(Constant.REQUEST_ID, Constant.USER_NAME);
    Mockito.verify(productRepository)
        .createProduct(INTERNAL_PROCESS_REQUEST_CODE, Constant.SYSTEM, productCreationRequest);
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));
    Mockito.verify(internalProcessService).findByInternalProcessRequestCode(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void uploadInternalBulkUploadToBulkInternalProcessTest(){
    internalProcessServiceWrapper.uploadInternalBulkUploadToBulkInternalProcess(STORE_ID, new MasterDataBulkUpdateRequest());
    Mockito.verify(internalProcessService).saveInternalProcess(any(BulkInternalProcess.class));
  }

  @Test
  public void uploadVendorBulkAssignmentProcessTest() {
    bulkVendorProductAssignRequest.setVendorCode(VENDOR_CODE);
    internalProcessServiceWrapper.uploadVendorBulkAssignmentProcess(STORE_ID, bulkVendorProductAssignRequest);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(VENDOR_CODE, bulkInternalProcessArgumentCaptor.getValue().getSellerCode());
    Assertions.assertEquals(VENDOR_CODE, bulkInternalProcessArgumentCaptor.getValue().getSellerName());
    Assertions.assertEquals(VendorProductDataBulkParameters.ASSIGN,
        bulkInternalProcessArgumentCaptor.getValue().getNotes());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void uploadVendorBulkAssignmentProcessImageTest() {
    bulkVendorProductAssignRequest.setVendorCode(VENDOR_CODE);
    bulkVendorProductAssignRequest.setAssignmentType(STORE_ID);
    internalProcessServiceWrapper.uploadVendorBulkAssignmentProcess(STORE_ID, bulkVendorProductAssignRequest);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(VENDOR_CODE, bulkInternalProcessArgumentCaptor.getValue().getSellerCode());
    Assertions.assertEquals(VENDOR_CODE, bulkInternalProcessArgumentCaptor.getValue().getSellerName());
    Assertions.assertEquals(VendorProductDataBulkParameters.ASSIGN,
        bulkInternalProcessArgumentCaptor.getValue().getNotes());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processStatusUpdateNoProductData_Suspension() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SUSPEND.name());
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.SUSPENSION_FILE_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.SUSPEND.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    internalProcessServiceWrapper
        .processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.SUSPEND.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.SUSPENSION_FILE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.SUSPEND.name());
    Mockito.verify(bulkProductSuspensionService)
        .setFinalStatusAndNotificationOnSuspension(bulkInternalProcess, STORE_ID);
  }

  @Test
  public void processStatusUpdateNoProductData_ConfigUpdate() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.CONFIGURATION.name());
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.CONFIG_UPDATE_FILE_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.CONFIGURATION.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    internalProcessServiceWrapper
        .processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME, BulkInternalProcessType.CONFIGURATION.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.CONFIG_UPDATE_FILE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.CONFIGURATION.name());
    Mockito.verify(bulkConfigurationUpdateService)
        .setFinalStatusAndNotificationOnConfigUpdate(bulkInternalProcess, STORE_ID);
  }


  @Test
  @Disabled
  public void processEventProtectedBrandExceptionTest() throws Exception {
    setMdcParameters();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setProductSpecialAttributes(Arrays.asList(new ProductSpecialAttributeDTO()));
    productAndItemsResponse.setProduct(productResponse);
    categoryAttributeResponse.setMarkForDelete(true);
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
      InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).processType(PROCESS_TYPE_STORE_COPY)
        .parentCode(PRODUCT_CODE).internalProcessRequestId(INTERNAL_PROCESS_REQUEST_CODE).build();
    Mockito.when(internalProcessService
      .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
      .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(xProductOutboundService.getProductInfoByItemSku(any())).thenReturn(
        new GdnRestListResponse<ProductAndItemInfoResponseV2>(null, null, true,
            Arrays.asList(productAndItemInfoResponseV2), new PageMetaData(0L, 0L, 0L), REQUEST_ID));
    Mockito.when(pcbOutboundService.getProductDetailByProductCode(PRODUCT_CODE, false, false))
      .thenReturn(productDetailResponse);
    Mockito.when(
      categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(categoryDetailResponse);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setStoreId(STORE_ID);
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setBrandCode(BRAND_CODE);
    Mockito.when(pcbOutboundService
      .getBrandAuthorisation(STORE_ID, GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, BUSINESS_PARTNER_CODE, BRAND_CODE))
      .thenReturn(new SimpleBooleanResponse(false));
    try (MockedStatic<BulkCreationCommonUtil> mockedStatic = Mockito.mockStatic(
        BulkCreationCommonUtil.class)) {
      mockedStatic.when(
              () -> BulkCreationCommonUtil.toProductCreationRequestForCopyStore(Mockito.anyMap(),
                  Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
                  Mockito.anyMap(), Mockito.anyBoolean(), Mockito.anyBoolean()))
          .thenReturn(productCreationRequest);

      mockedStatic.when(
          () -> BulkCreationCommonUtil.generateProductBusinessPartnerRequest(Mockito.anyList(),
              Mockito.any(), Mockito.any())).thenAnswer(invocation -> null);
    }



    internalProcessServiceWrapper.processEvent(internalProcessDataDomainEventModel);
    Mockito.verify(internalProcessService)
      .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(PRODUCT_CODE,
        PROCESS_TYPE_STORE_COPY, INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_IN_PROGRESS);
    Mockito.verify(xProductOutboundService)
        .getProductInfoByItemSku(any(GetProductInfoRequestV2.class));
    Mockito.verify(pcbOutboundService).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE);

    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(Arrays.asList(bulkInternalProcessData));

  }

  @Test
  public void processNewFbbL5CreateRequestTest() {
    bulkInternalProcess.setProcessType(BulkInternalProcessType.FBB_L5_CREATE.name());
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.FETCH_PENDING_FBB_L5_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
        BulkInternalProcessType.FBB_L5_CREATE.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
      .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID,
        INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_PENDING))
      .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
      BulkInternalProcessType.FBB_L5_CREATE.name());
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
        BulkInternalProcessType.FBB_L5_CREATE.name());
    Mockito.verify(internalProcessService)
      .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.FETCH_PENDING_FBB_L5_SIZE);
    Mockito.verify(internalProcessService)
      .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID,
        INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_PENDING);
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
  }

  @Test
  public void processFbbL5CreateTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
      BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
        .processType(bulkInternalProcess.getProcessType()).build()));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.FETCH_PENDING_FBB_L4_ROW_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.FETCH_PENDING_FBB_L5_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.L4_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
      internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
          BulkInternalProcessType.FBB_L5_CREATE.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));

    Mockito.when(internalProcessService
      .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(1);
    internalProcessServiceWrapper
      .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.FBB_L5_CREATE.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.FETCH_PENDING_FBB_L4_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.FETCH_PENDING_FBB_L5_SIZE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.L4_FETCH_BATCH_SIZE);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
        BulkInternalProcessType.FBB_L5_CREATE.name());
    Mockito.verify(internalProcessService)
      .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void processNotFbbL5CreateTest() {
    pageable = PageRequest.of(0, 1);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    Mockito.when(
            internalProcessService
                .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                    BulkProcessType.PRODUCT_LEVEL_3.getValue()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));

    Mockito.when(internalProcessService
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(1);
    internalProcessServiceWrapper
        .processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
            BulkProcessType.PRODUCT_LEVEL_3.getValue());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkProcessType.PRODUCT_LEVEL_3.getValue());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
  }

  @Test
  public void processStatusUpdateForFbbTest() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.FBB_L4_RESULT_UPDATE_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
        BulkInternalProcessType.FBB_L5_CREATE.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
      BulkInternalProcessType.FBB_L5_CREATE.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.FBB_L4_RESULT_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
        BulkInternalProcessType.FBB_L5_CREATE.name());
  }

  @Test
  public void processStatusUpdateForBulkPriceUpdateTest() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_FINAL_UPDATE_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.BULK_PRICE_UPDATE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BULK_PRICE_UPDATE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_FINAL_UPDATE_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_UPDATE.name());
  }

  @Test
  public void processVendorAutoAssignmentsTest() throws Exception {
    VendorAutoAssignmentFilterRequest autoAssignmentFilterRequest =
      VendorAutoAssignmentFilterRequest.builder().contentPending(true).imagePending(true)
        .postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    BoostedProductFilterRequest boostedProductFilterRequest =
      BoostedProductFilterRequest.builder().postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setTotalCount(3);
    VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest =
      new VendorAutoAssignmentFilterRequest();
    vendorAutoAssignmentFilterRequest.setEdited(true);
    List<String> assigneeList = Arrays.asList("apiautomation_vendor12@mailinator.com",
      "apiautomation_vendor3@mailinator.com", "apiautomationvendor4@mailinator.com");
    String assignees = String.join(", ", assigneeList);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    bulkInternalProcess.setNotes(assignees);
    bulkInternalProcess.setFileName(filterAssignmentRequest);
    Mockito.when(objectMapper.readValue(bulkInternalProcess.getFileName(),
        VendorAutoAssignmentFilterRequest.class)).thenReturn(autoAssignmentFilterRequest);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(productDistributionTaskRepository.fetchProductsForAutoAssignment(anyString(),
        anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class))).thenReturn(productCodeResponse);
    Mockito.when(internalProcessService.saveInternalProcess(any())).thenReturn(bulkInternalProcess);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatusOrderByDateAsc(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID,
          INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_PENDING))
      .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
      BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(productDistributionTaskRepository, times(3)).fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class));
    Mockito.verify(internalProcessService, times(2))
      .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE);
    Mockito.verify(internalProcessService).getAllBulkInternalProcessByStatusOrderByDateAsc(anyString(),anyString(),any(PageRequest.class),anyString());
    Mockito.verify(internalProcessService, times(3)).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService).findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList());
  }

  @Test
  public void processVendorAutoAssignmentsRandomTest() throws Exception {
    VendorAutoAssignmentFilterRequest autoAssignmentFilterRequest =
      VendorAutoAssignmentFilterRequest.builder().contentPending(true).imagePending(true)
        .postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    BoostedProductFilterRequest boostedProductFilterRequest =
      BoostedProductFilterRequest.builder().postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setTotalCount(3);
    Mockito.when(internalProcessService.findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList())).thenReturn(null);
    VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest =
      new VendorAutoAssignmentFilterRequest();
    vendorAutoAssignmentFilterRequest.setEdited(true);
    List<String> assigneeList =
      Arrays.asList("apiautomation_vendor12@mailinator.com", "apiautomation_vendor3@mailinator.com",
        "apiautomationvendor4@mailinator.com", "apiautomation_vendor22@mailinator.com",
        "apiautomation_vendor8@mailinator.com", "apiautomationvendor9@mailinator.com");
    String assignees = String.join(", ", assigneeList);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    bulkInternalProcess.setNotes(assignees);
    bulkInternalProcess.setFileName(filterAssignmentRequest);
    Mockito.when(objectMapper.readValue(bulkInternalProcess.getFileName(),
      VendorAutoAssignmentFilterRequest.class)).thenReturn(autoAssignmentFilterRequest);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(productDistributionTaskRepository.fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class))).thenReturn(productCodeResponse);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatusOrderByDateAsc(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcess(any())).thenReturn(bulkInternalProcess);
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID,
          INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_PENDING))
      .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
      BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(productDistributionTaskRepository, times(6)).fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class));
    Mockito.verify(internalProcessService, times(2))
      .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE);
    Mockito.verify(internalProcessService).getAllBulkInternalProcessByStatusOrderByDateAsc(anyString(),anyString(),any(PageRequest.class),anyString());
    Mockito.verify(internalProcessService, times(6)).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService).findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList());
  }

  @Test
  public void processVendorAutoAssignmentsForAlreadyPendingTest() throws Exception {
    VendorAutoAssignmentFilterRequest autoAssignmentFilterRequest =
      VendorAutoAssignmentFilterRequest.builder().contentPending(true).imagePending(true)
        .postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    BoostedProductFilterRequest boostedProductFilterRequest =
      BoostedProductFilterRequest.builder().postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    Mockito.when(internalProcessService.findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList())).thenReturn(bulkInternalProcess);
    bulkInternalProcess.setTotalCount(3);
    VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest =
      new VendorAutoAssignmentFilterRequest();
    vendorAutoAssignmentFilterRequest.setEdited(true);
    List<String> assigneeList = Arrays.asList("apiautomation_vendor12@mailinator.com",
      "apiautomation_vendor3@mailinator.com", "apiautomationvendor4@mailinator.com");
    String assignees = String.join(", ", assigneeList);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    bulkInternalProcess.setNotes(assignees);
    bulkInternalProcess.setFileName(filterAssignmentRequest);
    bulkInternalProcess.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(objectMapper.readValue(bulkInternalProcess.getFileName(),
      VendorAutoAssignmentFilterRequest.class)).thenReturn(autoAssignmentFilterRequest);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(productDistributionTaskRepository.fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class))).thenReturn(productCodeResponse);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatusOrderByDateAsc(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID,
          INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_PENDING))
      .thenReturn(Collections.singletonList(bulkInternalProcessData));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
      BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE);
    Mockito.verify(internalProcessService).getAllBulkInternalProcessByStatusOrderByDateAsc(anyString(),anyString(),any(PageRequest.class),anyString());
    Mockito.verify(internalProcessService).findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList());
  }

  @Test
  public void processVendorAutoAssignmentsExceptionTest() throws Exception {
    VendorAutoAssignmentFilterRequest autoAssignmentFilterRequest =
      VendorAutoAssignmentFilterRequest.builder().contentPending(true).imagePending(true)
        .postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    BoostedProductFilterRequest boostedProductFilterRequest =
      BoostedProductFilterRequest.builder().postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setTotalCount(3);
    VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest =
      new VendorAutoAssignmentFilterRequest();
    vendorAutoAssignmentFilterRequest.setEdited(true);
    List<String> assigneeList = Arrays.asList("apiautomation_vendor12@mailinator.com",
      "apiautomation_vendor3@mailinator.com", "apiautomationvendor4@mailinator.com");
    String assignees = String.join(", ", assigneeList);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    bulkInternalProcess.setNotes(new ObjectMapper().writeValueAsString(assigneeList));
    bulkInternalProcess.setFileName(filterAssignmentRequest);
    Mockito.when(objectMapper.readValue(bulkInternalProcess.getFileName(),
      VendorAutoAssignmentFilterRequest.class)).thenReturn(autoAssignmentFilterRequest);
    Mockito.when(internalProcessService.findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList())).thenReturn(null);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.doThrow(ApplicationException.class).when(productDistributionTaskRepository).fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class));
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatusOrderByDateAsc(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito.when(internalProcessService.saveInternalProcess(any())).thenReturn(bulkInternalProcess);
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID,
          INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_PENDING))
      .thenReturn(Collections.singletonList(bulkInternalProcessData));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
      BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(productDistributionTaskRepository, times(3)).fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class));
    Mockito.verify(internalProcessService, times(2))
      .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE);
    Mockito.verify(internalProcessService).getAllBulkInternalProcessByStatusOrderByDateAsc(anyString(),anyString(),any(PageRequest.class),anyString());
    Mockito.verify(internalProcessService).findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList());
  }

  @Test
  public void processVendorAutoAssignmentsForNoVendorTest() throws Exception {
    VendorAutoAssignmentFilterRequest autoAssignmentFilterRequest =
      VendorAutoAssignmentFilterRequest.builder().contentPending(true).imagePending(true)
        .postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    BoostedProductFilterRequest boostedProductFilterRequest =
      BoostedProductFilterRequest.builder().postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setTotalCount(3);
    VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest =
      new VendorAutoAssignmentFilterRequest();
    vendorAutoAssignmentFilterRequest.setEdited(true);
    String assignees = null;
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    bulkInternalProcess.setNotes(null);
    bulkInternalProcess.setFileName(filterAssignmentRequest);
    Mockito.when(objectMapper.readValue(bulkInternalProcess.getFileName(),
      VendorAutoAssignmentFilterRequest.class)).thenReturn(autoAssignmentFilterRequest);
    Mockito.when(internalProcessService.findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList())).thenReturn(null);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(productDistributionTaskRepository.fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class))).thenReturn(productCodeResponse);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatusOrderByDateAsc(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcess(any())).thenReturn(bulkInternalProcess);
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID,
          INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_PENDING))
      .thenReturn(Collections.singletonList(bulkInternalProcessData));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
      BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(internalProcessService, times(2))
      .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE);
    Mockito.verify(internalProcessService).getAllBulkInternalProcessByStatusOrderByDateAsc(anyString(),anyString(),any(PageRequest.class),anyString());
    Mockito.verify(internalProcessService).findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList());
  }

  @Test
  public void processVendorAutoAssignmentsForSingleAssignmentTest() throws Exception {
    VendorAutoAssignmentFilterRequest autoAssignmentFilterRequest =
      VendorAutoAssignmentFilterRequest.builder().contentPending(true).imagePending(true)
        .postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    this.productCodeResponse.getContent().add(ProductCodeResponse.builder().productCodes(PRODUCT_SKU).build());
    BoostedProductFilterRequest boostedProductFilterRequest =
      BoostedProductFilterRequest.builder().postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setTotalCount(3);
    VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest =
      new VendorAutoAssignmentFilterRequest();
    vendorAutoAssignmentFilterRequest.setEdited(true);
    List<String> assigneeList = Arrays.asList("apiautomation_vendor12@mailinator.com");
    String assignees = String.join(", ", assigneeList);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    bulkInternalProcess.setNotes(new ObjectMapper().writeValueAsString(assigneeList));
    bulkInternalProcess.setFileName(filterAssignmentRequest);
    Mockito.when(internalProcessService.countByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList())).thenReturn(0L);
    Mockito.when(objectMapper.readValue(bulkInternalProcess.getFileName(),
      VendorAutoAssignmentFilterRequest.class)).thenReturn(autoAssignmentFilterRequest);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(productDistributionTaskRepository.fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class))).thenReturn(productCodeResponse);
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatusOrderByDateAsc(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcess(any())).thenReturn(bulkInternalProcess);
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID,
          INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_PENDING))
      .thenReturn(Collections.singletonList(bulkInternalProcessData));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
      BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(productDistributionTaskRepository).fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class));
    Mockito.verify(internalProcessService, times(2))
      .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE);
    Mockito.verify(internalProcessService).getAllBulkInternalProcessByStatusOrderByDateAsc(anyString(),anyString(),any(PageRequest.class),anyString());
    Mockito.verify(internalProcessService, times(2)).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService).findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList());
  }

  @Test
  public void processNewInternalProcessRequestBulkRebateTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_REBATE.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRICE_REBATE_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_REBATE.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));

    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito
        .when(fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
        .thenReturn(getSheetByInputPath("src/test/resources/PriceRebate/Price_Rebate.xlsx"));
    internalProcessServiceWrapper
        .processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.BULK_PRICE_REBATE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRICE_REBATE_SIZE);
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_REBATE.name());
  }


  @Test
  public void processNewInternalProcessRequestBulkNotRebateTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_REBATE.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRICE_REBATE_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.BULK_PRICE_REBATE.name()))
      .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));

    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
      .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito
      .when(fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(getSheetByInputPath("src/test/resources/PriceRebate/Price_Rebate.xlsx"));
    internalProcessServiceWrapper
      .processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.BULK_PRICE_REBATE.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRICE_REBATE_SIZE);
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
        BulkInternalProcessType.BULK_PRICE_REBATE.name());
  }

  @Test
  public void processNewInternalProcessRequestBulkTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    String errorMessage = "ItemSku must not be blank or InValid. ";
    String INTERNAL_PROCESS_REQUEST_CODE = "request-code";
    String processType = "BULK_PRICE_PRODUCT_TYPE_TAGGING";
    BulkInternalProcessData data1 = new BulkInternalProcessData(
      INTERNAL_PROCESS_REQUEST_CODE, null, "FAILED", errorMessage,
      "{\"storeId\":null,\"itemSku\":\"BLA-123456\",\"pickupPointCode\":\"PP-123\","
        + "\"productTypeTagging\":\"KVI\",\"deleteProductTypeTagging\":\"No\",\"id\":null,\"excelRowNumber\":1}",
      null, "ID", processType, null
    );

    BulkInternalProcessData data2 = new BulkInternalProcessData(
      INTERNAL_PROCESS_REQUEST_CODE, null, "FAILED", errorMessage,
      "{\"storeId\":null,\"itemSku\":\"GLA-345678\",\"pickupPointCode\":\"PP-425\","
        + "\"productTypeTagging\":\"Margin Puller\",\"deleteProductTypeTagging\":\"No\",\"id\":null,\"excelRowNumber\":2}",
      null, "ID", processType, null
    );
    ArrayList<BulkInternalProcessData> processDataList = new ArrayList<>();
    processDataList.add(data1);
    processDataList.add(data2);

    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name()))
      .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));

    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
      .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito
      .when(fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(getSheetByInputPath("src/test/resources/ProductTypeTagging/ProductTagging.xlsx"));
    internalProcessServiceWrapper
      .processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE);
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
        BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(internalProcessService).saveInternalProcessData(internalProcessDataArgumentCaptor.capture());

  }

  @Test
  public void processNewInternalProcessRequestBulkRebateFailedTest() throws Exception {
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "bulkRebateMaxRows", 5);
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_REBATE.name());
    bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_PRICE_REBATE_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
                BulkInternalProcessType.BULK_PRICE_REBATE.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito.when(
            fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
        .thenReturn(getSheetByInputPath("src/test/resources/PriceRebate/Price_Rebate_Failed.xlsx"));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
        BulkInternalProcessType.BULK_PRICE_REBATE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRICE_REBATE_SIZE);
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_REBATE.name());
  }

  @Test
  public void processNewInternalProcessRequestBulkRebateEmptyFileTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_REBATE.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRICE_REBATE_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_REBATE.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));

    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito
        .when(fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
        .thenReturn(getSheetByInputPath("src/test/resources/PriceRebate/Price_Rebate_Empty_File.xlsx"));
    internalProcessServiceWrapper
        .processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.BULK_PRICE_REBATE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRICE_REBATE_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_REBATE.name());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(mailDeliveryService).sendEmailByForInternalBulkProcess(bulkInternalProcessArgumentCaptor.capture(),
        eq(ProcessStatus.FAILED.name()));
  }

  @Test
  public void processNewInternalProcessRequestProductTypeTaggingFailedTest() throws Exception {
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "bulkRebateMaxRows", 5);
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_REBATE.name());
    bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name()))
      .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
      .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito.when(
        fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(getSheetByInputPath("src/test/resources/ProductTypeTagging/ProductTaggingFailed"
        + ".xlsx"));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
      BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE);
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
        BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(mailDeliveryService).sendEmailByForInternalBulkProcess(bulkInternalProcessArgumentCaptor.capture(),
      eq(ProcessStatus.FAILED.name()));
  }

  private XSSFSheet getSheetByInputPath(String path) throws Exception {
    try(FileInputStream insertQrExcelStream = new FileInputStream(path)) {
      XSSFWorkbook workBook = new XSSFWorkbook(insertQrExcelStream);
      return workBook.getSheetAt(0);
    }
  }

  @Test
  public void processNewInternalProcessRequestBulkProductTypeTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name()))
      .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));

    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
      .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito
      .when(fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(getSheetByInputPath("src/test/resources/ProductTypeTagging/ProductTagging.xlsx"));
    internalProcessServiceWrapper
      .processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE);
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
        BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Mockito.verify(internalProcessService).saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
  }

  @Test
  public void processNewInternalProcessRequestBulkProductTypeWithMaxRowsTest() throws Exception {
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "bulkProductTypeTaggingMaxRows",
      10);
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name()))
      .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));

    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
      .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito
      .when(fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(getSheetByInputPath("src/test/resources/ProductTypeTagging/ProductTagging.xlsx"));
    internalProcessServiceWrapper
      .processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE);
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
        BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
  }

  @Test
  public void processNewInternalProcessRequestBulkProductTypeEmptyFileTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name()))
      .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));

    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
      .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito
      .when(fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(getSheetByInputPath("src/test/resources/ProductTypeTagging/ProductTaggingEmpty.xlsx"));
    internalProcessServiceWrapper
      .processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
        BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(mailDeliveryService).sendEmailByForInternalBulkProcess(bulkInternalProcessArgumentCaptor.capture(),
      eq(ProcessStatus.FAILED.name()));
  }

  @Test
  public void processVendorAutoAssignmentForMultipleProductsTest() throws Exception {
    VendorAutoAssignmentFilterRequest autoAssignmentFilterRequest =
      VendorAutoAssignmentFilterRequest.builder().contentPending(true).imagePending(true)
        .postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    BoostedProductFilterRequest boostedProductFilterRequest =
      BoostedProductFilterRequest.builder().postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setTotalCount(3);
    VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest =
      new VendorAutoAssignmentFilterRequest();
    vendorAutoAssignmentFilterRequest.setEdited(true);
    List<String> assigneeList = Arrays.asList("apiautomation_vendor12@mailinator.com");
    String assignees = String.join(", ", assigneeList);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    bulkInternalProcess.setNotes(assignees);
    bulkInternalProcess.setFileName(filterAssignmentRequest);
    Mockito.when(internalProcessService.saveInternalProcess(any())).thenReturn(bulkInternalProcess);
    Mockito.when(objectMapper.readValue(bulkInternalProcess.getFileName(),
      VendorAutoAssignmentFilterRequest.class)).thenReturn(autoAssignmentFilterRequest);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(productDistributionTaskRepository.fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class))).thenReturn(productCodeResponse);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatusOrderByDateAsc(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID,
          INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_PENDING))
      .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(internalProcessService.findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList())).thenReturn(null);
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
      BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(productDistributionTaskRepository).fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class));
    Mockito.verify(internalProcessService, times(2))
      .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE);
    Mockito.verify(internalProcessService).getAllBulkInternalProcessByStatusOrderByDateAsc(anyString(),anyString(),any(PageRequest.class),anyString());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService).findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList());
  }

  @Test
  public void failPendingForBulkApprovalTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(BULK_MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BULK_APPROVAL_FAIL_BEFORE_IN_MINUTES)).thenReturn(systemParameterConfig);
    internalProcessServiceWrapper.failPendingBulkInternalProcessDataBefore(STORE_ID, BulkInternalProcessType.BULK_APPROVAL.name());
    Mockito.verify(internalProcessService).failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
            .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_APPROVAL_FAIL_BEFORE_IN_MINUTES);
  }

  @Test
  public void failPendingForIPRProductsTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(BULK_MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_FAIL_BEFORE_IN_MINUTES))
        .thenReturn(systemParameterConfig);
    internalProcessServiceWrapper.failPendingBulkInternalProcessDataBefore(STORE_ID,
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    Mockito.verify(internalProcessService)
        .failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_FAIL_BEFORE_IN_MINUTES);
  }

  @Test
  public void failPendingForBulkRejectionTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(BULK_MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BULK_REJECTION_FAIL_BEFORE_IN_MINUTES)).thenReturn(systemParameterConfig);
    internalProcessServiceWrapper.failPendingBulkInternalProcessDataBefore(STORE_ID, BulkInternalProcessType.BULK_REJECTION.name());
    Mockito.verify(internalProcessService).failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
            .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_REJECTION_FAIL_BEFORE_IN_MINUTES);
  }

  @Test
  public void getAbortMinutesForBulkApprovalTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(BULK_MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BULK_APPROVAL_ABORT_BEFORE_IN_MINUTES)).thenReturn(systemParameterConfig);
    internalProcessServiceWrapper.abortPendingBulkInternalProcessBefore(STORE_ID, BulkInternalProcessType.BULK_APPROVAL.name());
    Mockito.verify(internalProcessService).abortPendingBulkInternalProcess(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
            .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_APPROVAL_ABORT_BEFORE_IN_MINUTES);
  }

  @Test
  public void getAbortMinutesForBulkRejectionTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(BULK_MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BULK_REJECTION_ABORT_BEFORE_IN_MINUTES)).thenReturn(systemParameterConfig);
    internalProcessServiceWrapper.abortPendingBulkInternalProcessBefore(STORE_ID, BulkInternalProcessType.BULK_REJECTION.name());
    Mockito.verify(internalProcessService).abortPendingBulkInternalProcess(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
            .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_REJECTION_ABORT_BEFORE_IN_MINUTES);
  }

  @Test
  public void processBulkRebateUploadTest() throws JsonProcessingException {
    String jsonString = "{\"month\":\"April\",\"year\":\"2024\",\"storeId\":\"GLA-230001\",\"mainCategoryCode\":\"CAT-1234\",\"categoryCode\":\"CAT-6789\",\"projectedRebate\":\"200000\",\"rowNumber\":1}";
    bulkInternalProcessData.setData(jsonString);
    Mockito.when(internalProcessService
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(priceAnalyticsOutboundService.updatePriceRebate(Mockito.any(), Mockito.any())).thenReturn("error");
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceRebateRequestData.class))
        .thenReturn(
            new BulkPriceRebateRequestData("April", "2024", "storeId", "brandName", "mainCategoryCode", "categoryCode",
                "200", 2));
    internalProcessServiceWrapper.processBulkRebateUpload(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2)).saveBulkInternalProcessData(Mockito.any());
    Mockito.verify(objectMapper).readValue(bulkInternalProcessData.getData(), BulkPriceRebateRequestData.class);
  }


  @Test
  public void processBulkRebateUploadTest2() throws JsonProcessingException {
    String jsonString = "{\"month\":\"April\",\"year\":\"2024\",\"storeId\":\"GLA-230001\",\"mainCategoryCode\":\"CAT-1234\",\"categoryCode\":\"CAT-6789\",\"projectedRebate\":\"200000\",\"rowNumber\":1}";
    bulkInternalProcessData.setData(jsonString);
    Mockito.when(internalProcessService
            .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(priceAnalyticsOutboundService.updatePriceRebate(Mockito.any(), Mockito.any())).thenReturn(null);
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceRebateRequestData.class))
        .thenReturn(
            new BulkPriceRebateRequestData("April", "2024", "storeId", "brandName", "mainCategoryCode", "categoryCode",
                "200", 2));
    internalProcessServiceWrapper.processBulkRebateUpload(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2)).saveBulkInternalProcessData(Mockito.any());
    Mockito.verify(objectMapper).readValue(bulkInternalProcessData.getData(), BulkPriceRebateRequestData.class);
  }

  @Test
  public void processBulkRebateUploadExceptionTest() throws JsonProcessingException {
    String jsonString =
        "{\"month\":\"April\",\"year\":\"2024\",\"storeId\":\"GLA-230001\",\"mainCategoryCode\":\"CAT-1234\",\"categoryCode\":\"CAT-6789\",\"projectedRebate\":\"200000\",\"rowNumber\":1}";
    bulkInternalProcessData.setData(jsonString);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_ID,
        ProcessStatus.IN_PROGRESS.name())).thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    Mockito.doThrow(new RuntimeException()).when(priceAnalyticsOutboundService)
        .updatePriceRebate(Mockito.any(), Mockito.any());
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceRebateRequestData.class))
        .thenReturn(
            new BulkPriceRebateRequestData("April", "2024", "storeId", "brandName", "mainCategoryCode", "categoryCode",
                "200", 2));
    try {
      internalProcessServiceWrapper.processBulkRebateUpload(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
    } finally {
      Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_ID,
          ProcessStatus.IN_PROGRESS.name());
      Mockito.verify(internalProcessService, times(2)).saveBulkInternalProcessData(Mockito.any());
      Mockito.verify(objectMapper).readValue(bulkInternalProcessData.getData(), BulkPriceRebateRequestData.class);
    }
  }

    @Test
    public void processBulkRebateUploadInvalidBulkProcessData() {
      Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_ID,
          ProcessStatus.IN_PROGRESS.name())).thenReturn(null);
      internalProcessServiceWrapper.processBulkRebateUpload(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
      Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_ID,
          ProcessStatus.IN_PROGRESS.name());
      Mockito.verify(internalProcessService).saveBulkInternalProcessData(Mockito.any());
    }


  @Test
  public void processVendorAutoAssignmentForRepeatedHitsTest() throws Exception {
    VendorAutoAssignmentFilterRequest autoAssignmentFilterRequest =
      VendorAutoAssignmentFilterRequest.builder().contentPending(true).imagePending(true)
        .postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    BoostedProductFilterRequest boostedProductFilterRequest =
      BoostedProductFilterRequest.builder().postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    BulkInternalProcess bulkInternalProcess2 = new BulkInternalProcess();
    bulkInternalProcess.setTotalCount(3);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess2.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest =
      new VendorAutoAssignmentFilterRequest();
    vendorAutoAssignmentFilterRequest.setEdited(true);
    List<String> assigneeList = Arrays.asList("apiautomation_vendor12@mailinator.com");
    String assignees = String.join(", ", assigneeList);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    bulkInternalProcess.setNotes(assignees);
    bulkInternalProcess.setFileName(filterAssignmentRequest);
    BeanUtils.copyProperties(bulkInternalProcess, bulkInternalProcess2);
    Mockito.when(objectMapper.readValue(bulkInternalProcess.getFileName(),
      VendorAutoAssignmentFilterRequest.class)).thenReturn(autoAssignmentFilterRequest);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(productDistributionTaskRepository.fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class))).thenReturn(productCodeResponse);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatusOrderByDateAsc(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess ,bulkInternalProcess2), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID,
          INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_PENDING))
      .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(internalProcessService.findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList())).thenReturn(null);
    Mockito.when(internalProcessService.saveInternalProcess(any())).thenReturn(bulkInternalProcess);
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess, bulkInternalProcess2));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
      BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(productDistributionTaskRepository, times(2)).fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class));
    Mockito.verify(internalProcessService, times(4))
      .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE);
    Mockito.verify(internalProcessService).getAllBulkInternalProcessByStatusOrderByDateAsc(anyString(),anyString(),any(PageRequest.class),anyString());
    Mockito.verify(internalProcessService, times(2)).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService, times(2)).findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList());
  }

  @Test
  public void processVendorAutoAssignmentForSameRequests() throws Exception {
    VendorAutoAssignmentFilterRequest autoAssignmentFilterRequest =
      VendorAutoAssignmentFilterRequest.builder().contentPending(true).imagePending(true)
        .postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    BoostedProductFilterRequest boostedProductFilterRequest =
      BoostedProductFilterRequest.builder().postLive(true).edited(true).revised(true).restrictedKeyword(true).build();
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    BulkInternalProcess bulkInternalProcess2 = new BulkInternalProcess();
    bulkInternalProcess.setTotalCount(3);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess2.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest =
      new VendorAutoAssignmentFilterRequest();
    vendorAutoAssignmentFilterRequest.setEdited(true);
    List<String> assigneeList = Arrays.asList("apiautomation_vendor12@mailinator.com");
    String assignees = String.join(", ", assigneeList);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    bulkInternalProcess.setNotes(assignees);
    bulkInternalProcess.setFileName(filterAssignmentRequest);
    BeanUtils.copyProperties(bulkInternalProcess, bulkInternalProcess2);
    Mockito.when(objectMapper.readValue(bulkInternalProcess.getFileName(),
      VendorAutoAssignmentFilterRequest.class)).thenReturn(autoAssignmentFilterRequest);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(productDistributionTaskRepository.fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class))).thenReturn(productCodeResponse);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatusOrderByDateAsc(STORE_ID, ProcessStatus.PENDING.name(), pageable,
          BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess ,bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID,
          INTERNAL_PROCESS_REQUEST_CODE, BulkProcessData.STATUS_PENDING))
      .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(internalProcessService.findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList())).thenReturn(null);
    Mockito.when(internalProcessService.saveInternalProcess(any())).thenReturn(bulkInternalProcess);
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess, bulkInternalProcess));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
      BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
    Mockito.verify(productDistributionTaskRepository, times(2)).fetchProductsForAutoAssignment(anyString(),
      anyString(),anyInt(),anyInt(),any(BoostedProductFilterRequest.class));
    Mockito.verify(internalProcessService, times(4))
      .saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE);
    Mockito.verify(internalProcessService).getAllBulkInternalProcessByStatusOrderByDateAsc(anyString(),anyString(),any(PageRequest.class),anyString());
    Mockito.verify(internalProcessService, times(2)).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService, times(2)).findFirstByStoreIdAndProcessTypeAndStatusIn(anyString(),
      anyString(),anyList());
  }

  @Test
  public void uploadBulkRestrictedKeywordProcessTest() {
    internalProcessServiceWrapper.uploadBulkRestrictedKeywordProcess(STORE_ID, bulkRestrictedKeywordUploadModel);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(INTERNAL_PROCESS_REQUEST_CODE,
        bulkInternalProcessArgumentCaptor.getValue().getInternalProcessRequestCode());
    Assertions.assertEquals(BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name(),
        bulkInternalProcessArgumentCaptor.getValue().getProcessType());
    Assertions.assertEquals(FILE_NAME, bulkInternalProcessArgumentCaptor.getValue().getFileName());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void uploadBulkBrandAuthProcessTest() {
    internalProcessServiceWrapper.uploadBulkBrandAuthProcess(STORE_ID, bulkBrandAuthUploadModel);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(INTERNAL_PROCESS_REQUEST_CODE,
        bulkInternalProcessArgumentCaptor.getValue().getInternalProcessRequestCode());
    Assertions.assertEquals(BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name(),
        bulkInternalProcessArgumentCaptor.getValue().getProcessType());
    Assertions.assertEquals(FILE_NAME, bulkInternalProcessArgumentCaptor.getValue().getFileName());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processStatusUpdateForRestrictedKeywordUpsertTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.RESTRICTED_KEYWORD_UPSERT_FINAL_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.doNothing().when(restrictedKeywordService).setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.RESTRICTED_KEYWORD_UPSERT_FINAL_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
    Mockito.verify(restrictedKeywordService).setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
  }

  @Test
  public void processStatusUpdateForRestrictedKeywordDeletionTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.RESTRICTED_KEYWORD_DELETE_FINAL_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.doNothing().when(restrictedKeywordService).setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.RESTRICTED_KEYWORD_DELETE_FINAL_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());
    Mockito.verify(restrictedKeywordService).setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
  }

  @Test
  public void processStatusUpdateForBrandAuthAddTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BRAND_AUTH_ADD_FINAL_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.BRAND_AUTH_ADD.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.doNothing().when(brandAuthorisationService)
        .setFinalStatusAndGenerateFailedExcelForBulkBrandAuth(STORE_ID, bulkInternalProcess);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BRAND_AUTH_ADD.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BRAND_AUTH_ADD_FINAL_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.BRAND_AUTH_ADD.name());
    Mockito.verify(brandAuthorisationService)
        .setFinalStatusAndGenerateFailedExcelForBulkBrandAuth(STORE_ID, bulkInternalProcess);
  }

  @Test
  public void processStatusUpdateForBrandAuthDeleteTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BRAND_AUTH_DELETE_FINAL_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.BRAND_AUTH_DELETE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.doNothing().when(brandAuthorisationService)
        .setFinalStatusAndGenerateFailedExcelForBulkBrandAuth(STORE_ID, bulkInternalProcess);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BRAND_AUTH_DELETE_FINAL_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    Mockito.verify(brandAuthorisationService)
        .setFinalStatusAndGenerateFailedExcelForBulkBrandAuth(STORE_ID, bulkInternalProcess);
  }
  @Test
  public void processInternalProcessDataRequestBrandAuthAddTest() throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE)
            .internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(),
            anyInt())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BRAND_AUTH_BULK_ADD_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BRAND_AUTH_ADD_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BRAND_AUTH_BULK_ADD_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.BRAND_AUTH_ADD.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(
            objectMapper.readValue(bulkInternalProcessData.getData(),
                BrandAuthAddRequestData.class))
        .thenReturn(BrandAuthAddRequestData.builder().brandCode(BRAND_CODE).sellerCode(SELLER_CODE)
            .build());

    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BRAND_AUTH_ADD.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BRAND_AUTH_BULK_ADD_TOTAL_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BRAND_AUTH_ADD_FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BRAND_AUTH_BULK_ADD_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.BRAND_AUTH_ADD.name());
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(), anyInt());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBrandAuthorisationBulkProcess()), anyString(), any());
    verify(kafkaTopicProperties, times(2)).getBrandAuthorisationBulkProcess();
  }

  @Test
  public void processInternalProcessDataRequestBrandAuthAddTest_emptyList() throws JsonProcessingException {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(),
            anyInt())).thenReturn(new ArrayList<>());
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BRAND_AUTH_BULK_ADD_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BRAND_AUTH_ADD_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BRAND_AUTH_BULK_ADD_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.BRAND_AUTH_ADD.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BRAND_AUTH_ADD.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BRAND_AUTH_BULK_ADD_TOTAL_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BRAND_AUTH_ADD_FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BRAND_AUTH_BULK_ADD_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.BRAND_AUTH_ADD.name());
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(), anyInt());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
  }

  @Test
  public void processInternalProcessDataRequestBrandAuthAddTest_errorCase()
      throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE)
            .internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(),
            anyInt())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BRAND_AUTH_BULK_ADD_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BRAND_AUTH_ADD_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BRAND_AUTH_BULK_ADD_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.BRAND_AUTH_ADD.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(
            objectMapper.readValue(bulkInternalProcessData.getData(),
                BrandAuthAddRequestData.class))
        .thenThrow(JsonProcessingException.class);

    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BRAND_AUTH_ADD.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BRAND_AUTH_BULK_ADD_TOTAL_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BRAND_AUTH_ADD_FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BRAND_AUTH_BULK_ADD_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.BRAND_AUTH_ADD.name());
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(), anyInt());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
  }

  @Test
  public void processInternalProcessDataRequestBrandAuthDeleteTest()
      throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE)
            .internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(),
            anyInt())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BRAND_AUTH_BULK_DELETE_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BRAND_AUTH_DELETE_FILE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BRAND_AUTH_BULK_DELETE_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.BRAND_AUTH_DELETE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(
            objectMapper.readValue(bulkInternalProcessData.getData(),
                BrandAuthAddRequestData.class))
        .thenReturn(BrandAuthAddRequestData.builder().brandCode(BRAND_CODE).sellerCode(SELLER_CODE)
            .build());
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BRAND_AUTH_BULK_DELETE_TOTAL_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BRAND_AUTH_DELETE_FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BRAND_AUTH_BULK_DELETE_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(), anyInt());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBrandAuthorisationBulkProcess()),
        anyString(), any());
    verify(kafkaTopicProperties, times(2)).getBrandAuthorisationBulkProcess();
  }

  @Test
  public void processBulkBrandAuthorisationEventTest() throws JsonProcessingException {
    String message = mapper.writeValueAsString(BrandAuthAddRequestData.builder().brandCode(BRAND_CODE).build());
    bulkInternalProcessData.setData(message);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(message, BrandAuthAddRequestData.class))
        .thenReturn(new BrandAuthAddRequestData());
    Mockito.when(internalProcessService.saveInternalProcessData(any())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(pcbOutboundService.createBulkBrandAuthorisation(anyString(), any())).thenReturn(null);
    internalProcessServiceWrapper.processBulkBrandAuthorisationEvent(STORE_ID,
        BulkInternalProcessType.BRAND_AUTH_ADD.name(), INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(objectMapper).readValue(message, BrandAuthAddRequestData.class);
    Mockito.verify(pcbOutboundService).createBulkBrandAuthorisation(anyString(), any());
    Mockito.verify(internalProcessService).saveInternalProcessData(any());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
  }

  @Test
  public void processBulkBrandAuthorisationDeleteEventTest() throws JsonProcessingException {
    String message = mapper.writeValueAsString(BrandAuthDeleteRequestData.builder().brandCode(BRAND_CODE).build());
    bulkInternalProcessData.setData(message);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(message, BrandAuthDeleteRequestData.class))
        .thenReturn(new BrandAuthDeleteRequestData());
    Mockito.when(internalProcessService.saveInternalProcessData(any())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(pcbOutboundService.deleteBulkBrandAuthorisation(anyString(), any())).thenReturn(BRAND_CODE);
    internalProcessServiceWrapper.processBulkBrandAuthorisationEvent(STORE_ID,
        BulkInternalProcessType.BRAND_AUTH_DELETE.name(), INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(objectMapper).readValue(message, BrandAuthDeleteRequestData.class);
    Mockito.verify(pcbOutboundService).deleteBulkBrandAuthorisation(anyString(), any());
    Mockito.verify(internalProcessService).saveInternalProcessData(any());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
  }

  @Test
  public void processBulkBrandAuthorisationDeleteNullEventTest() throws JsonProcessingException {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(any())).thenReturn(null);
    internalProcessServiceWrapper.processBulkBrandAuthorisationEvent(STORE_ID,
        BulkInternalProcessType.BRAND_AUTH_DELETE.name(), INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService).saveInternalProcessData(any());
  }

  @Test
  public void processBulkBrandAuthorisationNullEventTest() throws JsonProcessingException {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
        .thenReturn(null);
    internalProcessServiceWrapper.processBulkBrandAuthorisationEvent(STORE_ID,
        BulkInternalProcessType.BRAND_AUTH_DELETE.name(), INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
  }

  @Test
  public void uploadBulkReviewProcessTest() {
    internalProcessServiceWrapper.uploadBulkReviewProcess(STORE_ID, bulkReviewUploadModel);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(INTERNAL_PROCESS_REQUEST_CODE,
            bulkInternalProcessArgumentCaptor.getValue().getInternalProcessRequestCode());
    Assertions.assertEquals(BulkInternalProcessType.BULK_APPROVAL.name(),
            bulkInternalProcessArgumentCaptor.getValue().getProcessType());
    Assertions.assertEquals(FILE_NAME, bulkInternalProcessArgumentCaptor.getValue().getFileName());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processInternalProcessDataRequestBulkApprovalTest() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
      BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
        .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
      InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE)
        .internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(
      internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(),
        anyInt())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.BULK_APPROVAL_DB_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.BULK_APPROVAL_ROW_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_APPROVAL_FETCH_BATCH_SIZE))
      .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
          BulkInternalProcessType.BULK_APPROVAL.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
      .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
      .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
      BulkInternalProcessType.BULK_APPROVAL.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_APPROVAL_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_APPROVAL_ROW_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_APPROVAL_DB_BATCH_SIZE))
      .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
        BulkInternalProcessType.BULK_APPROVAL.name());
    Mockito.verify(internalProcessService)
      .getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(), anyInt());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService)
      .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBulkApprovalRejectionProcessEvent()), any());
    verify(kafkaTopicProperties, times(2)).getBulkApprovalRejectionProcessEvent();
  }

  @Test
  public void processInternalProcessDataRequestBrandUpdateTest() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE)
            .internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BRAND_UPDATE_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BRAND_UPDATE_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BRAND_UPDATE_ROW_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.BRAND_UPDATE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BRAND_UPDATE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BRAND_UPDATE_TOTAL_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BRAND_UPDATE_FETCH_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BRAND_UPDATE_ROW_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.BRAND_UPDATE.name());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
  }

  @Test
  public void processInternalProcessDataRequestBulkRejectionTest() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
      BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
        .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
      InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE)
        .internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(
      internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(),
        anyInt())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.BULK_REJECTION_DB_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.BULK_REJECTION_ROW_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_REJECTION_FETCH_BATCH_SIZE))
      .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
          BulkInternalProcessType.BULK_REJECTION.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
      .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
      .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
      BulkInternalProcessType.BULK_REJECTION.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_REJECTION_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_REJECTION_ROW_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_REJECTION_DB_BATCH_SIZE);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
        BulkInternalProcessType.BULK_REJECTION.name());
    Mockito.verify(internalProcessService)
      .getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(), anyInt());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService)
      .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBulkApprovalRejectionProcessEvent()), any());
    verify(kafkaTopicProperties, times(2)).getBulkApprovalRejectionProcessEvent();
  }

  @Test
  public void processInternalProcessDataRequestBulkAssigneeTest() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
      BulkInternalProcessPendingDataDTO.builder()
        .internalProcessRequestId(bulkInternalProcess.getId())
        .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
      InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE)
        .internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(
      internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(),
        anyString(), anyInt())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(
      Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.MASTER_SKU_BULK_ASSIGNEE_DB_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.MASTER_SKU_BULK_ASSIGNEE_ROW_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.MASTER_SKU_BULK_ASSIGNEE_FETCH_BATCH_SIZE))
      .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
        ProcessStatus.IN_PROGRESS.name(), pageable,
        BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
      Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(),
      Integer.parseInt(FETCH_BATCH_SIZE))).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
      .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
      BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.MASTER_SKU_BULK_ASSIGNEE_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.MASTER_SKU_BULK_ASSIGNEE_ROW_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.MASTER_SKU_BULK_ASSIGNEE_DB_BATCH_SIZE);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
        BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name());
    Mockito.verify(internalProcessService)
      .getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(),
        anyInt());
    Mockito.verify(internalProcessService)
      .saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService)
      .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBulkAssigneeMasterSkuProcessEvent()),
      anyString(), any());
    verify(kafkaTopicProperties, times(2)).getBulkAssigneeMasterSkuProcessEvent();
  }

  @Test
  public void processInternalProcessDataRequestBulkMasterSkuReviewTest() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE)
            .internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(),
            anyInt())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.MASTER_SKU_REVIEW_DB_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_MASTER_SKU_REVIEW_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.MASTER_SKU_REVIEW_ROW_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.FETCH_PENDING_MASTER_SKU_REVIEW_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.MASTER_SKU_REVIEW_ROW_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.MASTER_SKU_REVIEW_DB_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(), anyInt());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBulkMasterSkuReviewProcessEvent()), anyString(), any());
    verify(kafkaTopicProperties, times(2)).getBulkMasterSkuReviewProcessEvent();
  }

  @Test
  @Disabled
  public void processBulkMasterSkuReviewDataEventDataNotFoundTest() throws Exception {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
        .thenReturn(null);
    try {
      internalProcessServiceWrapper.processBulkMasterSkuReviewDataEvent(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
    } finally {
      Mockito.verify(internalProcessService)
          .bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    }
  }

  @Test
  public void processBulkMasterSkuReviewDataEventFeignExceptionTest() throws Exception {
    ClusterActionResponse clusterActionResponse = new ClusterActionResponse();
    clusterActionResponse.getClusterItemErrorListListResponse().add(new ClusterItemErrorListResponse());
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(Mockito.any())).thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(BulkMasterSkuReviewRequestData.class)))
        .thenReturn(BulkMasterSkuReviewRequestData.builder().action(BulkMasterSkuUploadParameters.DISMANTLE_CLUSTER)
            .firstMasterSku(ITEM_SKU).secondMasterSku(MASTER_SKU).build());
    Mockito.when(masterSkuItemsRepository.performClusterAction(Mockito.anyString(), Mockito.any(), Mockito.anyString()))
        .thenThrow(new ApplicationException());
    internalProcessServiceWrapper.processBulkMasterSkuReviewDataEvent(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(internalProcessService, Mockito.times(2)).saveBulkInternalProcessData(Mockito.any());
    Mockito.verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(BulkMasterSkuReviewRequestData.class));
    Mockito.verify(masterSkuItemsRepository)
        .performClusterAction(Mockito.eq(ITEM_SKU), Mockito.any(), Mockito.anyString());
  }

  @Test
  public void processBulkMasterSkuReviewDataEventTest() throws Exception {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(Mockito.any())).thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(
        BulkMasterSkuReviewRequestData.class)))
        .thenReturn(BulkMasterSkuReviewRequestData.builder().action(BulkMasterSkuUploadParameters.ADD_TO_CLUSTER)
            .firstMasterSku(ITEM_SKU).secondMasterSku(ITEM_SKU).build());
    Mockito.when(masterSkuItemsRepository.performClusterAction(Mockito.anyString(), Mockito.any(),
      Mockito.any())).thenReturn(new ClusterActionResponse());
    internalProcessServiceWrapper.processBulkMasterSkuReviewDataEvent(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(internalProcessService, Mockito.times(2)).saveBulkInternalProcessData(Mockito.any());
    Mockito.verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(
      BulkMasterSkuReviewRequestData.class));
    Mockito.verify(masterSkuItemsRepository)
      .performClusterAction(Mockito.eq(ITEM_SKU), Mockito.any(), Mockito.any());
  }

  @Test
  public void processBulkMasterSkuReviewDataEventDismantleActionTest() throws Exception {
    ClusterActionResponse clusterActionResponse = new ClusterActionResponse();
    clusterActionResponse.getClusterItemErrorListListResponse().add(new ClusterItemErrorListResponse());
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(Mockito.any())).thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(
        BulkMasterSkuReviewRequestData.class)))
        .thenReturn(BulkMasterSkuReviewRequestData.builder().action(BulkMasterSkuUploadParameters.DISMANTLE_CLUSTER)
            .firstMasterSku(ITEM_SKU).secondMasterSku(MASTER_SKU).build());
    Mockito.when(masterSkuItemsRepository.performClusterAction(Mockito.anyString(), Mockito.any(),
      Mockito.any())).thenReturn(clusterActionResponse);
    internalProcessServiceWrapper.processBulkMasterSkuReviewDataEvent(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(internalProcessService, Mockito.times(2)).saveBulkInternalProcessData(Mockito.any());
    Mockito.verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(
      BulkMasterSkuReviewRequestData.class));
    Mockito.verify(masterSkuItemsRepository)
      .performClusterAction(Mockito.eq(ITEM_SKU), Mockito.any(), Mockito.any());
  }

  @Test
  public void processBulkMasterSkuReviewDataEventExceptionTest() throws Exception {
    ClusterActionResponse clusterActionResponse = new ClusterActionResponse();
    clusterActionResponse.getClusterItemErrorListListResponse().add(new ClusterItemErrorListResponse());
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(Mockito.any())).thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(
        BulkMasterSkuReviewRequestData.class)))
        .thenThrow(new RuntimeException());
    Mockito.when(masterSkuItemsRepository.performClusterAction(Mockito.anyString(), Mockito.any(),
      Mockito.any())).thenReturn(clusterActionResponse);
    try {
      internalProcessServiceWrapper.processBulkMasterSkuReviewDataEvent(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
    } finally {
      Mockito.verify(internalProcessService)
          .bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
      Mockito.verify(internalProcessService, Mockito.times(2)).saveBulkInternalProcessData(Mockito.any());
      Mockito.verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(
        BulkMasterSkuReviewRequestData.class));
    }
  }

  @Test
  public void processBulkVendorActionsEventBulkRejectionTest() throws JsonProcessingException, ApplicationException {
    String REASON = Constant.PRODUK.concat(":").concat("Upload dua kali");
    String message =
            mapper.writeValueAsString(BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID).reason(REASON).build());
    bulkInternalProcessData.setData(message);
    bulkInternalProcessData.setStoreId(STORE_ID);
    bulkInternalProcessData.setCreatedBy(USER_NAME);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(any())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
            .thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(message, BulkApprovalRejectionRequestData.class))
            .thenReturn(BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID).reason(REASON).build());
    Mockito.when(productDistributionTaskRepository.vendorRejection(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any(RejectProductVendorRequest.class))).thenReturn("Product Already Approved");
    internalProcessServiceWrapper.processBulkVendorActionsEvent(STORE_ID, BulkInternalProcessType.BULK_REJECTION.name(),
            INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(productDistributionTaskRepository).vendorRejection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any(RejectProductVendorRequest.class));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
  }

  @Test
  public void processBulkVendorActionsEventBulkRejectionFailedTest()
      throws JsonProcessingException, ApplicationException {
    String PRODUCT_REASON = "Upload dua kali";
    String REASON = Constant.PRODUK.concat(":").concat(PRODUCT_REASON);
    String message = mapper.writeValueAsString(
        BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID)
            .reason(REASON).build());
    bulkInternalProcessData.setData(message);
    bulkInternalProcessData.setStoreId(STORE_ID);
    bulkInternalProcessData.setCreatedBy(USER_NAME);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(any()))
        .thenReturn(bulkInternalProcessDataList);
    Mockito.when(
        internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(),
            anyString())).thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(message, BulkApprovalRejectionRequestData.class))
        .thenReturn(
            BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID)
                .reason(REASON).build());
    Mockito.doThrow(ValidationException.class).when(productDistributionTaskRepository)
        .vendorRejection(eq(STORE_ID), eq(SELLER_CODE), eq(VENDOR_CODE),
            any(RejectProductVendorRequest.class));
    internalProcessServiceWrapper.processBulkVendorActionsEvent(STORE_ID,
        BulkInternalProcessType.BULK_REJECTION.name(), INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(internalProcessService)
        .saveInternalProcessData(Mockito.anyList());
    Mockito.verify(productDistributionTaskRepository)
        .vendorRejection(eq(STORE_ID), eq(SELLER_CODE), eq(VENDOR_CODE),
            any(RejectProductVendorRequest.class));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
  }

  @Test
  public void processBulkVendorActionsEventBulkRejectionErrorTest() throws JsonProcessingException,
          ApplicationException {
    String REASON = Constant.PRODUK.concat(":").concat("Upload dua kali");
    String message =
            mapper.writeValueAsString(BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID).reason(REASON).build());
    bulkInternalProcessData.setData(message);
    bulkInternalProcessData.setStoreId(STORE_ID);
    bulkInternalProcessData.setCreatedBy(USER_NAME);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(any())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
            .thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(message, BulkApprovalRejectionRequestData.class))
            .thenReturn(BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID).reason(REASON).build());
    Mockito.when(productDistributionTaskRepository.vendorRejection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any(RejectProductVendorRequest.class))).thenReturn(null);
    internalProcessServiceWrapper.processBulkVendorActionsEvent(STORE_ID, BulkInternalProcessType.BULK_REJECTION.name(),
            INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(productDistributionTaskRepository).vendorRejection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any(RejectProductVendorRequest.class));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
  }

  @Test
  public void processBulkVendorActionsEventBulkApprovalTest() throws JsonProcessingException, ApplicationException {
    String message =
            mapper.writeValueAsString(BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID).build());
    bulkInternalProcessData.setData(message);
    bulkInternalProcessData.setStoreId(STORE_ID);
    bulkInternalProcessData.setCreatedBy(USER_NAME);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(any())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
            .thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(message, BulkApprovalRejectionRequestData.class))
            .thenReturn(BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID).build());
    Mockito.when(productDistributionTaskRepository.vendorQuickApproval(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(VendorQuickApprovalRequest.class))).thenReturn(null);
    internalProcessServiceWrapper.processBulkVendorActionsEvent(STORE_ID, BulkInternalProcessType.BULK_APPROVAL.name(),
            INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(productDistributionTaskRepository).vendorQuickApproval(Mockito.anyString(), Mockito.anyString(), Mockito.any(VendorQuickApprovalRequest.class));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
  }

  @Test
  public void processBulkVendorActionsEventNullTest() throws JsonProcessingException {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
            .thenReturn(null);
    internalProcessServiceWrapper.processBulkVendorActionsEvent(STORE_ID,
            BulkInternalProcessType.BULK_APPROVAL.name(), INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
  }

  @Test
  public void processBulkVendorActionNullEventTest() throws JsonProcessingException {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
            .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(any())).thenReturn(null);
    internalProcessServiceWrapper.processBulkVendorActionsEvent(STORE_ID,
            BulkInternalProcessType.BULK_APPROVAL.name(), INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService).saveInternalProcessData(any());
  }

  @Test
  public void processStatusUpdateForVendorRejectionTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_VENDOR_REJECTION_FINAL_UPDATE_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
        ProcessStatus.PUBLISHED.name(), pageable, BulkInternalProcessType.BULK_REJECTION.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.doNothing().when(bulkVendorActionService)
      .setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
      BulkInternalProcessType.BULK_REJECTION.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.BULK_VENDOR_REJECTION_FINAL_UPDATE_SIZE);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
        BulkInternalProcessType.BULK_REJECTION.name());
    Mockito.verify(bulkVendorActionService)
      .setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
  }

  @Test
  public void processStatusUpdateForVendorApprovalTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_VENDOR_APPROVAL_FINAL_UPDATE_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
        ProcessStatus.PUBLISHED.name(), pageable, BulkInternalProcessType.BULK_APPROVAL.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.doNothing().when(bulkVendorActionService)
      .setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
      BulkInternalProcessType.BULK_APPROVAL.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.BULK_VENDOR_APPROVAL_FINAL_UPDATE_SIZE);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
        BulkInternalProcessType.BULK_APPROVAL.name());
    Mockito.verify(bulkVendorActionService)
      .setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
  }


  @Test
  public void processStatusUpdateForIPRProductsTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_FINAL_UPDATE_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
            ProcessStatus.PUBLISHED.name(), pageable, BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.doNothing().when(bulkVendorActionService)
        .setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_FINAL_UPDATE_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    Mockito.verify(bulkIPRProductService)
        .setFinalStatusAndGenerateFailedExcelForBulkAddReviewIPRProducts(STORE_ID, bulkInternalProcess);
  }

  @Test
  public void processStatusUpdateForBulkAssigneeMasterSkuTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_MASTER_SKU_ASSIGNEE_FINAL_UPDATE_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
        ProcessStatus.PUBLISHED.name(), pageable,
        BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.doNothing().when(bulkMasterSkuReviewService)
      .setFinalStatusAndGenerateFailedExcelForBulkAssignee(STORE_ID, bulkInternalProcess);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
      BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.BULK_MASTER_SKU_ASSIGNEE_FINAL_UPDATE_SIZE);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
        BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name());
    Mockito.verify(bulkMasterSkuReviewService)
      .setFinalStatusAndGenerateFailedExcelForBulkAssignee(STORE_ID, bulkInternalProcess);
  }

  @Test
  public void processStatusUpdateForBulkReviewMasterSkuTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_MASTER_SKU_REVIEW_FINAL_UPDATE_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.doNothing().when(bulkMasterSkuReviewService)
        .setFinalStatusAndGenerateFailedExcelForBulkAssignee(STORE_ID, bulkInternalProcess);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_MASTER_SKU_REVIEW_FINAL_UPDATE_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
    Mockito.verify(bulkMasterSkuReviewService)
        .setFinalStatusAndGenerateFailedExcelForBulkMasterSkuReview(STORE_ID, bulkInternalProcess);
  }

  @Test
  public void processBulkVendorActionsEventBulkApprovalWithErrorTest() throws JsonProcessingException {
    GdnRestSingleResponse<VendorQuickApprovalResponse> vendorQuickApprovalResponse =
      new GdnRestSingleResponse<>();
    List<String> errorCodes = Arrays.asList("ERR-PDT400001","ERR-PDT400003","ERR-PDT400006","ERR-PDT400008");
    vendorQuickApprovalResponse.setSuccess(true);
    vendorQuickApprovalResponse.setValue(VendorQuickApprovalResponse.builder().errorCodes(errorCodes).build());
    String message =
      mapper.writeValueAsString(BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID).build());
    bulkInternalProcessData.setData(message);
    bulkInternalProcessData.setStoreId(STORE_ID);
    bulkInternalProcessData.setCreatedBy(USER_NAME);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(any())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
      .thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(message, BulkApprovalRejectionRequestData.class))
      .thenReturn(BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID).build());
    Mockito.when(productDistributionTaskRepository.vendorQuickApproval(Mockito.anyString(), Mockito.anyString(),
      Mockito.any(VendorQuickApprovalRequest.class))).thenReturn(vendorQuickApprovalResponse);
    internalProcessServiceWrapper.processBulkVendorActionsEvent(STORE_ID, BulkInternalProcessType.BULK_APPROVAL.name(),
      INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(productDistributionTaskRepository).vendorQuickApproval(Mockito.anyString(), Mockito.anyString(), Mockito.any(VendorQuickApprovalRequest.class));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
  }

  @Test
  public void processBulkVendorActionsEventBulkApprovalWithErrorTest2() throws JsonProcessingException {
    GdnRestSingleResponse<VendorQuickApprovalResponse> vendorQuickApprovalResponse =
      new GdnRestSingleResponse<>();
    List<String> errorCodes = Arrays.asList("ERR-PDT400001","ERR-PDT400003","ERR-PDT400006","ERR"
      + "ERR-PDT400009","ERR-PDT400010","ERR-PDT400011" ,"ERR-PDT400013", "ERR-PDT400012", "ERR"
      + "-PDT400009", "ERR-PDT400014","ERR-PDT400015", "ERR-PDT400016");
    vendorQuickApprovalResponse.setSuccess(true);
    vendorQuickApprovalResponse.setValue(VendorQuickApprovalResponse.builder().errorCodes(errorCodes).build());
    String message =
      mapper.writeValueAsString(BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID).build());
    bulkInternalProcessData.setData(message);
    bulkInternalProcessData.setStoreId(STORE_ID);
    bulkInternalProcessData.setCreatedBy(USER_NAME);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(any())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
      .thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(message, BulkApprovalRejectionRequestData.class))
      .thenReturn(BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID).build());
    Mockito.when(productDistributionTaskRepository.vendorQuickApproval(Mockito.anyString(), Mockito.anyString(),
      Mockito.any(VendorQuickApprovalRequest.class))).thenReturn(vendorQuickApprovalResponse);
    internalProcessServiceWrapper.processBulkVendorActionsEvent(STORE_ID, BulkInternalProcessType.BULK_APPROVAL.name(),
      INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(productDistributionTaskRepository).vendorQuickApproval(Mockito.anyString(), Mockito.anyString(), Mockito.any(VendorQuickApprovalRequest.class));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
  }

  @Test
  public void processBulkVendorActionsEventBulkApprovalSuccessFalseTest() throws JsonProcessingException {
    GdnRestSingleResponse<VendorQuickApprovalResponse> vendorQuickApprovalResponse =
      new GdnRestSingleResponse<>();
    List<String> errorCodes = Arrays.asList("ERR-PDT400001","ERR-PDT400003","ERR-PDT400006");
    vendorQuickApprovalResponse.setSuccess(false);
    vendorQuickApprovalResponse.setErrorMessage("Invalid Product");
    String message =
      mapper.writeValueAsString(BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID).build());
    bulkInternalProcessData.setData(message);
    bulkInternalProcessData.setStoreId(STORE_ID);
    bulkInternalProcessData.setCreatedBy(USER_NAME);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(any())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
      .thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(message, BulkApprovalRejectionRequestData.class))
      .thenReturn(BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID).build());
    Mockito.when(productDistributionTaskRepository.vendorQuickApproval(Mockito.anyString(), Mockito.anyString(),
      Mockito.any(VendorQuickApprovalRequest.class))).thenReturn(vendorQuickApprovalResponse);
    internalProcessServiceWrapper.processBulkVendorActionsEvent(STORE_ID, BulkInternalProcessType.BULK_APPROVAL.name(),
      INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(productDistributionTaskRepository).vendorQuickApproval(Mockito.anyString(), Mockito.anyString(), Mockito.any(VendorQuickApprovalRequest.class));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
  }

  @Test
  public void processBulkVendorActionsEventBulkApprovalSuccessFalseTest2() throws JsonProcessingException {
    GdnRestSingleResponse<VendorQuickApprovalResponse> vendorQuickApprovalResponse =
      new GdnRestSingleResponse<>();
    List<String> errorCodes = Arrays.asList("ERR-PDT400001","ERR-PDT400003","ERR-PDT400006","ERR"
      + "ERR-PDT400009","ERR-PDT400010","ERR-PDT400011" ,"ERR-PDT400013", "ERR-PDT400012", "ERR-PDT400009");
    vendorQuickApprovalResponse.setSuccess(false);
    vendorQuickApprovalResponse.setErrorMessage("Invalid Product");
    String message =
      mapper.writeValueAsString(BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID).build());
    bulkInternalProcessData.setData(message);
    bulkInternalProcessData.setStoreId(STORE_ID);
    bulkInternalProcessData.setCreatedBy(USER_NAME);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(any())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString()))
      .thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(message, BulkApprovalRejectionRequestData.class))
      .thenReturn(BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID).build());
    Mockito.when(productDistributionTaskRepository.vendorQuickApproval(Mockito.anyString(), Mockito.anyString(),
      Mockito.any(VendorQuickApprovalRequest.class))).thenReturn(vendorQuickApprovalResponse);
    internalProcessServiceWrapper.processBulkVendorActionsEvent(STORE_ID, BulkInternalProcessType.BULK_APPROVAL.name(),
      INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(productDistributionTaskRepository).vendorQuickApproval(Mockito.anyString(), Mockito.anyString(), Mockito.any(VendorQuickApprovalRequest.class));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
  }


  @Test
  public void processBulkVendorActionIllegalEventTest() throws Exception {
    String REASON = Constant.PRODUK.concat(":").concat("Upload dua kali");
    String message = mapper.writeValueAsString(
      BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID)
        .reason(REASON).build());
    bulkInternalProcessData.setData(message);
    bulkInternalProcessData.setStoreId(STORE_ID);
    bulkInternalProcessData.setCreatedBy(USER_NAME);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(any()))
      .thenReturn(bulkInternalProcessDataList);
    Mockito.when(
      internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(),
        anyString())).thenReturn(bulkInternalProcessData);
    Mockito.when(objectMapper.readValue(message, BulkApprovalRejectionRequestData.class))
      .thenReturn(
        BulkApprovalRejectionRequestData.builder().productCode(PRODUCT_CODE).comment(REQUEST_ID)
          .reason(REASON).build());
    doThrow(IllegalArgumentException.class).when(productDistributionTaskRepository)
      .vendorRejection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(RejectProductVendorRequest.class));
    try {
      internalProcessServiceWrapper.processBulkVendorActionsEvent(STORE_ID, BulkInternalProcessType.BULK_REJECTION.name(), INTERNAL_PROCESS_REQUEST_ID);
    }
    finally {
      Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(anyString(), anyString(), anyString());
      Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
      Mockito.verify(internalProcessService).saveInternalProcessData(any());
    }
  }

  @Test
  public void failPendingForMasterSkuAssigneeRejectionTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(BULK_MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.MASTER_SKU_BULK_ASSIGNEE_FAIL_BEFORE_IN_MINUTES))
      .thenReturn(systemParameterConfig);
    internalProcessServiceWrapper.failPendingBulkInternalProcessDataBefore(STORE_ID,
      BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name());
    Mockito.verify(internalProcessService)
      .failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.MASTER_SKU_BULK_ASSIGNEE_FAIL_BEFORE_IN_MINUTES);
  }

  @Test
  public void failPendingForMasterSkuAssigneeReviewTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(BULK_MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.MASTER_SKU_BULK_REVIEW_FAIL_BEFORE_IN_MINUTES))
      .thenReturn(systemParameterConfig);
    internalProcessServiceWrapper.failPendingBulkInternalProcessDataBefore(STORE_ID,
      BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
    Mockito.verify(internalProcessService)
      .failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.MASTER_SKU_BULK_REVIEW_FAIL_BEFORE_IN_MINUTES);
  }

  @Test
  public void failPendingForAutoApprovedAssigneeRejectionTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(BULK_MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.AUTO_APPROVED_BULK_ASSIGNEE_FAIL_BEFORE_IN_MINUTES))
      .thenReturn(systemParameterConfig);
    internalProcessServiceWrapper.failPendingBulkInternalProcessDataBefore(STORE_ID,
      BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    Mockito.verify(internalProcessService)
      .failPendingBulkInternalProcessData(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.AUTO_APPROVED_BULK_ASSIGNEE_FAIL_BEFORE_IN_MINUTES);
  }


  private void setMdcParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
      USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
      REQUESTID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
      CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
      CHANNEL_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
      STORE_ID);
  }

  @Test
  public void testProcessBulkMasterSkuAssigneeEvent() throws Exception {
    bulkInternalProcessData.setCreatedBy(USERNAME);
    String message = mapper.writeValueAsString(
      BulkAssigneeMasterSkuReviewRequestData.builder().firstAnchorSku(FIRST_ANCHOR)
        .secondAnchorSku(SECOND_ANCHOR).assignee(USERNAME).build());
    bulkInternalProcessData.setData(message);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
      .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(any()))
      .thenReturn(bulkInternalProcessData);
    UserResponse userResponse = new UserResponse();
    userResponse.setUsername(USERNAME);
    userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
    Mockito.when(partnersEngineOutboundService.userFilter(
        VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
        VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null))
      .thenReturn(userResponseListBaseResponse);
    BulkAssigneeMasterSkuReviewRequestData bulkAssigneeMasterSkuReviewRequestData =
      BulkAssigneeMasterSkuReviewRequestData.builder().firstAnchorSku(FIRST_ANCHOR)
      .secondAnchorSku(SECOND_ANCHOR).assignee(USERNAME).build();
    Mockito.when(
        objectMapper.readValue(anyString(), eq(BulkAssigneeMasterSkuReviewRequestData.class)))
      .thenReturn(bulkAssigneeMasterSkuReviewRequestData);
    Mockito.when(masterSkuReviewOutboundService.processBulkUploadAssigneeAction(anyString(),
        any(ChangeAssigneeRequest.class)))
      .thenReturn(null);
    internalProcessServiceWrapper.processBulkMasterSkuAssigneeEvent(STORE_ID,
      INTERNAL_PROCESS_REQUEST_ID);
    verify(internalProcessService, times(1)).bulkInternalProcessDataByIdAndStatus(STORE_ID,
      INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name());
    verify(objectMapper, times(1)).readValue(bulkInternalProcessData.getData(),
      BulkAssigneeMasterSkuReviewRequestData.class);
    verify(masterSkuReviewOutboundService, times(1)).processBulkUploadAssigneeAction(
      bulkInternalProcessData.getInternalProcessRequestId(),
      RequestHelper.toChangeAssigneeRequest(bulkAssigneeMasterSkuReviewRequestData,
        bulkInternalProcessData.getSellerCode()));
    verify(internalProcessService, times(2)).saveBulkInternalProcessData(bulkInternalProcessData);
    verify(partnersEngineOutboundService).userFilter(
      VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
      VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null);
  }

  @Test
  public void testProcessBulkMasterSkuAssigneeEventWrongAssignee() throws Exception {
    bulkInternalProcessData.setCreatedBy(USERNAME);
    String message = mapper.writeValueAsString(
      BulkAssigneeMasterSkuReviewRequestData.builder().firstAnchorSku(FIRST_ANCHOR)
        .secondAnchorSku(SECOND_ANCHOR).assignee(USERNAME).build());
    bulkInternalProcessData.setData(message);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
      .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(any()))
      .thenReturn(bulkInternalProcessData);
    UserResponse userResponse = new UserResponse();
    userResponse.setUsername(SYSTEM);
    userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
    Mockito.when(partnersEngineOutboundService.userFilter(
        VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
        VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null))
      .thenReturn(userResponseListBaseResponse);
    BulkAssigneeMasterSkuReviewRequestData bulkAssigneeMasterSkuReviewRequestData =
      BulkAssigneeMasterSkuReviewRequestData.builder().firstAnchorSku(FIRST_ANCHOR)
        .secondAnchorSku(SECOND_ANCHOR).assignee(USERNAME).build();
    Mockito.when(
        objectMapper.readValue(eq(message), eq(BulkAssigneeMasterSkuReviewRequestData.class)))
      .thenReturn(bulkAssigneeMasterSkuReviewRequestData);
    Mockito.when(masterSkuReviewOutboundService.processBulkUploadAssigneeAction(anyString(),
      any(ChangeAssigneeRequest.class))).thenReturn(null);
    internalProcessServiceWrapper.processBulkMasterSkuAssigneeEvent(STORE_ID,
      INTERNAL_PROCESS_REQUEST_ID);
    verify(internalProcessService, times(1)).bulkInternalProcessDataByIdAndStatus(STORE_ID,
      INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name());
    verify(objectMapper, times(1)).readValue(bulkInternalProcessData.getData(),
      BulkAssigneeMasterSkuReviewRequestData.class);
    verify(internalProcessService, times(2)).saveBulkInternalProcessData(bulkInternalProcessData);
    verify(partnersEngineOutboundService).userFilter(
      VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
      VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null);
  }

  @Test
  public void testProcessBulkMasterSkuAssigneeEventErrorMessage() throws Exception {
    bulkInternalProcessData.setCreatedBy(USERNAME);
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    String message = mapper.writeValueAsString(
      BulkAssigneeMasterSkuReviewRequestData.builder().firstAnchorSku(FIRST_ANCHOR)
        .secondAnchorSku(SECOND_ANCHOR).assignee(StringUtils.EMPTY).build());
    bulkInternalProcessData.setData(message);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
      .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(any()))
      .thenReturn(bulkInternalProcessData);
    UserResponse userResponse = new UserResponse();
    userResponse.setUsername(USERNAME);
    userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
    Mockito.when(partnersEngineOutboundService.userFilter(
        VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
        VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null))
      .thenReturn(userResponseListBaseResponse);
    BulkAssigneeMasterSkuReviewRequestData bulkAssigneeMasterSkuReviewRequestData =
      BulkAssigneeMasterSkuReviewRequestData.builder().firstAnchorSku(FIRST_ANCHOR)
      .secondAnchorSku(SECOND_ANCHOR).assignee(StringUtils.EMPTY).build();
    Mockito.when(
        objectMapper.readValue(anyString(), eq(BulkAssigneeMasterSkuReviewRequestData.class)))
      .thenReturn(bulkAssigneeMasterSkuReviewRequestData);
    Mockito.when(masterSkuReviewOutboundService.processBulkUploadAssigneeAction(anyString(),
      any(ChangeAssigneeRequest.class))).thenReturn("error message");
    internalProcessServiceWrapper.processBulkMasterSkuAssigneeEvent(STORE_ID,
      INTERNAL_PROCESS_REQUEST_ID);
    verify(internalProcessService, times(1)).bulkInternalProcessDataByIdAndStatus(STORE_ID,
      INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name());
    verify(objectMapper, times(1)).readValue(bulkInternalProcessData.getData(),
      BulkAssigneeMasterSkuReviewRequestData.class);
    verify(masterSkuReviewOutboundService, times(1)).processBulkUploadAssigneeAction(
      bulkInternalProcessData.getInternalProcessRequestId(),
      RequestHelper.toChangeAssigneeRequest(bulkAssigneeMasterSkuReviewRequestData,
        bulkInternalProcessData.getSellerCode()));
    verify(internalProcessService, times(2)).saveBulkInternalProcessData(bulkInternalProcessData);
    verify(partnersEngineOutboundService).userFilter(
      VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
      VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null);
  }

  @Test
  public void testProcessBulkMasterSkuAssigneeEventDataNotFoundErrorMessage() throws Exception {
    bulkInternalProcessData.setCreatedBy(USERNAME);
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    String message = mapper.writeValueAsString(
      BulkAssigneeMasterSkuReviewRequestData.builder().firstAnchorSku(FIRST_ANCHOR)
        .secondAnchorSku(SECOND_ANCHOR).assignee(StringUtils.EMPTY).build());
    bulkInternalProcessData.setData(message);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
      .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(any()))
      .thenReturn(bulkInternalProcessData);
    UserResponse userResponse = new UserResponse();
    userResponse.setUsername(USERNAME);
    userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
    Mockito.when(partnersEngineOutboundService.userFilter(
        VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
        VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null))
      .thenReturn(userResponseListBaseResponse);
    BulkAssigneeMasterSkuReviewRequestData bulkAssigneeMasterSkuReviewRequestData =
      BulkAssigneeMasterSkuReviewRequestData.builder().firstAnchorSku(FIRST_ANCHOR)
        .secondAnchorSku(SECOND_ANCHOR).assignee(StringUtils.EMPTY).build();
    Mockito.when(
        objectMapper.readValue(anyString(), eq(BulkAssigneeMasterSkuReviewRequestData.class)))
      .thenReturn(bulkAssigneeMasterSkuReviewRequestData);
    Mockito.when(masterSkuReviewOutboundService.processBulkUploadAssigneeAction(anyString(),
      any(ChangeAssigneeRequest.class))).thenReturn(ErrorCategory.DATA_NOT_FOUND.name());
    internalProcessServiceWrapper.processBulkMasterSkuAssigneeEvent(STORE_ID,
      INTERNAL_PROCESS_REQUEST_ID);
    verify(internalProcessService, times(1)).bulkInternalProcessDataByIdAndStatus(STORE_ID,
      INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name());
    verify(objectMapper, times(1)).readValue(bulkInternalProcessData.getData(),
      BulkAssigneeMasterSkuReviewRequestData.class);
    verify(masterSkuReviewOutboundService, times(1)).processBulkUploadAssigneeAction(
      bulkInternalProcessData.getInternalProcessRequestId(),
      RequestHelper.toChangeAssigneeRequest(bulkAssigneeMasterSkuReviewRequestData,
        bulkInternalProcessData.getSellerCode()));
    verify(internalProcessService, times(2)).saveBulkInternalProcessData(bulkInternalProcessData);
    verify(partnersEngineOutboundService).userFilter(
      VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
      VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null);
  }

  @Test
  public void testProcessBulkMasterSkuAssigneeEventNullData() {
    BulkInternalProcessData bulkInternalProcessData = null;
    internalProcessServiceWrapper.processBulkMasterSkuAssigneeEvent(STORE_ID,
      INTERNAL_PROCESS_REQUEST_ID);
    verify(internalProcessService, times(1)).bulkInternalProcessDataByIdAndStatus(STORE_ID,
      INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name());
    verify(internalProcessService, Mockito.never()).saveInternalProcessData(
      Collections.singletonList(bulkInternalProcessData));
    verify(internalProcessService, Mockito.never()).saveBulkInternalProcessData(
      bulkInternalProcessData);
  }


  @Test
  public void testProcessBulkMasterSkuAssigneeEvent_ExceptionHandling()
    throws JsonProcessingException {
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(
      internalProcessService.bulkInternalProcessDataByIdAndStatus(anyString(), anyString(),
        anyString())).thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(any()))
      .thenReturn(bulkInternalProcessData);
    doThrow(new JsonProcessingException("Simulated JsonProcessingException") {
    }).when(objectMapper).readValue(anyString(), eq(BulkAssigneeMasterSkuReviewRequestData.class));
    internalProcessServiceWrapper.processBulkMasterSkuAssigneeEvent(STORE_ID,
      INTERNAL_PROCESS_REQUEST_ID);
    verify(internalProcessService, times(1)).bulkInternalProcessDataByIdAndStatus(STORE_ID,
      INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name());
    verify(masterSkuReviewOutboundService, Mockito.never()).processBulkUploadAssigneeAction(
      anyString(), any());
    verify(internalProcessService, times(1)).saveBulkInternalProcessData(any());
    verify(partnersEngineOutboundService).userFilter(
      VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
      VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, null);
  }

  @Test
  public void processInternalProcessDataRequestAutoApprovedProductsBulkAssignTest() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder()
            .internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE)
            .internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(),
            anyString(), anyInt())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN_DB_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN_ROW_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
            ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(),
        Integer.parseInt(FETCH_BATCH_SIZE))).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN_ROW_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN_DB_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(),
            anyInt());
    Mockito.verify(internalProcessService)
        .saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getAutoApprovedProductsBulkAssignProcessEvent()),
        anyString(), any());
    verify(kafkaTopicProperties, times(2)).getAutoApprovedProductsBulkAssignProcessEvent();
  }


  @Test
  public void processInternalProcessDataRequestIPRProductsBulkAddReviewTest() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder()
            .internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        InternalProcessDataDomainEventModel.builder().storeId(STORE_ID).parentCode(PRODUCT_CODE)
            .internalProcessRequestId(REQUEST_ID).build();
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(),
            anyString(), anyInt())).thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_DB_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_ROW_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
            ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(),
        Integer.parseInt(FETCH_BATCH_SIZE))).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_ROW_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_DB_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByRequestIdsAndStatus(anyString(), anyList(), anyString(),
            anyInt());
    Mockito.verify(internalProcessService)
        .saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(any());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBulkIprPortalAddReviewProcessEvent()),
        anyString(), any());
    verify(kafkaTopicProperties, times(2)).getBulkIprPortalAddReviewProcessEvent();
  }

  @Test
  public void processInternalProcessDataRequestBulkPriceUpdateTest() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_DB_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_ROW_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.BULK_PRICE_UPDATE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BULK_PRICE_UPDATE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_ROW_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_DB_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_UPDATE.name());
    Mockito.verify(internalProcessService).getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Collections.singletonList(bulkInternalProcessData.getId()), ProcessStatus.PENDING.name(), 10);
    Mockito.verify(internalProcessService).saveInternalProcesses(bulkInternalProcessListArgumentCaptor.capture());
    Mockito.verify(internalProcessService).saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBulkInternalPriceUpdateEvent()),
        eq(bulkInternalProcessData.getParentCode() + bulkInternalProcess.getInternalProcessRequestCode()),
        internalBulkUploadDataDomainEventModelArgumentCaptor.capture());
    verify(kafkaTopicProperties, times(2)).getBulkInternalPriceUpdateEvent();
    Assertions.assertEquals(REQUEST_ID,
        internalBulkUploadDataDomainEventModelArgumentCaptor.getValue().getInternalProcessDataRequestIdList().get(0));
    Assertions.assertEquals(1,
        internalBulkUploadDataDomainEventModelArgumentCaptor.getValue().getInternalProcessDataRequestIdList().size(), 0);
  }

  @Test
  public void processInternalProcessDataRequestBulkPriceUpdateMultiVariantTest() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessData.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData1.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    bulkInternalProcessData1.setId(CLIENT_ID);
    bulkInternalProcessData1.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData1.setInternalProcessRequestCode(bulkInternalProcessData.getInternalProcessRequestCode());
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    bulkInternalProcessDataList.add(bulkInternalProcessData1);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_DB_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_ROW_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.BULK_PRICE_UPDATE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
        .thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BULK_PRICE_UPDATE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_ROW_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_DB_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_UPDATE.name());
    Mockito.verify(internalProcessService).getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Collections.singletonList(bulkInternalProcessData.getId()), ProcessStatus.PENDING.name(), 10);
    Mockito.verify(internalProcessService).saveInternalProcesses(bulkInternalProcessListArgumentCaptor.capture());
    Mockito.verify(internalProcessService).saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBulkInternalPriceUpdateEvent()),
        eq(bulkInternalProcessData.getParentCode() + bulkInternalProcess.getInternalProcessRequestCode()),
        internalBulkUploadDataDomainEventModelArgumentCaptor.capture());
    verify(kafkaTopicProperties, times(2)).getBulkInternalPriceUpdateEvent();
    Assertions.assertEquals(REQUEST_ID,
        internalBulkUploadDataDomainEventModelArgumentCaptor.getValue().getInternalProcessDataRequestIdList().get(0));
    Assertions.assertEquals(2,
        internalBulkUploadDataDomainEventModelArgumentCaptor.getValue().getInternalProcessDataRequestIdList().size(),
        0);
  }

  @Test
  public void processInternalProcessDataRequestBulkPriceUpdate2ItemsTest() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessData.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData1.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    bulkInternalProcessData1.setId(CLIENT_ID);
    bulkInternalProcessData1.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData1.setInternalProcessRequestCode(PRODUCT_CODE);
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    bulkInternalProcessDataList.add(bulkInternalProcessData1);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_DB_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_ROW_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.BULK_PRICE_UPDATE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
        .thenReturn(bulkInternalProcessDataList);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BULK_PRICE_UPDATE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_ROW_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_DB_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_UPDATE.name());
    Mockito.verify(internalProcessService).getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Collections.singletonList(bulkInternalProcessData.getId()), ProcessStatus.PENDING.name(), 10);
    Mockito.verify(internalProcessService).saveInternalProcesses(bulkInternalProcessListArgumentCaptor.capture());
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBulkInternalPriceUpdateEvent()),
        eq(bulkInternalProcessData.getParentCode() + bulkInternalProcess.getInternalProcessRequestCode()),
        internalBulkUploadDataDomainEventModelArgumentCaptor.capture());
    verify(kafkaTopicProperties, times(2)).getBulkInternalPriceUpdateEvent();
    Assertions.assertEquals(REQUEST_ID,
        internalBulkUploadDataDomainEventModelArgumentCaptor.getValue().getInternalProcessDataRequestIdList().get(0));
    Assertions.assertEquals(1,
        internalBulkUploadDataDomainEventModelArgumentCaptor.getValue().getInternalProcessDataRequestIdList().size(),
        0);
  }

  @Test
  public void processInternalProcessDataRequestBulkProductTypeTaggingEvent() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessData.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
      BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
        .processType(bulkInternalProcess.getProcessType()).build()));

    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_UPDATE_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_UPDATE_DB_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE))
      .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRODUCT_TYPE_TAGGING_PUBLISH_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
          BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
      .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
      .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
      BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_UPDATE_DB_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_UPDATE_FETCH_BATCH_SIZE);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
        BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(internalProcessService).getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
      Collections.singletonList(bulkInternalProcessData.getId()), ProcessStatus.PENDING.name(), 10);
    Mockito.verify(internalProcessService)
      .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.PRODUCT_TYPE_TAGGING_PUBLISH_BATCH_SIZE);
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBulkProductTypeTaggingUpdateEvent()),
      eq(bulkInternalProcessData.getInternalProcessRequestCode()),
      internalBulkUploadDataDomainEventModelArgumentCaptor.capture());
    verify(kafkaTopicProperties, times(3)).getBulkProductTypeTaggingUpdateEvent();
    verify(internalProcessService).saveInternalProcesses(bulkInternalProcessListArgumentCaptor.capture());
    verify(internalProcessService).saveInternalProcessData(Collections.singletonList(bulkInternalProcessData));
  }

  @Test
  public void processInternalProcessDataRequestBulkProductTypeTaggingEventWithEmptyList() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessData.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRODUCT_TYPE_TAGGING_PUBLISH_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
      BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
        .processType(bulkInternalProcess.getProcessType()).build()));
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_UPDATE_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_UPDATE_DB_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE))
      .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
          BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
      .thenReturn(Collections.emptyList());
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
      .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
      BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_UPDATE_DB_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_UPDATE_FETCH_BATCH_SIZE);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
        BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      "bulk_product_type_tagging_update_fetch_batch_size");
    Mockito.verify(internalProcessService).getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
      Collections.singletonList(bulkInternalProcessData.getId()), ProcessStatus.PENDING.name(), 10);
  }


  @Test
  public void processAutoApprovedProductsBulkAssignEvent_successTest()
      throws JsonProcessingException {
    bulkInternalProcessData.setCreatedBy(USERNAME);
    String message = mapper.writeValueAsString(
        BulkAssignAutoApprovedProductsRequestData.builder().productCode(PRODUCT_CODE)
            .assignee(USERNAME).build());
    bulkInternalProcessData.setData(message);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
            INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(any()))
        .thenReturn(bulkInternalProcessData);
    UserResponse userResponse = new UserResponse();
    userResponse.setUsername(USERNAME);
    userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
    BulkAssignAutoApprovedProductsRequestData bulkAssignAutoApprovedProductsRequestData =
        BulkAssignAutoApprovedProductsRequestData.builder().productCode(PRODUCT_CODE)
            .assignee(USERNAME).build();
    Mockito.when(
            objectMapper.readValue(anyString(), eq(BulkAssignAutoApprovedProductsRequestData.class)))
        .thenReturn(bulkAssignAutoApprovedProductsRequestData);
    Mockito.when(
            productAnalyticsOutboundService.processUpdateAssigneeForAutoApprovedProducts(bulkInternalProcessData.getInternalProcessRequestId(),
                RequestHelper.toAutoApprovedAssigneeRequest(bulkAssignAutoApprovedProductsRequestData)))
        .thenReturn(null);
    internalProcessServiceWrapper.processAutoApprovedProductsBulkAssignEvent(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID);
    verify(internalProcessService, times(1)).bulkInternalProcessDataByIdAndStatus(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name());
    verify(objectMapper, times(1)).readValue(bulkInternalProcessData.getData(),
        BulkAssignAutoApprovedProductsRequestData.class);
    verify(productAnalyticsOutboundService, times(1)).processUpdateAssigneeForAutoApprovedProducts(
        bulkInternalProcessData.getInternalProcessRequestId(),
        RequestHelper.toAutoApprovedAssigneeRequest(bulkAssignAutoApprovedProductsRequestData));
    verify(internalProcessService, times(2)).saveBulkInternalProcessData(bulkInternalProcessData);
    Assertions.assertEquals(ProcessStatus.COMPLETED.name(), bulkInternalProcessData.getStatus());
  }

  @Test
  public void processAutoApprovedProductsBulkAssignEvent_productNotFound()
      throws JsonProcessingException {
    bulkInternalProcessData.setCreatedBy(USERNAME);
    String message = mapper.writeValueAsString(
        BulkAssignAutoApprovedProductsRequestData.builder().productCode(PRODUCT_CODE)
            .assignee(USERNAME).build());
    bulkInternalProcessData.setData(message);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
            INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(any()))
        .thenReturn(bulkInternalProcessData);
    UserResponse userResponse = new UserResponse();
    userResponse.setUsername(USERNAME);
    userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
    BulkAssignAutoApprovedProductsRequestData bulkAssignAutoApprovedProductsRequestData =
        BulkAssignAutoApprovedProductsRequestData.builder().productCode(PRODUCT_CODE)
            .assignee(USERNAME).build();
    Mockito.when(
            objectMapper.readValue(anyString(), eq(BulkAssignAutoApprovedProductsRequestData.class)))
        .thenReturn(bulkAssignAutoApprovedProductsRequestData);
    Mockito.when(
            productAnalyticsOutboundService.processUpdateAssigneeForAutoApprovedProducts(bulkInternalProcessData.getInternalProcessRequestId(),
                RequestHelper.toAutoApprovedAssigneeRequest(bulkAssignAutoApprovedProductsRequestData)))
        .thenReturn("Product not found");
    internalProcessServiceWrapper.processAutoApprovedProductsBulkAssignEvent(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID);
    verify(internalProcessService, times(1)).bulkInternalProcessDataByIdAndStatus(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name());
    verify(objectMapper, times(1)).readValue(bulkInternalProcessData.getData(),
        BulkAssignAutoApprovedProductsRequestData.class);
    verify(productAnalyticsOutboundService, times(1)).processUpdateAssigneeForAutoApprovedProducts(
        bulkInternalProcessData.getInternalProcessRequestId(),
        RequestHelper.toAutoApprovedAssigneeRequest(bulkAssignAutoApprovedProductsRequestData));
    verify(internalProcessService, times(2)).saveBulkInternalProcessData(bulkInternalProcessData);
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessData.getStatus());
    Assertions.assertEquals("Product not found", bulkInternalProcessData.getErrorMessage());
  }

  @Test
  public void processAutoApprovedProductsBulkAssignEvent_productAlreadyUnassigned()
      throws JsonProcessingException {
    bulkInternalProcessData.setCreatedBy(USERNAME);
    String message = mapper.writeValueAsString(
        BulkAssignAutoApprovedProductsRequestData.builder().productCode(PRODUCT_CODE)
            .assignee(USERNAME).build());
    bulkInternalProcessData.setData(message);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
            INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(any()))
        .thenReturn(bulkInternalProcessData);
    UserResponse userResponse = new UserResponse();
    userResponse.setUsername(USERNAME);
    userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
    BulkAssignAutoApprovedProductsRequestData bulkAssignAutoApprovedProductsRequestData =
        BulkAssignAutoApprovedProductsRequestData.builder().productCode(PRODUCT_CODE)
            .assignee(USERNAME).build();
    Mockito.when(
            objectMapper.readValue(anyString(), eq(BulkAssignAutoApprovedProductsRequestData.class)))
        .thenReturn(bulkAssignAutoApprovedProductsRequestData);
    Mockito.when(
            productAnalyticsOutboundService.processUpdateAssigneeForAutoApprovedProducts(bulkInternalProcessData.getInternalProcessRequestId(),
                RequestHelper.toAutoApprovedAssigneeRequest(bulkAssignAutoApprovedProductsRequestData)))
        .thenReturn("Product is already un-assigned.");
    internalProcessServiceWrapper.processAutoApprovedProductsBulkAssignEvent(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID);
    verify(internalProcessService, times(1)).bulkInternalProcessDataByIdAndStatus(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name());
    verify(objectMapper, times(1)).readValue(bulkInternalProcessData.getData(),
        BulkAssignAutoApprovedProductsRequestData.class);
    verify(productAnalyticsOutboundService, times(1)).processUpdateAssigneeForAutoApprovedProducts(
        bulkInternalProcessData.getInternalProcessRequestId(),
        RequestHelper.toAutoApprovedAssigneeRequest(bulkAssignAutoApprovedProductsRequestData));
    verify(internalProcessService, times(2)).saveBulkInternalProcessData(bulkInternalProcessData);
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessData.getStatus());
    Assertions.assertEquals("Product is already un-assigned.",
        bulkInternalProcessData.getErrorMessage());
  }

  @Test
  public void processAutoApprovedProductsBulkAssignEvent_exceptionTest()
      throws JsonProcessingException {
    bulkInternalProcessData.setCreatedBy(USERNAME);
    String message = mapper.writeValueAsString(
        BulkAssignAutoApprovedProductsRequestData.builder().productCode(PRODUCT_CODE)
            .assignee(USERNAME).build());
    bulkInternalProcessData.setData(message);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
            INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(any()))
        .thenReturn(bulkInternalProcessData);
    UserResponse userResponse = new UserResponse();
    userResponse.setUsername(USERNAME);
    userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
    BulkAssignAutoApprovedProductsRequestData bulkAssignAutoApprovedProductsRequestData =
        BulkAssignAutoApprovedProductsRequestData.builder().productCode(PRODUCT_CODE)
            .assignee(USERNAME).build();
    Mockito.doThrow(new ApplicationRuntimeException()).when(
        objectMapper).readValue(anyString(), eq(BulkAssignAutoApprovedProductsRequestData.class));
    internalProcessServiceWrapper.processAutoApprovedProductsBulkAssignEvent(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID);
    verify(internalProcessService, times(1)).bulkInternalProcessDataByIdAndStatus(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name());
    verify(objectMapper, times(1)).readValue(bulkInternalProcessData.getData(),
        BulkAssignAutoApprovedProductsRequestData.class);
    verify(internalProcessService, times(2)).saveBulkInternalProcessData(bulkInternalProcessData);
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessData.getStatus());
    Assertions.assertEquals(Constant.SYSTEM_ERROR, bulkInternalProcessData.getErrorMessage());
  }

  @Test
  public void processAutoApprovedProductsBulkAssignEvent_emptyData()
      throws JsonProcessingException {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
            INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(null);
    internalProcessServiceWrapper.processAutoApprovedProductsBulkAssignEvent(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID);
    verify(internalProcessService, times(1)).bulkInternalProcessDataByIdAndStatus(STORE_ID,
        INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name());
  }

  @Test
  public void processStatusUpdateForBulkAssigneeAutoApprovedTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.AUTO_APPROVED_BULK_ASSIGNEE_FINAL_UPDATE_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
        ProcessStatus.PUBLISHED.name(), pageable,
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.doNothing().when(bulkAutoApprovedProductsService)
      .setFinalStatusAndGenerateNotificationForAutoApprovedBulkAssignee(STORE_ID, bulkInternalProcess);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
      BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.AUTO_APPROVED_BULK_ASSIGNEE_FINAL_UPDATE_SIZE);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    Mockito.verify(bulkAutoApprovedProductsService)
      .setFinalStatusAndGenerateNotificationForAutoApprovedBulkAssignee(STORE_ID, bulkInternalProcess);
  }

  @Test
  public void processStatusUpdateForBulkRebateUploadTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REBATE_UPDATE_FINAL_UPDATE_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.BULK_PRICE_REBATE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.doNothing().when(bulkRebateUpdateService)
        .setFinalStatusAndGenerateErrorFileForBulkRebateUpdate(STORE_ID, bulkInternalProcess);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BULK_PRICE_REBATE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REBATE_UPDATE_FINAL_UPDATE_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_REBATE.name());
    Mockito.verify(bulkRebateUpdateService)
        .setFinalStatusAndGenerateErrorFileForBulkRebateUpdate(STORE_ID, bulkInternalProcess);
  }

  @Test
  public void processStatusUpdateForBulkProductTypeTaggingTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_UPDATE_FINAL_UPDATE_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
          BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name()))
      .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.doNothing().when(bulkRebateUpdateService)
      .setFinalStatusAndGenerateErrorFileForBulkRebateUpdate(STORE_ID, bulkInternalProcess);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
      BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_UPDATE_FINAL_UPDATE_SIZE);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
        BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    Mockito.verify(bulkProductTypeTaggingUpdateService)
      .setFinalStatusAndGenerateFailedExcelForBulkProductTypeUpdate(STORE_ID, bulkInternalProcess);
  }

  @Test
  public void preProcessBulkPriceRebate() throws Exception {
    BulkUpdateProcessDTO bulkUpdateProcessDTO = new BulkUpdateProcessDTO();
    bulkUpdateProcessDTO.setBulkProcessType(BulkInternalProcessType.BULK_PRICE_REBATE.name());
    Mockito.doNothing().when(fileStorageService)
        .createBulkFile(eq(bulkUpdateProcessDTO), Mockito.anyString(), eq(bulkUpdateProcessDTO.getFileName()));
    internalProcessServiceWrapper.preProcessBulkPriceRecommendationFile(bulkUpdateProcessDTO);

    Mockito.verify(fileStorageService)
        .createBulkFile(eq(bulkUpdateProcessDTO), Mockito.anyString(), eq(bulkUpdateProcessDTO.getFileName()));
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
  }

  @Test
  public void processInternalBulkRebateRequest() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRICE_REBATE_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_PRICE_REBATE_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_PRICE_REBATE_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfig);
    bulkInternalProcessData.setInternalProcessRequestId("Id");
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.BULK_PRICE_REBATE.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USERNAME,
        BulkInternalProcessType.BULK_PRICE_REBATE.getValue());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_REBATE.name());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(Mockito.any());
    Mockito.verify(internalProcessService).getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Collections.singletonList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(),
        Integer.parseInt(FETCH_BATCH_SIZE));
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(kafkaTopicProperties).getBulkPriceRebateUpload();
  }

  @Test
  public void processInternalBulkRebateRequest1() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRICE_REBATE_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_PRICE_REBATE_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_PRICE_REBATE_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.BULK_PRICE_REBATE.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, new ArrayList<>(),
        ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE))).thenReturn(new ArrayList<>());
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(1);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USERNAME,
        BulkInternalProcessType.BULK_PRICE_REBATE.getValue());
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, Collections.singletonList("request-id"),
            ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE));
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_REBATE.name());
  }

  @Test
  public void processBulkInternalBulkPriceUpdateEmptyDataTest() {
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, false);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
  }

  @Test
  public void processBulkInternalBulkPriceUpdateExceptionTest() throws Exception {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData))).thenReturn(
        Collections.singletonList(bulkInternalProcessData));
    Mockito.doThrow(Exception.class)
        .when(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE);
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, false);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Assertions.assertEquals(ProcessStatus.FAILED.name(),
        internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
  }

  @Test
  public void processBulkInternalBulkPriceInvalidProductSkuUpdateTest() throws Exception {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(null);
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, false);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(ERROR_MESSAGE, internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPriceMarkForDeleteProductSkuUpdateTest() throws Exception {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    basicProductResponse.setMarkForDelete(true);
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(basicProductResponse);
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, false);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(ERROR_MESSAGE, internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPriceArchivedProductSkuUpdateTest() throws Exception {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    basicProductResponse.setArchived(true);
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(basicProductResponse);
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, false);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(ERROR_MESSAGE, internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPriceTakeDownProductSkuUpdateTest() throws Exception {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    basicProductResponse.setTakenDown(true);
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(basicProductResponse);
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, false);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(ERROR_MESSAGE, internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPriceErrorWhileFetchL5ResponseTest() throws Exception {
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "internalItemPickupPointListFetchSize", 10);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(basicProductResponse);
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(0, pickupPointFilterRequest))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.doThrow(Exception.class).when(productLevel3BulkUpdateServiceBean)
        .getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request);
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, false);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Mockito.verify(objectMapper).readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class);
    Mockito.verify(productLevel3BulkUpdateServiceBean)
        .getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(0, pickupPointFilterRequest);
    Assertions.assertEquals(ProcessStatus.FAILED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(Constant.SYSTEM_ERROR, internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPriceTest() throws Exception {
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "internalItemPickupPointListFetchSize", 10);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(basicProductResponse);
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData);
    Mockito.when(
            productLevel3BulkUpdateServiceBean.getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request))
        .thenReturn(Collections.singletonList(itemPickupPointListingL3Response));
    Mockito.when(productLevel3BulkUpdateServiceBean.getPickUpPoints(profileResponse.getBusinessPartnerCode(),
        Collections.singleton(PICKUP_POINT))).thenReturn(Collections.singletonList(PICKUP_POINT));
    Mockito.when(productLevel3BulkUpdateServiceBean.getMinimumPrice(Constant.STORE_ID)).thenReturn(1000);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(0, pickupPointFilterRequest))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.when(bulkUpdateServiceUtil.validateExcelDatasBulkUpdateProduct(listMapArgumentCaptor.capture(),
        eq(Collections.singletonList(PICKUP_POINT)), eq(new ArrayList<>()), eq(new ArrayList<>()), Mockito.any(),
        eq(1000), eq(null), eq(false), eq(StringUtils.EMPTY))).thenAnswer(invocationOnMock -> {
      List<Map<String, String>> param = invocationOnMock.getArgument(2);
      Map<String, String> map = new HashMap<>();
      map.put(PRODUCT_CODE, PRODUCT_CODE);
      param.add(map);
      return null;
    });
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, false);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Mockito.verify(objectMapper).readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class);
    Mockito.verify(productLevel3BulkUpdateServiceBean)
        .getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(0, pickupPointFilterRequest);
    Mockito.verify(productLevel3BulkUpdateServiceBean).getMinimumPrice(Constant.STORE_ID);
    Mockito.verify(bulkUpdateServiceUtil).validateExcelDatasBulkUpdateProduct(listMapArgumentCaptor.capture(),
        eq(Collections.singletonList(PICKUP_POINT)), listMapArgumentCaptor.capture(), eq(new ArrayList<>()), Mockito.any(),
        eq(1000), eq(null), eq(false), eq(StringUtils.EMPTY));
    Mockito.verify(productLevel3BulkUpdateServiceBean)
        .processBulkUpdateL5(eq(profileResponse.getBusinessPartnerCode()), eq(RequestHelper.getDefaultPrivilegeMap()),
            eq(null), listMapArgumentCaptor.capture(), Mockito.any(), eq(false),
            eq(bulkInternalProcessData.getCreatedBy()),
            eq(Collections.singletonList(itemPickupPointListingL3Response)), eq(Constant.INTERNAL_BULK_UPDATE),
            eq(profileResponse));
    Assertions.assertEquals(ProcessStatus.FINISHED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertNull(internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPriceValidationFailureTest() throws Exception {
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "internalItemPickupPointListFetchSize", 10);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(basicProductResponse);
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData);
    Mockito.when(
            productLevel3BulkUpdateServiceBean.getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request))
        .thenReturn(Collections.singletonList(itemPickupPointListingL3Response));
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(0, pickupPointFilterRequest))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.when(productLevel3BulkUpdateServiceBean.getPickUpPoints(profileResponse.getBusinessPartnerCode(),
        Collections.singleton(PICKUP_POINT))).thenReturn(Collections.singletonList(PICKUP_POINT));
    BulkUpdateErrorDTO bulkUpdateErrorDTO = new BulkUpdateErrorDTO();
    bulkUpdateErrorDTO.setPickupPointCode(PICKUP_POINT);
    bulkUpdateErrorDTO.setProductSku(ITEM_SKU);
    bulkUpdateErrorDTO.setReason(MESSAGE);
    Mockito.when(productLevel3BulkUpdateServiceBean.getMinimumPrice(Constant.STORE_ID)).thenReturn(1000);
    Mockito.when(bulkUpdateServiceUtil.validateExcelDatasBulkUpdateProduct(listMapArgumentCaptor.capture(),
            eq(Collections.singletonList(PICKUP_POINT)), eq(new ArrayList<>()), eq(new ArrayList<>()), Mockito.any(),
            eq(1000), eq(null), eq(false), eq(StringUtils.EMPTY)))
        .thenReturn(Collections.singletonList(bulkUpdateErrorDTO));
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, false);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Mockito.verify(objectMapper).readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class);
    Mockito.verify(productLevel3BulkUpdateServiceBean)
        .getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(0, pickupPointFilterRequest);
    Mockito.verify(productLevel3BulkUpdateServiceBean).getMinimumPrice(Constant.STORE_ID);
    Mockito.verify(bulkUpdateServiceUtil).validateExcelDatasBulkUpdateProduct(listMapArgumentCaptor.capture(),
        eq(Collections.singletonList(PICKUP_POINT)), eq(new ArrayList<>()), eq(new ArrayList<>()), Mockito.any(),
        eq(1000), eq(null), eq(false), eq(StringUtils.EMPTY));
    Assertions.assertEquals(ProcessStatus.FAILED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(MESSAGE, internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPrice_withCampaignPriceTest() throws Exception {
    bulkPriceUpdateRequestData.setCampaignPrice(CAMPAIGN_PRICE);
    bulkPriceUpdateRequestData.setSalesPrice(SALES_PRICE);
    bulkPriceUpdateRequestData.setListPrice(LIST_PRICE);
    bulkPriceUpdateRequestData.setCampaignCode(CAMPAIGN_CODE);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT);
    itemPickupPointListingL3Response.setPrices(List.of(new ProductLevel3PriceResponse(null, 10000d, 9500d)));
    ProductLevel3ViewConfigResponse viewConfigResponseDefault = new ProductLevel3ViewConfigResponse();
    viewConfigResponseDefault.setChannelId("DEFAULT");
    viewConfigResponseDefault.setBuyable(true);
    viewConfigResponseDefault.setDisplay(true);
    ProductLevel3ViewConfigResponse viewConfigResponseCnc = new ProductLevel3ViewConfigResponse();
    viewConfigResponseCnc.setChannelId("CNC");
    viewConfigResponseCnc.setBuyable(true);
    viewConfigResponseCnc.setDisplay(true);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    itemPickupPointListingL3Response.getViewConfigs().add(viewConfigResponseDefault);
    itemPickupPointListingL3Response.getViewConfigs().add(viewConfigResponseCnc);
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "internalItemPickupPointListFetchSize", 10);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(basicProductResponse);
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData);
    Mockito.when(
            productLevel3BulkUpdateServiceBean.getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request))
        .thenReturn(Collections.singletonList(itemPickupPointListingL3Response));
    Mockito.when(productLevel3BulkUpdateServiceBean.getPickUpPoints(profileResponse.getBusinessPartnerCode(),
        Collections.singleton(PICKUP_POINT))).thenReturn(Collections.singletonList(PICKUP_POINT));
    Mockito.when(productLevel3BulkUpdateServiceBean.getMinimumPrice(Constant.STORE_ID)).thenReturn(1000);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(0, pickupPointFilterRequest))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.when(bulkUpdateServiceUtil.validateExcelDatasBulkUpdateProduct(listMapArgumentCaptor.capture(),
        eq(Collections.singletonList(PICKUP_POINT)), eq(new ArrayList<>()), eq(new ArrayList<>()), Mockito.any(),
        eq(1000), eq(null), eq(false), eq(StringUtils.EMPTY))).thenAnswer(invocationOnMock -> {
      List<Map<String, String>> param = invocationOnMock.getArgument(2);
      Map<String, String> map = new HashMap<>();
      map.put(PRODUCT_CODE, PRODUCT_CODE);
      param.add(map);
      return new ArrayList<>();
    });
    Mockito.when(priceAnalyticsOutboundService.getOfficerTaggedSkus(Set.of(ITEM_SKU + Constant.HYPHEN + PICKUP_POINT)))
        .thenReturn(Set.of(ITEM_SKU + Constant.HYPHEN + PICKUP_POINT));
    Mockito.when(campaignRepository.getCampaignProductDetailsV2(List.of(ItemDetailsDto.builder().itemSku(ITEM_SKU)
            .pickUpPointCode(PICKUP_POINT).build()), BulkAddCampaignProductQueue.builder().campaignCode(CAMPAIGN_CODE).build()))
        .thenReturn(List.of(CampaignProductDetailResponse.builder().itemSku(ITEM_SKU).pickUpPointCode(PICKUP_POINT).quota(10).build()));
    Mockito.when(campaignRepository.updateCampaignFinalPriceAndQuota(List.of(CampaignProductUpdateDto.builder().itemSku(ITEM_SKU)
        .itemPickupPointId(ITEM_SKU + Constant.HYPHEN + PICKUP_POINT).quota(10).finalPrice(9000d).originalSellingPrice(9500d)
        .sellingPrice(9500d).build()), CAMPAIGN_CODE)).thenReturn(new CampaignUpdateResponse(Map.of()));
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, true);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(3))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Mockito.verify(objectMapper, times(2)).readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class);
    Mockito.verify(productLevel3BulkUpdateServiceBean, times(2))
        .getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(0, pickupPointFilterRequest);
    Mockito.verify(productLevel3BulkUpdateServiceBean).getMinimumPrice(Constant.STORE_ID);
    Mockito.verify(bulkUpdateServiceUtil).validateExcelDatasBulkUpdateProduct(listMapArgumentCaptor.capture(),
        eq(Collections.singletonList(PICKUP_POINT)), listMapArgumentCaptor.capture(), eq(new ArrayList<>()), Mockito.any(),
        eq(1000), eq(null), eq(false), eq(StringUtils.EMPTY));
    Mockito.verify(productLevel3BulkUpdateServiceBean)
        .processBulkUpdateL5(eq(profileResponse.getBusinessPartnerCode()), eq(RequestHelper.getDefaultPrivilegeMap()),
            eq(new ArrayList<>()), listMapArgumentCaptor.capture(), Mockito.any(), eq(false),
            eq(bulkInternalProcessData.getCreatedBy()),
            eq(Collections.singletonList(itemPickupPointListingL3Response)), eq(Constant.INTERNAL_BULK_UPDATE),
            eq(profileResponse));
    Mockito.verify(priceAnalyticsOutboundService).getOfficerTaggedSkus(any());
    Mockito.verify(campaignRepository).getCampaignProductDetailsV2(List.of(ItemDetailsDto.builder().itemSku(ITEM_SKU)
        .pickUpPointCode(PICKUP_POINT).build()), BulkAddCampaignProductQueue.builder().campaignCode(CAMPAIGN_CODE).build());
    Mockito.verify(campaignRepository).updateCampaignFinalPriceAndQuota(List.of(CampaignProductUpdateDto.builder().itemSku(ITEM_SKU)
        .itemPickupPointId(ITEM_SKU + Constant.HYPHEN + PICKUP_POINT).quota(10).finalPrice(9000d).originalSellingPrice(9500d)
        .sellingPrice(9500d).build()), CAMPAIGN_CODE);
    Assertions.assertEquals(ProcessStatus.FINISHED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertNull(internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPrice_withCampaignPriceAndOfficerNotTaggedToSkusTest()
      throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(bulkInternalProcessDataList);
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(bulkInternalProcessDataList);
    Mockito.when(priceAnalyticsOutboundService.getOfficerTaggedSkus(Set.of(ITEM_SKU + Constant.HYPHEN + PICKUP_POINT)))
        .thenReturn(Set.of());
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData);
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, true);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(priceAnalyticsOutboundService).getOfficerTaggedSkus(any());
    Mockito.verify(internalProcessService, times(2)).saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(objectMapper).readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class);
    Assertions.assertEquals(ProcessStatus.FAILED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.SKU_ID_DOES_NOT_BELONG_TO_EMAIL_ADDRESS,
        internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPrice_withCampaignPriceErrorAndNoSellingPriceChangeTest() throws Exception {
    String itemSku = "TEB-24219-00001-00001";
    bulkPriceUpdateRequestData.setItemSku(itemSku);
    bulkPriceUpdateRequestData.setCampaignPrice(CAMPAIGN_PRICE);
    bulkPriceUpdateRequestData.setCampaignCode(CAMPAIGN_CODE);
    itemPickupPointListingL3Request.setItemSku(itemSku);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT);
    itemPickupPointListingL3Response.setItemSku(itemSku);
    itemPickupPointListingL3Response.setPrices(List.of(new ProductLevel3PriceResponse(null, 10000d, 9500d)));
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "internalItemPickupPointListFetchSize", 10);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(basicProductResponse);
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData);
    Mockito.when(
            productLevel3BulkUpdateServiceBean.getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request))
        .thenReturn(Collections.singletonList(itemPickupPointListingL3Response));
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(0, pickupPointFilterRequest))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.when(priceAnalyticsOutboundService.getOfficerTaggedSkus(Set.of(itemSku + Constant.HYPHEN + PICKUP_POINT)))
        .thenReturn(Set.of(itemSku + Constant.HYPHEN + PICKUP_POINT));
    Mockito.when(campaignRepository.getCampaignProductDetailsV2(List.of(ItemDetailsDto.builder().itemSku(itemSku)
            .pickUpPointCode(PICKUP_POINT).build()), BulkAddCampaignProductQueue.builder().campaignCode(CAMPAIGN_CODE).build()))
        .thenReturn(List.of());
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, true);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(3))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Mockito.verify(objectMapper, times(2)).readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class);
    Mockito.verify(productLevel3BulkUpdateServiceBean)
        .getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(0, pickupPointFilterRequest);
    Mockito.verify(priceAnalyticsOutboundService).getOfficerTaggedSkus(any());
    Mockito.verify(campaignRepository).getCampaignProductDetailsV2(List.of(ItemDetailsDto.builder().itemSku(itemSku)
        .pickUpPointCode(PICKUP_POINT).build()), BulkAddCampaignProductQueue.builder().campaignCode(CAMPAIGN_CODE).build());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.CAMPAIGN_PRICE_UPDATE_FAILURE
            + BulkProcessValidationErrorMessages.RECHECK_CAMPAIGN_CODE_ERROR,
        internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPrice_validationFailureAndCampaignPriceErrorTest() throws Exception {
    String itemSku = "TEB-24219-00001-00001";
    bulkPriceUpdateRequestData.setItemSku(itemSku);
    bulkPriceUpdateRequestData.setSalesPrice(SALES_PRICE);
    bulkPriceUpdateRequestData.setCampaignPrice(CAMPAIGN_PRICE);
    bulkPriceUpdateRequestData.setCampaignCode(CAMPAIGN_CODE);
    itemPickupPointListingL3Request.setItemSku(itemSku);
    itemPickupPointListingL3Response.setPrices(List.of(new ProductLevel3PriceResponse(null, 10000d, 9500d)));
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "internalItemPickupPointListFetchSize", 10);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(basicProductResponse);
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData);
    Mockito.when(
            productLevel3BulkUpdateServiceBean.getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request))
        .thenReturn(Collections.singletonList(itemPickupPointListingL3Response));
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(0, pickupPointFilterRequest))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.when(productLevel3BulkUpdateServiceBean.getPickUpPoints(profileResponse.getBusinessPartnerCode(),
        Collections.singleton(PICKUP_POINT))).thenReturn(Collections.singletonList(PICKUP_POINT));
    BulkUpdateErrorDTO bulkUpdateErrorDTO = new BulkUpdateErrorDTO();
    bulkUpdateErrorDTO.setPickupPointCode(PICKUP_POINT);
    bulkUpdateErrorDTO.setProductSku(itemSku);
    bulkUpdateErrorDTO.setReason(ERROR_MESSAGE);
    Mockito.when(productLevel3BulkUpdateServiceBean.getMinimumPrice(Constant.STORE_ID)).thenReturn(1000);
    Mockito.when(bulkUpdateServiceUtil.validateExcelDatasBulkUpdateProduct(listMapArgumentCaptor.capture(),
            eq(Collections.singletonList(PICKUP_POINT)), eq(new ArrayList<>()), eq(new ArrayList<>()), Mockito.any(),
            eq(1000), eq(null), eq(false), eq(StringUtils.EMPTY)))
        .thenReturn(Collections.singletonList(bulkUpdateErrorDTO));
    Mockito.when(priceAnalyticsOutboundService.getOfficerTaggedSkus(Set.of(itemSku + Constant.HYPHEN + PICKUP_POINT)))
        .thenReturn(Set.of(itemSku + Constant.HYPHEN + PICKUP_POINT));
    Mockito.when(campaignRepository.getCampaignProductDetailsV2(List.of(ItemDetailsDto.builder().itemSku(itemSku)
            .pickUpPointCode(PICKUP_POINT).build()), BulkAddCampaignProductQueue.builder().campaignCode(CAMPAIGN_CODE).build()))
        .thenReturn(List.of());
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, true);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(3))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Mockito.verify(objectMapper, times(2)).readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class);
    Mockito.verify(productLevel3BulkUpdateServiceBean, times(2))
        .getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(0, pickupPointFilterRequest);
    Mockito.verify(productLevel3BulkUpdateServiceBean).getMinimumPrice(Constant.STORE_ID);
    Mockito.verify(bulkUpdateServiceUtil).validateExcelDatasBulkUpdateProduct(listMapArgumentCaptor.capture(),
        eq(Collections.singletonList(PICKUP_POINT)), eq(new ArrayList<>()), eq(new ArrayList<>()), Mockito.any(),
        eq(1000), eq(null), eq(false), eq(StringUtils.EMPTY));
    Mockito.verify(priceAnalyticsOutboundService).getOfficerTaggedSkus(any());
    Mockito.verify(campaignRepository).getCampaignProductDetailsV2(List.of(ItemDetailsDto.builder().itemSku(itemSku)
        .pickUpPointCode(PICKUP_POINT).build()), BulkAddCampaignProductQueue.builder().campaignCode(CAMPAIGN_CODE).build());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.SELLING_PRICE_UPDATE_FAILURE + ERROR_MESSAGE
            + Constant.NEW_LINE + BulkProcessValidationErrorMessages.CAMPAIGN_PRICE_UPDATE_FAILURE
            + BulkProcessValidationErrorMessages.RECHECK_CAMPAIGN_CODE_ERROR,
        internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPrice_withCampaignPriceFailureAndSellingPriceSuccessTest() throws Exception {
    String itemSku = "TEB-24219-00001-00001";
    String itemPickupPointId = itemSku + Constant.HYPHEN + PICKUP_POINT;
    bulkPriceUpdateRequestData.setItemSku(itemSku);
    bulkPriceUpdateRequestData.setCampaignPrice(CAMPAIGN_PRICE);
    bulkPriceUpdateRequestData.setSalesPrice(SALES_PRICE);
    bulkPriceUpdateRequestData.setListPrice(LIST_PRICE);
    bulkPriceUpdateRequestData.setCampaignCode(CAMPAIGN_CODE);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT);
    itemPickupPointListingL3Request.setItemSku(itemSku);
    itemPickupPointListingL3Response.setItemSku(itemSku);
    itemPickupPointListingL3Response.setPrices(List.of(new ProductLevel3PriceResponse(null, 10000d, 9500d)));
    ProductLevel3ViewConfigResponse viewConfigResponseDefault = new ProductLevel3ViewConfigResponse();
    viewConfigResponseDefault.setChannelId("DEFAULT");
    viewConfigResponseDefault.setBuyable(true);
    viewConfigResponseDefault.setDisplay(true);
    ProductLevel3ViewConfigResponse viewConfigResponseCnc = new ProductLevel3ViewConfigResponse();
    viewConfigResponseCnc.setChannelId("CNC");
    viewConfigResponseCnc.setBuyable(true);
    viewConfigResponseCnc.setDisplay(true);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    itemPickupPointListingL3Response.getViewConfigs().add(viewConfigResponseDefault);
    itemPickupPointListingL3Response.getViewConfigs().add(viewConfigResponseCnc);
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "internalItemPickupPointListFetchSize", 10);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(basicProductResponse);
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData);
    Mockito.when(
            productLevel3BulkUpdateServiceBean.getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request))
        .thenReturn(Collections.singletonList(itemPickupPointListingL3Response));
    Mockito.when(productLevel3BulkUpdateServiceBean.getPickUpPoints(profileResponse.getBusinessPartnerCode(),
        Collections.singleton(PICKUP_POINT))).thenReturn(Collections.singletonList(PICKUP_POINT));
    Mockito.when(productLevel3BulkUpdateServiceBean.getMinimumPrice(Constant.STORE_ID)).thenReturn(1000);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(0, pickupPointFilterRequest))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.when(bulkUpdateServiceUtil.validateExcelDatasBulkUpdateProduct(listMapArgumentCaptor.capture(),
        eq(Collections.singletonList(PICKUP_POINT)), eq(new ArrayList<>()), eq(new ArrayList<>()), Mockito.any(),
        eq(1000), eq(null), eq(false), eq(StringUtils.EMPTY))).thenAnswer(invocationOnMock -> {
      List<Map<String, String>> param = invocationOnMock.getArgument(2);
      Map<String, String> map = new HashMap<>();
      map.put(PRODUCT_CODE, PRODUCT_CODE);
      param.add(map);
      return new ArrayList<>();
    });
    Mockito.when(productLevel3BulkUpdateServiceBean.processBulkUpdateL5(eq(profileResponse.getBusinessPartnerCode()),
            eq(RequestHelper.getDefaultPrivilegeMap()), eq(new ArrayList<>()), Mockito.any(), Mockito.any(), eq(false),
            eq(bulkInternalProcessData.getCreatedBy()), eq(Collections.singletonList(itemPickupPointListingL3Response)),
            eq(Constant.INTERNAL_BULK_UPDATE), eq(profileResponse)))
        .thenReturn(List.of(BulkUpdateSuccessDTO.builder().productSku(itemSku).pickupPointCode(PICKUP_POINT).build()));
    Mockito.when(priceAnalyticsOutboundService.getOfficerTaggedSkus(Set.of(itemPickupPointId)))
        .thenReturn(Set.of(itemPickupPointId));
    Mockito.when(campaignRepository.getCampaignProductDetailsV2(List.of(ItemDetailsDto.builder().itemSku(itemSku)
            .pickUpPointCode(PICKUP_POINT).build()), BulkAddCampaignProductQueue.builder().campaignCode(CAMPAIGN_CODE).build()))
        .thenReturn(List.of(CampaignProductDetailResponse.builder().itemSku(itemSku).pickUpPointCode(PICKUP_POINT).quota(10).build()));
    Mockito.when(campaignRepository.updateCampaignFinalPriceAndQuota(List.of(CampaignProductUpdateDto.builder().itemSku(itemSku)
        .itemPickupPointId(itemPickupPointId).quota(10).finalPrice(9000d).originalSellingPrice(9500d)
        .sellingPrice(9500d).build()), CAMPAIGN_CODE)).thenReturn(new CampaignUpdateResponse(Map.of(itemPickupPointId, ERROR_MESSAGE)));
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, true);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(3))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Mockito.verify(objectMapper, times(2)).readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class);
    Mockito.verify(productLevel3BulkUpdateServiceBean, times(2))
        .getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(0, pickupPointFilterRequest);
    Mockito.verify(productLevel3BulkUpdateServiceBean).getMinimumPrice(Constant.STORE_ID);
    Mockito.verify(bulkUpdateServiceUtil).validateExcelDatasBulkUpdateProduct(listMapArgumentCaptor.capture(),
        eq(Collections.singletonList(PICKUP_POINT)), listMapArgumentCaptor.capture(), eq(new ArrayList<>()), Mockito.any(),
        eq(1000), eq(null), eq(false), eq(StringUtils.EMPTY));
    Mockito.verify(productLevel3BulkUpdateServiceBean)
        .processBulkUpdateL5(eq(profileResponse.getBusinessPartnerCode()), eq(RequestHelper.getDefaultPrivilegeMap()),
            Mockito.anyList(), listMapArgumentCaptor.capture(), Mockito.any(), eq(false),
            eq(bulkInternalProcessData.getCreatedBy()),
            eq(Collections.singletonList(itemPickupPointListingL3Response)), eq(Constant.INTERNAL_BULK_UPDATE),
            eq(profileResponse));
    Mockito.verify(priceAnalyticsOutboundService).getOfficerTaggedSkus(any());
    Mockito.verify(campaignRepository).getCampaignProductDetailsV2(List.of(ItemDetailsDto.builder().itemSku(itemSku)
        .pickUpPointCode(PICKUP_POINT).build()), BulkAddCampaignProductQueue.builder().campaignCode(CAMPAIGN_CODE).build());
    Mockito.verify(campaignRepository).updateCampaignFinalPriceAndQuota(List.of(CampaignProductUpdateDto.builder().itemSku(itemSku)
        .itemPickupPointId(itemPickupPointId).quota(10).finalPrice(9000d).originalSellingPrice(9500d)
        .sellingPrice(9500d).build()), CAMPAIGN_CODE);
    Assertions.assertEquals(ProcessStatus.FAILED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.SELLING_PRICE_UPDATE_SUCCESS + Constant.NEW_LINE
            + BulkProcessValidationErrorMessages.CAMPAIGN_PRICE_UPDATE_FAILURE + ERROR_MESSAGE,
        internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPrice_withCampaignPriceSuccessAndSellingPriceFailuresTest() throws Exception {
    String itemSku = "TEB-24219-00001-00001";
    bulkPriceUpdateRequestData.setItemSku(itemSku);
    bulkPriceUpdateRequestData.setCampaignPrice(CAMPAIGN_PRICE);
    bulkPriceUpdateRequestData.setSalesPrice(SALES_PRICE);
    bulkPriceUpdateRequestData.setListPrice(LIST_PRICE);
    bulkPriceUpdateRequestData.setCampaignCode(CAMPAIGN_CODE);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT);
    itemPickupPointListingL3Request.setItemSku(itemSku);
    itemPickupPointListingL3Response.setItemSku(itemSku);
    itemPickupPointListingL3Response.setPrices(List.of(new ProductLevel3PriceResponse(null, 10000d, 9500d)));
    ProductLevel3ViewConfigResponse viewConfigResponseDefault = new ProductLevel3ViewConfigResponse();
    viewConfigResponseDefault.setChannelId("DEFAULT");
    viewConfigResponseDefault.setBuyable(true);
    viewConfigResponseDefault.setDisplay(true);
    ProductLevel3ViewConfigResponse viewConfigResponseCnc = new ProductLevel3ViewConfigResponse();
    viewConfigResponseCnc.setChannelId("CNC");
    viewConfigResponseCnc.setBuyable(true);
    viewConfigResponseCnc.setDisplay(true);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    itemPickupPointListingL3Response.getViewConfigs().add(viewConfigResponseDefault);
    itemPickupPointListingL3Response.getViewConfigs().add(viewConfigResponseCnc);
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "internalItemPickupPointListFetchSize", 10);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(basicProductResponse);
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData);
    Mockito.when(
            productLevel3BulkUpdateServiceBean.getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request))
        .thenReturn(Collections.singletonList(itemPickupPointListingL3Response));
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(0, pickupPointFilterRequest))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.when(productLevel3BulkUpdateServiceBean.getPickUpPoints(profileResponse.getBusinessPartnerCode(),
        Collections.singleton(PICKUP_POINT))).thenReturn(Collections.singletonList(PICKUP_POINT));
    BulkUpdateErrorDTO bulkUpdateErrorDTO = new BulkUpdateErrorDTO();
    bulkUpdateErrorDTO.setPickupPointCode(PICKUP_POINT);
    bulkUpdateErrorDTO.setProductSku(itemSku);
    bulkUpdateErrorDTO.setReason(ERROR_MESSAGE);
    Mockito.when(productLevel3BulkUpdateServiceBean.getMinimumPrice(Constant.STORE_ID)).thenReturn(1000);
    Mockito.when(bulkUpdateServiceUtil.validateExcelDatasBulkUpdateProduct(listMapArgumentCaptor.capture(),
            eq(Collections.singletonList(PICKUP_POINT)), eq(new ArrayList<>()), eq(new ArrayList<>()), Mockito.any(),
            eq(1000), eq(null), eq(false), eq(StringUtils.EMPTY)))
        .thenReturn(Collections.singletonList(bulkUpdateErrorDTO));
    Mockito.when(priceAnalyticsOutboundService.getOfficerTaggedSkus(Set.of(itemSku + Constant.HYPHEN + PICKUP_POINT)))
        .thenReturn(Set.of(itemSku + Constant.HYPHEN + PICKUP_POINT));
    Mockito.when(campaignRepository.getCampaignProductDetailsV2(List.of(ItemDetailsDto.builder().itemSku(itemSku)
            .pickUpPointCode(PICKUP_POINT).build()), BulkAddCampaignProductQueue.builder().campaignCode(CAMPAIGN_CODE).build()))
        .thenReturn(List.of(CampaignProductDetailResponse.builder().itemSku(itemSku).pickUpPointCode(PICKUP_POINT).quota(10).build()));
    Mockito.when(campaignRepository.updateCampaignFinalPriceAndQuota(List.of(CampaignProductUpdateDto.builder().itemSku(itemSku)
        .itemPickupPointId(itemSku + Constant.HYPHEN + PICKUP_POINT).quota(10).finalPrice(9000d).originalSellingPrice(9500d)
        .sellingPrice(9500d).build()), CAMPAIGN_CODE)).thenReturn(new CampaignUpdateResponse(Map.of()));
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, true);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(3))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Mockito.verify(objectMapper, times(2)).readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class);
    Mockito.verify(productLevel3BulkUpdateServiceBean, times(2))
        .getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(0, pickupPointFilterRequest);
    Mockito.verify(productLevel3BulkUpdateServiceBean).getMinimumPrice(Constant.STORE_ID);
    Mockito.verify(bulkUpdateServiceUtil).validateExcelDatasBulkUpdateProduct(listMapArgumentCaptor.capture(),
        eq(Collections.singletonList(PICKUP_POINT)), eq(new ArrayList<>()), eq(new ArrayList<>()), Mockito.any(),
        eq(1000), eq(null), eq(false), eq(StringUtils.EMPTY));
    Mockito.verify(priceAnalyticsOutboundService).getOfficerTaggedSkus(any());
    Mockito.verify(campaignRepository).getCampaignProductDetailsV2(List.of(ItemDetailsDto.builder().itemSku(itemSku)
        .pickUpPointCode(PICKUP_POINT).build()), BulkAddCampaignProductQueue.builder().campaignCode(CAMPAIGN_CODE).build());
    Mockito.verify(campaignRepository).updateCampaignFinalPriceAndQuota(List.of(CampaignProductUpdateDto.builder().itemSku(itemSku)
        .itemPickupPointId(itemSku + Constant.HYPHEN + PICKUP_POINT).quota(10).finalPrice(9000d).originalSellingPrice(9500d)
        .sellingPrice(9500d).build()), CAMPAIGN_CODE);
    Assertions.assertEquals(ProcessStatus.FAILED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.CAMPAIGN_PRICE_UPDATE_SUCCESS
            + Constant.NEW_LINE + BulkProcessValidationErrorMessages.SELLING_PRICE_UPDATE_FAILURE + ERROR_MESSAGE,
        internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPrice_withCampaignPriceExceptionTest() throws Exception {
    String itemSku = "TEB-24219-00001-00001";
    bulkPriceUpdateRequestData.setItemSku(itemSku);
    bulkPriceUpdateRequestData.setCampaignPrice(CAMPAIGN_PRICE);
    bulkPriceUpdateRequestData.setCampaignCode(CAMPAIGN_CODE);
    itemPickupPointListingL3Request.setItemSku(itemSku);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT);
    itemPickupPointListingL3Response.setItemSku(itemSku);
    itemPickupPointListingL3Response.setPrices(List.of(new ProductLevel3PriceResponse(null, 10000d, 9500d)));
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "internalItemPickupPointListFetchSize", 10);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(basicProductResponse);
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData);
    Mockito.when(
            productLevel3BulkUpdateServiceBean.getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request))
        .thenReturn(Collections.singletonList(itemPickupPointListingL3Response));
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(0, pickupPointFilterRequest))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.when(priceAnalyticsOutboundService.getOfficerTaggedSkus(Set.of(itemSku + Constant.HYPHEN + PICKUP_POINT)))
        .thenReturn(Set.of(itemSku + Constant.HYPHEN + PICKUP_POINT));
    Mockito.when(campaignRepository.getCampaignProductDetailsV2(List.of(ItemDetailsDto.builder().itemSku(itemSku)
            .pickUpPointCode(PICKUP_POINT).build()), BulkAddCampaignProductQueue.builder().campaignCode(CAMPAIGN_CODE).build()))
        .thenThrow(ApplicationRuntimeException.class);
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, true);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(3))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Mockito.verify(objectMapper, times(2)).readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class);
    Mockito.verify(productLevel3BulkUpdateServiceBean)
        .getItemPickupPointListingL3ResponseList(10, itemPickupPointListingL3Request);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(0, pickupPointFilterRequest);
    Mockito.verify(priceAnalyticsOutboundService).getOfficerTaggedSkus(any());
    Mockito.verify(campaignRepository).getCampaignProductDetailsV2(List.of(ItemDetailsDto.builder().itemSku(itemSku)
        .pickUpPointCode(PICKUP_POINT).build()), BulkAddCampaignProductQueue.builder().campaignCode(CAMPAIGN_CODE).build());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.CAMPAIGN_PRICE_UPDATE_FAILURE + Constant.SYSTEM_ERROR,
        internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPrice_withNoCampaignPriceChangeTest() throws Exception {
    String itemSku = "TEB-24219-00001-00001";
    bulkPriceUpdateRequestData.setItemSku(itemSku);
    bulkPriceUpdateRequestData.setCampaignCode(CAMPAIGN_CODE);
    itemPickupPointListingL3Request.setItemSku(itemSku);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT);
    itemPickupPointListingL3Response.setItemSku(itemSku);
    itemPickupPointListingL3Response.setPrices(List.of(new ProductLevel3PriceResponse(null, 10000d, 9500d)));
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "internalItemPickupPointListFetchSize", 10);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(basicProductResponse);
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(0, pickupPointFilterRequest))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.when(priceAnalyticsOutboundService.getOfficerTaggedSkus(Set.of(itemSku + Constant.HYPHEN + PICKUP_POINT)))
        .thenReturn(Set.of(itemSku + Constant.HYPHEN + PICKUP_POINT));
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, true);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(3))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Mockito.verify(objectMapper, times(2)).readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(0, pickupPointFilterRequest);
    Mockito.verify(priceAnalyticsOutboundService).getOfficerTaggedSkus(any());
    Assertions.assertEquals(ProcessStatus.FINISHED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertNull(internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkInternalBulkPrice_withNoCampaignCodeAndPriceTest() throws Exception {
    String itemSku = "TEB-24219-00001-00001";
    bulkPriceUpdateRequestData.setItemSku(itemSku);
    itemPickupPointListingL3Request.setItemSku(itemSku);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT);
    itemPickupPointListingL3Response.setItemSku(itemSku);
    itemPickupPointListingL3Response.setPrices(List.of(new ProductLevel3PriceResponse(null, 10000d, 9500d)));
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "internalItemPickupPointListFetchSize", 10);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(
            internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(), ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, SELLER_CODE))
        .thenReturn(profileResponse);
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode()))
        .thenReturn(basicProductResponse);
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(0, pickupPointFilterRequest))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.when(priceAnalyticsOutboundService.getOfficerTaggedSkus(Set.of(itemSku + Constant.HYPHEN + PICKUP_POINT)))
        .thenReturn(Set.of(itemSku + Constant.HYPHEN + PICKUP_POINT));
    internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, true);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
            internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(3))
        .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, bulkInternalProcessData.getParentCode());
    Mockito.verify(objectMapper, times(2)).readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(0, pickupPointFilterRequest);
    Mockito.verify(priceAnalyticsOutboundService).getOfficerTaggedSkus(any());
    Assertions.assertEquals(ProcessStatus.FINISHED.name(), internalProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertNull(internalProcessDataArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processBulkProductTypeTaggingUpdate_Success_Test() throws Exception {
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    bulkInternalProcessData.setInternalProcessRequestId(ID);
    Map<String, String> idXErrorMap = new HashMap<>();
    idXErrorMap.put(bulkInternalProcessData1.getId(), ERROR_MESSAGE);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
      INTERNAL_PROCESS_REQUEST_CODE, ProcessStatus.IN_PROGRESS.name())).thenReturn(bulkInternalProcessData1);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(STORE_ID,
        Collections.singletonList(REQUEST_ID), ProcessStatus.IN_PROGRESS.name()))
      .thenReturn(Collections.singletonList(bulkInternalProcessData1));
    Mockito.when(internalProcessService.saveInternalProcessData(
        Collections.singletonList(bulkInternalProcessData1)))
      .thenReturn(Collections.singletonList(bulkInternalProcessData1));
    BulkPriceProductTypeTaggingRequest request = new BulkPriceProductTypeTaggingRequest();
    request.setDeleteProductTypeTagging("yes");
    request.setItemSku(ITEM_SKU);
    Mockito.when(objectMapper.readValue(anyString(), eq(BulkPriceProductTypeTaggingRequest.class)))
      .thenReturn(request);
    Mockito.when(
      internalProcessService.updateRemoveBulkProductTypeTagging(Collections.singletonList(request),
        internalBulkUploadDataDomainEventModel.getUpdatedBy())).thenReturn(idXErrorMap);
      internalProcessServiceWrapper.processBulkProductTypeTaggingUpdate(internalBulkUploadDataDomainEventModel);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdInAndStatus(STORE_ID,
      Collections.singletonList(REQUEST_ID), ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(objectMapper).readValue(anyString(), eq(BulkPriceProductTypeTaggingRequest.class));
    Mockito.verify(internalProcessService)
      .updateRemoveBulkProductTypeTagging(Collections.singletonList(request),
        bulkInternalProcessData1.getCreatedBy());
    Mockito.verify(internalProcessService).saveInternalProcessData(Collections.singletonList(bulkInternalProcessData1));
  }

  @Test
  public void processBulkProductTypeTaggingUpdate_EmptyMessageSuccess_Test() throws Exception {
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    bulkInternalProcessData.setInternalProcessRequestId(ID);
    Map<String, String> idXErrorMap = new HashMap<>();
    idXErrorMap.put(bulkInternalProcessData1.getId(), null);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
      INTERNAL_PROCESS_REQUEST_CODE, ProcessStatus.IN_PROGRESS.name())).thenReturn(bulkInternalProcessData1);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(STORE_ID,
        Collections.singletonList(REQUEST_ID), ProcessStatus.IN_PROGRESS.name()))
      .thenReturn(Collections.singletonList(bulkInternalProcessData1));
    Mockito.when(internalProcessService.saveInternalProcessData(
        Collections.singletonList(bulkInternalProcessData1)))
      .thenReturn(Collections.singletonList(bulkInternalProcessData1));
    BulkPriceProductTypeTaggingRequest request = new BulkPriceProductTypeTaggingRequest();
    request.setDeleteProductTypeTagging("yes");
    request.setItemSku(ITEM_SKU);
    Mockito.when(objectMapper.readValue(anyString(), eq(BulkPriceProductTypeTaggingRequest.class)))
      .thenReturn(request);
    Mockito.when(
      internalProcessService.updateRemoveBulkProductTypeTagging(Collections.singletonList(request),
        internalBulkUploadDataDomainEventModel.getUpdatedBy())).thenReturn(idXErrorMap);
    internalProcessServiceWrapper.processBulkProductTypeTaggingUpdate(internalBulkUploadDataDomainEventModel);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdInAndStatus(STORE_ID,
      Collections.singletonList(REQUEST_ID), ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(objectMapper).readValue(anyString(), eq(BulkPriceProductTypeTaggingRequest.class));
    Mockito.verify(internalProcessService)
      .updateRemoveBulkProductTypeTagging(Collections.singletonList(request),
        bulkInternalProcessData1.getCreatedBy());
    Mockito.verify(internalProcessService).saveInternalProcessData(Collections.singletonList(bulkInternalProcessData1));
  }

  @Test
  public void processBulkProductTypeTaggingUpdate_NullBulkInternalProcessData_Test()
    throws JsonProcessingException {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(STORE_ID,
        Collections.singletonList(REQUEST_ID), ProcessStatus.IN_PROGRESS.name()))
      .thenReturn((Collections.emptyList()));
    internalProcessServiceWrapper.processBulkProductTypeTaggingUpdate(internalBulkUploadDataDomainEventModel);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdInAndStatus(STORE_ID,
      Collections.singletonList(REQUEST_ID), ProcessStatus.IN_PROGRESS.name());

  }

  @Test
  public void processBulkProductTypeTaggingUpdate_Exception_Test() throws Exception {
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    bulkInternalProcessData.setInternalProcessRequestId(ID);
    Mockito.when(internalProcessService.saveInternalProcessData(
        Collections.singletonList(bulkInternalProcessData1)))
      .thenReturn(Collections.singletonList(bulkInternalProcessData1));
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdInAndStatus(STORE_ID,
        Collections.singletonList(REQUEST_ID), ProcessStatus.IN_PROGRESS.name()))
      .thenReturn(Collections.singletonList(bulkInternalProcessData1));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(any(BulkInternalProcessData.class)))
      .thenReturn(bulkInternalProcessData1);
    BulkPriceProductTypeTaggingRequest request = new BulkPriceProductTypeTaggingRequest();
    request.setDeleteProductTypeTagging("yes");
    request.setItemSku(ITEM_SKU);
    Mockito.when(objectMapper.readValue(anyString(), eq(BulkPriceProductTypeTaggingRequest.class)))
      .thenReturn(request);
    Mockito.doThrow(new ApplicationRuntimeException()).when(internalProcessService)
      .updateRemoveBulkProductTypeTagging(Collections.singletonList(request),
        internalBulkUploadDataDomainEventModel.getUpdatedBy());

    try{
      internalProcessServiceWrapper.processBulkProductTypeTaggingUpdate(internalBulkUploadDataDomainEventModel);
    }
    finally {
      Mockito.verify(internalProcessService).updateRemoveBulkProductTypeTagging(Collections.singletonList(request),
        internalBulkUploadDataDomainEventModel.getUpdatedBy());
      Mockito.verify(internalProcessService).bulkInternalProcessDataByIdInAndStatus(STORE_ID,
        Collections.singletonList(REQUEST_ID), ProcessStatus.IN_PROGRESS.name());
      Mockito.verify(internalProcessService).bulkInternalProcessDataByIdInAndStatus(STORE_ID,
        Collections.singletonList(REQUEST_ID), ProcessStatus.IN_PROGRESS.name());
      Mockito.verify(objectMapper).readValue(anyString(), eq(BulkPriceProductTypeTaggingRequest.class));
      Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkInternalProcessData1);
    }


  }

  @Test
  public void processNewInternalProcessRequestBulkAddReviewIprProductsTest() throws IOException {
    getStoreCopyFiles(IPR_PRODUCT_BULK_ADD_REVIEW, BULK_ADD_REVIEW_IPR_PRODUCTS_FILE);
    bulkInternalProcess.setFileName(BULK_ADD_REVIEW_IPR_PRODUCTS_FILE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    XSSFWorkbook workBook;
    try(InputStream is = new FileInputStream(
        ProcessorUtils.DATA_BASE_DIR + IPR_PRODUCT_BULK_ADD_REVIEW + "/" + File.separator
            + INTERNAL_PROCESS_REQUEST_CODE + File.separator + BULK_ADD_REVIEW_IPR_PRODUCTS_FILE)) {
      workBook = new XSSFWorkbook(is);
      XSSFSheet sheet = workBook.getSheetAt(0);
      bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .fileName(bulkInternalProcess.getFileName())
          .bulkInternalProcessType(BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW)
          .relativePath(bulkInternalProcess.getFileName()).build();
      systemParameterConfig.setValue("1");
      Mockito.when(
              systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
          .thenReturn(systemParameterConfig);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
          SystemParameterConfigNames.IPR_REASONS)).thenReturn(systemParameterConfig);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
          SystemParameterConfigNames.IPR_ACTIONS)).thenReturn(systemParameterConfig);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
          SystemParameterConfigNames.IPR_SOURCE)).thenReturn(systemParameterConfig);
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
          SystemParameterConfigNames.IPR_VIOLATION_TYPES)).thenReturn(systemParameterConfig);
      Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
          any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
      Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
          .thenReturn(Arrays.asList(bulkInternalProcess));
      Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
          eq(ProcessStatus.PENDING.name()), any(Pageable.class),
          eq(BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name()))).thenReturn(
          new PageImpl<>(Arrays.asList(bulkInternalProcess), PageRequest.of(0, 1), TOTAL_COUNT));
      UserResponse userResponse = new UserResponse();
      userResponse.setUsername(SYSTEM);
      userResponseListBaseResponse.setContent(Collections.singletonList(userResponse));
      Mockito.when(partnersEngineOutboundService.userFilter(null, Constant.CREATED_DATE,
              VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, IPR_REVIEWER_CODES))
          .thenReturn(userResponseListBaseResponse);
      internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
          BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
      Mockito.verify(internalProcessService)
          .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(),
              PageRequest.of(0, 1), BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
      Mockito.verify(partnersEngineOutboundService).userFilter(null, Constant.CREATED_DATE,
          VendorProductDataBulkParameters.SORT_DIRECTION_ASC, 0, IPR_REVIEWER_CODES);
      Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
      Mockito.verify(internalProcessService)
          .saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
      Mockito.verify(internalProcessService, times(1))
          .saveInternalProcess(any(BulkInternalProcess.class));
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.IPR_ACTIONS);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.IPR_REASONS);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.IPR_SOURCE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.IPR_VIOLATION_TYPES);
    }
  }

  @Test
  public void processBulkIprAddReviewTest() throws JsonProcessingException {
    String jsonString =
        "{\"productName\":\"productName\",\"action\":\"IN_REVIEW\",\"productSku\":\"productSku\","
            + "\"rowNumber\":2}";
    bulkInternalProcessData.setData(jsonString);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
            INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(productDistributionTaskRepository.performIprAction(any(), any(),
        any(IprActionRequest.class))).thenReturn(new GdnBaseRestResponse());
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(),
        BulkAddReviewIPRProductsRequestData.class)).thenReturn(
        new BulkAddReviewIPRProductsRequestData("productSku", "productName", "IN_REVIEW", "", "", "",
            null, "","", 2, "","","","","","",""));
    internalProcessServiceWrapper.processIPRProductsBulkAddReviewEvent(STORE_ID, USERNAME,
        INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2)).saveBulkInternalProcessData(Mockito.any());
    Mockito.verify(objectMapper)
        .readValue(bulkInternalProcessData.getData(), BulkAddReviewIPRProductsRequestData.class);
  }

  @Test
  public void processBulkIprAddReviewErrorMessageTest()
      throws JsonProcessingException {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setErrorMessage(ERROR_MESSAGE);
    String jsonString =
        "{\"productName\":\"productName\",\"action\":\"IN_REVIEW\",\"productSku\":\"productSku\","
            + "\"rowNumber\":2}";
    bulkInternalProcessData.setData(jsonString);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
            INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(productDistributionTaskRepository.performIprAction(any(), any(),
        any(IprActionRequest.class))).thenReturn(response);
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(),
        BulkAddReviewIPRProductsRequestData.class)).thenReturn(
        new BulkAddReviewIPRProductsRequestData("productSku", "productName", "IN_REVIEW", "", "", "",
            null, "","", 2, "", "", "", "", "", "", ""));
    internalProcessServiceWrapper.processIPRProductsBulkAddReviewEvent(STORE_ID, USERNAME,
        INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2)).saveBulkInternalProcessData(Mockito.any());
    Mockito.verify(objectMapper)
        .readValue(bulkInternalProcessData.getData(), BulkAddReviewIPRProductsRequestData.class);
  }

  @Test
  public void processBulkIprAddReviewErrorMessageDetailTest()
      throws JsonProcessingException {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setErrorMessage(ERROR_MESSAGE);
    response.setSuccess(false);
    String jsonString =
        "{\"productName\":\"productName\",\"action\":\"IN_REVIEW\",\"productSku\":\"productSku\","
            + "\"rowNumber\":2}";
    bulkInternalProcessData.setData(jsonString);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
            INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    Mockito.doThrow(ApplicationRuntimeException.class).when(productDistributionTaskRepository).performIprAction(any(), any(),
        any(IprActionRequest.class));
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(),
        BulkAddReviewIPRProductsRequestData.class)).thenReturn(
        new BulkAddReviewIPRProductsRequestData("productSku", "productName", "IN_REVIEW", "", "", "",
            null,"", "", 2, "", "", "", "", "", "", ""));
    internalProcessServiceWrapper.processIPRProductsBulkAddReviewEvent(STORE_ID, USERNAME,
        INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2)).saveBulkInternalProcessData(Mockito.any());
    Mockito.verify(objectMapper)
        .readValue(bulkInternalProcessData.getData(), BulkAddReviewIPRProductsRequestData.class);
  }

  @Test
  public void processBulkIprAddReviewNullTest() throws JsonProcessingException {
    String jsonString =
        "{\"productName\":\"productName\",\"action\":\"IN_REVIEW\",\"productSku\":\"productSku\","
            + "\"rowNumber\":2}";
    bulkInternalProcessData.setData(jsonString);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
            INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(null);
    internalProcessServiceWrapper.processIPRProductsBulkAddReviewEvent(STORE_ID, USERNAME,
        INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(Mockito.any());
  }

  @Test
  public void processBulkIprAddReviewExceptionTest()
      throws JsonProcessingException {
    String jsonString =
        "{\"productName\":\"productName\",\"action\":\"IN_REVIEW\",\"productSku\":\"productSku\","
            + "\"rowNumber\":2}";
    bulkInternalProcessData.setData(jsonString);
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID,
            INTERNAL_PROCESS_REQUEST_ID, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    Mockito.doThrow(new RuntimeException()).when(productDistributionTaskRepository)
        .performIprAction(any(), any(), any(IprActionRequest.class));
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(),
        BulkAddReviewIPRProductsRequestData.class)).thenReturn(
        new BulkAddReviewIPRProductsRequestData("productSku", "productName", "IN_REVIEW", "", "", "",
            null, "", "", 2, "", "", "", "", "", "", ""));
    try {
      internalProcessServiceWrapper.processIPRProductsBulkAddReviewEvent(STORE_ID, USERNAME,
          INTERNAL_PROCESS_REQUEST_ID);
    } finally {
      Mockito.verify(internalProcessService)
          .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_ID,
              ProcessStatus.IN_PROGRESS.name());
      Mockito.verify(internalProcessService, times(2)).saveBulkInternalProcessData(Mockito.any());
      Mockito.verify(objectMapper)
          .readValue(bulkInternalProcessData.getData(), BulkAddReviewIPRProductsRequestData.class);
    }
  }

  @Test
  public void processNewInternalProcessRequest_BulkSkuLevelRebateTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_ROW_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
        .thenReturn(getSheetByInputPath("src/test/resources/SkuLevelRebate/SkuLevelRebate.xlsx"));
    internalProcessServiceWrapper
        .processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_ROW_BATCH_SIZE);
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
  }

  @Test
  public void processNewInternalProcessRequest_BulkSkuLevelRebateFailedTest() throws Exception {
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "bulkSkuLevelRebateMaxRows", 1);
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
    bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_ROW_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
                BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito.when(
            fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
        .thenReturn(getSheetByInputPath("src/test/resources/SkuLevelRebate/SkuLevelRebate.xlsx"));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
        BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_ROW_BATCH_SIZE);
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
    Mockito.verify(bulkSkuLevelRebateService)
        .sendEmailNotification(anyString(), anyString(), anyString(), anyString(), anyInt());
  }

  @Test
  public void processNewInternalProcessRequest_BulkSkuLevelRebateFailedEmptyFileTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_ROW_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito
        .when(fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
        .thenReturn(getSheetByInputPath("src/test/resources/SkuLevelRebate/SkuLevelRebate_EmptyFile.xlsx"));
    internalProcessServiceWrapper
        .processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_ROW_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(mailDeliveryService).sendEmailByForInternalBulkProcess(bulkInternalProcessArgumentCaptor.capture(),
        eq(ProcessStatus.FAILED.name()));
  }

  @Test
  public void processInternalProcessDataRequest_SkuLevelRebateTest() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_ROW_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfig);
    bulkInternalProcessData.setInternalProcessRequestId("Id");
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USERNAME,
        BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.getValue());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(Mockito.any());
    Mockito.verify(internalProcessService).getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Collections.singletonList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(),
        Integer.parseInt(FETCH_BATCH_SIZE));
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(kafkaTopicProperties).getBulkSkuLevelRebateUpload();
  }

  @Test
  public void processInternalProcessDataRequestTest_SkuLevelRebate() {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_ROW_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, new ArrayList<>(),
        ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE))).thenReturn(new ArrayList<>());
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(1);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USERNAME,
        BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.getValue());
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID, Collections.singletonList("request-id"),
            ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE));
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
  }

  @Test
  public void processBulkSkuLevelRebateUploadTest() throws JsonProcessingException {
    String jsonString = "{\"itemSku\":\"TEB-24219-00001-00001\",\"pickupPointCode\":\"PP-123456\",\"rebate\":\"10000\",\"rowNumber\":1}";
    bulkInternalProcessData.setData(jsonString);
    Mockito.when(internalProcessService
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_CODE, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(priceAnalyticsOutboundService.updateSkuRebate(Mockito.any())).thenReturn("error");
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkSkuLevelRebateRequestData.class))
        .thenReturn(new BulkSkuLevelRebateRequestData("TEB-24219-00001-00001", "PP-123456", "10000", 1));
    internalProcessServiceWrapper.processBulkSkuLevelRebateUpload(internalBulkUploadDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_CODE, ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2)).saveBulkInternalProcessData(Mockito.any());
    Mockito.verify(objectMapper).readValue(bulkInternalProcessData.getData(), BulkSkuLevelRebateRequestData.class);
  }


  @Test
  public void processBulkSkuLevelRebateUpload_nullResponseTest() throws JsonProcessingException {
    String jsonString = "{\"itemSku\":\"TEB-24219-00001-00001\",\"pickupPointCode\":\"PP-123456\",\"rebate\":\"10000\",\"rowNumber\":1}";
    bulkInternalProcessData.setData(jsonString);
    Mockito.when(internalProcessService
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_CODE, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(priceAnalyticsOutboundService.updateSkuRebate(Mockito.any())).thenReturn(null);
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkSkuLevelRebateRequestData.class))
        .thenReturn(new BulkSkuLevelRebateRequestData("TEB-24219-00001-00001", "PP-123456", "10000", 1));
    internalProcessServiceWrapper.processBulkSkuLevelRebateUpload(internalBulkUploadDataDomainEventModel);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_CODE, ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2)).saveBulkInternalProcessData(Mockito.any());
    Mockito.verify(objectMapper).readValue(bulkInternalProcessData.getData(), BulkSkuLevelRebateRequestData.class);
  }

  @Test
  public void processBulkSkuLevelRebateUpload_ExceptionTest() throws JsonProcessingException {
    String jsonString = "{\"itemSku\":\"TEB-24219-00001-00001\",\"pickupPointCode\":\"PP-123456\",\"rebate\":\"10000\",\"rowNumber\":1}";
    bulkInternalProcessData.setData(jsonString);
    Mockito.when(internalProcessService
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_CODE, ProcessStatus.IN_PROGRESS.name()))
        .thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    Mockito.doThrow(new RuntimeException()).when(priceAnalyticsOutboundService)
        .updateSkuRebate(Mockito.any());
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkSkuLevelRebateRequestData.class))
        .thenReturn(new BulkSkuLevelRebateRequestData("TEB-24219-00001-00001", "PP-123456", "10000", 1));
    try {
      internalProcessServiceWrapper.processBulkSkuLevelRebateUpload(internalBulkUploadDataDomainEventModel);
    } finally {
      Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_CODE, ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService, times(2)).saveBulkInternalProcessData(Mockito.any());
    Mockito.verify(objectMapper).readValue(bulkInternalProcessData.getData(), BulkSkuLevelRebateRequestData.class);
    }
  }

  @Test
  public void processBulkSkuLevelRebateUpload_InvalidBulkProcessDataTest() {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_CODE,
        ProcessStatus.IN_PROGRESS.name())).thenReturn(null);
    internalProcessServiceWrapper.processBulkSkuLevelRebateUpload(internalBulkUploadDataDomainEventModel);
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_REQUEST_CODE,
        ProcessStatus.IN_PROGRESS.name());
  }

  @Test
  public void processStatusUpdateForBulkSkuLevelRebateUploadTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_FINAL_UPDATE_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.doNothing().when(bulkSkuLevelRebateService)
        .setFinalStatusAndGenerateErrorFileForBulkSkuLevelRebateUpdate(STORE_ID, bulkInternalProcess);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_FINAL_UPDATE_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
    Mockito.verify(bulkSkuLevelRebateService)
        .setFinalStatusAndGenerateErrorFileForBulkSkuLevelRebateUpdate(STORE_ID, bulkInternalProcess);
  }

  @Test
  public void processNewInternalProcessRequest_BulkPriceUpdateNewTest() throws Exception {
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "bulkPriceUpdateNewMaxRows", 5);
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_ROW_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
        .thenReturn(getSheetByInputPath("src/test/resources/BulkPriceUpdateNew/BulkPriceUpdateNew.xlsx"));
    internalProcessServiceWrapper
        .processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_ROW_BATCH_SIZE);
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
  }

  @Test
  public void processNewInternalProcessRequest_BulkPriceUpdateNewFailedTest() throws Exception {
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "bulkPriceUpdateNewMaxRows", 1);
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_ROW_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
                BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito.when(
            fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
        .thenReturn(getSheetByInputPath("src/test/resources/BulkPriceUpdateNew/BulkPriceUpdateNew.xlsx"));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
        BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_ROW_BATCH_SIZE);
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    Mockito.verify(bulkPriceUpdateNewService)
        .sendEmailNotification(anyString(), anyString(), anyString(), anyString(), anyInt(), anyInt(), anyInt(), anyInt());
  }

  @Test
  public void processNewInternalProcessRequest_BulkPriceUpdateNewAndNoPriceChangeDetectedTest() throws Exception {
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "bulkPriceUpdateNewMaxRows", 5);
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_ROW_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
                BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito.when(
            fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
        .thenReturn(getSheetByInputPath("src/test/resources/BulkPriceUpdateNew/BulkPriceUpdateNew_NoPriceChange.xlsx"));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
        BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_ROW_BATCH_SIZE);
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    Mockito.verify(bulkPriceUpdateNewService)
        .sendEmailNotification(anyString(), anyString(), anyString(), anyString(), anyInt(), anyInt(), anyInt(), anyInt());
  }

  @Test
  public void processNewInternalProcessRequest_BulkPriceUpdateNewAndEmptyFileTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_ROW_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name()))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito
        .when(fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
        .thenReturn(getSheetByInputPath("src/test/resources/BulkPriceUpdateNew/BulkPriceUpdateNew_EmptyFile.xlsx"));
    internalProcessServiceWrapper
        .processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_ROW_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PENDING.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(bulkPriceUpdateNewService)
        .sendEmailNotification(anyString(), anyString(), anyString(), anyString(), anyInt(), anyInt(), anyInt(), anyInt());
  }

  @Test
  public void processInternalProcessDataRequestBulkPriceUpdateNewTest() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessMap = new HashMap<>();
    bulkInternalProcessMap.put(bulkInternalProcess.getId(), Arrays.asList(
        BulkInternalProcessPendingDataDTO.builder().internalProcessRequestId(bulkInternalProcess.getId())
            .processType(bulkInternalProcess.getProcessType()).build()));
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_TOTAL_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_ROW_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_FETCH_BATCH_SIZE))
        .thenReturn(systemParameterConfigForTotalBatchSize);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);
    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_ROW_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_TOTAL_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    Mockito.verify(internalProcessService).getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Collections.singletonList(bulkInternalProcessData.getId()), ProcessStatus.PENDING.name(), 10);
    Mockito.verify(internalProcessService).saveInternalProcesses(bulkInternalProcessListArgumentCaptor.capture());
    Mockito.verify(internalProcessService).saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBulkPriceUpdateNewEvent()),
        eq(bulkInternalProcessData.getParentCode() + bulkInternalProcess.getInternalProcessRequestCode()),
        internalBulkUploadDataDomainEventModelArgumentCaptor.capture());
    verify(kafkaTopicProperties, times(2)).getBulkPriceUpdateNewEvent();
    Assertions.assertEquals(REQUEST_ID,
        internalBulkUploadDataDomainEventModelArgumentCaptor.getValue().getInternalProcessDataRequestIdList().get(0));
    Assertions.assertEquals(1,
        internalBulkUploadDataDomainEventModelArgumentCaptor.getValue().getInternalProcessDataRequestIdList().size(), 0);
  }

  @Test
  public void processStatusUpdateForBulkPriceUpdateNewTest() throws Exception {
    systemParameterConfig.setValue(FILE_BATCH_SIZE);
    bulkInternalProcess.setId(GdnBaseEntity.ID);
    bulkInternalProcess.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_FINAL_UPDATE_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
                BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.doNothing().when(bulkPriceUpdateNewService)
        .setFinalStatusAndGenerateErrorFileForBulkNewPriceUpdate(STORE_ID, bulkInternalProcess);
    internalProcessServiceWrapper.processStatusUpdate(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_FINAL_UPDATE_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.PUBLISHED.name(), pageable,
            BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
    Mockito.verify(bulkPriceUpdateNewService)
        .setFinalStatusAndGenerateErrorFileForBulkNewPriceUpdate(STORE_ID, bulkInternalProcess);
  }

  @Test
  void testProcessInternalBrandUpdates() throws Exception {
    ReflectionTestUtils.setField(internalProcessServiceWrapper,"bulkBrandUpdateProductsFetchSize",2);
    InternalBrandUpdateNotes brandUpdateNotes = new InternalBrandUpdateNotes();
    brandUpdateNotes.setSourceBrandCode("BR-0002");
    brandUpdateNotes.setSourceBrandName("Brand-B");

    Mockito.when(objectMapper.readValue(anyString(), eq(InternalBrandUpdateNotes.class)))
        .thenReturn(brandUpdateNotes);

    bulkInternalProcess.setNotes("{\"destinationBrandCode\":\"BR-0002\",\"destinationBrandName\":\"Brand-B\"}");

    bulkInternalProcessData.setParentCode("PARENT-001");
    ProductAndBrandResponse productResponse = new ProductAndBrandResponse();
    productResponse.setProductCode("PROD-001");
    productResponse.setBusinessPartnerCodes(new HashSet<>(Arrays.asList("BP-001", "BP-002")));

    GdnRestListResponse<ProductAndBrandResponse> gdnResponse = new GdnRestListResponse<>();
    gdnResponse.setContent(Arrays.asList(productResponse));
    gdnResponse.setPageMetaData(new PageMetaData(1, 0, 2));

    Mockito.when(pbpOutboundService.getProductAndBrandResponseGdnRestListResponse(anyInt(), anyString(), anyString()))
        .thenReturn(gdnResponse);

    systemParameterConfig.setValue("10");

    bulkInternalProcess.setProcessType(BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.any(),
        Mockito.any())).thenReturn(systemParameterConfig);

    Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
            ProcessStatus.PENDING.name(), pageable, BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito.when(pcbOutboundService
            .getBrandAuthorisation(Mockito.any(),Mockito.any(),Mockito.any(),Mockito.any(),Mockito.any(),Mockito.any(),Mockito.any()))
        .thenReturn(new SimpleBooleanResponse(true));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name());
    Mockito.verify(internalProcessService).saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    List<BulkInternalProcessData> capturedData = internalProcessDataArgumentCaptor.getValue();
    verify(objectMapper, times(1))
        .readValue(anyString(), eq(InternalBrandUpdateNotes.class));
    verify(pbpOutboundService, times(1))
        .getProductAndBrandResponseGdnRestListResponse(anyInt(), anyString(), anyString());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(any(), any());
    verify(internalProcessService, times(1))
        .getAllBulkInternalProcessByStatus(eq(STORE_ID),
            eq(ProcessStatus.PENDING.name()), eq(pageable),
            eq(BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name()));
    verify(internalProcessService, times(1))
        .saveInternalProcesses(anyList());
    verify(internalProcessService)
        .saveInternalProcessData(any());

    verify(internalProcessService).saveInternalProcess(bulkInternalProcess);

  }

  @Test
  void testProcessInternalBrandNameUpdatesTest() throws Exception {
    ReflectionTestUtils.setField(internalProcessServiceWrapper,"bulkBrandUpdateProductsFetchSize",2);
    InternalBrandUpdateNotes brandUpdateNotes = new InternalBrandUpdateNotes();
    brandUpdateNotes.setSourceBrandCode("BR-0002");
    brandUpdateNotes.setSourceBrandName("Brand-B");

    Mockito.when(objectMapper.readValue(anyString(), eq(InternalBrandUpdateNotes.class)))
        .thenReturn(brandUpdateNotes);

    bulkInternalProcess.setNotes("{\"destinationBrandCode\":\"BR-0002\",\"destinationBrandName\":\"Brand-B\"}");

    bulkInternalProcessData.setParentCode("PARENT-001");
    ProductAndBrandResponse productResponse = new ProductAndBrandResponse();
    productResponse.setProductCode("PROD-001");
    productResponse.setBusinessPartnerCodes(new HashSet<>(Arrays.asList("BP-001", "BP-002")));

    GdnRestListResponse<ProductAndBrandResponse> gdnResponse = new GdnRestListResponse<>();
    gdnResponse.setContent(Arrays.asList(productResponse));
    gdnResponse.setPageMetaData(new PageMetaData(1, 0, 2));

    Mockito.when(pbpOutboundService.getProductAndBrandResponseGdnRestListResponse(anyInt(), anyString(), anyString()))
        .thenReturn(gdnResponse);

    systemParameterConfig.setValue("10");

    bulkInternalProcess.setProcessType(BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.any(),
        Mockito.any())).thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID,
            ProcessStatus.PENDING.name(), pageable, BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.saveInternalProcesses(Mockito.anyList()))
        .thenReturn(Arrays.asList(bulkInternalProcess));
    Mockito.when(pcbOutboundService
            .getBrandAuthorisation(Mockito.any(),Mockito.any(),Mockito.any(),Mockito.any(),Mockito.any(),Mockito.any(),Mockito.any()))
        .thenReturn(new SimpleBooleanResponse(true));
    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME, BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name());
    Mockito.verify(internalProcessService).saveInternalProcessData(internalProcessDataArgumentCaptor.capture());
    List<BulkInternalProcessData> capturedData = internalProcessDataArgumentCaptor.getValue();
    verify(objectMapper, times(1))
        .readValue(anyString(), eq(InternalBrandUpdateNotes.class));
    verify(pbpOutboundService, times(1))
        .getProductAndBrandResponseGdnRestListResponse(anyInt(), anyString(), anyString());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(any(), any());
    verify(internalProcessService, times(1))
        .getAllBulkInternalProcessByStatus(eq(STORE_ID),
            eq(ProcessStatus.PENDING.name()), eq(pageable),
            eq(BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name()));
    verify(internalProcessService, times(1))
        .saveInternalProcesses(anyList());
    verify(internalProcessService)
        .saveInternalProcessData(any());
    verify(internalProcessService).saveInternalProcess(bulkInternalProcess);
    Assertions.assertEquals(BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name(),capturedData.get(0).getData());
  }

  @Test
  void testProcessInternalBrandUpdates_BrandAuthorized() throws Exception {
    ReflectionTestUtils.setField(internalProcessServiceWrapper, "bulkBrandUpdateProductsFetchSize", 2);
    InternalBrandUpdateNotes brandUpdateNotes = new InternalBrandUpdateNotes();
    brandUpdateNotes.setSourceBrandCode("BR-0002");
    brandUpdateNotes.setSourceBrandName("Brand-B");

    Mockito.when(objectMapper.readValue(anyString(), eq(InternalBrandUpdateNotes.class)))
        .thenReturn(brandUpdateNotes);

    bulkInternalProcess.setNotes("{\"destinationBrandCode\":\"BR-0002\",\"destinationBrandName\":\"Brand-B\"}");
    bulkInternalProcess.setProcessType(BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name());

    ProductAndBrandResponse productResponse = new ProductAndBrandResponse();
    productResponse.setProductCode("PROD-001");
    productResponse.setBusinessPartnerCodes(new HashSet<>(Arrays.asList("BP-001")));

    GdnRestListResponse<ProductAndBrandResponse> gdnResponse = new GdnRestListResponse<>();
    gdnResponse.setContent(Collections.singletonList(productResponse));
    gdnResponse.setPageMetaData(new PageMetaData(1, 0, 10));

    Mockito.when(pbpOutboundService.getProductAndBrandResponseGdnRestListResponse(anyInt(), anyString(), anyString()))
        .thenReturn(gdnResponse);
    systemParameterConfig.setValue("10");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(any(), any()))
        .thenReturn(systemParameterConfig);

    Mockito.when(internalProcessService.getAllBulkInternalProcessByStatus(eq(STORE_ID),
        eq(ProcessStatus.PENDING.name()), eq(pageable),
        eq(BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name())))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));

    Mockito.when(internalProcessService.saveInternalProcesses(anyList()))
        .thenReturn(Collections.singletonList(bulkInternalProcess));
    ArgumentCaptor<List<BulkInternalProcessData>> captor = ArgumentCaptor.forClass(List.class);

    internalProcessServiceWrapper.processNewInternalProcessRequest(STORE_ID, USER_NAME,
        BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name());
    verify(internalProcessService,times(5)).saveInternalProcessData(captor.capture());
    List<BulkInternalProcessData> capturedData = captor.getValue();
    verify(objectMapper, times(1)).readValue(anyString(), eq(InternalBrandUpdateNotes.class));
    verify(pbpOutboundService, times(5)).getProductAndBrandResponseGdnRestListResponse(anyInt(), anyString(), anyString());
    verify(internalProcessService, times(1)).saveInternalProcesses(anyList());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(any(), any(), any(),
            any());
    verify(internalProcessService).saveInternalProcess(bulkInternalProcess);
  }

  @Test
  public void processInternalProcessDataRequest_InternalBrandUpdateTest() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    // Set notes with valid JSON for InternalBrandUpdateNotes
    bulkInternalProcessData.setNotes("{\"sourceBrandCode\":\"OLD123\",\"destinationBrandCode\":\"NEW123\",\"destinationBrandName\":\"New Brand\",\"destinationAttributeId\":\"ATTR123\"}");
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    systemParameterConfig.setValue("10");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_BRAND_UPDATE_DATA_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_BRAND_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);

    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BRAND_UPDATE_DATA_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BRAND_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkInternalProcessData);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name());
    Mockito.verify(internalProcessService).getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE));
    Mockito.verify(kafkaProducer).send(any(),
        Mockito.any());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(kafkaTopicProperties).getInternalBrandUpdateEvent();
  }

  @Test
  public void processInternalProcessDataRequest_InternalBrandNameUpdateTest() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    // Set notes with valid JSON for InternalBrandUpdateNotes
    bulkInternalProcessData.setNotes("{\"sourceBrandCode\":\"OLD123\",\"destinationBrandCode\":\"NEW123\",\"destinationBrandName\":\"New Brand Name\",\"destinationAttributeId\":\"ATTR123\"}");
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    systemParameterConfig.setValue("10");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_BRAND_NAME_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_BRAND_NAME_UPDATE_DATA_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
        .thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);

    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BRAND_NAME_UPDATE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BRAND_NAME_UPDATE_DATA_BATCH_SIZE);
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkInternalProcessData);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name());
    Mockito.verify(internalProcessService).getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE));
    Mockito.verify(kafkaProducer).send(any(),
        Mockito.any());
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
    Mockito.verify(kafkaTopicProperties).getInternalBrandUpdateEvent();
  }

  @Test
  public void processInternalProcessDataRequest_InternalBrandUpdateBulkInternalProcessDatasTest() {
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(FETCH_BATCH_SIZE);
    SystemParameterConfig systemParameterConfigForTotalBatchSize = new SystemParameterConfig();
    systemParameterConfigForTotalBatchSize.setValue(TOTAL_BATCH_SIZE);
    bulkInternalProcessData.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);
    Mockito.when(internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(0);
    systemParameterConfig.setValue("10");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_BRAND_UPDATE_DATA_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.INTERNAL_BRAND_UPDATE_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
                BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name()))
        .thenReturn(new PageImpl<>(Arrays.asList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
            Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE)))
        .thenReturn(new ArrayList<>());
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData))
        .thenReturn(bulkInternalProcessData);

    internalProcessServiceWrapper.processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USER_NAME,
        BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name());

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BRAND_UPDATE_DATA_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.INTERNAL_BRAND_UPDATE_BATCH_SIZE);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessByStatus(STORE_ID, ProcessStatus.IN_PROGRESS.name(), pageable,
            BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name());
    Mockito.verify(internalProcessService).getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Arrays.asList(INTERNAL_PROCESS_REQUEST_ID), ProcessStatus.PENDING.name(), Integer.parseInt(FETCH_BATCH_SIZE));
    Mockito.verify(internalProcessService)
        .getCountByStoreIdAndStatusAndInternalProcessRequestId(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(internalProcessService).saveInternalProcesses(Mockito.anyList());
  }

  @Test
  void testProcessInternalBrandUpdateEvent_Success() throws Exception {
    // Given
    InternalBrandUpdateEventModel eventModel = InternalBrandUpdateEventModel.builder()
        .storeId(STORE_ID)
        .oldBrandCode("BRAND123")
        .processType(BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name())
        .updatedBy(USER_NAME)
        .internalProcessDataRequestId("REQ123")
        .build();

    BulkInternalProcessData mockProcessData = new BulkInternalProcessData();
    mockProcessData.setId("DATA123");
    mockProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());

    GdnBaseRestResponse successResponse = new GdnBaseRestResponse();
    successResponse.setSuccess(true);

    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(
        STORE_ID, "REQ123", ProcessStatus.IN_PROGRESS.name())).thenReturn(mockProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Collections.singletonList(mockProcessData));
    Mockito.when(pbpOutboundService.updateProductBrandName(Mockito.eq(eventModel), Mockito.any(BulkInternalProcessData.class))).thenReturn(successResponse);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(Mockito.any()))
        .thenReturn(mockProcessData);

    // When
    internalProcessServiceWrapper.processInternalBrandUpdateEvent(eventModel);

    // Then
    Mockito.verify(pbpOutboundService).updateProductBrandName(Mockito.eq(eventModel), Mockito.any(BulkInternalProcessData.class));
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(
        STORE_ID, "REQ123", ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService).saveInternalProcessData(
        Collections.singletonList(mockProcessData));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(Mockito.any());
  }

  @Test
  void testProcessInternalBrandUpdateEvent_Failure() throws Exception {
    // Given
    InternalBrandUpdateEventModel eventModel = InternalBrandUpdateEventModel.builder()
        .storeId(STORE_ID)
        .oldBrandCode("BRAND123")
        .processType(BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name())
        .updatedBy(USER_NAME)
        .internalProcessDataRequestId("REQ123")
        .build();

    BulkInternalProcessData mockProcessData = new BulkInternalProcessData();
    mockProcessData.setId("DATA123");
    mockProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());

    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(
        STORE_ID, "REQ123", ProcessStatus.IN_PROGRESS.name())).thenReturn(mockProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Collections.singletonList(mockProcessData));
    Mockito.when(pbpOutboundService.updateProductBrandName(Mockito.eq(eventModel), Mockito.any(BulkInternalProcessData.class)))
        .thenThrow(new RuntimeException("API call failed"));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(Mockito.any()))
        .thenReturn(mockProcessData);

    // When & Then - Should not throw exception, just log it
    Assertions.assertDoesNotThrow(() -> internalProcessServiceWrapper.processInternalBrandUpdateEvent(eventModel));

    // Verify that the process data was still saved even after failure
    Mockito.verify(internalProcessService).saveInternalProcessData(
        Collections.singletonList(mockProcessData));
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(
        STORE_ID, "REQ123", ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(Mockito.any());
  }

  @Test
  void testProcessInternalBrandUpdateEvent_FailureWithErrorMessage() throws Exception {
    // Given
    InternalBrandUpdateEventModel eventModel = InternalBrandUpdateEventModel.builder()
        .storeId(STORE_ID)
        .oldBrandCode("BRAND123")
        .processType(BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name())
        .updatedBy(USER_NAME)
        .internalProcessDataRequestId("REQ123")
        .build();
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setErrorMessage("Error Message");

    BulkInternalProcessData mockProcessData = new BulkInternalProcessData();
    mockProcessData.setId("DATA123");
    mockProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());

    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(
        STORE_ID, "REQ123", ProcessStatus.IN_PROGRESS.name())).thenReturn(mockProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Collections.singletonList(mockProcessData));
    Mockito.when(pbpOutboundService.updateProductBrandName(Mockito.eq(eventModel), Mockito.any(BulkInternalProcessData.class)))
        .thenReturn(response);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(Mockito.any()))
        .thenReturn(mockProcessData);

    internalProcessServiceWrapper.processInternalBrandUpdateEvent(eventModel);

    // Verify that the process data was still saved even after failure
    Mockito.verify(internalProcessService).saveInternalProcessData(
        Collections.singletonList(mockProcessData));
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(
        STORE_ID, "REQ123", ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(Mockito.any());
  }

  @Test
  void testProcessInternalBrandUpdateEvent_NoDataFound() throws Exception {
    // Given
    InternalBrandUpdateEventModel eventModel = InternalBrandUpdateEventModel.builder()
        .storeId(STORE_ID)
        .oldBrandCode("BRAND123")
        .processType(BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name())
        .updatedBy(USER_NAME)
        .internalProcessDataRequestId("REQ123")
        .build();

    // Mock returns null - no data found
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(
        STORE_ID, "REQ123", ProcessStatus.IN_PROGRESS.name())).thenReturn(null);

    // When
    internalProcessServiceWrapper.processInternalBrandUpdateEvent(eventModel);

    // Then
    // Verify that only the lookup method was called
    Mockito.verify(internalProcessService).bulkInternalProcessDataByIdAndStatus(
        STORE_ID, "REQ123", ProcessStatus.IN_PROGRESS.name());

    // Verify that no other methods were called since data was null
    Mockito.verify(internalProcessService, Mockito.never()).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(pbpOutboundService, Mockito.never()).updateProductBrandName(Mockito.any(), Mockito.any());
    Mockito.verify(internalProcessService, Mockito.never()).saveBulkInternalProcessData(Mockito.any());
  }

}
