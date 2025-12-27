package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.time.DateUtils;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.BrandAndCategoryPredictionRequest;
import com.gdn.mta.bulk.dto.product.BulkVideoDownloadRequestEventModel;
import com.gdn.mta.bulk.entity.BulkProcessImageQC;
import com.gdn.mta.bulk.entity.BulkProcessVideo;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.BulkDownloadEntityStatus;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkActivityStatus;
import com.gdn.mta.bulk.dto.BulkPendingProductResponse;
import com.gdn.mta.bulk.dto.BulkPendingProductsDTO;
import com.gdn.mta.bulk.dto.BulkPendingRequestsResponse;
import com.gdn.mta.bulk.dto.BulkProcessDataDTO;
import com.gdn.mta.bulk.dto.BulkProcessNotesResponse;
import com.gdn.mta.bulk.dto.BulkProcessStatusListingResponse;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.GenericTemplateFileType;
import com.gdn.mta.bulk.dto.QRCodeExcelQueue;
import com.gdn.mta.bulk.dto.QrCodeRowInfo;
import com.gdn.mta.bulk.dto.QrCodeRowItemInfo;
import com.gdn.mta.bulk.dto.QrExcelUploadRequest;
import com.gdn.mta.bulk.dto.RowNumberParentCodeDTO;
import com.gdn.mta.bulk.dto.SystemParameterConfigRequest;
import com.gdn.mta.bulk.dto.WholeSaleCount;
import com.gdn.mta.bulk.dto.WholeSaleCountResponse;
import com.gdn.mta.bulk.dto.product.BulkImageDownloadEventModel;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.mta.bulk.entity.BulkCreateProductEventModel;
import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessDataEstimation;
import com.gdn.mta.bulk.entity.BulkProcessImage;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.entity.Configuration;
import com.gdn.mta.bulk.entity.KafkaEventLog;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.entity.WorkOrderEventModel;
import com.gdn.mta.bulk.entity.constants.BulkProcessConstant;
import com.gdn.mta.bulk.models.BulkDataDeletionModel;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.models.ImageDownloadResult;
import com.gdn.mta.bulk.repository.BulkDownloadAuditRepository;
import com.gdn.mta.bulk.repository.BulkProcessCustomRepository;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.service.download.BulkFailedProductFileService;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.GenericBulkParameters;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.notification.queue.NotificationQueue;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.model.vo.UnmappedSkuResponse;
import com.gdn.x.product.rest.web.model.response.ItemL5ListingResponse;
import com.gdn.x.product.rest.web.model.response.SimpleListStringResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInfo;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.CollectionUtils;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.text.ParseException;
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
import java.util.UUID;

import static com.gdn.mta.bulk.dto.BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD;
import static com.gdn.mta.bulk.dto.BulkProcessType.EXTERNAL_CREATION_UPLOAD;
import static com.gdn.mta.bulk.dto.BulkProcessType.QR_GENERATION;
import static com.gdn.mta.bulk.models.EmailConstants.FILE_PREFIX;
import static com.gdn.mta.bulk.models.EmailConstants.PRODUCT_CENTER_DOWNLOAD_ID;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public class BulkProcessServiceBeanTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String REQUEST_ID_2 = "requestId2";
  private static final String CATEGORY_NAME11 = "C11";
  private static final String CATEGORY_NAME1 = "C1";
  private static final String CATEGORY_NAME2 = "C2";
  private static final String CATEGORY_NAME12 = "C12";
  private static final String CATEGORY_NAME111 = "C111";
  private static final String CATEGORY_NAME2_3 = "C2-3 Name";
  private static final String CATEGORY_NAME3_3 = "C3-3 Name";
  private static final String CATEGORY_CODE11 = "CategoryCode11";
  private static final String CATEGORY_CODE1 = "CategoryCode1";
  private static final String CATEGORY_CODE2_3 = "C2-3 Code";
  private static final String CATEGORY_CODE3_3 = "C3-3 Code";
  private static final String CATEGORY_CODE2 = "CategoryCode2";
  private static final String ATTRIBUTE_ID1 = "AttributeId1";
  private static final String ATTRIBUTE_ID2 = "AttributeId2";
  private static final String ATTRIBUTE_ID3 = "AttributeId3";
  private static final String ATTRIBUTE_ID4 = "AttributeId4";
  private static final String ATTRIBUTE_ID5 = "AttributeId5";
  private static final String ATTRIBUTE_ID6 = "AttributeId6";
  private static final String ATTRIBUTE_CODE1 = "AttributeCode1";
  private static final String ATTRIBUTE_CODE2 = "AttributeCode2";
  private static final String ATTRIBUTE_CODE3 = "AttributeCode3";
  private static final String ATTRIBUTE_CODE4 = "AttributeCode4";
  private static final String ATTRIBUTE_CODE5 = "AttributeCode5";
  private static final String ATTRIBUTE_CODE6 = "AttributeCode6";
  private static final String ATTRIBUTE_NAME1 = "AttributeName1";
  private static final String ATTRIBUTE_NAME2 = "AttributeName2";
  private static final String ATTRIBUTE_NAME3 = "AttributeName3";
  private static final String ATTRIBUTE_NAME4 = "AttributeName4";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String RANDOM_VALUE_1 = "RANDOM1";
  private static final String RANDOM_VALUE_2 = "RANDOM2";
  private static final String ENGLISH = "-English";
  private static final String DEFAULT_BULK_PROCESS_CODE = "defaultBulkProcessCode";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00106";
  private static final String PRIMARY_IDENTIFIER = "primaryIdentifier";
  private static final Pageable DEFAULT_PAGEABLE = PageRequest.of(0, 10);
  private static final VerificationMode AT_LEAST_ONE = times(1);
  private static final VerificationMode AT_LEAST_NONE = times(0);
  private static final String DEFAULT_BULK_UPDATE_STATE = "PENDING";
  private static final long COUNT_TRUE = 0;
  private static final long COUNT_FALSE = 1;
  private static final String BULK_UPDATE_MERCHANT_CODE = "BLI-00106";
  private static final String DEFAULT_FOLDER = "target/x-bulk/";
  private static final String CREATE_CAMPAIGN_PRODUCT = "createCampaignProduct/";
  private static final String DOWNLOAD_CAMPAIGN_PRODUCT = "downloadCampaignProduct/";
  private static final String DOWNLOAD_PRODUCTS = "downloadProducts/";
  private static final String DOWNLOAD_MASTER_PRODUCTS = "downloadMasterProducts/";
  private static final String DOWNLOAD_FAILED_PRODUCTS = "downloadFailedProducts/";
  private static final String DOWNLOAD_PRODUCT_VENDORS = "downloadProductVendors/";
  private static final String DOWNLOAD_ORDERS = "downloadOrders/";
  private static final String DOWNLOAD_PRODUCT_REVIEW = "downloadProductReview/";
  private static final String BULK_UPDATE_DIR = "productUpdate/";
  private static final String TEST_FOLDER = "testFolder/";
  private static final String TEST_FILE = "testFile.txt";
  private static final String DEFAULT_SERVICE_KEY = "x_bulk";
  private static final int DEFAULT_AGE = 0;
  private static final String DEFAULT_BULK_NOTES = "default notes";
  private static final boolean GENERIC_TEMPLATE_ELIGIBLE = true;
  private static final String CATEGORY_ID = "categoryId";
  private static final ClassLoader CLASS_LOADER = ClassLoader.getSystemClassLoader();
  private static final String BASE_DIRECTORY = CLASS_LOADER.getResource(StringUtils.EMPTY).getPath();
  private static final String GENERIC_TEMPLATE_1_PATH =
      BASE_DIRECTORY + "/ExcelTemplate//Blibli-mass-upload-template-1.xlsm";
  private static final String GENERIC_TEMPLATE_2_PATH =
      BASE_DIRECTORY + "/ExcelTemplate//Blibli-mass222.xlsm";
  private static final String GENERIC_TEMPLATE_3_PATH =
      BASE_DIRECTORY + "/ExcelTemplate//Blibli-mass-upload-template.xlsm";
  private static final String CATEGORY_ATTRIBUTE_SHEET = "inputs";
  private static final String DEFAULT_PATH = "path";
  private static final int CATEGORY_ATTRIBUTE_SHEET_INDEX = 6;
  private static final String UNIFIED_TEMPLATE_UPLOAD_PATH =
      BASE_DIRECTORY + "/ExcelTemplate/Blibli-mass-upload-template.xlsm";
  private static final String CATEGORY_C1_C12_MAPPING = "C1->C12";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String PRODUCT_NAME = "product-name";
  private static final String CATALOG_CODE = "12501";
  private static final String THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS_VALUE = "1";
  private static final String MAILS_FOR_PENDING_BULK_REQUESTS_VALUE = "temp@gmail.com";
  private static final String BUSINESS_PARTNER = "CODE";
  private static final String PARENT_PRODUCT = "parentProduct";
  private static final String BULK_PROCESS_TYPE = "BULK_PROCESS_TYPE";
  private static final Date DATE = new Date();
  private static final String NULL = "NULL";
  private static final Boolean IS_BULK_UPDATE = true;
  private static final String IMAGE_URL =
      "https://www.static-src.com/wcsstore/Indraprastha/images/catalog/full//97/MTA-31184287/fashion_original-timberland-men-s-outdoor-waterproof-shoes-martin-boots-work-boots-t031_full03.jpg";
  private static final String UPLOADED_URL = "UPLOADED_URL";
  private static final String IMAGE_NAME = "image0.jpg";
  private static final String DEFAULT_DIRECTORY_PATH = "dirpath";
  private static final String FAILED_MESSAGE_LINK = "link";
  private static final String DEFAULT_NOTIFICATION_EXCHANGE = "exchange";
  private static final String DEFAULT_NOTIFICATION_ROUTING_KEY = "routingKey";
  private static final String PRODUCT_LEVEL3_PROCESSOR_DIR = "ProductLevel3Processor";
  private static final String BATCH_SIZE = "10";
  private static final String STATUS = "PENDING";
  public static final String ITEM_SKU = "itemSku-itemSku";
  private static final String PRODUCT_SKU_1 = "HIC-60001-00003";
  private static final String BULK_GENERIC_CREATE_PRODUCT = "com.gdn.bulk.generic.create.product";
  private static final String BULK_GENERIC_CREATE_PRODUCT_PRIORITY_1 = "com.gdn.bulk.generic.create.product.priority.1";
  private static final String BULK_GENERIC_CREATE_PRODUCT_PRIORITY_2 = "com.gdn.bulk.generic.create.product.priority.2";
  private static final String BULK_CN_CREATE_PRODUCT_EVENT = "com.gdn.bulk.cn.create.product";
  private static final String BULK_CN_CREATE_PRODUCT_EVENT_PRIORITY_1 = "com.gdn.bulk.cn.create.product";
  private static final String BULK_CN_CREATE_PRODUCT_EVENT_PRIORITY_2 = "com.gdn.bulk.cn.create.product";
  private static final String CREATION_BULK_PROCESS_TYPE = BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue();
  private static final String QR_GENERATION_TYPE = QR_GENERATION.getValue();
  private static final String CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_1 =
    BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue();
  private static final String CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_2 =
    BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue();
  private static final String PRODUCT_BASIC_INFO =
      BulkProcessType.PRODUCT_BASIC_INFO.getValue();
  private static final String PRODUCT_BASIC_INFO_PRIORITY_1 =
      BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue();
  private static final String PRODUCT_BASIC_INFO_PRIORITY_2 =
      BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue();
  private static final String CAMPAIGN_BULK_PROCESS_TYPE = BulkProcessType.CAMPAIGN.getValue();
  private static final String CAMPAIGN_DOWNLOAD = BulkProcessEntity.CAMPAIGN_PRODUCT.name();
  private static final String IN_PROGRESS = "IN_PROGRESS";
  private static final String PUBLISHED = "PUBLISHED";
  private static final String STATUS_LIST = "IN_PROGRESS,PUBLISHED";
  private RowNumberParentCodeDTO rowNumberParentCodeDTO;
  private RowNumberParentCodeDTO rowNumberParentCodeDTONew;
  RowNumberParentCodeDTO rowNumberParentCodeDTO3;
  private List<RowNumberParentCodeDTO> rowNumberParentCodeDTOList = new ArrayList<>();
  private List<RowNumberParentCodeDTO> rowNumberParentCodeDTOList1 = new ArrayList<>();
  private ObjectMapper mapper = new ObjectMapper();
  private static final String deletePickupPointEventType = "DeletePickupPoint";
  private static final String DEFAULT_PICKUP_POINT_CODE = "pp-code";
  private static final String QR_UPLOAD_EXCEL_PATH = "src/test/resources/QrExcelUpload/%s.xlsx";
  private static final String WORKORDER_EXCEL_PATH = "src/test/resources/Workorder/disassembly_request_template.xlsx";
  private static final String QR_CODE_EXCEL_LIMIT = "10";
  private static final String L5_ID = ITEM_SKU + "-" + DEFAULT_PICKUP_POINT_CODE;
  private static final String CAMPAIGN_CODE = "CAMP-12345";
  private static final String BULK_CREATE_DOWNLOAD_IMAGE_EVENT = "com.gdn.bulk.create.download.image";
  public static final String START_DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS";
  private static final String PURE_DELIVERY_FILE = "PURE_DELIVERY_FILE";
  private static final String CNC_FILE = "CNC_FILE";
  private static final String BFB_NON_CNC_FILE = "BFB_NON_CNC_FILE";
  private static final String BFB_CNC_FILE = "BFB_CNC_FILE";
  private static final String PURE_DELIVERY_BUNDLING_FILE = "PURE_DELIVERY_BUNDLING_FILE";
  private static final String CNC_BUNDLING_FILE = "CNC_BUDNLING_FILE";
  private static final String BFB_NON_CNC_BUNDLING_FILE = "BFB_NON_CNC_BUNDLING_FILE";
  private static final String BFB_CNC_BUNDLING_FILE = "BFB_CNC_BUNDLING_FILE";
  private static final String PURE_DELIVERY_INSTORE_FILE = "PURE_DELIVERY_INSTORE_FILE";
  private static final String CNC_INSTORE_FILE = "CNC_INSTORE_FILE";
  private static final String BFB_NON_CNC_INSTORE_FILE = "BFB_NON_CNC_INSTORE_FILE";
  private static final String BFB_CNC_INSTORE_FILE = "BFB_CNC_INSTORE_FILE";
  private static final String PURE_DELIVERY_BUNDLING_INSTORE_FILE = "PURE_DELIVERY_BUNDLING_INSTORE_FILE";
  private static final String CNC_BUNDLING_INSTORE_FILE = "CNC_BUDNLING_INSTORE_FILE";
  private static final String BFB_NON_CNC_BUNDLING_INSTORE_FILE = "BFB_NON_CNC_BUNDLING_INSTORE_FILE";
  private static final String BFB_CNC_BUNDLING_INSTORE_FILE = "BFB_CNC_BUNDLING_INSTORE_FILE";
  public static final String GENERIC_FILE_TYPE_LIST =
      String.join(",", PURE_DELIVERY_FILE, CNC_FILE, BFB_NON_CNC_FILE, BFB_CNC_FILE, PURE_DELIVERY_BUNDLING_FILE,
          CNC_BUNDLING_FILE, BFB_NON_CNC_BUNDLING_FILE, BFB_CNC_BUNDLING_FILE, PURE_DELIVERY_INSTORE_FILE,
          CNC_INSTORE_FILE, BFB_NON_CNC_INSTORE_FILE, BFB_CNC_INSTORE_FILE, PURE_DELIVERY_BUNDLING_INSTORE_FILE,
          CNC_BUNDLING_INSTORE_FILE, BFB_NON_CNC_BUNDLING_INSTORE_FILE, BFB_CNC_BUNDLING_INSTORE_FILE);



  private List<String> bulkProcessCodes = new ArrayList<>();
  private Configuration DEFAULT_CONFIGURATION = new Configuration();
  private List<Configuration> configurations = new ArrayList<>();
  private Page<Configuration> configurationResponse;
  private BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
  private List<CategoryTreeResponse> genericTemplateCategoryResponseList;
  private CategoryDetailResponse categoryDetailResponse;
  private CategoryAttributeResponse categoryAttributeResponse1;
  private CategoryAttributeResponse categoryAttributeResponse2;
  private CategoryAttributeResponse categoryAttributeResponse3;
  private CategoryAttributeResponse categoryAttributeResponse4;
  private CategoryAttributeResponse categoryAttributeResponse5;
  private CategoryAttributeResponse categoryAttributeResponse6;
  private AttributeResponse attributeResponse1;
  private AttributeResponse attributeResponse2;
  private AttributeResponse attributeResponse3;
  private AttributeResponse attributeResponse4;
  private AttributeResponse attributeResponse5;
  private AttributeResponse attributeResponse6;
  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse1;
  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse2;
  private AllowedAttributeValueResponse allowedAttributeValueResponse1;
  private AllowedAttributeValueResponse allowedAttributeValueResponse2;
  private List<KafkaEventLog> kafkaEventLogList;
  private List<CategoryTreeResponse> genericTemplateCategories = new ArrayList<>();
  private List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueResponses = new ArrayList<>();
  private List<BrandWipResponse> inReviewBrandsResponse = new ArrayList<>();
  private AttributeResponse attributeResponse7;
  private List<AttributeResponse> attributeResponseList;
  private List<UnmappedSkuResponse> unmappedSkuResponses;
  private SystemParameterConfig thresholdPendingBulkRequests;
  private SystemParameterConfig thresholdPendingBulkRequestsFailedValue;
  private SystemParameterConfig mailTo;
  private SystemParameterConfig thresholdAbortedBulkRequests;
  private SystemParameterConfig abortedRequestsRequired;
  private SystemParameterConfig bulkProcessDataBatchSize;
  private List<BulkPendingProductsDTO> bulkPendingProductsDTOListNull;
  private List<BulkPendingProductsDTO> bulkPendingProductsDTOList;
  private List<BulkPendingProductResponse> bulkPendingProductResponseList;
  private Map<String, Boolean> stringBooleanMap = new HashMap<>();
  private BulkProcessImage bulkProcessImage;
  private BulkProcessVideo bulkProcessVideo;
  private BulkProcessImageQC bulkProcessImageQC;
  private BulkProcessData bulkProcessData1;
  private RowNumberParentCodeDTO rowNumberItemSkuDTO;
  private RowNumberParentCodeDTO rowNumberItemSkuDTO1;
  private XSSFWorkbook workbook;
  private QrExcelUploadRequest qrExcelUploadRequest = new QrExcelUploadRequest();
  private DownloadQRCodeRequest downloadQRCodeRequest = new DownloadQRCodeRequest();
  private BulkUpdateProcessDTO bulkUpdateProcessDTO = new BulkUpdateProcessDTO();
  private CompanyDTO companyDTO = new CompanyDTO();
  private ProfileResponse profileResponse = new ProfileResponse();
  private BulkProcess qrCodeBulkProcess = new BulkProcess();
  private QRCodeExcelQueue qrCodeExcelQueue = new QRCodeExcelQueue();
  private SystemParameterConfig qrCodeSystemParameter = new SystemParameterConfig();
  private QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
  private QrCodeRowItemInfo qrCodeRowItemInfo = new QrCodeRowItemInfo();
  private ItemL5ListingResponse itemL5ListingResponse = new ItemL5ListingResponse();

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private BulkProcessServiceBean bulkProcessServiceBean;

  @Mock
  private BulkProcessRepository bulkProcessRepository;

  @Mock
  private BulkDownloadAuditRepository bulkDownloadAuditRepository;

  @Mock
  private BulkProcessCustomRepository bulkProcessCustomRepository;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ConfigurationService configurationService;

  @Mock
  private PCBOutboundService pcbOutboundService;

  @Mock
  private PBPOutboundService pbpOutboundService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private KafkaEventLogService kafkaEventLogService;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private MailDeliveryService mailDeliveryService;

  @Mock
  private BulkProcessImageService bulkProcessImageService;

  @Mock
  private BulkProcessVideoService bulkProcessVideoService;

  @Mock
  private BulkProcessImageQCService bulkProcessImageQCService;

  @Mock
  private POIUtil poiUtil;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private NotificationService notificationService;

  @Mock
  private InternalProcessService internalProcessService;

  @Captor
  private ArgumentCaptor<BulkProcess> bulkProcessCaptor;

  @Captor
  private ArgumentCaptor<List<BulkProcess>> bulkProcessListCaptor;

  @Captor
  private ArgumentCaptor<SystemParameterConfigRequest> systemParameterConfigRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<BulkPendingProductResponse>> bulkPendingProductResponseArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<BulkProcessImage>> bulkProcessImageArgumentCaptor;

  private ClassLoader classLoader;

  @Mock
  private BulkFailedProductFileService bulkFailedProductFileService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private PickupPointDeleteService pickupPointDeleteService;

  @Captor
  private ArgumentCaptor<NotificationQueue> notificationQueueArgumentCaptor;

  @Mock
  private BulkProcessDataEstimationService bulkProcessDataEstimationService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private KafkaTopicProperties topicProperties;

  @Captor
  private ArgumentCaptor<List<BulkProcessData>> bulkProcessDataListArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<String>> statusListArgumentCapture;


  @AfterEach
  public void finalizeTest() {
    Mockito.verifyNoMoreInteractions(configurationService);
    Mockito.verifyNoMoreInteractions(systemParameterConfigService);
    Mockito.verifyNoMoreInteractions(pcbOutboundService);
    Mockito.verifyNoMoreInteractions(kafkaEventLogService);
    Mockito.verifyNoMoreInteractions(pbpOutboundService);
    Mockito.verifyNoMoreInteractions(mailDeliveryService);
    verifyNoMoreInteractions(bulkProcessImageService);
    verifyNoMoreInteractions(bulkProcessDataService);
    verifyNoMoreInteractions(bulkFailedProductFileService, businessPartnerRepository);
    verifyNoMoreInteractions(bulkProcessCustomRepository);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(pickupPointDeleteService);
    verifyNoMoreInteractions(bulkDownloadAuditRepository, bulkProcessDataEstimationService);
    verifyNoMoreInteractions(notificationService);
    verifyNoMoreInteractions(topicProperties);
    verifyNoMoreInteractions(internalProcessService);
  }

  private List<BulkProcess> getBulkProcess() throws Exception {
    Date date = Calendar.getInstance().getTime();
    List<BulkProcess> bulkProcess = new ArrayList<BulkProcess>();
    bulkProcess.add(
        new BulkProcess(DEFAULT_BULK_PROCESS_CODE, "ProductLevel3", DEFAULT_BUSINESS_PARTNER_CODE, null, null,
            BulkProcess.STATUS_PENDING, null, new ArrayList<BulkProcessNotes>()));
    bulkProcess.add(
        new BulkProcess(UUID.randomUUID().toString(), "ProductLevel3", DEFAULT_BUSINESS_PARTNER_CODE, date, null,
            BulkProcess.STATUS_IN_PROGRESS, null, new ArrayList<BulkProcessNotes>()));
    for (BulkProcess bulkProcessItem : bulkProcess) {
      bulkProcessItem.setInternationalMerchant(false);
      bulkProcessItem.setStoreId(DEFAULT_STORE_ID);
      bulkProcessItem.setCreatedBy(DEFAULT_USERNAME);
      bulkProcessItem.setCreatedDate(date);
      bulkProcessItem.setUpdatedBy(DEFAULT_USERNAME);
      bulkProcessItem.setUpdatedDate(date);
      bulkProcessItem.setUploadedFile(FILE_PREFIX);
      bulkProcessItem.setTotalCount(100);
      bulkProcessItem.setErrorCount(1);
      bulkProcessItem.setSuccessCount(99);
      bulkProcessItem.setNotes("{}");
    }
    return bulkProcess;
  }

  private List<BulkDownloadEntity> getDownloadEntities(){
    List<BulkDownloadEntity> bulkDownloadEntities = new ArrayList<>();
    bulkDownloadEntities.add(
        new BulkDownloadEntity());
    for(BulkDownloadEntity bulkDownloadEntity : bulkDownloadEntities){
      bulkDownloadEntity.setId(123L);
      bulkDownloadEntity.setBusinessPartnerCode(BUSINESS_PARTNER);
      bulkDownloadEntity.setStatus(BulkProcessConstant.DOWNLOAD_SUCCESS);
      bulkDownloadEntity.setEntityType(CAMPAIGN_DOWNLOAD);
      bulkDownloadEntity.setPrimaryIdentifier(PRIMARY_IDENTIFIER);
      bulkDownloadEntity.setRecordsDownload(100);
    }
    return bulkDownloadEntities;
  }

  private BulkProcess getBulkProcessWithWholeSaleConfig() throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setDescription(
        DEFAULT_PATH + " selesai diunggah. Sukses: 28 of 28  produk(Produk akan direview maks 5 hari kerja)");
    WholeSaleCount wholeSaleCount = new WholeSaleCount(10, 10, 10, "path");
    BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
    bulkProcessNotes.setMarkForDelete(false);
    bulkProcessNotes.setNotes(new ObjectMapper().writeValueAsString(wholeSaleCount));
    bulkProcessNotes.setWholeSaleConfig(true);
    bulkProcess.setBulkProcessNotes(Arrays.asList(bulkProcessNotes));
    return bulkProcess;
  }

  private BulkProcess getBulkProcess_1() throws Exception {
    Date date = Calendar.getInstance().getTime();
    BulkProcess bulkProcess =
        new BulkProcess(DEFAULT_BULK_PROCESS_CODE, "ProductLevel3", DEFAULT_BUSINESS_PARTNER_CODE, date, date,
            BulkProcess.STATUS_FINISHED, null, new ArrayList<BulkProcessNotes>());
    bulkProcess.setStoreId(DEFAULT_STORE_ID);
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setCreatedDate(date);
    bulkProcess.setUpdatedBy(DEFAULT_USERNAME);
    bulkProcess.setUpdatedDate(date);
    return bulkProcess;
  }

  private List<BulkProcess> getBulkProcess_externalCreation() {
    Date date = Calendar.getInstance().getTime();
    BulkProcess bulkProcess =
        new BulkProcess(DEFAULT_BULK_PROCESS_CODE, BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue(), DEFAULT_BUSINESS_PARTNER_CODE, date, date,
            BulkProcess.STATUS_READY_TO_PROCESS, null, new ArrayList<BulkProcessNotes>());
    bulkProcess.setStoreId(DEFAULT_STORE_ID);
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setCreatedDate(date);
    bulkProcess.setUpdatedBy(DEFAULT_USERNAME);
    bulkProcess.setUpdatedDate(date);
    return List.of(bulkProcess);
  }

  public BulkProcessRepository getBulkProcessRepository() {
    return bulkProcessRepository;
  }

  public BulkProcessServiceBean getBulkProcessServiceBean() {
    return bulkProcessServiceBean;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    setMdcParameters();
    MockitoAnnotations.initMocks(this);
    classLoader = getClass().getClassLoader();
    bulkProcessServiceBean = new BulkProcessServiceBean();
    bulkProcessServiceBean.setBulkProcessRepository(bulkProcessRepository);
    bulkProcessServiceBean.setConfigurationService(configurationService);
    bulkProcessServiceBean.setSystemParameterConfigService(systemParameterConfigService);
    bulkProcessServiceBean.setPcbOutboundService(pcbOutboundService);
    bulkProcessServiceBean.setKafkaEventLogService(kafkaEventLogService);
    bulkProcessServiceBean.setPbpOutboundService(pbpOutboundService);
    bulkProcessServiceBean.setPcbOutboundService(pcbOutboundService);
    bulkProcessServiceBean.setXProductOutboundService(xProductOutboundService);
    bulkProcessServiceBean.setMailDeliveryService(mailDeliveryService);
    bulkProcessServiceBean.setBulkProcessDataService(bulkProcessDataService);
    bulkProcessServiceBean.setBusinessPartnerRepository(businessPartnerRepository);
    bulkProcessServiceBean.setBulkFailedProductFileService(bulkFailedProductFileService);
    bulkProcessServiceBean.setBulkProcessImageService(bulkProcessImageService);
    bulkProcessServiceBean.setPoiUtil(poiUtil);
    bulkProcessServiceBean.setBulkProcessImageService(bulkProcessImageService);
    bulkProcessServiceBean.setBulkProcessDataService(bulkProcessDataService);
    bulkProcessServiceBean.setBulkProcessCustomRepository(bulkProcessCustomRepository);
    bulkProcessServiceBean.setKafkaProducer(kafkaProducer);
    bulkProcessServiceBean.setPickupPointDeleteService(pickupPointDeleteService);
    bulkProcessServiceBean.setBulkProcessDataEstimationService(bulkProcessDataEstimationService);
    bulkProcessServiceBean.setFileStorageService(fileStorageService);
    bulkProcessServiceBean.setKafkaTopicProperties(topicProperties);
    bulkProcessServiceBean.setNotificationService(notificationService);
    bulkProcessServiceBean.setBulkProcessVideoService(bulkProcessVideoService);
    bulkProcessServiceBean.setBulkProcessImageQCService(bulkProcessImageQCService);
    bulkProcessServiceBean.setInternalProcessService(internalProcessService);
    bulkProcessServiceBean.setObjectMapper(objectMapper);
    bulkProcessServiceBean.setBulkDownloadAuditRepository(bulkDownloadAuditRepository);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxBulkRequests", 1);
    bulkProcessCodes.add(DEFAULT_BULK_PROCESS_CODE);
    DEFAULT_CONFIGURATION.setAgeOfDeletion(DEFAULT_AGE);
    DEFAULT_CONFIGURATION.setDisabled(false);
    DEFAULT_CONFIGURATION.setFolderName(DEFAULT_FOLDER);
    DEFAULT_CONFIGURATION.setServiceKey(DEFAULT_SERVICE_KEY);
    configurations.add(DEFAULT_CONFIGURATION);
    configurationResponse = new PageImpl<>(configurations);
    bulkProcessNotes.setPromoNote(false);
    bulkProcessNotes.setNotes(DEFAULT_BULK_NOTES);
    bulkProcessNotes.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);

    genericTemplateCategoryResponseList = new ArrayList<>();
    CategoryTreeResponse categoryTreeResponse = new CategoryTreeResponse();

    categoryTreeResponse.setCategoryName(CATEGORY_NAME11);
    categoryTreeResponse.setCategoryEnglishName(CATEGORY_NAME11 + ENGLISH);
    categoryTreeResponse.setCategoryCode(CATEGORY_CODE11);
    CategoryTreeResponse categoryTreeResponse11 = new CategoryTreeResponse();
    categoryTreeResponse11.setCategoryName(CATEGORY_NAME111);
    categoryTreeResponse11.setCategoryEnglishName(CATEGORY_NAME111 + ENGLISH);
    categoryTreeResponse11.setCategoryCode(CATEGORY_CODE1);
    List<CategoryTreeResponse> categoryTreeResponseListL3 = new ArrayList<>();
    categoryTreeResponseListL3.add(categoryTreeResponse11);
    categoryTreeResponse.setChildren(categoryTreeResponseListL3);

    CategoryTreeResponse categoryTreeResponse1 = new CategoryTreeResponse();
    categoryTreeResponse1.setCategoryName(CATEGORY_NAME12);
    categoryTreeResponse1.setCategoryEnglishName(CATEGORY_NAME12 + ENGLISH);
    categoryTreeResponse1.setCategoryCode(CATEGORY_CODE2);

    CategoryTreeResponse categoryTreeResponse2_3 = new CategoryTreeResponse();
    categoryTreeResponse2_3.setCategoryName(CATEGORY_NAME2_3);
    categoryTreeResponse2_3.setCategoryEnglishName(CATEGORY_NAME3_3 + ENGLISH);
    categoryTreeResponse2_3.setCategoryCode(CATEGORY_CODE2_3);

    CategoryTreeResponse categoryTreeResponse3_3 = new CategoryTreeResponse();
    categoryTreeResponse3_3.setCategoryName(CATEGORY_NAME3_3);
    categoryTreeResponse3_3.setCategoryEnglishName(CATEGORY_NAME3_3 + ENGLISH);
    categoryTreeResponse3_3.setCategoryCode(CATEGORY_CODE3_3);

    List<CategoryTreeResponse> categoryTreeResponseList2_3_child = new ArrayList<>();
    categoryTreeResponseList2_3_child.add(categoryTreeResponse3_3);

    categoryTreeResponse2_3.setChildren(categoryTreeResponseList2_3_child);

    List<CategoryTreeResponse> categoryTreeResponseList = new ArrayList<>();
    categoryTreeResponseList.add(categoryTreeResponse1);
    categoryTreeResponseList.add(categoryTreeResponse);
    categoryTreeResponseList.add(categoryTreeResponse2_3);

    CategoryTreeResponse genericTemplateCategoryResponse0 = new CategoryTreeResponse();
    genericTemplateCategoryResponse0.setCategoryName(CATEGORY_NAME1);
    genericTemplateCategoryResponse0.setCategoryEnglishName(CATEGORY_NAME1 + ENGLISH);
    genericTemplateCategoryResponse0.setCategoryCode(CATEGORY_CODE1);
    CategoryTreeResponse genericTemplateCategoryResponse1 = new CategoryTreeResponse();
    genericTemplateCategoryResponse1.setCategoryName(CATEGORY_NAME2);
    genericTemplateCategoryResponse1.setCategoryEnglishName(CATEGORY_NAME2 + ENGLISH);
    genericTemplateCategoryResponse1.setCategoryCode(CATEGORY_CODE2);
    genericTemplateCategoryResponse0.setChildren(categoryTreeResponseList);
    genericTemplateCategoryResponseList.add(genericTemplateCategoryResponse0);
    genericTemplateCategoryResponseList.add(genericTemplateCategoryResponse1);

    predefinedAllowedAttributeValueResponse1 = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse1.setValue(RANDOM_VALUE_1);

    predefinedAllowedAttributeValueResponse2 = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse2.setValue(RANDOM_VALUE_2);

    allowedAttributeValueResponse1 = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse1.setValue(RANDOM_VALUE_1);

    allowedAttributeValueResponse2 = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse2.setValue(RANDOM_VALUE_2);

    attributeResponse1 = new AttributeResponse();
    attributeResponse1.setId(ATTRIBUTE_ID1);
    attributeResponse1.setAttributeCode(ATTRIBUTE_CODE1);
    attributeResponse1.setName(ATTRIBUTE_NAME1);
    attributeResponse1.setNameEnglish(ATTRIBUTE_NAME1 + ENGLISH);
    attributeResponse1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    attributeResponse1.setPredefinedAllowedAttributeValues(
        Arrays.asList(predefinedAllowedAttributeValueResponse1, predefinedAllowedAttributeValueResponse2));
    attributeResponse1
        .setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse1, allowedAttributeValueResponse2));

    attributeResponse2 = new AttributeResponse();
    attributeResponse2.setId(ATTRIBUTE_ID2);
    attributeResponse2.setAttributeCode(ATTRIBUTE_CODE2);
    attributeResponse2.setName(ATTRIBUTE_NAME2);
    attributeResponse2.setNameEnglish(ATTRIBUTE_NAME2 + ENGLISH);
    attributeResponse2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    attributeResponse2.setPredefinedAllowedAttributeValues(
        Arrays.asList(predefinedAllowedAttributeValueResponse1, predefinedAllowedAttributeValueResponse2));
    attributeResponse2
        .setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse1, allowedAttributeValueResponse2));

    attributeResponse3 = new AttributeResponse();
    attributeResponse3.setId(ATTRIBUTE_ID3);
    attributeResponse3.setAttributeCode(ATTRIBUTE_CODE3);
    attributeResponse3.setName(BulkParameters.WARNA);
    attributeResponse3.setVariantCreation(true);
    attributeResponse3.setNameEnglish(BulkParameters.WARNA + ENGLISH);
    attributeResponse3.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    attributeResponse3.setPredefinedAllowedAttributeValues(
        Arrays.asList(predefinedAllowedAttributeValueResponse1, predefinedAllowedAttributeValueResponse2));
    attributeResponse3
        .setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse1, allowedAttributeValueResponse2));

    attributeResponse4 = new AttributeResponse();
    attributeResponse4.setId(ATTRIBUTE_ID4);
    attributeResponse4.setAttributeCode(ATTRIBUTE_CODE4);
    attributeResponse4.setName(ATTRIBUTE_NAME4);
    attributeResponse4.setNameEnglish(ATTRIBUTE_NAME4 + ENGLISH);
    attributeResponse4.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    attributeResponse4.setPredefinedAllowedAttributeValues(
        Arrays.asList(predefinedAllowedAttributeValueResponse1, predefinedAllowedAttributeValueResponse2));
    attributeResponse4
        .setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse1, allowedAttributeValueResponse2));

    attributeResponse5 = new AttributeResponse();
    attributeResponse5.setId(ATTRIBUTE_ID5);
    attributeResponse5.setAttributeCode(ATTRIBUTE_CODE5);
    attributeResponse5.setName(ATTRIBUTE_NAME3);
    attributeResponse5.setNameEnglish(ATTRIBUTE_NAME3 + ENGLISH);
    attributeResponse5.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    attributeResponse5.setPredefinedAllowedAttributeValues(
        Arrays.asList(predefinedAllowedAttributeValueResponse1, predefinedAllowedAttributeValueResponse2));
    attributeResponse5
        .setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse1, allowedAttributeValueResponse2));

    attributeResponse6 = new AttributeResponse();
    attributeResponse6.setId(ATTRIBUTE_ID6);
    attributeResponse6.setAttributeCode(ATTRIBUTE_CODE6);
    attributeResponse6.setName(BulkParameters.UKURAN);
    attributeResponse6.setVariantCreation(true);
    attributeResponse6.setNameEnglish(BulkParameters.UKURAN + ENGLISH);
    attributeResponse6.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    attributeResponse6.setPredefinedAllowedAttributeValues(
        Arrays.asList(predefinedAllowedAttributeValueResponse1, predefinedAllowedAttributeValueResponse2));
    attributeResponse6
        .setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse1, allowedAttributeValueResponse2));

    categoryAttributeResponse1 = new CategoryAttributeResponse();
    categoryAttributeResponse1.setAttribute(attributeResponse1);

    categoryAttributeResponse2 = new CategoryAttributeResponse();
    categoryAttributeResponse2.setAttribute(attributeResponse2);

    categoryAttributeResponse3 = new CategoryAttributeResponse();
    categoryAttributeResponse3.setAttribute(attributeResponse3);

    categoryAttributeResponse4 = new CategoryAttributeResponse();
    categoryAttributeResponse4.setAttribute(attributeResponse4);

    categoryAttributeResponse5 = new CategoryAttributeResponse();
    categoryAttributeResponse5.setAttribute(attributeResponse5);

    categoryAttributeResponse6 = new CategoryAttributeResponse();
    categoryAttributeResponse6.setAttribute(attributeResponse6);

    categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryCode(CATEGORY_NAME111);
    categoryDetailResponse.setCategoryAttributes(Arrays
        .asList(categoryAttributeResponse1, categoryAttributeResponse2, categoryAttributeResponse3,
            categoryAttributeResponse4, categoryAttributeResponse5, categoryAttributeResponse6));

    ReflectionTestUtils
        .setField(bulkProcessServiceBean, "blibliMassTemplateLocation", BASE_DIRECTORY + "/ExcelTemplate/");
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedBaseTemplateFile", "Blibli-mass-base-template.xlsm");
    ReflectionTestUtils
        .setField(bulkProcessServiceBean, "unifiedUploadTemplateFile", "Blibli-mass-upload-template.xlsm");
    ReflectionTestUtils
        .setField(bulkProcessServiceBean, "unifiedUploadTemplateFileEnglish", "Blibli-mass-upload-template-en.xlsm");

    KafkaEventLog kafkaEventLog = new KafkaEventLog();
    kafkaEventLog.setTopicName("com.gdn.x.productcategorybase.category.publish");
    kafkaEventLog.setEventMessage("category change event");
    kafkaEventLogList = new ArrayList<>();
    kafkaEventLogList.add(kafkaEventLog);

    Mockito.when(this.kafkaEventLogService.getKafkaEventLogs()).thenReturn(kafkaEventLogList);
    Mockito.doNothing().when(this.kafkaEventLogService).deleteKafkaEventLogsMarkForDelete(kafkaEventLogList);
    Mockito.when(this.pcbOutboundService.getGenericTemplateCategories(GENERIC_TEMPLATE_ELIGIBLE,
        true))
        .thenReturn(genericTemplateCategoryResponseList);
    Mockito.when(this.pcbOutboundService.getCategoryDetailResponse(Mockito.anyString()))
        .thenReturn(categoryDetailResponse);
    Mockito.when(this.pcbOutboundService.getAttributeDetail(ATTRIBUTE_ID1)).thenReturn(attributeResponse1);
    Mockito.when(this.pcbOutboundService.getAttributeDetail(ATTRIBUTE_ID2)).thenReturn(attributeResponse2);
    Mockito.when(this.pcbOutboundService.getAttributeDetail(ATTRIBUTE_ID4)).thenReturn(attributeResponse4);
    Mockito.when(this.pcbOutboundService.getAttributeDetail(ATTRIBUTE_ID5)).thenReturn(attributeResponse5);
    Mockito.when(this.pcbOutboundService.getAttributeDetail(ATTRIBUTE_ID6)).thenReturn(attributeResponse6);
    Mockito.when(bulkProcessDataService.getDistinctParentProduct(anyString(), anyString()))
        .thenReturn(Arrays.asList(PARENT_PRODUCT));
    ProfileResponse businessPartner = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    company.setInternationalFlag(false);
    businessPartner.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    businessPartner.setCompany(company);
    Mockito.doNothing().when(this.systemParameterConfigService)
        .update(Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(DEFAULT_USERNAME),
            Mockito.any(SystemParameterConfigRequest.class));
    Mockito.when(fileStorageService.isMassTemplateFileExist(anyString())).thenReturn(true);
    CategoryTreeResponse genericTemplateCategoryResponse = new CategoryTreeResponse();
    genericTemplateCategoryResponse.setId(CATEGORY_ID);
    genericTemplateCategoryResponse.setParentCategory(StringUtils.EMPTY);
    genericTemplateCategoryResponse.setCategoryName(CATEGORY_ID);
    genericTemplateCategoryResponse.setCategoryEnglishName(CATEGORY_ID);
    genericTemplateCategories.add(genericTemplateCategoryResponse);
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("Brand 1");
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse1 =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse1.setValue("Brand 1 (IN Review)");
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse2 =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse2.setValue("Brand 2");
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse3 =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse3.setValue("Brand 2 (In review)");
    predefinedAllowedAttributeValueResponses.add(predefinedAllowedAttributeValueResponse);
    predefinedAllowedAttributeValueResponses.add(predefinedAllowedAttributeValueResponse1);
    predefinedAllowedAttributeValueResponses.add(predefinedAllowedAttributeValueResponse2);
    predefinedAllowedAttributeValueResponses.add(predefinedAllowedAttributeValueResponse3);
    BrandWipResponse brandWipResponse = new BrandWipResponse();
    brandWipResponse.setBrandName("Brand 1 (IN Review)");
    BrandWipResponse brandWipResponse1 = new BrandWipResponse();
    brandWipResponse1.setBrandName("Brand 2 (IN Review)");
    inReviewBrandsResponse.add(brandWipResponse);
    inReviewBrandsResponse.add(brandWipResponse1);

    attributeResponse7 = new AttributeResponse();
    attributeResponse7.setId(ATTRIBUTE_ID1);
    attributeResponse7.setAttributeCode(ATTRIBUTE_CODE1);
    attributeResponse7.setName(Constant.ATTRIBUTE_NAME_BRAND);
    attributeResponse7.setNameEnglish(ATTRIBUTE_NAME1 + ENGLISH);
    attributeResponse7.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());

    attributeResponseList = new ArrayList<>();
    attributeResponseList.add(attributeResponse7);

    UnmappedSkuResponse unmappedSkuResponse1 =
        new UnmappedSkuResponse(PRODUCT_SKU, PRODUCT_NAME, CATALOG_CODE, CATEGORY_CODE1, new Date());
    UnmappedSkuResponse unmappedSkuResponse2 =
        new UnmappedSkuResponse(PRODUCT_SKU, PRODUCT_NAME, CATALOG_CODE, CATEGORY_CODE2, new Date());
    UnmappedSkuResponse unmappedSkuResponse3 =
        new UnmappedSkuResponse(PRODUCT_SKU, PRODUCT_NAME, CATALOG_CODE, CATEGORY_CODE3_3, new Date());
    unmappedSkuResponses = new ArrayList<>();
    unmappedSkuResponses.add(unmappedSkuResponse1);
    unmappedSkuResponses.add(unmappedSkuResponse2);
    unmappedSkuResponses.add(unmappedSkuResponse3);

    ReflectionTestUtils
        .setField(bulkProcessServiceBean, "categoryTemplateLocation", BASE_DIRECTORY + "/CategoryTemplate/");
    ReflectionTestUtils.setField(bulkProcessServiceBean, "categoryUploadTemplateFile", "products-upload-template.xlsx");
    ReflectionTestUtils
        .setField(bulkProcessServiceBean, "categoryUploadTemplateFileEnglish", "products-template-upload-English.xlsx");
    ReflectionTestUtils.setField(bulkProcessServiceBean, "categoryBaseTemplateFile", "products-template.xlsx");
    ReflectionTestUtils
        .setField(bulkProcessServiceBean, "categoryBaseTemplateFileEnglish", "products-template-English.xlsx");

    thresholdPendingBulkRequests = new SystemParameterConfig();
    thresholdPendingBulkRequests.setVariable(Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS);
    thresholdPendingBulkRequests.setValue(THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS_VALUE);
    mailTo = new SystemParameterConfig();
    mailTo.setVariable(Constant.MAILS_FOR_PENDING_BULK_REQUESTS);
    mailTo.setValue(MAILS_FOR_PENDING_BULK_REQUESTS_VALUE);
    thresholdPendingBulkRequestsFailedValue = new SystemParameterConfig();
    thresholdPendingBulkRequestsFailedValue.setVariable(Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS);
    thresholdPendingBulkRequestsFailedValue.setValue(RANDOM_VALUE_1);
    thresholdAbortedBulkRequests = new SystemParameterConfig();
    thresholdAbortedBulkRequests.setVariable(Constant.THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS);
    thresholdAbortedBulkRequests.setValue(THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS_VALUE);
    abortedRequestsRequired = new SystemParameterConfig();
    abortedRequestsRequired.setVariable(Constant.ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES);
    abortedRequestsRequired.setValue(THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS_VALUE);

    BulkPendingProductsDTO bulkPendingProductsDTO = new BulkPendingProductsDTO();
    bulkPendingProductsDTO.setBulkProcessType(BULK_PROCESS_TYPE);
    bulkPendingProductsDTO.setBusinessPartnerCode(BUSINESS_PARTNER);
    bulkPendingProductsDTO.setStartDate(DATE);
    bulkPendingProductsDTO.setBulkUpdate(IS_BULK_UPDATE);
    BulkPendingProductsDTO bulkPendingProductsDTO1 = new BulkPendingProductsDTO();
    bulkPendingProductsDTO1.setBulkProcessType(null);
    bulkPendingProductsDTO1.setBusinessPartnerCode(null);
    bulkPendingProductsDTO1.setStartDate(null);
    bulkPendingProductsDTO1.setBulkUpdate(null);
    bulkPendingProductsDTOList = new ArrayList<>();
    bulkPendingProductsDTOList.add(bulkPendingProductsDTO);
    bulkPendingProductsDTOList.add(bulkPendingProductsDTO1);
    bulkPendingProductsDTOListNull = new ArrayList<>();
    bulkPendingProductsDTOListNull.add(bulkPendingProductsDTO1);
    BulkPendingProductResponse bulkPendingProductResponse = new BulkPendingProductResponse();
    bulkPendingProductResponse.setBusinessPartnerCode(BUSINESS_PARTNER);
    bulkPendingProductResponse.setBulkProcessType(BULK_PROCESS_TYPE);
    bulkPendingProductResponse.setStartDate(DATE.toString());
    BulkPendingProductResponse bulkPendingProductResponse1 = new BulkPendingProductResponse();
    bulkPendingProductResponse1.setBusinessPartnerCode(null);
    bulkPendingProductResponse1.setBulkProcessType(null);
    bulkPendingProductResponse1.setStartDate(null);
    bulkPendingProductResponseList = new ArrayList<>();
    bulkPendingProductResponseList.add(bulkPendingProductResponse);
    bulkPendingProductResponseList.add(bulkPendingProductResponse1);

    stringBooleanMap.putIfAbsent(PRODUCT_SKU, true);

    bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setBulkProcessId(getBulkProcess_1().getId());
    bulkProcessImage.setCompleted(false);
    bulkProcessImage.setSequence(0);
    bulkProcessImage.setImageURL(IMAGE_URL);

    bulkProcessVideo = new BulkProcessVideo();
    bulkProcessVideo.setCompleted(false);
    bulkProcessVideo.setUploadedURL(UPLOADED_URL);

    bulkProcessImageQC = new BulkProcessImageQC();
    bulkProcessImageQC.setCompleted(false);

    bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setBulkProcessId(getBulkProcess().get(0).getId());
    bulkProcessData1.setStatus(BulkProcessData.STATUS_IN_PROGRESS);

    bulkProcessDataBatchSize = new SystemParameterConfig();
    bulkProcessDataBatchSize.setValue(BATCH_SIZE);

    rowNumberParentCodeDTO = new RowNumberParentCodeDTO();
    rowNumberParentCodeDTONew = new RowNumberParentCodeDTO();
    rowNumberParentCodeDTONew.setRowNumber(10);
    rowNumberParentCodeDTO.setBulkRequestData(
        "{\"itemSku\":\"HIC-60001-00003-00001\",\"pickupPointCode\":\"PP-3010025\",\"Blibli Product SKU\":\"HIC-60001-00003\",\"reason\":\"Blibli SKU: HIC-60001-00003-00001, Kode Lokasi: PP-3010025. Alasan Kegagalan: Item SKU tidak ditemukan.\",\"Blibli SKU\":\"itemSku\"}");
    rowNumberParentCodeDTONew.setBulkRequestData(
        "{\"itemSku\":\"HIC-60001-00003-00002\",\"pickupPointCode\":\"PP-3010025\",\"Blibli Product SKU\":\"HIC-60001-00003\",\"reason\":\"Blibli SKU: HIC-60001-00003-00002, Kode Lokasi: PP-3010025. Alasan Kegagalan: Item SKU tidak ditemukan.\",\"Blibli SKU\":\"HIC-60001-00003-00002\"}");
    rowNumberParentCodeDTO.setRowNumber(1);

    RowNumberParentCodeDTO rowNumberParentCodeDTO1 = new RowNumberParentCodeDTO();
    rowNumberParentCodeDTO1.setBulkRequestData(
        "{\"itemSku\":\"HIC-60001-00003-00002\",\"pickupPointCode\":\"PP-3010025\",\"Blibli Product SKU\":\"HIC-60001-00003\",\"reason\":\"Blibli SKU: HIC-60001-00003-00001, Kode Lokasi: PP-3010025. Alasan Kegagalan: Item SKU tidak ditemukan.\",\"Blibli SKU\":\"itemSku\"}");
    rowNumberParentCodeDTO1.setRowNumber(2);
    rowNumberParentCodeDTOList.add(rowNumberParentCodeDTO);
    rowNumberParentCodeDTOList.add(rowNumberParentCodeDTO1);


    rowNumberParentCodeDTO3 = new RowNumberParentCodeDTO();
    rowNumberParentCodeDTO3.setBulkRequestData(
        "{\"itemSku\":\"HIC-60001-00003-00001\",\"pickupPointCode\":\"PP-3010025\",\"productSku\":\"HIC-60001-00003\",\"reason\":\"Blibli SKU: HIC-60001-00003-00001, Kode Lokasi: PP-3010025. Alasan Kegagalan: Item SKU tidak ditemukan.\",\"Blibli SKU\":\"itemSku\"}");
    rowNumberParentCodeDTO3.setRowNumber(1);

    RowNumberParentCodeDTO rowNumberParentCodeDTO4 = new RowNumberParentCodeDTO();
    rowNumberParentCodeDTO4.setBulkRequestData(
        "{\"itemSku\":\"HIC-60001-00003-00002\",\"pickupPointCode\":\"PP-3010025\",\"productSku\":\"HIC-60001-00003\",\"reason\":\"Blibli SKU: HIC-60001-00003-00001, Kode Lokasi: PP-3010025. Alasan Kegagalan: Item SKU tidak ditemukan.\",\"Blibli SKU\":\"itemSku\"}");
    rowNumberParentCodeDTO4.setRowNumber(2);
    rowNumberParentCodeDTOList1.add(rowNumberParentCodeDTO3);
    rowNumberParentCodeDTOList1.add(rowNumberParentCodeDTO4);

    downloadQRCodeRequest.setQrGenerationType("ITEM");
    downloadQRCodeRequest.setMerchantCode(BUSINESS_PARTNER);
    downloadQRCodeRequest.setQrPerPage(1);
    qrExcelUploadRequest.setDownloadQRCodeRequest(downloadQRCodeRequest);
    qrCodeExcelQueue.setStoreId(DEFAULT_STORE_ID);
    qrCodeExcelQueue.setDownloadQRCodeRequest(downloadQRCodeRequest);
    qrCodeExcelQueue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    qrCodeExcelQueue.getDownloadQRCodeRequest().setMerchantCode(BUSINESS_PARTNER);
    qrCodeSystemParameter.setValue(QR_CODE_EXCEL_LIMIT);
    bulkUpdateProcessDTO.setBusinessPartnerCode(BUSINESS_PARTNER);

    qrCodeRowItemInfo.setItemSku(ITEM_SKU);
    qrCodeRowItemInfo.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    qrCodeRowInfo.setQrGenerationType(Constant.ITEM_PICKUP_POINT);
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    itemL5ListingResponse.setItemSku(ITEM_SKU);
    itemL5ListingResponse.setPpCode(DEFAULT_PICKUP_POINT_CODE);
    qrCodeBulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "genericTemplateFileTypes", GENERIC_FILE_TYPE_LIST);
  }

  public void setBulkProcessRepository(BulkProcessRepository bulkProcessRepository) {
    this.bulkProcessRepository = bulkProcessRepository;
  }

  public void setBulkProcessServiceBean(BulkProcessServiceBean bulkProcessServiceBean) {
    this.bulkProcessServiceBean = bulkProcessServiceBean;
  }

  @Test
  public void testDeleteByBulkProcessCode() throws Exception {
    BulkProcess bulkProcess = getBulkProcess_1();
    Mockito.when(getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(getBulkProcessRepository().save(bulkProcess)).thenReturn(bulkProcess);
    getBulkProcessServiceBean().deleteByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(getBulkProcessRepository(), AT_LEAST_ONE)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(getBulkProcessRepository(), AT_LEAST_ONE).save(bulkProcess);
  }

  @Test
  public void testDeleteByBulkProcessCodeWithBulkProcessNotes() throws Exception {
    BulkProcess bulkProcess = getBulkProcess_1();
    bulkProcess.getBulkProcessNotes().add(new BulkProcessNotes());
    Mockito.when(getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(getBulkProcessRepository().save(bulkProcess)).thenReturn(bulkProcess);
    getBulkProcessServiceBean().deleteByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(getBulkProcessRepository(), AT_LEAST_ONE)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(getBulkProcessRepository(), AT_LEAST_ONE).save(bulkProcess);
  }

  @Test
  public void testDeleteByBulkProcessCodeWithInvalidBulkProcessCode() throws Exception {
    BulkProcess bulkProcess = getBulkProcess_1();
    Mockito.when(getBulkProcessRepository().findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(null);
    try {
      assertThrows(ApplicationException.class,
          () -> getBulkProcessServiceBean().deleteByBulkProcessCode(DEFAULT_STORE_ID,
              DEFAULT_BULK_PROCESS_CODE));
    } catch (Exception e) {
    } finally {
      Mockito.verify(getBulkProcessRepository(), AT_LEAST_ONE)
          .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
              DEFAULT_BULK_PROCESS_CODE);
      Mockito.verify(getBulkProcessRepository(), AT_LEAST_NONE).save(bulkProcess);
    }
  }

  @Test
  public void testFindByBulkProcessCode() throws Exception {
    List<BulkProcess> bulkProcess = getBulkProcess();
    Mockito.when(getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess.get(0));
    getBulkProcessServiceBean().findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(getBulkProcessRepository(), AT_LEAST_ONE)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
  }

  @Test
  public void testFindByBulkProcessCodeAndStatus() throws Exception {
    List<BulkProcess> bulkProcess = getBulkProcess();
    Mockito.when(
        getBulkProcessRepository().findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BULK_PROCESS_CODE, STATUS)).thenReturn(bulkProcess.get(0));
    getBulkProcessServiceBean().findByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE, STATUS);
    Mockito.verify(getBulkProcessRepository(), AT_LEAST_ONE)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE,
            STATUS);
  }

  @Test
  public void testFindByBusinessPartnerCodeAndBulkProcessTypeAndCreatedBy() throws Exception {
    List<BulkProcess> bulkProcess = getBulkProcess();
    Page<BulkProcess> page = new PageImpl<BulkProcess>(bulkProcess);
    Mockito.when(getBulkProcessRepository()
        .getBulkProcessNotification(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, "ProductLevel3", DEFAULT_PAGEABLE))
        .thenReturn(page);
    getBulkProcessServiceBean()
        .findByBusinessPartnerCodeAndBulkProcessTypeAndCreatedBy(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            "ProductLevel3", DEFAULT_PAGEABLE);
    Mockito.verify(getBulkProcessRepository(), AT_LEAST_ONE)
        .getBulkProcessNotification(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, "ProductLevel3", DEFAULT_PAGEABLE);
  }

  @Test
  public void testFindByBusinessPartnerCodeAndCreatedBy() throws Exception {
    List<BulkProcess> bulkProcess = getBulkProcess();
    Page<BulkProcess> page = new PageImpl<BulkProcess>(bulkProcess);
    Mockito.when(getBulkProcessRepository()
        .findByStoreIdAndBusinessPartnerCodeAndCreatedByAndMarkForDeleteFalseOrderByStartDateDesc(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_USERNAME, DEFAULT_PAGEABLE)).thenReturn(page);
    getBulkProcessServiceBean()
        .findByBusinessPartnerCodeAndCreatedBy(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_USERNAME,
            DEFAULT_PAGEABLE);
    Mockito.verify(getBulkProcessRepository(), AT_LEAST_ONE)
        .findByStoreIdAndBusinessPartnerCodeAndCreatedByAndMarkForDeleteFalseOrderByStartDateDesc(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_USERNAME, DEFAULT_PAGEABLE);
  }

  @Test
  public void testUpdateBulkProcessRecordAsMarkForDeleteTrue() throws Exception {
    Mockito.doNothing().when(getBulkProcessRepository()).updateBulkProcessRecordAsMarkForDeleteTrue(anyString());
    getBulkProcessServiceBean().updateBulkProcessRecordAsMarkForDeleteTrue("08-09-2016");
    Mockito.verify(getBulkProcessRepository()).updateBulkProcessRecordAsMarkForDeleteTrue(anyString());
  }

  @Test
  public void saveOperationTest() throws Exception {
    this.bulkProcessServiceBean.saveOperation(new BulkProcess());
    Mockito.verify(this.bulkProcessRepository).save(any(BulkProcess.class));
  }

  @Test
  public void saveBulkProcessTest() throws Exception {
    this.bulkProcessServiceBean.saveBulkProcess(new BulkProcess());
    Mockito.verify(this.bulkProcessRepository).save(any(BulkProcess.class));
  }

  @Test
  public void saveBulkProcessListTest() throws Exception {
    this.bulkProcessServiceBean.saveBulkProcessList(Arrays.asList(new BulkProcess()));
    Mockito.verify(this.bulkProcessRepository).saveAll(anyList());
  }

  @Test
  public void checkForPendingBulkProcessByBusinessPartnerCodeTest_BulkUpdateStatusFlagFalse() throws Exception {
    Mockito.when(
        this.bulkProcessRepository.countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(
            DEFAULT_STORE_ID, BULK_UPDATE_MERCHANT_CODE,
          Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
            BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
            BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_PROCESSED, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
            BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2),
          ImmutableSet.of(Constant.INSTANT_PICKUP_PRODUCT))).thenReturn(COUNT_FALSE);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE);
    systemParameterConfig.setValue(String.valueOf(false));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE)).thenReturn(systemParameterConfig);
    BulkPendingRequestsResponse pendingRequestsResponse =
        this.bulkProcessServiceBean.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            Constant.BULK_UPLOAD_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE, Constant.INSTANT_PICKUP_PRODUCT);
    Mockito.verify(this.bulkProcessRepository)
        .countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
            BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
            BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_PROCESSED, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
            BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2),
          ImmutableSet.of(Constant.INSTANT_PICKUP_PRODUCT));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE);
    Assertions.assertFalse(pendingRequestsResponse.isBulkUpdateStatusFlag());
    Assertions.assertEquals(COUNT_FALSE, pendingRequestsResponse.getPendingRequestsCount());
  }


  @Test
  public void checkForPendingBulkProcessByBusinessPartnerCodeTest_BulkUpdateStatusFlagFalse_BulkProcessTypeEmpty()
      throws Exception {
    Mockito.when(this.bulkProcessRepository.countByStoreIdAndBusinessPartnerCodeAndStatusInAndMarkForDeleteFalse(
            DEFAULT_STORE_ID, BULK_UPDATE_MERCHANT_CODE,
        Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
          BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
          BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2, BulkProcess.STATUS_READY_TO_PROCESS,
          BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_PROCESSED, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
          BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2)))
        .thenReturn(COUNT_FALSE);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE);
    systemParameterConfig.setValue(String.valueOf(false));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE)).thenReturn(systemParameterConfig);
    BulkPendingRequestsResponse pendingRequestsResponse =
        this.bulkProcessServiceBean.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            Constant.BULK_UPLOAD_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE, StringUtils.EMPTY);
    Mockito.verify(this.bulkProcessRepository)
        .countByStoreIdAndBusinessPartnerCodeAndStatusInAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
            BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
            BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_PROCESSED, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
            BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE);
    Assertions.assertFalse(pendingRequestsResponse.isBulkUpdateStatusFlag());
    Assertions.assertEquals(COUNT_FALSE, pendingRequestsResponse.getPendingRequestsCount());
  }

  @Test
  public void checkForPendingBulkProcessByBusinessPartnerCodeTest_BulkUpdateStatusFlagTrue() throws Exception {
    Mockito.when(
        this.bulkProcessRepository.countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(
            DEFAULT_STORE_ID, BULK_UPDATE_MERCHANT_CODE,
          Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
            BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
            BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_PROCESSED, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
            BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2),
          ImmutableSet.of(Constant.INSTANT_PICKUP_PRODUCT))).thenReturn(COUNT_TRUE);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE);
    systemParameterConfig.setValue(String.valueOf(false));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE)).thenReturn(systemParameterConfig);
    BulkPendingRequestsResponse pendingRequestsResponse =
        this.bulkProcessServiceBean.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            Constant.BULK_UPLOAD_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE, Constant.INSTANT_PICKUP_PRODUCT);
    Mockito.verify(this.bulkProcessRepository)
        .countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
            BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
            BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_PROCESSED, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
            BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2),
          ImmutableSet.of(Constant.INSTANT_PICKUP_PRODUCT));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE);
    assertTrue(pendingRequestsResponse.isBulkUpdateStatusFlag());
    Assertions.assertEquals(0, pendingRequestsResponse.getPendingRequestsCount());
  }

  @Test
  public void checkForPendingBulkProcessByBusinessPartnerCodeTest_SwitchTrue() throws Exception {
    Mockito.when(
        this.bulkProcessRepository.countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(
            DEFAULT_STORE_ID, BULK_UPDATE_MERCHANT_CODE,
            Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
                BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_READY_TO_PROCESS, BulkProcess.STATUS_PUBLISHED,
                BulkProcess.STATUS_PROCESSED, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
              BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2),
          ImmutableSet.of(Constant.INSTANT_PICKUP_PRODUCT))).thenReturn(COUNT_TRUE);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE);
    systemParameterConfig.setValue(String.valueOf(true));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE)).thenReturn(systemParameterConfig);
    BulkPendingRequestsResponse pendingRequestsResponse =
        this.bulkProcessServiceBean.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            Constant.BULK_UPLOAD_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE, Constant.INSTANT_PICKUP_PRODUCT);
    Mockito.verify(this.bulkProcessRepository)
        .countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
            BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
            BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_PROCESSED, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
            BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2),
          ImmutableSet.of(Constant.INSTANT_PICKUP_PRODUCT));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE);
    Assertions.assertFalse(pendingRequestsResponse.isBulkUpdateStatusFlag());
    Assertions.assertEquals(0, pendingRequestsResponse.getPendingRequestsCount());
  }

  @Test
  public void checkForPendingBulkProcessByBusinessPartnerCodeInternalBulkProcessTest() throws Exception {
    Mockito.when(
        this.bulkProcessRepository.countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(
            DEFAULT_STORE_ID, BULK_UPDATE_MERCHANT_CODE,
            Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
                BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_READY_TO_PROCESS, BulkProcess.STATUS_PUBLISHED,
                BulkProcess.STATUS_PROCESSED), ImmutableSet.of(Constant.INSTANT_PICKUP_PRODUCT))).thenReturn(COUNT_TRUE);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE);
    systemParameterConfig.setValue(String.valueOf(true));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE)).thenReturn(systemParameterConfig);
    BulkPendingRequestsResponse pendingRequestsResponse =
        this.bulkProcessServiceBean.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            Constant.BULK_INTERNAL_UPLOAD, DEFAULT_BUSINESS_PARTNER_CODE,
            SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE, Constant.INSTANT_PICKUP_PRODUCT);
    Mockito.verify(internalProcessService)
        .countByStoreIdAndProcessTypeAndStatusInAndCreatedByAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            Constant.INSTANT_PICKUP_PRODUCT,
            Arrays.asList(ProcessStatus.PICKED.name(), ProcessStatus.PENDING.name(), ProcessStatus.IN_PROGRESS.name()),
            DEFAULT_USERNAME);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE);
    Assertions.assertFalse(pendingRequestsResponse.isBulkUpdateStatusFlag());
    Assertions.assertEquals(0, pendingRequestsResponse.getPendingRequestsCount());
  }

  @Test
  public void checkForPendingBulkProcessByBusinessPartnerCodeInternalBulkProcessEmptyTypeTest() throws Exception {
    Mockito.when(
        this.bulkProcessRepository.countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(
            DEFAULT_STORE_ID, BULK_UPDATE_MERCHANT_CODE,
            Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
                BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_READY_TO_PROCESS, BulkProcess.STATUS_PUBLISHED,
                BulkProcess.STATUS_PROCESSED), ImmutableSet.of(Constant.INSTANT_PICKUP_PRODUCT))).thenReturn(COUNT_TRUE);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE);
    systemParameterConfig.setValue(String.valueOf(true));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE)).thenReturn(systemParameterConfig);
    BulkPendingRequestsResponse pendingRequestsResponse =
        this.bulkProcessServiceBean.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            Constant.BULK_INTERNAL_UPLOAD, DEFAULT_BUSINESS_PARTNER_CODE,
            SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE, StringUtils.EMPTY);
    Mockito.verify(internalProcessService)
        .countByStoreIdAndProcessTypeAndStatusInAndCreatedByAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            StringUtils.EMPTY,
            Arrays.asList(ProcessStatus.PICKED.name(), ProcessStatus.PENDING.name(), ProcessStatus.IN_PROGRESS.name()),
            DEFAULT_USERNAME);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE);
    Assertions.assertFalse(pendingRequestsResponse.isBulkUpdateStatusFlag());
    Assertions.assertEquals(0, pendingRequestsResponse.getPendingRequestsCount());
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_isDisabledSetTrue() {
    configurationResponse.getContent().get(0).setDisabled(Boolean.TRUE);
    File file = new File("/target/test-classes/");
    file.mkdir();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_failsCreateCampaignProduct() {
    configurationResponse.getContent().get(0).setFolderName(StringUtils.EMPTY);
    File file = new File(DEFAULT_FOLDER + CREATE_CAMPAIGN_PRODUCT);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_failsDownloadCampaignProduct() {
    configurationResponse.getContent().get(0).setFolderName(StringUtils.EMPTY);
    File file = new File(DEFAULT_FOLDER + DOWNLOAD_CAMPAIGN_PRODUCT);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_failsDownloadProducts() {
    configurationResponse.getContent().get(0).setFolderName(StringUtils.EMPTY);
    File file = new File(DEFAULT_FOLDER + DOWNLOAD_PRODUCTS);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_failsBulkUpdate() {
    configurationResponse.getContent().get(0).setFolderName(StringUtils.EMPTY);
    File file = new File(DEFAULT_FOLDER + BULK_UPDATE_DIR);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_failsDownloadMasterProducts() {
    configurationResponse.getContent().get(0).setFolderName(StringUtils.EMPTY);
    File file = new File(DEFAULT_FOLDER + DOWNLOAD_MASTER_PRODUCTS);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_failsDownloadFailedProducts() {
    configurationResponse.getContent().get(0).setFolderName(StringUtils.EMPTY);
    File file = new File(DEFAULT_FOLDER + DOWNLOAD_FAILED_PRODUCTS);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_failsDownloadOrders() {
    configurationResponse.getContent().get(0).setFolderName(StringUtils.EMPTY);
    File file = new File(DEFAULT_FOLDER + DOWNLOAD_ORDERS);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_failsDownloadProductVendors() {
    configurationResponse.getContent().get(0).setFolderName(StringUtils.EMPTY);
    File file = new File(DEFAULT_FOLDER + DOWNLOAD_PRODUCT_VENDORS);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_failsDownloadProductReview() {
    configurationResponse.getContent().get(0).setFolderName(StringUtils.EMPTY);
    File file = new File(DEFAULT_FOLDER + DOWNLOAD_PRODUCT_REVIEW);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_deleteFolderFromRoot() {
    configurationResponse.getContent().get(0).setFolderName(StringUtils.EMPTY);
    File file = new File(DEFAULT_FOLDER + TEST_FOLDER);
    file.mkdirs();
    file.setLastModified(System.currentTimeMillis());
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_AgeOfDeletionNotEnough() {
    configurationResponse.getContent().get(0).setFolderName(StringUtils.EMPTY);
    configurationResponse.getContent().get(0).setAgeOfDeletion(1);
    File file = new File(DEFAULT_FOLDER + TEST_FOLDER);
    file.mkdirs();
    file.setLastModified(System.currentTimeMillis());
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_deleteFileFromRoot() throws Exception {
    configurationResponse.getContent().get(0).setFolderName(StringUtils.EMPTY);
    File file = new File(DEFAULT_FOLDER);
    file.mkdir();
    File file1 = new File(DEFAULT_FOLDER + TEST_FILE);
    file1.createNewFile();
    file1.setLastModified(System.currentTimeMillis());
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_forCreateCampaignProduct() {
    configurationResponse.getContent().get(0).setFolderName(CREATE_CAMPAIGN_PRODUCT);
    File file = new File(DEFAULT_FOLDER + CREATE_CAMPAIGN_PRODUCT + TEST_FOLDER);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_forDownloadCampaignProduct() {
    configurationResponse.getContent().get(0).setFolderName(DOWNLOAD_CAMPAIGN_PRODUCT);
    File file = new File(DEFAULT_FOLDER + DOWNLOAD_CAMPAIGN_PRODUCT + TEST_FOLDER);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_forDownloadProducts() {
    configurationResponse.getContent().get(0).setFolderName(DOWNLOAD_PRODUCTS);
    File file = new File(DEFAULT_FOLDER + DOWNLOAD_PRODUCTS + TEST_FOLDER);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_forDownloadMasterProducts() {
    configurationResponse.getContent().get(0).setFolderName(DOWNLOAD_MASTER_PRODUCTS);
    File file = new File(DEFAULT_FOLDER + DOWNLOAD_MASTER_PRODUCTS + TEST_FOLDER);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_forDownloadFailedProducts() {
    configurationResponse.getContent().get(0).setFolderName(DOWNLOAD_FAILED_PRODUCTS);
    File file = new File(DEFAULT_FOLDER + DOWNLOAD_FAILED_PRODUCTS + TEST_FOLDER);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_forDownloadOrders() {
    configurationResponse.getContent().get(0).setFolderName(DOWNLOAD_ORDERS);
    File file = new File(DEFAULT_FOLDER + DOWNLOAD_ORDERS + TEST_FOLDER);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void findAndDeleteBulkProcessCodesBeforeXDays_forDownloadProductVendors() {
    configurationResponse.getContent().get(0).setFolderName(DOWNLOAD_PRODUCT_VENDORS);
    File file = new File(DEFAULT_FOLDER + DOWNLOAD_PRODUCT_VENDORS + TEST_FOLDER);
    file.mkdirs();
    Mockito.doReturn(configurationResponse).when(this.configurationService)
        .findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    bulkProcessServiceBean.findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    Mockito.verify(this.configurationService).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    file.delete();
  }

  @Test
  public void abortPendingBulkProcessBeforeTest() {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "abortSchedulerProcessStatusList", STATUS_LIST);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.ABORT_PENDING_BULK_PROCESS_LIMIT);
    systemParameterConfig.setValue(String.valueOf(20));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.ABORT_PENDING_BULK_PROCESS_LIMIT)).thenReturn(systemParameterConfig);
    Mockito.doNothing().when(bulkProcessRepository)
        .updateStatusInPendingOrInProgressBulkProcessToAborted(anyList(), any(Date.class));
    bulkProcessServiceBean.abortPendingBulkProcessBefore(DEFAULT_STORE_ID);
    Mockito.verify(bulkProcessRepository)
        .updateStatusInPendingOrInProgressBulkProcessToAborted(statusListArgumentCapture.capture(), any(Date.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.ABORT_PENDING_BULK_PROCESS_LIMIT);
    Assertions.assertEquals(IN_PROGRESS, statusListArgumentCapture.getValue().get(0));
    Assertions.assertEquals(PUBLISHED, statusListArgumentCapture.getValue().get(1));
  }

  @Test
  public void abortBulkProcessTest_WithPendingProcess() throws Exception {
    final BulkProcess bulkProcess = getBulkProcess().get(0);
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(null);
    bulkProcessServiceBean.abortBulkProcess(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessRepository).save(bulkProcess);
  }

  @Test
  public void abortBulkProcessTest_WithInProgressProcess() throws Exception {
    final BulkProcess bulkProcess = getBulkProcess().get(1);
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(null);
    bulkProcessServiceBean.abortBulkProcess(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessRepository).save(bulkProcess);
  }

  @Test
  public void abortBulkProcessTest_WithoutPendingProcess() throws Exception {
    BulkProcess bulkProcess = getBulkProcess().get(1);
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(null);
    bulkProcessServiceBean.abortBulkProcess(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
  }

  @Test
  public void abortPendingInprogressBulkProcess_WithPendingProcess() throws Exception {
    final BulkProcess bulkProcess = getBulkProcess().get(0);
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    bulkProcessServiceBean.abortPendingInprogressBulkProcess(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessRepository).save(bulkProcess);
  }

  @Test
  public void abortPendingInprogressBulkProcess_WithInProgressProcess() throws Exception {
    final BulkProcess bulkProcess = getBulkProcess().get(0);
    bulkProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    bulkProcessServiceBean.abortPendingInprogressBulkProcess(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessRepository).save(bulkProcess);
  }

  @Test
  public void abortPendingInprogressBulkProcess_WithNullProcess() throws Exception {
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(null);
    try {
      assertThrows(ApplicationException.class,
          () -> bulkProcessServiceBean.abortPendingInprogressBulkProcess(DEFAULT_STORE_ID,
              DEFAULT_BULK_PROCESS_CODE));
    } finally {
      Mockito.verify(bulkProcessRepository)
          .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    }
  }

  @Test
  public void abortPendingInprogressBulkProcess_WithoutPendingProcess() throws Exception {
    BulkProcess bulkProcess = getBulkProcess().get(1);
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    Mockito.when(
        bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    try {
      assertThrows(ApplicationException.class,
          () -> bulkProcessServiceBean.abortPendingInprogressBulkProcess(DEFAULT_STORE_ID,
              DEFAULT_BULK_PROCESS_CODE));
    } finally {
      Mockito.verify(bulkProcessRepository)
          .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
              DEFAULT_BULK_PROCESS_CODE);
    }
  }

  private List<BulkProcess> getPendingBulkProcesses() {
    List<BulkProcess> pendingBulkProcess = new ArrayList<>();
    pendingBulkProcess.add(
        new BulkProcess(DEFAULT_BULK_PROCESS_CODE, "ProductLevel3", DEFAULT_BUSINESS_PARTNER_CODE, null, null,
            BulkProcess.STATUS_PENDING, null, new ArrayList<BulkProcessNotes>()));
    pendingBulkProcess.add(
        new BulkProcess(UUID.randomUUID().toString(), "ProductLevel3", DEFAULT_BUSINESS_PARTNER_CODE, null, null,
            BulkProcess.STATUS_PENDING, null, new ArrayList<BulkProcessNotes>()));
    return pendingBulkProcess;
  }

  @Test
  public void checkForPendingBulkCncProcessByBusinessPartnerCodeTest_withBulkUpdateStatusFlagFalse() throws Exception {
    Mockito.when(
        this.bulkProcessRepository.countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(
            DEFAULT_STORE_ID, BULK_UPDATE_MERCHANT_CODE,
          Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
            BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
            BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_PROCESSED, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
            BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2),
          ImmutableSet.of(Constant.INSTANT_PICKUP_PRODUCT))).thenReturn(COUNT_FALSE);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE);
    systemParameterConfig.setValue(String.valueOf(false));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.DISABLE_BULK_CNC_UPDATES_VARIABLE)).thenReturn(systemParameterConfig);
    BulkPendingRequestsResponse pendingRequestsResponse =
        this.bulkProcessServiceBean.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            Constant.BULK_UPLOAD_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            SystemParameterConfigNames.DISABLE_BULK_CNC_UPDATES_VARIABLE, Constant.INSTANT_PICKUP_PRODUCT);
    Mockito.verify(this.bulkProcessRepository)
        .countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
            BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
            BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_PROCESSED, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
            BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2),
          ImmutableSet.of(Constant.INSTANT_PICKUP_PRODUCT));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.DISABLE_BULK_CNC_UPDATES_VARIABLE);
    Assertions.assertFalse(pendingRequestsResponse.isBulkUpdateStatusFlag());
    Assertions.assertEquals(COUNT_FALSE, pendingRequestsResponse.getPendingRequestsCount());
  }

  @Test
  public void checkForPendingBulkCncProcessByBusinessPartnerCodeTest_withBulkUpdateStatusFlagTrue() throws Exception {
    Mockito.when(
        this.bulkProcessRepository.countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(
            DEFAULT_STORE_ID, BULK_UPDATE_MERCHANT_CODE,
          Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
            BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
            BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_PROCESSED,
            BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
            BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2),
          ImmutableSet.of(Constant.INSTANT_PICKUP_PRODUCT))).thenReturn(COUNT_TRUE);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE);
    systemParameterConfig.setValue(String.valueOf(false));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.DISABLE_BULK_CNC_UPDATES_VARIABLE)).thenReturn(systemParameterConfig);
    BulkPendingRequestsResponse pendingRequestsResponse =
        this.bulkProcessServiceBean.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            Constant.BULK_UPLOAD_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            SystemParameterConfigNames.DISABLE_BULK_CNC_UPDATES_VARIABLE, Constant.INSTANT_PICKUP_PRODUCT);
    Mockito.verify(this.bulkProcessRepository)
        .countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
            BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
            BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_PROCESSED, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
            BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2),
          ImmutableSet.of(Constant.INSTANT_PICKUP_PRODUCT));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.DISABLE_BULK_CNC_UPDATES_VARIABLE);
    assertTrue(pendingRequestsResponse.isBulkUpdateStatusFlag());
    Assertions.assertEquals(COUNT_TRUE, pendingRequestsResponse.getPendingRequestsCount());
  }

  @Test
  public void filterPromoBulkProcessNotesTest() throws Exception {
    BulkProcess bulkProcess = getBulkProcess().get(0);
    bulkProcessNotes.setBulkProcess(bulkProcess);
    bulkProcessNotes.setPromoNote(true);
    bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
    Mockito.when(this.bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    List<BulkProcessNotesResponse> response =
        bulkProcessServiceBean.filterPromoBulkProcessNotes(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(this.bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Assertions.assertFalse(CollectionUtils.isEmpty(response));
    Assertions.assertEquals(DEFAULT_BULK_NOTES, response.get(0).getNotes());
  }

  @Test
  public void filterPromoBulkProcessNotesTest_emptyNotes() throws Exception {
    BulkProcess bulkProcess = getBulkProcess().get(0);
    bulkProcessNotes.setBulkProcess(bulkProcess);
    bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
    Mockito.when(this.bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    List<BulkProcessNotesResponse> response =
        bulkProcessServiceBean.filterPromoBulkProcessNotes(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(this.bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    assertTrue(CollectionUtils.isEmpty(response));
  }

  @Test
  public void regenerateCategoryAttributeMappingInGenericBulkTemplateTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedUploadTemplateFile",
        GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + ","
            + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + ","
            + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedUploadTemplateFileEnglish",
        GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + ","
            + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + ","
            + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedBaseTemplateFile",
        GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + ","
            + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + ","
            + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH + "," + GENERIC_TEMPLATE_3_PATH);
    File file = new File(GENERIC_TEMPLATE_3_PATH);
    this.workbook = new XSSFWorkbook(file);
    Mockito.when(fileStorageService.getUnifiedBaseTemplate(Mockito.anyString(),
      Mockito.anyString(),Mockito.anyBoolean())).thenReturn(workbook);
    when(fileStorageService.isMassTemplateFileExist(GENERIC_TEMPLATE_3_PATH)).thenReturn(true);
    this.bulkProcessServiceBean
        .regenerateCategoryAttributeMappingInGenericBulkTemplate();
    Mockito.verify(this.pcbOutboundService, times(5)).getGenericTemplateCategories(GENERIC_TEMPLATE_ELIGIBLE,
      true);
    Mockito.verify(this.pcbOutboundService, times(4)).getGenericTemplateCategories(GENERIC_TEMPLATE_ELIGIBLE,
      false);
    Mockito.verify(this.pcbOutboundService, times(40)).getCategoryDetailResponse(Mockito.anyString());
    Mockito.verify(this.pcbOutboundService, times(30)).getAttributeDetail(Mockito.anyString());
    Mockito.verify(this.kafkaEventLogService).getKafkaEventLogs();
    Mockito.verify(this.kafkaEventLogService).deleteKafkaEventLogsMarkForDelete(kafkaEventLogList);
    verify(fileStorageService, times(9)).isMassTemplateFileExist(GENERIC_TEMPLATE_3_PATH);
    verify(fileStorageService, times(18)).getUnifiedBaseTemplate(Mockito.anyString(),
      Mockito.anyString(),Mockito.anyBoolean());
    Mockito.verify(this.systemParameterConfigService)
        .update(Mockito.eq(Constant.STORE_ID), Mockito.eq(Constant.USER_NAME),
            systemParameterConfigRequestArgumentCaptor.capture());
    Assertions.assertEquals(SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP,
        systemParameterConfigRequestArgumentCaptor.getValue().getVariable());
    XSSFWorkbook workBook;
    if (new File(UNIFIED_TEMPLATE_UPLOAD_PATH).exists()) {
      try (InputStream is = new BufferedInputStream(new FileInputStream(UNIFIED_TEMPLATE_UPLOAD_PATH))) {
        workBook = new XSSFWorkbook(is);
        XSSFSheet sheet = workBook.getSheet(CATEGORY_ATTRIBUTE_SHEET);
        Assertions.assertEquals(CATEGORY_ATTRIBUTE_SHEET_INDEX, workBook.getSheetIndex(CATEGORY_ATTRIBUTE_SHEET));
        for (int header = 0; header < GenericBulkParameters.HEADER_COLUMN_NUMBER.size(); header++) {
          Assertions.assertEquals(GenericBulkParameters.HEADER_COLUMN_VALUE.get(header),
              sheet.getRow(GenericBulkParameters.HEADER_ROW)
                  .getCell(GenericBulkParameters.HEADER_COLUMN_NUMBER.get(header)).getStringCellValue());
        }
        for (int subHeader = 0; subHeader < GenericBulkParameters.SUB_HEADER_COLUMN_NUMBER.size(); subHeader++) {
          Assertions.assertEquals(GenericBulkParameters.SUB_HEADER_COLUMN_VALUE.get(subHeader),
              sheet.getRow(GenericBulkParameters.SUB_HEADER_ROW)
                  .getCell(GenericBulkParameters.SUB_HEADER_COLUMN_NUMBER.get(subHeader)).getStringCellValue());
        }
        for (int familyColour = 0; familyColour < GenericBulkParameters.FAMILY_COLOUR_VALUE.size(); familyColour++) {
          Assertions.assertEquals(GenericBulkParameters.FAMILY_COLOUR_VALUE.get(familyColour),
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + familyColour)
                  .getCell(GenericBulkParameters.FAMILY_COLOUR_DATA_COLUMN).getStringCellValue());
        }
        for (int productType = 0; productType < BulkParameters.BULK_OPTION_UPLOAD_SUPPORT.size(); productType++) {
          Assertions.assertEquals(BulkParameters.BULK_OPTION_UPLOAD_SUPPORT.get(productType),
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + productType)
                  .getCell(GenericBulkParameters.HANDLING_TYPE_HEADER_COLUMN).getStringCellValue());
        }
        for (int warnaFamilyColourMapping = 0;
            warnaFamilyColourMapping < GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING
                .size(); warnaFamilyColourMapping++) {
          Assertions.assertEquals(
              GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING.get(warnaFamilyColourMapping).split(Constant.COMMA)[0],
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + warnaFamilyColourMapping)
                  .getCell(GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING_HEADER_COLUMN).getStringCellValue());
          Assertions.assertEquals(
              GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING.get(warnaFamilyColourMapping).split(Constant.COMMA)[1],
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + warnaFamilyColourMapping)
                  .getCell(GenericBulkParameters.FAMILY_COLOUR_MAPPING_HEADER_COLUMN).getStringCellValue());
        }
        Assertions.assertEquals(CATEGORY_C1_C12_MAPPING,
            sheet.getRow(5).getCell(GenericBulkParameters.C1_CATEGORY).getStringCellValue());
        Assertions.assertEquals(GenericBulkParameters.NOT_APPLICABLE,
            sheet.getRow(5).getCell(GenericBulkParameters.C2_CATEGORY).getStringCellValue());
      }
    }
  }

  @Test
  public void regenerateCategoryAttributeMappingFromAlreadyExistingGenericBulkTemplateTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedUploadTemplateFile",
        GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedUploadTemplateFileEnglish",
        GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedBaseTemplateFile",
        GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH);

    File file = new File(GENERIC_TEMPLATE_1_PATH);
    this.workbook = new XSSFWorkbook(file);
    Mockito.when(fileStorageService.getUnifiedBaseTemplate(Mockito.anyString(),
        Mockito.anyString(),Mockito.anyBoolean())).thenReturn(workbook);
    when(fileStorageService.isMassTemplateFileExist(GENERIC_TEMPLATE_1_PATH)).thenReturn(true);
    this.bulkProcessServiceBean
        .regenerateCategoryAttributeMappingInGenericBulkTemplate();
    Mockito.verify(this.pcbOutboundService, times(5)).getGenericTemplateCategories(GENERIC_TEMPLATE_ELIGIBLE,
      true);
    Mockito.verify(this.pcbOutboundService, times(4)).getGenericTemplateCategories(GENERIC_TEMPLATE_ELIGIBLE,
      false);
    Mockito.verify(this.pcbOutboundService, times(40)).getCategoryDetailResponse(Mockito.anyString());
    Mockito.verify(this.pcbOutboundService, times(30)).getAttributeDetail(Mockito.anyString());
    Mockito.verify(this.kafkaEventLogService).getKafkaEventLogs();
    verify(fileStorageService, times(18)).getUnifiedBaseTemplate(Mockito.anyString(),
        Mockito.anyString(),Mockito.anyBoolean());
    Mockito.verify(this.kafkaEventLogService).deleteKafkaEventLogsMarkForDelete(kafkaEventLogList);
    Mockito.verify(this.systemParameterConfigService)
        .update(Mockito.eq(Constant.STORE_ID), Mockito.eq(Constant.USER_NAME),
            systemParameterConfigRequestArgumentCaptor.capture());
    Assertions.assertEquals(SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP,
        systemParameterConfigRequestArgumentCaptor.getValue().getVariable());
    XSSFWorkbook workBook;
    if (new File(GENERIC_TEMPLATE_1_PATH).exists()) {
      try (InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
        workBook = new XSSFWorkbook(is);
        XSSFSheet sheet = workBook.getSheet(CATEGORY_ATTRIBUTE_SHEET);
        assertTrue(workBook.isSheetVeryHidden(GenericBulkParameters.CATEGORY_ATTRIBUTE_SHEET_INDEX));
        Assertions.assertEquals(CATEGORY_ATTRIBUTE_SHEET_INDEX, workBook.getSheetIndex(CATEGORY_ATTRIBUTE_SHEET));
        for (int header = 0; header < GenericBulkParameters.HEADER_COLUMN_NUMBER.size(); header++) {
          Assertions.assertEquals(GenericBulkParameters.HEADER_COLUMN_VALUE.get(header),
              sheet.getRow(GenericBulkParameters.HEADER_ROW)
                  .getCell(GenericBulkParameters.HEADER_COLUMN_NUMBER.get(header)).getStringCellValue());
        }
        for (int subHeader = 0; subHeader < GenericBulkParameters.SUB_HEADER_COLUMN_NUMBER.size(); subHeader++) {
          Assertions.assertEquals(GenericBulkParameters.SUB_HEADER_COLUMN_VALUE.get(subHeader),
              sheet.getRow(GenericBulkParameters.SUB_HEADER_ROW)
                  .getCell(GenericBulkParameters.SUB_HEADER_COLUMN_NUMBER.get(subHeader)).getStringCellValue());
        }
        for (int familyColour = 0; familyColour < GenericBulkParameters.FAMILY_COLOUR_VALUE.size(); familyColour++) {
          Assertions.assertEquals(GenericBulkParameters.FAMILY_COLOUR_VALUE.get(familyColour),
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + familyColour)
                  .getCell(GenericBulkParameters.FAMILY_COLOUR_DATA_COLUMN).getStringCellValue());
        }
        for (int productType = 0; productType < BulkParameters.BULK_OPTION_UPLOAD_SUPPORT.size(); productType++) {
          Assertions.assertEquals(BulkParameters.BULK_OPTION_UPLOAD_SUPPORT.get(productType),
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + productType)
                  .getCell(GenericBulkParameters.HANDLING_TYPE_HEADER_COLUMN).getStringCellValue());
        }
        for (int warnaFamilyColourMapping = 0;
            warnaFamilyColourMapping < GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING
                .size(); warnaFamilyColourMapping++) {
          Assertions.assertEquals(
              GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING.get(warnaFamilyColourMapping).split(Constant.COMMA)[0],
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + warnaFamilyColourMapping)
                  .getCell(GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING_HEADER_COLUMN).getStringCellValue());
          Assertions.assertEquals(
              GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING.get(warnaFamilyColourMapping).split(Constant.COMMA)[1],
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + warnaFamilyColourMapping)
                  .getCell(GenericBulkParameters.FAMILY_COLOUR_MAPPING_HEADER_COLUMN).getStringCellValue());
        }
      }
    }
  }

  @Test
  public void regenerateCategoryAttributeMappingInGenericBulkTemplateNoChangeEventTest() throws Exception {
    Mockito.when(this.kafkaEventLogService.getKafkaEventLogs()).thenReturn(new ArrayList<>());
    this.bulkProcessServiceBean
        .regenerateCategoryAttributeMappingInGenericBulkTemplate();
    Mockito.verify(this.kafkaEventLogService).getKafkaEventLogs();
  }

  @Test
  public void regenerateCategoryAttributeMappingInGenericBulkTemplateTest2() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedUploadTemplateFile",
        GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + ","
            + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + ","
            + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedUploadTemplateFileEnglish",
        GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + ","
            + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + ","
            + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedBaseTemplateFile",
        GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + ","
            + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + ","
            + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH + "," + GENERIC_TEMPLATE_2_PATH);
    File file = new File(GENERIC_TEMPLATE_2_PATH);
    this.workbook = new XSSFWorkbook(file);
    Mockito.when(fileStorageService.getUnifiedBaseTemplate(Mockito.anyString(),
      Mockito.anyString(),Mockito.anyBoolean())).thenReturn(workbook);
    when(fileStorageService.isMassTemplateFileExist(GENERIC_TEMPLATE_2_PATH)).thenReturn(false);
    this.bulkProcessServiceBean
        .regenerateCategoryAttributeMappingInGenericBulkTemplate();
    Mockito.verify(this.pcbOutboundService, times(5)).getGenericTemplateCategories(GENERIC_TEMPLATE_ELIGIBLE, true);
    Mockito.verify(this.pcbOutboundService, times(4)).getGenericTemplateCategories(GENERIC_TEMPLATE_ELIGIBLE, false);
    Mockito.verify(this.pcbOutboundService, times(40)).getCategoryDetailResponse(Mockito.anyString());
    Mockito.verify(this.pcbOutboundService, times(30)).getAttributeDetail(Mockito.anyString());
    Mockito.verify(this.kafkaEventLogService).getKafkaEventLogs();
    verify(fileStorageService, times(9)).isMassTemplateFileExist(GENERIC_TEMPLATE_2_PATH);
    verify(fileStorageService, times(18)).getUnifiedBaseTemplate(Mockito.anyString(),
      Mockito.anyString(),Mockito.anyBoolean());
    Mockito.verify(this.kafkaEventLogService).deleteKafkaEventLogsMarkForDelete(kafkaEventLogList);
    Mockito.verify(this.systemParameterConfigService)
        .update(Mockito.eq(Constant.STORE_ID), Mockito.eq(Constant.USER_NAME),
            systemParameterConfigRequestArgumentCaptor.capture());
    Assertions.assertEquals(SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP,
        systemParameterConfigRequestArgumentCaptor.getValue().getVariable());
    XSSFWorkbook workBook;
    if (new File(GENERIC_TEMPLATE_1_PATH).exists()) {
      try (InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
        workBook = new XSSFWorkbook(is);
        XSSFSheet sheet = workBook.getSheet(CATEGORY_ATTRIBUTE_SHEET);
        assertTrue(workBook.isSheetVeryHidden(GenericBulkParameters.CATEGORY_ATTRIBUTE_SHEET_INDEX));
        Assertions.assertEquals(CATEGORY_ATTRIBUTE_SHEET_INDEX, workBook.getSheetIndex(CATEGORY_ATTRIBUTE_SHEET));
        for (int header = 0; header < GenericBulkParameters.HEADER_COLUMN_NUMBER.size(); header++) {
          Assertions.assertEquals(GenericBulkParameters.HEADER_COLUMN_VALUE.get(header),
              sheet.getRow(GenericBulkParameters.HEADER_ROW)
                  .getCell(GenericBulkParameters.HEADER_COLUMN_NUMBER.get(header)).getStringCellValue());
        }
        for (int subHeader = 0; subHeader < GenericBulkParameters.SUB_HEADER_COLUMN_NUMBER.size(); subHeader++) {
          Assertions.assertEquals(GenericBulkParameters.SUB_HEADER_COLUMN_VALUE.get(subHeader),
              sheet.getRow(GenericBulkParameters.SUB_HEADER_ROW)
                  .getCell(GenericBulkParameters.SUB_HEADER_COLUMN_NUMBER.get(subHeader)).getStringCellValue());
        }
        for (int familyColour = 0; familyColour < GenericBulkParameters.FAMILY_COLOUR_VALUE.size(); familyColour++) {
          Assertions.assertEquals(GenericBulkParameters.FAMILY_COLOUR_VALUE.get(familyColour),
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + familyColour)
                  .getCell(GenericBulkParameters.FAMILY_COLOUR_DATA_COLUMN).getStringCellValue());
        }
        for (int productType = 0; productType < BulkParameters.BULK_OPTION_UPLOAD_SUPPORT.size(); productType++) {
          Assertions.assertEquals(BulkParameters.BULK_OPTION_UPLOAD_SUPPORT.get(productType),
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + productType)
                  .getCell(GenericBulkParameters.HANDLING_TYPE_HEADER_COLUMN).getStringCellValue());
        }
        for (int warnaFamilyColourMapping = 0;
            warnaFamilyColourMapping < GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING
                .size(); warnaFamilyColourMapping++) {
          Assertions.assertEquals(
              GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING.get(warnaFamilyColourMapping).split(Constant.COMMA)[0],
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + warnaFamilyColourMapping)
                  .getCell(GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING_HEADER_COLUMN).getStringCellValue());
          Assertions.assertEquals(
              GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING.get(warnaFamilyColourMapping).split(Constant.COMMA)[1],
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + warnaFamilyColourMapping)
                  .getCell(GenericBulkParameters.FAMILY_COLOUR_MAPPING_HEADER_COLUMN).getStringCellValue());
        }
      }
    }
  }

  @Test
  public void regenerateBrandValuesInGenericBulkTemplateExceptionTest() throws Exception {
    String imagePath = classLoader.getResource("UnifiedBulk").getPath();
    ReflectionTestUtils.setField(bulkProcessServiceBean, "blibliMassTemplateLocation", imagePath);
    Mockito.doReturn(genericTemplateCategories).when(pcbOutboundService).getGenericTemplateCategories(true,
      true);
    Mockito.when(pbpOutboundService.getActiveBrandsByCategoryId(CATEGORY_ID))
        .thenReturn(predefinedAllowedAttributeValueResponses);
    Mockito.when(pcbOutboundService.getInReviewBrands(Mockito.any(BrandWipSummaryRequest.class)))
        .thenThrow(new ApplicationRuntimeException());
    bulkProcessServiceBean.regenerateBrandValuesInGenericBulkTemplate(GenericTemplateFileType.DEFAULT_FILE.name());
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pbpOutboundService).getActiveBrandsByCategoryId(CATEGORY_ID);
    Mockito.verify(pcbOutboundService).getInReviewBrands(Mockito.any(BrandWipSummaryRequest.class));
  }
  @Test
  public void regenerateBrandValuesInGenericBulkTemplateTest() throws Exception {
    InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH));
    XSSFWorkbook workBook = new XSSFWorkbook(is);
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    workBook.write(baos);
    byte[] xls = baos.toByteArray();
    byte[] xlsEn = baos.toByteArray();
    Map<String, byte[]> data = new HashMap<>();
    data.put(Constant.CATEGORY_UPLOAD_TEMPLATE,xls);
    data.put(Constant.CATEGORY_UPLOAD_TEMPLATE_EN, xlsEn);
    String imagePath = classLoader.getResource("UnifiedBulk").getPath();
    ReflectionTestUtils.setField(bulkProcessServiceBean, "blibliMassTemplateLocation", imagePath);
    Mockito.doReturn(genericTemplateCategories).when(pcbOutboundService).getGenericTemplateCategories(true,
      true);
    Mockito.when(fileStorageService.regenerateMasterBrandValuesSheet(anyString())).thenReturn(data);
    Mockito.doNothing().when(fileStorageService).uploadRegeneratedTemplates(any(),any(),
      anyString(),anyBoolean());
    Mockito.when(pbpOutboundService.getActiveBrandsByCategoryId(CATEGORY_ID))
        .thenReturn(predefinedAllowedAttributeValueResponses);
    Mockito.when(pcbOutboundService.getInReviewBrands(Mockito.any(BrandWipSummaryRequest.class)))
        .thenReturn(inReviewBrandsResponse);
    bulkProcessServiceBean.regenerateBrandValuesInGenericBulkTemplate(GenericTemplateFileType.DEFAULT_FILE.name());
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pbpOutboundService).getActiveBrandsByCategoryId(CATEGORY_ID);
    Mockito.verify(pcbOutboundService).getInReviewBrands(Mockito.any(BrandWipSummaryRequest.class));
  }

  private void setMdcParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
  }

  @Test
  public void filterWholesaleConfigBulkProcessNotesTest() throws Exception {
    BulkProcess bulkProcess = getBulkProcessWithWholeSaleConfig();
    Mockito.when(this.bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    WholeSaleCountResponse response =
        bulkProcessServiceBean.filterWholeSaleConfigBulkProcessNotes(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(this.bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Assertions.assertEquals(DEFAULT_PATH, response.getDownloadFilePath());
    Assertions.assertEquals(10, response.getWholeSaleUpdatedCount());
    Assertions.assertEquals(10, response.getWholeSaleFailedCount());
    Assertions.assertEquals(10, response.getWholeSaleTurnOffCount());
  }

  @Test
  public void regenerateMasterBrandValuesInGenericBulkTemplateExceptionTest() throws Exception {
    Mockito.when(pcbOutboundService.getInReviewBrands(Mockito.any(BrandWipSummaryRequest.class)))
        .thenThrow(new ApplicationRuntimeException());
    Mockito.when(pcbOutboundService.getAttributeByNameStartingWithAndPageable(Constant.ATTRIBUTE_NAME_BRAND))
        .thenReturn(attributeResponseList);
    Mockito.when(pcbOutboundService.getAttributeDetail(ATTRIBUTE_ID1)).thenReturn(attributeResponse1);
    this.bulkProcessServiceBean
        .regenerateMasterBrandValuesInGenericBulkTemplate(GenericTemplateFileType.DEFAULT_FILE.name());
    Mockito.verify(pcbOutboundService).getAttributeDetail(ATTRIBUTE_ID1);
    Mockito.verify(pcbOutboundService).getAttributeByNameStartingWithAndPageable(Constant.ATTRIBUTE_NAME_BRAND);
    Mockito.verify(pcbOutboundService).getInReviewBrands(Mockito.any(BrandWipSummaryRequest.class));
  }

  @Test
  public void downloadUnmappedProductSkusTest() throws Exception {
    Map<String, List<String>> catMap = new HashMap<>();
    catMap.put("C3-3", Arrays.asList("C3-3 Name", "C2-3 Name", "C1"));
    catMap.put("CategoryCode2", Arrays.asList("C12", "C1"));
    catMap.put("CategoryCode1", Arrays.asList("C111", "C11", "C1"));

    Workbook workbook = new SXSSFWorkbook();
    workbook.createSheet(Constant.UNMAPPED_PRODUCT);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UNMAPPED_SKU_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(pcbOutboundService.getCategoryTree(CATEGORY_CODE1))
        .thenReturn(genericTemplateCategoryResponseList.get(0));
    Mockito
        .when(xProductOutboundService.getUnmappedSkus(Arrays.asList(CATEGORY_CODE2, CATEGORY_CODE1, CATEGORY_CODE3_3)))
        .thenReturn(unmappedSkuResponses);
    Mockito.doNothing().when(mailDeliveryService)
        .sendBulkDownloadEmail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any());
    Mockito.when(poiUtil.createUnmappedSkusSheetWithData(Mockito.eq(DEFAULT_USERNAME), Mockito.eq(REQUEST_ID_2),
        Mockito.eq(CATEGORY_NAME1), Mockito.eq(3), Mockito.anyMap(), Mockito.eq(unmappedSkuResponses)))
        .thenReturn(workbook);
    bulkProcessServiceBean
        .downloadUnmappedProductSkus(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_USERNAME, REQUEST_ID_2, CATEGORY_CODE1,
            "EN");
    Mockito.verify(pcbOutboundService).getCategoryTree(CATEGORY_CODE1);
    Mockito.verify(mailDeliveryService)
        .sendBulkDownloadEmail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UNMAPPED_SKU_FETCH_BATCH_SIZE);
  }

  @Test
  public void downloadUnmappedProductSkusIdTest() throws Exception {
    Map<String, List<String>> catMap = new HashMap<>();
    catMap.put("C3-3", Arrays.asList("C3-3 Name", "C2-3 Name", "C1"));
    catMap.put("CategoryCode2", Arrays.asList("C12", "C1"));
    catMap.put("CategoryCode1", Arrays.asList("C111", "C11", "C1"));

    Workbook workbook = new SXSSFWorkbook();
    workbook.createSheet(Constant.UNMAPPED_PRODUCT);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UNMAPPED_SKU_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(pcbOutboundService.getCategoryTree(CATEGORY_CODE1))
        .thenReturn(genericTemplateCategoryResponseList.get(0));
    Mockito
        .when(xProductOutboundService.getUnmappedSkus(Arrays.asList(CATEGORY_CODE2, CATEGORY_CODE1, CATEGORY_CODE3_3)))
        .thenReturn(unmappedSkuResponses);
    Mockito.doNothing().when(mailDeliveryService)
        .sendBulkDownloadEmail(Mockito.eq(PRODUCT_CENTER_DOWNLOAD_ID), Mockito.eq("no-reply@blibli.com"),
            Mockito.eq("Produk Download"), Mockito.any(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any());
    Mockito.when(poiUtil.createUnmappedSkusSheetWithData(Mockito.eq(DEFAULT_USERNAME), Mockito.eq(REQUEST_ID_2),
        Mockito.eq(CATEGORY_NAME1), Mockito.eq(3), Mockito.anyMap(), Mockito.eq(unmappedSkuResponses)))
        .thenReturn(workbook);
    bulkProcessServiceBean
        .downloadUnmappedProductSkus(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_USERNAME, REQUEST_ID_2, CATEGORY_CODE1,
            "ID");
    Mockito.verify(pcbOutboundService).getCategoryTree(CATEGORY_CODE1);
    Mockito.verify(mailDeliveryService)
        .sendBulkDownloadEmail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UNMAPPED_SKU_FETCH_BATCH_SIZE);
  }

  @Test
  public void downloadUnmappedProductSkusWithoutDataTest() throws Exception {
    Workbook workbook = new SXSSFWorkbook();
    workbook.createSheet(Constant.UNMAPPED_PRODUCT);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(BATCH_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UNMAPPED_SKU_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(pcbOutboundService.getCategoryTree(CATEGORY_CODE1))
        .thenReturn(genericTemplateCategoryResponseList.get(0));
    Mockito
        .when(xProductOutboundService.getUnmappedSkus(Arrays.asList(CATEGORY_CODE2, CATEGORY_CODE1, CATEGORY_CODE3_3)))
        .thenReturn(new ArrayList<>());
    Mockito.doNothing().when(mailDeliveryService)
        .sendBulkDownloadEmail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any());
    Mockito.when(poiUtil.createUnamappedProductSkusSheetWithoutData(DEFAULT_USERNAME, REQUEST_ID, CATEGORY_NAME1))
        .thenReturn(workbook);
    bulkProcessServiceBean
        .downloadUnmappedProductSkus(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_USERNAME, REQUEST_ID, CATEGORY_CODE1,
            "ID");
    Mockito.verify(pcbOutboundService).getCategoryTree(CATEGORY_CODE1);
    Mockito.verify(mailDeliveryService)
        .sendBulkDownloadEmail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UNMAPPED_SKU_FETCH_BATCH_SIZE);
  }

  @Test
  public void regenerateBrandValuesInCategoryTemplateTest() throws Exception {
    Map<String, byte[]> map = new HashMap<>();
    File file = new File(BASE_DIRECTORY + "CategoryTemplate/products-template.xlsx");
    File fileEn = new File(BASE_DIRECTORY + "CategoryTemplate/products-template-English.xlsx");
    map.put(Constant.CATEGORY_UPLOAD_TEMPLATE, FileUtils.readFileToByteArray(file));
    map.put(Constant.CATEGORY_UPLOAD_TEMPLATE_EN, FileUtils.readFileToByteArray(fileEn));
    Mockito.when(pcbOutboundService.getInReviewBrands(Mockito.any(BrandWipSummaryRequest.class)))
        .thenReturn(inReviewBrandsResponse);
    Mockito.when(pcbOutboundService.getAttributeByNameStartingWithAndPageable(Constant.ATTRIBUTE_NAME_BRAND))
      .thenReturn(attributeResponseList);
    Mockito.when(fileStorageService.isCategoryTemplateFileExist(anyString(),anyString(),
      anyString(),anyString())).thenReturn(map);
    Mockito.when(fileStorageService.getCategoryTemplateFilePath()).thenReturn(BASE_DIRECTORY);
    Mockito.when(pcbOutboundService.getAttributeDetail(ATTRIBUTE_ID1)).thenReturn(attributeResponse1);
    this.bulkProcessServiceBean.regenerateBrandValuesInCategoryTemplate();
    Mockito.verify(pcbOutboundService).getAttributeDetail(ATTRIBUTE_ID1);
    Mockito.verify(pcbOutboundService).getAttributeByNameStartingWithAndPageable(Constant.ATTRIBUTE_NAME_BRAND);
    Mockito.verify(pcbOutboundService).getInReviewBrands(Mockito.any(BrandWipSummaryRequest.class));
    Mockito.verify(systemParameterConfigService)
        .update(eq(Constant.STORE_ID), eq(Constant.USER_NAME), Mockito.any(SystemParameterConfigRequest.class));
    Mockito.verify(fileStorageService).isCategoryTemplateFileExist("products-upload-template.xlsx",
      "products-template-upload-English.xlsx",
      "products-template.xlsx",
      "products-template-English.xlsx");
  }

  @Test
  public void regenerateBrandValuesInCategoryTemplateExceptionTest() {
    Mockito.when(pcbOutboundService.getInReviewBrands(Mockito.any(BrandWipSummaryRequest.class)))
        .thenThrow(new ApplicationRuntimeException());
    Mockito.when(pcbOutboundService.getAttributeByNameStartingWithAndPageable(Constant.ATTRIBUTE_NAME_BRAND))
        .thenReturn(attributeResponseList);
    Mockito.when(pcbOutboundService.getAttributeDetail(ATTRIBUTE_ID1)).thenReturn(attributeResponse1);
    this.bulkProcessServiceBean.regenerateBrandValuesInCategoryTemplate();
    Mockito.verify(pcbOutboundService).getAttributeDetail(ATTRIBUTE_ID1);
    Mockito.verify(pcbOutboundService).getAttributeByNameStartingWithAndPageable(Constant.ATTRIBUTE_NAME_BRAND);
    Mockito.verify(pcbOutboundService).getInReviewBrands(Mockito.any(BrandWipSummaryRequest.class));
  }

  @Test
  public void countNumberOfUploadsByUserTest() {
    Mockito.when(bulkProcessRepository
        .countByStoreIdAndBulkProcessTypeAndCreatedByAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            BulkProcessType.VENDOR_BULK_ASSIGN.getValue(), DEFAULT_USERNAME, BulkProcess.STATUS_PENDING))
        .thenReturn(1L);
    long count = this.bulkProcessServiceBean
        .countNumberOfUploadsByUser(DEFAULT_STORE_ID, BulkProcessType.VENDOR_BULK_ASSIGN.getValue(), DEFAULT_USERNAME,
            BulkProcess.STATUS_PENDING);
    Mockito.verify(bulkProcessRepository)
        .countByStoreIdAndBulkProcessTypeAndCreatedByAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            BulkProcessType.VENDOR_BULK_ASSIGN.getValue(), DEFAULT_USERNAME, BulkProcess.STATUS_PENDING);
    Assertions.assertEquals(1L, count);
  }

  @Test
  public void sendMailFailIfNoPendingRequestAvaliableTest() throws Exception {
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS))
        .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MAILS_FOR_PENDING_BULK_REQUESTS)).thenReturn(mailTo);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS))
        .thenReturn(thresholdAbortedBulkRequests);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES))
        .thenReturn(abortedRequestsRequired);
    Mockito.when(this.bulkProcessRepository.getBulkPendingRequest(Constant.STORE_ID)).thenReturn(new ArrayList<>());
    Mockito.when(this.bulkProcessRepository.getBulkInProgressRequest(Constant.STORE_ID)).thenReturn(new ArrayList<>());
    bulkProcessServiceBean.sendMailIfPendingRequestMoreThanThreshold(Constant.STORE_ID);
    Mockito.verify(this.bulkProcessRepository).getBulkPendingRequest(Constant.STORE_ID);
    Mockito.verify(this.bulkProcessRepository).getBulkInProgressRequest(Constant.STORE_ID);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MAILS_FOR_PENDING_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES);
    Mockito.verify(this.bulkProcessRepository).getBulkAbortedRequest(eq(Constant.STORE_ID), any());
  }

  @Test
  public void sendMailIfPendingRequestMoreThanThresholdFailedTest() throws Exception {
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS))
        .thenReturn(thresholdPendingBulkRequestsFailedValue);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.MAILS_FOR_PENDING_BULK_REQUESTS)).thenReturn(mailTo);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            Constant.THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS))
        .thenReturn(thresholdAbortedBulkRequests);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES)).thenReturn(abortedRequestsRequired);
    try {
      assertThrows(ApplicationException.class,
          () -> bulkProcessServiceBean.sendMailIfPendingRequestMoreThanThreshold(
              Constant.STORE_ID));
    } finally {
      Mockito.verify(this.systemParameterConfigService)
          .findValueByStoreIdAndVariable(Constant.STORE_ID,
              Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS);
      Mockito.verify(this.systemParameterConfigService)
          .findValueByStoreIdAndVariable(Constant.STORE_ID,
              Constant.MAILS_FOR_PENDING_BULK_REQUESTS);
      Mockito.verify(this.systemParameterConfigService)
          .findValueByStoreIdAndVariable(Constant.STORE_ID,
              Constant.THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS);
      Mockito.verify(this.systemParameterConfigService)
          .findValueByStoreIdAndVariable(Constant.STORE_ID,
              Constant.ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES);
    }
  }

  @Test
  public void sendMailFailIfPendingRequestLessThanThresholdTest() throws Exception {
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS))
        .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MAILS_FOR_PENDING_BULK_REQUESTS)).thenReturn(mailTo);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS))
        .thenReturn(thresholdAbortedBulkRequests);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES))
        .thenReturn(abortedRequestsRequired);
    Mockito.when(this.bulkProcessRepository.getBulkInProgressRequest(Constant.STORE_ID))
        .thenReturn(bulkPendingProductsDTOListNull);
    Mockito.when(this.bulkProcessRepository.getBulkPendingRequest(Constant.STORE_ID)).thenReturn(null);
    bulkProcessServiceBean.sendMailIfPendingRequestMoreThanThreshold(Constant.STORE_ID);
    Mockito.verify(this.bulkProcessRepository).getBulkPendingRequest(Constant.STORE_ID);
    Mockito.verify(this.bulkProcessRepository).getBulkInProgressRequest(Constant.STORE_ID);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MAILS_FOR_PENDING_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES);
    Mockito.verify(this.bulkProcessRepository).getBulkAbortedRequest(eq(Constant.STORE_ID), any());
  }

  @Test
  public void sendMailIfPendingRequestMoreThanThresholdTest() throws Exception {
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS))
        .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MAILS_FOR_PENDING_BULK_REQUESTS)).thenReturn(mailTo);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS))
        .thenReturn(thresholdAbortedBulkRequests);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES))
        .thenReturn(abortedRequestsRequired);
    Mockito.when(this.bulkProcessRepository.getBulkPendingRequest(Constant.STORE_ID))
        .thenReturn(bulkPendingProductsDTOList);
    Mockito.when(this.bulkProcessRepository.getBulkInProgressRequest(Constant.STORE_ID))
        .thenReturn(bulkPendingProductsDTOList);
    Mockito.when(this.bulkProcessRepository.getBulkAbortedRequest(Constant.STORE_ID, eq(any(Date.class))))
        .thenReturn(null);
    bulkProcessServiceBean.sendMailIfPendingRequestMoreThanThreshold(Constant.STORE_ID);
    Mockito.verify(this.bulkProcessRepository).getBulkPendingRequest(Constant.STORE_ID);
    Mockito.verify(this.bulkProcessRepository).getBulkInProgressRequest(Constant.STORE_ID);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MAILS_FOR_PENDING_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES);
    Mockito.verify(this.bulkProcessRepository).getBulkAbortedRequest(eq(Constant.STORE_ID), any());
    Mockito.verify(this.mailDeliveryService).sendPendingBulkRequestMail(eq(MAILS_FOR_PENDING_BULK_REQUESTS_VALUE),
        bulkPendingProductResponseArgumentCaptor.capture(), bulkPendingProductResponseArgumentCaptor.capture(),
        eq(new ArrayList<>()), eq(abortedRequestsRequired.getValue()));
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(0).getBulkProcessType(),
        BULK_PROCESS_TYPE);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(0).getBusinessPartnerCode(),
        BUSINESS_PARTNER);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(0).getStartDate(), DATE.toString());
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(0).getIsBulkUpdate(),
        IS_BULK_UPDATE.toString());
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(1).getBulkProcessType(), NULL);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(1).getBusinessPartnerCode(), NULL);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(1).getStartDate(), NULL);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(1).getIsBulkUpdate(), NULL);
  }

  @Test
  public void sendMailIfAbortedRequestMoreThanThresholdTest() throws Exception {
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS))
        .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MAILS_FOR_PENDING_BULK_REQUESTS)).thenReturn(mailTo);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS))
        .thenReturn(thresholdAbortedBulkRequests);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES))
        .thenReturn(abortedRequestsRequired);
    Mockito.when(this.bulkProcessRepository.getBulkPendingRequest(Constant.STORE_ID)).thenReturn(null);
    Mockito.when(this.bulkProcessRepository.getBulkInProgressRequest(Constant.STORE_ID)).thenReturn(null);
    Mockito.when(this.bulkProcessRepository.getBulkAbortedRequest(eq(Constant.STORE_ID), any(Date.class)))
        .thenReturn(bulkPendingProductsDTOList);
    bulkProcessServiceBean.sendMailIfPendingRequestMoreThanThreshold(Constant.STORE_ID);
    Mockito.verify(this.bulkProcessRepository).getBulkPendingRequest(Constant.STORE_ID);
    Mockito.verify(this.bulkProcessRepository).getBulkInProgressRequest(Constant.STORE_ID);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MAILS_FOR_PENDING_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES);
    Mockito.verify(this.bulkProcessRepository).getBulkAbortedRequest(eq(Constant.STORE_ID), any());
    Mockito.verify(this.mailDeliveryService)
        .sendPendingBulkRequestMail(eq(MAILS_FOR_PENDING_BULK_REQUESTS_VALUE), eq(new ArrayList<>()),
            eq(new ArrayList<>()), bulkPendingProductResponseArgumentCaptor.capture(),
            eq(abortedRequestsRequired.getValue()));
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(0).getBulkProcessType(),
        BULK_PROCESS_TYPE);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(0).getBusinessPartnerCode(),
        BUSINESS_PARTNER);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(0).getStartDate(), DATE.toString());
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(0).getIsBulkUpdate(),
        IS_BULK_UPDATE.toString());
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(1).getBulkProcessType(), NULL);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(1).getBusinessPartnerCode(), NULL);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(1).getStartDate(), NULL);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(1).getIsBulkUpdate(), NULL);
  }

  @Test
  public void sendMailIfPendingAndAbortedRequestMoreThanThresholdTest() throws Exception {
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS))
        .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MAILS_FOR_PENDING_BULK_REQUESTS)).thenReturn(mailTo);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS))
        .thenReturn(thresholdAbortedBulkRequests);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES))
        .thenReturn(abortedRequestsRequired);
    Mockito.when(this.bulkProcessRepository.getBulkPendingRequest(Constant.STORE_ID))
        .thenReturn(bulkPendingProductsDTOList);
    Mockito.when(this.bulkProcessRepository.getBulkInProgressRequest(Constant.STORE_ID))
        .thenReturn(bulkPendingProductsDTOList);
    Mockito.when(this.bulkProcessRepository.getBulkAbortedRequest(eq(Constant.STORE_ID), any(Date.class)))
        .thenReturn(bulkPendingProductsDTOList);
    bulkProcessServiceBean.sendMailIfPendingRequestMoreThanThreshold(Constant.STORE_ID);
    Mockito.verify(this.bulkProcessRepository).getBulkPendingRequest(Constant.STORE_ID);
    Mockito.verify(this.bulkProcessRepository).getBulkInProgressRequest(Constant.STORE_ID);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MAILS_FOR_PENDING_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES);
    Mockito.verify(this.bulkProcessRepository).getBulkAbortedRequest(eq(Constant.STORE_ID), any());
    Mockito.verify(this.mailDeliveryService)
        .sendPendingBulkRequestMail(eq(MAILS_FOR_PENDING_BULK_REQUESTS_VALUE), anyList(), anyList(),
            bulkPendingProductResponseArgumentCaptor.capture(), eq(abortedRequestsRequired.getValue()));
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(0).getBulkProcessType(),
        BULK_PROCESS_TYPE);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(0).getBusinessPartnerCode(),
        BUSINESS_PARTNER);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(0).getStartDate(), DATE.toString());
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(0).getIsBulkUpdate(),
        IS_BULK_UPDATE.toString());
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(1).getBulkProcessType(), NULL);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(1).getBusinessPartnerCode(), NULL);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(1).getStartDate(), NULL);
    Assertions.assertEquals(bulkPendingProductResponseArgumentCaptor.getValue().get(1).getIsBulkUpdate(), NULL);
  }

  @Test
  public void bulkUpdateOff2OnTest() throws Exception {
    List<String> response =
        bulkProcessServiceBean.bulkUpdateOff2On(stringBooleanMap, Constant.USER_NAME, Constant.REQUEST_ID);
    Mockito.verify(xProductOutboundService)
        .bulkUpdateOff2OnByProductSkus(stringBooleanMap, Constant.REQUEST_ID, Constant.USER_NAME, Boolean.TRUE);
    assertTrue(response.isEmpty());
  }

  @Test
  public void bulkUpdateOff2OnTest1() throws Exception {
    Mockito.when(xProductOutboundService
        .bulkUpdateOff2OnByProductSkus(stringBooleanMap, Constant.USER_NAME, Constant.REQUEST_ID, Boolean.TRUE))
        .thenReturn(null);
    List<String> response =
        bulkProcessServiceBean.bulkUpdateOff2On(stringBooleanMap, Constant.REQUEST_ID, Constant.USER_NAME);
    Mockito.verify(xProductOutboundService)
        .bulkUpdateOff2OnByProductSkus(stringBooleanMap, Constant.USER_NAME, Constant.REQUEST_ID, Boolean.TRUE);
    assertTrue(response.isEmpty());
  }

  @Test
  public void bulkUpdateOff2OnTest2() throws Exception {
    SimpleListStringResponse simpleListStringResponse = new SimpleListStringResponse();
    simpleListStringResponse.setValue(Collections.singletonList(PRODUCT_SKU));
    Mockito.when(xProductOutboundService
        .bulkUpdateOff2OnByProductSkus(stringBooleanMap, Constant.USER_NAME, Constant.REQUEST_ID, Boolean.TRUE))
        .thenReturn(simpleListStringResponse);
    List<String> response =
        bulkProcessServiceBean.bulkUpdateOff2On(stringBooleanMap, Constant.REQUEST_ID, Constant.USER_NAME);
    Mockito.verify(xProductOutboundService)
        .bulkUpdateOff2OnByProductSkus(stringBooleanMap, Constant.USER_NAME, Constant.REQUEST_ID, Boolean.TRUE);
    Assertions.assertEquals(PRODUCT_SKU, response.get(0));
  }

  @Test
  public void preProcessSubjectToVatUploadEvent() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(profileResponse);
    bulkProcessServiceBean.preProcessSubjectToVatUploadEvent(Constant.STORE_ID, Constant.REQUEST_ID, Constant.USER_NAME,
        BUSINESS_PARTNER, DEFAULT_BULK_PROCESS_CODE, BUSINESS_PARTNER);
    verify(bulkProcessRepository).saveAndFlush(bulkProcessCaptor.capture());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER);
    verify(kafkaProducer).send(eq(topicProperties.getSubjectToVatUploadEvent()), Mockito.any());
    verify(topicProperties, times(2)).getSubjectToVatUploadEvent();
  }

  @Test
  public void preProcessSubjectToVatUploadEventNullCompanyTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(profileResponse);
    bulkProcessServiceBean.preProcessSubjectToVatUploadEvent(Constant.STORE_ID, Constant.REQUEST_ID, Constant.USER_NAME,
        BUSINESS_PARTNER, DEFAULT_BULK_PROCESS_CODE, BUSINESS_PARTNER);
    verify(bulkProcessRepository).saveAndFlush(bulkProcessCaptor.capture());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER);
    verify(kafkaProducer).send(eq(topicProperties.getSubjectToVatUploadEvent()), Mockito.any());
    verify(topicProperties, times(2)).getSubjectToVatUploadEvent();
  }

  @Test
  public void preProcessSubjectToVatUploadProfileResponseNullTestEvent() throws Exception {
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(null);
    bulkProcessServiceBean.preProcessSubjectToVatUploadEvent(Constant.STORE_ID, Constant.REQUEST_ID, Constant.USER_NAME,
        BUSINESS_PARTNER, DEFAULT_BULK_PROCESS_CODE, BUSINESS_PARTNER);
    verify(bulkProcessRepository).saveAndFlush(bulkProcessCaptor.capture());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER);
    verify(kafkaProducer).send(eq(topicProperties.getSubjectToVatUploadEvent()), Mockito.any());
    verify(topicProperties, times(2)).getSubjectToVatUploadEvent();
  }

  @Test
  public void downloadImagesExceptionTest() throws Exception {
    BulkProcess bulkProcess = getBulkProcess_1();
    Mockito.when(bulkProcessImageService
        .findByBulkProcessCodeAndImageUrl(bulkProcess, Collections.singletonList(IMAGE_URL)))
        .thenReturn(Collections.singletonList(bulkProcessImage));
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    bulkProcessServiceBean.downloadImages(DEFAULT_BULK_PROCESS_CODE, Collections.singletonList(IMAGE_URL));
    Mockito.verify(bulkProcessImageService)
        .findByBulkProcessCodeAndImageUrl(bulkProcess, Collections.singletonList(IMAGE_URL));
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).saveBulkProcessImage(bulkProcessImageArgumentCaptor.capture());
    assertTrue(bulkProcessImageArgumentCaptor.getValue().get(0).isCompleted());
  }

  @Test
  public void downloadImagesTest() throws Exception {
    ImageDownloadResult imageDownloadResult = new ImageDownloadResult();
    ProcessorUtils.createDirectories(
        ProcessorUtils.DATA_BASE_DIR + DEFAULT_BULK_PROCESS_CODE + File.separator + ProcessorUtils.DATA_RAW_DIR);
    BulkProcess bulkProcess_1 = getBulkProcess_1();
    bulkProcess_1.setInternationalMerchant(true);
    Mockito.when(
        bulkProcessImageService.findByBulkProcessCodeAndImageUrl(bulkProcess_1, Collections.singletonList(IMAGE_URL)))
        .thenReturn(Collections.singletonList(bulkProcessImage));
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess_1);
    Mockito.when(
        fileStorageService.downloadImages(Mockito.anyString(), Mockito.anyMap(), Mockito.anyInt(),
          Mockito.anyInt(), Mockito.anySet(), Mockito.anyList(),
          Mockito.any(BulkUploadErrorCounter.class), Mockito.any(StringBuilder.class),
          Mockito.anyBoolean()))
      .thenReturn(ImageDownloadResult.builder().downloadSuccess(true).build());
    bulkProcessServiceBean.downloadImages(DEFAULT_BULK_PROCESS_CODE, Collections.singletonList(IMAGE_URL));
    Mockito.verify(bulkProcessImageService)
        .findByBulkProcessCodeAndImageUrl(bulkProcess_1, Collections.singletonList(IMAGE_URL));
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).saveBulkProcessImage(bulkProcessImageArgumentCaptor.capture());
    assertTrue(bulkProcessImageArgumentCaptor.getValue().get(0).isCompleted());
    assertTrue(StringUtils.isBlank(bulkProcessImageArgumentCaptor.getValue().get(0).getErrorMessage()));
  }

  @Test
  public void publishBulkImageDownloadEventModelTest() {
    Mockito.when(topicProperties.getBulkCreateDownloadImageEvent()).thenReturn(BULK_CREATE_DOWNLOAD_IMAGE_EVENT);
    bulkProcessServiceBean.publishBulkImageDownloadEventModel(DEFAULT_BULK_PROCESS_CODE, BULK_PROCESS_TYPE,
        new ArrayList<>(), true);
    Mockito.verify(kafkaProducer).send(Mockito.eq(topicProperties.getBulkCreateDownloadImageEvent()),
        Mockito.any(BulkImageDownloadEventModel.class));
    Mockito.verify(topicProperties, times(2)).getBulkCreateDownloadImageEvent();
  }

  @Test
  public void publishBulkBasicInfoImageDownloadEventModel() {
    Mockito.when(topicProperties.getBulkCreateDownloadImageEvent()).thenReturn(BULK_CREATE_DOWNLOAD_IMAGE_EVENT);
    bulkProcessServiceBean.publishBulkBasicInfoImageDownloadEventModel(DEFAULT_BULK_PROCESS_CODE, BULK_PROCESS_TYPE,
        new ArrayList<>());
    Mockito.verify(kafkaProducer).send(Mockito.eq(topicProperties.getBulkBasicInfoDownloadImageEvent()),
        Mockito.any(BulkImageDownloadEventModel.class));
    Mockito.verify(topicProperties, times(2)).getBulkBasicInfoDownloadImageEvent();
  }

  @Test
  public void publishBulkBasicInfoVideoDownloadEventModel() {
    Mockito.when(topicProperties.getBulkCreateDownloadImageEvent())
      .thenReturn(BULK_CREATE_DOWNLOAD_IMAGE_EVENT);
    bulkProcessServiceBean.publishBulkBasicInfoVideoDownloadEventModel(DEFAULT_BULK_PROCESS_CODE,
      BULK_PROCESS_TYPE, "", "");
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(topicProperties.getBulkBasicInfoDownloadVideoEvent()),
        Mockito.eq(DEFAULT_BULK_PROCESS_CODE),
        Mockito.any(BulkVideoDownloadRequestEventModel.class));
    Mockito.verify(topicProperties, times(2)).getBulkBasicInfoDownloadVideoEvent();
  }

  @Test
  public void checkBulkProcessStatusEmptyTest() throws Exception {
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE))
        .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE))
        .thenReturn(thresholdPendingBulkRequests);
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, CREATION_BULK_PROCESS_TYPE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, CREATION_BULK_PROCESS_TYPE, BulkProcess.STATUS_IMAGE_PROCESSING,  PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, CREATION_BULK_PROCESS_TYPE,  BulkProcess.STATUS_PUBLISHED,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
  }

  @Test
  public void checkBulkProcessStatusDeletePickupPointTest() {
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE))
        .thenReturn(thresholdPendingBulkRequests);
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, BulkProcessType.DELETE_PICKUP_POINT.getValue());
    Mockito.verify(pickupPointDeleteService)
        .processPendingDeletePickupPointEvent(Constant.STORE_ID, BulkProcessType.DELETE_PICKUP_POINT.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, BulkProcessType.DELETE_PICKUP_POINT.getValue(),  BulkProcess.STATUS_PUBLISHED,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
  }

  @Test
  public void checkBulkProcessStatusPendingTest() throws Exception {
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE))
        .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE))
        .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, CREATION_BULK_PROCESS_TYPE, BulkProcess.STATUS_IMAGE_PROCESSING,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, CREATION_BULK_PROCESS_TYPE, BulkProcess.STATUS_PUBLISHED,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    Mockito.when(bulkProcessDataService.getPendingBulkProcessCodes(Constant.STORE_ID,
            Arrays.asList(getBulkProcess().get(0).getBulkProcessCode())))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0).getBulkProcessCode()));
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, CREATION_BULK_PROCESS_TYPE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, CREATION_BULK_PROCESS_TYPE,  BulkProcess.STATUS_IMAGE_PROCESSING,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.eq(Constant.STORE_ID), Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, CREATION_BULK_PROCESS_TYPE, BulkProcess.STATUS_PUBLISHED,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessDataService).getPendingBulkProcessCodes(Constant.STORE_ID,
        Arrays.asList(getBulkProcess().get(0).getBulkProcessCode()));
  }

  @Test
  public void checkBulkProcessStatusPriority1Test() throws Exception {
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE))
      .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE))
      .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE_FOR_PRIORITY_1))
      .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE))
      .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
          CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_1, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
      .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_1, BulkProcess.STATUS_PUBLISHED,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
      .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    Mockito.when(bulkProcessDataService.getPendingBulkProcessCodes(Constant.STORE_ID,
        Arrays.asList(getBulkProcess().get(0).getBulkProcessCode())))
      .thenReturn(Collections.singletonList(getBulkProcess().get(0).getBulkProcessCode()));
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_1);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE_FOR_PRIORITY_1);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
        CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_1,  BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
          PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.eq(Constant.STORE_ID), Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_1, BulkProcess.STATUS_PUBLISHED,
          PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessDataService).getPendingBulkProcessCodes(Constant.STORE_ID,
      Arrays.asList(getBulkProcess().get(0).getBulkProcessCode()));
  }

  @Test
  public void checkBulkProcessStatusPriority2Test() throws Exception {
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE))
      .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE))
      .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID,
          Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE_FOR_PRIORITY_2))
      .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE))
      .thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
          CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_2, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
      .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
          CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_2, BulkProcess.STATUS_PUBLISHED,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
      .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    Mockito.when(bulkProcessDataService.getPendingBulkProcessCodes(Constant.STORE_ID,
        Arrays.asList(getBulkProcess().get(0).getBulkProcessCode())))
      .thenReturn(Collections.singletonList(getBulkProcess().get(0).getBulkProcessCode()));
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID,
      CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_2);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE_FOR_PRIORITY_2);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
        CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_2,  BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2,
          PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.eq(Constant.STORE_ID), Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
        CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_2, BulkProcess.STATUS_PUBLISHED,
          PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessDataService).getPendingBulkProcessCodes(Constant.STORE_ID,
      Arrays.asList(getBulkProcess().get(0).getBulkProcessCode()));
  }

  @Test
  public void checkBulkProcessStatusProductBasicInfoTest() throws Exception {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.VIDEO_PROCESSING_FETCH_BATCH_SIZE_FOR_PRODUCT_BASIC_INFO)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.VIDEO_PROCESSING_FETCH_BATCH_SIZE_FOR_PRODUCT_BASIC_INFO)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            PRODUCT_BASIC_INFO, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            PRODUCT_BASIC_INFO, BulkProcess.STATUS_PUBLISHED,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    Mockito.when(bulkProcessDataService.getPendingBulkProcessCodes(Constant.STORE_ID,
            Arrays.asList(getBulkProcess().get(0).getBulkProcessCode())))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0).getBulkProcessCode()));
    Mockito.when(bulkProcessImageService.findByStoreIdAndBulkProcess(Constant.STORE_ID, getBulkProcess().get(0)))
        .thenReturn(Collections.singletonList(bulkProcessImage));
    Mockito.when(bulkProcessVideoService.findByStoreIdAndBulkProcess(Constant.STORE_ID, getBulkProcess().get(0)))
        .thenReturn(Collections.singletonList(bulkProcessVideo));
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, PRODUCT_BASIC_INFO);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.VIDEO_PROCESSING_FETCH_BATCH_SIZE_FOR_PRODUCT_BASIC_INFO);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, PRODUCT_BASIC_INFO,
            BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessImageService)
        .findByStoreIdAndBulkProcess(Mockito.eq(Constant.STORE_ID), Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessVideoService)
        .findByStoreIdAndBulkProcess(Mockito.eq(Constant.STORE_ID), Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, PRODUCT_BASIC_INFO,
            BulkProcess.STATUS_PUBLISHED, PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessDataService)
        .getPendingBulkProcessCodes(Constant.STORE_ID, Arrays.asList(getBulkProcess().get(0).getBulkProcessCode()));
  }

  @Test
  public void checkBulkProcessStatusProductBasicInfoPriority1Test() throws Exception {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.VIDEO_PROCESSING_FETCH_BATCH_SIZE_FOR_PRODUCT_BASIC_INFO)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.VIDEO_PROCESSING_FETCH_BATCH_SIZE_FOR_PRODUCT_BASIC_INFO)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            PRODUCT_BASIC_INFO_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            PRODUCT_BASIC_INFO_PRIORITY_1, BulkProcess.STATUS_PUBLISHED,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    Mockito.when(bulkProcessDataService.getPendingBulkProcessCodes(Constant.STORE_ID,
            Arrays.asList(getBulkProcess().get(0).getBulkProcessCode())))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0).getBulkProcessCode()));
    bulkProcessImage.setCompleted(true);
    bulkProcessVideo.setCompleted(true);
    Mockito.when(bulkProcessImageService.findByStoreIdAndBulkProcess(Constant.STORE_ID, getBulkProcess().get(0)))
        .thenReturn(Collections.singletonList(bulkProcessImage));
    Mockito.when(bulkProcessVideoService.findByStoreIdAndBulkProcess(Constant.STORE_ID, getBulkProcess().get(0)))
        .thenReturn(Collections.singletonList(bulkProcessVideo));
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, PRODUCT_BASIC_INFO_PRIORITY_1);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.VIDEO_PROCESSING_FETCH_BATCH_SIZE_FOR_PRODUCT_BASIC_INFO);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, PRODUCT_BASIC_INFO_PRIORITY_1,
            BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessImageService)
        .findByStoreIdAndBulkProcess(Mockito.eq(Constant.STORE_ID), Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessVideoService)
        .findByStoreIdAndBulkProcess(Mockito.eq(Constant.STORE_ID), Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, PRODUCT_BASIC_INFO_PRIORITY_1,
            BulkProcess.STATUS_PUBLISHED, PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessDataService)
        .getPendingBulkProcessCodes(Constant.STORE_ID, Arrays.asList(getBulkProcess().get(0).getBulkProcessCode()));
  }

  @Test
  public void checkBulkProcessStatusProductBasicInfoPriority2Test() throws Exception {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.VIDEO_PROCESSING_FETCH_BATCH_SIZE_FOR_PRODUCT_BASIC_INFO)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.VIDEO_PROCESSING_FETCH_BATCH_SIZE_FOR_PRODUCT_BASIC_INFO)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            PRODUCT_BASIC_INFO_PRIORITY_2, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            PRODUCT_BASIC_INFO_PRIORITY_2, BulkProcess.STATUS_PUBLISHED,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    Mockito.when(bulkProcessDataService.getPendingBulkProcessCodes(Constant.STORE_ID,
            Arrays.asList(getBulkProcess().get(0).getBulkProcessCode())))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0).getBulkProcessCode()));
    Mockito.when(bulkProcessImageService.findByStoreIdAndBulkProcess(Constant.STORE_ID, getBulkProcess().get(0)))
        .thenReturn(Collections.singletonList(bulkProcessImage));
    Mockito.when(bulkProcessVideoService.findByStoreIdAndBulkProcess(Constant.STORE_ID, getBulkProcess().get(0)))
        .thenReturn(Collections.singletonList(bulkProcessVideo));
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, PRODUCT_BASIC_INFO_PRIORITY_2);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.VIDEO_PROCESSING_FETCH_BATCH_SIZE_FOR_PRODUCT_BASIC_INFO);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, PRODUCT_BASIC_INFO_PRIORITY_2,
            BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessImageService)
        .findByStoreIdAndBulkProcess(Mockito.eq(Constant.STORE_ID), Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessVideoService)
        .findByStoreIdAndBulkProcess(Mockito.eq(Constant.STORE_ID), Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, PRODUCT_BASIC_INFO_PRIORITY_2,
            BulkProcess.STATUS_PUBLISHED, PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessDataService)
        .getPendingBulkProcessCodes(Constant.STORE_ID, Arrays.asList(getBulkProcess().get(0).getBulkProcessCode()));
  }

  @Test
  public void checkBulkProcessStatusPending1Test() throws Exception {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            CAMPAIGN_BULK_PROCESS_TYPE, BulkProcess.STATUS_PUBLISHED,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    bulkProcessImage.setCompleted(false);
    Mockito.when(bulkProcessImageService.findByStoreIdAndBulkProcess(Constant.STORE_ID, getBulkProcess().get(0)))
        .thenReturn(Collections.singletonList(bulkProcessImage));
    Mockito.when(bulkProcessDataService.getPendingBulkProcessCodes(Constant.STORE_ID,
            Arrays.asList(getBulkProcess().get(0).getBulkProcessCode())))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0).getBulkProcessCode()));
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, CAMPAIGN_BULK_PROCESS_TYPE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, CAMPAIGN_BULK_PROCESS_TYPE,
            BulkProcess.STATUS_PUBLISHED, PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessDataService)
        .getPendingBulkProcessCodes(Constant.STORE_ID, Arrays.asList(getBulkProcess().get(0).getBulkProcessCode()));
  }

  @Test
  public void checkBulkProcessStatusPending2Test() throws Exception {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            CAMPAIGN_BULK_PROCESS_TYPE, BulkProcess.STATUS_PUBLISHED,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    bulkProcessImage.setCompleted(false);
    Mockito.when(bulkProcessImageService.findByStoreIdAndBulkProcess(Constant.STORE_ID, getBulkProcess().get(0)))
        .thenReturn(Collections.singletonList(bulkProcessImage));
    Mockito.when(bulkProcessDataService.getPendingBulkProcessCodes(Constant.STORE_ID,
        Arrays.asList(getBulkProcess().get(0).getBulkProcessCode()))).thenReturn(null);
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, CAMPAIGN_BULK_PROCESS_TYPE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, CAMPAIGN_BULK_PROCESS_TYPE,
            BulkProcess.STATUS_PUBLISHED,  PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessDataService)
        .getPendingBulkProcessCodes(Constant.STORE_ID, Arrays.asList(getBulkProcess().get(0).getBulkProcessCode()));
    Mockito.verify(bulkProcessRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void checkBulkProcessStatusTest() throws Exception {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            CREATION_BULK_PROCESS_TYPE, BulkProcess.STATUS_IMAGE_PROCESSING,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            CREATION_BULK_PROCESS_TYPE, BulkProcess.STATUS_PUBLISHED,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(getBulkProcess().get(0)));
    bulkProcessImage.setCompleted(true);
    Mockito.when(bulkProcessImageService.findByStoreIdAndBulkProcess(Constant.STORE_ID, getBulkProcess().get(0)))
        .thenReturn(Collections.singletonList(bulkProcessImage));
    bulkProcessData1.setStatus(BulkProcessData.STATUS_SUCCESS);
    Mockito.when(bulkProcessDataService.getPendingBulkProcessCodes(Constant.STORE_ID,
        Arrays.asList(getBulkProcess().get(0).getBulkProcessCode()))).thenReturn(null);
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, CREATION_BULK_PROCESS_TYPE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, CREATION_BULK_PROCESS_TYPE,
            BulkProcess.STATUS_IMAGE_PROCESSING,
            PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.eq(Constant.STORE_ID), Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, CREATION_BULK_PROCESS_TYPE,
            BulkProcess.STATUS_PUBLISHED,  PageRequest.of(0, Integer.valueOf(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessDataService)
        .getPendingBulkProcessCodes(Constant.STORE_ID, Arrays.asList(getBulkProcess().get(0).getBulkProcessCode()));
  }

  @Test
  public void checkBulkProcessStatusExternalCreationTest() {
    bulkProcessImage.setCompleted(true);
    bulkProcessImageQC.setCompleted(true);
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE, "10", "desc");
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    systemParameterConfig.setVariable(Constant.PUBLISHED_FETCH_BATCH_SIZE);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(
        DEFAULT_STORE_ID, BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue(), BulkProcess.STATUS_IMAGE_PROCESSING ,
        DEFAULT_PAGEABLE)).thenReturn(getBulkProcess_externalCreation());
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(Constant.STORE_ID, getBulkProcess_externalCreation().getFirst()))
        .thenReturn(Collections.singletonList(bulkProcessImage));
    when(bulkProcessImageQCService.fetchAllBulkProcessImageQCByBulkProcess(Constant.STORE_ID, getBulkProcess_externalCreation().getFirst()))
        .thenReturn(Collections.singletonList(bulkProcessImageQC));
    when(bulkProcessRepository.saveAll(Mockito.anyList())).thenReturn(null);
    bulkProcessServiceBean.checkBulkProcessStatus(DEFAULT_STORE_ID, BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(DEFAULT_STORE_ID, BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue(), BulkProcess.STATUS_IMAGE_PROCESSING ,
        DEFAULT_PAGEABLE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessImageQCService).fetchAllBulkProcessImageQCByBulkProcess(Mockito.anyString(), Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void checkBulkProcessStatusExternalCreation_EmptyListTest() {
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE, "10", "desc");
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    systemParameterConfig.setVariable(Constant.PUBLISHED_FETCH_BATCH_SIZE);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(systemParameterConfig);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(
        DEFAULT_STORE_ID, BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue(), BulkProcess.STATUS_IMAGE_PROCESSING ,
        DEFAULT_PAGEABLE)).thenReturn(getBulkProcess_externalCreation());
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(Constant.STORE_ID, getBulkProcess_externalCreation().getFirst()))
        .thenReturn(Collections.singletonList(bulkProcessImage));
    when(bulkProcessImageQCService.fetchAllBulkProcessImageQCByBulkProcess(Constant.STORE_ID, getBulkProcess_externalCreation().getFirst()))
        .thenReturn(Collections.singletonList(bulkProcessImageQC));
    bulkProcessServiceBean.checkBulkProcessStatus(DEFAULT_STORE_ID, BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constant.IMAGE_PROCESSING_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(DEFAULT_STORE_ID, BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue(), BulkProcess.STATUS_IMAGE_PROCESSING ,
        DEFAULT_PAGEABLE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessImageQCService).fetchAllBulkProcessImageQCByBulkProcess(Mockito.anyString(), Mockito.any(BulkProcess.class));
  }

  @Test
  public void publishBulkProductCreationEvent() throws Exception {
    Mockito.when(topicProperties.getBulkCnCreateProductEvent()).thenReturn(BULK_CN_CREATE_PRODUCT_EVENT);
    bulkProcessServiceBean.publishBulkProductCreationEvent(Constant.STORE_ID, getBulkProcess_1(),
        CREATION_BULK_PROCESS_TYPE);
    verify(kafkaProducer).send(Mockito.eq(BULK_CN_CREATE_PRODUCT_EVENT),
        Mockito.any(BulkCreateProductEventModel.class));
    Mockito.verify(bulkProcessDataService).getDistinctParentProduct(anyString(), anyString());
    Mockito.verify(topicProperties).getBulkCnCreateProductEvent();
    Mockito.verify(topicProperties).getBulkCnCreateProductForPriority1Event();
    Mockito.verify(topicProperties).getBulkCnCreateProductForPriority2Event();
  }

  @Test
  public void publishBulkConvertedProductCreationEvent() throws Exception {
    Mockito.when(topicProperties.getBulkGenericCreateProductForConvertedUpload()).thenReturn(BULK_CN_CREATE_PRODUCT_EVENT);
    bulkProcessServiceBean.publishBulkProductCreationEvent(Constant.STORE_ID, getBulkProcess_1(),
      CONVERTED_PRODUCT_CREATION_UPLOAD.getValue());
    verify(kafkaProducer).send(Mockito.eq(BULK_CN_CREATE_PRODUCT_EVENT),
      Mockito.any(BulkCreateProductEventModel.class));
    Mockito.verify(bulkProcessDataService).getDistinctParentProduct(anyString(), anyString());
    Mockito.verify(topicProperties).getBulkGenericCreateProductForConvertedUpload();
  }

  @Test
  public void publishBulkExternalProductCreationEvent() throws Exception {
    Mockito.when(topicProperties.getBulkGenericCreateProductForExternalUpload())
      .thenReturn(BULK_CN_CREATE_PRODUCT_EVENT);
    bulkProcessServiceBean.publishBulkProductCreationEvent(Constant.STORE_ID, getBulkProcess_1(),
      EXTERNAL_CREATION_UPLOAD.getValue());
    verify(kafkaProducer).send(Mockito.eq(BULK_CN_CREATE_PRODUCT_EVENT),
      Mockito.any(BulkCreateProductEventModel.class));
    Mockito.verify(bulkProcessDataService).getDistinctParentProduct(anyString(), anyString());
    Mockito.verify(topicProperties).getBulkGenericCreateProductForExternalUpload();
  }

  @Test
  public void publishBulkProductCreationEvent_Generic() throws Exception {
    BulkProcess bulkProcess = getBulkProcess_1();
    Mockito.when(topicProperties.getBulkGenericCreateProductEvent()).thenReturn(BULK_GENERIC_CREATE_PRODUCT);
    bulkProcess.setNotes(Constant.GENERIC);
    bulkProcessServiceBean.publishBulkProductCreationEvent(Constant.STORE_ID, bulkProcess, CREATION_BULK_PROCESS_TYPE);
    verify(kafkaProducer).send(Mockito.eq(BULK_GENERIC_CREATE_PRODUCT), Mockito.any(BulkCreateProductEventModel.class));
    Mockito.verify(bulkProcessDataService).getDistinctParentProduct(anyString(), anyString());
    Mockito.verify(topicProperties).getBulkGenericCreateProductForPriority1Event();
    Mockito.verify(topicProperties).getBulkGenericCreateProductForPriority2Event();
    Mockito.verify(topicProperties).getBulkGenericCreateProductEvent();
  }


  @Test
  public void publishBulkProductCreationEvent_InvalidProcessType_ThrowsException()
    throws Exception {
    BulkProcess bulkProcess = getBulkProcess_1();
    bulkProcess.setNotes(null); // or whatever causes processType to be null
    Exception exception = assertThrows(IllegalArgumentException.class, () -> {
      bulkProcessServiceBean.publishBulkProductCreationEvent(Constant.STORE_ID, bulkProcess,
        PRODUCT_NAME);
    });
    Mockito.verify(bulkProcessDataService).getDistinctParentProduct(anyString(), anyString());
    assertTrue(exception.getMessage().contains("Invalid bulkProcessType"));
  }

  @Test
  public void publishBulkProductCreationEvent_GenericWithPriority1() throws Exception {
    BulkProcess bulkProcess = getBulkProcess_1();
    bulkProcess.setNotes(Constant.GENERIC);
    Mockito.when(topicProperties.getBulkGenericCreateProductForPriority1Event()).thenReturn(BULK_GENERIC_CREATE_PRODUCT_PRIORITY_1);
    bulkProcessServiceBean.publishBulkProductCreationEvent(Constant.STORE_ID, bulkProcess,
        CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_1);
    verify(kafkaProducer).send(Mockito.eq(BULK_GENERIC_CREATE_PRODUCT_PRIORITY_1),
        Mockito.any(BulkCreateProductEventModel.class));
    Mockito.verify(bulkProcessDataService).getDistinctParentProduct(anyString(), anyString());
    Mockito.verify(topicProperties).getBulkGenericCreateProductForPriority1Event();
    Mockito.verify(topicProperties).getBulkGenericCreateProductForPriority2Event();
    Mockito.verify(topicProperties).getBulkGenericCreateProductEvent();
  }

  @Test
  public void publishBulkProductCreationEvent_GenericWithPriority2() throws Exception {
    BulkProcess bulkProcess = getBulkProcess_1();
    bulkProcess.setNotes(Constant.GENERIC);
    Mockito.when(topicProperties.getBulkGenericCreateProductForPriority2Event())
        .thenReturn(BULK_GENERIC_CREATE_PRODUCT_PRIORITY_2);
    bulkProcessServiceBean.publishBulkProductCreationEvent(Constant.STORE_ID, bulkProcess,
        CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_2);
    verify(kafkaProducer).send(Mockito.eq(BULK_GENERIC_CREATE_PRODUCT_PRIORITY_2),
        Mockito.any(BulkCreateProductEventModel.class));
    Mockito.verify(bulkProcessDataService).getDistinctParentProduct(anyString(), anyString());
    Mockito.verify(topicProperties).getBulkGenericCreateProductForPriority1Event();
    Mockito.verify(topicProperties).getBulkGenericCreateProductForPriority2Event();
    Mockito.verify(topicProperties).getBulkGenericCreateProductEvent();
  }

  @Test
  public void publishBulkProductCreationEvent_CN_WithPriority2() throws Exception {
    BulkProcess bulkProcess = getBulkProcess_1();
    bulkProcess.setNotes(Constant.CATEGORY);
    Mockito.when(topicProperties.getBulkCnCreateProductForPriority2Event())
        .thenReturn(BULK_CN_CREATE_PRODUCT_EVENT_PRIORITY_2);
    bulkProcessServiceBean.publishBulkProductCreationEvent(Constant.STORE_ID, bulkProcess,
        CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_2);
    verify(kafkaProducer).send(Mockito.eq(BULK_CN_CREATE_PRODUCT_EVENT_PRIORITY_2),
        Mockito.any(BulkCreateProductEventModel.class));
    Mockito.verify(bulkProcessDataService).getDistinctParentProduct(anyString(), anyString());
    Mockito.verify(topicProperties).getBulkCnCreateProductEvent();
    Mockito.verify(topicProperties).getBulkCnCreateProductForPriority1Event();
    Mockito.verify(topicProperties).getBulkCnCreateProductForPriority2Event();
  }

  @Test
  public void publishBulkProductCreationEvent_CN_WithPriority1() throws Exception {
    BulkProcess bulkProcess = getBulkProcess_1();
    bulkProcess.setNotes(Constant.CATEGORY);
    Mockito.when(topicProperties.getBulkCnCreateProductForPriority1Event())
        .thenReturn(BULK_CN_CREATE_PRODUCT_EVENT_PRIORITY_1);
    bulkProcessServiceBean.publishBulkProductCreationEvent(Constant.STORE_ID, bulkProcess,
        CREATION_BULK_PROCESS_TYPE_FOR_PRIORITY_1);
    verify(kafkaProducer).send(Mockito.eq(BULK_CN_CREATE_PRODUCT_EVENT_PRIORITY_1),
        Mockito.any(BulkCreateProductEventModel.class));
    Mockito.verify(bulkProcessDataService).getDistinctParentProduct(anyString(), anyString());
    Mockito.verify(topicProperties).getBulkCnCreateProductForPriority1Event();
    Mockito.verify(topicProperties).getBulkCnCreateProductForPriority2Event();
    Mockito.verify(topicProperties).getBulkCnCreateProductEvent();
  }

  @Test
  public void publishBulkDeleteItemPickupPointEventTest() throws Exception {
    BulkProcess bulkProcess = getBulkProcess_1();
    Mockito.when(bulkProcessDataService.getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(rowNumberParentCodeDTO3));
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList1.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList1.get(1).getBulkRequestData(), typeRef));
    bulkProcessServiceBean.publishBulkDeleteItemPickupPointEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessDataService)
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, bulkProcess.getBulkProcessCode(),
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(kafkaProducer).send(topicProperties.getBulkDeleteItemPickupPoint(), "HIC-60001-00003",
        BulkUpdateEventModel.builder().storeId(DEFAULT_STORE_ID)
            .businessPartnerCode(bulkProcess.getBusinessPartnerCode()).bulkProcessCode(bulkProcess.getBulkProcessCode())
            .rowNumbers(Collections.singletonList(1)).build());
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList1.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(topicProperties, times(2)).getBulkDeleteItemPickupPoint();
  }

  @Test
  public void publishBulkDeleteItemPickupPointEventExceptionTest() throws Exception {
    BulkProcess bulkProcess = getBulkProcess_1();
    Mockito.when(bulkProcessDataService.getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(rowNumberParentCodeDTO));
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
        .thenThrow(JsonProcessingException.class);
    bulkProcessServiceBean.publishBulkDeleteItemPickupPointEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessDataService)
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, bulkProcess.getBulkProcessCode(),
            BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(topicProperties).getBulkDeleteItemPickupPoint();
  }


  @Test
  public void publishBulkUpdateEventTest() throws Exception {
    bulkProcessDataBatchSize.setVariable(SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE);
    BulkProcess bulkProcess = getBulkProcess_1();
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE, "2", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessDataService.getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(rowNumberParentCodeDTO));
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(1).getBulkRequestData(), typeRef));
    bulkProcessServiceBean.publishBulkUpdateEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE);
    Mockito.verify(bulkProcessDataService)
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, bulkProcess.getBulkProcessCode(),
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(kafkaProducer).send(topicProperties.getBulkUploadUpdateItem(), PRODUCT_SKU_1,
        BulkUpdateEventModel.builder().storeId(DEFAULT_STORE_ID)
            .businessPartnerCode(bulkProcess.getBusinessPartnerCode()).bulkProcessCode(bulkProcess.getBulkProcessCode())
            .rowNumbers(Collections.singletonList(1)).build());
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(topicProperties, times(2)).getBulkUploadUpdateItem();
    verify(topicProperties).getBulkUploadUpdateItemPriority1();
    verify(topicProperties).getBulkUploadUpdateItemPriority2();
  }

  @Test
  public void publishBulkUpdateEventPriority1Test() throws Exception {
    bulkProcessDataBatchSize.setVariable(SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE);
    BulkProcess bulkProcess = getBulkProcess_1();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue());
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE, "2", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessDataService
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(Arrays.asList(rowNumberParentCodeDTO));
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(1).getBulkRequestData(), typeRef));
    bulkProcessServiceBean.publishBulkUpdateEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE);
    Mockito.verify(bulkProcessDataService)
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, bulkProcess.getBulkProcessCode(),
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(kafkaProducer).send(topicProperties.getBulkUploadUpdateItemPriority1(), PRODUCT_SKU_1,
        BulkUpdateEventModel.builder().storeId(DEFAULT_STORE_ID)
            .businessPartnerCode(bulkProcess.getBusinessPartnerCode()).bulkProcessCode(bulkProcess.getBulkProcessCode())
            .rowNumbers(Collections.singletonList(1)).build());
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(topicProperties).getBulkUploadUpdateItem();
    verify(topicProperties, times(2)).getBulkUploadUpdateItemPriority1();
    verify(topicProperties).getBulkUploadUpdateItemPriority2();
  }

  @Test
  public void publishBulkUpdateEventInBatchesTest() throws Exception {
    BulkProcess bulkProcess = getBulkProcess_1();
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE, "2", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessDataService.getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(rowNumberParentCodeDTOList);
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(1).getBulkRequestData(), typeRef));
    bulkProcessDataBatchSize.setVariable(SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE);
    bulkProcessServiceBean.publishBulkUpdateEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE);
    Mockito.verify(bulkProcessDataService)
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, bulkProcess.getBulkProcessCode(),
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(kafkaProducer).send(topicProperties.getBulkUploadUpdateItem(), PRODUCT_SKU_1,
        BulkUpdateEventModel.builder().storeId(DEFAULT_STORE_ID)
            .businessPartnerCode(bulkProcess.getBusinessPartnerCode()).bulkProcessCode(bulkProcess.getBulkProcessCode())
            .rowNumbers(Arrays.asList(1, 2)).build());
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList.get(1).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(topicProperties, times(2)).getBulkUploadUpdateItem();
    verify(topicProperties).getBulkUploadUpdateItemPriority1();
    verify(topicProperties).getBulkUploadUpdateItemPriority2();
  }

  @Test
  public void publishBulkUpdateEventInBatchSize1Test() throws Exception {
    bulkProcessDataBatchSize.setVariable(SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE);
    BulkProcess bulkProcess = getBulkProcess_1();
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE, "1", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessDataService.getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(rowNumberParentCodeDTOList);
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(1).getBulkRequestData(), typeRef));
    bulkProcessServiceBean.publishBulkUpdateEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE);
    Mockito.verify(bulkProcessDataService)
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, bulkProcess.getBulkProcessCode(),
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(kafkaProducer).send(topicProperties.getBulkUploadUpdateItem(), PRODUCT_SKU_1,
        BulkUpdateEventModel.builder().storeId(DEFAULT_STORE_ID)
            .businessPartnerCode(bulkProcess.getBusinessPartnerCode()).bulkProcessCode(bulkProcess.getBulkProcessCode())
            .rowNumbers(Collections.singletonList(1)).build());
    Mockito.verify(kafkaProducer).send(topicProperties.getBulkUploadUpdateItem(), PRODUCT_SKU_1,
        BulkUpdateEventModel.builder().storeId(DEFAULT_STORE_ID)
            .businessPartnerCode(bulkProcess.getBusinessPartnerCode()).bulkProcessCode(bulkProcess.getBulkProcessCode())
            .rowNumbers(Collections.singletonList(2)).build());
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList.get(1).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(topicProperties, times(3)).getBulkUploadUpdateItem();
    verify(topicProperties).getBulkUploadUpdateItemPriority1();
    verify(topicProperties).getBulkUploadUpdateItemPriority2();
  }

  @Test
  public void publishBulkUpdateEventInBatchSize2Test() throws Exception {
    bulkProcessDataBatchSize.setVariable(SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE);
    BulkProcess bulkProcess = getBulkProcess_1();
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE, "1", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(systemParameterConfig);
    rowNumberParentCodeDTOList.add(rowNumberParentCodeDTONew);
    Mockito.when(bulkProcessDataService.getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(rowNumberParentCodeDTOList);
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(1).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(2).getBulkRequestData(), typeRef));
    bulkProcessServiceBean.publishBulkUpdateEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE);
    Mockito.verify(bulkProcessDataService)
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, bulkProcess.getBulkProcessCode(),
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(kafkaProducer).send(topicProperties.getBulkUploadUpdateItem(), PRODUCT_SKU_1,
        BulkUpdateEventModel.builder().storeId(DEFAULT_STORE_ID)
            .businessPartnerCode(bulkProcess.getBusinessPartnerCode()).bulkProcessCode(bulkProcess.getBulkProcessCode())
            .rowNumbers(Collections.singletonList(1)).build());
    Mockito.verify(kafkaProducer).send(topicProperties.getBulkUploadUpdateItem(), PRODUCT_SKU_1,
        BulkUpdateEventModel.builder().storeId(DEFAULT_STORE_ID)
            .businessPartnerCode(bulkProcess.getBusinessPartnerCode()).bulkProcessCode(bulkProcess.getBulkProcessCode())
            .rowNumbers(Collections.singletonList(2)).build());
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList.get(1).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList.get(2).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(topicProperties, times(3)).getBulkUploadUpdateItem();
    verify(topicProperties).getBulkUploadUpdateItemPriority1();
    verify(topicProperties).getBulkUploadUpdateItemPriority2();
  }

  @Test
  public void publishCampaignUploadEventTest() throws Exception {
    bulkProcessDataBatchSize.setVariable(SystemParameterConfigNames.PROCESS_DATA_CAMPAIGN_BATCH_SIZE);
    BulkProcess bulkProcess = getBulkProcess_1();
    bulkProcess.setBulkProcessType(BulkProcessType.CAMPAIGN.getValue());
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.PROCESS_DATA_CAMPAIGN_BATCH_SIZE, "2", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessDataService
        .findRowNumberByStoreIdAndBulkProcessCodeAndStatus(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(Arrays.asList(1, 2));
    bulkProcessServiceBean.publishCampaignUploadEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.PROCESS_DATA_CAMPAIGN_BATCH_SIZE);
    Mockito.verify(bulkProcessDataService)
        .findRowNumberByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, bulkProcess.getBulkProcessCode(),
            BulkProcessData.STATUS_PENDING);
    verify(kafkaProducer, times(1))
        .send(eq(topicProperties.getBulkUploadCampaignItem()), Mockito.any());
    verify(topicProperties, times(2)).getBulkUploadCampaignItem();
  }

  @Test
  public void deleteFromDbTest() {
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.DELETE_DAY_RANGE, "12", "desc");
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.DELETE_DAY_RANGE))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessRepository.findByStatusAndUpdatedDateInBetween(eq(DEFAULT_STORE_ID), any(), any(),
        eq(Arrays.asList(BulkProcess.STATUS_FINISHED, BulkProcess.STATUS_PARTIALLY_DONE, BulkProcess.STATUS_ABORTED))))
        .thenReturn(Arrays.asList(DEFAULT_BULK_PROCESS_CODE));
    Mockito.doNothing().when(bulkProcessDataService)
        .deleteDataByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.doNothing().when(bulkProcessImageService)
        .deleteImageByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    bulkProcessServiceBean.deleteFromDb(DEFAULT_STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.DELETE_DAY_RANGE);
    Mockito.verify(bulkProcessRepository).findByStatusAndUpdatedDateInBetween(eq(DEFAULT_STORE_ID), any(), any(),
        eq(Arrays.asList(BulkProcess.STATUS_FINISHED, BulkProcess.STATUS_PARTIALLY_DONE, BulkProcess.STATUS_ABORTED)));
    Mockito.verify(kafkaProducer).send(topicProperties.getBulkDataDeletionEvent(), DEFAULT_BULK_PROCESS_CODE,
        new BulkDataDeletionModel(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE));
    Mockito.verify(topicProperties, times(2)).getBulkDataDeletionEvent();
  }

  @Test
  public void deleteFromDb_ZeroTest() {
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.DELETE_DAY_RANGE, "0", "desc");
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.DELETE_DAY_RANGE))
        .thenReturn(systemParameterConfig);
    bulkProcessServiceBean.deleteFromDb(Constant.STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.DELETE_DAY_RANGE);
  }

  @Test
  public void deleteFromDbTest_Error() {
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.DELETE_DAY_RANGE, "12", "desc");
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.DELETE_DAY_RANGE))
        .thenReturn(systemParameterConfig);
    Mockito.doThrow(ApplicationRuntimeException.class).when(bulkProcessRepository)
        .findByStatusAndUpdatedDateInBetween(eq(DEFAULT_STORE_ID), any(), any(), eq(Arrays
            .asList(BulkProcess.STATUS_FINISHED, BulkProcess.STATUS_PARTIALLY_DONE, BulkProcess.STATUS_ABORTED)));
    bulkProcessServiceBean.deleteFromDb(Constant.STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.DELETE_DAY_RANGE);
    Mockito.verify(bulkProcessRepository).findByStatusAndUpdatedDateInBetween(eq(DEFAULT_STORE_ID), any(), any(),
        eq(Arrays.asList(BulkProcess.STATUS_FINISHED, BulkProcess.STATUS_PARTIALLY_DONE, BulkProcess.STATUS_ABORTED)));
  }

  @Test
  public void checkStuckProcessStatusTest() {
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.STUCK_PROCESS_CHECK_TIME, "12", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.STUCK_PROCESS_CHECK_TIME)).thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessRepository.findByStatusAndUpdatedDate(eq(DEFAULT_STORE_ID), any(), Mockito.anyList()))
        .thenReturn(Arrays.asList(DEFAULT_BULK_PROCESS_CODE));
    Mockito.doNothing().when(bulkProcessDataService)
        .updatePendingProcesses(eq(DEFAULT_STORE_ID), eq(Arrays.asList(DEFAULT_BULK_PROCESS_CODE)), any());
    Mockito.doNothing().when(bulkProcessImageService)
        .updatePendingImageDownloads(eq(DEFAULT_STORE_ID), eq(Arrays.asList(DEFAULT_BULK_PROCESS_CODE)), any());
    Mockito.doNothing().when(bulkProcessRepository)
        .updateStatusToAbortedByBulkProcessCodes(any(), eq(Arrays.asList(DEFAULT_BULK_PROCESS_CODE)));
    bulkProcessServiceBean.checkStuckProcessStatus("");
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.STUCK_PROCESS_CHECK_TIME);
    Mockito.verify(bulkProcessRepository, times(3))
        .findByStatusAndUpdatedDate(eq(DEFAULT_STORE_ID), any(), Mockito.anyList());
    Mockito.verify(bulkProcessDataService)
        .updatePendingProcesses(eq(DEFAULT_STORE_ID), eq(Arrays.asList(DEFAULT_BULK_PROCESS_CODE)), any());
    Mockito.verify(bulkProcessImageService)
        .updatePendingImageDownloads(eq(DEFAULT_STORE_ID), eq(Arrays.asList(DEFAULT_BULK_PROCESS_CODE)), any());
    Mockito.verify(bulkProcessRepository)
        .updateStatusToAbortedByBulkProcessCodes(any(), eq(Arrays.asList(DEFAULT_BULK_PROCESS_CODE)));
  }

  @Test
  public void checkStuckProcessStatusEmptyListTest() {
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.STUCK_PROCESS_CHECK_TIME, "12", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.STUCK_PROCESS_CHECK_TIME)).thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessRepository.findByStatusAndUpdatedDate(eq(DEFAULT_STORE_ID), any(), Mockito.anyList()))
        .thenReturn(new ArrayList());
    bulkProcessServiceBean.checkStuckProcessStatus("");
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.STUCK_PROCESS_CHECK_TIME);
    Mockito.verify(bulkProcessRepository, times(3))
        .findByStatusAndUpdatedDate(eq(DEFAULT_STORE_ID), any(), Mockito.anyList());
  }

  @Test
  public void checkStuckProcessStatusExceptionTest() {
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.STUCK_PROCESS_CHECK_TIME, "12", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.STUCK_PROCESS_CHECK_TIME)).thenReturn(systemParameterConfig);
    Mockito.doThrow(ApplicationRuntimeException.class).when(bulkProcessRepository)
        .findByStatusAndUpdatedDate(eq(DEFAULT_STORE_ID), any(), Mockito.anyList());
    bulkProcessServiceBean.checkStuckProcessStatus("");
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.STUCK_PROCESS_CHECK_TIME);
    Mockito.verify(bulkProcessRepository).findByStatusAndUpdatedDate(eq(DEFAULT_STORE_ID), any(), Mockito.anyList());
  }

  @Test
  public void checkStuckProcessStatusWithBulkProcessCodeTest() {
    bulkProcessServiceBean.checkStuckProcessStatus(DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessRepository)
        .updateStatusToAbortedByBulkProcessCodes(any(), eq(Arrays.asList(DEFAULT_BULK_PROCESS_CODE)));
  }

  @Test
  public void publishBulkInstantPickupItemUpsertEventTest() throws Exception {
    bulkProcessDataBatchSize.setVariable(SystemParameterConfigNames.PROCESS_DATA_UPSERT_BATCH_SIZE);
    BulkProcess bulkProcess = getBulkProcess_1();
    bulkProcess.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.PROCESS_DATA_UPSERT_BATCH_SIZE, "2", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessDataService
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(rowNumberParentCodeDTOList);
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(1).getBulkRequestData(), typeRef));
    bulkProcessServiceBean.publishBulkInstantPickupItemUpsertEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.PROCESS_DATA_UPSERT_BATCH_SIZE);
    verify(kafkaProducer).send(eq(topicProperties.getBulkUpsertInstantPickupItemEvent()), eq(Constant.SKU),
        Mockito.any(BulkUpdateEventModel.class));
    Mockito.verify(bulkProcessDataService)
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, bulkProcess.getBulkProcessCode(),
            BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList.get(1).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(topicProperties, times(2)).getBulkUpsertInstantPickupItemEvent();
  }

  @Test
  public void publishInstoreUpdateEventTest() throws Exception {
    bulkProcessDataBatchSize.setVariable(SystemParameterConfigNames.PROCESS_DATA_INSTORE_BATCH_SIZE);
    BulkProcess bulkProcess = getBulkProcess_1();
    bulkProcess.setBulkProcessType(BulkProcessType.IN_STORE.getValue());
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.PROCESS_DATA_INSTORE_BATCH_SIZE, "2", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessDataService.findRowNumberByStoreIdAndBulkProcessCodeAndStatus(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(1, 2));
    bulkProcessServiceBean.publishInstoreUpdateEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.PROCESS_DATA_INSTORE_BATCH_SIZE);
    Mockito.verify(bulkProcessDataService)
        .findRowNumberByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, bulkProcess.getBulkProcessCode(),
            BulkProcessData.STATUS_PENDING);
    verify(kafkaProducer, times(1)).send(eq(topicProperties.getBulkUploadInstoreUpdate()),
        Mockito.any());
    verify(topicProperties, times(2)).getBulkUploadInstoreUpdate();
  }

  @Test
  public void publishBulkInstantPickupItemDeleteEventTest() throws Exception {
    bulkProcessDataBatchSize.setVariable(SystemParameterConfigNames.PROCESS_DATA_DELETE_BATCH_SIZE);
    BulkProcess bulkProcess = getBulkProcess_1();
    bulkProcess.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue());
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.PROCESS_DATA_DELETE_BATCH_SIZE, "2", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessDataService
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(rowNumberParentCodeDTOList);
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(0).getBulkRequestData(), typeRef))
        .thenReturn(mapper.readValue(rowNumberParentCodeDTOList.get(1).getBulkRequestData(), typeRef));
    bulkProcessServiceBean.publishBulkInstantPickupItemDeleteEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.PROCESS_DATA_DELETE_BATCH_SIZE);
    Mockito.verify(bulkProcessDataService)
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, bulkProcess.getBulkProcessCode(),
            BulkProcessData.STATUS_PENDING);
    verify(kafkaProducer).send(eq(topicProperties.getBulkDeleteInstantPickupItemEvent()), eq(Constant.SKU),
        Mockito.any(BulkUpdateEventModel.class));
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList.get(0).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.eq(rowNumberParentCodeDTOList.get(1).getBulkRequestData()),
        Mockito.any(TypeReference.class));
    Mockito.verify(topicProperties, times(2)).getBulkDeleteInstantPickupItemEvent();
  }

  @Test
  public void publishArchiveProductEventTest() throws Exception {
    bulkProcessDataBatchSize.setVariable(SystemParameterConfigNames.PROCESS_DATA_ARCHIVE_BATCH_SIZE);
    BulkProcess bulkProcess = getBulkProcess_1();
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(bulkProcessDataBatchSize);
    Mockito.when(bulkProcessDataService
      .findRowNumberByStoreIdAndBulkProcessCodeAndStatus(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(Arrays.asList(1));
    bulkProcessServiceBean.publishArchiveProductEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.PROCESS_DATA_ARCHIVE_BATCH_SIZE);
    Mockito.verify(bulkProcessDataService)
      .findRowNumberByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, bulkProcess.getBulkProcessCode(),
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(kafkaProducer, times(1))
      .send(eq(topicProperties.getBulkArchiveProductRows()), Mockito.any());
    Mockito.verify(topicProperties, times(2)).getBulkArchiveProductRows();
  }

  @Test
  public void publishVatUpdateEventTest() throws Exception {
    bulkProcessDataBatchSize.setVariable(SystemParameterConfigNames.PROCESS_DATA_VAT_UPDATE_BATCH_SIZE);
    BulkProcess bulkProcess = getBulkProcess_1();
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(bulkProcessDataBatchSize);
    Mockito.when(bulkProcessDataService
        .findRowNumberByStoreIdAndBulkProcessCodeAndStatus(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(Arrays.asList(1));
    bulkProcessServiceBean.publishVatUpdateEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.PROCESS_DATA_VAT_UPDATE_BATCH_SIZE);
    Mockito.verify(bulkProcessDataService)
        .findRowNumberByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, bulkProcess.getBulkProcessCode(),
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(kafkaProducer, times(1))
        .send(eq(topicProperties.getBulkVatUpdateEvent()), Mockito.any());
    Mockito.verify(topicProperties, times(2)).getBulkVatUpdateEvent();
  }

  @Test
  public void findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndNoteTest() throws Exception {
    BulkProcess bulkProcess = getBulkProcess_1();
    Mockito.when(getBulkProcessRepository()
      .findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndNotes(DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, deletePickupPointEventType, DEFAULT_PICKUP_POINT_CODE))
      .thenReturn(Collections.singletonList(bulkProcess));
    bulkProcessServiceBean
      .findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndPPCode(DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, deletePickupPointEventType, DEFAULT_PICKUP_POINT_CODE);
    Mockito.verify(getBulkProcessRepository())
      .findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndNotes(DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, deletePickupPointEventType, DEFAULT_PICKUP_POINT_CODE);
  }

  @Test
  public void findByBulkProcessTypeAndStatusTest() {
    bulkProcessServiceBean
        .findByBulkProcessTypeAndStatus(DEFAULT_STORE_ID, BULK_PROCESS_TYPE, STATUS, DEFAULT_PAGEABLE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(DEFAULT_STORE_ID, BULK_PROCESS_TYPE, STATUS,
            DEFAULT_PAGEABLE);
  }

  @Test
  public void testFetchProcessListingResponse_ProductCreationUpload() throws Exception {
    String bulkProcessType = BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue();
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    ReflectionTestUtils.setField(bulkProcessServiceBean, "estimationFetchEnabledForListing", false);
    List<BulkProcess> bulkProcess = getBulkProcess();
    Page<BulkProcess> page = new PageImpl<BulkProcess>(bulkProcess,PageRequest.of(0,
      bulkProcess.size()),bulkProcess.size());
    Set<String> expectedBulkProcessTypes = new HashSet<>(Arrays.asList(
      BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
      BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY.getValue(),
      BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue(),
      BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue(),
      BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue(),
      BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue(),
      CONVERTED_PRODUCT_CREATION_UPLOAD.getValue()
    ));
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn(Collections.singletonList(bulkProcessData1));
    when(bulkProcessRepository.findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(expectedBulkProcessTypes), any(Date.class),eq(null),
      any(Pageable.class)))
      .thenReturn(page);
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean.fetchProcessListingResponse(
      DEFAULT_STORE_ID, bulkProcessType, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
      Optional.empty(), Optional.empty(), false, PageRequest.of(page.getNumber(),
        page.getSize()));
    verify(bulkProcessRepository).findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(expectedBulkProcessTypes), any(Date.class),eq(null),
      any(Pageable.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    assertTrue(page.hasContent());
  }

  @Test
  public void testFetchProcessListingResponse_ExternalProductCreationUpload() throws Exception {
    String bulkProcessType = BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue();
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    ReflectionTestUtils.setField(bulkProcessServiceBean, "estimationFetchEnabledForListing", false);
    List<BulkProcess> bulkProcess = getBulkProcess();
    Page<BulkProcess> page =
        new PageImpl<BulkProcess>(bulkProcess, PageRequest.of(0, bulkProcess.size()),
            bulkProcess.size());
    Set<String> expectedBulkProcessTypes = new HashSet<>(
        Arrays.asList(BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue(),
            BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
        any(BulkProcess.class))).thenReturn(Collections.singletonList(bulkProcessData1));
    when(
        bulkProcessRepository.findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
            eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(expectedBulkProcessTypes),
            any(Date.class), eq(null), any(Pageable.class))).thenReturn(page);
    Page<BulkProcessStatusListingResponse> result =
        bulkProcessServiceBean.fetchProcessListingResponse(DEFAULT_STORE_ID, bulkProcessType,
            DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE, Optional.empty(), Optional.empty(),
            false, PageRequest.of(page.getNumber(), page.getSize()));
    verify(
        bulkProcessRepository).findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
        eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(expectedBulkProcessTypes),
        any(Date.class), eq(null), any(Pageable.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    assertTrue(page.hasContent());
  }

  @Test
  public void testFetchProcessListingResponseExternalProductCreationUpload() throws Exception {
    String bulkProcessType = BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue();
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    ReflectionTestUtils.setField(bulkProcessServiceBean, "estimationFetchEnabledForListing", false);
    Set<String> processTypes = new HashSet<>();
    processTypes.add(EXTERNAL_CREATION_UPLOAD.getValue());
    ReflectionTestUtils.setField(bulkProcessServiceBean, "skipOverrideParentStatusBulkProcessTypes", processTypes);
    List<BulkProcess> bulkProcess = getBulkProcess();
    bulkProcess.get(1).setBulkProcessType(EXTERNAL_CREATION_UPLOAD.getValue());
    Page<BulkProcess> page =
        new PageImpl<BulkProcess>(bulkProcess, PageRequest.of(0, bulkProcess.size()),
            bulkProcess.size());
    Set<String> expectedBulkProcessTypes = new HashSet<>(
        Arrays.asList(BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue(),
            BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
        any(BulkProcess.class))).thenReturn(Collections.singletonList(bulkProcessData1));
    when(
        bulkProcessRepository.findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
            eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(expectedBulkProcessTypes),
            any(Date.class), eq(null), any(Pageable.class))).thenReturn(page);
    Page<BulkProcessStatusListingResponse> result =
        bulkProcessServiceBean.fetchProcessListingResponse(DEFAULT_STORE_ID, bulkProcessType,
            DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE, Optional.empty(), Optional.empty(),
            false, PageRequest.of(page.getNumber(), page.getSize()));
    verify(
        bulkProcessRepository).findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
        eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(expectedBulkProcessTypes),
        any(Date.class), eq(null), any(Pageable.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    assertTrue(page.hasContent());
  }

  @Test
  public void testFetchProcessListingResponse_ProductCreationUpload_withEstimationNeeded() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean,"estimationFetchEnabledForListing",true);
    String bulkProcessType = BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue();
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    List<BulkProcess> bulkProcess = getBulkProcess();
    Page<BulkProcess> page = new PageImpl<BulkProcess>(bulkProcess,PageRequest.of(0,
      bulkProcess.size()),bulkProcess.size());
    Set<String> expectedBulkProcessTypes = new HashSet<>(Arrays.asList(
      BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
      BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY.getValue(),
      BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue(),
      BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue(),
      BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue(),
      BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue(),
      CONVERTED_PRODUCT_CREATION_UPLOAD.getValue()

    ));
    String deltaTimeEstimations = "{\"4\":4743.0,\"5\":37.72,\"8\":3732.5384615384614,\"9\":32.0,"
      + "\"11\":24.0,\"12\":1549.6666666666667,\"13\":2273.6666666666665,\"14\":23.25,\"15\":46.0,\"16\":30.0,\"18\":27.583333333333332,\"19\":30.0,\"20\":24.0,\"21\":25.0,\"22\":23.0,\"23\":19.5}";
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductLevel3")
        .processLevelFetch(false).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    Map<String, Double> bulkProcessCodeXEstimationsMap = new HashMap<>();
    bulkProcessCodeXEstimationsMap.put(DEFAULT_BULK_PROCESS_CODE, 3732.5384615384614);

    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
      bulkProcessCodeXEstimationsMap);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn(Collections.singletonList(bulkProcessData1));
    when(bulkProcessRepository.findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(expectedBulkProcessTypes), any(Date.class),eq(null),
      any(Pageable.class)))
      .thenReturn(page);
    when(bulkProcessDataEstimationService.fetchAllEstimationResponsesByProcessTypes(anyList())).thenReturn(Collections.singletonList(bulkProcessDataEstimation));
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean.fetchProcessListingResponse(
      DEFAULT_STORE_ID, bulkProcessType, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
      Optional.empty(), Optional.empty(), true, PageRequest.of(page.getNumber(),
        page.getSize()));
    verify(bulkProcessRepository).findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(expectedBulkProcessTypes), any(Date.class),eq(null),
      any(Pageable.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    assertTrue(page.hasContent());
    verify(bulkProcessDataEstimationService).fetchAllEstimationResponsesByProcessTypes(any());
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));

  }

  @Test
  public void testFetchProcessListingResponse_ProductCreationUpload_NullCheckForDataTableEmptyTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean,"estimationFetchEnabledForListing",true);
    String bulkProcessType = BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue();
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    List<BulkProcess> bulkProcess = getBulkProcess();
    Page<BulkProcess> page = new PageImpl<BulkProcess>(bulkProcess,PageRequest.of(0,
        bulkProcess.size()),bulkProcess.size());
    Set<String> expectedBulkProcessTypes = new HashSet<>(Arrays.asList(
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
        BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY.getValue(),
        BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue(),
        BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue(),
        BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue(),
        CONVERTED_PRODUCT_CREATION_UPLOAD.getValue()));
    String deltaTimeEstimations = "{\"4\":4743.0,\"5\":37.72,\"8\":3732.5384615384614,\"9\":32.0,"
        + "\"11\":24.0,\"12\":1549.6666666666667,\"13\":2273.6666666666665,\"14\":23.25,\"15\":46.0,\"16\":30.0,\"18\":27.583333333333332,\"19\":30.0,\"20\":24.0,\"21\":25.0,\"22\":23.0,\"23\":19.5}";
    BulkProcessDataEstimation bulkProcessDataEstimation =
        BulkProcessDataEstimation.builder().processType("ProductLevel3")
            .processLevelFetch(false).deltaTimeEstimations(deltaTimeEstimations)
            .lastFetchTime(new Date()).build();
    Map<String, Double> bulkProcessCodeXEstimationsMap = new HashMap<>();
    bulkProcessCodeXEstimationsMap.put(DEFAULT_BULK_PROCESS_CODE, 3732.5384615384614);

    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
        bulkProcessCodeXEstimationsMap);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
        any(BulkProcess.class))).thenReturn(Collections.EMPTY_LIST);
    when(bulkProcessRepository.findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
        eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(expectedBulkProcessTypes), any(Date.class),eq(null),
        any(Pageable.class)))
        .thenReturn(page);
    when(bulkProcessDataEstimationService.fetchAllEstimationResponsesByProcessTypes(anyList())).thenReturn(Collections.singletonList(bulkProcessDataEstimation));
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean.fetchProcessListingResponse(
        DEFAULT_STORE_ID, bulkProcessType, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
        Optional.empty(), Optional.empty(), true, PageRequest.of(page.getNumber(),
            page.getSize()));
    verify(bulkProcessRepository).findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
        eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(expectedBulkProcessTypes), any(Date.class),eq(null),
        any(Pageable.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    assertTrue(page.hasContent());
    verify(bulkProcessDataEstimationService).fetchAllEstimationResponsesByProcessTypes(any());
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
  }


  @Test
  public void testFetchProcessListingResponse_ProductLevel3() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    String bulkProcessType = BulkProcessType.PRODUCT_LEVEL_3.getValue();
    List<BulkProcess> bulkProcess = getBulkProcess();
    bulkProcess.forEach(bulkProcess1 -> bulkProcess1.setBulkProcessType(bulkProcessType));
    Page<BulkProcess> page =
      new PageImpl<BulkProcess>(bulkProcess, PageRequest.of(0, bulkProcess.size()), bulkProcess.size());
    Set<String> expectedBulkProcessTypes = new HashSet<>(
      Arrays.asList(BulkProcessType.PRODUCT_LEVEL_3.getValue(), BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue(),BulkProcessType.IN_STORE.getValue(), BulkProcessType.SUBJECT_TO_VAT.getValue(),
        BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue(),
        BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue()));
    when(bulkProcessRepository.findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(expectedBulkProcessTypes), any(Date.class),eq(null),
      any(Pageable.class))).thenReturn(page);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn(Collections.singletonList(bulkProcessData1));
    Page<BulkProcessStatusListingResponse> result =
      bulkProcessServiceBean.fetchProcessListingResponse(DEFAULT_STORE_ID, bulkProcessType,
        DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE, Optional.empty(), Optional.empty(), false,
        PageRequest.of(page.getNumber(), page.getSize()));
    verify(bulkProcessRepository).findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(expectedBulkProcessTypes), any(Date.class), eq(null),
      any(Pageable.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    assertTrue(page.hasContent());
    Assertions.assertEquals(2,result.getContent().size());
  }

  @Test
  public void testFetchProcessListingResponse_campaign() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    String bulkProcessType = BulkProcessType.CAMPAIGN.getValue();
    List<BulkProcess> bulkProcess = getBulkProcess();
    bulkProcess.forEach(bp -> bp.setBulkProcessType(bulkProcessType));
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcess, PageRequest.of(0, 50),
      2);

    when(bulkProcessRepository.findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), anySet(), any(Date.class), eq(null), any(Pageable.class)))
      .thenReturn(bulkProcessPage);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn(Collections.singletonList(bulkProcessData1));
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean.fetchProcessListingResponse(
      DEFAULT_STORE_ID, CREATION_BULK_PROCESS_TYPE, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
      Optional.empty(), Optional.empty(), true, PageRequest.of(0,
        50));
    Mockito.verify(bulkProcessRepository).findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), anySet(), any(Date.class), eq(null), any(Pageable.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    Assertions.assertEquals(2, result.getTotalElements());
    Assertions.assertEquals(0, result.getNumber());
    Assertions.assertEquals(50, result.getSize());}

  @Test
  public void testFetchProcessListingResponse_campaignWithPrimaryIdentifier() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    String bulkProcessType = BulkProcessType.CAMPAIGN.getValue();
    List<BulkProcess> bulkProcess = getBulkProcess();
    bulkProcess.forEach(bp -> bp.setBulkProcessType(bulkProcessType));
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcess, PageRequest.of(0, 50), 2);

    when(bulkProcessRepository
        .findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
            eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), anySet(), any(Date.class), eq(PRIMARY_IDENTIFIER),
            any(Pageable.class))).thenReturn(bulkProcessPage);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class)))
        .thenReturn(Collections.singletonList(bulkProcessData1));
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean
        .fetchProcessListingResponse(DEFAULT_STORE_ID, CAMPAIGN_BULK_PROCESS_TYPE, DEFAULT_USERNAME,
            DEFAULT_BUSINESS_PARTNER_CODE, Optional.empty(), Optional.of(PRIMARY_IDENTIFIER), false,
            PageRequest.of(0, 50));
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
            eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), anySet(), any(Date.class), eq(PRIMARY_IDENTIFIER),
            any(Pageable.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    Assertions.assertEquals(2, result.getTotalElements());
    Assertions.assertEquals(0, result.getNumber());
    Assertions.assertEquals(50, result.getSize());
  }


  @Test
  public void testFetchProcessListingResponse_campaignDownloadwithPrimaryIdentifier() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    List<BulkDownloadEntity> bulkDownloadEntities = getDownloadEntities();
    bulkDownloadEntities.get(0).setStatus(BulkProcessConstant.DOWNLOAD_FAILED);
    Page<BulkDownloadEntity> bulkProcessPage = new PageImpl<>(bulkDownloadEntities, PageRequest.of(0, 50), 2);

    when(bulkDownloadAuditRepository
        .findByBusinessPartnerCodeAndPrimaryIdentifierAndEntityTypeInAndMarkForDeleteFalseAndCreatedDateLimit(
            eq(DEFAULT_BUSINESS_PARTNER_CODE), anySet(), any(Date.class), eq(PRIMARY_IDENTIFIER),
          any(Pageable.class)))
        .thenReturn(bulkProcessPage);
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean
        .fetchProcessListingResponse(DEFAULT_STORE_ID, CAMPAIGN_DOWNLOAD, DEFAULT_USERNAME,
            DEFAULT_BUSINESS_PARTNER_CODE, Optional.empty(), Optional.of(PRIMARY_IDENTIFIER), false,
            PageRequest.of(0, 50));
    Mockito.verify(bulkDownloadAuditRepository)
        .findByBusinessPartnerCodeAndPrimaryIdentifierAndEntityTypeInAndMarkForDeleteFalseAndCreatedDateLimit(
            eq(DEFAULT_BUSINESS_PARTNER_CODE), anySet(), any(Date.class), eq(PRIMARY_IDENTIFIER),
          any(Pageable.class));
    Assertions.assertEquals(1, result.getTotalElements());
    Assertions.assertEquals(0, result.getNumber());
    Assertions.assertEquals(50, result.getSize());
  }

  @Test
  public void testFetchProcessListingResponse_campaignDownloadwithPrimaryIdentifierNull() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    List<BulkDownloadEntity> bulkDownloadEntities = getDownloadEntities();
    bulkDownloadEntities.get(0).setStatus(BulkProcessConstant.DOWNLOAD_FAILED);
    Page<BulkDownloadEntity> bulkProcessPage = new PageImpl<>(bulkDownloadEntities, PageRequest.of(0, 50), 2);

    when(bulkDownloadAuditRepository
        .findByBusinessPartnerCodeAndPrimaryIdentifierAndEntityTypeInAndMarkForDeleteFalseAndCreatedDateLimit(
            eq(DEFAULT_BUSINESS_PARTNER_CODE), anySet(), any(Date.class), eq(null),
          any(Pageable.class)))
        .thenReturn(bulkProcessPage);
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean
        .fetchProcessListingResponse(DEFAULT_STORE_ID, CAMPAIGN_DOWNLOAD, DEFAULT_USERNAME,
            DEFAULT_BUSINESS_PARTNER_CODE, Optional.empty(), Optional.empty(), false,
            PageRequest.of(0, 50));
    Mockito.verify(bulkDownloadAuditRepository)
        .findByBusinessPartnerCodeAndPrimaryIdentifierAndEntityTypeInAndMarkForDeleteFalseAndCreatedDateLimit(
            eq(DEFAULT_BUSINESS_PARTNER_CODE), anySet(),any(Date.class), eq(null),
          any(Pageable.class));
    Assertions.assertEquals(1, result.getTotalElements());
    Assertions.assertEquals(0, result.getNumber());
    Assertions.assertEquals(50, result.getSize());
  }

  @Test
  public void testFetchProcessListingResponse_emptyResponse() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    String bulkProcessType = BulkProcessType.PRODUCT_LEVEL_3.getValue();
    List<BulkProcess> bulkProcess = getBulkProcess();
    bulkProcess.forEach(bulkProcess1 -> bulkProcess1.setBulkProcessType(bulkProcessType));
    Page<BulkProcess> page =
      new PageImpl<>(Collections.emptyList(), PageRequest.of(0, bulkProcess.size()),
        bulkProcess.size());
    Set<String> expectedBulkProcessTypes = new HashSet<>(
      Arrays.asList(BulkProcessType.PRODUCT_LEVEL_3.getValue(),
        BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue(),
        BulkProcessType.IN_STORE.getValue(), BulkProcessType.SUBJECT_TO_VAT.getValue(),
        BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue(),
        BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue()));
    when(bulkProcessRepository.findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(expectedBulkProcessTypes), any(Date.class), eq(null),
      any(Pageable.class))).thenReturn(page);
    Page<BulkProcessStatusListingResponse> result =
      bulkProcessServiceBean.fetchProcessListingResponse(DEFAULT_STORE_ID, bulkProcessType,
        DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE, Optional.empty(), Optional.empty(), false,
        PageRequest.of(page.getNumber(), page.getSize()));
    verify(bulkProcessRepository).findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(expectedBulkProcessTypes), any(Date.class), eq(null),
      any(Pageable.class));
    Assertions.assertFalse(page.hasContent());
    Assertions.assertEquals(0,result.getContent().size());
  }



  @Test
  public void testFetchProcessListingResponse_ProductCreationUploadForSwitchOff3() throws Exception {
    String bulkProcessType = BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue();
    bulkProcessCodes.add(DEFAULT_BULK_PROCESS_CODE);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    List<BulkProcess> bulkProcess = getBulkProcess();
    Page<BulkProcess> page = new PageImpl<BulkProcess>(bulkProcess,PageRequest.of(0,
      bulkProcess.size()),bulkProcess.size());
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn((Collections.singletonList(bulkProcessData1)));
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(bulkProcessCodes), any(Date.class),
      any(Pageable.class)))
      .thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn(Collections.singletonList(bulkProcessData1));
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean.fetchProcessListingResponse(
      DEFAULT_STORE_ID, bulkProcessType, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
      Optional.ofNullable(bulkProcessCodes), Optional.ofNullable(CAMPAIGN_CODE), false, PageRequest.of(page.getNumber(),
        page.getSize()));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(bulkProcessCodes), any(Date.class),
      any(Pageable.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    assertTrue(page.hasContent());
  }

  @Test
  public void testFetchProcessListingResponse_ProductCreationUploadForSwitchOff3Complete() throws Exception {
    String bulkProcessType = BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue();
    bulkProcessCodes.add(DEFAULT_BULK_PROCESS_CODE);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    List<BulkProcess> bulkProcess = getBulkProcess();
    bulkProcess.forEach(b ->{ b.setTotalCount(1);b.setErrorCount(0); b.setSuccessCount(1); b.setStatus(BulkProcess.STATUS_IN_PROGRESS);});
    bulkProcessData1.setStatus(BulkProcessData.STATUS_SUCCESS);
    Page<BulkProcess> page = new PageImpl<BulkProcess>(bulkProcess,PageRequest.of(0,
      bulkProcess.size()),bulkProcess.size());
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn((Collections.singletonList(bulkProcessData1)));
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(bulkProcessCodes), any(Date.class),
      any(Pageable.class)))
      .thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn(Collections.singletonList(bulkProcessData1));
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean.fetchProcessListingResponse(
      DEFAULT_STORE_ID, bulkProcessType, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
      Optional.ofNullable(bulkProcessCodes), Optional.of(CAMPAIGN_CODE), false, PageRequest.of(page.getNumber(),
        page.getSize()));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(bulkProcessCodes), any(Date.class),
      any(Pageable.class));
    verify(bulkProcessDataService, times(2)).findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class));
    Assertions.assertEquals(result.getContent().get(0).getBulkActivityStatus(),BulkActivityStatus.SUCCESS
    );
  }

  @Test
  public void testFetchProcessListingResponse_ProductCreationUploadForSwitchOff3Complete2() throws Exception {
    String bulkProcessType = BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue();
    bulkProcessCodes.add(DEFAULT_BULK_PROCESS_CODE);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    List<BulkProcess> bulkProcess = getBulkProcess();
    bulkProcess.forEach(b ->{ b.setTotalCount(2);b.setErrorCount(0); b.setSuccessCount(0); b.setStatus(BulkProcess.STATUS_IN_PROGRESS);});
    bulkProcessData1.setStatus(BulkProcessData.STATUS_FAIL);
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setBulkProcessCode(bulkProcessData1.getBulkProcessCode());
    bulkProcessData2.setBulkProcessId(bulkProcessData1.getBulkProcessId());
    bulkProcessData2.setStatus(BulkProcessData.STATUS_SUCCESS);
    Page<BulkProcess> page = new PageImpl<BulkProcess>(bulkProcess,PageRequest.of(0,
      bulkProcess.size()),bulkProcess.size());
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn((Arrays.asList(bulkProcessData1, bulkProcessData2)));
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(bulkProcessCodes), any(Date.class),
      any(Pageable.class)))
      .thenReturn(bulkProcess);
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean.fetchProcessListingResponse(
      DEFAULT_STORE_ID, bulkProcessType, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
      Optional.ofNullable(bulkProcessCodes), Optional.of(CAMPAIGN_CODE), false, PageRequest.of(page.getNumber(),
        page.getSize()));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(bulkProcessCodes), any(Date.class),
      any(Pageable.class));
    verify(bulkProcessDataService, times(2)).findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class));
    Assertions.assertEquals(result.getContent().get(0).getBulkActivityStatus(),BulkActivityStatus.PARTIAL_SUCCESS
    );
  }

  @Test
  public void testFetchProcessListingResponse_ProductCreationUploadForSwitchOffPartialSuccess() throws Exception {
    String bulkProcessType = BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue();
    bulkProcessCodes.add(DEFAULT_BULK_PROCESS_CODE);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    List<BulkProcess> bulkProcess = getBulkProcess();
    bulkProcess.forEach(b ->{ b.setTotalCount(2);b.setErrorCount(1); b.setSuccessCount(1); b.setStatus(BulkProcess.STATUS_IN_PROGRESS);});
    bulkProcessData1.setStatus(BulkProcessData.STATUS_SUCCESS);
    BulkProcessData bulkProcessData2 = bulkProcessData1;
    bulkProcessData2.setStatus(BulkProcessData.STATUS_FAIL);
    Page<BulkProcess> page = new PageImpl<BulkProcess>(bulkProcess,PageRequest.of(0,
      bulkProcess.size()),bulkProcess.size());
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn((Arrays.asList(bulkProcessData1, bulkProcessData2)));
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(bulkProcessCodes), any(Date.class),
      any(Pageable.class)))
      .thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn(Arrays.asList(bulkProcessData1, bulkProcessData2));
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean.fetchProcessListingResponse(
      DEFAULT_STORE_ID, bulkProcessType, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
      Optional.ofNullable(bulkProcessCodes), Optional.of(CAMPAIGN_CODE), false, PageRequest.of(page.getNumber(),
        page.getSize()));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(bulkProcessCodes), any(Date.class),
      any(Pageable.class));
    verify(bulkProcessDataService, times(2)).findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class));
    Assertions.assertEquals(result.getContent().get(0).getBulkActivityStatus(),BulkActivityStatus.FAILED
    );
  }


  @Test
  public void testFetchProcessListingResponse_ProductCreationUploadForSwitchOffNullBulkCode() throws Exception {
    String bulkProcessType = BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue();
    bulkProcessCodes.add(DEFAULT_BULK_PROCESS_CODE);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    List<BulkProcess> bulkProcess = getBulkProcess();
    bulkProcess.forEach(b ->{ b.setTotalCount(2);b.setErrorCount(1); b.setSuccessCount(1); b.setStatus(BulkProcess.STATUS_IN_PROGRESS);});
    bulkProcess.get(1).setBulkProcessCode(null);
    bulkProcessData1.setStatus(BulkProcessData.STATUS_SUCCESS);
    BulkProcessData bulkProcessData2 = bulkProcessData1;
    bulkProcessData2.setStatus(BulkProcessData.STATUS_FAIL);
    Page<BulkProcess> page = new PageImpl<BulkProcess>(bulkProcess,PageRequest.of(0,
      bulkProcess.size()),bulkProcess.size());
    bulkProcessData2.setBulkProcessCode(null);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn((Arrays.asList(bulkProcessData1, bulkProcessData2)));
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(bulkProcessCodes), any(Date.class),
      any(Pageable.class)))
      .thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn(Arrays.asList(bulkProcessData1, bulkProcessData2));
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean.fetchProcessListingResponse(
      DEFAULT_STORE_ID, bulkProcessType, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
      Optional.ofNullable(bulkProcessCodes), Optional.of(CAMPAIGN_CODE), false, PageRequest.of(page.getNumber(),
        page.getSize()));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(bulkProcessCodes), any(Date.class),
      any(Pageable.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class));
    Assertions.assertEquals(result.getContent().get(0).getBulkActivityStatus(),BulkActivityStatus.FAILED
    );
  }


  @Test
  public void testFetchProcessListingResponse_ProductLevel3SwitchOff() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    String bulkProcessType = BulkProcessType.PRODUCT_LEVEL_3.getValue();
    bulkProcessCodes.add(DEFAULT_BULK_PROCESS_CODE);
    List<BulkProcess> bulkProcess = getBulkProcess();
    bulkProcess.forEach(bulkProcess1 -> bulkProcess1.setBulkProcessType(bulkProcessType));
    Page<BulkProcess> page =
      new PageImpl<>(bulkProcess, PageRequest.of(0, bulkProcess.size()), bulkProcess.size());
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(bulkProcessCodes), any(Date.class),
      any(Pageable.class)))
      .thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn(Collections.singletonList(bulkProcessData1));
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn((Collections.singletonList(bulkProcessData1)));
    Page<BulkProcessStatusListingResponse> result =
      bulkProcessServiceBean.fetchProcessListingResponse(DEFAULT_STORE_ID, bulkProcessType,
        DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE, Optional.ofNullable(bulkProcessCodes),
        Optional.of(CAMPAIGN_CODE), false, PageRequest.of(page.getNumber(), page.getSize()));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(bulkProcessCodes), any(Date.class),
      any(Pageable.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    assertTrue(page.hasContent());
    Assertions.assertEquals(2,result.getContent().size());
  }

  @Test
  public void testFetchProcessListingResponse_campaignSwitchOff() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    String bulkProcessType = BulkProcessType.CAMPAIGN.getValue();
    BulkProcess bulkProcess1 = new BulkProcess(DEFAULT_BULK_PROCESS_CODE, "Campaign",
      DEFAULT_BUSINESS_PARTNER_CODE, null, null,
      BulkProcess.STATUS_PENDING, null, new ArrayList<BulkProcessNotes>());
    List<BulkProcess> bulkProcess = getBulkProcess();
    bulkProcess.forEach(bp -> bp.setBulkProcessType(bulkProcessType));
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcess, PageRequest.of(0, 50),
      2);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      eq(DEFAULT_STORE_ID), eq(bulkProcessCodes), any(Date.class),
      any(Pageable.class)))
      .thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class))).thenReturn((Collections.singletonList(bulkProcessData1)));
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean.fetchProcessListingResponse(
      DEFAULT_STORE_ID, BULK_PROCESS_TYPE, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
      Optional.ofNullable(bulkProcessCodes), Optional.of(CAMPAIGN_CODE), false, PageRequest.of(0,
        50));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
      anyString(), anyList(), any(Date.class),
      any(Pageable.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class));
    Assertions.assertEquals(2, result.getTotalElements());
    Assertions.assertEquals(0, result.getNumber());
    Assertions.assertEquals(50, result.getSize());}

  @Test
  public void testFetchProcessListingResponse_InProgressandEstimationneededwithBulkProcessCode() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    ReflectionTestUtils.setField(bulkProcessServiceBean, "estimationFetchEnabledForListing", true);
    String bulkProcessType = BulkProcessType.CAMPAIGN.getValue();
    BulkProcess bulkProcess1 = new BulkProcess(DEFAULT_BULK_PROCESS_CODE, "Campaign",
        DEFAULT_BUSINESS_PARTNER_CODE, null, null,
        BulkProcess.STATUS_PENDING, null, new ArrayList<BulkProcessNotes>());
    List<BulkProcess> bulkProcess = getBulkProcess();
    bulkProcess.forEach(bp -> bp.setBulkProcessType(bulkProcessType));
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcess, PageRequest.of(0, 50),
        2);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
        eq(DEFAULT_STORE_ID), eq(bulkProcessCodes), any(Date.class),
        any(Pageable.class)))
        .thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(),
        any(BulkProcess.class))).thenReturn((Collections.singletonList(bulkProcessData1)));
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean.fetchProcessListingResponse(
        DEFAULT_STORE_ID, BULK_PROCESS_TYPE, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
        Optional.ofNullable(bulkProcessCodes), Optional.of(CAMPAIGN_CODE), true, PageRequest.of(0,
            50));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
        anyString(), anyList(), any(Date.class),
        any(Pageable.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(),
        any(BulkProcess.class));
    verify(bulkProcessDataEstimationService).fetchAllEstimationResponsesByProcessTypes(anyList());
    Assertions.assertEquals(2, result.getTotalElements());
    Assertions.assertEquals(0, result.getNumber());
    Assertions.assertEquals(50, result.getSize());
  }

  @Test
  public void testFetchProcessListingResponse_InProgressandEstimationneededFalsewithBulkProcessCode() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "maxFetchDaysForListing", "1");
    ReflectionTestUtils.setField(bulkProcessServiceBean, "estimationFetchEnabledForListing", false);
    String bulkProcessType = BulkProcessType.CAMPAIGN.getValue();
    BulkProcess bulkProcess1 =
        new BulkProcess(DEFAULT_BULK_PROCESS_CODE, "Campaign", DEFAULT_BUSINESS_PARTNER_CODE, null, null,
            BulkProcess.STATUS_PENDING, null, new ArrayList<BulkProcessNotes>());
    List<BulkProcess> bulkProcess = getBulkProcess();
    bulkProcess.forEach(bp -> bp.setBulkProcessType(bulkProcessType));
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcess, PageRequest.of(0, 50), 2);
    when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(eq(DEFAULT_STORE_ID),
            eq(bulkProcessCodes), any(Date.class), any(Pageable.class))).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class)))
        .thenReturn((Collections.singletonList(bulkProcessData1)));
    Page<BulkProcessStatusListingResponse> result = bulkProcessServiceBean
        .fetchProcessListingResponse(DEFAULT_STORE_ID, BULK_PROCESS_TYPE, DEFAULT_USERNAME,
            DEFAULT_BUSINESS_PARTNER_CODE, Optional.ofNullable(bulkProcessCodes), Optional.of(CAMPAIGN_CODE), false,
            PageRequest.of(0, 50));
    verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(anyString(), anyList(),
            any(Date.class), any(Pageable.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    Assertions.assertEquals(2, result.getTotalElements());
    Assertions.assertEquals(0, result.getNumber());
    Assertions.assertEquals(50, result.getSize());
  }

  @Test
  public void evaluateBulkProcessEstimationTest() throws JsonProcessingException {
    doNothing().when(bulkProcessDataEstimationService).updateBulkDataEstimationByProcessTypes(DEFAULT_STORE_ID
      ,Constant.USER_NAME);
    bulkProcessServiceBean.evaluateBulkProcessEstimation(DEFAULT_STORE_ID, Constant.USER_NAME);
    verify(bulkProcessDataEstimationService).updateBulkDataEstimationByProcessTypes(DEFAULT_STORE_ID,
      Constant.USER_NAME);
  }

  @Test
  public void testSetBulkProcessCompletionEstimatesForInProgressProcess() throws Exception {
    BulkProcessStatusListingResponse response = new BulkProcessStatusListingResponse();
    response.setBulkProcessType("ProductCreationUploadPriority1");
    response.setBulkActivityStatus(BulkActivityStatus.IN_PROGRESS);
    response.setCreatedDate(new Date());
    Map<String, Double> bulkProcessCodeXEstimationsMap = new HashMap<>();
    bulkProcessCodeXEstimationsMap.put(DEFAULT_BULK_PROCESS_CODE, 3732.5384615384614);
    String deltaTimeEstimations = "{\"4\":4743.0,\"5\":37.72,\"8\":3732.5384615384614,\"9\":32.0,"
      + "\"11\":24.0,\"12\":1549.6666666666667,\"13\":2273.6666666666665,\"14\":23.25,\"15\":46.0,\"16\":30.0,\"18\":27.583333333333332,\"19\":30.0,\"20\":24.0,\"21\":25.0,\"22\":23.0,\"23\":19.5}";
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(false).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    List<BulkProcessDataEstimation> estimations = new ArrayList<>();
    estimations.add(bulkProcessDataEstimation);
    Map<String, Double> estimationsMap = new HashMap<>();
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
      bulkProcessCodeXEstimationsMap);
    bulkProcessServiceBean.setEstimationsForListingResponses(response, estimations,
      bulkProcessCodeXEstimationsMap);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
  }

  @Test
  public void testSetBulkProcessCompletionEstimatesForInProgress_withIgnoreZeroTime() throws Exception {
    BulkProcessStatusListingResponse response = new BulkProcessStatusListingResponse();
    response.setBulkProcessType("ProductCreationUploadPriority1");
    response.setBulkActivityStatus(BulkActivityStatus.IN_PROGRESS);
    response.setCreatedDate(new Date());
    Map<String, Double> bulkProcessCodeXEstimationsMap = new HashMap<>();
    String deltaTimeEstimations = "{\"4\":4743.0,\"5\":37.72,\"8\":0.0,\"9\":null}";
    TypeReference<Map<Integer, Double>> typeRef = new TypeReference<Map<Integer, Double>>() {
    };
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(false).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    List<BulkProcessDataEstimation> estimations = new ArrayList<>();
    estimations.add(bulkProcessDataEstimation);
    Map<String, Double> estimationsMap = new HashMap<>();
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
      mapper.readValue(deltaTimeEstimations,typeRef));
    bulkProcessServiceBean.setEstimationsForListingResponses(response, estimations, bulkProcessCodeXEstimationsMap);
    Map<Integer, Double> deltaHourXEstimationForRecordLevelMap = objectMapper.readValue(deltaTimeEstimations, typeRef);
    deltaHourXEstimationForRecordLevelMap.forEach(
      (key, value) -> Assertions.assertFalse(Objects.isNull(value) || value == 0.0));
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));

  }

  @Test
  public void testSetBulkProcessCompletionEstimatesForPending_withIgnoreZeroTime() throws Exception {
    BulkProcessStatusListingResponse response = new BulkProcessStatusListingResponse();
    response.setBulkProcessType("ProductCreationUploadPriority1");
    Date currentDate = new Date();
    Date pastDate = new Date(currentDate.getTime() - 3600000);
    response.setBulkActivityStatus(BulkActivityStatus.PENDING);
    response.setCreatedDate(pastDate);
    response.setStoreId(DEFAULT_STORE_ID);
    when(bulkProcessRepository.countByStoreIdAndBulkProcessTypeAndStatusInAndCreatedDateBefore(
      any(), anyString(), anyList(),
      any(Date.class))).thenReturn(3L);
    Map<String, Double> bulkProcessCodeXEstimationsMap = new HashMap<>();
    String deltaTimeEstimations = "{\"4\":4743.0,\"5\":37.72,\"8\":0.0,\"9\":null}";
    TypeReference<Map<Integer, Double>> typeRef = new TypeReference<Map<Integer, Double>>() {
    };
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(true).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    List<BulkProcessDataEstimation> estimations = new ArrayList<>();
    estimations.add(bulkProcessDataEstimation);
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
      mapper.readValue(deltaTimeEstimations,typeRef));
    bulkProcessServiceBean.setEstimationsForListingResponses(response, estimations,
      bulkProcessCodeXEstimationsMap);
    Map<Integer, Double> deltaHourXEstimationForProcessLevelMap =
      objectMapper.readValue(deltaTimeEstimations, typeRef);
    deltaHourXEstimationForProcessLevelMap.forEach(
      (key, value) -> Assertions.assertFalse(Objects.isNull(value) || value == 0.0));
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessRepository).countByStoreIdAndBulkProcessTypeAndStatusInAndCreatedDateBefore(any(), anyString(), anyList(),
      any(Date.class));
  }

  @Test
  public void testSetBulkProcessCompletionEstimatesForInProgressWithZeroEstimatesProcess() throws Exception {
    BulkProcessStatusListingResponse response = new BulkProcessStatusListingResponse();
    response.setBulkProcessType("ProductCreationUploadPriority1");
    response.setBulkActivityStatus(BulkActivityStatus.IN_PROGRESS);
    response.setCreatedDate(new Date());
    Map<String, Double> bulkProcessCodeXEstimationsMap = new HashMap<>();
    bulkProcessCodeXEstimationsMap.put(DEFAULT_BULK_PROCESS_CODE, 0.0);
    String deltaTimeEstimations = "";
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(false).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    List<BulkProcessDataEstimation> estimations = new ArrayList<>();
    estimations.add(bulkProcessDataEstimation);
    Map<String, Double> estimationsMap = new HashMap<>();
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
      bulkProcessCodeXEstimationsMap);
    bulkProcessServiceBean.setEstimationsForListingResponses(response, estimations,
      bulkProcessCodeXEstimationsMap);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
  }

  @Test
  public void testSetBulkProcessCompletionEstimatesForInProgressWithNullEstimationsProcess()
    throws Exception {
    BulkProcessStatusListingResponse response = new BulkProcessStatusListingResponse();
    response.setBulkProcessType("ProductCreationUploadPriority1");
    response.setBulkActivityStatus(BulkActivityStatus.IN_PROGRESS);
    response.setCreatedDate(new Date());
    Map<String, Double> bulkProcessCodeXEstimationsMap = new HashMap<>();
    bulkProcessCodeXEstimationsMap.put(DEFAULT_BULK_PROCESS_CODE, 3732.5384615384614);
    String deltaTimeEstimations = "{\"4\":4743.0,\"5\":37.72,\"8\":3732.5384615384614,\"9\":32.0,"
      + "\"11\":24.0,\"12\":1549.6666666666667,\"13\":2273.6666666666665,\"14\":23.25,\"15\":46.0,\"16\":30.0,\"18\":27.583333333333332,\"19\":30.0,\"20\":24.0,\"21\":25.0,\"22\":23.0,\"23\":19.5}";
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(true).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    List<BulkProcessDataEstimation> estimations = new ArrayList<>();
    estimations.add(bulkProcessDataEstimation);
    Map<String, Double> estimationsMap = new HashMap<>();
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
      bulkProcessCodeXEstimationsMap);
    bulkProcessServiceBean.setEstimationsForListingResponses(response, estimations,
      bulkProcessCodeXEstimationsMap);
  }

  @Test
  public void testSetBulkProcessCompletionEstimatesForInProgressProcessWithException()
    throws Exception {
    BulkProcessStatusListingResponse response = new BulkProcessStatusListingResponse();
    response.setBulkProcessType("ProductCreationUploadPriority1");
    response.setBulkActivityStatus(BulkActivityStatus.IN_PROGRESS);
    response.setCreatedDate(new Date());
    Map<String, Double> bulkProcessCodeXEstimationsMap = new HashMap<>();
    bulkProcessCodeXEstimationsMap.put(DEFAULT_BULK_PROCESS_CODE, 3732.5384615384614);
    String deltaTimeEstimations = "{\"4\":4743.0,\"5\":37.72,\"8\":3732.5384615384614,\"9\":32.0,"
      + "\"11\":24.0,\"12\":1549.6666666666667,\"13\":2273.6666666666665,\"14\":23.25,\"15\":46.0,\"16\":30.0,\"18\":27.583333333333332,\"19\":30.0,\"20\":24.0,\"21\":25.0,\"22\":23.0,\"23\":19.5}";
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(false).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    List<BulkProcessDataEstimation> estimations = new ArrayList<>();
    estimations.add(bulkProcessDataEstimation);
    Map<String, Double> estimationsMap = new HashMap<>();
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenThrow(
      JsonProcessingException.class);
    bulkProcessServiceBean.setEstimationsForListingResponses(response, estimations,
      bulkProcessCodeXEstimationsMap);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    Assertions.assertNull(response.getEstimatedCompletionTime());
  }


  @Test
  public void testSetBulkProcessCompletionEstimatesForPendingProcess() throws Exception {
    BulkProcessStatusListingResponse response = new BulkProcessStatusListingResponse();
    response.setBulkProcessType("ProductCreationUploadPriority1");
    Date currentDate = new Date();
    Date pastDate = new Date(currentDate.getTime() - 3600000);
    response.setBulkActivityStatus(BulkActivityStatus.PENDING);
    response.setCreatedDate(pastDate);
    response.setStoreId(DEFAULT_STORE_ID);
    when(bulkProcessRepository.countByStoreIdAndBulkProcessTypeAndStatusInAndCreatedDateBefore(
      any(), anyString(), anyList(),
      any(Date.class))).thenReturn(3L);
    Map<String, Double> bulkProcessCodeXEstimationsMap = new HashMap<>();
    bulkProcessCodeXEstimationsMap.put(DEFAULT_BULK_PROCESS_CODE, 3732.5384615384614);
    String deltaTimeEstimations = "{\"4\":4743.0,\"5\":37.72,\"8\":3732.5384615384614,\"9\":32.0,"
      + "\"11\":24.0,\"12\":1549.6666666666667,\"13\":2273.6666666666665,\"14\":23.25,\"15\":46.0,\"16\":30.0,\"18\":27.583333333333332,\"19\":30.0,\"20\":24.0,\"21\":25.0,\"22\":23.0,\"23\":19.5}";
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(true).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    List<BulkProcessDataEstimation> estimations = new ArrayList<>();
    estimations.add(bulkProcessDataEstimation);
    Map<String, Double> estimationsMap = new HashMap<>();
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
      bulkProcessCodeXEstimationsMap);
    bulkProcessServiceBean.setEstimationsForListingResponses(response, estimations,
      bulkProcessCodeXEstimationsMap);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessRepository).countByStoreIdAndBulkProcessTypeAndStatusInAndCreatedDateBefore(any(), anyString(), anyList(),
      any(Date.class));
  }

  @Test
  public void testSetBulkProcessCompletionEstimatesForPendingProcess_WithNoProcessInQueue() throws Exception {
    BulkProcessStatusListingResponse response = new BulkProcessStatusListingResponse();
    response.setBulkProcessType("ProductCreationUploadPriority1");
    Date currentDate = new Date();
    Date pastDate = new Date(currentDate.getTime() - 3600000);
    response.setBulkActivityStatus(BulkActivityStatus.PENDING);
    response.setCreatedDate(pastDate);
    response.setStoreId(DEFAULT_STORE_ID);
    when(bulkProcessRepository.countByStoreIdAndBulkProcessTypeAndStatusInAndCreatedDateBefore(
      any(), anyString(), anyList(),
      any(Date.class))).thenReturn(0L);
    Map<String, Double> bulkProcessCodeXEstimationsMap = new HashMap<>();
    bulkProcessCodeXEstimationsMap.put(DEFAULT_BULK_PROCESS_CODE, 3732.5384615384614);
    String deltaTimeEstimations = "{\"4\":4743.0,\"5\":37.72,\"8\":3732.5384615384614,\"9\":32.0,"
      + "\"11\":24.0,\"12\":1549.6666666666667,\"13\":2273.6666666666665,\"14\":23.25,\"15\":46.0,\"16\":30.0,\"18\":27.583333333333332,\"19\":30.0,\"20\":24.0,\"21\":25.0,\"22\":23.0,\"23\":19.5}";
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(true).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    List<BulkProcessDataEstimation> estimations = new ArrayList<>();
    estimations.add(bulkProcessDataEstimation);
    Map<String, Double> estimationsMap = new HashMap<>();
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
      bulkProcessCodeXEstimationsMap);
    bulkProcessServiceBean.setEstimationsForListingResponses(response, estimations,
      bulkProcessCodeXEstimationsMap);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessRepository).countByStoreIdAndBulkProcessTypeAndStatusInAndCreatedDateBefore(any(), anyString(), anyList(),
      any(Date.class));
    Assertions.assertNotNull(response.getEstimatedCompletionTime());
  }

  @Test
  public void testSetBulkProcessCompletionEstimatesForPendingProcess_nullEstimations() throws Exception {
    BulkProcessStatusListingResponse response = new BulkProcessStatusListingResponse();
    response.setBulkProcessType("ProductCreationUploadPriority1");
    Date currentDate = new Date();
    Date pastDate = new Date(currentDate.getTime() - 3600000);
    response.setBulkActivityStatus(BulkActivityStatus.PENDING);
    response.setCreatedDate(pastDate);
    response.setStoreId(DEFAULT_STORE_ID);
    when(bulkProcessRepository.countByStoreIdAndBulkProcessTypeAndStatusInAndCreatedDateBefore(
      any(), anyString(), anyList(),
      any(Date.class))).thenReturn(3L);
    Map<String, Double> bulkProcessCodeXEstimationsMap = new HashMap<>();
    bulkProcessCodeXEstimationsMap.put(DEFAULT_BULK_PROCESS_CODE, 3732.5384615384614);
    String deltaTimeEstimations = "{\"4\":4743.0,\"5\":37.72,\"8\":3732.5384615384614,\"9\":32.0,"
      + "\"11\":24.0,\"12\":1549.6666666666667,\"13\":2273.6666666666665,\"14\":23.25,\"15\":46.0,\"16\":30.0,\"18\":27.583333333333332,\"19\":30.0,\"20\":24.0,\"21\":25.0,\"22\":23.0,\"23\":19.5}";
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(false).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    List<BulkProcessDataEstimation> estimations = new ArrayList<>();
    estimations.add(bulkProcessDataEstimation);
    Map<String, Double> estimationsMap = new HashMap<>();
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
      bulkProcessCodeXEstimationsMap);
    bulkProcessServiceBean.setEstimationsForListingResponses(response, estimations,
      bulkProcessCodeXEstimationsMap);
    verify(bulkProcessRepository).countByStoreIdAndBulkProcessTypeAndStatusInAndCreatedDateBefore(any(), anyString(), anyList(),
      any(Date.class));
  }

  @Test
  public void testSetBulkProcessCompletionEstimatesForPendingWithZeroEstimationsProcess() throws Exception {
    BulkProcessStatusListingResponse response = new BulkProcessStatusListingResponse();
    response.setBulkProcessType("ProductCreationUploadPriority1");
    Date currentDate = new Date();
    Date pastDate = new Date(currentDate.getTime() - 3600000);
    response.setBulkActivityStatus(BulkActivityStatus.PENDING);
    response.setCreatedDate(pastDate);
    response.setStoreId(DEFAULT_STORE_ID);
    when(bulkProcessRepository.countByStoreIdAndBulkProcessTypeAndStatusInAndCreatedDateBefore(
      any(), anyString(), anyList(),
      any(Date.class))).thenReturn(3L);
    Map<String, Double> bulkProcessCodeXEstimationsMap = new HashMap<>();
    bulkProcessCodeXEstimationsMap.put(DEFAULT_BULK_PROCESS_CODE, 0.0);
    String deltaTimeEstimations = "";
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(true).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    List<BulkProcessDataEstimation> estimations = new ArrayList<>();
    estimations.add(bulkProcessDataEstimation);
    Map<String, Double> estimationsMap = new HashMap<>();
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
      bulkProcessCodeXEstimationsMap);
    bulkProcessServiceBean.setEstimationsForListingResponses(response, estimations,
      bulkProcessCodeXEstimationsMap);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessRepository).countByStoreIdAndBulkProcessTypeAndStatusInAndCreatedDateBefore(any(), anyString(), anyList(),
      any(Date.class));
  }

  @Test
  public void testSetBulkProcessCompletionEstimatesForPendingWithException() throws Exception {
    BulkProcessStatusListingResponse response = new BulkProcessStatusListingResponse();
    response.setBulkProcessType("ProductCreationUploadPriority1");
    Date currentDate = new Date();
    Date pastDate = new Date(currentDate.getTime() - 3600000);
    response.setBulkActivityStatus(BulkActivityStatus.PENDING);
    response.setCreatedDate(pastDate);
    response.setStoreId(DEFAULT_STORE_ID);
    when(bulkProcessRepository.countByStoreIdAndBulkProcessTypeAndStatusInAndCreatedDateBefore(
      any(), anyString(), anyList(),
      any(Date.class))).thenReturn(3L);
    Map<String, Double> bulkProcessCodeXEstimationsMap = new HashMap<>();
    bulkProcessCodeXEstimationsMap.put(DEFAULT_BULK_PROCESS_CODE, 0.0);
    String deltaTimeEstimations = "";
    BulkProcessDataEstimation bulkProcessDataEstimation =
      BulkProcessDataEstimation.builder().processType("ProductCreationUploadPriority1")
        .processLevelFetch(true).deltaTimeEstimations(deltaTimeEstimations)
        .lastFetchTime(new Date()).build();
    List<BulkProcessDataEstimation> estimations = new ArrayList<>();
    estimations.add(bulkProcessDataEstimation);
    Map<String, Double> estimationsMap = new HashMap<>();
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenThrow(
      JsonProcessingException.class);
    bulkProcessServiceBean.setEstimationsForListingResponses(response, estimations,
      bulkProcessCodeXEstimationsMap);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessRepository).countByStoreIdAndBulkProcessTypeAndStatusInAndCreatedDateBefore(any(), anyString(), anyList(),
      any(Date.class));
    Assertions.assertNull(response.getEstimatedCompletionTime());
  }

  @Test
  public void testCheckForQrProcessAllowedSuccess() {
    List<String> unprocessedStatus = Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
        BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
        BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_PROCESSED, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
      BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2);
    Mockito.when(
        this.bulkProcessRepository.countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(
            DEFAULT_STORE_ID, BULK_UPDATE_MERCHANT_CODE, unprocessedStatus,
            ImmutableSet.of(QR_GENERATION.getValue()))).thenReturn(COUNT_FALSE);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.MAX_QR_ALLOWED_BY_PARTNER);
    systemParameterConfig.setValue(String.valueOf(10));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.MAX_QR_ALLOWED_BY_PARTNER)).thenReturn(systemParameterConfig);
    boolean result = bulkProcessServiceBean.checkForQrProcessAllowed(DEFAULT_STORE_ID,
        BULK_UPDATE_MERCHANT_CODE);
    assertTrue(result);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.MAX_QR_ALLOWED_BY_PARTNER);
    verify(
        this.bulkProcessRepository).countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, BULK_UPDATE_MERCHANT_CODE, unprocessedStatus,
        ImmutableSet.of(QR_GENERATION.getValue()));
  }

  @Test
  public void testCheckForQrProcessAllowedFalse() {
    List<String> unprocessedStatus = Arrays.asList(DEFAULT_BULK_UPDATE_STATE, BulkProcess.STATUS_IN_PROGRESS,
        BulkProcess.STATUS_IMAGE_PROCESSING, BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
        BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcess.STATUS_PUBLISHED, BulkProcess.STATUS_PROCESSED, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
      BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1, BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2);
    Mockito.when(
        this.bulkProcessRepository.countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(
            DEFAULT_STORE_ID, BULK_UPDATE_MERCHANT_CODE, unprocessedStatus,
            ImmutableSet.of(QR_GENERATION.getValue()))).thenReturn(COUNT_FALSE);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.MAX_QR_ALLOWED_BY_PARTNER);
    systemParameterConfig.setValue(String.valueOf(1));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.MAX_QR_ALLOWED_BY_PARTNER)).thenReturn(systemParameterConfig);
    boolean result = bulkProcessServiceBean.checkForQrProcessAllowed(DEFAULT_STORE_ID,
        BULK_UPDATE_MERCHANT_CODE);
    Assertions.assertFalse(result);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.MAX_QR_ALLOWED_BY_PARTNER);
    verify(
        this.bulkProcessRepository).countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, BULK_UPDATE_MERCHANT_CODE, unprocessedStatus,
        ImmutableSet.of(QR_GENERATION.getValue()));
  }

  @Test
  public void checkBulkProcessStatusQRCodeTest() throws Exception {
    BulkProcess bulkProcess = getBulkProcess().get(0);
    bulkProcess.setUploadedFile(null);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_QRCODE_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            QR_GENERATION_TYPE, BulkProcess.STATUS_PENDING,
            PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(bulkProcess));
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            QR_GENERATION_TYPE, BulkProcess.STATUS_PUBLISHED,
            PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(bulkProcess));
    Mockito.when(bulkProcessDataService.getPendingBulkProcessCodes(Constant.STORE_ID,
        Collections.singletonList(bulkProcess.getBulkProcessCode()))).thenReturn(null);
    Mockito.doNothing().when(bulkProcessDataService)
        .saveRequestInBulkProcessData(Mockito.any(DownloadQRCodeRequest.class), Mockito.any(BulkProcess.class));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(DownloadQRCodeRequest.class))).thenReturn(new DownloadQRCodeRequest());
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, QR_GENERATION_TYPE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.FETCH_PENDING_QRCODE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, QR_GENERATION_TYPE,
            BulkProcess.STATUS_PENDING, PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, QR_GENERATION_TYPE,
            BulkProcess.STATUS_PUBLISHED, PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessDataService)
        .getPendingBulkProcessCodes(Constant.STORE_ID, Collections.singletonList(bulkProcess.getBulkProcessCode()));
    Mockito.verify(bulkProcessDataService)
        .saveRequestInBulkProcessData(Mockito.any(DownloadQRCodeRequest.class), Mockito.any(BulkProcess.class));
    Mockito.verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(DownloadQRCodeRequest.class));
    Mockito.verify(bulkProcessRepository).save(Mockito.any(BulkProcess.class));
  }

  @Test
  public void checkBulkProcessStatusQRCodeExceptionTest() throws Exception {
    BulkProcess bulkProcess = getBulkProcess().get(0);
    bulkProcess.setUploadedFile(null);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_QRCODE_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            QR_GENERATION_TYPE, BulkProcess.STATUS_PENDING,
            PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(bulkProcess));
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            QR_GENERATION_TYPE, BulkProcess.STATUS_PUBLISHED,
            PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(bulkProcess));
    Mockito.when(bulkProcessDataService.getPendingBulkProcessCodes(Constant.STORE_ID,
        Collections.singletonList(bulkProcess.getBulkProcessCode()))).thenReturn(null);
    Mockito.doNothing().when(bulkProcessDataService)
        .saveRequestInBulkProcessData(Mockito.any(DownloadQRCodeRequest.class), Mockito.any(BulkProcess.class));
    Mockito.doThrow(new ApplicationRuntimeException()).when(objectMapper)
        .readValue(Mockito.anyString(), Mockito.eq(DownloadQRCodeRequest.class));

    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, QR_GENERATION_TYPE);

    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.FETCH_PENDING_QRCODE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, QR_GENERATION_TYPE,
            BulkProcess.STATUS_PENDING, PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, QR_GENERATION_TYPE,
            BulkProcess.STATUS_PUBLISHED, PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessDataService)
        .getPendingBulkProcessCodes(Constant.STORE_ID, Collections.singletonList(bulkProcess.getBulkProcessCode()));
    Mockito.verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(DownloadQRCodeRequest.class));
    Mockito.verify(bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    Mockito.verify(notificationService)
        .sendGenerateQrCodeFailedNotification(bulkProcess.getBusinessPartnerCode(), bulkProcess.getDescription());
  }

  @Test
  public void uploadQrExcelRequestTest() throws Exception {
    bulkUpdateProcessDTO.setBulkProcessType(QR_GENERATION.getValue());
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
        downloadQRCodeRequest.getMerchantCode())).thenReturn(profileResponse);
    bulkProcessServiceBean.insertQrExcelRequest(DEFAULT_STORE_ID, qrExcelUploadRequest, REQUEST_ID, DEFAULT_USERNAME);
    verify(fileStorageService).createBulkFile(eq(bulkUpdateProcessDTO), Mockito.anyString(),
        eq(qrExcelUploadRequest.getFileName()));
    verify(bulkProcessRepository).saveAndFlush(Mockito.any(BulkProcess.class));
    verify(kafkaProducer).send(eq(topicProperties.getGenerateQrCodeExcel()), Mockito.any(QRCodeExcelQueue.class));
    verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
        downloadQRCodeRequest.getMerchantCode());
    verify(topicProperties, times(2)).getGenerateQrCodeExcel();
  }

  @Test
  public void uploadQrExcelRequest_profileResponseNullTest() throws Exception {
    bulkUpdateProcessDTO.setBulkProcessType(QR_GENERATION.getValue());
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
        downloadQRCodeRequest.getMerchantCode())).thenReturn(null);
    bulkProcessServiceBean.insertQrExcelRequest(DEFAULT_STORE_ID, qrExcelUploadRequest, REQUEST_ID, DEFAULT_USERNAME);
    verify(fileStorageService).createBulkFile(eq(bulkUpdateProcessDTO), Mockito.anyString(),
        eq(qrExcelUploadRequest.getFileName()));
    verify(bulkProcessRepository).saveAndFlush(Mockito.any(BulkProcess.class));
    verify(kafkaProducer).send(eq(topicProperties.getGenerateQrCodeExcel()), Mockito.any(QRCodeExcelQueue.class));
    verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
        downloadQRCodeRequest.getMerchantCode());
    verify(topicProperties, times(2)).getGenerateQrCodeExcel();
  }

  @Test
  public void uploadQrExcelRequest_companyDtoNullTest() throws Exception {
    bulkUpdateProcessDTO.setBulkProcessType(QR_GENERATION.getValue());
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
        downloadQRCodeRequest.getMerchantCode())).thenReturn(new ProfileResponse());
    bulkProcessServiceBean.insertQrExcelRequest(DEFAULT_STORE_ID, qrExcelUploadRequest, REQUEST_ID, DEFAULT_USERNAME);
    verify(fileStorageService).createBulkFile(eq(bulkUpdateProcessDTO), Mockito.anyString(),
        eq(qrExcelUploadRequest.getFileName()));
    verify(bulkProcessRepository).saveAndFlush(Mockito.any(BulkProcess.class));
    verify(kafkaProducer).send(eq(topicProperties.getGenerateQrCodeExcel()), Mockito.any(QRCodeExcelQueue.class));
    verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
        downloadQRCodeRequest.getMerchantCode());
    verify(topicProperties, times(2)).getGenerateQrCodeExcel();
  }

  @Test
  public void insertQrExcelRequestTest(TestInfo testInfo) throws Exception {
    profileResponse.setCompany(new CompanyDTO());
    Mockito.when(this.bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, qrCodeExcelQueue.getBulkProcessCode())).thenReturn(qrCodeBulkProcess);
    XSSFSheet sheet = getSheetByInputPath(
        String.format(QR_UPLOAD_EXCEL_PATH, testInfo.getTestMethod().get().getName()));
    Mockito.when(fileStorageService.getFileDataByFileName(qrCodeExcelQueue.getFilename(), qrCodeBulkProcess))
        .thenReturn(sheet);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(qrCodeExcelQueue.getStoreId(),
        BUSINESS_PARTNER)).thenReturn(profileResponse);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT)).thenReturn(qrCodeSystemParameter);
    bulkProcessServiceBean.insertQrExcelRequest(qrCodeExcelQueue);
    Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, qrCodeExcelQueue.getBulkProcessCode());
    Mockito.verify(fileStorageService).getFileDataByFileName(qrCodeExcelQueue.getFilename(), qrCodeBulkProcess);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(qrCodeExcelQueue.getStoreId(),
        qrCodeExcelQueue.getDownloadQRCodeRequest().getMerchantCode());
    Mockito.verify(bulkProcessRepository, times(2)).save(Mockito.any(BulkProcess.class));
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT);
    Mockito.verify(this.objectMapper).writeValueAsString(Mockito.any(QrCodeRowInfo.class));
    Mockito.verify(this.bulkProcessDataService).saveBulkProcessData(bulkProcessDataListArgumentCaptor.capture());
    Assertions.assertEquals(1, bulkProcessDataListArgumentCaptor.getValue().size());
  }

  @Test
  public void insertQrExcelRequest_productTest(TestInfo testInfo) throws Exception {
    qrCodeExcelQueue.getDownloadQRCodeRequest().setQrGenerationType(Constant.PRODUCT);
    profileResponse.setCompany(new CompanyDTO());
    Mockito.when(this.bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, qrCodeExcelQueue.getBulkProcessCode())).thenReturn(qrCodeBulkProcess);
    XSSFSheet sheet = getSheetByInputPath(
        String.format(QR_UPLOAD_EXCEL_PATH, testInfo.getTestMethod().get().getName()));
    Mockito.when(fileStorageService.getFileDataByFileName(qrCodeExcelQueue.getFilename(), qrCodeBulkProcess))
        .thenReturn(sheet);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(qrCodeExcelQueue.getStoreId(),
        BUSINESS_PARTNER)).thenReturn(profileResponse);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT)).thenReturn(qrCodeSystemParameter);
    bulkProcessServiceBean.insertQrExcelRequest(qrCodeExcelQueue);
    Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, qrCodeExcelQueue.getBulkProcessCode());
    Mockito.verify(fileStorageService).getFileDataByFileName(qrCodeExcelQueue.getFilename(), qrCodeBulkProcess);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(qrCodeExcelQueue.getStoreId(),
        qrCodeExcelQueue.getDownloadQRCodeRequest().getMerchantCode());
    Mockito.verify(bulkProcessRepository, times(2)).save(Mockito.any(BulkProcess.class));
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT);
    Mockito.verify(this.objectMapper).writeValueAsString(Mockito.any(QrCodeRowInfo.class));
    Mockito.verify(this.bulkProcessDataService).saveBulkProcessData(bulkProcessDataListArgumentCaptor.capture());
    Assertions.assertEquals(1, bulkProcessDataListArgumentCaptor.getValue().size());
  }

  @Test
  public void insertQrExcelRequest_itemPickupPointTest(TestInfo testInfo) throws Exception {
    qrCodeExcelQueue.getDownloadQRCodeRequest().setQrGenerationType(Constant.ITEM_PICKUP_POINT);
    profileResponse.setCompany(new CompanyDTO());
    Mockito.when(this.bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, qrCodeExcelQueue.getBulkProcessCode())).thenReturn(qrCodeBulkProcess);
    XSSFSheet sheet = getSheetByInputPath(
        String.format(QR_UPLOAD_EXCEL_PATH, testInfo.getTestMethod().get().getName()));
    Mockito.when(fileStorageService.getFileDataByFileName(qrCodeExcelQueue.getFilename(), qrCodeBulkProcess))
        .thenReturn(sheet);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(qrCodeExcelQueue.getStoreId(),
        BUSINESS_PARTNER)).thenReturn(profileResponse);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT)).thenReturn(qrCodeSystemParameter);
    bulkProcessServiceBean.insertQrExcelRequest(qrCodeExcelQueue);
    Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, qrCodeExcelQueue.getBulkProcessCode());
    Mockito.verify(fileStorageService).getFileDataByFileName(qrCodeExcelQueue.getFilename(), qrCodeBulkProcess);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(qrCodeExcelQueue.getStoreId(),
        qrCodeExcelQueue.getDownloadQRCodeRequest().getMerchantCode());
    Mockito.verify(bulkProcessRepository, times(2)).save(Mockito.any(BulkProcess.class));
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT);
    Mockito.verify(this.objectMapper).writeValueAsString(Mockito.any(QrCodeRowInfo.class));
    Mockito.verify(this.bulkProcessDataService).saveBulkProcessData(bulkProcessDataListArgumentCaptor.capture());
    Assertions.assertEquals(1, bulkProcessDataListArgumentCaptor.getValue().size());
  }

  @Test
  public void insertQrExcelRequest_merchantCodeTest(TestInfo testInfo) throws Exception {
    profileResponse.setCompany(new CompanyDTO());
    qrCodeBulkProcess.setDescription(Constant.ITEM);
    Mockito.when(this.bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        qrCodeExcelQueue.getBulkProcessCode())).thenReturn(qrCodeBulkProcess);
    XSSFSheet sheet = getSheetByInputPath(
        String.format(QR_UPLOAD_EXCEL_PATH, testInfo.getTestMethod().get().getName()));
    Mockito.when(fileStorageService.getFileDataByFileName(qrCodeExcelQueue.getFilename(), qrCodeBulkProcess))
        .thenReturn(sheet);
    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(qrCodeExcelQueue.getStoreId(), BUSINESS_PARTNER))
        .thenReturn(profileResponse);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT)).thenReturn(qrCodeSystemParameter);
    bulkProcessServiceBean.insertQrExcelRequest(qrCodeExcelQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, qrCodeExcelQueue.getBulkProcessCode());
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT);
    Mockito.verify(fileStorageService).getFileDataByFileName(qrCodeExcelQueue.getFilename(), qrCodeBulkProcess);
    verify(bulkProcessRepository, times(2)).save(Mockito.any(BulkProcess.class));
    verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
        downloadQRCodeRequest.getMerchantCode());
    Mockito.verify(notificationService).sendGenerateQrCodeFailedNotification(qrCodeBulkProcess.getBusinessPartnerCode(),
        qrCodeBulkProcess.getDescription());
  }

  @Test
  public void insertQrExcelRequest_product_merchantCodeTest(TestInfo testInfo) throws Exception {
    qrCodeExcelQueue.getDownloadQRCodeRequest().setQrGenerationType(Constant.PRODUCT);
    qrCodeBulkProcess.setDescription(Constant.PRODUCT);
    profileResponse.setCompany(new CompanyDTO());
    Mockito.when(this.bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        qrCodeExcelQueue.getBulkProcessCode())).thenReturn(qrCodeBulkProcess);
    XSSFSheet sheet = getSheetByInputPath(
        String.format(QR_UPLOAD_EXCEL_PATH, testInfo.getTestMethod().get().getName()));
    Mockito.when(fileStorageService.getFileDataByFileName(qrCodeExcelQueue.getFilename(), qrCodeBulkProcess))
        .thenReturn(sheet);
    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(qrCodeExcelQueue.getStoreId(), BUSINESS_PARTNER))
        .thenReturn(profileResponse);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT)).thenReturn(qrCodeSystemParameter);
    bulkProcessServiceBean.insertQrExcelRequest(qrCodeExcelQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, qrCodeExcelQueue.getBulkProcessCode());
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT);
    Mockito.verify(fileStorageService).getFileDataByFileName(qrCodeExcelQueue.getFilename(), qrCodeBulkProcess);
    verify(bulkProcessRepository, times(2)).save(Mockito.any(BulkProcess.class));
    verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
        downloadQRCodeRequest.getMerchantCode());
    Mockito.verify(notificationService).sendGenerateQrCodeFailedNotification(qrCodeBulkProcess.getBusinessPartnerCode(),
        qrCodeBulkProcess.getDescription());
  }

  @Test
  public void insertQrExcelRequest_itemPickupPoint_merchantCodeTest(TestInfo testInfo) throws Exception {
    qrCodeExcelQueue.getDownloadQRCodeRequest().setQrGenerationType(Constant.ITEM_PICKUP_POINT);
    qrCodeBulkProcess.setDescription(Constant.ITEM_PICKUP_POINT);
    profileResponse.setCompany(new CompanyDTO());
    Mockito.when(this.bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        qrCodeExcelQueue.getBulkProcessCode())).thenReturn(qrCodeBulkProcess);
    XSSFSheet sheet = getSheetByInputPath(
        String.format(QR_UPLOAD_EXCEL_PATH, testInfo.getTestMethod().get().getName()));
    Mockito.when(fileStorageService.getFileDataByFileName(qrCodeExcelQueue.getFilename(), qrCodeBulkProcess))
        .thenReturn(sheet);
    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(qrCodeExcelQueue.getStoreId(), BUSINESS_PARTNER))
        .thenReturn(profileResponse);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT)).thenReturn(qrCodeSystemParameter);
    bulkProcessServiceBean.insertQrExcelRequest(qrCodeExcelQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, qrCodeExcelQueue.getBulkProcessCode());
    Mockito.verify(fileStorageService).getFileDataByFileName(qrCodeExcelQueue.getFilename(), qrCodeBulkProcess);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT);
    verify(bulkProcessRepository, times(2)).save(Mockito.any(BulkProcess.class));
    verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
        downloadQRCodeRequest.getMerchantCode());
    Mockito.verify(notificationService).sendGenerateQrCodeFailedNotification(qrCodeBulkProcess.getBusinessPartnerCode(),
        qrCodeBulkProcess.getDescription());
  }

  @Test
  public void insertQrExcelRequest_aboveLimitTest(TestInfo testInfo) throws Exception {
    profileResponse.setCompany(new CompanyDTO());
    qrCodeBulkProcess.setDescription(Constant.ITEM);
    Mockito.when(this.bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        qrCodeExcelQueue.getBulkProcessCode())).thenReturn(qrCodeBulkProcess);
    XSSFSheet sheet = getSheetByInputPath(
        String.format(QR_UPLOAD_EXCEL_PATH, testInfo.getTestMethod().get().getName()));
    Mockito.when(fileStorageService.getFileDataByFileName(qrCodeExcelQueue.getFilename(), qrCodeBulkProcess))
        .thenReturn(sheet);
    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(qrCodeExcelQueue.getStoreId(), BUSINESS_PARTNER))
        .thenReturn(profileResponse);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT)).thenReturn(qrCodeSystemParameter);
    bulkProcessServiceBean.insertQrExcelRequest(qrCodeExcelQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, qrCodeExcelQueue.getBulkProcessCode());
    Mockito.verify(fileStorageService).getFileDataByFileName(qrCodeExcelQueue.getFilename(), qrCodeBulkProcess);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(qrCodeExcelQueue.getStoreId(),
        qrCodeExcelQueue.getDownloadQRCodeRequest().getMerchantCode());
    Mockito.verify(bulkProcessRepository, times(2)).save(Mockito.any(BulkProcess.class));
    Mockito.verify(notificationService).sendGenerateQrCodeFailedNotification(qrCodeBulkProcess.getBusinessPartnerCode(),
        qrCodeBulkProcess.getDescription());
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_QR_UPLOAD_MAXIMUM_LIMIT);
  }

  @Test
  public void checkBulkProcessStatusQRCode_excelL5Upload_requestFalseTest() throws Exception {
    qrCodeRowItemInfo.setCncActivated(false);
    itemL5ListingResponse.setCncActivated(true);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "fetchItemPickupPointSize", 10);
    BulkProcess bulkProcess = getBulkProcess().get(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_QRCODE_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            QR_GENERATION_TYPE, BulkProcess.STATUS_PENDING,
            PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(bulkProcess));
    Mockito.when(this.bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndStatus(Constant.STORE_ID,
            bulkProcess.getBulkProcessCode(), BulkProcessData.STATUS_DATA_FETCH_PENDING, PageRequest.of(0, 10)))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkProcessData1)));
    Mockito.when(this.objectMapper.readValue(bulkProcessData1.getBulkRequestData(), QrCodeRowInfo.class))
        .thenReturn(qrCodeRowInfo);
    Mockito.when(
        this.xProductOutboundService.getItemL5DetailsByL5Ids(eq(Collections.singletonList(L5_ID)), eq(false), eq(0),
            eq(10))).thenReturn(new PageImpl<>(Collections.singletonList(itemL5ListingResponse)));
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, QR_GENERATION_TYPE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.FETCH_PENDING_QRCODE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, QR_GENERATION_TYPE,
            BulkProcess.STATUS_PENDING, PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    Mockito.verify(this.bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndStatus(Constant.STORE_ID,
        bulkProcess.getBulkProcessCode(), BulkProcessData.STATUS_DATA_FETCH_PENDING, PageRequest.of(0, 10));
    Mockito.verify(this.objectMapper, times(2)).readValue(bulkProcessData1.getBulkRequestData(), QrCodeRowInfo.class);
    Mockito.verify(this.xProductOutboundService)
        .getItemL5DetailsByL5Ids(eq(Collections.singletonList(L5_ID)), eq(false), eq(0), eq(10));
    Mockito.verify(this.objectMapper).writeValueAsString(qrCodeRowInfo);
    Mockito.verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
  }

  @Test
  public void checkBulkProcessStatusQRCode_excelL5Upload_requestedTrueTest() throws Exception {
    qrCodeRowItemInfo.setCncActivated(true);
    itemL5ListingResponse.setCncActivated(true);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "fetchItemPickupPointSize", 10);
    BulkProcess bulkProcess = getBulkProcess().get(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_QRCODE_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            QR_GENERATION_TYPE, BulkProcess.STATUS_PENDING,
            PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(bulkProcess));
    Mockito.when(this.bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndStatus(Constant.STORE_ID,
            bulkProcess.getBulkProcessCode(), BulkProcessData.STATUS_DATA_FETCH_PENDING, PageRequest.of(0, 10)))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkProcessData1)));
    Mockito.when(this.objectMapper.readValue(bulkProcessData1.getBulkRequestData(), QrCodeRowInfo.class))
        .thenReturn(qrCodeRowInfo);
    Mockito.when(
        this.xProductOutboundService.getItemL5DetailsByL5Ids(eq(Collections.singletonList(L5_ID)), eq(false), eq(0),
            eq(10))).thenReturn(new PageImpl<>(Collections.singletonList(itemL5ListingResponse)));
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, QR_GENERATION_TYPE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.FETCH_PENDING_QRCODE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, QR_GENERATION_TYPE,
            BulkProcess.STATUS_PENDING, PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    Mockito.verify(this.bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndStatus(Constant.STORE_ID,
        bulkProcess.getBulkProcessCode(), BulkProcessData.STATUS_DATA_FETCH_PENDING, PageRequest.of(0, 10));
    Mockito.verify(this.objectMapper, times(2)).readValue(bulkProcessData1.getBulkRequestData(), QrCodeRowInfo.class);
    Mockito.verify(this.xProductOutboundService)
        .getItemL5DetailsByL5Ids(eq(Collections.singletonList(L5_ID)), eq(false), eq(0), eq(10));
    Mockito.verify(this.objectMapper).writeValueAsString(qrCodeRowInfo);
    Mockito.verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
  }

  @Test
  public void checkBulkProcessStatusQRCode_excelL5Upload_requestedTrue_misMatchTest() throws Exception {
    qrCodeRowItemInfo.setCncActivated(true);
    itemL5ListingResponse.setCncActivated(false);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "fetchItemPickupPointSize", 10);
    BulkProcess bulkProcess = getBulkProcess().get(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_QRCODE_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            QR_GENERATION_TYPE, BulkProcess.STATUS_PENDING,
            PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(bulkProcess));
    Mockito.when(this.bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndStatus(Constant.STORE_ID,
            bulkProcess.getBulkProcessCode(), BulkProcessData.STATUS_DATA_FETCH_PENDING, PageRequest.of(0, 10)))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkProcessData1)));
    Mockito.when(this.objectMapper.readValue(bulkProcessData1.getBulkRequestData(), QrCodeRowInfo.class))
        .thenReturn(qrCodeRowInfo);
    Mockito.when(
        this.xProductOutboundService.getItemL5DetailsByL5Ids(eq(Collections.singletonList(L5_ID)), eq(false), eq(0),
            eq(10))).thenReturn(new PageImpl<>(Collections.singletonList(itemL5ListingResponse)));
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, QR_GENERATION_TYPE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.FETCH_PENDING_QRCODE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, QR_GENERATION_TYPE,
            BulkProcess.STATUS_PENDING, PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    Mockito.verify(this.bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndStatus(Constant.STORE_ID,
        bulkProcess.getBulkProcessCode(), BulkProcessData.STATUS_DATA_FETCH_PENDING, PageRequest.of(0, 10));
    Mockito.verify(this.objectMapper, times(2)).readValue(bulkProcessData1.getBulkRequestData(), QrCodeRowInfo.class);
    Mockito.verify(this.xProductOutboundService)
        .getItemL5DetailsByL5Ids(eq(Collections.singletonList(L5_ID)), eq(false), eq(0), eq(10));
    Mockito.verify(this.objectMapper).writeValueAsString(qrCodeRowInfo);
    Mockito.verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
  }

  @Test
  public void checkBulkProcessStatusQRCode_excelL5UploadXproductExceptionTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "fetchItemPickupPointSize", 10);
    BulkProcess bulkProcess = getBulkProcess().get(0);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.FETCH_PENDING_QRCODE_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.PUBLISHED_FETCH_BATCH_SIZE)).thenReturn(thresholdPendingBulkRequests);
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID,
            QR_GENERATION_TYPE, BulkProcess.STATUS_PENDING,
            PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue()))))
        .thenReturn(Collections.singletonList(bulkProcess));
    Mockito.when(this.bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndStatus(Constant.STORE_ID,
            bulkProcess.getBulkProcessCode(), BulkProcessData.STATUS_DATA_FETCH_PENDING, PageRequest.of(0, 10)))
        .thenReturn(new PageImpl<>(Collections.singletonList(bulkProcessData1)));
    Mockito.when(this.objectMapper.readValue(bulkProcessData1.getBulkRequestData(), QrCodeRowInfo.class))
        .thenReturn(qrCodeRowInfo);
    Mockito.when(
        this.xProductOutboundService.getItemL5DetailsByL5Ids(eq(Collections.singletonList(L5_ID)), eq(false), eq(0),
            eq(10))).thenReturn(new PageImpl<>(Collections.singletonList(itemL5ListingResponse)));
    Mockito.when(this.objectMapper.writeValueAsString(qrCodeRowInfo)).thenThrow(ApplicationRuntimeException.class);
    bulkProcessServiceBean.checkBulkProcessStatus(Constant.STORE_ID, QR_GENERATION_TYPE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.FETCH_PENDING_QRCODE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.PUBLISHED_FETCH_BATCH_SIZE);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(Constant.STORE_ID, QR_GENERATION_TYPE,
            BulkProcess.STATUS_PENDING, PageRequest.of(0, Integer.parseInt(thresholdPendingBulkRequests.getValue())));
    Mockito.verify(bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    Mockito.verify(this.bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndStatus(Constant.STORE_ID,
        bulkProcess.getBulkProcessCode(), BulkProcessData.STATUS_DATA_FETCH_PENDING, PageRequest.of(0, 10));
    Mockito.verify(this.objectMapper, times(2)).readValue(bulkProcessData1.getBulkRequestData(), QrCodeRowInfo.class);
    Mockito.verify(this.xProductOutboundService)
        .getItemL5DetailsByL5Ids(eq(Collections.singletonList(L5_ID)), eq(false), eq(0), eq(10));
    Mockito.verify(this.objectMapper).writeValueAsString(qrCodeRowInfo);
    Mockito.verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
  }

  private XSSFSheet getSheetByInputPath(String path) throws Exception {
    FileInputStream insertQrExcelStream = new FileInputStream(path);
    XSSFWorkbook workBook = new XSSFWorkbook(insertQrExcelStream);
    return workBook.getSheetAt(0);
  }

  @Test
  public void deleteBulkProcessDataByBulkProcessCodeTest() {
    Mockito.doNothing().when(bulkProcessDataService)
        .deleteDataByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.doNothing().when(bulkProcessImageService)
        .deleteImageByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);

    bulkProcessServiceBean.deleteBulkProcessDataByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);

    Mockito.verify(bulkProcessDataService).deleteDataByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).deleteImageByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
  }

  @Test
  public void findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalseTest() {
    Mockito.when(bulkProcessRepository
      .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, STATUS)).thenReturn(new BulkProcess());
    bulkProcessServiceBean.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
      DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE, STATUS);
    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, STATUS);
  }

  @Test
  public void testPublishBulkWorkOrderUploadEvent() {
    BulkProcess bulkProcess = new BulkProcess();
    SystemParameterConfig systemParameterConfig =
      new SystemParameterConfig(SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE, "2", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(systemParameterConfig);
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.TRANSFER_REQUEST.getValue());
    List<BulkProcessDataDTO> bulkProcessDataDTOList = Arrays.asList(
      new BulkProcessDataDTO("id1", 1, DEFAULT_BULK_PROCESS_CODE,
        DEFAULT_BULK_PROCESS_CODE.concat("-1"),
        ITEM_SKU),
      new BulkProcessDataDTO("id2", 2, DEFAULT_BULK_PROCESS_CODE,
        DEFAULT_BULK_PROCESS_CODE.concat("-2"),
        ITEM_SKU.concat("-2"))
    );


    when(bulkProcessDataService.getBulkProcessDataDTOByStoreIdAndBulkProcessCodeAndStatus(
      eq(DEFAULT_STORE_ID), eq(bulkProcess.getBulkProcessCode()), eq(BulkProcessData.STATUS_PENDING)))
      .thenReturn(bulkProcessDataDTOList);

    bulkProcessServiceBean.publishBulkWorkOrderUploadEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE);
    verify(kafkaProducer, times(2)).send(eq(topicProperties.getWorkOrderEvent()), eq("itemSku"),
        any(WorkOrderEventModel.class));
    verify(bulkProcessDataService).getBulkProcessDataDTOByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE, STATUS);
    verify(bulkProcessDataService).getBulkProcessDataDTOByStoreIdAndBulkProcessCodeAndStatus(DEFAULT_STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, STATUS);
    verify(topicProperties, times(4)).getBulkWorkOrderUploadEvent();
    verify(topicProperties).getWorkOrderEvent();
  }

  @Test
  public void processWorkOrderTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean,"wareHouseNameDelimeter","||");
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkUpdateQueue.setStoreId(DEFAULT_STORE_ID);
    bulkUpdateQueue.setFileName("disassembly_request_template.xlsx");
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBusinessPartnerCode("TOQ-70043");
    bulkProcess.setBulkProcessType(BulkProcessType.DISASSEMBLY_REQUEST.getValue());
    XSSFSheet sheet = getSheetByInputPath(WORKORDER_EXCEL_PATH);
    Mockito.when(fileStorageService.getFileDataByFileName(bulkUpdateQueue.getFileName(), bulkProcess))
        .thenReturn(sheet);
    Mockito.when(bulkProcessRepository.save(Mockito.any())).thenReturn(bulkProcess);
    bulkProcessServiceBean.processWorkOrder(bulkUpdateQueue, bulkProcess);
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    Mockito.verify(bulkProcessRepository).save(Mockito.any());
    Mockito.verify(objectMapper, times(3)).writeValueAsString(Mockito.any());

  }

  @Test
  public void testAbortStruckProcessesByProcessTypes() throws ParseException {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "supportedStatusForAbortJobMainTable",
      "PENDING,IN_PROGRESS");
    ReflectionTestUtils.setField(bulkProcessServiceBean, "supportedStatusForAbortJobDataTable",
      "PUBLISHED");
    Map<String, Integer> processTypeXAbortTimeMap = new HashMap<>();
    SimpleDateFormat simpleDateFormat = new SimpleDateFormat(START_DATE_FORMAT);
    Date startDate = simpleDateFormat.parse("2024-07-10T16:01:05.091");
    Date updatedDateCampaign = DateUtils.addMinutes(startDate, -10);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.FETCH_BATCH_SIZE_FOR_PUBLISHED_STRUCK_PROCESSES);
    systemParameterConfig.setValue(String.valueOf(2));
    Mockito.doNothing().when(bulkProcessRepository)
      .updateStatusInPendingOrInProgressByBulkProcessTypeAndTimeToAborted(
        BulkProcessType.CAMPAIGN.getValue(), updatedDateCampaign,
        List.of("PENDING", "IN_PROGRESS"));
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
          SystemParameterConfigNames.FETCH_BATCH_SIZE_FOR_PUBLISHED_STRUCK_PROCESSES))
      .thenReturn(systemParameterConfig);
    processTypeXAbortTimeMap.put(BulkProcessType.CAMPAIGN.getValue(), 10);
    Mockito.when(
      bulkProcessRepository.findBulkProcessCodeByStoreIdAndBulkProcessTypeAndStatusInAndUpdatedDateBeforeOrderByUpdatedDateAsc(
        eq(DEFAULT_STORE_ID), eq(BulkProcessType.CAMPAIGN.getValue()), eq(List.of("PUBLISHED")),
        any(Date.class), eq(PageRequest.of(0, 2)))).thenReturn(new PageImpl<>(List.of("BLP")));
    bulkProcessServiceBean.abortStruckProcessesByProcessTypes(DEFAULT_STORE_ID,
      processTypeXAbortTimeMap);
    verify(bulkProcessRepository).updateStatusInPendingOrInProgressByBulkProcessTypeAndTimeToAborted(
      eq(BulkProcessType.CAMPAIGN.getValue()), any(Date.class),
      eq(List.of("PENDING", "IN_PROGRESS")));
    verify(bulkProcessDataService).updateStatusToFailByBulkProcessCodeAndStatusIn("BLP", List.of(
      "PENDING", "IN_PROGRESS"));
    verify(
      bulkProcessRepository).findBulkProcessCodeByStoreIdAndBulkProcessTypeAndStatusInAndUpdatedDateBeforeOrderByUpdatedDateAsc(
      eq(DEFAULT_STORE_ID), eq(BulkProcessType.CAMPAIGN.getValue()), eq(List.of("PUBLISHED")),
      any(Date.class), eq(PageRequest.of(0, 2)));
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.FETCH_BATCH_SIZE_FOR_PUBLISHED_STRUCK_PROCESSES);
  }

  @Test
  public void testAbortStruckProcessesWithMultipleProcessByProcessTypes() throws ParseException {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "supportedStatusForAbortJobMainTable",
      "PENDING,IN_PROGRESS");
    ReflectionTestUtils.setField(bulkProcessServiceBean, "supportedStatusForAbortJobDataTable",
      "PUBLISHED");
    Map<String, Integer> processTypeXAbortTimeMap = new HashMap<>();
    SimpleDateFormat simpleDateFormat = new SimpleDateFormat(START_DATE_FORMAT);
    Date startDate = simpleDateFormat.parse("2024-07-10T16:01:05.091");
    Date updatedDateCampaign = DateUtils.addMinutes(startDate, -10);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.FETCH_BATCH_SIZE_FOR_PUBLISHED_STRUCK_PROCESSES);
    systemParameterConfig.setValue(String.valueOf(2));
    Mockito.doNothing().when(bulkProcessRepository)
      .updateStatusInPendingOrInProgressByBulkProcessTypeAndTimeToAborted(
        BulkProcessType.CAMPAIGN.getValue(), updatedDateCampaign,
        List.of("PENDING", "IN_PROGRESS"));
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
          SystemParameterConfigNames.FETCH_BATCH_SIZE_FOR_PUBLISHED_STRUCK_PROCESSES))
      .thenReturn(systemParameterConfig);
    processTypeXAbortTimeMap.put(BulkProcessType.CAMPAIGN.getValue(), 10);
    Mockito.when(
      bulkProcessRepository.findBulkProcessCodeByStoreIdAndBulkProcessTypeAndStatusInAndUpdatedDateBeforeOrderByUpdatedDateAsc(
        eq(DEFAULT_STORE_ID), eq(BulkProcessType.CAMPAIGN.getValue()), eq(List.of("PUBLISHED")),
        any(Date.class), eq(PageRequest.of(0, 2)))).thenReturn(new PageImpl<>(List.of("BLP",
      "BLP-2"))).thenReturn(new PageImpl<>(Collections.emptyList()));
    bulkProcessServiceBean.abortStruckProcessesByProcessTypes(DEFAULT_STORE_ID,
      processTypeXAbortTimeMap);
    verify(bulkProcessRepository).updateStatusInPendingOrInProgressByBulkProcessTypeAndTimeToAborted(
      eq(BulkProcessType.CAMPAIGN.getValue()), any(Date.class),
      eq(List.of("PENDING", "IN_PROGRESS")));
    verify(bulkProcessDataService).updateStatusToFailByBulkProcessCodeAndStatusIn("BLP", List.of(
      "PENDING", "IN_PROGRESS"));
    verify(bulkProcessDataService).updateStatusToFailByBulkProcessCodeAndStatusIn("BLP-2", List.of(
      "PENDING", "IN_PROGRESS"));
    verify(
      bulkProcessRepository).findBulkProcessCodeByStoreIdAndBulkProcessTypeAndStatusInAndUpdatedDateBeforeOrderByUpdatedDateAsc(
      eq(DEFAULT_STORE_ID), eq(BulkProcessType.CAMPAIGN.getValue()), eq(List.of("PUBLISHED")),
      any(Date.class), eq(PageRequest.of(0, 2)));
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.FETCH_BATCH_SIZE_FOR_PUBLISHED_STRUCK_PROCESSES);
  }

  @Test
  public void testAbortStruckProcessesByProcessTypesWithEmptyResponse() throws ParseException {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "supportedStatusForAbortJobMainTable",
      "PENDING,IN_PROGRESS");
    ReflectionTestUtils.setField(bulkProcessServiceBean, "supportedStatusForAbortJobDataTable",
      "PUBLISHED");
    Map<String, Integer> processTypeXAbortTimeMap = new HashMap<>();
    SimpleDateFormat simpleDateFormat = new SimpleDateFormat(START_DATE_FORMAT);
    Date startDate = simpleDateFormat.parse("2024-07-10T16:01:05.091");
    Date updatedDateCampaign = DateUtils.addMinutes(startDate, -10);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.FETCH_BATCH_SIZE_FOR_PUBLISHED_STRUCK_PROCESSES);
    systemParameterConfig.setValue(String.valueOf(2));
    Mockito.doNothing().when(bulkProcessRepository)
      .updateStatusInPendingOrInProgressByBulkProcessTypeAndTimeToAborted(
        BulkProcessType.CAMPAIGN.getValue(), updatedDateCampaign,
        List.of("PENDING", "IN_PROGRESS"));
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
          SystemParameterConfigNames.FETCH_BATCH_SIZE_FOR_PUBLISHED_STRUCK_PROCESSES))
      .thenReturn(systemParameterConfig);
    processTypeXAbortTimeMap.put(BulkProcessType.CAMPAIGN.getValue(), 10);
    Mockito.when(
      bulkProcessRepository.findBulkProcessCodeByStoreIdAndBulkProcessTypeAndStatusInAndUpdatedDateBeforeOrderByUpdatedDateAsc(
        eq(DEFAULT_STORE_ID), eq(BulkProcessType.CAMPAIGN.getValue()), eq(List.of("PUBLISHED")),
        any(Date.class), eq(PageRequest.of(0, 2)))).thenReturn(new PageImpl<>(Collections.emptyList()));
    bulkProcessServiceBean.abortStruckProcessesByProcessTypes(DEFAULT_STORE_ID,
      processTypeXAbortTimeMap);
    verify(bulkProcessRepository).updateStatusInPendingOrInProgressByBulkProcessTypeAndTimeToAborted(
      eq(BulkProcessType.CAMPAIGN.getValue()), any(Date.class),
      eq(List.of("PENDING", "IN_PROGRESS")));
    verify(
      bulkProcessRepository).findBulkProcessCodeByStoreIdAndBulkProcessTypeAndStatusInAndUpdatedDateBeforeOrderByUpdatedDateAsc(
      eq(DEFAULT_STORE_ID), eq(BulkProcessType.CAMPAIGN.getValue()), eq(List.of("PUBLISHED")),
      any(Date.class), eq(PageRequest.of(0, 2)));
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.FETCH_BATCH_SIZE_FOR_PUBLISHED_STRUCK_PROCESSES);
  }

  @Test
  public void testAbortStruckProcessesByProcessTypesWithIncorrectHasNext() throws ParseException {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "supportedStatusForAbortJobMainTable",
      "PENDING,IN_PROGRESS");
    ReflectionTestUtils.setField(bulkProcessServiceBean, "supportedStatusForAbortJobDataTable",
      "PUBLISHED");
    Map<String, Integer> processTypeXAbortTimeMap = new HashMap<>();
    SimpleDateFormat simpleDateFormat = new SimpleDateFormat(START_DATE_FORMAT);
    Date startDate = simpleDateFormat.parse("2024-07-10T16:01:05.091");
    Date updatedDateCampaign = DateUtils.addMinutes(startDate, -10);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.FETCH_BATCH_SIZE_FOR_PUBLISHED_STRUCK_PROCESSES);
    systemParameterConfig.setValue(String.valueOf(2));
    Mockito.doNothing().when(bulkProcessRepository)
      .updateStatusInPendingOrInProgressByBulkProcessTypeAndTimeToAborted(
        BulkProcessType.CAMPAIGN.getValue(), updatedDateCampaign,
        List.of("PENDING", "IN_PROGRESS"));
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
          SystemParameterConfigNames.FETCH_BATCH_SIZE_FOR_PUBLISHED_STRUCK_PROCESSES))
      .thenReturn(systemParameterConfig);
    processTypeXAbortTimeMap.put(BulkProcessType.CAMPAIGN.getValue(), 10);
    Page<String> firstPage = new PageImpl<>(List.of("BLP", "BLP-2"), PageRequest.of(0, 2), 4);
    Page<String> secondPage = new PageImpl<>(List.of("BLP-3", "BLP-4"), PageRequest.of(1, 2), 4);

    Mockito.when(
        bulkProcessRepository.findBulkProcessCodeByStoreIdAndBulkProcessTypeAndStatusInAndUpdatedDateBeforeOrderByUpdatedDateAsc(
          eq(DEFAULT_STORE_ID), eq(BulkProcessType.CAMPAIGN.getValue()), eq(List.of("PUBLISHED")),
          any(Date.class), any(Pageable.class)))
      .thenReturn(firstPage, secondPage);

    bulkProcessServiceBean.abortStruckProcessesByProcessTypes(DEFAULT_STORE_ID,
      processTypeXAbortTimeMap);

    verify(bulkProcessRepository).updateStatusInPendingOrInProgressByBulkProcessTypeAndTimeToAborted(
      eq(BulkProcessType.CAMPAIGN.getValue()), any(Date.class),
      eq(List.of("PENDING", "IN_PROGRESS")));

    verify(bulkProcessDataService).updateStatusToFailByBulkProcessCodeAndStatusIn("BLP", List.of(
      "PENDING", "IN_PROGRESS"));
    verify(bulkProcessDataService).updateStatusToFailByBulkProcessCodeAndStatusIn("BLP-2", List.of(
      "PENDING", "IN_PROGRESS"));
    verify(bulkProcessRepository, times(1))
      .findBulkProcessCodeByStoreIdAndBulkProcessTypeAndStatusInAndUpdatedDateBeforeOrderByUpdatedDateAsc(
        eq(DEFAULT_STORE_ID), eq(BulkProcessType.CAMPAIGN.getValue()), eq(List.of("PUBLISHED")),
        any(Date.class), any(Pageable.class));

    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.FETCH_BATCH_SIZE_FOR_PUBLISHED_STRUCK_PROCESSES);
  }




  @Test
  public void checkForPendingBulkProcessPendingExternalDownloadPendingTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "restrictDuplicateExternalBulkDownloadProcess",
        Set.of(BulkProcessEntity.PRODUCT.name()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "restrictDuplicateInternalBulkDownloadProcess",
        Set.of(BulkProcessEntity.VENDOR_FILTERED_PRODUCT.name()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "bulkPendingExternalDownloadStatus",
        Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
            BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "bulkPendingInternalDownloadStatus",
        Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
            BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));

    Mockito.when(bulkDownloadAuditRepository.findFirstByEntityTypeAndBusinessPartnerCodeAndStatusIn(
        BulkProcessEntity.PRODUCT.name(), BUSINESS_PARTNER,
        Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
            BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()))).thenReturn(new BulkDownloadEntity());

    BulkPendingRequestsResponse bulkPendingRequestsResponse =
        bulkProcessServiceBean.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            Constant.BULK_DOWNLOAD_TYPE, BUSINESS_PARTNER, null, BulkProcessEntity.PRODUCT.name());

    Mockito.verify(bulkDownloadAuditRepository)
        .findFirstByEntityTypeAndBusinessPartnerCodeAndStatusIn(BulkProcessEntity.PRODUCT.name(), BUSINESS_PARTNER,
            Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
                BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));

    Assertions.assertEquals(1L, bulkPendingRequestsResponse.getPendingRequestsCount());
  }

  @Test
  public void checkForPendingBulkProcessPendingExternalDownloadNotPendingTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "restrictDuplicateExternalBulkDownloadProcess",
        Set.of(BulkProcessEntity.PRODUCT.name()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "restrictDuplicateInternalBulkDownloadProcess",
        Set.of(BulkProcessEntity.VENDOR_FILTERED_PRODUCT.name()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "bulkPendingExternalDownloadStatus",
        Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
            BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "bulkPendingInternalDownloadStatus",
        Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
            BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));

    Mockito.when(bulkDownloadAuditRepository.findFirstByEntityTypeAndBusinessPartnerCodeAndStatusIn(
        BulkProcessEntity.PRODUCT.name(), BUSINESS_PARTNER,
        Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
            BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()))).thenReturn(null);

    BulkPendingRequestsResponse bulkPendingRequestsResponse =
        bulkProcessServiceBean.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            Constant.BULK_DOWNLOAD_TYPE, BUSINESS_PARTNER, null, BulkProcessEntity.PRODUCT.name());

    Mockito.verify(bulkDownloadAuditRepository)
        .findFirstByEntityTypeAndBusinessPartnerCodeAndStatusIn(BulkProcessEntity.PRODUCT.name(), BUSINESS_PARTNER,
            Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
                BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));

    Assertions.assertEquals(0L, bulkPendingRequestsResponse.getPendingRequestsCount());
  }

  @Test
  public void checkForPendingBulkProcessPendingInternalDownloadPendingTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "restrictDuplicateExternalBulkDownloadProcess",
        Set.of(BulkProcessEntity.PRODUCT.name()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "restrictDuplicateInternalBulkDownloadProcess",
        Set.of(BulkProcessEntity.VENDOR_FILTERED_PRODUCT.name()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "bulkPendingExternalDownloadStatus",
        Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
            BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "bulkPendingInternalDownloadStatus",
        Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
            BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));

    Mockito.when(bulkDownloadAuditRepository.findFirstByEntityTypeAndCreatedByAndStatusIn(
        BulkProcessEntity.VENDOR_FILTERED_PRODUCT.name(), DEFAULT_USERNAME,
        Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
            BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()))).thenReturn(new BulkDownloadEntity());

    BulkPendingRequestsResponse bulkPendingRequestsResponse =
        bulkProcessServiceBean.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            Constant.BULK_DOWNLOAD_TYPE, BUSINESS_PARTNER, null, BulkProcessEntity.VENDOR_FILTERED_PRODUCT.name());

    Mockito.verify(bulkDownloadAuditRepository)
        .findFirstByEntityTypeAndCreatedByAndStatusIn(BulkProcessEntity.VENDOR_FILTERED_PRODUCT.name(), DEFAULT_USERNAME,
            Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
                BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));

    Assertions.assertEquals(Constant.LONG_ONE, bulkPendingRequestsResponse.getPendingRequestsCount());
  }

  @Test
  public void checkForPendingBulkProcessPendingInternalDownloadNotPendingTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "restrictDuplicateExternalBulkDownloadProcess",
        Set.of(BulkProcessEntity.PRODUCT.name()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "restrictDuplicateInternalBulkDownloadProcess",
        Set.of(BulkProcessEntity.VENDOR_FILTERED_PRODUCT.name()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "bulkPendingExternalDownloadStatus",
        Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
            BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "bulkPendingInternalDownloadStatus",
        Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
            BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));

    Mockito.when(bulkDownloadAuditRepository.findFirstByEntityTypeAndCreatedByAndStatusIn(
        BulkProcessEntity.VENDOR_FILTERED_PRODUCT.name(), DEFAULT_USERNAME,
        Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
            BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()))).thenReturn(null);

    BulkPendingRequestsResponse bulkPendingRequestsResponse =
        bulkProcessServiceBean.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            Constant.BULK_DOWNLOAD_TYPE, BUSINESS_PARTNER, null, BulkProcessEntity.VENDOR_FILTERED_PRODUCT.name());

    Mockito.verify(bulkDownloadAuditRepository)
        .findFirstByEntityTypeAndCreatedByAndStatusIn(BulkProcessEntity.VENDOR_FILTERED_PRODUCT.name(), DEFAULT_USERNAME,
            Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
                BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));

    Assertions.assertEquals(0L, bulkPendingRequestsResponse.getPendingRequestsCount());
  }

  @Test
  public void checkForPendingBulkProcessPendingInternalDownloadNotConfiguredTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "restrictDuplicateExternalBulkDownloadProcess",
        Set.of(BulkProcessEntity.PRODUCT.name()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "restrictDuplicateInternalBulkDownloadProcess",
        Set.of(BulkProcessEntity.VENDOR_FILTERED_PRODUCT.name()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "bulkPendingExternalDownloadStatus",
        Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
            BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));
    ReflectionTestUtils.setField(bulkProcessServiceBean, "bulkPendingInternalDownloadStatus",
        Set.of(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
            BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));

    BulkPendingRequestsResponse bulkPendingRequestsResponse =
        bulkProcessServiceBean.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            Constant.BULK_DOWNLOAD_TYPE, BUSINESS_PARTNER, null, BulkProcessEntity.ORDER.name());

    Assertions.assertEquals(0L, bulkPendingRequestsResponse.getPendingRequestsCount());
  }

  @Test
  public void validateAndUpdateWorkOrderTest() {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "wareHouseNameDelimeter", "||");
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkUpdateQueue.setStoreId(DEFAULT_STORE_ID);
    bulkUpdateQueue.setFileName("disassembly_request_template.xlsx");
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBusinessPartnerCode("TOQ-70043");
    bulkProcess.setBulkProcessType(BulkProcessType.DISASSEMBLY_REQUEST.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE, BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    bulkProcessServiceBean.validateAndUpdateWorkOrder(bulkUpdateQueue);
    Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE, BulkProcess.STATUS_PENDING);
  }

  @Test
  public void validateAndUpdateWorkOrderStateNotValidTest() {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "wareHouseNameDelimeter", "||");
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkUpdateQueue.setStoreId(DEFAULT_STORE_ID);
    bulkUpdateQueue.setFileName("disassembly_request_template.xlsx");
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
                DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE, BulkProcess.STATUS_PENDING))
        .thenReturn(null);
    try {
      assertThrows(RuntimeException.class,
          () -> bulkProcessServiceBean.validateAndUpdateWorkOrder(bulkUpdateQueue));
    } finally {
      Mockito.verify(bulkProcessRepository)
          .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
              DEFAULT_BULK_PROCESS_CODE, BulkProcess.STATUS_PENDING);
    }
  }

  @Test
  public void regenerateTemplateByFileTypeTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedUploadTemplateFile",
        GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedUploadTemplateFileEnglish",
        GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedBaseTemplateFile",
        GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH);

    File file = new File(GENERIC_TEMPLATE_1_PATH);
    this.workbook = new XSSFWorkbook(file);
    Mockito.when(
            fileStorageService.getUnifiedBaseTemplate(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean()))
        .thenReturn(workbook);
    when(fileStorageService.isMassTemplateFileExist(GENERIC_TEMPLATE_1_PATH)).thenReturn(true);
    this.bulkProcessServiceBean.regenerateTemplateByFileType(Constant.STORE_ID,
        GenericTemplateFileType.PURE_DELIVERY_FILE, Constant.REQUEST_ID);
    Mockito.verify(this.pcbOutboundService, times(1)).getGenericTemplateCategories(GENERIC_TEMPLATE_ELIGIBLE, true);
    Mockito.verify(this.pcbOutboundService, times(8)).getCategoryDetailResponse(Mockito.anyString());
    Mockito.verify(this.pcbOutboundService, times(6)).getAttributeDetail(Mockito.anyString());
    Mockito.verify(this.kafkaEventLogService).getKafkaEventLogs();
    verify(fileStorageService, times(2)).getUnifiedBaseTemplate(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyBoolean());
    Mockito.verify(this.kafkaEventLogService).deleteKafkaEventLogsMarkForDelete(kafkaEventLogList);
    XSSFWorkbook workBook;
    if (new File(GENERIC_TEMPLATE_1_PATH).exists()) {
      try (InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
        workBook = new XSSFWorkbook(is);
        XSSFSheet sheet = workBook.getSheet(CATEGORY_ATTRIBUTE_SHEET);
        assertTrue(workBook.isSheetVeryHidden(GenericBulkParameters.CATEGORY_ATTRIBUTE_SHEET_INDEX));
        Assertions.assertEquals(CATEGORY_ATTRIBUTE_SHEET_INDEX, workBook.getSheetIndex(CATEGORY_ATTRIBUTE_SHEET));
        for (int header = 0; header < GenericBulkParameters.HEADER_COLUMN_NUMBER.size(); header++) {
          Assertions.assertEquals(GenericBulkParameters.HEADER_COLUMN_VALUE.get(header),
              sheet.getRow(GenericBulkParameters.HEADER_ROW)
                  .getCell(GenericBulkParameters.HEADER_COLUMN_NUMBER.get(header)).getStringCellValue());
        }
        for (int subHeader = 0; subHeader < GenericBulkParameters.SUB_HEADER_COLUMN_NUMBER.size(); subHeader++) {
          Assertions.assertEquals(GenericBulkParameters.SUB_HEADER_COLUMN_VALUE.get(subHeader),
              sheet.getRow(GenericBulkParameters.SUB_HEADER_ROW)
                  .getCell(GenericBulkParameters.SUB_HEADER_COLUMN_NUMBER.get(subHeader)).getStringCellValue());
        }
        for (int familyColour = 0; familyColour < GenericBulkParameters.FAMILY_COLOUR_VALUE.size(); familyColour++) {
          Assertions.assertEquals(GenericBulkParameters.FAMILY_COLOUR_VALUE.get(familyColour),
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + familyColour)
                  .getCell(GenericBulkParameters.FAMILY_COLOUR_DATA_COLUMN).getStringCellValue());
        }
        for (int productType = 0; productType < BulkParameters.BULK_OPTION_UPLOAD_SUPPORT.size(); productType++) {
          Assertions.assertEquals(BulkParameters.BULK_OPTION_UPLOAD_SUPPORT.get(productType),
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + productType)
                  .getCell(GenericBulkParameters.HANDLING_TYPE_HEADER_COLUMN).getStringCellValue());
        }
        for (int warnaFamilyColourMapping = 0; warnaFamilyColourMapping
            < GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING.size(); warnaFamilyColourMapping++) {
          Assertions.assertEquals(
              GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING.get(warnaFamilyColourMapping).split(Constant.COMMA)[0],
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + warnaFamilyColourMapping)
                  .getCell(GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING_HEADER_COLUMN).getStringCellValue());
          Assertions.assertEquals(
              GenericBulkParameters.WARNA_FAMILY_COLOUR_MAPPING.get(warnaFamilyColourMapping).split(Constant.COMMA)[1],
              sheet.getRow(GenericBulkParameters.POSSIBLE_VALUE_START_COLUMN + warnaFamilyColourMapping)
                  .getCell(GenericBulkParameters.FAMILY_COLOUR_MAPPING_HEADER_COLUMN).getStringCellValue());
        }
      }
    }
  }
  @Test
  public void regenerateTemplateByFileTypeRegenrationNotRequiredTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedUploadTemplateFile",
        GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedUploadTemplateFileEnglish",
        GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH);
    ReflectionTestUtils.setField(bulkProcessServiceBean, "unifiedBaseTemplateFile",
        GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + ","
            + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH + "," + GENERIC_TEMPLATE_1_PATH);
    Mockito.when(this.kafkaEventLogService.getKafkaEventLogs()).thenReturn(null);
    File file = new File(GENERIC_TEMPLATE_1_PATH);
    this.workbook = new XSSFWorkbook(file);
    Mockito.when(
            fileStorageService.getUnifiedBaseTemplate(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean()))
        .thenReturn(workbook);
    when(fileStorageService.isMassTemplateFileExist(GENERIC_TEMPLATE_1_PATH)).thenThrow(
        ApplicationRuntimeException.class);
    this.bulkProcessServiceBean.regenerateTemplateByFileType(Constant.STORE_ID,
        GenericTemplateFileType.PURE_DELIVERY_FILE, Constant.REQUEST_ID);
    Mockito.verify(this.kafkaEventLogService).getKafkaEventLogs();
  }

  @Test
  public void testGetBasicInfoImageDownloadEventNameByPriority_Priority1() {
    when(topicProperties.getBulkBasicInfoDownloadImagePriority1()).thenReturn("image-priority1-topic");

    String result = bulkProcessServiceBean.getBasicInfoImageDownloadEventNameByPriority(
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue());
    Assertions.assertEquals("image-priority1-topic", result);
    verify(topicProperties).getBulkBasicInfoDownloadImagePriority1();
  }

  @Test
  public void testGetBasicInfoImageDownloadEventNameByPriority_Priority2() {
    when(topicProperties.getBulkBasicInfoDownloadImagePriority2()).thenReturn("image-priority2-topic");
    String result = bulkProcessServiceBean.getBasicInfoImageDownloadEventNameByPriority(
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue());
    Assertions.assertEquals("image-priority2-topic", result);
    verify(topicProperties).getBulkBasicInfoDownloadImagePriority2();
  }

  @Test
  public void testGetBasicInfoImageDownloadEventNameByPriority_Default() {
    when(topicProperties.getBulkBasicInfoDownloadImageEvent()).thenReturn("image-default-topic");
    String result = bulkProcessServiceBean.getBasicInfoImageDownloadEventNameByPriority("OTHER_TYPE");
    Assertions.assertEquals("image-default-topic", result);
    verify(topicProperties).getBulkBasicInfoDownloadImageEvent();
  }

  @Test
  public void testGetBasicInfoVideoDownloadEventNameByPriority_Priority1() {
    when(topicProperties.getBulkBasicInfoDownloadVideoPriority1()).thenReturn("video-priority1-topic");
    String result = bulkProcessServiceBean.getBasicInfoVideoDownloadEventNameByPriority(
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue());
    Assertions.assertEquals("video-priority1-topic", result);
    verify(topicProperties).getBulkBasicInfoDownloadVideoPriority1();
  }

  @Test
  public void testGetBasicInfoVideoDownloadEventNameByPriority_Priority2() {
    when(topicProperties.getBulkBasicInfoDownloadVideoPriority2()).thenReturn("video-priority2-topic");
    String result = bulkProcessServiceBean.getBasicInfoVideoDownloadEventNameByPriority(
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue());
    Assertions.assertEquals("video-priority2-topic", result);
    verify(topicProperties).getBulkBasicInfoDownloadVideoPriority2();
  }

  @Test
  public void testGetBasicInfoVideoDownloadEventNameByPriority_Default() {
    when(topicProperties.getBulkBasicInfoDownloadVideoEvent()).thenReturn("video-default-topic");
    String result = bulkProcessServiceBean.getBasicInfoVideoDownloadEventNameByPriority("OTHER_TYPE");
    Assertions.assertEquals("video-default-topic", result);
    verify(topicProperties).getBulkBasicInfoDownloadVideoEvent();
  }

  @Test
  public void publishBulkExternalImageQCDownloadEventModelTest() throws Exception {
    // Arrange
    BulkProcess bulkProcess = getBulkProcess_1(); // reuse your helper method
    Map<String, BrandAndCategoryPredictionRequest> productIdToImageQCModelMap = new HashMap<>();
    BrandAndCategoryPredictionRequest request1 = new BrandAndCategoryPredictionRequest();
    BrandAndCategoryPredictionRequest request2 = new BrandAndCategoryPredictionRequest();
    productIdToImageQCModelMap.put("prod1", request1);
    productIdToImageQCModelMap.put("prod2", request2);
    when(topicProperties.getExternalUploadCategoryAndBrandPrediction()).thenReturn("bulkTopic");
    // Act
    bulkProcessServiceBean.publishBulkExternalImageQCDownloadEventModel(bulkProcess,
      productIdToImageQCModelMap);
    // Assert: Verify that Kafka producer was called for each productId
    verify(kafkaProducer).send("bulkTopic", "prod1", request1);
    verify(kafkaProducer).send("bulkTopic", "prod2", request2);
    verify(topicProperties, times(3)).getExternalUploadCategoryAndBrandPrediction();
  }

  @Test
  public void publishBulkExternalImageDownloadEventModelTest() throws Exception {
    // Arrange
    String bulkProcessCode = "BULK123";
    String bulkProcessType = "TYPE_A"; // currently unused in the method
    List<String> imageList = Arrays.asList("img1.jpg", "img2.jpg");

    // Stub the topicProperties to return a test topic
    when(topicProperties.getBulkExternalCreateDownloadImageEvent()).thenReturn("downloadTopic");

    // Act
    bulkProcessServiceBean.publishBulkExternalImageDownloadEventModel(
      bulkProcessCode, bulkProcessType, imageList
    );

    // Assert: Verify that Kafka producer was called with the correct topic and event model
    verify(kafkaProducer).send(
      eq("downloadTopic"),
      argThat(argument -> {
        if (!(argument instanceof BulkImageDownloadEventModel)) return false;
        BulkImageDownloadEventModel eventModel = (BulkImageDownloadEventModel) argument;
        return eventModel.getBulkProcessCode().equals(bulkProcessCode)
          && eventModel.getImageDownloadList().equals(imageList);
      })
    );

    // Optional: Verify topicProperties interaction
    verify(topicProperties).getBulkExternalCreateDownloadImageEvent();
    verifyNoMoreInteractions(topicProperties);
  }

  @Test
  void publishBulkUpdateEANEventTest() throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(UUID.randomUUID().toString());
    bulkProcess.setBulkProcessType("EAN");
    bulkProcess.setBusinessPartnerCode("BB");
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE, "1", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessDataService.getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(anyString(),
        anyString(), anyString())).thenReturn(Collections.emptyList());
    Mockito.when(topicProperties.getBulkUploadUpdateEANItem()).thenReturn("bulk-upload-update-ean-item");
    bulkProcessServiceBean.publishBulkUpdateEANEvent(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.PROCESS_DATA_UPDATE_BATCH_SIZE);
    Mockito.verify(bulkProcessDataService)
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(Constant.STORE_ID, bulkProcess.getBulkProcessCode(),
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(topicProperties).getBulkUploadUpdateEANItem();
  }

}
