package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.FAILED;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.HYPHEN;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_EVENT;
import static com.gdn.mta.bulk.service.ProductLevel3BulkUpdateServiceBean.INVALID_HEADER_ARCHIVAL_ERROR_MESSAGE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.partners.bulk.util.BulkWorkOrderConstants;
import com.gdn.x.businesspartner.constant.ProfileFlagNames;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.util.IOUtils;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductRequest;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.VariantsErrorListResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkAddCampaignProductDTO;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateErrorDTO;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.BulkUpdateSuccessDTO;
import com.gdn.mta.bulk.dto.VatUpdateDto;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkAddCampaignProductQueue;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.factory.BulkProcessHelperFactory;
import com.gdn.mta.bulk.feignConfig.XCampaignFeign;
import com.gdn.mta.bulk.helper.BulkCampaignProductProcessHelper;
import com.gdn.mta.bulk.helper.BulkProductProcessHelper;
import com.gdn.mta.bulk.helper.DeleteUpdatePickupPointsProcessHelper;
import com.gdn.mta.bulk.models.AuditTrailInfo;
import com.gdn.mta.bulk.models.BulkErrorCategory;

import com.gdn.mta.bulk.models.BulkUpdateErrorCounter;
import com.gdn.mta.bulk.models.DeletePickupPointResponseEventModel;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.repository.AuditTrailUpdateProductRepository;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.mta.bulk.repository.campaign.CampaignRepository;
import com.gdn.mta.bulk.repository.pcb.ProductCategoryBaseRepository;
import com.gdn.mta.bulk.service.download.BulkFailedProductFileService;
import com.gdn.mta.bulk.service.download.BulkProcessFileGeneration;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.notification.queue.NotificationQueue;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.campaign.dto.FailedItemInfoDto;
import com.gdn.x.campaign.dto.ItemInfoDto;
import com.gdn.x.campaign.dto.ProductCampaignAvailabilityInfoDto;
import com.gdn.x.campaign.request.CampaignProductRequest;
import com.gdn.x.campaign.request.ProductCampaignAvailabilityRequest;
import com.gdn.x.campaign.response.CampaignUpdateDiscountResponse;
import com.gdn.x.campaign.response.FailedProductsResponse;
import com.gdn.x.campaign.response.ProductCampaignAvailabilityResponse;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.ItemCodeDetailResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.BatchVatUpdateResponse;
import com.google.cloud.storage.Blob;
import com.google.common.collect.ImmutableMap;

public class ProductLevel3BulkUpdateServiceBeanTest {

  private static final String DEFAULT_BULK_PROCESS_CODE = UUID.randomUUID().toString();
  private static final String DEFAULT_REQUEST_ID = "123";
  private static final String OFF_2_ON_EMPTY_DATA_SHEET = "BulkUpdateOff2OnEmptyDataSheet.xlsx";
  private static final String DEFAULT_BP_CODE = "TOA-14961";
  private static final String DEFAULT_BP_CODE_1 = "TOA-14962";
  private static final String GDN_SKU_1 = "TOA-14961-00117-00001";
  private static final String DEFAULT_GDN_SKU = "TOA-14961-00117-00006";
  private static final String DEFAULT_GDN_SKU2 = "TOA-14961-00118-00001";
  private static final String DEFAULT_GDN_SKU_3 = "TOA-14961-00118-00002";
  private static final String DEFAULT_PRODUCT_SKU = "TOA-14961-00117";
  private static final String DEFAULT_PRODUCT_SKU_2 = "TOA-14961-00118";
  private static final String DEFAULT_PRODUCT_SKU_3 = "TOA-14961-00118";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "username";
  private static final String PARTIALLY_SUCCESS_MESSAGE_EN =
      "test.xlsx. Product successfully archived: 3. Failed archived: 2. ";
  private static final String PARTIALLY_SUCCESS_MESSAGE =
      "test.xlsx. Produk berhasil diarsipkan: 3. Gagal diarsipkan: 2. ";
  private static final String X_BULK_CLIENT = "x-bulk";
  private static final String PRODUCT_LEVEL3_PROCESSOR_DIR = "ProductLevel3Processor";
  private static final String BULK_DATA = "BULK_DATA";
  public static final String STATUS_SUCCESS = "SUCCESS";
  private static final String BULK_DATA_1 = "BULK_DATA_1";
  private static final String BULK_DATA_2 = "BULK_DATA_2";
  private static final String EVENT = "event1";
  private static final String PRIVILEGED_MAP_DATA = "{\"isPrivilegedToEditAvailableStock\":true, "
      + "\"isPrivilegedToEditPrice\":true, \"isPrivilegedToEditProductType\":true,\"isPrivilegedToEditDisplayBuyable\":"
      + "true,\"isPrivilegedToEditPickupPoint\":true,\"isPrivilegedToEditO2O\":true}";
  private static final Map<String, Boolean> PRIVILEGED_MAP =
      ImmutableMap.<String, Boolean>builder().put("isPrivilegedToEditAvailableStock", true)
          .put("isPrivilegedToEditPrice", true).put("isPrivilegedToEditProductType", true)
          .put("isPrivilegedToEditDisplayBuyable", true).put("isPrivilegedToEditPickupPoint", true)
          .put("isPrivilegedToEditO2O", true).put("isOnlyExternalUser", false).build();

  private static final String PRIVILEGED_MAP_DATA_1 = "{\"isPrivilegedToEditAvailableStock\":true, "
      + "\"isPrivilegedToEditPrice\":false, \"isPrivilegedToEditProductType\":true,\"isPrivilegedToEditDisplayBuyable\":"
      + "true,\"isPrivilegedToEditPickupPoint\":true,\"isPrivilegedToEditO2O\":true}";
  private static final Map<String, Boolean> PRIVILEGED_MAP_1 =
      ImmutableMap.<String, Boolean>builder().put("isPrivilegedToEditAvailableStock", true)
          .put("isPrivilegedToEditPrice", false).put("isPrivilegedToEditProductType", true)
          .put("isPrivilegedToEditDisplayBuyable", true).put("isPrivilegedToEditPickupPoint", true)
          .put("isPrivilegedToEditO2O", true).put("isOnlyExternalUser", false).build();

  private static final String PRIVILEGED_MAP_DATA_2 = "{\"isPrivilegedToEditAvailableStock\":false, "
      + "\"isPrivilegedToEditPrice\":false, \"isPrivilegedToEditProductType\":true,\"isPrivilegedToEditDisplayBuyable\":"
      + "true,\"isPrivilegedToEditPickupPoint\":true,\"isPrivilegedToEditO2O\":true}";
  private static final Map<String, Boolean> PRIVILEGED_MAP_2 =
      ImmutableMap.<String, Boolean>builder().put("isPrivilegedToEditAvailableStock", false)
          .put("isPrivilegedToEditPrice", false).put("isPrivilegedToEditProductType", true)
          .put("isPrivilegedToEditDisplayBuyable", true).put("isPrivilegedToEditPickupPoint", true)
          .put("isPrivilegedToEditO2O", true).put("isOnlyExternalUser", false).build();

  private static final String PRIVILEGED_MAP_DATA_3 = "{\"isPrivilegedToEditAvailableStock\":false, "
      + "\"isPrivilegedToEditPrice\":false, \"isPrivilegedToEditProductType\":true,\"isPrivilegedToEditDisplayBuyable\":"
      + "true,\"isPrivilegedToEditPickupPoint\":false,\"isPrivilegedToEditO2O\":true}";
  private static final Map<String, Boolean> PRIVILEGED_MAP_3 =
      ImmutableMap.<String, Boolean>builder().put("isPrivilegedToEditAvailableStock", false)
          .put("isPrivilegedToEditPrice", false).put("isPrivilegedToEditProductType", true)
          .put("isPrivilegedToEditDisplayBuyable", true).put("isPrivilegedToEditPickupPoint", false)
          .put("isPrivilegedToEditO2O", true).put("isOnlyExternalUser", false).build();

  private static final String PRIVILEGED_MAP_DATA_4 = "{\"isPrivilegedToEditAvailableStock\":false, "
      + "\"isPrivilegedToEditPrice\":false, \"isPrivilegedToEditProductType\":true,\"isPrivilegedToEditDisplayBuyable\":"
      + "true,\"isPrivilegedToEditPickupPoint\":true,\"isOnlyExternalUser\":true,\"isPrivilegedToEditO2O\":true}";
  private static final Map<String, Boolean> PRIVILEGED_MAP_4 =
      ImmutableMap.<String, Boolean>builder().put("isPrivilegedToEditAvailableStock", false)
          .put("isPrivilegedToEditPrice", false).put("isPrivilegedToEditProductType", true)
          .put("isPrivilegedToEditDisplayBuyable", true).put("isPrivilegedToEditPickupPoint", true)
          .put("isPrivilegedToEditO2O", true).put("isOnlyExternalUser", true).build();

  private static final Map<String, Boolean> PRIVILEGED_MAP_5 =
      ImmutableMap.<String, Boolean>builder().put("isPrivilegedToEditAvailableStock", false)
          .put("isPrivilegedToEditPrice", false).put("isPrivilegedToEditProductType", true)
          .put("isPrivilegedToEditDisplayBuyable", true).put("isPrivilegedToEditPickupPoint", true)
          .put("isPrivilegedToEditO2O", true).put("isOnlyExternalUser", false).build();

  private static final String CATEGORY_CODE = "categoryCode";
  private static final String PICKUP_POINT_CODE ="PP-3000213";
  private static final String PICKUP_POINT_CODE_1 ="PP-3000214";
  private static final String PICKUP_POINT_CODE_2 ="PP-3000333";
  private static final String CATEGORY_CODE1 = "categoryCode1";
  private static final String ABORTED = "ABORTED";
  private static final String BUSINESS_PARTNER_CODE = "TOQ-16110";
  private static final String PRODUCT_SKU = "TOQ-16110-00001";
  private static final String PRODUCT_SKU_2 = "TOQ-16110-00002";
  private static final String USER_NAME_2 = "developer";
  private static final String BULK_OFF2ON_UPDATE_FILE = "BulkUpdateOff2On.xlsx";
  private static final String BULK_OFF2ON_UPDATE_MULTIPLE_PRODUCTS = "BulkUpdateOff2OnMultipleProducts.xlsx";
  private static final String BULK_OFF2ON_UPDATE_FILE_INPUT_ERROR = "BulkUpdateOff2OnInputError.xlsx";
  private static final String BULK_OFF2ON_UPDATE_FILE_INPUT_ERROR_PARTIAL_SUCCESS =
      "BulkUpdateOff2OnInputErrorPartialSuccess.xlsx";
  private static final String UPDATE_FILE_EMPTY = "BulkUpdateOff2OnEmptyFile.xlsx";
  private static final String BULK_OFF2ON_HEADER_WRONG = "BulkUpdateOff2OnWrongHeader.xlsx";
  private static final String BULK_ARCHIVE = "BulkArchive.xlsx";
  private static final String SUCCESS_UPDATE_NOTIFICATION =
      "test.xlsx. Produk berhasil diperbarui: 3. Gagal diperbarui: 0. ";
  private static final String SUCCESS_UPDATE_NOTIFICATION_2 =
      "test.xlsx. Produk berhasil diarsipkan: 2. Gagal diarsipkan: 0. ";
  private static final String SUCCESS_UPDATE_NOTIFICATION_3 =
      "test.xlsx. Produk berhasil diarsipkan: 2. Gagal diarsipkan: 2. ";
  private static final String SUCCESS_UPDATE_NOTIFICATION_4 =
      "test.xlsx. Produk berhasil diarsipkan: 1. Gagal diarsipkan: 3. ";
  private static final String SUCCESS_UPDATE_NOTIFICATION_5 =
      "test.xlsx. Product successfully archived: 1. Failed archived: 3. ";
  private static final String ABORTED_NOTIFICATION =
      "test.xlsx. Produk berhasil diarsipkan: 0. Gagal diarsipkan: 4. Silakan cek produk Anda dan coba lagi.";
  private static final String BULK_ARCHIVED_ABORTED =
      "test.xlsx. Produk berhasil diarsipkan: 0. Gagal diarsipkan: 0. Silakan cek produk Anda dan coba lagi.";
  private static final String SUCCESS_UPDATE_NOTIFICATION_EN =
      "test.xlsx. Product successfully updated: 3. Failed update: 0. ";
  private static final String SUCCESS_UPDATE_NOTIFICATION_EN_2 =
      "test.xlsx. Product successfully updated: 2. Failed update: 0. ";
  private static final String ABORTED_ERROR_MESSAGE =
      "test.xlsx. Product successfully updated: 0. Failed update: 0. Please check your product and try again.";
  private static final String ABORTED_ERROR_MESSAGE_IN =
      "test.xlsx. Produk berhasil diperbarui: 0. Gagal diperbarui: 0. Silakan cek produk Anda dan coba lagi.";
  private static final String PARTIAL_ERROR_MESSAGE = "test.xlsx. Product successfully updated: 1. Failed update: 1. ";
  private static final String ABORTED_ERROR_MESSAGE_1 =
      "test.xlsx. Product successfully updated: 0. Failed update: 4. Please check your product and try again.";
  private static final String PARTIAL_ERROR_MESSAGE_2 = "test.xlsx. Product successfully updated: 1. Failed update: 5. ";
  private static final String BULK_VAT_DIRECTORY_1 = "BulkVat/test1.xlsx";
  private static final String BULK_VAT_DIRECTORY_2 = "BulkVat/test5.xlsx";
  private static final String BLIBLI_SKU = "Blibli-sku";
  public static final String TD_MERCHANT = "TD";
  private Map<String, Boolean> privilegedMap = new HashMap<>();
  private static String MAX_PRODUCT_SIZE = "4";
  private static final String REQUEST_ID = "request-id";
  private static final Integer SUCCESS_COUNT = 0;
  private static final Integer ERROR_COUNT = 0;
  private static final String NOTES = "notes";

  @InjectMocks
  private ProductLevel3BulkUpdateServiceBean bulkUpdateServiceBean;

  @Mock
  private AuditTrailUpdateProductRepository auditTrailUpdateProductRepository;

  @Mock
  private BulkProcessRepository bulkProcessRepository;

  @Mock
  private BulkProcessFileGeneration bulkProcessFileGeneration;

  @Mock
  private BulkProcessHelperFactory bulkProcessHelperFactory;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private FileStorageServiceBean fileStorageServiceBean;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @Mock
  private GCSService gcsService;

  @Mock
  private Blob blob;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private NotificationService notificationService;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private TrackerService trackerService;

  @Mock
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Captor
  private ArgumentCaptor<BulkProcess> bulkProcessArgumentCaptor;

  @Captor
  private ArgumentCaptor<AuditTrailInfo> auditTrailInfoArgumentCaptor;

  @Captor
  private ArgumentCaptor<NotificationQueue> notificationQueueArgumentCaptor;

  @Captor
  private ArgumentCaptor<List> listArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<CampaignProductRequest>> campaignProductRequestListCaptor;

  @Captor
  private ArgumentCaptor<ProductVariantUpdateRequest> productVariantUpdateRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ItemPickupPointListingL3Request> itemPickupPointListingL3RequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<Map<String, Boolean>> stringBooleanMapCaptor;

  @Captor
  private ArgumentCaptor<SimpleListStringRequest> simpleListStringRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<DeletePickupPointResponseEventModel> deletePickupPointResponseEventModelArgumentCaptor;

  @Mock
  private XCampaignFeign xCampaignFeign;

  @Mock
  private CampaignRepository campaignRepository;

  @Mock
  private ProductCategoryBaseRepository productCategoryBaseRepository;

  @Mock
  private BulkDownloadService bulkDownloadService;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private PCBOutboundService pcbOutboundService;

  @Mock
  private PBPOutboundService pbpOutboundService;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private BulkFailedProductFileService bulkFailedProductFileService;

  @Captor
  private ArgumentCaptor<String> stringArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<List<String>>> listStringArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<BulkProcessData>> bulkProcessDataArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductLevel3UpdateSummaryRequest> productLevel3UpdateSummaryRequestArgumentCaptor;

  @Mock
  private BulkArchiveService bulkArchiveService;

  @Mock
  private BulkCampaignProductProcessHelper bulkCampaignProductProcessHelper;

  @Mock
  private BulkProductProcessHelper bulkProductProcessHelper;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private PickupPointService pickupPointService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  private BulkUpdateProcessDTO bulkUpdateProcessDTO;
  private BulkAddCampaignProductDTO bulkAddCampaignProductDTO;
  private BulkUpdateQueue bulkUpdateQueue;
  private BulkAddCampaignProductQueue bulkAddCampaignProductQueue;
  private BulkProcess bulkProcess;
  private ProductCampaignAvailabilityRequest productCampaignAvailabilityRequest =
      new ProductCampaignAvailabilityRequest();
  private ProductCampaignAvailabilityRequest productCampaignAvailabilityRequest2 =
      new ProductCampaignAvailabilityRequest();
  private ProductCampaignAvailabilityResponse productCampaignAvailabilityResponse =
      new ProductCampaignAvailabilityResponse();
  private SystemParameterConfig systemParameterConfig;
  private SystemParameterConfig bulkArchiveSwitch = new SystemParameterConfig();
  private CampaignUpdateDiscountResponse campaignUpdateDiscountResponse = new CampaignUpdateDiscountResponse();
  private BulkProcessData bulkProcessData = new BulkProcessData();
  private BulkProcessData bulkProcessData1 = new BulkProcessData();
  private BulkProcessData bulkProcessData2 = new BulkProcessData();
  private List<Map<String, String>> productDataMap = new ArrayList<>();
  private SystemParameterConfig bulkArchiveSystemParameter = new SystemParameterConfig();
  private AuditTrailInfo auditTrailInfo = new AuditTrailInfo();
  private BulkUpdateErrorCounter counter;


  private GdnRestSingleResponse<ProductLevel3SummaryResponse> getSucessResponse() {
    GdnRestSingleResponse<ProductLevel3SummaryResponse> response =
        new GdnRestSingleResponse<ProductLevel3SummaryResponse>(null, null, true, getProductLevel3SummaryResponse(),
            DEFAULT_REQUEST_ID);
    return response;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkUpdateProcessDTO = new BulkUpdateProcessDTO();
    bulkUpdateProcessDTO.setBulkProcessType("Update");
    bulkUpdateProcessDTO.setBusinessPartnerCode("TOA-14961");
    bulkUpdateProcessDTO.setFileName("test.xlsx");
    bulkUpdateProcessDTO.setFileContent("Hello".getBytes("UTF-8"));
    bulkUpdateProcessDTO.setUpdatedBy("developer");
    bulkUpdateProcessDTO.setClientHost("localhost");
    bulkUpdateProcessDTO.setPrivilegedMap(new HashMap<String, Boolean>());

    bulkAddCampaignProductDTO = new BulkAddCampaignProductDTO();
    bulkAddCampaignProductDTO.setBulkProcessType("Update");
    bulkAddCampaignProductDTO.setBusinessPartnerCode("TOA-14961");
    bulkAddCampaignProductDTO.setFileName("test.xlsx");
    bulkAddCampaignProductDTO.setFileContent("Hello".getBytes("UTF-8"));
    bulkAddCampaignProductDTO.setUpdatedBy("developer");
    bulkAddCampaignProductDTO.setClientHost("localhost");
    bulkAddCampaignProductDTO.setPrivilegedMap(new HashMap<String, Boolean>());
    bulkAddCampaignProductDTO.setMinDiscount(Double.valueOf(500));
    bulkAddCampaignProductDTO.setMaxDiscount(Double.valueOf(15000));

    bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(UUID.randomUUID().toString());
    bulkUpdateQueue.setBusinessPartnerCode("TOA-14961");
    bulkUpdateQueue.setBulkProcessType("Update");
    bulkUpdateQueue.setClientHost("localhost");
    bulkUpdateQueue.setFileName("test.xlsx");
    bulkUpdateQueue.setPrivilegedMap(new HashMap<String, Boolean>());
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setUpdatedBy("developer");

    bulkAddCampaignProductQueue = new BulkAddCampaignProductQueue();
    bulkAddCampaignProductQueue.setBulkProcessCode(UUID.randomUUID().toString());
    bulkAddCampaignProductQueue.setBusinessPartnerCode("TOA-14961");
    bulkAddCampaignProductQueue.setBulkProcessType("Update");
    bulkAddCampaignProductQueue.setClientHost("localhost");
    bulkAddCampaignProductQueue.setFileName("test.xlsx");
    bulkAddCampaignProductQueue.setPrivilegedMap(new HashMap<String, Boolean>());
    bulkAddCampaignProductQueue.setStoreId("10001");
    bulkAddCampaignProductQueue.setUpdatedBy("developer");

    bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType("Update");
    bulkProcess.setBusinessPartnerCode("TOA-14961");
    bulkProcess.setCreatedBy("developer");
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setId("12345");
    bulkProcess.setStartDate(new Date());
    bulkProcess.setStoreId("10001");
    bulkProcess.setUpdatedBy("developer");
    bulkProcess.setUpdatedDate(new Date());

    Set<String> itemSku = new HashSet<>();
    itemSku.add(DEFAULT_GDN_SKU);
    productCampaignAvailabilityRequest.setItemSkus(itemSku);
    productCampaignAvailabilityRequest.setItemInfo(Collections.singleton(new ItemInfoDto(DEFAULT_GDN_SKU, null, null)));
    productCampaignAvailabilityRequest.setMerchantCode(DEFAULT_BP_CODE);

    Map<String, Boolean> campaignMap = new HashMap<>();
    campaignMap.put(DEFAULT_GDN_SKU, false);
    productCampaignAvailabilityResponse.setProductCampaignAvailabilityMap(campaignMap);

    ReflectionTestUtils.setField(bulkUpdateServiceBean, "partitionSize", 100);

    productCampaignAvailabilityRequest2.setItemSkus(new HashSet<>(Collections.singletonList(DEFAULT_GDN_SKU_3)));
    productCampaignAvailabilityRequest2.setMerchantCode(DEFAULT_BP_CODE);

    systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue("1");
    systemParameterConfig.setVariable(Constant.MINIMUM_PRICE);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString(), eq(BulkProcess.STATUS_PENDING))).thenReturn(this.getBulkProcess());
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE)).thenReturn(systemParameterConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_UPDATE_SWITCH)).thenReturn(
        new SystemParameterConfig("newBulkWorkflowSwitch", "false", "newBulkWorkflowSwitch"));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.CAMPAIGN_UPLOAD_SWITCH)).thenReturn(
        new SystemParameterConfig("campaignUploadWorkflowSwitch", "false", "campaignWorkflowSwitch"));
    when(bulkFailedProductFileService.getDownloadLinkHtml(anyString())).thenReturn(
        "http://static-nginx.qa2-sg.cld/x-bulk.xls");
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH, "false",
            SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH));
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "campaignValidatePartitionSize", 50);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "staticBaseUrl", "http://static-nginx.qa2-sg.cld");
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "deletePickupPointStatusImprovement", true);

    privilegedMap.put(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK, true);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);

    bulkArchiveSwitch.setValue(String.valueOf(Boolean.FALSE));
    bulkArchiveSystemParameter.setValue(String.valueOf(10));
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE))
      .thenReturn(bulkArchiveSystemParameter);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", false);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "callPromoPriceValidate", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "itemPickupPointListFetchSize", 1);

    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setName(PICKUP_POINT_CODE);

    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PICKUP_POINT_CODE_1);
    pickupPointResponse1.setName(PICKUP_POINT_CODE);
    when(pcbOutboundService.getAllChildCategoriesFromC1CategoryCode(anyString(),
        any(CategoryCodeRequest.class), anyBoolean())).thenReturn(new ArrayList<>());
    Mockito.when(pickupPointService
      .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
      .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));

    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    counter = new BulkUpdateErrorCounter();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(trackerService);
    verifyNoMoreInteractions(productLevel3Repository, productCategoryBaseRepository);
    verifyNoMoreInteractions(xCampaignFeign);
    verifyNoMoreInteractions(productBusinessPartnerRepository);
    verifyNoMoreInteractions(systemParameterConfigService);
    verifyNoMoreInteractions(bulkProcessDataService);
    verifyNoMoreInteractions(campaignRepository);
    verifyNoMoreInteractions(bulkFailedProductFileService);
    verifyNoMoreInteractions(bulkArchiveService);
    verifyNoMoreInteractions(bulkProcessFileGeneration);
    verifyNoMoreInteractions(bulkProcessHelperFactory);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  private BulkUpdateQueue getBulkUpdateQueue() {
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkUpdateQueue.setBusinessPartnerCode("TOA-14961");
    bulkUpdateQueue.setBulkProcessType("Update");
    bulkUpdateQueue.setClientHost("localhost");
    bulkUpdateQueue.setFileName("test.xlsx");
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setUpdatedBy("developer");
    bulkUpdateQueue.setRequestId(DEFAULT_REQUEST_ID);

    Map<String, Boolean> privilegedMap = new HashMap<String, Boolean>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, true);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    bulkUpdateQueue.setPrivilegedMap(privilegedMap);

    return bulkUpdateQueue;
  }

  private BulkUpdateQueue getBulkUpdateAmphiQueue() {
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkUpdateQueue.setBusinessPartnerCode("TOA-14961");
    bulkUpdateQueue.setBulkProcessType("Update");
    bulkUpdateQueue.setClientHost("localhost");
    bulkUpdateQueue.setFileName("test.xlsx");
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setUpdatedBy("developer");
    bulkUpdateQueue.setRequestId(DEFAULT_REQUEST_ID);

    Map<String, Boolean> privilegedMap = new HashMap<String, Boolean>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, true);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    bulkUpdateQueue.setPrivilegedMap(privilegedMap);

    return bulkUpdateQueue;
  }

  private BulkAddCampaignProductQueue getBulkAddCampaignQueue() {
    BulkAddCampaignProductQueue bulkUpdateQueue = new BulkAddCampaignProductQueue();
    bulkUpdateQueue.setBulkProcessCode("9092");
    bulkUpdateQueue.setBusinessPartnerCode("TOA-14961");
    bulkUpdateQueue.setBulkProcessType("Update");
    bulkUpdateQueue.setClientHost("localhost");
    bulkUpdateQueue.setFileName("test.xlsx");
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setUpdatedBy("developer");
    bulkUpdateQueue.setRequestId("request-id");
    CampaignItemSummaryRequest campaignItemSummaryRequest = new CampaignItemSummaryRequest();
    campaignItemSummaryRequest.setBrands(Collections.emptyList());
    campaignItemSummaryRequest.setCategories(Collections.emptyList());
    bulkUpdateQueue.setCampaignItemSummaryRequest(campaignItemSummaryRequest);

    Map<String, Boolean> privilegedMap = new HashMap<String, Boolean>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, true);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    bulkUpdateQueue.setPrivilegedMap(privilegedMap);

    return bulkUpdateQueue;
  }

  private BulkProcess getBulkProcess() {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType("Update");
    bulkProcess.setBusinessPartnerCode("TOA-14961");
    bulkProcess.setCreatedBy("developer");
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setId("12345");
    bulkProcess.setStartDate(new Date());
    bulkProcess.setStoreId("10001");
    bulkProcess.setUpdatedBy("developer");
    bulkProcess.setUpdatedDate(new Date());
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setStatus("PENDING");
    bulkProcess.setRequestId(DEFAULT_REQUEST_ID);
    bulkProcess.setBulkProcessNotes(new ArrayList<BulkProcessNotes>());

    return bulkProcess;
  }

  private ItemPickupPointListingL3Response getItemPickupPointL3Response() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setItemSku(DEFAULT_GDN_SKU);
    ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
    itemPickupPointListingL3Response.setPrices(Collections.singletonList(productLevel3PriceResponse));
    itemPickupPointListingL3Response.setViewConfigs(
        Collections.singletonList(new ProductLevel3ViewConfigResponse(DEFAULT_USERNAME, false, false)));
    itemPickupPointListingL3Response.setWebSyncStock(true);
    return itemPickupPointListingL3Response;
  }

  private ProfileResponse getProfileResponse() {
    ProfileResponse profileResponse = new ProfileResponse();
    List<PickupPointDTO> pickupPointList = new ArrayList<PickupPointDTO>();
    PickupPointDTO pickupPoint = new PickupPointDTO();
    pickupPoint.setCode("PP-3000213");
    pickupPointList.add(pickupPoint);
    profileResponse.setPickupPoints(pickupPointList);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(false);
    profileResponse.setCompany(companyDTO);
    profileResponse.setTrustedSeller(true);

    return profileResponse;
  }

  private ProductLevel3SummaryResponse getProductLevel3SummaryResponse() {
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    productLevel3SummaryResponse.setItemSku("TOA-14961-00117-00006");

    productLevel3SummaryResponse.setSynchronizeStock(false);
    productLevel3SummaryResponse.setLateFulfillment(false);
    productLevel3SummaryResponse.setProductType(1);
    productLevel3SummaryResponse.setPickupPointCode("PP-3000213");
    productLevel3SummaryResponse.setOff2OnActiveFlag(false);
    productLevel3SummaryResponse.setAvailableStockLevel2(1);
    productLevel3SummaryResponse.setCategoryCode(CATEGORY_CODE);
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<ProductLevel3ViewConfigResponse>();
    ProductLevel3ViewConfigResponse viewConfig = new ProductLevel3ViewConfigResponse();
    viewConfig.setDisplay(true);
    viewConfig.setBuyable(true);
    viewConfigs.add(viewConfig);
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<ProductLevel3PriceResponse>();
    ProductLevel3PriceResponse price = new ProductLevel3PriceResponse();
    price.setPrice(50000.0);
    price.setSalePrice(25000.0);
    prices.add(price);
    productLevel3SummaryResponse.setPrices(prices);
    productLevel3SummaryResponse.setArchived(false);
    productLevel3SummaryResponse.setOriginalSellingPrice(1000.0);
    return productLevel3SummaryResponse;
  }

  private ProductLevel3SummaryResponse getProductLevel3SummaryResponse2() {
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    productLevel3SummaryResponse.setItemSku(DEFAULT_GDN_SKU_3);

    productLevel3SummaryResponse.setSynchronizeStock(false);
    productLevel3SummaryResponse.setLateFulfillment(false);
    productLevel3SummaryResponse.setProductType(1);
    productLevel3SummaryResponse.setPickupPointCode("PP-3000213");
    productLevel3SummaryResponse.setOff2OnActiveFlag(false);
    productLevel3SummaryResponse.setAvailableStockLevel2(1);
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<ProductLevel3ViewConfigResponse>();
    ProductLevel3ViewConfigResponse viewConfig = new ProductLevel3ViewConfigResponse();
    viewConfig.setDisplay(true);
    viewConfig.setBuyable(true);
    viewConfigs.add(viewConfig);
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<ProductLevel3PriceResponse>();
    ProductLevel3PriceResponse price = new ProductLevel3PriceResponse();
    price.setPrice(50000.0);
    price.setSalePrice(25000.0);
    prices.add(price);
    productLevel3SummaryResponse.setPrices(prices);
    productLevel3SummaryResponse.setArchived(false);
    return productLevel3SummaryResponse;
  }

  @Test
  public void test_preProcessBulkUpdate_positive() {
    try {
      ProfileResponse profileResponse = new ProfileResponse();
      profileResponse.setTrustedSeller(false);
      when(systemParameterConfigService
          .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED))
          .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "false",
              SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
      when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString()))
          .thenReturn(profileResponse);
      when(bulkUpdateServiceUtil
          .getBulkUpdateQueue(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class)))
          .thenReturn(bulkUpdateQueue);
      when(bulkUpdateServiceUtil
          .getBulkProcess(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class), anyInt(), anyInt(),
              anyBoolean(), anyBoolean())).thenReturn(bulkProcess);
      when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(new BulkProcess());
      when(objectMapper.writeValueAsString(any(BulkUpdateQueue.class))).thenReturn("Some Result String");
      bulkUpdateServiceBean.preProcessBulkUpdate("10001", DEFAULT_REQUEST_ID, bulkUpdateProcessDTO,
          null);
      Mockito.verify(fileStorageServiceBean)
        .createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
      Mockito.verify(kafkaProducer)
          .send(Mockito.eq(kafkaTopicProperties.getBulkUploadEvent()), Mockito.any(BulkUpdateQueue.class));
      verify(bulkProcessRepository).save(any(BulkProcess.class));
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
      Mockito.verify(kafkaTopicProperties,times(2)).getBulkUploadEvent();
    } catch (Exception e) {
      e.printStackTrace();
      Assertions.fail();
    }
  }

  @Test
  public void test_preProcessBulkUpdate_positive_accessiblePickupPoints() {
    try {
      ProfileResponse profileResponse = new ProfileResponse();
      profileResponse.setTrustedSeller(false);
      when(systemParameterConfigService
          .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED))
          .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "false",
              SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
      when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString()))
          .thenReturn(profileResponse);
      when(bulkUpdateServiceUtil
          .getBulkUpdateQueue(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class)))
          .thenReturn(bulkUpdateQueue);
      when(bulkUpdateServiceUtil
          .getBulkProcess(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class), anyInt(), anyInt(),
              anyBoolean(), anyBoolean())).thenReturn(bulkProcess);
      when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(new BulkProcess());
      when(objectMapper.writeValueAsString(any(BulkUpdateQueue.class))).thenReturn("Some Result String");
      bulkUpdateServiceBean.preProcessBulkUpdate("10001", DEFAULT_REQUEST_ID, bulkUpdateProcessDTO,
          Collections.singleton(PICKUP_POINT_CODE));
      Mockito.verify(fileStorageServiceBean)
          .createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
      Mockito.verify(kafkaProducer)
          .send(Mockito.eq(kafkaTopicProperties.getBulkUploadEvent()), Mockito.any(BulkUpdateQueue.class));
      verify(bulkProcessRepository).save(any(BulkProcess.class));
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
      Mockito.verify(kafkaTopicProperties,times(2)).getBulkUploadEvent();
    } catch (Exception e) {
      e.printStackTrace();
      Assertions.fail();
    }
  }

  @Test
  public void test_preProcessBulkUpdate_positive_withUpdatePriority1() {
    try {
      ProfileResponse profileResponse = new ProfileResponse();
      profileResponse.setTrustedSeller(true);
      when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString()))
          .thenReturn(profileResponse);
      bulkUpdateProcessDTO.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue());
      when(bulkUpdateServiceUtil
          .getBulkUpdateQueue(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class)))
          .thenReturn(bulkUpdateQueue);
      when(bulkUpdateServiceUtil
          .getBulkProcess(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class), anyInt(), anyInt(),
              anyBoolean(), anyBoolean())).thenReturn(bulkProcess);
      when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(new BulkProcess());
      when(objectMapper.writeValueAsString(any(BulkUpdateQueue.class))).thenReturn("Some Result String");
      bulkUpdateServiceBean.preProcessBulkUpdate("10001", DEFAULT_REQUEST_ID, bulkUpdateProcessDTO,
          null);
      Mockito.verify(fileStorageServiceBean).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
      Mockito.verify(kafkaProducer)
          .send(Mockito.eq(kafkaTopicProperties.getBulkUpdatePriority1Event()),
              Mockito.any(BulkUpdateQueue.class));
      verify(bulkProcessRepository).save(any(BulkProcess.class));
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
      Mockito.verify(kafkaTopicProperties, times(3)).getBulkUpdatePriority1Event();
    } catch (Exception e) {
      e.printStackTrace();
      Assertions.fail();
    }
  }

  @Test
  public void test_preProcessBulkUpdate_positive_withNonNullPRofileResponseAndNotTrustedSeller() {
    try {
      ProfileResponse profileResponse = new ProfileResponse();
      profileResponse.setTrustedSeller(false);
      when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString()))
          .thenReturn(profileResponse);
      bulkUpdateProcessDTO.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue());
      when(bulkUpdateServiceUtil
          .getBulkUpdateQueue(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class)))
          .thenReturn(bulkUpdateQueue);
      when(bulkUpdateServiceUtil
          .getBulkProcess(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class), anyInt(), anyInt(),
              anyBoolean(), anyBoolean())).thenReturn(bulkProcess);
      when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(new BulkProcess());
      when(objectMapper.writeValueAsString(any(BulkUpdateQueue.class))).thenReturn("Some Result String");
      bulkUpdateServiceBean.preProcessBulkUpdate("10001", DEFAULT_REQUEST_ID, bulkUpdateProcessDTO,
          null);
      Mockito.verify(fileStorageServiceBean).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
      Mockito.verify(kafkaProducer)
          .send(Mockito.eq(kafkaTopicProperties.getBulkUploadEvent()),
              Mockito.any(BulkUpdateQueue.class));
      verify(bulkProcessRepository).save(any(BulkProcess.class));
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
      Mockito.verify(kafkaTopicProperties,times(2)).getBulkUploadEvent();
    } catch (Exception e) {
      e.printStackTrace();
      Assertions.fail();
    }
  }

  @Test
  public void test_preProcessCampaignProductBulkUpdate_positive() {
    try {
      when(bulkUpdateServiceUtil
          .getBulkProcess(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class), anyInt(), anyInt(),
              anyBoolean(), anyBoolean())).thenReturn(bulkProcess);
      when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(new BulkProcess());
      when(objectMapper.writeValueAsString(any(BulkAddCampaignProductQueue.class))).thenReturn("Some Result String");
      bulkUpdateServiceBean.preProcessCampaignProductBulkUpdate("10001", DEFAULT_REQUEST_ID, bulkAddCampaignProductDTO);
      Mockito.verify(kafkaProducer).send(Mockito.eq(kafkaTopicProperties.getBulkUploadCampaignEvent()), Mockito.any());
      verify(bulkProcessRepository).save(any(BulkProcess.class));
      Mockito.verify(kafkaTopicProperties,times(2)).getBulkUploadCampaignEvent();
    } catch (Exception e) {
      e.printStackTrace();
      Assertions.fail();
    }
  }

  @Test
  public void test_preProcessCampaignProductBulkUpdate_throwsException() {
    try {
      when(bulkUpdateServiceUtil
          .getBulkProcess(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class), anyInt(), anyInt(),
              anyBoolean(), anyBoolean())).thenReturn(bulkProcess);
      when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(new BulkProcess());
      when(objectMapper.writeValueAsString(any(BulkAddCampaignProductQueue.class))).thenReturn("Some Result String");
      bulkUpdateServiceBean.preProcessCampaignProductBulkUpdate("10001", DEFAULT_REQUEST_ID, bulkAddCampaignProductDTO);
      Mockito.verify(kafkaProducer).send(Mockito.eq(kafkaTopicProperties.getBulkUploadCampaignEvent()), Mockito.any());
      verify(bulkProcessRepository).save(any(BulkProcess.class));
      Mockito.verify(kafkaTopicProperties, times(2)).getBulkUploadCampaignEvent();
    } catch (Exception e) {
      e.printStackTrace();
      Assertions.fail();
    }
  }

  private List<LogAuditTrailUpdatedProductRequest> getLogAuditTrailUpdatedProductRequestList(int size) {
    List<LogAuditTrailUpdatedProductRequest> auditLogForProductUpdateList =
        new ArrayList<LogAuditTrailUpdatedProductRequest>();
    for (int counter = 0; counter < size; counter++) {
      auditLogForProductUpdateList.add(getLogAuditTrailUpdatedProductRequest());
    }
    return auditLogForProductUpdateList;
  }

  private LogAuditTrailUpdatedProductRequest getLogAuditTrailUpdatedProductRequest() {
    LogAuditTrailUpdatedProductRequest auditLogForProductUpdate = new LogAuditTrailUpdatedProductRequest();
    auditLogForProductUpdate.setBusinessPartnerCode(UUID.randomUUID().toString());
    return auditLogForProductUpdate;
  }

  @Test
  public void processBulkUpdateExceptionTest() throws Exception {
    productCampaignAvailabilityRequest.setItemSkus(new HashSet<>());
    productCampaignAvailabilityResponse.setProductCampaignAvailabilityMap(new HashMap<>());
    setBulkUpdateServiceUtil();
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        bulkUpdateQueue.getStoreId(), bulkUpdateQueue.getBulkProcessCode(),
        BulkProcess.STATUS_PENDING)).thenReturn(null);
    try {
      RuntimeException runtimeException = Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceBean.processBulkUpdate(bulkUpdateQueue));
    } finally {
      BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
//      Mockito.verify(trackerService)
//          .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE),
//              Mockito.eq(HYPHEN), Mockito.eq(TrackerConstants.FAILED), Mockito.anyString());
    }
  }

  @Test
  public void processBulkUpdateExceptionV2Test() throws Exception {
    productCampaignAvailabilityRequest.setItemSkus(new HashSet<>());
    productCampaignAvailabilityResponse.setProductCampaignAvailabilityMap(new HashMap<>());
    setBulkUpdateServiceUtil();
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
      bulkUpdateQueue.getStoreId(), bulkUpdateQueue.getBulkProcessCode(),
      BulkProcess.STATUS_PENDING)).thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
        () -> bulkUpdateServiceBean.processBulkUpdateV2(bulkUpdateQueue));
    } finally {
      BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    }
  }

  @Test
  public void processBulkUpdateEventTest() throws Exception {
    productCampaignAvailabilityRequest.setItemSkus(new HashSet<>());
    productCampaignAvailabilityResponse.setProductCampaignAvailabilityMap(new HashMap<>());
    setBulkUpdateServiceUtil();
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
      bulkUpdateQueue.getStoreId(), bulkUpdateQueue.getBulkProcessCode(),
      BulkProcess.STATUS_PENDING)).thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
        () -> bulkUpdateServiceBean.processBulkUpdateEvent(bulkUpdateQueue));
    } finally {
      BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    }
  }

  @Test
  public void processBulkUpdateEventSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateUsingFastExcelEnabled", true);
    productCampaignAvailabilityRequest.setItemSkus(new HashSet<>());
    productCampaignAvailabilityResponse.setProductCampaignAvailabilityMap(new HashMap<>());
    setBulkUpdateServiceUtil();
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
      bulkUpdateQueue.getStoreId(), bulkUpdateQueue.getBulkProcessCode(),
      BulkProcess.STATUS_PENDING)).thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
        () -> bulkUpdateServiceBean.processBulkUpdateEvent(bulkUpdateQueue));
    } finally {
      BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    }
  }

  @Test
  void testHandleExceptionForBulkUpdate_HeaderMismatch() {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "headerValidationCheck", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "updateProductDownloadLink", "http://dummy-link");

    Exception e = new Exception(BulkUpdateServiceUtil.HEADER_MISMATCH);
    when(fileStorageServiceBean.getDownloadLinkHtml("http://dummy-link")).thenReturn("<a>link</a>");

    bulkUpdateServiceBean.handleExceptionForBulkUpdate(bulkUpdateQueue, e, bulkProcess, "store1",
      "code1", counter);

    assertEquals("File tidak valid. Silakan download template baru dan coba lagi.", bulkProcess.getDescription());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
      eq(NotificationType.BULK_UPDATED.getValue()), eq("<a>link</a>"));
    verify(trackerService).sendTracker(anyString(), anyString(), anyString(),
      eq(TrackerConstants.FAILED), eq("developer"));
    verify(bulkProcessService).saveOperation(bulkProcess);
  }

  @Test
  void testHandleExceptionForBulkUpdate_MaximumRowErrorEN() {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "headerValidationCheck", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "updateProductDownloadLink", "http://dummy-link");

    Exception e = new Exception(
      ErrorCategory.VALIDATION.getMessage() + ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN);

    bulkUpdateServiceBean.handleExceptionForBulkUpdate(bulkUpdateQueue, e, bulkProcess, "store1",
      "code1", counter);

    assertTrue(
      bulkProcess.getDescription().contains(ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN));
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
      eq(NotificationType.BULK_UPDATED.getValue()), eq(StringUtils.EMPTY));
    verify(trackerService).sendTracker(anyString(), anyString(), anyString(),
      eq(TrackerConstants.FAILED), eq("developer"));
    verify(bulkProcessService).saveOperation(bulkProcess);
  }

  @Test
  void testHandleExceptionForBulkUpdate_MaximumRowErrorIN() {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "headerValidationCheck", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "updateProductDownloadLink", "http://dummy-link");

    Exception e = new Exception(
      ErrorCategory.VALIDATION.getMessage() + ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_IN);

    bulkUpdateServiceBean.handleExceptionForBulkUpdate(bulkUpdateQueue, e, bulkProcess, "store1",
      "code1", counter);

    assertTrue(
      bulkProcess.getDescription().contains(ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_IN));
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
      eq(NotificationType.BULK_UPDATED.getValue()), eq(StringUtils.EMPTY));
    verify(trackerService).sendTracker(anyString(), anyString(), anyString(),
      eq(TrackerConstants.FAILED), eq("developer"));
    verify(bulkProcessService).saveOperation(bulkProcess);
  }

  @Test
  void testHandleExceptionForBulkUpdate_NoHeaderValidation() {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "headerValidationCheck", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "updateProductDownloadLink", "http://dummy-link");

    ReflectionTestUtils.setField(bulkUpdateServiceBean, "headerValidationCheck", false);
    Exception e = new Exception("random error");

    bulkUpdateServiceBean.handleExceptionForBulkUpdate(bulkUpdateQueue, e, bulkProcess, "store1",
      "code1", counter);

    assertNull(bulkProcess.getDescription());
    verify(trackerService).sendTracker(anyString(), anyString(), anyString(),
      eq(TrackerConstants.FAILED), eq("developer"));
    verify(bulkProcessService).saveOperation(bulkProcess);
    verifyNoInteractions(notificationService);
  }

  private void setBulkUpdateServiceUtil() {
    BulkUpdateServiceUtil bulkUpdateServiceUtil = new BulkUpdateServiceUtil();
    bulkUpdateServiceUtil.setObjectMapper(this.objectMapper);
    bulkUpdateServiceUtil.setBulkProcessService(this.bulkProcessService);
    bulkUpdateServiceUtil.setNotificationService(notificationService);
    bulkUpdateServiceUtil.setBusinessPartnerRepository(businessPartnerRepository);
    bulkUpdateServiceUtil.setxProductOutboundService(xProductOutboundService);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(bulkUpdateServiceUtil);
    bulkUpdateServiceUtil.setPcbOutboundService(pcbOutboundService);
  }

  @Test
  public void processCampaignProductBulkUpdateExceptionTest() throws Exception {
    setBulkUpdateServiceUtil();
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        anyString(), anyString(), eq(BulkProcess.STATUS_PENDING))).thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceBean.processCampaignProductBulkUpdate(
              bulkAddCampaignProductQueue));
    } finally {
      BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    }
  }

  @Test
  public void testProcessWithUpdateProductsInBulkSwitchOn() throws Exception {
    systemParameterConfig.setValue(MAX_PRODUCT_SIZE);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFiles();
    files.put(BulkParameters.STOCK_REMINDER_COLUMN_ID, "1");
    files.put(BulkParameters.PICKUP_POINT_NAME_COLUMN_ID, "1");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProfileResponse profileResponse = this.getProfileResponse();
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse = this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
      ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(file);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(Mockito.any(), Mockito.any()))
      .thenReturn(sheet);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    when(systemParameterConfigService
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class))).thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(new HashMap<>());
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn("");
    bulkUpdateServiceBean.processBulkUpdate(bulkUpdateQueue);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    Mockito.verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    Mockito.verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository).filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void testProcessWithUpdateProductsInBulkSwitchOn_cnc1P() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "cncForWarehouseFeatureSwitch", true);
    systemParameterConfig.setValue(MAX_PRODUCT_SIZE);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdate_cnc1P.xlsx");
    files.put(BulkParameters.STOCK_REMINDER_COLUMN_ID, "1");
    files.put(BulkParameters.PICKUP_POINT_NAME_COLUMN_ID, "1");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    Map<String, Boolean> privilegedMap1 = bulkUpdateQueue.getPrivilegedMap();
    privilegedMap1.put("isPrivilegedToEditCncStatus", true);
    bulkUpdateQueue.setPrivilegedMap(privilegedMap1);
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse =
        this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    ProcessorUtils.createDirectories(
        ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
        ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(file);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(Mockito.any(), Mockito.any()))
        .thenReturn(sheet);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE)).thenReturn(
        systemParameterConfig);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(0),
            Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
        .thenReturn(new HashMap<>());
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn("");
    bulkUpdateServiceBean.processBulkUpdate(bulkUpdateQueue);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    Mockito.verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    Mockito.verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
        .filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.anyInt(),
            Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void testProcessBulkUpdateV2_WithRealExcelFile() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "columnsToParseAsDouble", "Status;CNC");
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "cncForWarehouseFeatureSwitch", true);
    systemParameterConfig.setValue(MAX_PRODUCT_SIZE);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdate_cnc1P.xlsx");
    files.put(BulkParameters.STOCK_REMINDER_COLUMN_ID, "1");
    files.put(BulkParameters.PICKUP_POINT_NAME_COLUMN_ID, "1");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    Map<String, Boolean> privilegedMap1 = bulkUpdateQueue.getPrivilegedMap();
    privilegedMap1.put("isPrivilegedToEditCncStatus", true);
    bulkUpdateQueue.setPrivilegedMap(privilegedMap1);
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse =
      this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
      anyString())).thenReturn(profileResponse);
    ProcessorUtils.createDirectories(
      ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
      ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
      ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    InputStream is = new FileInputStream(file);
    assertNotNull(is);
    // Mock GCS layer to return actual FileInputStream (like your openGcsInputStream)
    when(fileStorageServiceBean.openGcsInputStream(any(), any())).thenReturn(is);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
      new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
      new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
      new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED)).thenReturn(
      new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
        SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE)).thenReturn(
      systemParameterConfig);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(0),
        Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
      .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
      .thenReturn(new HashMap<>());
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn("");
    bulkUpdateServiceBean.processBulkUpdateV2(bulkUpdateQueue);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    Mockito.verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    Mockito.verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
      .filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void testProcessWithUpdateProductsInBulkSwitchOn_cnc1P_amphi() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "cncForWarehouseFeatureSwitch", true);
    systemParameterConfig.setValue(MAX_PRODUCT_SIZE);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateAmphi_cnc1P.xlsx");
    files.put(BulkParameters.STOCK_REMINDER_COLUMN_ID, "1");
    files.put(BulkParameters.PICKUP_POINT_NAME_COLUMN_ID, "1");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    Map<String, Boolean> privilegedMap1 = bulkUpdateQueue.getPrivilegedMap();
    privilegedMap1.put("isPrivilegedToEditCncStatus", true);
    bulkUpdateQueue.setPrivilegedMap(privilegedMap1);
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse =
        this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    ProcessorUtils.createDirectories(
        ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
        ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(file);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(Mockito.any(), Mockito.any()))
        .thenReturn(sheet);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE)).thenReturn(
        systemParameterConfig);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(0),
            Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
        .thenReturn(new HashMap<>());
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn("");
    bulkUpdateServiceBean.processBulkUpdate(bulkUpdateQueue);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    Mockito.verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    Mockito.verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
        .filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.anyInt(),
            Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void testProcessWithUpdateProductsInBulkSwitchOn_cnc1P_amphi_non_cnc() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "cncForWarehouseFeatureSwitch", true);
    systemParameterConfig.setValue(MAX_PRODUCT_SIZE);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateAmphi_cnc1P.xlsx");
    files.put(BulkParameters.STOCK_REMINDER_COLUMN_ID, "1");
    files.put(BulkParameters.PICKUP_POINT_NAME_COLUMN_ID, "1");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    Map<String, Boolean> privilegedMap1 = bulkUpdateQueue.getPrivilegedMap();
    privilegedMap1.put("isPrivilegedToEditCncStatus", true);
    bulkUpdateQueue.setPrivilegedMap(privilegedMap1);
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse =
        this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    ProcessorUtils.createDirectories(
        ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
        ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(file);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(Mockito.any(), Mockito.any()))
        .thenReturn(sheet);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE)).thenReturn(
        systemParameterConfig);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(0),
            Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
        .thenReturn(new HashMap<>());
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn("");
    bulkUpdateServiceBean.processBulkUpdate(bulkUpdateQueue);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    Mockito.verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    Mockito.verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
        .filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.anyInt(),
            Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void testProcessWithUpdateProductsInBulkSwitchOn_cnc1P_external_non_cnc() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "cncForWarehouseFeatureSwitch", true);
    systemParameterConfig.setValue(MAX_PRODUCT_SIZE);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdate_cnc1P.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    Map<String, Boolean> privilegedMap1 = bulkUpdateQueue.getPrivilegedMap();
    privilegedMap1.put("isPrivilegedToEditCncStatus", true);
    bulkUpdateQueue.setPrivilegedMap(privilegedMap1);
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse =
        this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    ProcessorUtils.createDirectories(
        ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
        ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(file);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(Mockito.any(), Mockito.any()))
        .thenReturn(sheet);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE)).thenReturn(
        systemParameterConfig);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(0),
            Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
        .thenReturn(new HashMap<>());
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn("");
    bulkUpdateServiceBean.processBulkUpdate(bulkUpdateQueue);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    Mockito.verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    Mockito.verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
        .filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.anyInt(),
            Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void testProcessCampaignProductBulkUpdate_Success() throws Exception {
    GdnRestSingleResponse<ProductLevel3SummaryResponse> expectedResponse = getSucessResponse();
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 2);
    Map<String, String> files = this.getCampaignFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse = this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode()
            + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processCampaignProductBulkUpdate(bulkUpdateQueue);
    verify(bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void testProcessCampaignProductBulkUpdate_NewTemplateSuccess() throws Exception {
    setBulkUpdateServiceUtil();
    GdnRestSingleResponse<ProductLevel3SummaryResponse> expectedResponse = getSucessResponse();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 2);
    Map<String, String> files = this.getNewCampaignFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse = this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
      ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    byte[] byteArray = FileUtils.readFileToByteArray(file);
    when(fileStorageServiceBean.downloadFile(Mockito.any(), Mockito.anyString()))
      .thenReturn(byteArray);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processCampaignProductBulkUpdate(bulkUpdateQueue);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.CAMPAIGN_UPLOAD_SWITCH);
  }


  @Test
  public void testProcessCampaignProductBulkUpdate_mppSwitchOn() throws Exception {
    setBulkUpdateServiceUtil();
    setUpRepositoriesForCampaign();
    Map<String, String> files = this.getNewCampaignFiles();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 2);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    bulkUpdateQueue.getCampaignItemSummaryRequest().setCategories(Arrays.asList(CATEGORY_CODE, CATEGORY_CODE1));
    ProfileResponse profileResponse = this.getProfileResponse();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = getProductLevel3ReponsesForCampaign();
    productLevel3SummaryResponses.get(0).getPrices().get(0).setPrice(6000.0);
    when(pcbOutboundService.getAllChildCategoriesFromC1CategoryCode(anyString(),
        any(CategoryCodeRequest.class), anyBoolean())).thenReturn(Collections.emptyList());
    ProcessorUtils
        .createDirectories(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
      ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    byte[] byteArray = FileUtils.readFileToByteArray(file);
    when(fileStorageServiceBean.downloadFile(Mockito.any(), Mockito.anyString()))
      .thenReturn(byteArray);
    Map<String, String> errorMap = Collections.singletonMap(productLevel3SummaryResponses.get(0).getItemSku(),
        BulkErrorCategory.DUPLICATE_PRODUCT_SKUS.getDescription());
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenReturn(profileResponse);
    when(productCategoryBaseRepository.getAllChildCategoriesFromC1CategoryCode(anyString(), anyString(), any()))
        .thenReturn(Collections.emptyList());
    when(xCampaignFeign
        .addCampaignProductV2(eq(Constant.STORE_ID), eq(X_BULK_CLIENT), eq(X_BULK_CLIENT), eq(REQUEST_ID),
            eq(X_BULK_CLIENT), Mockito.anyList())).thenReturn(
        new GdnRestSingleResponse<>(null, null,
            true, FailedProductsResponse.builder().failedSkus(errorMap).build(), X_BULK_CLIENT));
    when(this.bulkProcessRepository.save(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processCampaignProductBulkUpdate(bulkUpdateQueue);
    verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    verify(pcbOutboundService, times(2)).getAllChildCategoriesFromC1CategoryCode(anyString(),any(CategoryCodeRequest.class),anyBoolean());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.CAMPAIGN_UPLOAD_SWITCH);
  }

  @Test
  public void testProcessCampaignProductBulkUpdate_WithCampaignException() throws Exception {
    setBulkUpdateServiceUtil();
    setUpRepositoriesForCampaign();
    Map<String, String> files = this.getNewCampaignFiles();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 2);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    bulkUpdateQueue.getCampaignItemSummaryRequest().setCategories(Arrays.asList(CATEGORY_CODE, CATEGORY_CODE1));
    ProfileResponse profileResponse = this.getProfileResponse();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = getProductLevel3ReponsesForCampaign();
    productLevel3SummaryResponses.get(0).getPrices().get(0).setPrice(6000.0);
    when(pcbOutboundService.getAllChildCategoriesFromC1CategoryCode(anyString(),
      any(CategoryCodeRequest.class), anyBoolean())).thenReturn(Collections.emptyList());
    ProcessorUtils
      .createDirectories(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
      ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
      ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    byte[] byteArray = FileUtils.readFileToByteArray(file);
    when(fileStorageServiceBean.downloadFile(Mockito.any(), Mockito.anyString()))
      .thenReturn(byteArray);
    Map<String, String> errorMap = Collections.singletonMap(productLevel3SummaryResponses.get(0).getItemSku(),
      BulkErrorCategory.DUPLICATE_PRODUCT_SKUS.getDescription());
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
      bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString()))
      .thenReturn(profileResponse);
    when(productCategoryBaseRepository.getAllChildCategoriesFromC1CategoryCode(anyString(), anyString(), any()))
      .thenReturn(Collections.emptyList());
    doThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
      ProductUpdateErrorMessages.CAMPAIGN_UPDATE_FAILED)).when(xCampaignFeign)
      .addCampaignProductV2(eq(Constant.STORE_ID), eq(X_BULK_CLIENT), eq(X_BULK_CLIENT),
        eq(REQUEST_ID), eq(X_BULK_CLIENT), Mockito.anyList());
    when(this.bulkProcessRepository.save(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processCampaignProductBulkUpdate(bulkUpdateQueue);
    verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    verify(pcbOutboundService, times(2)).getAllChildCategoriesFromC1CategoryCode(anyString(),any(CategoryCodeRequest.class),anyBoolean());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.CAMPAIGN_UPLOAD_SWITCH);
  }

  @Test
  public void testProcessCampaignProductBulkUpdatePricingExceptionTest() throws Exception {
    setBulkUpdateServiceUtil();
    setUpRepositoriesForCampaign();
    Map<String, String> files = this.getNewCampaignFiles();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "pricingMultiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 2);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    bulkUpdateQueue.getCampaignItemSummaryRequest().setCategories(Arrays.asList(CATEGORY_CODE, CATEGORY_CODE1));
    ProfileResponse profileResponse = this.getProfileResponse();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = getProductLevel3ReponsesForCampaign();
    productLevel3SummaryResponses.get(0).getPrices().get(0).setPrice(6000.0);
    ProcessorUtils
      .createDirectories(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
      ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
      ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    byte[] byteArray = FileUtils.readFileToByteArray(file);
    when(fileStorageServiceBean.downloadFile(Mockito.any(), Mockito.anyString()))
      .thenReturn(byteArray);
    Map<String, String> errorMap = Collections.singletonMap(productLevel3SummaryResponses.get(0).getItemSku(),
      BulkErrorCategory.DUPLICATE_PRODUCT_SKUS.getDescription());
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
      bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString()))
      .thenReturn(profileResponse);
    when(productCategoryBaseRepository.getAllChildCategoriesFromC1CategoryCode(anyString(), anyString(), any()))
      .thenReturn(Collections.emptyList());
    doThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
      ProductUpdateErrorMessages.CAMPAIGN_UPDATE_FAILED)).when(xCampaignFeign)
      .addCampaignProductV2(eq(Constant.STORE_ID), eq(X_BULK_CLIENT), eq(X_BULK_CLIENT),
        eq(REQUEST_ID), eq(X_BULK_CLIENT), Mockito.anyList());
    when(this.bulkProcessRepository.save(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processCampaignProductBulkUpdate(bulkUpdateQueue);
    verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    verify(pcbOutboundService, times(2)).getAllChildCategoriesFromC1CategoryCode(anyString(),any(CategoryCodeRequest.class),anyBoolean());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.CAMPAIGN_UPLOAD_SWITCH);
  }

  @Test
  public void testProcessCampaignProductBulkUpdatePricingMppSwitchOn() throws Exception {
    setBulkUpdateServiceUtil();
    setUpRepositoriesForCampaign();
    Map<String, String> files = this.getNewCampaignFiles();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "pricingMultiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 2);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    bulkUpdateQueue.getCampaignItemSummaryRequest().setCategories(Arrays.asList(CATEGORY_CODE, CATEGORY_CODE1));
    ProfileResponse profileResponse = this.getProfileResponse();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = getProductLevel3ReponsesForCampaign();
    productLevel3SummaryResponses.get(0).getPrices().get(0).setPrice(6000.0);
    ProcessorUtils
        .createDirectories(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
      ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    byte[] byteArray = FileUtils.readFileToByteArray(file);
    when(fileStorageServiceBean.downloadFile(Mockito.any(), Mockito.anyString()))
      .thenReturn(byteArray);
    Map<String, String> errorMap = Collections.singletonMap(productLevel3SummaryResponses.get(0).getItemSku(),
        BulkErrorCategory.DUPLICATE_PRODUCT_SKUS.getDescription());
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenReturn(profileResponse);
    when(productCategoryBaseRepository.getAllChildCategoriesFromC1CategoryCode(anyString(), anyString(), any()))
        .thenReturn(Collections.emptyList());
    when(xCampaignFeign
        .addCampaignProductV2(eq(Constant.STORE_ID), eq(X_BULK_CLIENT), eq(X_BULK_CLIENT), eq(REQUEST_ID),
            eq(X_BULK_CLIENT), Mockito.anyList())).thenReturn(
        new GdnRestSingleResponse<>(null, null,
            true, FailedProductsResponse.builder().failedSkus(errorMap).build(), X_BULK_CLIENT));
    when(this.bulkProcessRepository.save(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processCampaignProductBulkUpdate(bulkUpdateQueue);
    verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    verify(pcbOutboundService, times(2)).getAllChildCategoriesFromC1CategoryCode(anyString(),any(CategoryCodeRequest.class),anyBoolean());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.CAMPAIGN_UPLOAD_SWITCH);
  }

  @Test
  public void testProcessCampaignProductBulkUpdate_mppSwitchOn_emptyStoreId() throws Exception {
    setBulkUpdateServiceUtil();
    setUpRepositoriesForCampaign();
    Map<String, String> files = this.getNewCampaignFiles();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 2);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    when(pcbOutboundService.getAllChildCategoriesFromC1CategoryCode(anyString(),
        any(CategoryCodeRequest.class), anyBoolean())).thenReturn(Collections.emptyList());
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    bulkUpdateQueue.getCampaignItemSummaryRequest().setCategories(Arrays.asList(CATEGORY_CODE, CATEGORY_CODE1));
    ProfileResponse profileResponse = this.getProfileResponse();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = getProductLevel3ReponsesForCampaign();
    productLevel3SummaryResponses.get(0).getPrices().get(0).setPrice(6000.0);
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
      ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
      ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    byte[] byteArray = FileUtils.readFileToByteArray(file);
    when(fileStorageServiceBean.downloadFile(Mockito.any(), Mockito.anyString()))
      .thenReturn(byteArray);
    Map<String, String> errorMap =
      Collections.singletonMap(productLevel3SummaryResponses.get(0).getItemSku(),
        BulkErrorCategory.DUPLICATE_PRODUCT_SKUS.getDescription());
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
      bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    when(productCategoryBaseRepository.getAllChildCategoriesFromC1CategoryCode(anyString(), anyString(),
      any())).thenReturn(Collections.emptyList());
    when(xCampaignFeign.addCampaignProductV2(eq(Constant.STORE_ID), eq(X_BULK_CLIENT),
      eq(X_BULK_CLIENT), eq(REQUEST_ID), eq(X_BULK_CLIENT), Mockito.anyList())).thenReturn(
      new GdnRestSingleResponse<>(
        null, null, true, FailedProductsResponse.builder().failedSkus(errorMap).build(),
        X_BULK_CLIENT));
    when(this.bulkProcessRepository.save(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processCampaignProductBulkUpdate(bulkUpdateQueue);
    verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    verify(pcbOutboundService, times(2)).getAllChildCategoriesFromC1CategoryCode(anyString(),any(CategoryCodeRequest.class),anyBoolean());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.CAMPAIGN_UPLOAD_SWITCH);
  }


  @Test
  public void testProcessCampaignProductBulkUpdate_mppSwitchOn_emptyRequestId() throws Exception {
    setBulkUpdateServiceUtil();
    setUpRepositoriesForCampaign();
    Map<String, String> files = this.getNewCampaignFiles();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 2);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    when(pcbOutboundService.getAllChildCategoriesFromC1CategoryCode(any(),
        any(CategoryCodeRequest.class), anyBoolean())).thenReturn(Collections.emptyList());
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    bulkUpdateQueue.getCampaignItemSummaryRequest().setCategories(Arrays.asList(CATEGORY_CODE, CATEGORY_CODE1));
    ProfileResponse profileResponse = this.getProfileResponse();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = getProductLevel3ReponsesForCampaign();
    productLevel3SummaryResponses.get(0).getPrices().get(0).setPrice(6000.0);
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
      ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
      ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    byte[] byteArray = FileUtils.readFileToByteArray(file);
    when(fileStorageServiceBean.downloadFile(Mockito.any(), Mockito.anyString()))
      .thenReturn(byteArray);
    FailedItemInfoDto failedItemInfoDto = new FailedItemInfoDto();
    failedItemInfoDto.setItemSku(productLevel3SummaryResponses.get(0).getItemSku());
    failedItemInfoDto.setFailedReason( BulkErrorCategory.DUPLICATE_PRODUCT_SKUS.getDescription());
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
      bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    when(productCategoryBaseRepository.getAllChildCategoriesFromC1CategoryCode(anyString(), anyString(),
      any())).thenReturn(Collections.emptyList());
    when(xCampaignFeign.addCampaignProductV2(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(
        new GdnRestSingleResponse<>(null, null,
            true, FailedProductsResponse.builder().failedItemInfo(Arrays.asList(failedItemInfoDto)).build(), X_BULK_CLIENT));
    when(this.bulkProcessRepository.save(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateQueue.setRequestId(null);
    bulkUpdateServiceBean.processCampaignProductBulkUpdate(bulkUpdateQueue);
    verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    verify(pcbOutboundService, times(2)).getAllChildCategoriesFromC1CategoryCode(any(),
        any(CategoryCodeRequest.class), anyBoolean());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.CAMPAIGN_UPLOAD_SWITCH);
  }


  private List<ProductLevel3SummaryResponse> getProductLevel3ReponsesForCampaign() {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse summaryResponse = getProductLevel3SummaryResponse();
    summaryResponse.setItemSku("TOQ-15120-00001-00002");
    productLevel3SummaryResponses.add(summaryResponse);
    summaryResponse = getProductLevel3SummaryResponse();
    summaryResponse.setItemSku("TOQ-15120-00003-00001");
    productLevel3SummaryResponses.add(summaryResponse);
    return productLevel3SummaryResponses;
  }

  private void setUpRepositoriesForCampaign() {
    bulkUpdateServiceBean.getBulkUpdateServiceUtil()
        .setProductCategoryBaseRepository(this.productCategoryBaseRepository);
    bulkUpdateServiceBean.getBulkUpdateServiceUtil().setProductLevel3Repository(productLevel3Repository);
    bulkUpdateServiceBean.getBulkUpdateServiceUtil().setCampaignRepository(campaignRepository);
  }

  @Test
  public void testProcessCampaignProductBulkUpdate_Failure() throws Exception {
    GdnRestSingleResponse<ProductLevel3SummaryResponse> expectedResponse = getSucessResponse();
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 2);
    Map<String, String> files = this.getCampaignFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    bulkUpdateQueue.setRequestId("");
    ProfileResponse profileResponse = this.getProfileResponse();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse = this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode()
            + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(), bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(
        bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processCampaignProductBulkUpdate(bulkUpdateQueue);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void testProcessWithBusinessPartnerNull() throws Exception {
    productCampaignAvailabilityRequest.setItemSkus(new HashSet<>());
    productCampaignAvailabilityResponse.setProductCampaignAvailabilityMap(new HashMap<>());
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 2);
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse = this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
      ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(file);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(Mockito.any(), Mockito.any()))
      .thenReturn(sheet);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(null);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdate(bulkUpdateQueue);
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(anyString(), anyString(),
        eq(BulkProcess.STATUS_PENDING));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verifyNoInteractions(productLevel3Repository);
    Mockito.verify(trackerService)
        .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE), Mockito.eq(HYPHEN),
            Mockito.eq(TrackerConstants.FAILED), Mockito.anyString());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkUpdateHeaderCheck() throws Exception {
    productCampaignAvailabilityRequest.setItemSkus(new HashSet<>());
    productCampaignAvailabilityResponse.setProductCampaignAvailabilityMap(new HashMap<>());
    ReflectionTestUtils.setField(bulkUpdateServiceBean,"headerValidationCheck",true);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 2);
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse = this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
        ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(file);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(Mockito.any(), Mockito.any()))
        .thenReturn(sheet);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(null);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdate(bulkUpdateQueue);
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(anyString(), anyString(),
        eq(BulkProcess.STATUS_PENDING));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verifyNoMoreInteractions(productLevel3Repository);
    Mockito.verify(trackerService)
        .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE), Mockito.eq(HYPHEN),
            Mockito.eq(TrackerConstants.FAILED), Mockito.anyString());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @ParameterizedTest
  @ValueSource(strings = {"featureOnAndOMGFalse","featureOnAndOMGTrue","featureOffAndOMGTrue","featureOffAndOMGFalse"})
  public void processBulkUpdateHeaderCheckAsOMGSeller(String test) throws Exception {
    productCampaignAvailabilityRequest.setItemSkus(new HashSet<>());
    productCampaignAvailabilityResponse.setProductCampaignAvailabilityMap(new HashMap<>());
    ReflectionTestUtils.setField(bulkUpdateServiceBean,"headerValidationCheck",true);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 2);
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse = this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
        ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(file);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(Mockito.any(), Mockito.any()))
        .thenReturn(sheet);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    ProfileResponse profileResponse;
    if (test.equals("featureOnAndOMGTrue") || test.equals("featureOnAndOMGFalse")) {
      ReflectionTestUtils.setField(bulkUpdateServiceBean, "preOrderQuotaFeatureSwitch", true);
    }
    if (test.equals("featureOnAndOMGTrue") || test.equals("featureOffAndOMGTrue")) {
      profileResponse = ProfileResponse.builder().flags(Map.of(ProfileFlagNames.BLIBLI_OMG, true)).build();
    } else {
      profileResponse = null;
    }
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdate(bulkUpdateQueue);
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(anyString(), anyString(),
        eq(BulkProcess.STATUS_PENDING));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verifyNoMoreInteractions(productLevel3Repository);
    Mockito.verify(trackerService)
        .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE), Mockito.eq(HYPHEN),
            Mockito.eq(TrackerConstants.FAILED), Mockito.anyString());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  private Map<String, String> getFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("BulkUpdate" + File.separator + "BulkUpdate.xlsx"))), "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }


  private Map<String, String> getFile(String fileName) throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(
        Thread.currentThread().getContextClassLoader().getResourceAsStream("BulkUpdate" + File.separator + fileName))),
        "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getBulkUpdateAmphiFileFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()

        .getResourceAsStream("BulkUpdate" + File.separator + "BulkUpdateAmphi.xlsx"))), "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getCompleteSuccessFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("BulkUpdate" + File.separator + "BulkUpdateCompleteSuccess.xlsx"))), "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getNonMaxDiscountFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("BulkUpdate" + File.separator + "BulkUpdateNonMaxDiscount.xlsx"))), "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getCampaignBatchingFile() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("BulkUpdate" + File.separator + "BulkUpdateCampaignBatching.xlsx"))), "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getFileForPartialSuccess() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("BulkUpdate" + File.separator + "BulkUpdatePartialSuccess.xlsx"))), "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getCampaignFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("BulkUpdate" + File.separator + "BulkCampaignUpdate.xlsx"))), "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getNewCampaignFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("BulkUpdate" + File.separator + "NewCampaignUpload.xlsx"))), "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }

  @Test
  public void getterTest() throws Exception {
    CampaignRepository test = this.bulkUpdateServiceBean.getCampaignRepository();
  }

  @Test
  public void testPreProcessBulkUpdate_Exception() throws Exception {
    doNothing().when(this.trackerService)
        .sendTracker(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString());
    try {
      this.bulkUpdateServiceBean.preProcessBulkUpdate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, bulkUpdateProcessDTO,
          null);

    } catch (Exception e) {

    } finally {
      Mockito.verify(this.trackerService)
          .sendTracker(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString());
      verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    }
  }

  @Test
  public void setterTest() throws Exception {
    CampaignRepository c = null;
    this.bulkUpdateServiceBean.setCampaignRepository(c);
  }

  @Test
  public void preProcessBulkArchiveItemsTest() throws Exception {
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(new BulkProcess());
    when(bulkUpdateServiceUtil
        .getBulkProcess(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class), anyInt(), anyInt(),
            anyBoolean(), anyBoolean())).thenReturn(bulkProcess);
    when(objectMapper.writeValueAsString(any(BulkUpdateQueue.class))).thenReturn("Some Result String");
    bulkUpdateServiceBean.preProcessBulkArchiveItems("10001", DEFAULT_REQUEST_ID, bulkUpdateProcessDTO);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getBulkArchiveItems()), Mockito.any());
    verify(bulkProcessRepository).save(bulkProcessArgumentCaptor.capture());
    BulkProcess bulkProcess = bulkProcessArgumentCaptor.getValue();
    Assertions.assertEquals(DEFAULT_BP_CODE, bulkProcess.getBusinessPartnerCode());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkArchiveItems();
  }

  @Test
  public void preProcessBulkArchiveProductsTest() throws Exception {
    when(bulkUpdateServiceUtil.getBulkProcess(anyString(),anyString(),anyString(),any(BulkUpdateProcessDTO.class),anyInt(),anyInt(),anyBoolean(),anyBoolean())).thenReturn(bulkProcess);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(new BulkProcess());
    when(objectMapper.writeValueAsString(any(BulkUpdateQueue.class))).thenReturn("Some Result String");
    bulkUpdateServiceBean.preProcessBulkArchiveProducts("10001", DEFAULT_REQUEST_ID, bulkUpdateProcessDTO);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getBulkArchiveProducts()), Mockito.any());
    verify(bulkProcessRepository).save(bulkProcessArgumentCaptor.capture());
    BulkProcess bulkProcess = bulkProcessArgumentCaptor.getValue();
    Assertions.assertEquals(DEFAULT_BP_CODE, bulkProcess.getBusinessPartnerCode());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkArchiveProducts();
  }

  @Test
  public void preProcessBulkUpdateOff2OnTest() throws Exception {
    when(bulkUpdateServiceUtil
        .getBulkProcess(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class), anyInt(), anyInt(),
            anyBoolean(), anyBoolean())).thenReturn(bulkProcess);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(new BulkProcess());
    when(objectMapper.writeValueAsString(any(BulkUpdateQueue.class))).thenReturn("Some Result String");
    bulkUpdateServiceBean.preProcessBulkUpdateOff2On(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, bulkUpdateProcessDTO);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getBulkUpdateOff2On()), Mockito.any());
    verify(bulkProcessRepository).save(bulkProcessArgumentCaptor.capture());
    BulkProcess bulkProcess = bulkProcessArgumentCaptor.getValue();
    Assertions.assertEquals(DEFAULT_BP_CODE, bulkProcess.getBusinessPartnerCode());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkUpdateOff2On();
  }

  @Test
  public void preProcessBulkUpdateOff2OnExceptionTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    when(bulkUpdateServiceUtil
        .getBulkProcess(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class), anyInt(), anyInt(),
            anyBoolean(), anyBoolean())).thenReturn(bulkProcess);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(new BulkProcess());
    when(objectMapper.writeValueAsString(any(BulkUpdateQueue.class))).thenReturn("Some Result String");
    when(kafkaTopicProperties.getBulkUpdateOff2On()).thenReturn(EVENT);
    doThrow(ApplicationRuntimeException.class).when(kafkaProducer)
        .send(Mockito.eq(EVENT),
            Mockito.any());
    bulkUpdateServiceBean.preProcessBulkUpdateOff2On(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, bulkUpdateProcessDTO);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(EVENT),
            Mockito.any());
    verify(bulkProcessRepository).save(bulkProcessArgumentCaptor.capture());
    BulkProcess bulkProcess = bulkProcessArgumentCaptor.getValue();
    Assertions.assertEquals(DEFAULT_BP_CODE, bulkProcess.getBusinessPartnerCode());
    verify(trackerService).sendTracker(eq(PRODUCT_UPDATE_EVENT), eq(PRODUCT_UPDATE_ATTRI_TYPE), eq(HYPHEN), eq(FAILED),
        eq(DEFAULT_USERNAME));
    Mockito.verify(kafkaTopicProperties, times(1)).getBulkUpdateOff2On();
  }

  @Test
  public void preProcessBulkArchiveItemsTest_ExpectException() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    when(bulkUpdateServiceUtil
        .getBulkProcess(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class), anyInt(), anyInt(),
            anyBoolean(), anyBoolean())).thenReturn(bulkProcess);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenThrow(ApplicationRuntimeException.class);
    bulkUpdateServiceBean.preProcessBulkArchiveItems("10001", DEFAULT_REQUEST_ID, bulkUpdateProcessDTO);
    verify(bulkProcessRepository).save(bulkProcessArgumentCaptor.capture());
    BulkProcess bulkProcess = bulkProcessArgumentCaptor.getValue();
    Assertions.assertEquals(DEFAULT_BP_CODE, bulkProcess.getBusinessPartnerCode());
    verify(trackerService).sendTracker(eq(PRODUCT_UPDATE_EVENT), eq(PRODUCT_UPDATE_ATTRI_TYPE), eq(HYPHEN), eq(FAILED),
        eq(DEFAULT_USERNAME));
  }

  @Test
  public void preProcessBulkArchiveProductsExceptionTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    when(bulkUpdateServiceUtil
        .getBulkProcess(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class), anyInt(), anyInt(),
            anyBoolean(), anyBoolean())).thenReturn(bulkProcess);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenThrow(ApplicationRuntimeException.class);
    bulkUpdateServiceBean.preProcessBulkArchiveProducts("10001", DEFAULT_REQUEST_ID, bulkUpdateProcessDTO);
    verify(bulkProcessRepository).save(bulkProcessArgumentCaptor.capture());
    BulkProcess bulkProcess = bulkProcessArgumentCaptor.getValue();
    Assertions.assertEquals(DEFAULT_BP_CODE, bulkProcess.getBusinessPartnerCode());
    verify(trackerService).sendTracker(eq(PRODUCT_UPDATE_EVENT), eq(PRODUCT_UPDATE_ATTRI_TYPE), eq(HYPHEN), eq(FAILED),
        eq(DEFAULT_USERNAME));
  }

  @Test
  public void processBulkArchiveProductsTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    ProfileResponse profileResponse = getProfileResponse();
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    List<String> strings = new ArrayList<>();
    strings.add(DEFAULT_PRODUCT_SKU);
    strings.add(DEFAULT_PRODUCT_SKU_2);
    strings.add(DEFAULT_PRODUCT_SKU_3);
    simpleListStringRequest.setValue(strings);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    systemParameterConfig.setValue("100");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION)).thenReturn(bulkArchiveSwitch);
    bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE);
    verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(BulkProcessType.PRODUCT_LEVEL_3.getValue(),
        bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processAlreadyProcessedBulkArchiveProductsTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue));
    } finally {
      verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
          bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    }
  }

  @Test
  public void processAlreadyProcessedBulkUpdateOff2OnProductsTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceBean.processBulkUpdateOff2On(bulkUpdateQueue));
    } finally {
      verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
          DEFAULT_STORE_ID, bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    }
  }

  @Test
  public void processBulkArchiveProductsBatchTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    ProfileResponse profileResponse = getProfileResponse();
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    List<String> strings = new ArrayList<>();
    strings.add(DEFAULT_PRODUCT_SKU);
    strings.add(DEFAULT_PRODUCT_SKU_2);
    strings.add(DEFAULT_PRODUCT_SKU_3);
    simpleListStringRequest.setValue(strings);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    systemParameterConfig.setValue("1");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION)).thenReturn(bulkArchiveSwitch);
    bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(BulkProcessType.PRODUCT_LEVEL_3.getValue(),
        bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkArchiveProductsHeaderValidationSwitchOnSuccessTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "validateBulkArchiveHeadersEnabled", true);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    BulkUpdateServiceUtil newUtil = new BulkUpdateServiceUtil();
    ReflectionTestUtils.setField(newUtil, "validateBulkArchiveHeadersEnabled", true);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(newUtil);
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    ProfileResponse profileResponse = getProfileResponse();
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, bulkUpdateQueue.getBulkProcessCode(),
        BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    XSSFWorkbook workBook;
    InputStream is = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "BulkArchiveTemplate.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess)))
        .thenReturn(sheet);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    List<String> strings = new ArrayList<>();
    strings.add(DEFAULT_PRODUCT_SKU);
    strings.add(DEFAULT_PRODUCT_SKU_2);
    strings.add(DEFAULT_PRODUCT_SKU_3);
    simpleListStringRequest.setValue(strings);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    systemParameterConfig.setValue("1");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
            SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION)).thenReturn(bulkArchiveSwitch);
    bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE);
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
    Assertions.assertEquals(INVALID_HEADER_ARCHIVAL_ERROR_MESSAGE,
        bulkProcessArgumentCaptor.getValue().getDescription());
  }

  @Test
  public void processBulkArchiveProductsPartialTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    ProfileResponse profileResponse = getProfileResponse();
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    List<String> strings = new ArrayList<>();
    strings.add(DEFAULT_PRODUCT_SKU);
    strings.add(DEFAULT_PRODUCT_SKU_2);
    simpleListStringRequest.setValue(strings);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    systemParameterConfig.setValue("100");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION)).thenReturn(bulkArchiveSwitch);
    bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(BulkProcessType.PRODUCT_LEVEL_3.getValue(),
        bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkArchiveProductsAbortedTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    ProfileResponse profileResponse = getProfileResponse();
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    List<String> strings = new ArrayList<>();
    strings.add(DEFAULT_PRODUCT_SKU);
    strings.add(DEFAULT_PRODUCT_SKU_2);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    systemParameterConfig.setValue("100");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION)).thenReturn(bulkArchiveSwitch);
    bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(BulkProcessType.PRODUCT_LEVEL_3.getValue(),
        bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkArchiveProductsPartialSystemErrorTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    ProfileResponse profileResponse = getProfileResponse();
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.getCompany().setInternationalFlag(true);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    List<String> strings = new ArrayList<>();
    strings.add(PRODUCT_SKU);
    simpleListStringRequest.setValue(strings);
    systemParameterConfig.setValue("1");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE)).thenReturn(systemParameterConfig);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION)).thenReturn(bulkArchiveSwitch);
    bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(BulkProcessType.PRODUCT_LEVEL_3.getValue(),
        bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkArchiveProducts_maxRowsExceededTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    bulkArchiveSystemParameter.setValue(String.valueOf(1));
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    ProfileResponse profileResponse = getProfileResponse();
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.getCompany().setInternationalFlag(true);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
      bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    List<String> strings = new ArrayList<>();
    strings.add(PRODUCT_SKU);
    simpleListStringRequest.setValue(strings);
    systemParameterConfig.setValue("1");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE)).thenReturn(systemParameterConfig);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION)).thenReturn(bulkArchiveSwitch);
    bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(BulkProcessType.PRODUCT_LEVEL_3.getValue(),
      bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkArchiveProductsPartialSystemErrorInTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    ProfileResponse profileResponse = getProfileResponse();
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    List<String> strings = new ArrayList<>();
    strings.add(PRODUCT_SKU);
    simpleListStringRequest.setValue(strings);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    systemParameterConfig.setValue("1");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION)).thenReturn(bulkArchiveSwitch);
    bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(BulkProcessType.PRODUCT_LEVEL_3.getValue(),
        bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkArchiveProductsEmptyFileTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    ProfileResponse profileResponse = getProfileResponse();
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    Map<String, String> files = this.getFile(UPDATE_FILE_EMPTY);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode()
        + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION)).thenReturn(bulkArchiveSwitch);
    bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE), Mockito.eq(HYPHEN),
            Mockito.eq(TrackerConstants.FAILED), Mockito.anyString());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(BulkProcessType.PRODUCT_LEVEL_3.getValue(),
        bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkArchiveProductsWrongHeaderTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    ProfileResponse profileResponse = getProfileResponse();
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    Map<String, String> files = this.getFile(BULK_OFF2ON_HEADER_WRONG);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode()
        + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION)).thenReturn(bulkArchiveSwitch);
    bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE), Mockito.eq(HYPHEN),
            Mockito.eq(TrackerConstants.FAILED), Mockito.anyString());
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(BulkProcessType.PRODUCT_LEVEL_3.getValue(),
        bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkUpdateOff2OnTest() throws Exception {
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    ProfileResponse profileResponse = getProfileResponse();
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    systemParameterConfig.setValue("100");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE)).thenReturn(systemParameterConfig);
    bulkUpdateServiceBean.processBulkUpdateOff2On(bulkUpdateQueue);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH);
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE);
    verify(bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(Constant.INSTORE_BULK_PROCESS_TYPE, bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkUpdateOff2OnEnTest() throws Exception {
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    systemParameterConfig.setValue("100");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE)).thenReturn(systemParameterConfig);
    bulkUpdateServiceBean.processBulkUpdateOff2On(bulkUpdateQueue);
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(Constant.INSTORE_BULK_PROCESS_TYPE, bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkUpdateOff2OnEnMultipleSkusTest() throws Exception {
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    systemParameterConfig.setValue("100");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE)).thenReturn(systemParameterConfig);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateOff2On(bulkUpdateQueue);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH);
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
   // verify(bulkProcessService).bulkUpdateOff2On(stringBooleanMapCaptor.capture(), anyString(), anyString());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(Constant.INSTORE_BULK_PROCESS_TYPE, bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkUpdateOff2OnEnMultipleSkusBatchSize1Test() throws Exception {
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    XSSFWorkbook workBook;
    InputStream is = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    systemParameterConfig.setValue("1");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE)).thenReturn(systemParameterConfig);
    bulkUpdateServiceBean.processBulkUpdateOff2On(bulkUpdateQueue);
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED , bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(Constant.INSTORE_BULK_PROCESS_TYPE, bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkUpdateOff2OnWrongHeaderTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    Map<String, String> files = this.getFile(BULK_OFF2ON_HEADER_WRONG);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    systemParameterConfig.setValue("100");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE)).thenReturn(systemParameterConfig);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateOff2On(bulkUpdateQueue);
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    verify(trackerService).sendTracker(eq(PRODUCT_UPDATE_EVENT), eq(PRODUCT_UPDATE_ATTRI_TYPE), eq(HYPHEN), eq(FAILED),
        eq(DEFAULT_USERNAME));;
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(Constant.INSTORE_BULK_PROCESS_TYPE, bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkUpdateOff2OnEmptyFileTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    Map<String, String> files = this.getFile(UPDATE_FILE_EMPTY);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    systemParameterConfig.setValue("100");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE)).thenReturn(systemParameterConfig);
    bulkUpdateServiceBean.processBulkUpdateOff2On(bulkUpdateQueue);
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    verify(trackerService).sendTracker(eq(PRODUCT_UPDATE_EVENT), eq(PRODUCT_UPDATE_ATTRI_TYPE), eq(HYPHEN), eq(FAILED),
        eq(DEFAULT_USERNAME));
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(Constant.INSTORE_BULK_PROCESS_TYPE, bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkUpdateOff2OnWrongHeaderInTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    ProfileResponse profileResponse = getProfileResponse();
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    Map<String, String> files = this.getFile(BULK_OFF2ON_HEADER_WRONG);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    systemParameterConfig.setValue("100");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE)).thenReturn(systemParameterConfig);
    bulkUpdateServiceBean.processBulkUpdateOff2On(bulkUpdateQueue);
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    verify(trackerService).sendTracker(eq(PRODUCT_UPDATE_EVENT), eq(PRODUCT_UPDATE_ATTRI_TYPE), eq(HYPHEN), eq(FAILED),
        eq(DEFAULT_USERNAME));
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(Constant.INSTORE_BULK_PROCESS_TYPE, bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkUpdateOff2OnEnMultipleSkusBatchSize1UpdateFailedTest() throws Exception {
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    systemParameterConfig.setValue("1");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE)).thenReturn(systemParameterConfig);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    Map<String, Boolean> stringBooleanMap = new HashMap<>();
    stringBooleanMap.putIfAbsent(PRODUCT_SKU, true);
    bulkUpdateServiceBean.processBulkUpdateOff2On(bulkUpdateQueue);
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(Constant.INSTORE_BULK_PROCESS_TYPE, bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkUpdateOff2OnEnInputError() throws Exception {
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    systemParameterConfig.setValue("1");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Map<String, Boolean> stringBooleanMap = new HashMap<>();
    stringBooleanMap.putIfAbsent(PRODUCT_SKU, true);
    Mockito.when(bulkProcessService.bulkUpdateOff2On(eq(stringBooleanMap), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Collections.singletonList(PRODUCT_SKU));
    bulkUpdateServiceBean.processBulkUpdateOff2On(bulkUpdateQueue);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(Constant.INSTORE_BULK_PROCESS_TYPE, bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkUpdateOff2OnEnInputErrorAndPartialSuccess() throws Exception {
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    XSSFWorkbook workBook;
    InputStream is = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    systemParameterConfig.setValue("1");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE)).thenReturn(systemParameterConfig);
    Map<String, Boolean> stringBooleanMap = new HashMap<>();
    stringBooleanMap.putIfAbsent(PRODUCT_SKU, true);
    Mockito.when(bulkProcessService.bulkUpdateOff2On(eq(stringBooleanMap), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Collections.singletonList(PRODUCT_SKU));
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateOff2On(bulkUpdateQueue);
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPDATE_OFF2ON_BATCH_SIZE);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(Constant.INSTORE_BULK_PROCESS_TYPE, bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void getMinimumPriceTest() {
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Integer response = this.bulkUpdateServiceBean.getMinimumPrice(DEFAULT_STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constant.MINIMUM_PRICE);
  }

  @Test
  public void testGetHeaderListByProcessTypeForWorkOrderUploads_AssemblyRequest() {
    List<String> headers =
      bulkUpdateServiceBean.getHeaderListByProcessTypeForWorkOrderUploads(BulkProcessType.ASSEMBLY_REQUEST.getValue());
    assertNotNull(headers);
    Assertions.assertEquals(BulkWorkOrderConstants.ASSEMBLY_HEADERS.size(), headers.size());
    Assertions.assertTrue(headers.containsAll(BulkWorkOrderConstants.ASSEMBLY_HEADERS));
  }

  @Test
  public void testGetHeaderListByProcessTypeForWorkOrderUploads_DisassemblyRequest() {
    List<String> headers =
      bulkUpdateServiceBean.getHeaderListByProcessTypeForWorkOrderUploads(BulkProcessType.ASSEMBLY_REQUEST.getValue());
    assertNotNull(headers);
  }

  @Test
  public void testGetHeaderListByProcessTypeForWorkOrderUploads_TransferRequest() {
    List<String> headers =
      bulkUpdateServiceBean.getHeaderListByProcessTypeForWorkOrderUploads(BulkProcessType.DISASSEMBLY_REQUEST.getValue());
    assertNotNull(headers);
  }

  @Test
  public void testGetHeaderListByProcessTypeForWorkOrderUploads_Default() {
    List<String> headers =
      bulkUpdateServiceBean.getHeaderListByProcessTypeForWorkOrderUploads(BulkProcessType.TRANSFER_REQUEST.getValue());
    assertNotNull(headers);
    Assertions.assertEquals(BulkWorkOrderConstants.TRANSFER_REQUEST_HEADERS.size(), headers.size());
    Assertions.assertTrue(headers.containsAll(BulkWorkOrderConstants.TRANSFER_REQUEST_HEADERS));
  }

  @Test
  public void getMinimumPriceWithNullTest() {
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(null);
    Integer response = this.bulkUpdateServiceBean.getMinimumPrice(DEFAULT_STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constant.MINIMUM_PRICE);
    Assertions.assertNull(response);
  }

  @Test
  public void createWholesaleErrorWorkbookTest() throws Exception {
    BulkUpdateErrorDTO bulkUpdateErrorDTO = new BulkUpdateErrorDTO();
    bulkUpdateErrorDTO.setProductSku("DEFAULT_SKU_1");
    bulkUpdateErrorDTO.setReason("REASON");
    BulkUpdateSuccessDTO bulkUpdateSuccessDTO = new BulkUpdateSuccessDTO();
    bulkUpdateSuccessDTO.setSameThreshold(true);
    bulkUpdateSuccessDTO.setProductSku("DEFAULT_SKU_2");
    Mockito.when(bulkDownloadService.generateWholeSaleErrorWorkbookBulkDownload(stringArgumentCaptor.capture(),
        listStringArgumentCaptor.capture())).thenReturn("file_path");
    Method method = bulkUpdateServiceBean.getClass()
        .getDeclaredMethod("createWholesaleErrorWorkbook", String.class, List.class, List.class);
    method.setAccessible(true);
    String path = (String) method.invoke(bulkUpdateServiceBean, "REQUEST_ID", Arrays.asList(bulkUpdateErrorDTO),
        Arrays.asList(bulkUpdateSuccessDTO));
  }

  private void createVatUpdateFiles(String dirName) throws Exception {
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader().getResourceAsStream(dirName))),
        StandardCharsets.UTF_8);
    byte[] excelFile = Base64.decodeBase64(excelData);
    String directoryPath = ProcessorUtils.BULK_VAT_UPDATE_DIR + DEFAULT_BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath + File.separator + DEFAULT_BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    File file = new File(
      directoryPath + File.separator + DEFAULT_BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(file);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(Mockito.any(), Mockito.any()))
      .thenReturn(sheet);
  }


  @Test
  public void processBulkVatUpdateTest() throws Exception {
    createVatUpdateFiles(BULK_VAT_DIRECTORY_2);

    BatchVatUpdateResponse batchVatUpdateResponse =
        new BatchVatUpdateResponse("MTA-12345678-123457", "0", "No item found");

    bulkUpdateQueue.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    bulkUpdateQueue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkUpdateQueue.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkUpdateQueue.setRequestId(REQUEST_ID);
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    ItemCodeDetailResponse itemCodeDetailResponse =
        new ItemCodeDetailResponse(PRODUCT_SKU, BUSINESS_PARTNER_CODE, TD_MERCHANT, "MTA-12345678-12345");

    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue("false");
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(),
        Mockito.anyString())).thenReturn(systemParameterConfig);
    when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
            bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
            bulkProcess.getBusinessPartnerCode())).thenReturn(getProfileResponse());
    when(bulkUpdateServiceUtil.validateExcelFileForBulkVatUpdate(Mockito.any(Sheet.class), Mockito.any(BulkProcess.class),
        Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkUpdateErrorCounter.class))).thenReturn(true);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(bulkUpdateQueue.getStoreId(),
        SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE, "3",
            SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE));
    when(pcbOutboundService.batchVatFlagUpdate(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyMap())).thenReturn(new ArrayList<>());
    when(xProductOutboundService.getItemDetailsByItemCodes(Mockito.anyString(), Mockito.anyString(), Mockito.any(SimpleSetStringRequest.class))).thenReturn(Arrays.asList(itemCodeDetailResponse));
    when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);

    bulkUpdateServiceBean.processBulkVatUpdate(bulkUpdateQueue);

    verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
            bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,SystemParameterConfigNames.BULK_VAT_UPDATE_SWITCH);
    verify(bulkProcessRepository).save(bulkProcessArgumentCaptor.getAllValues().get(0));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
        bulkProcess.getBusinessPartnerCode());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(bulkUpdateQueue.getStoreId(),
        SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE);
    verify(bulkUpdateServiceUtil).validateExcelFileForBulkVatUpdate(Mockito.any(Sheet.class), Mockito.any(BulkProcess.class),
        Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkUpdateErrorCounter.class));
    verify(pcbOutboundService).batchVatFlagUpdate(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyMap());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.getAllValues().get(1));
    verify(xProductOutboundService).getItemDetailsByItemCodes(Mockito.anyString(), Mockito.anyString(), Mockito.any(SimpleSetStringRequest.class));

    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkVatUpdateSwitchOnTest() throws Exception {
    createVatUpdateFiles(BULK_VAT_DIRECTORY_2);

    BatchVatUpdateResponse batchVatUpdateResponse =
        new BatchVatUpdateResponse("MTA-12345678-123457", "0", "No item found");

    bulkUpdateQueue.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    bulkUpdateQueue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkUpdateQueue.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);

    ItemCodeDetailResponse itemCodeDetailResponse =
        new ItemCodeDetailResponse(PRODUCT_SKU, BUSINESS_PARTNER_CODE, TD_MERCHANT, "MTA-12345678-12345");

    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue("true");
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(),
        Mockito.anyString())).thenReturn(systemParameterConfig);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        bulkUpdateQueue.getStoreId(), bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(
        bulkProcess);
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
            bulkProcess.getBusinessPartnerCode())).thenReturn(getProfileResponse());
    when(bulkUpdateServiceUtil.validateExcelFileForBulkVatUpdate(Mockito.any(Sheet.class),
        Mockito.any(BulkProcess.class), Mockito.any(BulkUpdateQueue.class),
        Mockito.any(BulkUpdateErrorCounter.class))).thenReturn(true);
    when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkVatUpdate(bulkUpdateQueue);

    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        bulkUpdateQueue.getStoreId(), bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_VAT_UPDATE_SWITCH);
    verify(bulkProcessRepository).save(bulkProcessArgumentCaptor.getAllValues().get(0));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(
        bulkUpdateQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
    verify(bulkUpdateServiceUtil).validateExcelFileForBulkVatUpdate(Mockito.any(Sheet.class),
        Mockito.any(BulkProcess.class), Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkUpdateErrorCounter.class));
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.getAllValues().get(1));
    verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }


  @Test
  public void processBulkVatUpdateInternationalMerchantTrueTest() throws Exception {
    createVatUpdateFiles(BULK_VAT_DIRECTORY_1);

    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);

    BatchVatUpdateResponse batchVatUpdateResponse = new BatchVatUpdateResponse("MTA-12345678-12347", "0", "No item found");

    ItemCodeDetailResponse itemCodeDetailResponse1 =
        new ItemCodeDetailResponse(PRODUCT_SKU, BUSINESS_PARTNER_CODE, TD_MERCHANT, "MTA-12345678-12345");
    ItemCodeDetailResponse itemCodeDetailResponse2 =
        new ItemCodeDetailResponse(PRODUCT_SKU, BUSINESS_PARTNER_CODE, TD_MERCHANT, "MTA-12345678-12347");
    ItemCodeDetailResponse itemCodeDetailResponse3 =
        new ItemCodeDetailResponse(PRODUCT_SKU, BUSINESS_PARTNER_CODE, "CM", "MTA-12345678-12348");
    ItemCodeDetailResponse itemCodeDetailResponse4 =
        new ItemCodeDetailResponse(PRODUCT_SKU, "NEW_BP_CODE", TD_MERCHANT, "MTA-12345678-12349");

    bulkUpdateQueue.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    bulkUpdateQueue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkUpdateQueue.setRequestId(REQUEST_ID);
    bulkUpdateQueue.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setNotes("");

    when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
            bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue("false");
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(),
        Mockito.anyString())).thenReturn(systemParameterConfig);
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
            bulkProcess.getBusinessPartnerCode())).thenReturn(profileResponse);
    when(bulkUpdateServiceUtil.validateExcelFileForBulkVatUpdate(Mockito.any(Sheet.class), Mockito.any(BulkProcess.class),
        Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkUpdateErrorCounter.class))).thenReturn(true);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(bulkUpdateQueue.getStoreId(),
        SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE, "5",
            SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE));
    when(pcbOutboundService.batchVatFlagUpdate(Mockito.anyString(), Mockito.anyString(), Mockito.anyMap())).thenReturn(Arrays.asList(batchVatUpdateResponse));
    when(xProductOutboundService.getItemDetailsByItemCodes(Mockito.anyString(), Mockito.anyString(), Mockito.any(SimpleSetStringRequest.class))).thenReturn(
        Arrays.asList(itemCodeDetailResponse1, itemCodeDetailResponse2, itemCodeDetailResponse3,
            itemCodeDetailResponse4));
    when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);

    bulkUpdateServiceBean.processBulkVatUpdate(bulkUpdateQueue);

    verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
            bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_VAT_UPDATE_SWITCH);
    verify(bulkProcessRepository).save(bulkProcessArgumentCaptor.getAllValues().get(0));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
        bulkProcess.getBusinessPartnerCode());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(bulkUpdateQueue.getStoreId(),
        SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE);
    verify(bulkUpdateServiceUtil).validateExcelFileForBulkVatUpdate(Mockito.any(Sheet.class), Mockito.any(BulkProcess.class),
        Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkUpdateErrorCounter.class));
    verify(pcbOutboundService).batchVatFlagUpdate(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyMap());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.getAllValues().get(1));
    verify(xProductOutboundService).getItemDetailsByItemCodes(Mockito.anyString(), Mockito.anyString(), Mockito.any(SimpleSetStringRequest.class));

    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkVatUpdateShareddProductTest() throws Exception {
    createVatUpdateFiles(BULK_VAT_DIRECTORY_1);

    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);

    ItemCodeDetailResponse itemCodeDetailResponse1 =
        new ItemCodeDetailResponse(PRODUCT_SKU, BUSINESS_PARTNER_CODE, TD_MERCHANT, "MTA-12345678-12345");
    ItemCodeDetailResponse itemCodeDetailResponse2 =
        new ItemCodeDetailResponse(PRODUCT_SKU, BUSINESS_PARTNER_CODE, TD_MERCHANT, "MTA-12345678-12347");
    ItemCodeDetailResponse itemCodeDetailResponse3 =
        new ItemCodeDetailResponse(PRODUCT_SKU, BUSINESS_PARTNER_CODE, "CM", "MTA-12345678-12348");
    ItemCodeDetailResponse itemCodeDetailResponse4 =
        new ItemCodeDetailResponse(PRODUCT_SKU, "NEW_BP_CODE", TD_MERCHANT, "MTA-12345678-12349");
    ItemCodeDetailResponse itemCodeDetailResponse5 =
        new ItemCodeDetailResponse(PRODUCT_SKU, BUSINESS_PARTNER_CODE, "CM", "MTA-12345678-12349");
    ItemCodeDetailResponse itemCodeDetailResponse6 =
        new ItemCodeDetailResponse(PRODUCT_SKU, "NEW_BP_CODE", TD_MERCHANT, "MTA-12345678-12348");
    ItemCodeDetailResponse itemCodeDetailResponse7 =
        new ItemCodeDetailResponse(PRODUCT_SKU, "NEW_BP_CODE", TD_MERCHANT, "MTA-12345678-12347");
    ItemCodeDetailResponse itemCodeDetailResponse8 =
        new ItemCodeDetailResponse(PRODUCT_SKU, BUSINESS_PARTNER_CODE, "CM", "MTA-12345678-12345");

    bulkUpdateQueue.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    bulkUpdateQueue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkUpdateQueue.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkUpdateQueue.setRequestId(REQUEST_ID);
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setNotes("");

    when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
            bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue("false");
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(),
        Mockito.anyString())).thenReturn(systemParameterConfig);
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
            bulkProcess.getBusinessPartnerCode())).thenReturn(profileResponse);
    when(bulkUpdateServiceUtil.validateExcelFileForBulkVatUpdate(Mockito.any(Sheet.class), Mockito.any(BulkProcess.class),
        Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkUpdateErrorCounter.class))).thenReturn(true);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(bulkUpdateQueue.getStoreId(),
        SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE, "5",
            SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE));
    when(pcbOutboundService.batchVatFlagUpdate(Mockito.anyString(), Mockito.anyString(), Mockito.anyMap())).thenReturn(new ArrayList<>());
    when(xProductOutboundService.getItemDetailsByItemCodes(Mockito.anyString(), Mockito.anyString(), Mockito.any(SimpleSetStringRequest.class))).thenReturn(
        Arrays.asList(itemCodeDetailResponse1, itemCodeDetailResponse2, itemCodeDetailResponse3, itemCodeDetailResponse4,
            itemCodeDetailResponse5, itemCodeDetailResponse6, itemCodeDetailResponse7, itemCodeDetailResponse8));
    when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);

    bulkUpdateServiceBean.processBulkVatUpdate(bulkUpdateQueue);

    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(bulkProcessRepository).save(bulkProcessArgumentCaptor.getAllValues().get(0));
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_VAT_UPDATE_SWITCH);
    verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
            bulkProcess.getBusinessPartnerCode());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(bulkUpdateQueue.getStoreId(),
        SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE);
    verify(bulkUpdateServiceUtil).validateExcelFileForBulkVatUpdate(Mockito.any(Sheet.class), Mockito.any(BulkProcess.class),
        Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkUpdateErrorCounter.class));
    verify(pcbOutboundService).batchVatFlagUpdate(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyMap());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.getAllValues().get(1));
    verify(xProductOutboundService).getItemDetailsByItemCodes(Mockito.anyString(), Mockito.anyString(), Mockito.any(SimpleSetStringRequest.class));

    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkVatUpdateNullPCBResponseTest() throws Exception {
    createVatUpdateFiles(BULK_VAT_DIRECTORY_1);

    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);

    BatchVatUpdateResponse batchVatUpdateResponse = new BatchVatUpdateResponse("MTA-12345678-123457", "0", "No item found");

    ItemCodeDetailResponse itemCodeDetailResponse =
        new ItemCodeDetailResponse(PRODUCT_SKU, BUSINESS_PARTNER_CODE, TD_MERCHANT, "MTA-12345678-12345");

    bulkUpdateQueue.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    bulkUpdateQueue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkUpdateQueue.setRequestId(REQUEST_ID);
    bulkUpdateQueue.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setNotes("");

    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue("false");
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(),
        Mockito.anyString())).thenReturn(systemParameterConfig);
    when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
            bulkProcess.getBusinessPartnerCode())).thenReturn(profileResponse);
    when(bulkUpdateServiceUtil.validateExcelFileForBulkVatUpdate(Mockito.any(Sheet.class), Mockito.any(BulkProcess.class),
        Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkUpdateErrorCounter.class))).thenReturn(true);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(bulkUpdateQueue.getStoreId(),
        SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE, "5",
            SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE));
    when(pcbOutboundService.batchVatFlagUpdate(Mockito.anyString(), Mockito.anyString(), Mockito.anyMap())).thenReturn(null);
    when(xProductOutboundService.getItemDetailsByItemCodes(Mockito.anyString(), Mockito.anyString(), Mockito.any(SimpleSetStringRequest.class))).thenReturn(Arrays.asList(itemCodeDetailResponse));
    when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);

    bulkUpdateServiceBean.processBulkVatUpdate(bulkUpdateQueue);

    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(bulkProcessRepository).save(bulkProcessArgumentCaptor.getAllValues().get(0));
    verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
            bulkProcess.getBusinessPartnerCode());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_VAT_UPDATE_SWITCH);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(bulkUpdateQueue.getStoreId(),
        SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE);
    verify(bulkUpdateServiceUtil).validateExcelFileForBulkVatUpdate(Mockito.any(Sheet.class), Mockito.any(BulkProcess.class),
        Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkUpdateErrorCounter.class));
    verify(pcbOutboundService).batchVatFlagUpdate(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyMap());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.getAllValues().get(1));
    verify(xProductOutboundService).getItemDetailsByItemCodes(Mockito.anyString(), Mockito.anyString(), Mockito.any(SimpleSetStringRequest.class));

    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkVatUpdateNullXproductResponseTest() throws Exception {
    createVatUpdateFiles(BULK_VAT_DIRECTORY_1);

    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);

    BatchVatUpdateResponse batchVatUpdateResponse = new BatchVatUpdateResponse("MTA-12345678-123457", "0", "No item found");

    bulkUpdateQueue.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    bulkUpdateQueue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkUpdateQueue.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);

    when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
            bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue("false");
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(),
        Mockito.anyString())).thenReturn(systemParameterConfig);
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
            bulkProcess.getBusinessPartnerCode())).thenReturn(profileResponse);
    when(bulkUpdateServiceUtil.validateExcelFileForBulkVatUpdate(Mockito.any(Sheet.class), Mockito.any(BulkProcess.class),
        Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkUpdateErrorCounter.class))).thenReturn(true);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(bulkUpdateQueue.getStoreId(),
        SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE, "5",
            SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE));
    when(xProductOutboundService.getItemDetailsByItemCodes(Mockito.anyString(), Mockito.anyString(), Mockito.any(SimpleSetStringRequest.class))).thenReturn(new ArrayList<>());
    when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);

    bulkUpdateServiceBean.processBulkVatUpdate(bulkUpdateQueue);

    verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
            bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_VAT_UPDATE_SWITCH);
    verify(bulkProcessRepository).save(bulkProcessArgumentCaptor.getAllValues().get(0));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
        bulkProcess.getBusinessPartnerCode());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(bulkUpdateQueue.getStoreId(),
        SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE);
    verify(bulkUpdateServiceUtil).validateExcelFileForBulkVatUpdate(Mockito.any(Sheet.class), Mockito.any(BulkProcess.class),
        Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkUpdateErrorCounter.class));
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.getAllValues().get(1));
    verify(xProductOutboundService).getItemDetailsByItemCodes(Mockito.isNull(), Mockito.anyString(), Mockito.any(SimpleSetStringRequest.class));
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkVatUpdateWrongHeadersTest() throws Exception {
    createVatUpdateFiles(BULK_VAT_DIRECTORY_1);

    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);

    BatchVatUpdateResponse batchVatUpdateResponse = new BatchVatUpdateResponse("MTA-12345678-123457", "0", "No item found");

    bulkUpdateQueue.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    bulkUpdateQueue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkUpdateQueue.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);

    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
            bulkProcess.getBusinessPartnerCode())).thenReturn(profileResponse);
    when(bulkUpdateServiceUtil.validateExcelFileForBulkVatUpdate(Mockito.any(Sheet.class), Mockito.any(BulkProcess.class),
        Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkUpdateErrorCounter.class))).thenReturn(false);
    when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);

    bulkUpdateServiceBean.processBulkVatUpdate(bulkUpdateQueue);

    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(bulkProcessRepository).save(bulkProcessArgumentCaptor.getAllValues().get(0));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
        bulkProcess.getBusinessPartnerCode());
    verify(bulkUpdateServiceUtil).validateExcelFileForBulkVatUpdate(Mockito.any(Sheet.class), Mockito.any(BulkProcess.class),
        Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkUpdateErrorCounter.class));
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.getAllValues().get(1));
    Mockito.verify(trackerService)
        .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE), Mockito.eq(HYPHEN),
            Mockito.eq(TrackerConstants.FAILED), Mockito.anyString());

    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkVatUpdateInprogressStateTest() throws Exception {
    createVatUpdateFiles(BULK_VAT_DIRECTORY_1);

    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);

    BatchVatUpdateResponse batchVatUpdateResponse = new BatchVatUpdateResponse("MTA-12345678-123457", "0", "No item found");

    bulkUpdateQueue.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    bulkUpdateQueue.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkUpdateQueue.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);

    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(null);

    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceBean.processBulkVatUpdate(bulkUpdateQueue));
    } finally {
      verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
          bulkUpdateQueue.getStoreId(), bulkUpdateQueue.getBulkProcessCode(),
          BulkProcess.STATUS_PENDING);
      BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    }
  }

  private LinkedHashMap<String, String> getInputRowData(String fileName) throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + fileName);
    TypeReference<LinkedHashMap<String, String>> typeRef = new TypeReference<LinkedHashMap<String, String>>() {
    };
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, typeRef);
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOn() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    itemPickupPointListingL3ResponseList.add(getItemPickupPointL3Response());
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOn_withAccessiblePickupPoints() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    bulkProcess.setNotes(NOTES);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(this.objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class)).thenReturn(auditTrailInfo);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    itemPickupPointListingL3ResponseList.add(getItemPickupPointL3Response());
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateCheckForBfbFieldsChangedTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "checkForBulkUpdateChangeEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    bulkProcess.setNotes(NOTES);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    LinkedHashMap<String, String> rowInputData = getInputRowData("updateInputMppSwitchOn");
    rowInputData.put(BulkParameters.CNC_STATUS_HEADER, "0");
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        rowInputData);
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(this.objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class)).thenReturn(auditTrailInfo);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setMerchantSku("1");
    itemPickupPointListingL3Response.getPrices().get(0).setPrice(60000.67);
    itemPickupPointListingL3Response.getPrices().get(0).setSalePrice(3000.0);
    ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse = new ProductLevel3ViewConfigResponse();
    productLevel3ViewConfigResponse.setChannelId(Constant.DEFAULT_CHANNEL);
    productLevel3ViewConfigResponse.setDisplay(false);
    productLevel3ViewConfigResponse.setBuyable(false);
    itemPickupPointListingL3Response.setViewConfigs(List.of(productLevel3ViewConfigResponse));
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOn_withAccessiblePickupPoints_exception() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    bulkProcess.setNotes(NOTES);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    doThrow(JsonProcessingException.class).when(this.objectMapper).readValue(bulkProcess.getNotes(), AuditTrailInfo.class);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    itemPickupPointListingL3ResponseList.add(getItemPickupPointL3Response());
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOnWithFreeSampleONTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
      BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
        .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
      getInputRowData("updateInputMppSwitchOn6"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
      bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("mppSwitchOnItemUpdateShipping.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setWebSyncStock(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFreeSample(Boolean.TRUE);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
      + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
      bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
      anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOnWithFreeSampleOffTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "instoreNewFlowEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
      BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
        .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
      getInputRowData("updateInputMppSwitchOn4"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_2), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_2);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
      bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setFreeSample(Boolean.TRUE);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode("PP-3000214");
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
      + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
      bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
      anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
      .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
          eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
      new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOnDimensionMissingTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bopisCategoryRestrictionEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bopisCategoryValidationForSellerTypes", "CM");
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn4"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_2), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_2);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setDimensionsMissing(true);
    itemPickupPointListingL3Response.setMissingFields(Set.of(Constant.DIMENSIONS_MISSING));
    itemPickupPointListingL3Response.setFreeSample(false);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode("PP-3000214");
    profileResponse.getPickupPoints().add(pickupPointDTO);
    profileResponse.getCompany().setMerchantType("CM");
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOnInStoreTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "instoreNewFlowEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
      BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
        .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    LinkedHashMap<String, String> rowInputData = getInputRowData("updateInputMppSwitchOn4");
    rowInputData.put(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, "1");
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
      getInputRowData("updateInputMppSwitchOn4"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_2), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_2);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
      bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setMissingFields(Set.of(Constant.DIMENSIONS_MISSING));
    itemPickupPointListingL3Response.setFreeSample(false);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode("PP-3000214");
    profileResponse.getPickupPoints().add(pickupPointDTO);
    profileResponse.getCompany().setMerchantType("CM");
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
      + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
      Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
      bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
      anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
      .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
      .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
        eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
      new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOnInStoreCNCActiveTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "instoreNewFlowEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
      BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
        .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    LinkedHashMap<String, String> rowInputData = getInputRowData("updateInputMppSwitchOn4");
    rowInputData.put(BulkParameters.CNC_STATUS_HEADER, "1");
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(rowInputData);
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_2), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_2);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
      bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setMissingFields(Set.of(Constant.DESCRIPTION_MISSING));
    itemPickupPointListingL3Response.setFreeSample(false);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode("PP-3000214");
    profileResponse.getPickupPoints().add(pickupPointDTO);
    profileResponse.getCompany().setMerchantType("CM");
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
      + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
      Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
      bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
      anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
      .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
      .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
        eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
      new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }


  @Test
  public void processBulkUpdateItemMPPSwitchOnBopisCncTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bopisCncRestrictionEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn6"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("mppSwitchOnItemUpdateShipping.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setWebSyncStock(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setProductType(3);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOnWithFreeSampleOnTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn7"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_2), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_4);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setFreeSample(Boolean.TRUE);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOnWithFreeSampleOffShippingIssueTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn4"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_2), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_4);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        getItemPickupPointL3Response();
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOnWithFreeSampleOffShippingFineTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn8"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_2), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_4);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        getItemPickupPointL3Response();
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateCheckForBulkUpdateChangeEnabledTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "checkForBulkUpdateChangeEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn8"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_2), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_4);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        getItemPickupPointL3Response();
    itemPickupPointListingL3Response.getViewConfigs().get(0).setChannelId(Constant.DEFAULT_CHANNEL);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(bulkDownloadServiceBeanUtil.getSkuStatusValue(anyBoolean(), anyBoolean(), anyBoolean())).thenReturn(
        BulkParameters.SKU_STATUS_OFFLINE);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateCheckForBulkUpdateChangeEnabledAmphiUserTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "checkForBulkUpdateChangeEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn8"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_2), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_5);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        getItemPickupPointL3Response();
    itemPickupPointListingL3Response.getViewConfigs().get(0).setChannelId(Constant.DEFAULT_CHANNEL);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(bulkDownloadServiceBeanUtil.getSkuStatusValue(anyBoolean(), anyBoolean(), anyBoolean())).thenReturn(
        BulkParameters.SKU_STATUS_OFFLINE);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }


  @Test
  public void processBulkUpdateNotAbleToEditStockItemMPPSwitchOn() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn4"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_2), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_2);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    itemPickupPointListingL3ResponseList.add(getItemPickupPointL3Response());
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateNotAbleToEditPickupPointItemMPPSwitchOn() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn5"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_3), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_3);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    itemPickupPointListingL3ResponseList.add(getItemPickupPointL3Response());
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateAbleToEditStockItemMPPSwitchOn() throws Exception {
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    PreOrderDTO preOrderDTO = new PreOrderDTO(false,null,null,null);
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn3"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_1), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_1);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setWebSyncStock(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.setStoreId(DEFAULT_STORE_ID);
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    Mockito.when(xProductOutboundService.getBasicProductInfo(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(basicProductResponse);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateExternalUserWrongStatus() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn3"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_1), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_4);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setWebSyncStock(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateItemPBPFailedValidationMPPSwitchOn() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    itemPickupPointListingL3ResponseList.add(getItemPickupPointL3Response());
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    VariantsErrorListResponse variantsErrorListResponse = new VariantsErrorListResponse();
    variantsErrorListResponse.setItemSku(GDN_SKU_1);
    variantsErrorListResponse.setPickupPointCode("PP-3000213");
    variantsErrorListResponse.setMessage(StringUtils.EMPTY);
    itemsPriceStockImagesUpdateResponse.getVariantsErrorList().add(variantsErrorListResponse);
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateItemSwitchOnToRemoveFailedData() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "removeFailedDataFromPassedData", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    itemPickupPointListingL3ResponseList.add(getItemPickupPointL3Response());
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    VariantsErrorListResponse variantsErrorListResponse = new VariantsErrorListResponse();
    variantsErrorListResponse.setItemSku(GDN_SKU_1);
    variantsErrorListResponse.setPickupPointCode("PP-3000213");
    variantsErrorListResponse.setMessage(StringUtils.EMPTY);
    itemsPriceStockImagesUpdateResponse.getVariantsErrorList().add(variantsErrorListResponse);
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
        .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
            eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateItemInvalidBusinessCodeMPPSwitchOn() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE_1)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setWebSyncStock(true);
    itemPickupPointListingL3Response.setFreeSample(Boolean.TRUE);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
        new HashSet<>());
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOnNoAccessToEdit() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn3"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_1), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_1);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    PickupPointDTO pickupPoint = new PickupPointDTO();
    pickupPoint.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPoint);
    profileResponse.getCompany().setCncActivated(true);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
   }

  @Test
  public void processBulkUpdateItemMPPSwitchOnNoAccessToEditStockTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn4"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_2), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_2);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    PickupPointDTO pickupPoint = new PickupPointDTO();
    pickupPoint.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPoint);
    profileResponse.getCompany().setCncActivated(true);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOnNoAccessToPickupPointTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn5"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_3), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_3);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    PickupPointDTO pickupPoint = new PickupPointDTO();
    pickupPoint.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPoint);
    profileResponse.getCompany().setCncActivated(true);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkUpdateItemInvalidCncStatusMPPSwitchOn() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputInvalidStatusMppSwitchOn"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    itemPickupPointListingL3ResponseList.add(getItemPickupPointL3Response());
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    BulkProcess bulkProcess = this.getBulkProcess();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(trackerService,Mockito.times(1))
        .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE), Mockito.eq(HYPHEN),
            Mockito.eq(TrackerConstants.FAILED), Mockito.anyString());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOnException() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData1.setBulkRequestData(BULK_DATA_1);
    bulkProcessData1.setParentProduct(DEFAULT_GDN_SKU);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData1, bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1, 2)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn"));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    BulkProcess bulkProcess = this.getBulkProcess();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(trackerService)
        .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE), Mockito.eq(HYPHEN),
            Mockito.eq(TrackerConstants.FAILED), Mockito.anyString());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(4)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(2)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOnException3() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData1.setBulkRequestData(BULK_DATA_1);
    bulkProcessData1.setParentProduct(DEFAULT_GDN_SKU);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData1, bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1, 2)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn"));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    BulkProcess bulkProcess = this.getBulkProcess();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(trackerService)
        .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE), Mockito.eq(HYPHEN),
            Mockito.eq(TrackerConstants.FAILED), Mockito.anyString());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(4)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(2)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkUpdateItem_exception() throws Exception {
    bulkProcessData.setBulkRequestData(BULK_DATA);
    bulkProcessData.setParentProduct(DEFAULT_GDN_SKU2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1, 2)).storeId(Constant.STORE_ID).build();
    doThrow(JsonProcessingException.class).when(objectMapper).readValue(eq(BULK_DATA), any(TypeReference.class));
    GdnRestSingleResponse<ProductLevel3SummaryResponse> expectedResponse = getSucessResponse();
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING)).thenReturn(
        Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkProcess bulkProcess = this.getBulkProcess();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(this.bulkProcessDataService, times(1)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(1)).readValue(anyString(), any(TypeReference.class));
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Mockito.verify(trackerService)
        .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE), Mockito.eq(HYPHEN),
            Mockito.eq(TrackerConstants.FAILED), Mockito.anyString());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
  }

  @Test
  public void saveBulkProcessDataDuplicateSkuTest() throws Exception {
    Map<String, String> dataMap1 = new HashMap<>();
    dataMap1.put(BulkParameters.BLIBLI_SKU, BLIBLI_SKU);
    productDataMap.add(dataMap1);
    Map<String, String> dataMap2 = new HashMap<>();
    dataMap2.put(BulkParameters.BLIBLI_SKU, BLIBLI_SKU);
    productDataMap.add(dataMap2);
    bulkUpdateServiceBean.saveBulkProcessData(bulkProcess, productDataMap);
    Mockito.verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());
    Mockito.verify(bulkUpdateServiceUtil)
        .getFinalDataList(Mockito.anyMap(), Mockito.anyList(), Mockito.eq(true), Mockito.anyBoolean());
    Assertions.assertEquals("Update", bulkProcessArgumentCaptor.getValue().getBulkProcessType());
  }

  @Test
  public void saveBulkProcessDataEmptySkuTest() throws Exception {
    Map<String, String> dataMap1 = new HashMap<>();
    dataMap1.put(BulkParameters.BLIBLI_SKU, "");
    productDataMap.add(dataMap1);
    bulkUpdateServiceBean.saveBulkProcessData(bulkProcess, productDataMap);
    Mockito.verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());
    Mockito.verify(bulkUpdateServiceUtil)
        .getFinalDataList(Mockito.anyMap(), Mockito.anyList(), Mockito.eq(true), Mockito.anyBoolean());
  }

  @Test
  public void testProcessCampaignProductBulkUpdate_WhenSwitchIsTrue() throws Exception {
    setBulkUpdateServiceUtil();
    GdnRestSingleResponse<ProductLevel3SummaryResponse> expectedResponse = getSucessResponse();
    systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue("true");
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CAMPAIGN_UPLOAD_SWITCH)).thenReturn(systemParameterConfig);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 2);
    Map<String, String> files = this.getNewCampaignFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse = this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
      ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    byte[] byteArray = FileUtils.readFileToByteArray(file);
    when(fileStorageServiceBean.downloadFile(Mockito.any(), Mockito.anyString()))
      .thenReturn(byteArray);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processCampaignProductBulkUpdate(bulkUpdateQueue);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.CAMPAIGN_UPLOAD_SWITCH);
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void setFinalStatusAndNotificationOnUpdateTest() throws Exception {
    productCampaignAvailabilityRequest.getItemInfo().iterator().next().setPickUpPointCode(PICKUP_POINT_CODE);
    BulkUpdateErrorDTO bulkUpdateErrorDTO =
        new BulkUpdateErrorDTO("Tes Product", "TOA-14961-00118-00001", "Harga Normal harus lebih besar dari 1");
    BulkUpdateSuccessDTO bulkUpdateSuccessDTO =
        new BulkUpdateSuccessDTO("Tes Product", "TOA-14961-00117-00006", PICKUP_POINT_CODE, false
          , false, null, null, null);
    bulkProcessData.setErrorMessage(BULK_DATA);
    bulkProcessData.setParentProduct(DEFAULT_GDN_SKU2);
    bulkProcessData.setInputErrorCount(1);
    bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData1.setErrorMessage(BULK_DATA);
    bulkProcessData1.setParentProduct(DEFAULT_GDN_SKU_3);
    bulkProcessData1.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setNotes(BULK_DATA_2);
    bulkProcessData2.setParentProduct(DEFAULT_GDN_SKU);
    bulkProcessData2.setStatus(BulkProcessData.STATUS_SUCCESS);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(bulkUpdateErrorDTO);
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(bulkUpdateSuccessDTO);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess)).thenReturn(
        bulkProcessDataList);
    when(this.xCampaignFeign.getProductCampaign(DEFAULT_STORE_ID, X_BULK_CLIENT, X_BULK_CLIENT, DEFAULT_REQUEST_ID,
        X_BULK_CLIENT, productCampaignAvailabilityRequest)).thenReturn(
        new GdnRestSingleResponse<>(null, null,
            false, productCampaignAvailabilityResponse, bulkUpdateQueue.getRequestId()));
    Mockito.when(bulkDownloadService.generateWholeSaleErrorWorkbookBulkDownload(stringArgumentCaptor.capture(),
        listStringArgumentCaptor.capture())).thenReturn("file_path");
    setBulkUpdateServiceUtil();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setInternationalMerchant(false);
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnUpdate(bulkProcess, DEFAULT_STORE_ID);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndStatusAndIdentifier(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void setFinalStatusAndNotificationOnUpdateMppTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    productCampaignAvailabilityRequest.getItemInfo().iterator().next().setPickUpPointCode(PICKUP_POINT_CODE);
    BulkUpdateErrorDTO bulkUpdateErrorDTO =
        new BulkUpdateErrorDTO("Tes Product", "TOA-14961-00118-00001", "Harga Normal harus lebih besar dari 1");
    BulkUpdateSuccessDTO bulkUpdateSuccessDTO =
        new BulkUpdateSuccessDTO("Tes Product", "TOA-14961-00117-00006", PICKUP_POINT_CODE, false
          , false, null, null, null);
    bulkProcessData.setErrorMessage(BULK_DATA);
    bulkProcessData.setParentProduct(DEFAULT_GDN_SKU2);
    bulkProcessData.setInputErrorCount(1);
    bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData1.setErrorMessage(BULK_DATA);
    bulkProcessData1.setParentProduct(DEFAULT_GDN_SKU_3);
    bulkProcessData1.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setNotes(BULK_DATA_2);
    bulkProcessData2.setParentProduct(DEFAULT_GDN_SKU);
    bulkProcessData2.setStatus(BulkProcessData.STATUS_SUCCESS);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(bulkUpdateErrorDTO);
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(bulkUpdateSuccessDTO);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess)).thenReturn(
        bulkProcessDataList);
    when(this.xCampaignFeign.getProductCampaignV2(DEFAULT_STORE_ID, X_BULK_CLIENT, X_BULK_CLIENT, DEFAULT_REQUEST_ID,
        X_BULK_CLIENT, productCampaignAvailabilityRequest)).thenReturn(
        new GdnRestSingleResponse<>(null, null,
            false, productCampaignAvailabilityResponse, bulkUpdateQueue.getRequestId()));
    Mockito.when(bulkDownloadService.generateWholeSaleErrorWorkbookBulkDownload(stringArgumentCaptor.capture(),
        listStringArgumentCaptor.capture())).thenReturn("file_path");
    setBulkUpdateServiceUtil();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setInternationalMerchant(false);
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnUpdate(bulkProcess, DEFAULT_STORE_ID);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndStatusAndIdentifier(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void setFinalStatusAndNotificationOnUpdatePricingMppTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "pricingMultiPickupPointEnabled", true);
    productCampaignAvailabilityRequest.getItemInfo().iterator().next().setPickUpPointCode(PICKUP_POINT_CODE);
    BulkUpdateErrorDTO bulkUpdateErrorDTO =
        new BulkUpdateErrorDTO("Tes Product", "TOA-14961-00118-00001", "Harga Normal harus lebih besar dari 1");
    BulkUpdateSuccessDTO bulkUpdateSuccessDTO =
        new BulkUpdateSuccessDTO("Tes Product", "TOA-14961-00117-00006", PICKUP_POINT_CODE, false
          , false, null, null, null);
    bulkProcessData.setErrorMessage(BULK_DATA);
    bulkProcessData.setParentProduct(DEFAULT_GDN_SKU2);
    bulkProcessData.setInputErrorCount(1);
    bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData1.setErrorMessage(BULK_DATA);
    bulkProcessData1.setParentProduct(DEFAULT_GDN_SKU_3);
    bulkProcessData1.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setNotes(BULK_DATA_2);
    bulkProcessData2.setParentProduct(DEFAULT_GDN_SKU);
    bulkProcessData2.setStatus(BulkProcessData.STATUS_SUCCESS);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(bulkUpdateErrorDTO);
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(bulkUpdateSuccessDTO);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess)).thenReturn(
        bulkProcessDataList);
    ProductCampaignAvailabilityInfoDto productCampaignAvailabilityInfoDto = new ProductCampaignAvailabilityInfoDto();
    productCampaignAvailabilityInfoDto.setItemSku(DEFAULT_GDN_SKU);
    productCampaignAvailabilityInfoDto.setAvailability(false);
    productCampaignAvailabilityResponse.setProductCampaignAvailabilityInfo(
        Collections.singletonList(productCampaignAvailabilityInfoDto));
    when(this.xCampaignFeign.getProductCampaignV2(DEFAULT_STORE_ID, X_BULK_CLIENT, X_BULK_CLIENT, DEFAULT_REQUEST_ID,
        X_BULK_CLIENT, productCampaignAvailabilityRequest)).thenReturn(
        new GdnRestSingleResponse<>(null, null,
            false, productCampaignAvailabilityResponse, bulkUpdateQueue.getRequestId()));
    Mockito.when(bulkDownloadService.generateWholeSaleErrorWorkbookBulkDownload(stringArgumentCaptor.capture(),
        listStringArgumentCaptor.capture())).thenReturn("file_path");
    setBulkUpdateServiceUtil();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setInternationalMerchant(false);
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnUpdate(bulkProcess, DEFAULT_STORE_ID);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndStatusAndIdentifier(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void setFinalStatusAndNotificationOnUpdateTest_WholeSaleCountNull() throws Exception {
    productCampaignAvailabilityRequest.getItemInfo().iterator().next().setPickUpPointCode(PICKUP_POINT_CODE);
    BulkUpdateErrorDTO bulkUpdateErrorDTO =
        new BulkUpdateErrorDTO("Tes Product", "TOA-14961-00118-00001", "Harga Normal harus lebih besar dari 1");
    BulkUpdateSuccessDTO bulkUpdateSuccessDTO =
        new BulkUpdateSuccessDTO("Tes Product", "TOA-14961-00117-00006", PICKUP_POINT_CODE, false
          , false, false, null, null);
    bulkProcessData2.setNotes(BULK_DATA_2);
    bulkProcessData2.setParentProduct(DEFAULT_GDN_SKU);
    bulkProcessData2.setStatus(BulkProcessData.STATUS_SUCCESS);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(bulkUpdateErrorDTO);
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(bulkUpdateSuccessDTO);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess)).thenReturn(
        bulkProcessDataList);
    when(this.xCampaignFeign.getProductCampaign(DEFAULT_STORE_ID, X_BULK_CLIENT, X_BULK_CLIENT, DEFAULT_REQUEST_ID,
        X_BULK_CLIENT, productCampaignAvailabilityRequest)).thenReturn(
        new GdnRestSingleResponse<>(null, null,
            false, productCampaignAvailabilityResponse, bulkUpdateQueue.getRequestId()));
    setBulkUpdateServiceUtil();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setInternationalMerchant(false);
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnUpdate(bulkProcess, DEFAULT_STORE_ID);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndStatusAndIdentifier(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void setFinalStatusAndNotificationOnUpdateTest_NoSuccess() throws Exception {
    BulkUpdateErrorDTO bulkUpdateErrorDTO =
        new BulkUpdateErrorDTO("Tes Product", "TOA-14961-00118-00001", "Harga Normal harus lebih besar dari 1");
    bulkProcessData.setErrorMessage(BULK_DATA);
    bulkProcessData.setParentProduct(DEFAULT_GDN_SKU2);
    bulkProcessData.setInputErrorCount(1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData, bulkProcessData1);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(bulkUpdateErrorDTO);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess)).thenReturn(
        bulkProcessDataList);
    setBulkUpdateServiceUtil();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setInternationalMerchant(false);
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnUpdate(bulkProcess, DEFAULT_STORE_ID);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndStatusAndIdentifier(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void setFinalStatusAndNotificationOnUpdateExceptionTest() throws Exception {
    productCampaignAvailabilityRequest.getItemInfo().iterator().next().setPickUpPointCode(PICKUP_POINT_CODE);
    BulkUpdateErrorDTO bulkUpdateErrorDTO =
        new BulkUpdateErrorDTO("Tes Product", "TOA-14961-00118-00001", "Harga Normal harus lebih besar dari 1");
    BulkUpdateSuccessDTO bulkUpdateSuccessDTO =
        new BulkUpdateSuccessDTO("Tes Product", "TOA-14961-00117-00006", PICKUP_POINT_CODE, false
          , false, null, null, null);
    bulkProcessData.setErrorMessage(BULK_DATA);
    bulkProcessData.setParentProduct(DEFAULT_GDN_SKU2);
    bulkProcessData.setInputErrorCount(1);
    bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData1.setErrorMessage(BULK_DATA);
    bulkProcessData1.setParentProduct(DEFAULT_GDN_SKU_3);
    bulkProcessData1.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setNotes(BULK_DATA_2);
    bulkProcessData2.setParentProduct(DEFAULT_GDN_SKU);
    bulkProcessData2.setStatus(BulkProcessData.STATUS_SUCCESS);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(bulkUpdateErrorDTO);
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(bulkUpdateSuccessDTO);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess)).thenReturn(
        bulkProcessDataList);
    setBulkUpdateServiceUtil();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setInternationalMerchant(false);
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnUpdate(bulkProcess, DEFAULT_STORE_ID);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndStatusAndIdentifier(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void setFinalStatusAndNotificationOnCampaignUpload_NoSuccessTest() throws Exception {
    BulkUpdateErrorDTO bulkUpdateErrorDTO =
        new BulkUpdateErrorDTO("Tes Product", "TOA-14961-00118-00001", PICKUP_POINT_CODE,
            "Harga Normal harus lebih besar dari 1");
    Map<String, String> map = new LinkedHashMap<>();
    map.put("Blibli SKU", "TOQ-15120-00001-00001");
    map.put("Harga Akhir", "40000.0");
    map.put("Harga Jual", "50000");
    map.put("Harga Normal", "50000");
    map.put("Kuota", "2.0");
    map.put("Nama Produk", "Batik Testing AIFA M Brick Red");
    map.put("Persentase Diskon", "0.2");
    map.put("Potongan Harga", "10000");
    map.put("RowNumber", "6");
    map.put("Stok Tersedia", "5");
    bulkProcessData.setErrorMessage(BULK_DATA);
    bulkProcessData.setParentProduct(DEFAULT_GDN_SKU2);
    bulkProcessData.setInputErrorCount(1);
    bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData.setBulkRequestData("Test Data");
    bulkProcessData.setRowNumber(1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData, bulkProcessData1);
    List<String> campaignHeaderList = new ArrayList<>();
    campaignHeaderList.add(BulkParameters.BLIBLI_SKU);
    campaignHeaderList.add(BulkParameters.PICKUP_POINT_CODE);
    campaignHeaderList.add(BulkParameters.PRODUCT_NAME);
    when(bulkCampaignProductProcessHelper.getHeaderList(new BulkDataResponse())).thenReturn(campaignHeaderList);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(bulkUpdateErrorDTO);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(BulkAddCampaignProductQueue.class))).thenReturn(
        new BulkAddCampaignProductQueue());
    when(objectMapper.readValue(eq("Test Data"), any(TypeReference.class))).thenReturn(map);

    setBulkUpdateServiceUtil();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setNotes("");
    bulkProcess.setTotalCount(1);
    bulkProcess.setInternationalMerchant(false);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
      .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess)).thenReturn(
        bulkProcessDataList);
    Mockito.when(bulkProductProcessHelper.callSuperGenerateDataSheet(Mockito.anyList(), Mockito.anyList(), Mockito.eq(0)))
        .thenReturn(workBook);
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnCampaignUpload(bulkProcess, DEFAULT_STORE_ID);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(BulkAddCampaignProductQueue.class));
    verify(bulkCampaignProductProcessHelper).getHeaderList(new BulkDataResponse());
  }

  @Test
  public void setFinalStatusAndNotificationOnCampaignUpload_ForHargaRecommendationChangesTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean,"pricingCampaignRecommendationEnabled",true);
    BulkUpdateErrorDTO bulkUpdateErrorDTO =
            new BulkUpdateErrorDTO("Tes Product", "TOA-14961-00118-00001", PICKUP_POINT_CODE,
                    "Harga Normal harus lebih besar dari 1");
    Map<String, String> map = new LinkedHashMap<>();
    map.put("Blibli SKU", "TOQ-15120-00001-00001");
    map.put("Harga Akhir", "40000.0");
    map.put("Harga Jual", "50000");
    map.put("Harga Normal", "50000");
    map.put("Kuota", "2.0");
    map.put("Nama Produk", "Batik Testing AIFA M Brick Red");
    map.put("Persentase Diskon", "0.2");
    map.put("Potongan Harga", "10000");
    map.put("RowNumber", "6");
    map.put("Stok Tersedia", "5");
    map.put("Harga Rekomendasi","true");
    bulkProcessData.setErrorMessage(BULK_DATA);
    bulkProcessData.setParentProduct(DEFAULT_GDN_SKU2);
    bulkProcessData.setInputErrorCount(1);
    bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData.setBulkRequestData("Test Data");
    bulkProcessData.setRowNumber(1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData, bulkProcessData1);
    List<String> campaignHeaderList = new ArrayList<>();
    campaignHeaderList.add(BulkParameters.BLIBLI_SKU);
    campaignHeaderList.add(BulkParameters.PICKUP_POINT_CODE);
    campaignHeaderList.add(BulkParameters.PRODUCT_NAME);
    when(bulkCampaignProductProcessHelper.getHeaderList(new BulkDataResponse())).thenReturn(campaignHeaderList);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(bulkUpdateErrorDTO);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(BulkAddCampaignProductQueue.class))).thenReturn(
            new BulkAddCampaignProductQueue());
    when(objectMapper.readValue(eq("Test Data"), any(TypeReference.class))).thenReturn(map);

    setBulkUpdateServiceUtil();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setNotes("");
    bulkProcess.setTotalCount(1);
    bulkProcess.setInternationalMerchant(false);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
            .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess)).thenReturn(
            bulkProcessDataList);
    Mockito.when(bulkProductProcessHelper.callSuperGenerateDataSheet(Mockito.anyList(), Mockito.anyList(), Mockito.eq(0)))
        .thenReturn(workBook);
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnCampaignUpload(bulkProcess, DEFAULT_STORE_ID);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(BulkAddCampaignProductQueue.class));
    verify(bulkCampaignProductProcessHelper).getHeaderList(new BulkDataResponse());
  }

  @Test
  public void processEventEmptyDataTest() throws Exception {
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1, 2)).storeId(Constant.STORE_ID).build();
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING)).thenReturn(null);
    setBulkUpdateServiceUtil();
    bulkUpdateServiceBean.processEvent(bulkUpdateEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
  }

  @Test
  public void processEventInvalidItemSkuTest() throws Exception {
    BulkProcessData data = new BulkProcessData();
    Map<String, String> map = new LinkedHashMap<>();
    map.put("Blibli SKU", "TOQ-15120-00001");
    map.put("Harga Akhir", "40000.0");
    map.put("Harga Jual", "50000");
    map.put("Harga Normal", "50000");
    map.put("Kuota", "2.0");
    map.put("Nama Produk", "Batik Testing AIFA M Brick Red");
    map.put("Persentase Diskon", "0.2");
    map.put("Potongan Harga", "10000");
    map.put("RowNumber", "6");
    map.put("Stok Tersedia", "5");
    data.setBulkRequestData(new ObjectMapper().writeValueAsString(map));
    data.setRowNumber(6);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(data);
    bulkProcess.setNotes("");
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1, 2)).storeId(Constant.STORE_ID).build();
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    when(objectMapper.readValue(Mockito.anyString(), any(TypeReference.class))).thenReturn(map);
    bulkUpdateServiceBean.processEvent(bulkUpdateEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE,
            Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
  }

  @Test
  public void processEventTest() throws Exception {
    BulkProcessData data = new BulkProcessData();
    Map<String, String> map = new LinkedHashMap<>();
    map.put("Blibli SKU", "TOQ-15120-00001-00001");
    map.put("Harga Akhir", "40000.0");
    map.put("Harga Jual", "50000");
    map.put("Harga Normal", "50000");
    map.put("Kuota", "2.0");
    map.put("Nama Produk", "Batik Testing AIFA M Brick Red");
    map.put("Persentase Diskon", "0.2");
    map.put("Potongan Harga", "10000");
    map.put("RowNumber", "6");
    map.put("Stok Tersedia", "5");
    data.setBulkRequestData(new ObjectMapper().writeValueAsString(map));
    data.setRowNumber(6);
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    bulkUpdateQueue.getCampaignItemSummaryRequest().setCategories(Arrays.asList(CATEGORY_CODE, CATEGORY_CODE1));
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setNotes("");
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = getProductLevel3ReponsesForCampaign();
    productLevel3SummaryResponses.get(0).getPrices().get(0).setPrice(6000.0);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(data);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1, 2)).storeId(Constant.STORE_ID).build();
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    when(objectMapper.readValue(Mockito.anyString(), any(TypeReference.class))).thenReturn(map);
    when(objectMapper.readValue(Mockito.anyString(), eq(BulkAddCampaignProductQueue.class))).thenReturn(bulkUpdateQueue);
    when(productCategoryBaseRepository.getAllChildCategoriesFromC1CategoryCode(anyString(), anyString(),
        any())).thenReturn(Collections.emptyList());
    when(bulkProcessDataService.saveOperation(Mockito.any())).thenReturn(data);
    bulkUpdateServiceBean.processEvent(bulkUpdateEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE,
            Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    Mockito.verify(bulkProcessDataService).saveOperation(Mockito.any());
  }

  @Test
  public void processEventPrecisionSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "campaignFinalPricePrecisionEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "campaignFinalPricePrecision", 0);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "campaignFinalPriceRoundingMode", 6);
    BulkProcessData data = new BulkProcessData();
    Map<String, String> map = new LinkedHashMap<>();
    map.put("Blibli SKU", "TOQ-15120-00001-00001");
    map.put("Harga Akhir", "40000.0");
    map.put("Harga Jual", "50000");
    map.put("Harga Normal", "50000");
    map.put("Kuota", "2.0");
    map.put("Nama Produk", "Batik Testing AIFA M Brick Red");
    map.put("Persentase Diskon", "0.2");
    map.put("Potongan Harga", "10000");
    map.put("RowNumber", "6");
    map.put("Stok Tersedia", "5");
    data.setBulkRequestData(new ObjectMapper().writeValueAsString(map));
    data.setRowNumber(6);
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    bulkUpdateQueue.getCampaignItemSummaryRequest().setCategories(Arrays.asList(CATEGORY_CODE, CATEGORY_CODE1));
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setNotes("");
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = getProductLevel3ReponsesForCampaign();
    productLevel3SummaryResponses.get(0).getPrices().get(0).setPrice(6000.0);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(data);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1, 2)).storeId(Constant.STORE_ID).build();
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    when(objectMapper.readValue(Mockito.anyString(), any(TypeReference.class))).thenReturn(map);
    when(objectMapper.readValue(Mockito.anyString(), eq(BulkAddCampaignProductQueue.class))).thenReturn(bulkUpdateQueue);
    when(productCategoryBaseRepository.getAllChildCategoriesFromC1CategoryCode(anyString(), anyString(),
        any())).thenReturn(Collections.emptyList());
    when(bulkProcessDataService.saveOperation(Mockito.any())).thenReturn(data);
    bulkUpdateServiceBean.processEvent(bulkUpdateEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE,
            Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    Mockito.verify(bulkProcessDataService).saveOperation(Mockito.any());
  }

  @Test
  public void generateBulkProcessDataTest() throws Exception {
    bulkProcess.setInternationalMerchant(true);
    systemParameterConfig.setValue(MAX_PRODUCT_SIZE);
    Mockito.when(systemParameterConfigService
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE))
      .thenReturn(systemParameterConfig);
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE, "-1",
            SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE, "-1",
            SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE));
    this.bulkUpdateServiceBean.generateBulkProcessData(bulkProcess, Arrays.asList(getInputRowData("updateInput")),
        privilegedMap,getProfileResponse());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    verify(objectMapper, times(2)).writeValueAsString(any());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(anyList());
    Mockito.verify(bulkProcessService).saveOperation(any());
  }

  @Test
  public void generateBulkProcessDataNonTrustedAndUserInputRowsLTRegularsellerMaxCountTest() throws Exception {
    bulkProcess.setInternationalMerchant(true);
    bulkProcess.setBulkProcessType("ProductLevel");
    systemParameterConfig.setValue(MAX_PRODUCT_SIZE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "-1",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "-1",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "-1",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    this.bulkUpdateServiceBean
        .generateBulkProcessData(bulkProcess, Arrays.asList(getInputRowData("updateInput")), privilegedMap,
            getProfileResponse());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    verify(objectMapper, times(2)).writeValueAsString(any());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(anyList());
    Mockito.verify(bulkProcessService).saveOperation(any());
  }

  @Test
  public void generateBulkProcessDataNonTrustedAndUserInputRowsLTRegularsellerMaxCountTest1() throws Exception {
    bulkProcess.setInternationalMerchant(true);
    systemParameterConfig.setValue(MAX_PRODUCT_SIZE);
    bulkProcess.setBulkProcessType("ProductLevel3");
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE, "-1",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE, "-1",
            SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE, "-1",
            SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    this.bulkUpdateServiceBean
        .generateBulkProcessData(bulkProcess, Arrays.asList(getInputRowData("updateInput")), privilegedMap,
            getProfileResponse());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    verify(objectMapper, times(2)).writeValueAsString(any());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(anyList());
    Mockito.verify(bulkProcessService).saveOperation(any());
  }

  @Test
  public void generateBulkProcessDatawithUpdatePriorityQueueEnabledTestAndTrustedSellerFalse() throws Exception {
    GdnRestSingleResponse<ProfileResponse> gdnRestSingleResponse = new GdnRestSingleResponse<>();
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(true);
    gdnRestSingleResponse.setValue(profileResponse);
    bulkProcess.setInternationalMerchant(true);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    systemParameterConfig.setValue(MAX_PRODUCT_SIZE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    this.bulkUpdateServiceBean
        .generateBulkProcessData(bulkProcess, Arrays.asList(getInputRowData("updateInput")), privilegedMap,
            getProfileResponse());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    verify(objectMapper, times(2)).writeValueAsString(any());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(anyList());
    Mockito.verify(bulkProcessService).saveOperation(any());
  }

  @Test
  public void generateBulkProcessDatawithUpdatePriorityQueueEnabledTestAndTrustedSellerTrue() throws Exception {
    GdnRestSingleResponse<ProfileResponse> gdnRestSingleResponse = new GdnRestSingleResponse<>();
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(true);
    gdnRestSingleResponse.setValue(profileResponse);
    bulkProcess.setInternationalMerchant(true);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    systemParameterConfig.setValue(MAX_PRODUCT_SIZE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    this.bulkUpdateServiceBean
        .generateBulkProcessData(bulkProcess, Arrays.asList(getInputRowData("updateInput")), privilegedMap,
            getProfileResponse());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    verify(objectMapper, times(2)).writeValueAsString(any());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(anyList());
    Mockito.verify(bulkProcessService).saveOperation(any());
  }

  @Test
  public void generateBulkProcessDatawithUpdatePriorityQueueEnabledTestAndTrustedSellerTrueAndUserInputRowsLtTrustedSellerRows()
      throws Exception {
    GdnRestSingleResponse<ProfileResponse> gdnRestSingleResponse = new GdnRestSingleResponse<>();
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(true);
    gdnRestSingleResponse.setValue(profileResponse);
    bulkProcess.setInternationalMerchant(true);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    systemParameterConfig.setValue(MAX_PRODUCT_SIZE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "-1",
            SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "-1",
            SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    this.bulkUpdateServiceBean
        .generateBulkProcessData(bulkProcess, Arrays.asList(getInputRowData("updateInput")), privilegedMap,
            getProfileResponse());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    verify(objectMapper, times(2)).writeValueAsString(any());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(anyList());
    Mockito.verify(bulkProcessService).saveOperation(any());
  }


  @Test
  public void generateBulkProcessDatawithUpdatePriorityQueueEnabledTestAndTrustedSellerTrueAndUserInputRowsLTRegularSellerRows()
      throws Exception {
    GdnRestSingleResponse<ProfileResponse> gdnRestSingleResponse = new GdnRestSingleResponse<>();
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(false);
    gdnRestSingleResponse.setValue(profileResponse);
    bulkProcess.setInternationalMerchant(true);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    systemParameterConfig.setValue(MAX_PRODUCT_SIZE);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "-1",
            SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "1000",
            SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "-1",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    this.bulkUpdateServiceBean
        .generateBulkProcessData(bulkProcess, Arrays.asList(getInputRowData("updateInput")), privilegedMap,
            getProfileResponse());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    verify(objectMapper, times(2)).writeValueAsString(any());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(anyList());
    Mockito.verify(bulkProcessService).saveOperation(any());
  }

  @Test
  public void processBulkUpdateOff2OnNewFlowTest() throws Exception {
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH, "true",
            SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH));
    doNothing().when(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());

    bulkUpdateServiceBean.processBulkUpdateOff2On(bulkUpdateQueue);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH);
    verify(bulkProcessRepository, times(2)).save(bulkProcessArgumentCaptor.getValue());
    verify(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.getValue());

    Assertions.assertEquals(BulkProcess.STATUS_READY_TO_PROCESS, bulkProcessArgumentCaptor.getAllValues().get(1).getStatus());
    Assertions.assertEquals(BulkProcess.STATUS_FAIL, bulkProcessDataArgumentCaptor.getValue().get(0).getStatus());
  }

  @Test
  public void processBulkUpdateOff2OnWrongSellerTest() throws Exception {
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    bulkProcess.setBusinessPartnerCode("bp");
    bulkProcess.setBulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH, "true",
            SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH));
    doNothing().when(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());

    bulkUpdateServiceBean.processBulkUpdateOff2On(bulkUpdateQueue);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH);
    verify(bulkProcessRepository, times(2)).save(bulkProcessArgumentCaptor.getValue());
    verify(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.getValue());

    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessDataArgumentCaptor.getValue().get(0).getStatus());
  }

  @Test
  public void processBulkUpdateOff2OnNewFlowEmptyTest() throws Exception {
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setBulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    doNothing().when(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH, "true",
            SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH));

    bulkUpdateServiceBean.processBulkUpdateOff2On(bulkUpdateQueue);
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.INSTORE_BULK_UPDATE_SWITCH);
    verify(bulkProcessRepository, times(2)).save(bulkProcessArgumentCaptor.getValue());

    Assertions.assertEquals(BulkProcess.STATUS_READY_TO_PROCESS, bulkProcessArgumentCaptor.getAllValues().get(1).getStatus());
  }

  @Test
  public void setFinalStatusAndNotificationInstoreUpload() throws Exception {
    Map<String, String> files = this.getFile(OFF_2_ON_EMPTY_DATA_SHEET);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    bulkProcess.setBulkProcessCode(bulkUpdateQueue.getBulkProcessCode());
    bulkProcess.setTotalCount(4);
    bulkProcess.setInternationalMerchant(true);
    bulkProcess.setDescription(BULK_OFF2ON_UPDATE_FILE);
    bulkProcess.setBulkProcessType(BulkProcessType.IN_STORE.getValue());
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkProcess.class))).thenReturn(sheet);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess)).thenReturn(
        getBulkProcessDataForInstoreUpdate());
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnInstoreUpload(bulkProcess, DEFAULT_STORE_ID);
    Mockito.verify(fileStorageServiceBean).getFileData(Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkProcess.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
  }

  private List<BulkProcessData> getBulkProcessDataForInstoreUpdate() {
    String json1 = "{\"productSku\":\"productSku1\",\"off2OnFlag\":\"1.0\"}";
    String json2 = "{\"productSku\":\"productSku2\",\"off2OnFlag\":\"0.0\"}";
    String json3 = "{\"off2OnFlag\":\"1\"}";
    String json4 = "{\"productSku\":\"productSku3\",\"off2OnFlag\":\"3\"}";
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setBulkRequestData(json1);
    bulkProcessData1.setStatus(BulkProcessData.STATUS_SUCCESS);
    bulkProcessData1.setRowNumber(0);
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setBulkRequestData(json2);
    bulkProcessData2.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setSystemErrorCount(1);
    bulkProcessData2.setRowNumber(0);
    BulkProcessData bulkProcessData3 = new BulkProcessData();
    bulkProcessData3.setBulkRequestData(json3);
    BulkProcessData bulkProcessData4 = new BulkProcessData();
    bulkProcessData3.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData3.setInputErrorCount(1);
    bulkProcessData3.setRowNumber(1);
    bulkProcessData4.setBulkRequestData(json4);
    return Arrays.asList(bulkProcessData1, bulkProcessData2, bulkProcessData3, bulkProcessData4);
  }

  @Test
  public void processBulkArchiveProducts_newImplSwitchOnTest() throws Exception {
    bulkArchiveSwitch.setValue(String.valueOf(Boolean.TRUE));
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    ProfileResponse profileResponse = getProfileResponse();
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest(Arrays.asList(DEFAULT_PRODUCT_SKU, DEFAULT_PRODUCT_SKU_2, DEFAULT_PRODUCT_SKU_3));
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION)).thenReturn(bulkArchiveSwitch);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(
      anyString(), anyString())).thenReturn(profileResponse);
    bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(this.bulkArchiveService).addProcessDataAndUpdateProcess(eq(bulkUpdateQueue.getStoreId()), Mockito.any(Sheet.class), eq(bulkProcess));
    verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(
      anyString(), anyString());
  }

  @Test
  public void processBulkArchiveProducts_newImplSwitchOn_maxRowsExceededTest() throws Exception {
    bulkArchiveSwitch.setValue(String.valueOf(Boolean.TRUE));
    bulkArchiveSystemParameter.setValue(String.valueOf(1));
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    ProfileResponse profileResponse = getProfileResponse();
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
      bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode()
      + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest(Arrays.asList(DEFAULT_PRODUCT_SKU, DEFAULT_PRODUCT_SKU_2, DEFAULT_PRODUCT_SKU_3));
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION)).thenReturn(bulkArchiveSwitch);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(
      anyString(), anyString())).thenReturn(profileResponse);
    bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue);
    verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE);
    Mockito.verify(trackerService)
      .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE), Mockito.eq(HYPHEN),
        Mockito.eq(TrackerConstants.FAILED), Mockito.anyString());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(
      anyString(), anyString());
  }

  @Test
  public void processBulkArchiveProducts_newImplEmptyFileTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkArchiveSwitch.setValue(String.valueOf(Boolean.TRUE));
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    ProfileResponse profileResponse = getProfileResponse();
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    Map<String, String> files = this.getFile(UPDATE_FILE_EMPTY);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode()
        + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    when(bulkProcessService.saveOperation(any(BulkProcess.class))).thenReturn(null);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION)).thenReturn(bulkArchiveSwitch);
    bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE), Mockito.eq(HYPHEN),
            Mockito.eq(TrackerConstants.FAILED), Mockito.anyString());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(BulkProcessType.PRODUCT_LEVEL_3.getValue(),
        bulkProcessArgumentCaptor.getValue().getBulkProcessType());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getStartDate());
    assertNotNull(bulkProcessArgumentCaptor.getValue().getEndDate());
  }

  @Test
  public void processBulkArchiveProducts_newImplSwitchOn_errorOnRowDataTest() throws Exception {
    bulkArchiveSwitch.setValue(String.valueOf(Boolean.TRUE));
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    bulkUpdateServiceBean.setBulkUpdateServiceUtil(new BulkUpdateServiceUtil());
    BulkUpdateQueue bulkUpdateQueue = getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkUpdateQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(eq(bulkUpdateQueue), eq(bulkProcess))).thenReturn(sheet);
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest(Arrays.asList(DEFAULT_PRODUCT_SKU, DEFAULT_PRODUCT_SKU_2, DEFAULT_PRODUCT_SKU_3));
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(
      anyString(), anyString())).thenReturn(profileResponse);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION)).thenReturn(bulkArchiveSwitch);
    bulkUpdateServiceBean.processBulkArchiveProducts(bulkUpdateQueue);
    Mockito.verify(fileStorageServiceBean).getFileData(eq(bulkUpdateQueue), eq(bulkProcess));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(
      anyString(), anyString());
    verify(this.bulkArchiveService).addProcessDataAndUpdateProcess(eq(bulkUpdateQueue.getStoreId()),
      Mockito.any(Sheet.class), eq(bulkProcess));
    verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_ARCHIVE_IMPLEMENTATION);
    verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE);
  }

  @Test
  public void processEventExceptionTest() throws Exception {
    BulkProcessData data = new BulkProcessData();
    Map<String, String> map = new LinkedHashMap<>();
    map.put("Blibli SKU", "TOQ-15120-00001-00001");
    map.put("Harga Akhir", "40000.0");
    map.put("Harga Jual", "50000");
    map.put("Harga Normal", "50000");
    map.put("Kuota", "2.0");
    map.put("Nama Produk", "Batik Testing AIFA M Brick Red");
    map.put("Persentase Diskon", "0.2");
    map.put("Potongan Harga", "10000");
    map.put("RowNumber", "6");
    map.put("Stok Tersedia", "5");
    data.setBulkRequestData(new ObjectMapper().writeValueAsString(map));
    data.setRowNumber(6);
    setBulkUpdateServiceUtil();
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    bulkUpdateQueue.getCampaignItemSummaryRequest().setCategories(Arrays.asList(CATEGORY_CODE, CATEGORY_CODE1));
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setNotes("");
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = getProductLevel3ReponsesForCampaign();
    productLevel3SummaryResponses.get(0).getPrices().get(0).setPrice(6000.0);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(data);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1, 2)).storeId(Constant.STORE_ID).build();
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(objectMapper.readValue(Mockito.anyString(), any(TypeReference.class))).thenReturn(map);
    Mockito.doThrow(JsonProcessingException.class).when(objectMapper).readValue(Mockito.anyString(), eq(BulkAddCampaignProductQueue.class));
    when(bulkProcessDataService.saveOperation(Mockito.any())).thenReturn(data);
    bulkUpdateServiceBean.processEvent(bulkUpdateEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE,
            Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any());
  }

  @Test
  public void processEventExceptionTest1() throws Exception {
    BulkProcessData data = new BulkProcessData();
    Map<String, String> map = new LinkedHashMap<>();
    map.put("Blibli SKU", "TOQ-15120-00001-00001");
    map.put("Harga Akhir", "40000.0");
    map.put("Harga Jual", "50000");
    map.put("Harga Normal", "50000");
    map.put("Kuota", "2.0");
    map.put("Nama Produk", "Batik Testing AIFA M Brick Red");
    map.put("Persentase Diskon", "0.2");
    map.put("Potongan Harga", "10000");
    map.put("RowNumber", "6");
    map.put("Stok Tersedia", "5");
    data.setBulkRequestData(new ObjectMapper().writeValueAsString(map));
    data.setRowNumber(6);
    setBulkUpdateServiceUtil();
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    bulkUpdateQueue.getCampaignItemSummaryRequest().setCategories(Arrays.asList(CATEGORY_CODE, CATEGORY_CODE1));
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setNotes("");
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = getProductLevel3ReponsesForCampaign();
    productLevel3SummaryResponses.get(0).getPrices().get(0).setPrice(6000.0);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(data);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1, 2)).storeId(Constant.STORE_ID).build();
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(objectMapper.readValue(Mockito.anyString(), any(TypeReference.class))).thenReturn(map);
    Mockito.doThrow(JsonProcessingException.class).when(objectMapper).readValue(Mockito.anyString(), eq(BulkAddCampaignProductQueue.class));
    Mockito.doThrow(JsonProcessingException.class).when(objectMapper).writeValueAsString(Mockito.anyString());
    when(bulkProcessDataService.saveOperation(Mockito.any())).thenReturn(data);
    bulkUpdateServiceBean.processEvent(bulkUpdateEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE,
            Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any());
  }

//  @Test
//  public void setFinalStatusAndNotificationOnVatBulkUpdateTest() throws Exception {
//    createVatUpdateFiles(BULK_VAT_DIRECTORY_1);
//    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
//    bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
//    bulkProcess.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
//    bulkUpdateServiceBean.setFinalStatusAndNotificationOnVatBulkUpdate(bulkProcess, DEFAULT_STORE_ID);
//  }

  @Test
  public void setFinalStatusAndNotificationOnVatBulkUpdateTest2() throws Exception {
    createVatUpdateFiles(BULK_VAT_DIRECTORY_1);
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    bulkProcess.setInternationalMerchant(true);
    bulkProcess.setTotalCount(5);
    bulkProcess.setSuccessCount(4);
    bulkProcess.setErrorCount(1);
    VatUpdateDto vatUpdateDto = new VatUpdateDto("MTA-12345678-12345", "1");
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setStatus(BulkProcessData.STATUS_SUCCESS);
    bulkProcessData1.setBulkRequestData("");
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setErrorMessage("Error Message");
    bulkProcessData2.setBulkRequestData("");

    when(bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess)).thenReturn(
        Arrays.asList(bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(Mockito.anyString(), eq(VatUpdateDto.class))).thenReturn(vatUpdateDto);

    bulkUpdateServiceBean.setFinalStatusAndNotificationOnVatBulkUpdate(bulkProcess, DEFAULT_STORE_ID);

    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
  }

  @Test
  public void setFinalStatusAndNotificationOnVatBulkUpdateTest3() throws Exception {
    createVatUpdateFiles(BULK_VAT_DIRECTORY_1);
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    bulkProcess.setInternationalMerchant(false);
    bulkProcess.setTotalCount(5);
    bulkProcess.setSuccessCount(4);
    bulkProcess.setErrorCount(1);
    VatUpdateDto vatUpdateDto = new VatUpdateDto("MTA-12345678-12345", "1");
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setStatus(BulkProcessData.STATUS_SUCCESS);
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setErrorMessage("Error Message");
    bulkProcessData2.setBulkRequestData("");

    when(bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess)).thenReturn(
        Arrays.asList(bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(Mockito.anyString(), eq(VatUpdateDto.class))).thenReturn(vatUpdateDto);

    bulkUpdateServiceBean.setFinalStatusAndNotificationOnVatBulkUpdate(bulkProcess, DEFAULT_STORE_ID);

    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
  }

  @Test
  public void setFinalStatusAndNotificationOnVatBulkUpdateGenrateErrorFileSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean,"generateVatErrorFile",true);
    createVatUpdateFiles(BULK_VAT_DIRECTORY_1);
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    bulkProcess.setInternationalMerchant(true);
    bulkProcess.setTotalCount(5);
    bulkProcess.setSuccessCount(4);
    bulkProcess.setErrorCount(1);
    VatUpdateDto vatUpdateDto = new VatUpdateDto("MTA-12345678-12345", "1");
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setStatus(BulkProcessData.STATUS_SUCCESS);
    bulkProcessData1.setBulkRequestData("");
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setErrorMessage("Error Message");
    bulkProcessData2.setBulkRequestData("");
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess)).thenReturn(
            Arrays.asList(bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(Mockito.anyString(), eq(VatUpdateDto.class))).thenReturn(vatUpdateDto);
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnVatBulkUpdate(bulkProcess, DEFAULT_STORE_ID);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
  }

  @Test
  public void setFinalStatusAndNotificationOnVatBulkUpdateGenrateErrorFileSwitchOnNoFailedRowsTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean,"generateVatErrorFile",true);
    createVatUpdateFiles(BULK_VAT_DIRECTORY_1);
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    bulkProcess.setInternationalMerchant(true);
    bulkProcess.setTotalCount(5);
    bulkProcess.setSuccessCount(4);
    bulkProcess.setErrorCount(1);
    VatUpdateDto vatUpdateDto = new VatUpdateDto("MTA-12345678-12345", "1");
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setStatus(BulkProcessData.STATUS_SUCCESS);
    bulkProcessData1.setBulkRequestData("");
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setStatus(BulkProcessData.STATUS_SUCCESS);
    bulkProcessData2.setErrorMessage("Error Message");
    bulkProcessData2.setBulkRequestData("");
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess)).thenReturn(
            Arrays.asList(bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(Mockito.anyString(), eq(VatUpdateDto.class))).thenReturn(vatUpdateDto);
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnVatBulkUpdate(bulkProcess, DEFAULT_STORE_ID);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
  }

  @Test
  public void setFinalStatusAndNotificationOnVatBulkUpdateExceptionTest() throws Exception {
    createVatUpdateFiles(BULK_VAT_DIRECTORY_1);
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
    bulkProcess.setInternationalMerchant(true);
    bulkProcess.setTotalCount(5);
    bulkProcess.setSuccessCount(4);
    bulkProcess.setErrorCount(1);
    VatUpdateDto vatUpdateDto = new VatUpdateDto("MTA-12345678-12345", "1");
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setStatus(BulkProcessData.STATUS_SUCCESS);
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setErrorMessage("Error Message");
    bulkProcessData2.setBulkRequestData("");

    when(bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess)).thenReturn(
        Arrays.asList(bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(Mockito.anyString(), eq(VatUpdateDto.class))).thenThrow(JsonParseException.class);

    bulkUpdateServiceBean.setFinalStatusAndNotificationOnVatBulkUpdate(bulkProcess, DEFAULT_STORE_ID);

    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
  }

  @Test
  public void processEventForVatUpdateTest() throws Exception {
    bulkProcessData.setBulkRequestData("");
    ItemCodeDetailResponse itemCodeDetailResponse =
        new ItemCodeDetailResponse(PRODUCT_SKU, BUSINESS_PARTNER_CODE, TD_MERCHANT, "MTA-12345678-12345");
    Mockito.when(bulkProcessService.findByBulkProcessCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList(), Mockito.anyString())).thenReturn(Arrays.asList(bulkProcessData));
    VatUpdateDto vatUpdateDto = new VatUpdateDto("MTA-12345678-12345", "1");
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(VatUpdateDto.class))).thenReturn(vatUpdateDto);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(bulkUpdateQueue.getStoreId(),
        SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE, "3",
            SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE));
    when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
            bulkProcess.getBusinessPartnerCode())).thenReturn(getProfileResponse());
    when(pcbOutboundService.batchVatFlagUpdate(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyMap())).thenReturn(new ArrayList<>());
    when(xProductOutboundService.getItemDetailsByItemCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(SimpleSetStringRequest.class))).thenReturn(Arrays.asList(itemCodeDetailResponse));
    bulkUpdateServiceBean.processEventForVatUpdate(
        BulkUpdateEventModel.builder().bulkProcessCode(BUSINESS_PARTNER_CODE).bulkProcessCode(DEFAULT_BULK_PROCESS_CODE)
            .storeId(DEFAULT_STORE_ID).rowNumbers(Arrays.asList(1, 2)).build());
    Mockito.verify(bulkProcessService).findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE,
            Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(bulkUpdateQueue.getStoreId(),
        SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(
        bulkUpdateQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
    verify(xProductOutboundService).getItemDetailsByItemCodes(Mockito.isNull(), Mockito.anyString(),
        Mockito.any(SimpleSetStringRequest.class));
  }

  @Test
  public void processEventForVatUpdateEmptyDataListTest() throws Exception
  {
    Mockito.when(bulkProcessService.findByBulkProcessCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList(), Mockito.anyString())).thenReturn(new ArrayList());
    bulkUpdateServiceBean.processEventForVatUpdate(
        BulkUpdateEventModel.builder().bulkProcessCode(BUSINESS_PARTNER_CODE).bulkProcessCode(DEFAULT_BULK_PROCESS_CODE)
            .storeId(DEFAULT_STORE_ID).rowNumbers(Arrays.asList(1, 2)).build());
    Mockito.verify(bulkProcessService).findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE,
            Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
  }

  @Test
  public void processEventForVatUpdateFailedValidationVatDtoTest() throws Exception
  {
    bulkProcessData.setBulkRequestData("");
    ItemCodeDetailResponse itemCodeDetailResponse =
        new ItemCodeDetailResponse(PRODUCT_SKU, BUSINESS_PARTNER_CODE, TD_MERCHANT, "MTA-12345678-12345");
    Mockito.when(bulkProcessService.findByBulkProcessCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList(), Mockito.anyString())).thenReturn(Arrays.asList(bulkProcessData));
    VatUpdateDto vatUpdateDto = new VatUpdateDto("MTA-12345678-12345", "4");
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(VatUpdateDto.class))).thenReturn(vatUpdateDto);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(bulkUpdateQueue.getStoreId(),
        SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE, "3",
            SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE));
    when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
            bulkProcess.getBusinessPartnerCode())).thenReturn(getProfileResponse());
    when(pcbOutboundService.batchVatFlagUpdate(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyMap())).thenReturn(new ArrayList<>());
    when(xProductOutboundService.getItemDetailsByItemCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(SimpleSetStringRequest.class))).thenReturn(Arrays.asList(itemCodeDetailResponse));
    bulkUpdateServiceBean.processEventForVatUpdate(
        BulkUpdateEventModel.builder().bulkProcessCode(BUSINESS_PARTNER_CODE).bulkProcessCode(DEFAULT_BULK_PROCESS_CODE)
            .storeId(DEFAULT_STORE_ID).rowNumbers(Arrays.asList(1, 2)).build());
    Mockito.verify(bulkProcessService).findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE,
            Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Arrays.asList(bulkProcessData));
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(bulkUpdateQueue.getStoreId(),
        SystemParameterConfigNames.BULK_VAT_UPDATE_BATCH_SIZE);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(
        bulkUpdateQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
  }

  @Test
  public void processVatUpdateExceptionTest() throws Exception {
    Mockito.when(bulkProcessService.findByBulkProcessCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyList(), Mockito.anyString())).thenReturn(Arrays.asList(bulkProcessData));
    when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
            bulkProcess.getBusinessPartnerCode())).thenReturn(getProfileResponse());
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(VatUpdateDto.class)))
        .thenThrow(JsonProcessingException.class);
    bulkUpdateServiceBean.processEventForVatUpdate(
        BulkUpdateEventModel.builder().bulkProcessCode(BUSINESS_PARTNER_CODE).bulkProcessCode(DEFAULT_BULK_PROCESS_CODE)
            .storeId(DEFAULT_STORE_ID).rowNumbers(Arrays.asList(1, 2)).build());
    Mockito.verify(bulkProcessService).findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE,
            Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
    verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(bulkUpdateQueue.getStoreId(),
            bulkProcess.getBusinessPartnerCode());
    Mockito.verify(this.bulkProcessDataService).saveAndReturnBulkProcessData(anyList());
  }

  @Test
  public void generateBulkProcessDataMaxRowExceptionTest() throws Exception {
    bulkProcess.setInternationalMerchant(true);
    systemParameterConfig.setValue(String.valueOf(1));
    when(bulkUpdateServiceUtil
        .getBulkProcess(anyString(), anyString(), anyString(), any(BulkUpdateProcessDTO.class), anyInt(), anyInt(),
            anyBoolean(), anyBoolean())).thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE))
      .thenReturn(systemParameterConfig);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.bulkUpdateServiceBean.generateBulkProcessData(bulkProcess,
              Arrays.asList(getInputRowData("updateInput")), privilegedMap, getProfileResponse()));
    } finally {
      Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
      verify(objectMapper, times(2)).writeValueAsString(any());
      Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
          SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
          SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
          SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);

    }
  }

  @Test
  public void processEventMppOnTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    BulkProcessData data = new BulkProcessData();
    Map<String, String> map = new LinkedHashMap<>();
    map.put("Blibli SKU", "TOQ-15120-00001-00001");
    map.put("Harga Akhir", "40000.0");
    map.put("Harga Jual", "50000");
    map.put("Harga Normal", "50000");
    map.put("Kuota", "2.0");
    map.put("Nama Produk", "Batik Testing AIFA M Brick Red");
    map.put("Persentase Diskon", "0.2");
    map.put("Potongan Harga", "10000");
    map.put("RowNumber", "6");
    map.put("Stok Tersedia", "5");
    data.setBulkRequestData(new ObjectMapper().writeValueAsString(map));
    data.setRowNumber(6);
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    bulkUpdateQueue.getCampaignItemSummaryRequest().setCategories(Arrays.asList(CATEGORY_CODE, CATEGORY_CODE1));
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setNotes("");
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = getProductLevel3ReponsesForCampaign();
    productLevel3SummaryResponses.get(0).getPrices().get(0).setPrice(6000.0);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(data);
    BulkUpdateEventModel bulkUpdateEventModel =
      BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
        .rowNumbers(Arrays.asList(1, 2)).storeId(Constant.STORE_ID).build();
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
      bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    when(objectMapper.readValue(Mockito.anyString(), any(TypeReference.class))).thenReturn(map);
    when(objectMapper.readValue(Mockito.anyString(), eq(BulkAddCampaignProductQueue.class))).thenReturn(bulkUpdateQueue);
    when(productCategoryBaseRepository.getAllChildCategoriesFromC1CategoryCode(anyString(), anyString(),
      any())).thenReturn(Collections.emptyList());
    when(bulkProcessDataService.saveOperation(Mockito.any())).thenReturn(data);
    bulkUpdateServiceBean.processEvent(bulkUpdateEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
      .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE,
        Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    Mockito.verify(bulkProcessDataService).saveOperation(Mockito.any());
  }

  @Test
  public void processEventWithPricingOnCatalogOffTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "pricingMultiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", false);
    BulkProcessData data = new BulkProcessData();
    Map<String, String> map = new LinkedHashMap<>();
    map.put("Blibli SKU", "TOQ-15120-00001-00001");
    map.put("Harga Akhir", "40000.0");
    map.put("Harga Jual", "50000");
    map.put("Harga Normal", "50000");
    map.put("Kuota", "2.0");
    map.put("Nama Produk", "Batik Testing AIFA M Brick Red");
    map.put("Persentase Diskon", "0.2");
    map.put("Potongan Harga", "10000");
    map.put("RowNumber", "6");
    map.put("Stok Tersedia", "5");
    data.setBulkRequestData(new ObjectMapper().writeValueAsString(map));
    data.setRowNumber(6);
    BulkAddCampaignProductQueue bulkUpdateQueue = this.getBulkAddCampaignQueue();
    bulkUpdateQueue.getCampaignItemSummaryRequest().setCategories(Arrays.asList(CATEGORY_CODE, CATEGORY_CODE1));
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setNotes("");
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = getProductLevel3ReponsesForCampaign();
    productLevel3SummaryResponses.get(0).getPrices().get(0).setPrice(6000.0);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(data);
    BulkUpdateEventModel bulkUpdateEventModel =
      BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
        .rowNumbers(Arrays.asList(1, 2)).storeId(Constant.STORE_ID).build();
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
      bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    when(objectMapper.readValue(Mockito.anyString(), any(TypeReference.class))).thenReturn(map);
    when(objectMapper.readValue(Mockito.anyString(), eq(BulkAddCampaignProductQueue.class))).thenReturn(bulkUpdateQueue);
    when(productCategoryBaseRepository.getAllChildCategoriesFromC1CategoryCode(anyString(), anyString(),
      any())).thenReturn(Collections.emptyList());
    when(bulkProcessDataService.saveOperation(Mockito.any())).thenReturn(data);
    bulkUpdateServiceBean.processEvent(bulkUpdateEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
      .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE,
        Arrays.asList(1, 2), BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    Mockito.verify(bulkProcessDataService).saveOperation(Mockito.any());
  }

  @Test
  public void saveBulkProcessDataMppOnTestTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    Map<String, String> dataMap1 = new HashMap<>();
    dataMap1.put(BulkParameters.BLIBLI_SKU, BLIBLI_SKU);
    dataMap1.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    productDataMap.add(dataMap1);
    Map<String, String> dataMap2 = new HashMap<>();
    Map<String, String> dataMap3 = new HashMap<>();
    dataMap3.put(BulkParameters.BLIBLI_SKU, BLIBLI_SKU);
    productDataMap.add(dataMap2);
    productDataMap.add(dataMap3);
    bulkUpdateServiceBean.saveBulkProcessData(bulkProcess, productDataMap);
    Mockito.verify(bulkProcessService).saveOperation(bulkProcessArgumentCaptor.capture());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());
    Mockito.verify(bulkUpdateServiceUtil)
      .getFinalDataList(Mockito.anyMap(), Mockito.anyList(), Mockito.eq(true), Mockito.anyBoolean());
    Assertions.assertEquals("Update", bulkProcessArgumentCaptor.getValue().getBulkProcessType());
  }

  @Test
  public void sendFinalStatusEventForPickupPointDeleteTest() throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setTotalCount(10);
    bulkProcess.setStatus(FAILED);
    bulkProcess.setRequestId(REQUEST_ID);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setNotes(PICKUP_POINT_CODE);
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    BulkProcessData bulkProcessData3 = new BulkProcessData();
    bulkProcessData1.setSystemErrorCount(1);
    bulkProcessData2.setInputErrorCount(1);
    bulkProcessData1.setStatus(STATUS_SUCCESS);
    LinkedHashMap<String, String> boom = new LinkedHashMap<>();
    boom.put("222","3333");
    LinkedHashMap<String, String> ppCodeUpdatedRecords = new LinkedHashMap<>();
    ppCodeUpdatedRecords.put(BulkParameters.ITEM_SKU_REQUEST, "SKU123");
    ppCodeUpdatedRecords.put(BulkParameters.NEW_PICKUP_POINT_CODE_REQUEST, "PP123");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(ppCodeUpdatedRecords);
    bulkProcessData1.setNotes("string");
    bulkProcessData2.setNotes("string");
    bulkProcessData3.setStatus(FAILED);
    bulkProcessData3.setNotes(null);
    bulkProcessData2.setStatus(FAILED);
    Mockito.when(bulkProcessFileGeneration.generateFileFromResponse(any(), any(), any())).thenReturn("Among");
    Mockito.when(bulkProcessHelperFactory.getHelper(any())).thenReturn(new DeleteUpdatePickupPointsProcessHelper());
    bulkUpdateServiceBean.sendFinalStatusEventForPickupPointDelete(DEFAULT_STORE_ID, bulkProcess,
        Arrays.asList(bulkProcessData1, bulkProcessData2, bulkProcessData3));
    Mockito.verify(kafkaProducer)
        .send(eq(kafkaTopicProperties.getDeletePickupPointResponseEvent()), eq(BUSINESS_PARTNER_CODE),
            deletePickupPointResponseEventModelArgumentCaptor.capture());
    Mockito.verify(bulkProcessFileGeneration).generateFileFromResponse(any(), any(), any());
    Mockito.verify(bulkProcessHelperFactory).getHelper(any());
    Mockito.verify(kafkaTopicProperties, times(3)).getDeletePickupPointResponseEvent();
  }

  @Test
  public void processBulkUpdateItemWithFreeSampleOffTest() throws Exception {
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
      BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
        .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
      getInputRowData("updateInputMppSwitchOn4"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA_2), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP_4);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
      bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdateMppSwitchOn.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setFreeSample(Boolean.TRUE);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode("PP-3000214");
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
      + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
      bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
      anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(pbpOutboundService)
      .updateSummaryL5(Mockito.eq(bulkProcess.getCreatedBy()), productVariantUpdateRequestArgumentCaptor.capture(),
          eq(Constant.CLIENT_ID));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getItemSku(), GDN_SKU_1);
    Assertions.assertEquals(itemPickupPointListingL3RequestArgumentCaptor.getValue().getPickupPointCodes(),
      new HashSet<>());
    Assertions.assertEquals(productVariantUpdateRequestArgumentCaptor.getValue().getAddPickupPoints().size(), 0);
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOnWithFreeSampleOnCncOnFailureTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
      BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
        .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
      getInputRowData("updateInputMppSwitchOn6"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
      bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("mppSwitchOnItemUpdateShipping.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setWebSyncStock(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFreeSample(Boolean.TRUE);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
      + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
      bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
      anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkUpdateItemMPPSwitchOnWithFreeSampleOnBfbFailureTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
      BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
        .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
      getInputRowData("updateInputBffTest"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
      bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("mppSwitchOnItemUpdateShipping.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setWebSyncStock(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFreeSample(Boolean.TRUE);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
      + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
      bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
      anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void setFinalStatusAndNotificationInstoreNotificationEnabledUpload() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "generateInstoreErrorFile", true);
    Map<String, String> files = this.getFile(OFF_2_ON_EMPTY_DATA_SHEET);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    bulkProcess.setBulkProcessCode(bulkUpdateQueue.getBulkProcessCode());
    bulkProcess.setTotalCount(4);
    bulkProcess.setBulkProcessType(BulkProcessType.IN_STORE.getValue());
    bulkProcess.setInternationalMerchant(true);
    bulkProcess.setDescription(BULK_OFF2ON_UPDATE_FILE);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
      .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileData(Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkProcess.class))).thenReturn(sheet);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess)).thenReturn(
      getBulkProcessDataForInstoreUpdate());
    when(fileStorageService.getDownloadLink(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn("file");
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnInstoreUpload(bulkProcess, DEFAULT_STORE_ID);
    Mockito.verify(fileStorageServiceBean).getFileData(Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkProcess.class));
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
  }

  @Test
  public void setFinalStatusAndNotificationInstoreNotificationEnabledAllSuccessTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "generateInstoreErrorFile", true);
    Map<String, String> files = this.getFile(OFF_2_ON_EMPTY_DATA_SHEET);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    bulkProcess.setBulkProcessCode(bulkUpdateQueue.getBulkProcessCode());
    bulkProcess.setTotalCount(1);
    bulkProcess.setInternationalMerchant(true);
    bulkProcess.setDescription(BULK_OFF2ON_UPDATE_FILE);
    XSSFWorkbook workBook;
    InputStream is =  Thread.currentThread().getContextClassLoader()
      .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    List<BulkProcessData> bulkProcessDataList = getBulkProcessDataForInstoreUpdate();
    Mockito.when(fileStorageServiceBean.getFileData(Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkProcess.class))).thenReturn(sheet);
    when(
      bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess)).thenReturn(
      Arrays.asList(bulkProcessDataList.get(0)));
    when(fileStorageService.getDownloadLink(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn("file");
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnInstoreUpload(bulkProcess, DEFAULT_STORE_ID);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
  }

  @Test
  public void setFinalStatusAndNotificationWorkOrderTest() throws Exception {
    Map<String, String> files = this.getFile(OFF_2_ON_EMPTY_DATA_SHEET);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
      ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    bulkProcess.setBulkProcessCode(bulkUpdateQueue.getBulkProcessCode());
    bulkProcess.setTotalCount(1);
    bulkProcess.setInternationalMerchant(true);
    bulkProcess.setDescription(BULK_OFF2ON_UPDATE_FILE);
    bulkProcess.setBulkProcessType(BulkProcessType.TRANSFER_REQUEST.getValue());
    XSSFWorkbook workBook;
    InputStream is = Thread.currentThread().getContextClassLoader()
      .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    List<BulkProcessData> bulkProcessDataList = getBulkProcessDataForInstoreUpdate();
    Mockito.when(fileStorageServiceBean.getFileData(Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkProcess.class))).thenReturn(sheet);
    when(
      bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess)).thenReturn(
      Arrays.asList(bulkProcessDataList.get(0)));
    when(fileStorageService.getDownloadLink(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn("file");
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    bulkUpdateServiceBean.setFinalStatusAndNotificationForWorkOrderUpload(DEFAULT_STORE_ID,
      bulkProcess, bulkProcessDataList);
  }

  @Test
  public void setFinalStatusAndNotificationWorkOrderFinishedTest() throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStatus(BulkProcess.STATUS_PUBLISHED);
    bulkProcess.setBulkProcessCode(bulkUpdateQueue.getBulkProcessCode());
    bulkProcess.setDescription(BULK_OFF2ON_UPDATE_FILE);
    bulkProcess.setBulkProcessType(BulkProcessType.ASSEMBLY_REQUEST.getValue());
    bulkProcess.setInternationalMerchant(Boolean.TRUE);
    bulkProcess.setTotalCount(1);
    Map<String, String> files = this.getFile(OFF_2_ON_EMPTY_DATA_SHEET);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
      ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    XSSFWorkbook workBook;
    InputStream is = Thread.currentThread().getContextClassLoader()
      .getResourceAsStream("ExcelTemplate" + File.separator + "bulk-update-product-template.xlsx");
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    List<BulkProcessData> bulkProcessDataList = getBulkProcessDataForInstoreUpdate();
    bulkProcessDataList.forEach(bulkProcessData3 -> bulkProcessData3.setStatus(BulkProcessData.STATUS_SUCCESS));
    Mockito.when(fileStorageServiceBean.getFileData(Mockito.any(BulkUpdateQueue.class), Mockito.any(BulkProcess.class))).thenReturn(sheet);
    when(
      bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess)).thenReturn(
      Arrays.asList(bulkProcessDataList.get(0)));
    when(fileStorageService.getDownloadLink(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn("file");
    when(bulkProcessRepository.save(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    bulkUpdateServiceBean.setFinalStatusAndNotificationForWorkOrderUpload(DEFAULT_STORE_ID,
      bulkProcess, bulkProcessDataList);
  }

  @Test
  public void processBulkUpdateItemFreeSampleONTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
            .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
        getInputRowData("updateInputMppSwitchOn6"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("mppSwitchOnItemUpdateShipping.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setWebSyncStock(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFreeSample(Boolean.TRUE);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
        bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
        anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
        .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
        DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void processBulkUpdateItemUpdatePPWitWarehouseStock_nonMppSeller_Test() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "validateWarehouseVariantDeletionEnabled"
      , true);
    ReflectionTestUtils.setField(bulkUpdateServiceBean,
      "supportedMerchantsForWarehouseStockValidation", TD_MERCHANT);
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "multiPickupPointEnabled", true);
    bulkProcessData2.setBulkRequestData(BULK_DATA_2);
    bulkProcessData2.setParentProduct(GDN_SKU_1);
    List<BulkProcessData> bulkProcessDataList = Arrays.asList(bulkProcessData2);
    BulkUpdateEventModel bulkUpdateEventModel =
      BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE).businessPartnerCode(DEFAULT_BP_CODE)
        .rowNumbers(Arrays.asList(1)).storeId(Constant.STORE_ID).build();
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(
      getInputRowData("updateInputMppSwitchOn6"));
    when(objectMapper.readValue(eq(PRIVILEGED_MAP_DATA), any(TypeReference.class))).thenReturn(PRIVILEGED_MAP);
    when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
      bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("mppSwitchOnItemUpdateShipping.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = getItemPickupPointL3Response();
    itemPickupPointListingL3Response.setWebSyncStock(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    ProfileResponse profileResponse = this.getProfileResponse();
    PickupPointDTO pickupPointDTO=new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    PickupPointDTO pickupPointDTO2 =new PickupPointDTO();
    pickupPointDTO.setCode(PICKUP_POINT_CODE_2);
    profileResponse.getPickupPoints().add(pickupPointDTO);
    profileResponse.getPickupPoints().add(pickupPointDTO2);
    CompanyDTO company = CompanyDTO.builder().merchantType(TD_MERCHANT).build();
    profileResponse.setCompany(company);
    profileResponse.setFbbActivated(true);
    itemPickupPointListingL3Response.setAvailableStockLevel1(30);
    BulkProcess bulkProcess = this.getBulkProcess();
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
      + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(itemPickupPointListingL3ResponseList);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
      Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
      bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
      anyString())).thenReturn(profileResponse);
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(pbpOutboundService, times(2))
      .getItemPickupPointListingL3Response(eq(0), eq(1), itemPickupPointListingL3RequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(this.bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(objectMapper, times(1)).writeValueAsString(any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(Constant.STORE_ID,
      DEFAULT_BULK_PROCESS_CODE, Arrays.asList(1), BulkProcessData.STATUS_PENDING);
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }


  @Test
  public void getItemPickupPointListingL3ResponsesEmptyTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "itemPickupPointListFetchSize", 1);
    Mockito.when(pbpOutboundService.getItemPickupPointListingL3Response(0, 1, new ItemPickupPointListingL3Request()))
        .thenReturn(new PageImpl<>(List.of(new ItemPickupPointListingL3Response())));
    bulkUpdateServiceBean.getItemPickupPointListingL3Responses(new ArrayList<>(),
        new ItemPickupPointListingL3Request());
    Mockito.verify(pbpOutboundService).getItemPickupPointListingL3Response(0, 1, new ItemPickupPointListingL3Request());
  }

  @Test
  public void getItemPickupPointListingL3ResponsesTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "itemPickupPointListFetchSize", 1);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemName(USER_NAME_2);
    Mockito.when(pbpOutboundService.getItemPickupPointListingL3Response(0, 1, new ItemPickupPointListingL3Request()))
        .thenReturn(new PageImpl<>(List.of(itemPickupPointListingL3Response)));
    bulkUpdateServiceBean.getItemPickupPointListingL3Responses(
        Collections.singletonList(itemPickupPointListingL3Response), new ItemPickupPointListingL3Request());
  }

  @Test
  public void populateGenericErrorMessageInFailedSkusMapTest() {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkCampaignErrorHandling", true);
    CampaignProductRequest campaignProductRequest = new CampaignProductRequest();
    campaignProductRequest.setCampaignCode("campagin");
    bulkUpdateServiceBean.populateGenericErrorMessageInFailedSkusMap(Collections.singletonList(campaignProductRequest),
        new HashMap<>());
  }

  @Test
  public void populateGenericErrorMessageInFailedSkusMapSwitchOffTest() {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkCampaignErrorHandling", false);
    CampaignProductRequest campaignProductRequest = new CampaignProductRequest();
    campaignProductRequest.setCampaignCode("campagin");
    bulkUpdateServiceBean.populateGenericErrorMessageInFailedSkusMap(Collections.singletonList(campaignProductRequest),
        new HashMap<>());
  }

  @Test
  public void testSetFinalStatusAndNotificationOnBasicInfoUpdate() throws Exception {
    String storeId = "store-001";
    String processCode = "process-123";

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(processCode);
    bulkProcess.setUploadedFile("test.xlsx");
    bulkProcess.setDescription("errorLINK");

    BulkProcessData dataSuccess = new BulkProcessData();
    dataSuccess.setStatus(BulkProcessData.STATUS_SUCCESS);
    dataSuccess.setRowNumber(1);

    BulkProcessData dataFail = new BulkProcessData();
    dataFail.setStatus(BulkProcessData.STATUS_FAIL);
    dataFail.setRowNumber(2);

    List<BulkProcessData> processDataList = List.of(dataSuccess, dataFail);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(storeId, bulkProcess)).thenReturn(processDataList);

    // Mock Excel sheet
    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Sheet1");
    for (int i = 0; i <= 21; i++) {
      sheet.createRow(i);
    }
    sheet.getRow(0).createCell(2).setCellValue("Instore");
    sheet.getRow(1).createCell(20).setCellValue("some data");

    when(fileStorageServiceBean.getDataSheetForBasicInfo("test.xlsx", bulkProcess)).thenReturn(sheet);
    when(fileStorageService.getDownloadLink(any(), any(), any(), any())).thenReturn("http://mock-error-file-url");

    // When
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnBasicInfoUpdate(storeId, bulkProcess);

    // Then
    verify(fileStorageServiceBean).getDataSheetForBasicInfo("test.xlsx", bulkProcess);
    verify(fileStorageService).createBulkFile(any(), any(), any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(any(),any());
    verify(notificationService).sendNotificationWithErrorFileGenerated(
        eq(bulkProcess),
        contains("http://mock-error-file-url"),
        eq(false),
        eq(false)
    );
  }

  @Test
  public void testSetFinalStatusAndNotificationOnBasicInfoUpdateForRemoveCellFormula() throws Exception {
    String storeId = "store-001";
    String processCode = "process-123";

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(processCode);
    bulkProcess.setUploadedFile("test.xlsx");
    bulkProcess.setDescription("errorLINK");

    BulkProcessData dataSuccess = new BulkProcessData();
    dataSuccess.setStatus(BulkProcessData.STATUS_SUCCESS);
    dataSuccess.setRowNumber(1);

    BulkProcessData dataFail = new BulkProcessData();
    dataFail.setStatus(BulkProcessData.STATUS_FAIL);
    dataFail.setRowNumber(2);

    List<BulkProcessData> processDataList = List.of(dataSuccess, dataFail);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(storeId, bulkProcess)).thenReturn(processDataList);

    // Mock Excel sheet
    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Sheet1");
    for (int i = 0; i <= 21; i++) {
      sheet.createRow(i);
    }
    sheet.getRow(0).createCell(2).setCellValue("some value");
    sheet.getRow(1).createCell(20).setCellValue("some data");

    when(fileStorageServiceBean.getDataSheetForBasicInfo("test.xlsx", bulkProcess)).thenReturn(sheet);
    when(fileStorageService.getDownloadLink(any(), any(), any(), any())).thenReturn("http://mock-error-file-url");

    // When
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnBasicInfoUpdate(storeId, bulkProcess);

    // Then
    verify(fileStorageServiceBean).getDataSheetForBasicInfo("test.xlsx", bulkProcess);
    verify(fileStorageService).createBulkFile(any(), any(), any());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(any(),any());
    verify(notificationService).sendNotificationWithErrorFileGenerated(
        eq(bulkProcess),
        contains("http://mock-error-file-url"),
        eq(false),
        eq(false)
    );
  }

  @Test
  public void testSetFinalStatusAndNotification_WhenStatusIsFinished() throws Exception {
    // Given
    String storeId = "store-001";
    String processCode = "process-456";

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(processCode);
    bulkProcess.setUploadedFile("test.xlsx");
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    bulkProcess.setDescription("errorLINK");

    BulkProcessData dataSuccess = new BulkProcessData();
    dataSuccess.setStatus(BulkProcessData.STATUS_SUCCESS);
    dataSuccess.setRowNumber(1);

    BulkProcessData dataFail = new BulkProcessData();
    dataFail.setStatus(BulkProcessData.STATUS_SUCCESS);
    dataFail.setRowNumber(2);

    List<BulkProcessData> processDataList = List.of(dataSuccess, dataFail);
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(storeId, bulkProcess)).thenReturn(
        processDataList);
    bulkUpdateServiceBean.setFinalStatusAndNotificationOnBasicInfoUpdate(storeId, bulkProcess);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(any(),any());
    verify(notificationService).sendNotificationWithErrorFileGenerated(eq(bulkProcess),
        eq(bulkProcess.getDescription()), // no error file URL appended
        eq(false), eq(false));
  }

  @Test
  public void processBulkUpdateV2HeaderCheck() throws Exception {
    productCampaignAvailabilityRequest.setItemSkus(new HashSet<>());
    productCampaignAvailabilityResponse.setProductCampaignAvailabilityMap(new HashMap<>());
    ReflectionTestUtils.setField(bulkUpdateServiceBean,"headerValidationCheck",true);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 2);
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse = this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
      ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    InputStream is = new FileInputStream(file);
    assertNotNull(is);
    // Mock GCS layer to return actual FileInputStream (like your openGcsInputStream)
    when(fileStorageServiceBean.openGcsInputStream(any(), any())).thenReturn(is);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(bulkUpdateQueue.getStoreId(),
      bulkUpdateQueue.getBulkProcessCode())).thenReturn(bulkProcess);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(null);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    bulkUpdateServiceBean.processBulkUpdateV2(bulkUpdateQueue);
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(anyString(), anyString(),
      eq(BulkProcess.STATUS_PENDING));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(anyString(), anyString());
    verifyNoMoreInteractions(productLevel3Repository);
    Mockito.verify(trackerService)
      .sendTracker(Mockito.eq(PRODUCT_UPDATE_EVENT), Mockito.eq(PRODUCT_UPDATE_ATTRI_TYPE), Mockito.eq(HYPHEN),
        Mockito.eq(TrackerConstants.FAILED), Mockito.anyString());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void testProcessBulkUpdateV2_WithNoColumnstoParse() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "cncForWarehouseFeatureSwitch", true);
    systemParameterConfig.setValue(MAX_PRODUCT_SIZE);
    setBulkUpdateServiceUtil();
    ReflectionTestUtils.setField(bulkUpdateServiceBean, "bulkUpdateBatchSize", 1);
    Map<String, String> files = this.getFile("BulkUpdate_cnc1P.xlsx");
    files.put(BulkParameters.STOCK_REMINDER_COLUMN_ID, "1");
    files.put(BulkParameters.PICKUP_POINT_NAME_COLUMN_ID, "1");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProfileResponse profileResponse = this.getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    BulkUpdateQueue bulkUpdateQueue = this.getBulkUpdateQueue();
    Map<String, Boolean> privilegedMap1 = bulkUpdateQueue.getPrivilegedMap();
    privilegedMap1.put("isPrivilegedToEditCncStatus", true);
    bulkUpdateQueue.setPrivilegedMap(privilegedMap1);
    BulkProcess bulkProcess = this.getBulkProcess();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse =
      this.getProductLevel3SummaryResponse();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(),
      anyString())).thenReturn(profileResponse);
    ProcessorUtils.createDirectories(
      ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
      ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(
      ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    InputStream is = new FileInputStream(file);
    assertNotNull(is);
    // Mock GCS layer to return actual FileInputStream (like your openGcsInputStream)
    when(fileStorageServiceBean.openGcsInputStream(any(), any())).thenReturn(is);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
      new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
      new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
      new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED)).thenReturn(
      new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
        SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE)).thenReturn(
      systemParameterConfig);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(0),
        Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
      .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
      .thenReturn(new HashMap<>());
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn("");
    bulkUpdateServiceBean.processBulkUpdateV2(bulkUpdateQueue);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED);
    Mockito.verify(this.bulkProcessRepository).save(Mockito.any(BulkProcess.class));
    Mockito.verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
      .filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
  }
}
