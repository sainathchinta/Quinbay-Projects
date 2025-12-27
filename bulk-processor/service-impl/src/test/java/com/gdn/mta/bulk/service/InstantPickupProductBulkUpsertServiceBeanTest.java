package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.VariantsErrorListResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkCncUpsertErrorDTO;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.AuditTrailInfo;
import com.gdn.mta.bulk.models.BulkUpdateErrorCounter;
import com.gdn.mta.bulk.models.ExcelHeaderNames;
import com.gdn.mta.bulk.models.GenericErrorMessages;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.util.IOUtils;
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
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public class InstantPickupProductBulkUpsertServiceBeanTest {

  private static final String RANDOM_BULK_PROCESS_CODE = UUID.randomUUID().toString();
  private static final String REQUEST_ID = "requestId";
  private static final String BUSINESS_PARTNER_CODE = "TOA-14961";
  private static final String ITEM_SKU = "TOA-14961-00117-00006";
  private static final String FAAS_ACTIVATED = "faasActivated";
  private static final String ITEM_SKU_1 = "TOA-14961-00118-00002";
  private static final String ITEM_SKU_2 = "TOA-14961-00118-00001";
  private static final String PRODUCT_SKU_1 = "TOA-14961-00118";
  private static final Double LIST_PRICE = 10000d;
  private static final Double OFFER_PRICE = 5000d;
  private static final Double LIST_PRICE_1 = 12000d;
  private static final Double OFFER_PRICE_1 = 6000d;
  private static final Double LIST_PRICE_2 = 14000d;
  private static final Double OFFER_PRICE_2 = 7000d;
  private static final Integer STOCK = 1;
  private static final String FILE_NAME = "test.xlsx";
  private static final String PICKUP_POINT_CODE = "PP-12345";
  private static final String PICKUP_POINT_CODE_1 = "PP-12346";
  private static final String PICKUP_POINT_CODE_2 = "PP-12347";
  private static final String STORE_ID = "10001";
  private static final String USERNAME = "username";
  private static final String CLIENT_HOST = "localhost";
  private static final String ERROR_CODE = "errorCode";
  private static final String NOTES = "NOTES";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String PRODUCT_NAME = "PRODUCT_NAME";



  private ObjectMapper objectMapperForTest = new ObjectMapper();

  @InjectMocks
  private InstantPickupProductBulkUpsertServiceBean instantPickupProductBulkUpsertServiceBean;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private TrackerService trackerService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private ObjectMapper objectMapper;
  
  @Mock
  private NotificationService notificationService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private PickupPointService pickupPointService;

  @Mock
  private PBPOutboundServiceBean pbpOutboundService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Captor
  private ArgumentCaptor<AuditTrailInfo> auditTrailInfoArgumentCaptor;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private FileStorageServiceBean fileStorageServiceBean;

  private BulkUpdateProcessDTO bulkUpdateProcessDTO;
  private BulkUpdateQueue bulkUpdateQueue;
  private BulkProcess bulkProcess;
  private AuditTrailInfo auditTrailInfo;
  private UpsertOfflineItemRequest upsertOfflineItemRequest;
  private UpsertOfflineItemRequest upsertOfflineItemRequest1;
  private UpsertOfflineItemRequest upsertOfflineItemRequest2;
  private List<UpsertOfflineItemRequest> upsertOfflineItemRequests;
  private UpsertOfflineItemFailedResponse upsertOfflineItemFailedResponse;
  private List<UpsertOfflineItemFailedResponse> upsertOfflineItemFailedResponses;
  private List<BulkProcessData> bulkProcessDataList;
  private BulkUpdateEventModel bulkUpdateEventModel;
  private ProfileResponse profileResponse;
  private PickupPointResponse pickupPointResponse;

  @Test
  public void testPreProcessInstantPickupProductBulkUpsert() throws Exception {
    when(bulkUpdateServiceUtil.getBulkProcess(eq(STORE_ID), eq(REQUEST_ID), anyString(),
        eq(bulkUpdateProcessDTO), eq(0), eq(0), eq(false), eq(false))).thenReturn(bulkProcess);
    instantPickupProductBulkUpsertServiceBean.preProcessInstantPickupProductBulkUpsert(STORE_ID,
        REQUEST_ID, bulkUpdateProcessDTO, null);
    verify(bulkUpdateServiceUtil).getBulkProcess(eq(STORE_ID), eq(REQUEST_ID), anyString(),
        eq(bulkUpdateProcessDTO), eq(0), eq(0), eq(false), eq(false));
    verify(bulkUpdateServiceUtil).savePreProcessBulkData(any(BulkProcess.class));
    verify(bulkUpdateServiceUtil).getBulkUpdateQueue(eq(STORE_ID), eq(REQUEST_ID), anyString(),
        eq(bulkUpdateProcessDTO));
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBulkUploadInstantPickupProductEvent()), Mockito.isNull());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(fileStorageService).createBulkFile(Mockito.any(),Mockito.anyString(),Mockito.anyString());
    verify(kafkaTopicProperties, times(2)).getBulkUploadInstantPickupProductEvent();
  }

  @Test
  public void testPreProcessInstantPickupProductBulkUpsert_withPickupPoints() throws Exception {
    when(bulkUpdateServiceUtil.getBulkProcess(eq(STORE_ID), eq(REQUEST_ID), anyString(),
        eq(bulkUpdateProcessDTO), eq(0), eq(0), eq(false), eq(false))).thenReturn(bulkProcess);
    when(this.objectMapper.writeValueAsString(Mockito.any(AuditTrailInfo.class))).thenReturn(StringUtils.EMPTY);
    instantPickupProductBulkUpsertServiceBean.preProcessInstantPickupProductBulkUpsert(STORE_ID,
        REQUEST_ID, bulkUpdateProcessDTO, Collections.singleton(PICKUP_POINT_CODE));
    verify(this.objectMapper).writeValueAsString(auditTrailInfoArgumentCaptor.capture());
    verify(bulkUpdateServiceUtil).getBulkProcess(eq(STORE_ID), eq(REQUEST_ID), anyString(),
        eq(bulkUpdateProcessDTO), eq(0), eq(0), eq(false), eq(false));
    verify(bulkUpdateServiceUtil).savePreProcessBulkData(any(BulkProcess.class));
    verify(bulkUpdateServiceUtil).getBulkUpdateQueue(eq(STORE_ID), eq(REQUEST_ID), anyString(),
        eq(bulkUpdateProcessDTO));
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBulkUploadInstantPickupProductEvent()), Mockito.isNull());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(fileStorageService).createBulkFile(Mockito.any(),Mockito.anyString(),Mockito.anyString());
    verify(kafkaTopicProperties, times(2)).getBulkUploadInstantPickupProductEvent();
    Assertions.assertEquals(PICKUP_POINT_CODE,
        auditTrailInfoArgumentCaptor.getValue().getAccessiblePickupPointCodes().stream().findFirst().get());
  }

  @Test
  public void testPreProcessInstantPickupProductBulkUpsert_throwsException() throws Exception {
    when(bulkUpdateServiceUtil.getBulkProcess(eq(STORE_ID), eq(REQUEST_ID), anyString(),
        eq(bulkUpdateProcessDTO), eq(0), eq(0), eq(false), eq(false))).thenThrow(ApplicationRuntimeException.class);

    doNothing().when(this.trackerService).sendTracker(any(), any(), any(),
        any(), any());

    try {
      instantPickupProductBulkUpsertServiceBean.preProcessInstantPickupProductBulkUpsert(STORE_ID,
          REQUEST_ID, bulkUpdateProcessDTO, null);
    } catch (Exception e) {
    } finally {
      verify(this.bulkUpdateServiceUtil).getBulkProcess(eq(STORE_ID), eq(REQUEST_ID), anyString(),
          eq(bulkUpdateProcessDTO), eq(0), eq(0), eq(false), eq(false));
      verify(fileStorageService).createBulkFile(Mockito.any(),Mockito.anyString(),Mockito.anyString());
    }
  }

  @Test
  public void testProcessInstantPickupProductBulkUpsert_nullBulkProcess_throwsException()
      throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));

    ProcessorUtils.createDirectories(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR
        + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR
        + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);

    when(bulkProcessService.findByStoreIdAndBulkProcessCodeAndStatus(
        bulkUpdateQueue.getStoreId(), bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(null);

    try {
      instantPickupProductBulkUpsertServiceBean
          .processInstantPickupProductBulkUpsert(bulkUpdateQueue);
    } catch (Exception ex) {
      verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID,
          bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    }

    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void testProcessInstantPickupProductBulkUpsert_headersMismatch_throwsException()
      throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));

    ProcessorUtils.createDirectories(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR
        + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR
        + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    File file = new File(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR
      + bulkUpdateQueue.getBulkProcessCode() + File.separator
      + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    XSSFWorkbook workBook;
    InputStream is =  new FileInputStream(file);;
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileData(Mockito.any(),Mockito.any())).thenReturn(sheet);
    when(bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkUpsert(any(), any(), any(),
        any(), any())).thenReturn(false);
    when(this.bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    instantPickupProductBulkUpsertServiceBean
        .processInstantPickupProductBulkUpsert(bulkUpdateQueue);
    verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(bulkUpdateServiceUtil).authorizeUploadInstantPickupProductBulkUpsert(any(), any(), any(),
        any(), any());
    verify(bulkUpdateServiceUtil).updateBulkCncProductFinalStatus(eq(bulkProcess),
        eq(bulkUpdateQueue), eq(0), eq(0), anyList(),
        any(BulkUpdateErrorCounter.class), anyList());
    verify(notificationService).sendNotificationWithErrorFileGenerated(bulkProcess, StringUtils.EMPTY, false, false);
    verify(this.bulkProcessService, times(2)).saveOperation(Mockito.any(BulkProcess.class));
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(fileStorageService).getFileData(Mockito.any(),Mockito.any());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  private Map<String, String> getFiles() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(
        Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream("BulkUpsert" + File.separator + "BulkInstantPickupUpsert.xlsx"))),
        StandardCharsets.UTF_8);
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getFiles2() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(
        Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream("BulkUpsert" + File.separator + "BulkInstantPickupUpsert2.xlsx"))),
        StandardCharsets.UTF_8);
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getFiles_1() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(
        Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream("BulkUpsert" + File.separator + "BulkInstantPickupUpsert_1.xlsx"))),
        StandardCharsets.UTF_8);
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getFiles_newHeader() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(
        Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream("BulkUpsert" + File.separator + "BulkInstantPickupUpsert_new.xlsx"))),
        StandardCharsets.UTF_8);
    files.put("xlsx", excelData);
    return files;
  }

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    bulkUpdateProcessDTO = new BulkUpdateProcessDTO();
    bulkUpdateProcessDTO.setBulkProcessType("InstantPickupProduct");
    bulkUpdateProcessDTO.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkUpdateProcessDTO.setFileName("test.xlsx");
    bulkUpdateProcessDTO.setFileContent("Hello".getBytes(StandardCharsets.UTF_8));
    bulkUpdateProcessDTO.setUpdatedBy("developer");
    bulkUpdateProcessDTO.setClientHost("localhost");
    bulkUpdateProcessDTO.setPrivilegedMap(new HashMap<>());

    bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(RANDOM_BULK_PROCESS_CODE);
    bulkUpdateQueue.setBusinessPartnerCode("TOA-14961");
    bulkUpdateQueue.setBulkProcessType("Update");
    bulkUpdateQueue.setClientHost("localhost");
    bulkUpdateQueue.setFileName("test.xlsx");
    bulkUpdateQueue.setPrivilegedMap(new HashMap<>());
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setUpdatedBy("developer");

    bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType("InstantPickupProduct");
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedBy("developer");
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setId("12345");
    bulkProcess.setStartDate(new Date());
    bulkProcess.setStoreId("10001");
    bulkProcess.setUpdatedBy("developer");
    bulkProcess.setUpdatedDate(new Date());
    bulkProcess.setBulkProcessCode(RANDOM_BULK_PROCESS_CODE);
    bulkProcess.setInternationalMerchant(false);

    auditTrailInfo = new AuditTrailInfo();
    auditTrailInfo.setRequestId(REQUEST_ID);
    auditTrailInfo.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    auditTrailInfo.setRemoteAddress(CLIENT_HOST);
    auditTrailInfo.setUsername(USERNAME);

    upsertOfflineItemRequest = new UpsertOfflineItemRequest();
    upsertOfflineItemRequest.setItemSku(ITEM_SKU);
    upsertOfflineItemRequest.setPickupPointCode(PICKUP_POINT_CODE);
    upsertOfflineItemRequest.setListPrice(LIST_PRICE);
    upsertOfflineItemRequest.setOfferPrice(OFFER_PRICE);
    upsertOfflineItemRequest.setStock(STOCK);
    upsertOfflineItemRequest.setFileName(FILE_NAME);

    upsertOfflineItemRequest1 = new UpsertOfflineItemRequest();
    upsertOfflineItemRequest1.setItemSku(ITEM_SKU_1);
    upsertOfflineItemRequest1.setPickupPointCode(PICKUP_POINT_CODE_1);
    upsertOfflineItemRequest1.setListPrice(LIST_PRICE_1);
    upsertOfflineItemRequest1.setOfferPrice(OFFER_PRICE_1);
    upsertOfflineItemRequest1.setStock(STOCK);
    upsertOfflineItemRequest1.setFileName(FILE_NAME);

    upsertOfflineItemRequest2 = new UpsertOfflineItemRequest();
    upsertOfflineItemRequest2.setItemSku(ITEM_SKU_2);
    upsertOfflineItemRequest2.setPickupPointCode(PICKUP_POINT_CODE_2);
    upsertOfflineItemRequest2.setListPrice(LIST_PRICE_2);
    upsertOfflineItemRequest2.setOfferPrice(OFFER_PRICE_2);
    upsertOfflineItemRequest2.setStock(STOCK);
    upsertOfflineItemRequest2.setFileName(FILE_NAME);

    upsertOfflineItemRequests = new ArrayList<>();
    upsertOfflineItemRequests.add(upsertOfflineItemRequest);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest1);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest2);

    upsertOfflineItemFailedResponse = new UpsertOfflineItemFailedResponse();
    upsertOfflineItemFailedResponse.setItemSku(ITEM_SKU_2);
    upsertOfflineItemFailedResponse.setPickupPointCode(PICKUP_POINT_CODE_2);
    upsertOfflineItemFailedResponse.setErrorCode(ERROR_CODE);

    upsertOfflineItemFailedResponses = Collections.singletonList(upsertOfflineItemFailedResponse);

    when(bulkProcessService.findByStoreIdAndBulkProcessCodeAndStatus(
        bulkUpdateQueue.getStoreId(), bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING))
        .thenReturn(bulkProcess);

    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean,
        "bulkProcessUpsertBatchSize", 100);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "itemPickupPointListFetchSize", 1);

    bulkProcessDataList = new ArrayList<>();
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU_2);
    row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setParentProduct(ITEM_SKU);
    bulkProcessDataList.add(bulkProcessData);

    bulkUpdateEventModel = new BulkUpdateEventModel();
    bulkUpdateEventModel.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkUpdateEventModel.setStoreId(STORE_ID);
    bulkUpdateEventModel.setRowNumbers(Arrays.asList(1));

    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(true);
    profileResponse.setCompany(companyDTO);

    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);

    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue("0");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(),
      Mockito.anyString())).thenReturn(systemParameterConfig);
  }

  @Test
  public void testProcessInstantPickupProductBulkUpsert_withFastExcelEnabled_success() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    InputStream excelInputStream = new ByteArrayInputStream(excelFile);

    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bulkUpsertUsingFastExcelEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bulkMaxNumberOfRows", 1000);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "validateBulkMaxNumberOfRows", false);

    bulkProcess.setNotes("");
    when(fileStorageServiceBean.openGcsInputStream(any(BulkUpdateQueue.class), any(BulkProcess.class))).thenReturn(
        excelInputStream);
    when(bulkUpdateServiceUtil.validateForHeaderMismatchAndGetUpdatedHeaders(any(), any())).thenReturn(true);
    when(bulkProcessService.findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, bulkUpdateQueue.getBulkProcessCode(),
        BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(this.bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.PICKUP_UPSERT_SWITCH)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PICKUP_UPSERT_SWITCH, "true",
            SystemParameterConfigNames.PICKUP_UPSERT_SWITCH));
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(), any(AuditTrailInfo.class))).thenReturn(
        upsertOfflineItemFailedResponses);
    when(objectMapper.writeValueAsString(Mockito.any(AuditTrailInfo.class))).thenReturn(StringUtils.EMPTY);
    doNothing().when(bulkUpdateServiceUtil)
        .updateBulkCncProductFinalStatus(any(), any(), anyInt(), anyInt(), anyList(), any(), anyList());
    instantPickupProductBulkUpsertServiceBean.processInstantPickupProductBulkUpsert(bulkUpdateQueue);

    verify(fileStorageServiceBean).openGcsInputStream(any(BulkUpdateQueue.class), any(BulkProcess.class));
    verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, bulkUpdateQueue.getBulkProcessCode(),
        BulkProcess.STATUS_PENDING);
    verify(bulkUpdateServiceUtil).validateForHeaderMismatchAndGetUpdatedHeaders(any(), any());
    verify(this.bulkProcessService, times(2)).saveOperation(Mockito.any(BulkProcess.class));
    verify(objectMapper).writeValueAsString(Mockito.any(AuditTrailInfo.class));
    verify(this.bulkUpdateServiceUtil).getFinalDataList(Mockito.anyMap(), Mockito.anyList(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    Mockito.verify(bulkUpdateServiceUtil)
        .validateDuplicateFbbPickupPoints(Mockito.anyString(), Mockito.anyList(), Mockito.eq(true), Mockito.any());
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
  }

  @Test
  public void testProcessInstantPickupProductBulkUpsert_withFastExcelEnabled_headerMismatch() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    InputStream excelInputStream = new ByteArrayInputStream(excelFile);

    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bulkUpsertUsingFastExcelEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bulkMaxNumberOfRows", 1000);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "validateBulkMaxNumberOfRows", false);

    bulkProcess.setNotes("");
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.PICKUP_UPSERT_SWITCH)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PICKUP_UPSERT_SWITCH, "true",
            SystemParameterConfigNames.PICKUP_UPSERT_SWITCH));
    when(fileStorageServiceBean.openGcsInputStream(any(BulkUpdateQueue.class), any(BulkProcess.class))).thenReturn(
        excelInputStream);
    when(bulkUpdateServiceUtil.validateForHeaderMismatchAndGetUpdatedHeaders(any(), any())).thenReturn(false);
    when(bulkProcessService.findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, bulkUpdateQueue.getBulkProcessCode(),
        BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(this.bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    when(objectMapper.writeValueAsString(Mockito.any(AuditTrailInfo.class))).thenReturn(StringUtils.EMPTY);
    doNothing().when(bulkUpdateServiceUtil)
        .updateBulkCncProductFinalStatus(any(), any(), anyInt(), anyInt(), anyList(), any(), anyList());

    instantPickupProductBulkUpsertServiceBean.processInstantPickupProductBulkUpsert(bulkUpdateQueue);

    verify(fileStorageServiceBean).openGcsInputStream(any(BulkUpdateQueue.class), any(BulkProcess.class));
    verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, bulkUpdateQueue.getBulkProcessCode(),
        BulkProcess.STATUS_PENDING);
    verify(bulkUpdateServiceUtil).validateForHeaderMismatchAndGetUpdatedHeaders(any(), any());
    verify(this.bulkProcessService, times(2)).saveOperation(Mockito.any(BulkProcess.class));
    verify(objectMapper).writeValueAsString(Mockito.any(AuditTrailInfo.class));
    verify(bulkUpdateServiceUtil).updateBulkCncProductFinalStatus(any(), any(BulkUpdateQueue.class), anyInt(), anyInt(),
        anyList(), any(BulkUpdateErrorCounter.class), anyList());
  }

  @Test
  public void testProcessInstantPickupProductBulkUpsert_withFastExcelEnabled_exceptionDuringParsing() throws Exception {
    InputStream invalidInputStream = new ByteArrayInputStream("invalid excel data".getBytes());

    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bulkUpsertUsingFastExcelEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bulkMaxNumberOfRows", 1000);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "validateBulkMaxNumberOfRows", false);

    bulkProcess.setNotes("");
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.PICKUP_UPSERT_SWITCH)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PICKUP_UPSERT_SWITCH, "true",
            SystemParameterConfigNames.PICKUP_UPSERT_SWITCH));
    when(fileStorageServiceBean.openGcsInputStream(any(BulkUpdateQueue.class), any(BulkProcess.class))).thenReturn(
        invalidInputStream);
    when(bulkProcessService.findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, bulkUpdateQueue.getBulkProcessCode(),
        BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(this.bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    when(objectMapper.writeValueAsString(Mockito.any(AuditTrailInfo.class))).thenReturn(StringUtils.EMPTY);
    doNothing().when(bulkUpdateServiceUtil)
        .updateBulkCncProductFinalStatus(any(), any(), anyInt(), anyInt(), anyList(), any(), anyList());

    instantPickupProductBulkUpsertServiceBean.processInstantPickupProductBulkUpsert(bulkUpdateQueue);

    verify(fileStorageServiceBean).openGcsInputStream(any(BulkUpdateQueue.class), any(BulkProcess.class));
    verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, bulkUpdateQueue.getBulkProcessCode(),
        BulkProcess.STATUS_PENDING);
    verify(this.bulkProcessService, times(2)).saveOperation(Mockito.any(BulkProcess.class));
    verify(objectMapper).writeValueAsString(Mockito.any(AuditTrailInfo.class));
    verify(bulkUpdateServiceUtil).updateBulkCncProductFinalStatus(any(), any(BulkUpdateQueue.class), anyInt(), anyInt(),
        anyList(), any(BulkUpdateErrorCounter.class), anyList());
  }

  @Test
  public void testProcessInstantPickupProductBulkUpsert_withFastExcelEnabled_emptyHeaders() throws Exception {
    XSSFWorkbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Sheet1");
    // Create a header row but don't add any cells to it, resulting in 0 cells
    sheet.createRow(0);

    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    workbook.write(outputStream);
    workbook.close();
    InputStream excelInputStream = new ByteArrayInputStream(outputStream.toByteArray());
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bulkUpsertUsingFastExcelEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bulkMaxNumberOfRows", 1000);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "validateBulkMaxNumberOfRows", false);
    bulkProcess.setNotes("");
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.PICKUP_UPSERT_SWITCH)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PICKUP_UPSERT_SWITCH, "true",
            SystemParameterConfigNames.PICKUP_UPSERT_SWITCH));
    when(fileStorageServiceBean.openGcsInputStream(any(BulkUpdateQueue.class), any(BulkProcess.class))).thenReturn(
        excelInputStream);
    when(bulkProcessService.findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, bulkUpdateQueue.getBulkProcessCode(),
        BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(this.bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    when(objectMapper.writeValueAsString(Mockito.any(AuditTrailInfo.class))).thenReturn(StringUtils.EMPTY);
    doNothing().when(bulkUpdateServiceUtil).saveBulkProcessAsAborted(any(BulkProcess.class), any(BulkUpdateQueue.class),
        any(BulkUpdateErrorCounter.class));
    doNothing().when(bulkUpdateServiceUtil)
        .updateBulkCncProductFinalStatus(any(), any(), anyInt(), anyInt(), anyList(), any(), anyList());
    doNothing().when(trackerService).sendTracker(any(), any(), any(), any(), any());
    instantPickupProductBulkUpsertServiceBean.processInstantPickupProductBulkUpsert(bulkUpdateQueue);
    verify(bulkUpdateServiceUtil).saveBulkProcessAsAborted(any(BulkProcess.class), any(BulkUpdateQueue.class),
        any(BulkUpdateErrorCounter.class));
    verify(bulkUpdateServiceUtil).updateBulkCncProductFinalStatus(any(), any(BulkUpdateQueue.class), anyInt(), anyInt(),
        anyList(), any(BulkUpdateErrorCounter.class), anyList());
    verify(trackerService).sendTracker(any(), any(), any(), any(), any());
    verify(notificationService).sendNotificationWithErrorFileGenerated(any(BulkProcess.class), anyString(), eq(false),
        eq(false));
    verify(fileStorageServiceBean).openGcsInputStream(any(BulkUpdateQueue.class), any(BulkProcess.class));
    verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, bulkUpdateQueue.getBulkProcessCode(),
        BulkProcess.STATUS_PENDING);
    verify(this.bulkProcessService, times(2)).saveOperation(Mockito.any(BulkProcess.class));
    verify(objectMapper).writeValueAsString(Mockito.any(AuditTrailInfo.class));
  }

  @Test
  public void testProcessInstantPickupProductBulkUpsert_SwitchTrue() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));

    ProcessorUtils.createDirectories(
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    when(bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkUpsert(any(), any(), any(), any(),
        any())).thenReturn(true);
    when(this.bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    File file = new File(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR
      + bulkUpdateQueue.getBulkProcessCode() + File.separator
      + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    XSSFWorkbook workBook;
    InputStream is =  new FileInputStream(file);;
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileData(Mockito.any(),Mockito.any())).thenReturn(sheet);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.PICKUP_UPSERT_SWITCH)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PICKUP_UPSERT_SWITCH, "true",
            SystemParameterConfigNames.PICKUP_UPSERT_SWITCH));
    Mockito.when(bulkUpdateServiceUtil.validateDuplicateFbbPickupPoints(Mockito.anyString(),
        Mockito.anyList(), Mockito.eq(true), Mockito.any())).thenReturn(0);
    instantPickupProductBulkUpsertServiceBean.processInstantPickupProductBulkUpsert(bulkUpdateQueue);

    verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(bulkUpdateServiceUtil).authorizeUploadInstantPickupProductBulkUpsert(any(), any(), any(), any(), any());
    verify(this.bulkProcessService, times(2)).saveOperation(Mockito.any(BulkProcess.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(this.objectMapper, times(4)).writeValueAsString(Mockito.any());
    verify(this.bulkUpdateServiceUtil).getFinalDataList(Mockito.anyMap(), Mockito.anyList(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    verify(fileStorageService).getFileData(Mockito.any(),Mockito.any());
    Mockito.verify(bulkUpdateServiceUtil).validateDuplicateFbbPickupPoints(Mockito.anyString(),
        Mockito.anyList(), Mockito.eq(true), Mockito.any());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void testProcessInstantPickupProductBulkUpsert_SwitchTrue_accessiblePickupValidation() throws Exception {
    auditTrailInfo = new AuditTrailInfo();
    auditTrailInfo.setAccessiblePickupPointCodes(Set.of(PICKUP_POINT_CODE, PICKUP_POINT_CODE_1));
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    when(bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkUpsert(any(), any(), any(), any(),
        any())).thenReturn(true);
    when(this.bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    File file = new File(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR
        + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    XSSFWorkbook workBook;
    InputStream is =  new FileInputStream(file);;
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    bulkProcess.setNotes(NOTES);
    when(bulkProcessService.findByStoreIdAndBulkProcessCodeAndStatus(
        bulkUpdateQueue.getStoreId(), bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING))
        .thenReturn(bulkProcess);
    Mockito.when(fileStorageService.getFileData(Mockito.any(),Mockito.any())).thenReturn(sheet);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.PICKUP_UPSERT_SWITCH)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PICKUP_UPSERT_SWITCH, "true",
            SystemParameterConfigNames.PICKUP_UPSERT_SWITCH));
    Mockito.when(bulkUpdateServiceUtil.validateDuplicateFbbPickupPoints(Mockito.anyString(),
        Mockito.anyList(), Mockito.eq(true), Mockito.any())).thenReturn(0);
    Mockito.when(this.objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class)).thenReturn(auditTrailInfo);
    instantPickupProductBulkUpsertServiceBean.processInstantPickupProductBulkUpsert(bulkUpdateQueue);
    verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(bulkUpdateServiceUtil).authorizeUploadInstantPickupProductBulkUpsert(any(), any(), any(), any(), any());
    verify(this.bulkProcessService, times(2)).saveOperation(Mockito.any(BulkProcess.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(this.objectMapper, times(4)).writeValueAsString(Mockito.any());
    verify(this.bulkUpdateServiceUtil).getFinalDataList(Mockito.anyMap(), Mockito.anyList(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    verify(fileStorageService).getFileData(Mockito.any(),Mockito.any());
    verify(this.objectMapper).readValue(NOTES, AuditTrailInfo.class);
    Mockito.verify(bulkUpdateServiceUtil).validateDuplicateFbbPickupPoints(Mockito.anyString(),
        Mockito.anyList(), Mockito.eq(true), Mockito.any());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(bulkProcessService, productBusinessPartnerRepository,
        kafkaProducer, bulkUpdateServiceUtil, systemParameterConfigService, objectMapper, bulkProcessDataService,
        businessPartnerRepository, fileStorageService, pickupPointService, kafkaTopicProperties,
        xProductOutboundService, fileStorageServiceBean);
  }

  @Test
  public void processEventSuccessTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccessBliBliSkuInTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
     PRODUCT_SKU_1, null);
  }


  @Test
  public void processEventSuccessBliBliSkuIn_accessiblePPCodesTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes(
        "{\"accessiblePickupPointCodes\":[\"PP-3427956\",\"PP-3003332\",\"PP-3520305\"]}");
    auditTrailInfo = new AuditTrailInfo();
    auditTrailInfo.setAccessiblePickupPointCodes(Set.of(PICKUP_POINT_CODE, PICKUP_POINT_CODE_1));
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(),
        BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(
        anyList(), any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(
        row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        auditTrailInfo);
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page =
        new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),
            Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(
        itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(),
        BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(systemParameterConfigService, times(2))
        .findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(this.objectMapper).readValue(bulkProcess.getNotes(), AuditTrailInfo.class);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccessBliBliSkuIn_notAccessiblePPCodesTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes(
        "{\"accessiblePickupPointCodes\":[\"PP-3427956\",\"PP-3003332\",\"PP-3520305\"]}");
    auditTrailInfo = new AuditTrailInfo();
    auditTrailInfo.setAccessiblePickupPointCodes(Set.of(PICKUP_POINT_CODE_2));
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(auditTrailInfo);
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(this.objectMapper).readValue(bulkProcess.getNotes(), AuditTrailInfo.class);
    verify(this.objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void processEventSuccessMPPTrueTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.ONLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.ONLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccessBopisAndCategorySwitchOnTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCategoryRestrictionEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCncRestrictionEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.ONLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
        itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
        profileResponse.getCompany().isInternationalFlag(),
        PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccessBopisAndCategorySwitchOn2Test() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCategoryRestrictionEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCncRestrictionEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCategoryValidationForSellerTypes",
        "CM");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.ONLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.ONLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setMerchantType("CM");
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
        itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
        profileResponse.getCompany().isInternationalFlag(),
        PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccessBopisAndCategorySwitchOn3Test() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCategoryRestrictionEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCncRestrictionEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCategoryValidationForSellerTypes",
        "CM");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.ONLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setDimensionsMissing(true);
    itemPickupPointListingL3Response.setProductType(1);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setMerchantType("CM");
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
        itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
        profileResponse.getCompany().isInternationalFlag(),
        PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccessBopisAndCategorySwitchOn6Test() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCategoryRestrictionEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCncRestrictionEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCategoryValidationForSellerTypes",
        "CM");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.B2B_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.ONLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setDimensionsMissing(true);
    itemPickupPointListingL3Response.setProductType(1);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setMerchantType("CM");
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void processEventSuccessBopisAndCategorySwitchOn5Test() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCategoryRestrictionEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCncRestrictionEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCategoryValidationForSellerTypes",
        "CM");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.ONLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setDimensionsMissing(true);
    itemPickupPointListingL3Response.setProductType(3);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setMerchantType("CM");
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void processEventSuccessBopisAndCategorySwitchOn4Test() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCategoryRestrictionEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCncRestrictionEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "bopisCategoryValidationForSellerTypes",
        "CM");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.ONLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.ONLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setDimensionsMissing(true);
    itemPickupPointListingL3Response.setMissingFields(Set.of(Constant.DIMENSIONS_MISSING));
    itemPickupPointListingL3Response.setProductType(2);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setMerchantType("CM");
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void processEventSuccessMPPTrueOfflineCaseTest() throws Exception {
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setFreeSample(true);
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU_1, null);
  }

  @Test
  public void processBulkUpsertInstoreSwitchOnTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "instoreNewFlowEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
      any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.ONLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
      new AuditTrailInfo());
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
      Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU_1, null);
  }

  @Test
  public void processBulkUpsertInstoreSwitchOnWithOnlineTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "instoreNewFlowEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
      any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.ONLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
      new AuditTrailInfo());
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
      Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setMerchantType("CM");
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU_1, null);
  }

  @Test
  public void processBulkUpsertInstoreSwitchOnWithOnlineAndMissingFieldsTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "instoreNewFlowEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
      any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.ONLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
      new AuditTrailInfo());
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setMissingFields(Set.of(Constant.DIMENSIONS_MISSING));
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
      Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setMerchantType("CM");
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void processBulkUpsertInstoreSwitchOnWithCNCAndMissingFieldsTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "instoreNewFlowEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
      any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.ONLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
      new AuditTrailInfo());
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setMissingFields(Set.of(Constant.DIMENSIONS_MISSING));
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
      Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setMerchantType("CM");
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void processBulkUpsertInstoreSwitchOnWithOfflineMissingFieldsTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "instoreNewFlowEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
      any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
      new AuditTrailInfo());
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setMissingFields(Set.of(Constant.DIMENSIONS_MISSING));
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
      Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setMerchantType("CM");
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU_1, null);
  }



  @Test
  public void processEventSuccessMPPTrueOfflineCaseValidationTest() throws Exception {
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setWebSyncStock(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    List<UpsertOfflineItemFailedResponse> responses = new ArrayList<>();
    upsertOfflineItemFailedResponse.setPickupPointCode(PICKUP_POINT_CODE);
    responses.add(upsertOfflineItemFailedResponse);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(responses);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(null);

    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    Mockito.verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccessMPPTrueOfflineCaseValidationTest_cncForWarehouseTrue()
      throws Exception {
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean,
        "cncForWarehouseFeatureSwitch", true);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setWebSyncStock(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),
            Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean,
        "multiPickupPointEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(),
        BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(
        itemsPriceStockImagesUpdateResponse);
    Page<ItemPickupPointListingL3Response> page =
        new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    List<UpsertOfflineItemFailedResponse> responses = new ArrayList<>();
    upsertOfflineItemFailedResponse.setPickupPointCode(PICKUP_POINT_CODE);
    responses.add(upsertOfflineItemFailedResponse);
    when(productBusinessPartnerRepository.upsertOfflineItems(
        anyList(), any(AuditTrailInfo.class))).thenReturn(
        responses);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN_CNC_1P, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(
        row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(null);

    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(),
        BulkProcessData.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    Mockito.verify(systemParameterConfigService, times(2))
        .findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(
        bulkProcessDataList,
        itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
        profileResponse.getCompany().isInternationalFlag(), PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccessMPPTrueOfflineCaseValidationFaasSelerSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "faasFeatureSwitch", true);
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true));
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setWebSyncStock(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(), Mockito.any(ProductVariantUpdateRequest.class),
        eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    List<UpsertOfflineItemFailedResponse> responses = new ArrayList<>();
    upsertOfflineItemFailedResponse.setPickupPointCode(PICKUP_POINT_CODE);
    responses.add(upsertOfflineItemFailedResponse);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(responses);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(null);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    Mockito.verify(systemParameterConfigService, times(2))
        .findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
        itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
        profileResponse.getCompany().isInternationalFlag(), PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccessMPPTrueOfflineCaseValidationFaasSellerTest() throws Exception {
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true));
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setWebSyncStock(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(), Mockito.any(ProductVariantUpdateRequest.class),
        eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    List<UpsertOfflineItemFailedResponse> responses = new ArrayList<>();
    upsertOfflineItemFailedResponse.setPickupPointCode(PICKUP_POINT_CODE);
    responses.add(upsertOfflineItemFailedResponse);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(responses);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(null);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    Mockito.verify(systemParameterConfigService, times(2))
        .findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
        itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
        profileResponse.getCompany().isInternationalFlag(), PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccessMPPTrueOfflineCaseValidationCncNullTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    List<UpsertOfflineItemFailedResponse> responses = new ArrayList<>();
    upsertOfflineItemFailedResponse.setPickupPointCode(PICKUP_POINT_CODE);
    responses.add(upsertOfflineItemFailedResponse);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(responses);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, null);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(null);
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);

    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccessMPPTrueInternationalMerchantTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    bulkProcess.setInternationalMerchant(true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, "1.0");
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.ONLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccessMPPTrueInternationalMerchantOfflineTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    bulkProcess.setInternationalMerchant(true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, "1.0");
    row.put(ExcelHeaderNames.CNC_STATUS_EN, "0.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);

    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventExceptionTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    ApplicationRuntimeException applicationRuntimeException = new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        GenericErrorMessages.INVALID_STATUS_VALUE_ON_STATUS_FIELD);
    Mockito.doThrow(applicationRuntimeException).when(
        productBusinessPartnerRepository).upsertOfflineItems(anyList(),
            any(AuditTrailInfo.class));
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);

    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
  }

  @Test
  public void processEventExceptionInvalidStatusTest() throws Exception {
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    Mockito.doThrow(Exception.class).when(
        productBusinessPartnerRepository).upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class));
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, "3.0");
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.ONLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());

    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);

    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
  }

  @Test
  public void processEventExceptionInvalidCncStatusTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.ONLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, "3.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());

    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);

    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).writeValueAsString(Mockito.any(BulkCncUpsertErrorDTO.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void processEventEmptyDataListTest() throws Exception {
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        null);

    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
  }

  @Test
  public void setFinalStatusAndNotificationOnInstantPickupUpsertTest() throws Exception {
    List<BulkProcessData> bulkProcessData = getBulkProcessData();
    bulkProcessData.forEach(bulkProcessData1 -> {
      bulkProcessData1.setBulkRequestData("");
      bulkProcessData1.setErrorMessage("");
    });
    bulkProcess.setNotes("");
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Mockito.anyString(),
        Mockito.any(BulkProcess.class))).thenReturn(bulkProcessData);
    auditTrailInfo.setFileName(FILE_NAME);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(auditTrailInfo);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(BulkCncUpsertErrorDTO.class))).thenReturn(
        new BulkCncUpsertErrorDTO(ITEM_SKU, PICKUP_POINT_CODE, "Error"));

    LinkedHashMap<String, String> bulkProcessDataMap = new LinkedHashMap<>();
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(bulkProcessDataMap);

    bulkProcess.setTotalCount(4);
    instantPickupProductBulkUpsertServiceBean.setFinalStatusAndNotificationOnInstantPickupUpsert(bulkProcess,
        Constant.STORE_ID);

    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(bulkUpdateServiceUtil).updateBulkCncProductFinalStatus(eq(bulkProcess), any(BulkUpdateQueue.class), eq(2),
        eq(4), anyList(), any(BulkUpdateErrorCounter.class), anyList());
    verify(notificationService).sendNotificationWithErrorFileGenerated(eq(bulkProcess), Mockito.anyString(), eq(false),
        eq(false));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
    verify(objectMapper, times(2)).readValue(Mockito.anyString(), Mockito.eq(BulkCncUpsertErrorDTO.class));
    verify(objectMapper, times(2)).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(fileStorageService).getDownloadLink(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void setFinalStatusAndNotificationOnInstantPickupUpsertInTest() throws Exception {
    List<BulkProcessData> bulkProcessData = getBulkProcessData();
    bulkProcessData.forEach(bulkProcessData1 -> {
      bulkProcessData1.setBulkRequestData("");
      bulkProcessData1.setErrorMessage("");
    });
    bulkProcess.setNotes("");
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Mockito.anyString(),
        Mockito.any(BulkProcess.class))).thenReturn(bulkProcessData);
    auditTrailInfo.setFileName(FILE_NAME);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        auditTrailInfo);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(BulkCncUpsertErrorDTO.class))).thenReturn(
        new BulkCncUpsertErrorDTO(ITEM_SKU, PICKUP_POINT_CODE, "Error"));

    LinkedHashMap<String, String> bulkProcessDataMap = new LinkedHashMap<>();
    bulkProcessDataMap.put(ExcelHeaderNames.BLIBLI_SKU_IN, ITEM_SKU);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(bulkProcessDataMap);

    bulkProcess.setTotalCount(4);
    instantPickupProductBulkUpsertServiceBean.setFinalStatusAndNotificationOnInstantPickupUpsert(bulkProcess,
        Constant.STORE_ID);

    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(bulkUpdateServiceUtil).updateBulkCncProductFinalStatus(eq(bulkProcess), any(BulkUpdateQueue.class), eq(2),
        eq(4), anyList(), any(BulkUpdateErrorCounter.class), anyList());
    verify(notificationService).sendNotificationWithErrorFileGenerated(eq(bulkProcess), Mockito.anyString(), eq(false),
        eq(false));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
    verify(objectMapper, times(2)).readValue(Mockito.anyString(), Mockito.eq(BulkCncUpsertErrorDTO.class));
    verify(objectMapper, times(2)).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(fileStorageService).createBulkFile(Mockito.any(),Mockito.anyString(),Mockito.anyString());
    verify(fileStorageService).getDownloadLink(Mockito.anyString(),Mockito.anyString(),
      Mockito.anyString(),Mockito.anyString());
  }

  @Test
  public void setFinalStatusAndNotificationOnInstantPickupUpsertbulkProcessDataFailedMapNullTest() throws Exception {
    List<BulkProcessData> bulkProcessData = getBulkProcessData();
    bulkProcessData.forEach(bulkProcessData1 -> {
      bulkProcessData1.setBulkRequestData("");
      bulkProcessData1.setErrorMessage("");
    });
    bulkProcess.setNotes("");
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Mockito.anyString(),
        Mockito.any(BulkProcess.class))).thenReturn(bulkProcessData);
    auditTrailInfo.setFileName(FILE_NAME);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        auditTrailInfo);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(BulkCncUpsertErrorDTO.class))).thenReturn(
        new BulkCncUpsertErrorDTO(ITEM_SKU, PICKUP_POINT_CODE, "Error"));

    LinkedHashMap<String, String> bulkProcessDataMap = new LinkedHashMap<>();
    bulkProcessDataMap.put(ExcelHeaderNames.BLIBLI_SKU_IN, ITEM_SKU);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(null);

    try {
      bulkProcess.setTotalCount(4);
      Assertions.assertThrows(RuntimeException.class,
          () -> instantPickupProductBulkUpsertServiceBean.setFinalStatusAndNotificationOnInstantPickupUpsert(
              bulkProcess, Constant.STORE_ID));
    } finally {
      verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
      verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
      verify(objectMapper, times(2)).readValue(Mockito.anyString(), Mockito.eq(BulkCncUpsertErrorDTO.class));
      verify(objectMapper, times(2)).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    }
  }

  private List<BulkProcessData> getBulkProcessData() {
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setStatus(BulkProcessData.STATUS_SUCCESS);
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setSystemErrorCount(1);
    bulkProcessData2.setRowNumber(0);
    BulkProcessData bulkProcessData3 = new BulkProcessData();
    bulkProcessData3.setStatus(BulkProcessData.STATUS_SUCCESS);
    BulkProcessData bulkProcessData4 = new BulkProcessData();
    bulkProcessData4.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData4.setErrorMessage(Constant.SYSTEM_ERROR);
    bulkProcessData4.setInputErrorCount(1);
    bulkProcessData4.setRowNumber(1);
    return Arrays.asList(bulkProcessData1, bulkProcessData2, bulkProcessData3, bulkProcessData4);
  }

  @Test
  public void setFinalStatusAndNotificationOnInstantPickupUpsertAllFailureTest() throws Exception {
    List<BulkProcessData> dataList = getBulkProcessData();
    for (BulkProcessData bulkProcessData : dataList) {
      bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
      bulkProcessData.setBulkRequestData("");
      bulkProcessData.setErrorMessage("");
    }
    bulkProcess.setNotes("");
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Mockito.anyString(),
        Mockito.any(BulkProcess.class))).thenReturn(dataList);
    auditTrailInfo.setFileName(FILE_NAME);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        auditTrailInfo);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(BulkCncUpsertErrorDTO.class))).thenReturn(
        new BulkCncUpsertErrorDTO(ITEM_SKU, PICKUP_POINT_CODE, "Error"));
    LinkedHashMap<String, String> bulkProcessDataMap = new LinkedHashMap<>();
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(bulkProcessDataMap);
    bulkProcess.setTotalCount(4);
    instantPickupProductBulkUpsertServiceBean.setFinalStatusAndNotificationOnInstantPickupUpsert(bulkProcess,
        Constant.STORE_ID);

    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(bulkUpdateServiceUtil).updateBulkCncProductFinalStatus(eq(bulkProcess), any(BulkUpdateQueue.class), eq(0),
        eq(4), anyList(), any(BulkUpdateErrorCounter.class), anyList());
    verify(notificationService).sendNotificationWithErrorFileGenerated(eq(bulkProcess), Mockito.anyString(), eq(false),
        eq(false));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
    verify(objectMapper, times(4)).readValue(Mockito.anyString(), Mockito.eq(BulkCncUpsertErrorDTO.class));
    verify(objectMapper, times(4)).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(fileStorageService).createBulkFile(Mockito.any(),Mockito.anyString(),Mockito.anyString());
    verify(fileStorageService).getDownloadLink(Mockito.anyString(),Mockito.anyString(),
      Mockito.anyString(),Mockito.anyString());
  }

  @Test
  public void setFinalStatusAndNotificationOnInstantPickupUpsertAllSuccessTest() throws Exception {
    List<BulkProcessData> dataList = getBulkProcessData();
    for (BulkProcessData bulkProcessData : dataList) {
      bulkProcessData.setStatus(BulkProcessData.STATUS_SUCCESS);
      bulkProcessData.setBulkRequestData("");
    }
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    bulkProcess.setNotes("");
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Mockito.anyString(),
        Mockito.any(BulkProcess.class))).thenReturn(dataList);
    auditTrailInfo.setFileName(FILE_NAME);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        auditTrailInfo);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(BulkCncUpsertErrorDTO.class))).thenReturn(
        new BulkCncUpsertErrorDTO(ITEM_SKU, PICKUP_POINT_CODE, "Error"));
    bulkProcess.setTotalCount(4);
    instantPickupProductBulkUpsertServiceBean.setFinalStatusAndNotificationOnInstantPickupUpsert(bulkProcess,
        Constant.STORE_ID);

    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(bulkUpdateServiceUtil).updateBulkCncProductFinalStatus(eq(bulkProcess), any(BulkUpdateQueue.class), eq(4),
        eq(4), anyList(), any(BulkUpdateErrorCounter.class), anyList());
    verify(notificationService).sendNotificationWithErrorFileGenerated(eq(bulkProcess), Mockito.anyString(), eq(false),
        eq(false));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
  }

  @Test
  public void processEventWithEmptyStockTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_1);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    row.put(ExcelHeaderNames.STOCK_EN, "");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);

    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void processEventWithEmptyPPCodeTest() throws Exception {
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, "");
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);

    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(objectMapper).writeValueAsString(Mockito.any(BulkCncUpsertErrorDTO.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void processEventWithEmptyOfferPriceTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));

    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void processEventWithEmptyListPriceTest() throws Exception {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "1000");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);

    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(),
      Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventWithEmptyListPriceFetchSizeZeroTest() throws Exception {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "1000");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    ItemPickupPointListingL3Response itemPickupPointListingL3Response1 = new ItemPickupPointListingL3Response();
    BeanUtils.copyProperties(itemPickupPointListingL3Response, itemPickupPointListingL3Response1);
    itemPickupPointListingL3Response1.setPickupPointCode(PICKUP_POINT_CODE_2);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response, itemPickupPointListingL3Response1));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(1), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU_1, null);
  }


  @Test
  public void processEventWithEmptyOnlineStatusTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "1000");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, "");
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    pickupPointResponse.setFbbActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventWithNaNOfferPriceTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "NaN");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "10");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);

    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));

    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void processEventWithNaNListPriceTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "10000");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "NaN");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "10");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);

    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
  }

  @Test
  public void processEventSuccessUpdateFailedTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(ITEM_SKU);
    itemBasicDetailV2Response.setMerchantSku("merchantSku");
    when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU_2))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    pickupPointResponse.setFbbActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
      new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE,
        "Update Failed", PICKUP_POINT_CODE)));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);

    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU_2);
  }

  @Test
  public void processEventSuccessUpdateFailedNullResponseTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(ITEM_SKU);
    itemBasicDetailV2Response.setMerchantSku("merchantSku");
    when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU_2))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    pickupPointResponse.setFbbActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
        new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE,
            "Update Failed", PICKUP_POINT_CODE)));
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(new ArrayList<>());
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);

    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void processEventSuccessUpdateFailedFaasSellerTest() throws Exception {
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true));
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(ITEM_SKU);
    itemBasicDetailV2Response.setMerchantSku("merchantSku");
    when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU_2)).thenReturn(
        Collections.singletonList(itemBasicDetailV2Response));
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    pickupPointResponse.setFbbActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
        new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE, "Update Failed",
            PICKUP_POINT_CODE)));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(), Mockito.any(ProductVariantUpdateRequest.class),
        eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU_2);
  }

  @Test
  public void processEventSuccessUpdateFailedFaasSellerWithSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "faasFeatureSwitch", true);
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true));
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(ITEM_SKU);
    itemBasicDetailV2Response.setMerchantSku("merchantSku");
    when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU_2)).thenReturn(
        Collections.singletonList(itemBasicDetailV2Response));
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    pickupPointResponse.setFbbActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
        new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE, "Update Failed",
            PICKUP_POINT_CODE)));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(), Mockito.any(ProductVariantUpdateRequest.class),
        eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU_2);
  }

  @Test
  public void processEventSuccessUpdateAllowMultiFbbTest() throws Exception {
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "mppForWhEnabled", true);
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(ITEM_SKU);
    itemBasicDetailV2Response.setMerchantSku("merchantSku");
    when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU_2))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
    Map<String, String> row = setRowValues();
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    pickupPointResponse.setFbbActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
        new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE,
            "Update Failed", PICKUP_POINT_CODE)));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);

    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU_2);
  }

  @Test
  public void processEventSuccessUpdateAllowMultiFbbTest_cncForWarehouseSwitchTrue()
      throws Exception {
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean,
        "cncForWarehouseFeatureSwitch", true);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "mppForWhEnabled",
        true);
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(
        bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(),
        BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(ITEM_SKU);
    itemBasicDetailV2Response.setMerchantSku("merchantSku");
    when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU_2)).thenReturn(
        Collections.singletonList(itemBasicDetailV2Response));
    Map<String, String> row = setRowValues();
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(
        row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    pickupPointResponse.setFbbActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),
            Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
        new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE,
            "Update Failed", PICKUP_POINT_CODE)));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page =
        new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);

    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(
        itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(),
        BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU_2);
  }

  @Test
  public void processEventSuccessUpdateAllowMultiFbbBulkProcessDataTest() throws Exception {
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "mppForWhEnabled", true);
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(ITEM_SKU);
    itemBasicDetailV2Response.setMerchantSku("merchantSku");
    when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU_2))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    pickupPointResponse.setFbbActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
        new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE, "Update Failed",
            PICKUP_POINT_CODE),
        new VariantsErrorListResponse(ITEM_SKU, PICKUP_POINT_CODE, PICKUP_POINT_CODE, "Update Failed",
            PICKUP_POINT_CODE)));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);

    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU_2);
  }

  @Test
  public void processEventSuccessUpdateNoPPCodeUpdateTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(ITEM_SKU);
    itemBasicDetailV2Response.setMerchantSku("merchantSku");
    when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU_2))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCncActivated(true);
    pickupPointResponse.setFbbActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
        new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE,
            "Update Failed", PICKUP_POINT_CODE)));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);

    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU_2);
  }

  @Test
  public void processEventSuccessNonCncTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
      any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(false);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(false);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
      new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE,
        "Update Failed", PICKUP_POINT_CODE)));
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void processEventInvalidStockTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
      any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    row.put(ExcelHeaderNames.STOCK_EN, "ABC");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(false);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
      new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE,
        "Update Failed", PICKUP_POINT_CODE)));
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
  }

  @Test
  public void processEventNegativeStockTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
      any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    row.put(ExcelHeaderNames.STOCK_EN, "-1");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(false);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
      new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE,
        "Update Failed", PICKUP_POINT_CODE)));
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
  }

  @Test
  public void processEventInvalidItemSkuTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, PRODUCT_SKU);
    row.put(ExcelHeaderNames.STOCK_EN, "-1");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(false);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
        new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE,
            "Update Failed", PICKUP_POINT_CODE)));
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
  }

  @Test
  public void processEventEmptyPPListTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
      any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(false);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(new ArrayList<>());
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
      new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE,
        "Update Failed", PICKUP_POINT_CODE)));
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void processEventPpNotEligibleForCncTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
      any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    row.put(ExcelHeaderNames.CNC_STATUS_IN, "1");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(false);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
      new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE,
        "Update Failed", PICKUP_POINT_CODE)));
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void processEventSuccessFreeSampleErrorTest() throws Exception {
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFreeSample(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class)))
      .thenReturn(Collections.singletonList(pickupPointResponse));
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean,
      "multiPickupPointEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(
      bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(),
      BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(
      itemsPriceStockImagesUpdateResponse);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    List<UpsertOfflineItemFailedResponse> responses = new ArrayList<>();
    upsertOfflineItemFailedResponse.setPickupPointCode(PICKUP_POINT_CODE);
    responses.add(upsertOfflineItemFailedResponse);
    when(
      productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(responses);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(
      row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
      new AuditTrailInfo());
    when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(null);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(),
      BulkProcessData.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(),
      Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void processEventMinPriceGreaterTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
      any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "-10.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(false);

    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
      new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE,
        "Update Failed", PICKUP_POINT_CODE)));
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
  }

  @Test
  public void processEventSuccessFreeSampleCncErrorTest() throws Exception {
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFreeSample(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class)))
      .thenReturn(Collections.singletonList(pickupPointResponse));
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(
      bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(),
      BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(),
      BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(
      itemsPriceStockImagesUpdateResponse);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    List<UpsertOfflineItemFailedResponse> responses = new ArrayList<>();
    upsertOfflineItemFailedResponse.setPickupPointCode(PICKUP_POINT_CODE);
    responses.add(upsertOfflineItemFailedResponse);
    when(
      productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(responses);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.ONLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(
      row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
      new AuditTrailInfo());
    when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(null);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(),
      BulkProcessData.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(),
      Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void setModifiedItemInEditRequestTest() {
    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    Map<String, ProductVariantPriceStockAndImagesRequest> l4ProductItemMap = new HashMap<>();
    ProductVariantPriceStockAndImagesRequest request =
      new ProductVariantPriceStockAndImagesRequest();
    request.setModifiedItemPickupPoints(Collections.singletonList(pickupPointRequest));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    l4ProductItemMap.put(ITEM_SKU_2,request);
    instantPickupProductBulkUpsertServiceBean.setModifiedItemInEditRequest(l4ProductItemMap,
      upsertOfflineItemRequest, itemPickupPointListingL3Response, profileResponse, null);
  }

  @Test
  public void setModifiedItemInEditRequestTest_cncForWarehouseFeatureSwitch() {
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean,
        "cncForWarehouseFeatureSwitch", true);
    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    Map<String, ProductVariantPriceStockAndImagesRequest> l4ProductItemMap = new HashMap<>();
    ProductVariantPriceStockAndImagesRequest request =
        new ProductVariantPriceStockAndImagesRequest();
    request.setModifiedItemPickupPoints(Collections.singletonList(pickupPointRequest));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    l4ProductItemMap.put(ITEM_SKU_2, request);
    instantPickupProductBulkUpsertServiceBean.setModifiedItemInEditRequest(l4ProductItemMap,
        upsertOfflineItemRequest, itemPickupPointListingL3Response, profileResponse, null);
  }

  @Test
  public void setModifiedItemAlreadyExistingInEditRequestTest() throws JsonProcessingException {
    upsertOfflineItemRequest.setItemSku(ITEM_SKU_2);
    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    Map<String, ProductVariantPriceStockAndImagesRequest> l4ProductItemMap = new HashMap<>();
    ProductVariantPriceStockAndImagesRequest request =
      new ProductVariantPriceStockAndImagesRequest();
    request.setModifiedItemPickupPoints(Collections.singletonList(pickupPointRequest));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    l4ProductItemMap.put(ITEM_SKU_2, request);
    instantPickupProductBulkUpsertServiceBean.setModifiedItemInEditRequest(l4ProductItemMap,
      upsertOfflineItemRequest, itemPickupPointListingL3Response, profileResponse, null);
    verify(objectMapper, times(0)).writeValueAsString(Mockito.any());
  }


  @Test
  public void processEventSuccessUpdateFreeSampleFailedTest() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Map<String, String> row = setRowValues();
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    pickupPointResponse.setFbbActivated(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setVariantsErrorList(List.of(
      new VariantsErrorListResponse(ITEM_SKU_2, PICKUP_POINT_CODE, PICKUP_POINT_CODE,
        "Update Failed", PICKUP_POINT_CODE)));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    itemPickupPointListingL3Response.setFreeSample(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);

    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
      Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
      Constant.MINIMUM_PRICE);
    verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void setProductItemInRequestTest() throws JsonProcessingException {
    Map<String, ProductVariantPriceStockAndImagesRequest> l4ProductItemMap = new HashMap<>();
    ItemPickupPointListingL3Response itemLevelResponse = new ItemPickupPointListingL3Response();
    l4ProductItemMap.put(ITEM_SKU_2, new ProductVariantPriceStockAndImagesRequest());
    instantPickupProductBulkUpsertServiceBean.setProductItemInRequest(ITEM_SKU_2, l4ProductItemMap,
      itemLevelResponse);
    verify(objectMapper, Mockito.times(0)).writeValueAsString(Mockito.any());
  }

  private static Map<String, String> setRowValues(){
    Map<String, String> row = new LinkedHashMap<> ();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.ONLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    return row;
  }

  @Test
  public void processEventSuccessOfferPriceGreaterThanListPriceTest() throws Exception {
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setWebSyncStock(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "multiPickupPointEnabled", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    List<UpsertOfflineItemFailedResponse> responses = new ArrayList<>();
    upsertOfflineItemFailedResponse.setPickupPointCode(PICKUP_POINT_CODE);
    responses.add(upsertOfflineItemFailedResponse);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(responses);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "10000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "5000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    row.put(ExcelHeaderNames.CNC_STATUS_EN, BulkParameters.OFFLINE_VALUE);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        new AuditTrailInfo());
    when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(null);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(this.bulkProcessDataService, times(1)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    Mockito.verify(systemParameterConfigService, times(2))
        .findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void processEventSuccess_omgNonPreOrder() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "preOrderQuotaFeatureSwitch", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    profileResponse.setFlags(Collections.singletonMap("blibliOMG", true));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU_1))
        .thenReturn(new BasicProductResponse());
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, PRODUCT_SKU_1);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
        itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
        profileResponse.getCompany().isInternationalFlag(),
        PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccess_omgPreOrder() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderDate(Date.from(LocalDateTime.now().plusDays(5)
        .atZone(ZoneId.systemDefault()).toInstant()));
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "preOrderQuotaFeatureSwitch", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    profileResponse.setFlags(Collections.singletonMap("blibliOMG", true));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);

    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU_1))
        .thenReturn(basicProductResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, PRODUCT_SKU_1);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
        itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
        profileResponse.getCompany().isInternationalFlag(),
        PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccess_omgPreOrder_feature_switchOff() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderDate(Date.from(LocalDateTime.now().plusDays(5)
        .atZone(ZoneId.systemDefault()).toInstant()));
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "preOrderQuotaFeatureSwitch", false);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    profileResponse.setFlags(Collections.singletonMap("blibliOMG", true));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);

    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU_1))
        .thenReturn(basicProductResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
        itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
        profileResponse.getCompany().isInternationalFlag(),
        PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccess_omgPreOrder_dateInPast() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderDate(Date.from(LocalDateTime.now().minusDays(5)
        .atZone(ZoneId.systemDefault()).toInstant()));
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "preOrderQuotaFeatureSwitch", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    profileResponse.setFlags(Collections.singletonMap("blibliOMG", true));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);

    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU_1))
        .thenReturn(basicProductResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, PRODUCT_SKU_1);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
        itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
        profileResponse.getCompany().isInternationalFlag(),
        PRODUCT_SKU_1, null);
  }

  @Test
  public void processEventSuccess_omgPreOrder_addPickupPoint() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderDate(Date.from(LocalDateTime.now().plusDays(5)
        .atZone(ZoneId.systemDefault()).toInstant()));
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "preOrderQuotaFeatureSwitch", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    profileResponse.setFlags(Collections.singletonMap("blibliOMG", true));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(ITEM_SKU);
    itemBasicDetailV2Response.setMerchantSku("merchantSku");
    when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU_2))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU_1))
        .thenReturn(basicProductResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, PRODUCT_SKU_1);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
        itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
        profileResponse.getCompany().isInternationalFlag(),
        PRODUCT_SKU_1, null);
    verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU_2);
  }


  @Test
  public void processEventSuccess_nonOmgSeller_PreOrder_addPickupPoint() throws Exception {
    bulkProcessDataList.get(0).setBulkRequestData("");
    bulkProcess.setNotes("");
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderDate(Date.from(LocalDateTime.now().plusDays(5)
        .atZone(ZoneId.systemDefault()).toInstant()));
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(instantPickupProductBulkUpsertServiceBean, "preOrderQuotaFeatureSwitch", true);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.upsertOfflineItems(anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(ExcelHeaderNames.BLIBLI_SKU_EN, ITEM_SKU_2);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(new AuditTrailInfo());

    profileResponse.getCompany().setCncActivated(true);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setCncActivated(true);
    profileResponse.setFlags(Collections.singletonMap("blibliOMG", false));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ItemsPriceStockImagesUpdateResponse  itemsPriceStockImagesUpdateResponse =
        new ItemsPriceStockImagesUpdateResponse();
    when(pbpOutboundService.updateSummaryL5(Mockito.anyString(),
        Mockito.any(ProductVariantUpdateRequest.class), eq(Constant.CLIENT_ID))).thenReturn(itemsPriceStockImagesUpdateResponse);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU_2);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setAvailableStockLevel2(0);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Page<ItemPickupPointListingL3Response> page = new PageImpl<>(List.of(itemPickupPointListingL3Response));
    when(pbpOutboundService.getItemPickupPointListingL3Response(eq(0), eq(1),
        Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(page);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(ITEM_SKU);
    itemBasicDetailV2Response.setMerchantSku("merchantSku");
    when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU_2))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU_1))
        .thenReturn(basicProductResponse);
    instantPickupProductBulkUpsertServiceBean.processEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(STORE_ID,
        Constant.MINIMUM_PRICE);
    verify(bulkUpdateServiceUtil).setScheduleRemovalForBulkProcessUpdateAndUpsert(bulkProcessDataList,
        itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
        profileResponse.getCompany().isInternationalFlag(),
        PRODUCT_SKU_1, null);
    verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU_2);
  }
}
