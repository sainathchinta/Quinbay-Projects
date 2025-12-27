package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.util.BulkUpdateServiceUtil.FILE_BLANK_ERROR;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
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
import java.util.UUID;

import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoRequestDTO;
import com.gdn.mta.bulk.dto.inventory.WarehouseInventoryResponseDTO;
import com.gdn.mta.bulk.dto.inventory.WebInventoryResponseDTO;
import com.gdn.partners.pbp.dto.common.ListRequestDTO;
import com.gdn.x.product.enums.Constants;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.poi.util.IOUtils;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkCncUpsertErrorDTO;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.models.AuditTrailInfo;
import com.gdn.mta.bulk.models.BulkUpdateErrorCounter;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoResponseDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;

public class InstantPickupProductBulkDeleteServiceBeanTest {

  private static final String RANDOM_BULK_PROCESS_CODE = UUID.randomUUID().toString();
  private static final String REQUEST_ID = "requestId";
  private static final String BUSINESS_PARTNER_CODE = "TOA-14961";
  private static final String ITEM_SKU = "TOA-14961-00117-00006";
  private static final String ITEM_SKU_1 = "TOA-14961-00118-00002";
  private static final String ITEM_SKU_2 = "TOA-14961-00118-00001";
  private static final String PICKUP_POINT_CODE = "PP-12345";
  private static final String PICKUP_POINT_CODE_1 = "PP-12346";
  private static final String PICKUP_POINT_CODE_2 = "PP-12347";
  private static final String STORE_ID = "10001";
  private static final String USERNAME = "username";
  private static final String CLIENT_HOST = "localhost";
  private static final String ERROR_MESSAGE = "errorMessage";
  private static final String DEFAULT_NOTIFICATION_EXCHANGE = "exchange";
  private static final String DEFAULT_NOTIFICATION_ROUTING_KEY = "routingKey";
  private static final String AUDIT_TRAIL_PICKUP_POINTS = "{\"accessiblePickupPointCodes\":[\"PP-12345\"]}";
  public static final String ROW_NUMBER = "RowNumber";

  @InjectMocks
  private InstantPickupProductBulkDeleteServiceBean instantPickupProductBulkDeleteServiceBean;

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
  private ObjectMapper objectMapper;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private NotificationService notificationService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private InventoryOutboundServiceBean inventoryOutboundServiceBean;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Captor
   ArgumentCaptor<BulkProcess> bulkProcessCaptor;

  private BulkUpdateProcessDTO bulkUpdateProcessDTO;
  private BulkUpdateQueue bulkUpdateQueue;
  private BulkProcess bulkProcess;
  private AuditTrailInfo auditTrailInfo;
  private AuditTrailInfo auditTrailInfo1;

  private DeleteOfflineItemRequest deleteOfflineItemRequest;
  private DeleteOfflineItemRequest deleteOfflineItemRequest1;
  private DeleteOfflineItemRequest deleteOfflineItemRequest2;
  private List<DeleteOfflineItemRequest> deleteOfflineItemRequests;
  private DeleteOfflineItemDetailResponse deleteOfflineItemDetailResponse;
  private List<DeleteOfflineItemDetailResponse> deleteOfflineItemDetailResponses;
  private List<BulkProcessData> bulkProcessDataList;
  private BulkUpdateEventModel bulkUpdateEventModel;

  @Test
  public void testPreProcessInstantPickupProductBulkDelete() throws Exception {
    when(bulkUpdateServiceUtil.getBulkProcess(eq(STORE_ID), eq(REQUEST_ID), anyString(),
        eq(bulkUpdateProcessDTO), eq(0), eq(0), eq(false), eq(false))).thenReturn(bulkProcess);

    instantPickupProductBulkDeleteServiceBean.preProcessInstantPickupProductBulkDelete(STORE_ID,
        REQUEST_ID, bulkUpdateProcessDTO, new HashSet<>());

    verify(bulkUpdateServiceUtil).getBulkProcess(eq(STORE_ID), eq(REQUEST_ID), anyString(),
        eq(bulkUpdateProcessDTO), eq(0), eq(0), eq(false), eq(false));
    verify(bulkUpdateServiceUtil).savePreProcessBulkData(any(BulkProcess.class));
    verify(bulkUpdateServiceUtil).getBulkUpdateQueue(eq(STORE_ID), eq(REQUEST_ID), anyString(),
        eq(bulkUpdateProcessDTO));
    verify(kafkaProducer).send(eq(kafkaTopicProperties.getBulkUploadDeleteInstantPickupProductEvent()), Mockito.isNull());
    verify(fileStorageService).createBulkFile(Mockito.any(),Mockito.anyString(),Mockito.anyString());
    verify(objectMapper).writeValueAsString(any());
    verify(kafkaTopicProperties, times(2)).getBulkUploadDeleteInstantPickupProductEvent();
  }

  @Test
  public void testPreProcessInstantPickupProductBulkDelete_throwsException() throws Exception {
    when(bulkUpdateServiceUtil.getBulkProcess(eq(STORE_ID), eq(REQUEST_ID), anyString(),
        eq(bulkUpdateProcessDTO), eq(0), eq(0), eq(false), eq(false))).thenThrow(ApplicationRuntimeException.class);

    doNothing().when(this.trackerService).sendTracker(any(), any(), any(),
        any(), any());

    try {
      instantPickupProductBulkDeleteServiceBean.preProcessInstantPickupProductBulkDelete(STORE_ID,
          REQUEST_ID, bulkUpdateProcessDTO, new HashSet<>());
    } finally {
      verify(this.bulkUpdateServiceUtil).getBulkProcess(eq(STORE_ID), eq(REQUEST_ID), anyString(),
          eq(bulkUpdateProcessDTO), eq(0), eq(0), eq(false), eq(false));
      verify(fileStorageService).createBulkFile(Mockito.any(),Mockito.anyString(),Mockito.anyString());
    }
  }

  @Test
  public void testProcessInstantPickupProductBulkDelete_nullBulkProcess_throwsException()
      throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));

    ProcessorUtils.createDirectories(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR
        + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR
        + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);

    when(bulkProcessService
        .findByStoreIdAndBulkProcessCodeAndStatus(bulkUpdateQueue.getStoreId(),
            bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(null);

    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> instantPickupProductBulkDeleteServiceBean.processInstantPickupProductBulkDelete(
              bulkUpdateQueue));
    } finally {
      verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID,
          bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    }

    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
  }

  @Test
  public void testProcessInstantPickupProductBulkDelete_headersMismatch_throwsException()
      throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));

    ProcessorUtils.createDirectories(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR
        + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR
        + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    when(bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkDelete(any(), any(), any(),
        any())).thenReturn(false);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        auditTrailInfo1);
    when(objectMapper.writeValueAsString(any())).thenReturn(AUDIT_TRAIL_PICKUP_POINTS);
    instantPickupProductBulkDeleteServiceBean.processInstantPickupProductBulkDelete(bulkUpdateQueue);

    verify(bulkProcessService)
        .findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, bulkUpdateQueue.getBulkProcessCode(),
            BulkProcess.STATUS_PENDING);
    verify(bulkUpdateServiceUtil).authorizeUploadInstantPickupProductBulkDelete(any(), any(), any(),
        any());
    verify(objectMapper).writeValueAsString(Mockito.any(AuditTrailInfo.class));
    verify(fileStorageService).getFileData(Mockito.any(),Mockito.any());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
  }

  private Map<String, String> getFiles() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(
        Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream("BulkDelete" + File.separator + "BulkInstantPickupDelete.xlsx"))),
        StandardCharsets.UTF_8);
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getFiles2() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(
        Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream("BulkDelete" + File.separator + "BulkInstantPickupDelete.xlsx"))),
        StandardCharsets.UTF_8);
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getFiles3() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(
        Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream("BulkDelete" + File.separator + "BulkInstantPickupDelete1.xlsx"))),
        StandardCharsets.UTF_8);
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getFiles4() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(
        Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream("BulkDelete" + File.separator + "BulkInstantPickupDelete2.xlsx"))),
        StandardCharsets.UTF_8);
    files.put("xlsx", excelData);
    return files;
  }



  private Map<String, String> getFiles5() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(
        Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream("BulkDelete" + File.separator + "BulkInstantPickupDelete3.xlsx"))),
        StandardCharsets.UTF_8);
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getFiles6() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(
        Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream("BulkDelete" + File.separator + "BulkInstantPickupDelete5.xlsx"))),
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
    bulkProcess.setBulkProcessCode(RANDOM_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType("InstantPickupProduct");
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedBy("developer");
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setId("12345");
    bulkProcess.setStartDate(new Date());
    bulkProcess.setStoreId("10001");
    bulkProcess.setUpdatedBy("developer");
    bulkProcess.setUpdatedDate(new Date());
    auditTrailInfo1=new AuditTrailInfo();
    auditTrailInfo1.setAccessiblePickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    bulkProcess.setNotes(AUDIT_TRAIL_PICKUP_POINTS);

    auditTrailInfo = new AuditTrailInfo();
    auditTrailInfo.setRequestId(REQUEST_ID);
    auditTrailInfo.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    auditTrailInfo.setRemoteAddress(CLIENT_HOST);
    auditTrailInfo.setUsername(USERNAME);

    deleteOfflineItemRequest = new DeleteOfflineItemRequest();
    deleteOfflineItemRequest.setItemSku(ITEM_SKU);
    deleteOfflineItemRequest.setPickupPointCode(PICKUP_POINT_CODE);

    deleteOfflineItemRequest1 = new DeleteOfflineItemRequest();
    deleteOfflineItemRequest1.setItemSku(ITEM_SKU_1);
    deleteOfflineItemRequest1.setPickupPointCode(PICKUP_POINT_CODE_1);

    deleteOfflineItemRequest2 = new DeleteOfflineItemRequest();
    deleteOfflineItemRequest2.setItemSku(ITEM_SKU_2);
    deleteOfflineItemRequest2.setPickupPointCode(PICKUP_POINT_CODE_2);

    deleteOfflineItemRequests =
        Arrays.asList(deleteOfflineItemRequest, deleteOfflineItemRequest1, deleteOfflineItemRequest2);

    deleteOfflineItemDetailResponse = new DeleteOfflineItemDetailResponse();
    deleteOfflineItemDetailResponse.setItemSku(ITEM_SKU_2);
    deleteOfflineItemDetailResponse.setPickupPointCode(PICKUP_POINT_CODE_2);
    deleteOfflineItemDetailResponse.setErrorMessage(ERROR_MESSAGE);

    deleteOfflineItemDetailResponses = Collections.singletonList(deleteOfflineItemDetailResponse);

    ReflectionTestUtils.setField(instantPickupProductBulkDeleteServiceBean, "batchSize", 100);
    when(bulkProcessService
        .findByStoreIdAndBulkProcessCodeAndStatus(bulkUpdateQueue.getStoreId(),
            bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(this.bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);

    bulkProcessDataList = new ArrayList<>();
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU_2);
    row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    row.put(ROW_NUMBER, "1");
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setParentProduct(ITEM_SKU);
    bulkProcessDataList.add(bulkProcessData);

    bulkUpdateEventModel = new BulkUpdateEventModel();
    bulkUpdateEventModel.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkUpdateEventModel.setStoreId(STORE_ID);
    bulkUpdateEventModel.setRowNumbers(Arrays.asList(1));
  }

  @Test
  public void testProcessInstantPickupProductBulkDelete_SwitchTrueTest() throws Exception {
    Map<String, String> files = this.getFiles();
    Map<String, String> files2 = this.getFiles2();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] excelFile2 = Base64.decodeBase64(files2.get("xlsx"));

    ProcessorUtils.createDirectories(
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);

    File file = new File(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR
      + bulkUpdateQueue.getBulkProcessCode() + File.separator
      + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    XSSFWorkbook workBook;
    InputStream is =  new FileInputStream(file);;
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileData(Mockito.any(),Mockito.any())).thenReturn(sheet);

    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile2);

    when(bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkDelete(any(), any(), any(), any())).thenReturn(
        true);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        auditTrailInfo1);
    when(objectMapper.writeValueAsString(any())).thenReturn(AUDIT_TRAIL_PICKUP_POINTS);
    instantPickupProductBulkDeleteServiceBean.processInstantPickupProductBulkDelete(bulkUpdateQueue);

    verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(bulkUpdateServiceUtil).authorizeUploadInstantPickupProductBulkDelete(any(), any(), any(), any());
    verify(this.bulkProcessService, times(2)).saveOperation(Mockito.any(BulkProcess.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    verify(this.objectMapper, times(6)).writeValueAsString(Mockito.any());
    verify(this.bulkUpdateServiceUtil).getFinalDataList(Mockito.anyMap(), anyList(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    verify(fileStorageService).getFileData(Mockito.any(),Mockito.any());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    verify(objectMapper,times(2)).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
  }

  @Test
  public void testProcessInstantPickupProductBulkDelete_SwitchTrueInvalidItemSkuTest() throws Exception {
    Map<String, String> files = this.getFiles6();
    Map<String, String> files2 = this.getFiles6();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] excelFile2 = Base64.decodeBase64(files2.get("xlsx"));

    ProcessorUtils.createDirectories(
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);

    File file = new File(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR
        + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    XSSFWorkbook workBook;
    InputStream is =  new FileInputStream(file);;
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileData(Mockito.any(),Mockito.any())).thenReturn(sheet);

    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile2);

    when(bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkDelete(any(), any(), any(), any())).thenReturn(
        true);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        auditTrailInfo1);
    when(objectMapper.writeValueAsString(any())).thenReturn(AUDIT_TRAIL_PICKUP_POINTS);
    instantPickupProductBulkDeleteServiceBean.processInstantPickupProductBulkDelete(bulkUpdateQueue);

    verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(bulkUpdateServiceUtil).authorizeUploadInstantPickupProductBulkDelete(any(), any(), any(), any());
    verify(this.bulkProcessService, times(2)).saveOperation(Mockito.any(BulkProcess.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    verify(this.objectMapper, times(9)).writeValueAsString(Mockito.any());
    verify(this.bulkUpdateServiceUtil).getFinalDataList(Mockito.anyMap(), anyList(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    verify(fileStorageService).getFileData(Mockito.any(),Mockito.any());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    verify(objectMapper,times(2)).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
  }

  @Test
  public void testProcessInstantPickupProductBulkDeleteWithFileBlank() throws Exception {
    File file = new File("src/test/resources/BulkDelete/BulkInstantPickupDelete4.xlsx");
    InputStream is = new FileInputStream(file);
    XSSFWorkbook workBook;
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileData(Mockito.any(), Mockito.any())).thenReturn(sheet);
    when(bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkDelete(any(), any(), any(), any()))
        .thenReturn(true);
    when(bulkProcessService
        .findByStoreIdAndBulkProcessCodeAndStatus(bulkUpdateQueue.getStoreId(),
            bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    when(objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class)).thenReturn(auditTrailInfo);
    instantPickupProductBulkDeleteServiceBean.processInstantPickupProductBulkDelete(bulkUpdateQueue);
    verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, bulkUpdateQueue.getBulkProcessCode(),
        BulkProcess.STATUS_PENDING);
    verify(objectMapper).readValue(AUDIT_TRAIL_PICKUP_POINTS, AuditTrailInfo.class);
    verify(bulkUpdateServiceUtil).authorizeUploadInstantPickupProductBulkDelete(any(), any(), any(), any());
    verify(this.bulkProcessService, times(2)).saveOperation(bulkProcessCaptor.capture());
    verify(this.objectMapper).writeValueAsString(Mockito.any());
    verify(fileStorageService).getFileData(bulkUpdateQueue, bulkProcess);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessCaptor.getValue().getStatus());
    Assertions.assertEquals(FILE_BLANK_ERROR, bulkProcessCaptor.getValue().getDescription());
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(productBusinessPartnerRepository, kafkaProducer,
        bulkUpdateServiceUtil, systemParameterConfigService, objectMapper, bulkProcessDataService
      , fileStorageService, kafkaTopicProperties);
    verifyNoMoreInteractions(notificationService);
  }

  @Test
  public void processEventTest() throws Exception {
    bulkProcess.setNotes("");
    bulkProcessDataList.get(0).setBulkRequestData(new ObjectMapper().writeValueAsString(new LinkedHashMap<>()));
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.deleteOfflineItems(eq(STORE_ID), anyList(),
        any(AuditTrailInfo.class))).thenReturn(deleteOfflineItemDetailResponses);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU_2);
    row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE_2);
    row.put(ROW_NUMBER, "1");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);

    instantPickupProductBulkDeleteServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(productBusinessPartnerRepository).deleteOfflineItems(eq(STORE_ID), anyList(),
        Mockito.isNull());
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
  }

  @Test
  public void processEventSuccessTest() throws Exception {
    bulkProcess.setNotes("");
    bulkProcessDataList.get(0).setBulkRequestData(new ObjectMapper().writeValueAsString(new LinkedHashMap<>()));

    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.deleteOfflineItems(eq(STORE_ID), anyList(),
        any(AuditTrailInfo.class))).thenReturn(null);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU_2);
    row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    row.put(ROW_NUMBER, "1");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);

    instantPickupProductBulkDeleteServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(productBusinessPartnerRepository).deleteOfflineItems(eq(STORE_ID), anyList(),
        Mockito.isNull());
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
  }

  @Test
  public void processEventExceptionTest() throws Exception {
    bulkProcess.setNotes("");
    bulkProcessDataList.get(0).setBulkRequestData(new ObjectMapper().writeValueAsString(new LinkedHashMap<>()));
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        bulkProcessDataList);
    when(productBusinessPartnerRepository.deleteOfflineItems(eq(STORE_ID), anyList(),
        any(AuditTrailInfo.class))).thenReturn(deleteOfflineItemDetailResponses);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
        bulkProcessDataList.get(0));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU);
    row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    row.put(ROW_NUMBER, "1");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);

    instantPickupProductBulkDeleteServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(productBusinessPartnerRepository).deleteOfflineItems(eq(STORE_ID), anyList(),
        Mockito.isNull());
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(bulkProcessDataService).saveBulkProcessData(Mockito.any());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
  }

  @Test
  public void processEventEmptyDataListTest() throws Exception {
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        null);

    instantPickupProductBulkDeleteServiceBean.processEvent(bulkUpdateEventModel);

    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
        RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
  }

  @Test
  public void testProcessInstantPickupProductBulkDelete_SwitchTrue_EmptyParentTest() throws Exception {
    Map<String, String> files = this.getFiles3();
    Map<String, String> files2 = this.getFiles3();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] excelFile2 = Base64.decodeBase64(files2.get("xlsx"));

    ProcessorUtils.createDirectories(
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);

    File file = new File(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR
      + bulkUpdateQueue.getBulkProcessCode() + File.separator
      + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    XSSFWorkbook workBook;
    InputStream is =  new FileInputStream(file);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileData(Mockito.any(),Mockito.any())).thenReturn(sheet);

    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile2);

    when(bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkDelete(any(), any(), any(), any())).thenReturn(
        true);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        auditTrailInfo1);
    when(objectMapper.writeValueAsString(any())).thenReturn(AUDIT_TRAIL_PICKUP_POINTS);
    instantPickupProductBulkDeleteServiceBean.processInstantPickupProductBulkDelete(bulkUpdateQueue);

    verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID,
        bulkUpdateQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    verify(bulkUpdateServiceUtil).authorizeUploadInstantPickupProductBulkDelete(any(), any(), any(), any());
    verify(this.bulkProcessService, times(2)).saveOperation(Mockito.any(BulkProcess.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    verify(this.objectMapper, times(7)).writeValueAsString(Mockito.any());
    verify(this.bulkUpdateServiceUtil).getFinalDataList(Mockito.anyMap(), anyList(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    verify(fileStorageService).getFileData(Mockito.any(),Mockito.any());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    verify(objectMapper,times(2)).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
  }

  @Test
  public void testProcessInstantPickupProductBulkDelete_SwitchTrue_AllFailedTest() throws Exception {
    bulkProcess.setNotes("");
    auditTrailInfo1.setAccessiblePickupPointCodes(new HashSet<>());
    Map<String, String> files = this.getFiles4();
    Map<String, String> files2 = this.getFiles4();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] excelFile2 = Base64.decodeBase64(files2.get("xlsx"));

    ProcessorUtils.createDirectories(
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);

    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile2);

    File file = new File(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR
      + bulkUpdateQueue.getBulkProcessCode() + File.separator
      + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    XSSFWorkbook workBook;
    InputStream is =  new FileInputStream(file);;
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileData(Mockito.any(),Mockito.any())).thenReturn(sheet);

    when(bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkDelete(any(), any(), any(), any())).thenReturn(
        true);
    when(bulkUpdateServiceUtil.getFinalDataList(Mockito.anyMap(), anyList(), Mockito.anyBoolean(),
        Mockito.anyBoolean())).thenReturn(3);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        auditTrailInfo1);
    when(objectMapper.writeValueAsString(any())).thenReturn(
        "{\"accessiblePickupPointCodes\":[]}");

    instantPickupProductBulkDeleteServiceBean.processInstantPickupProductBulkDelete(bulkUpdateQueue);

    verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, bulkUpdateQueue.getBulkProcessCode(),
        BulkProcess.STATUS_PENDING);
    verify(bulkUpdateServiceUtil).authorizeUploadInstantPickupProductBulkDelete(any(), any(), any(), any());
    verify(this.bulkProcessService, times(2)).saveOperation(Mockito.any(BulkProcess.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    verify(this.objectMapper).writeValueAsString(Mockito.any(AuditTrailInfo.class));
    verify(this.objectMapper, times(2)).writeValueAsString(Mockito.any(BulkCncUpsertErrorDTO.class));
    verify(this.objectMapper, times(2)).writeValueAsString(Mockito.any(Map.class));
    verify(this.bulkUpdateServiceUtil).getFinalDataList(Mockito.anyMap(), anyList(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    verify(fileStorageService).getFileData(Mockito.any(),Mockito.any());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
  }

  @Test
  public void testProcessInstantPickupProductBulkDelete_SwitchTrue_DuplicateSkus_AllFailedTest() throws Exception {
    Map<String, String> files = this.getFiles5();
    Map<String, String> files2 = this.getFiles5();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] excelFile2 = Base64.decodeBase64(files2.get("xlsx"));

    ProcessorUtils.createDirectories(
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
            + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);

    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile2);

    File file = new File(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR
      + bulkUpdateQueue.getBulkProcessCode() + File.separator
      + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);

    XSSFWorkbook workBook;
    InputStream is =  new FileInputStream(file);;
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileData(Mockito.any(),Mockito.any())).thenReturn(sheet);

    when(bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkDelete(any(), any(), any(), any())).thenReturn(
        true);
    when(bulkUpdateServiceUtil.getFinalDataList(Mockito.anyMap(), anyList(), Mockito.anyBoolean(),
        Mockito.anyBoolean())).thenReturn(3);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        auditTrailInfo1);
    when(objectMapper.writeValueAsString(any())).thenReturn(AUDIT_TRAIL_PICKUP_POINTS);

    instantPickupProductBulkDeleteServiceBean.processInstantPickupProductBulkDelete(bulkUpdateQueue);

    verify(bulkProcessService).findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, bulkUpdateQueue.getBulkProcessCode(),
        BulkProcess.STATUS_PENDING);
    verify(bulkUpdateServiceUtil).authorizeUploadInstantPickupProductBulkDelete(any(), any(), any(), any());
    verify(this.bulkProcessService, times(2)).saveOperation(Mockito.any(BulkProcess.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    verify(this.objectMapper, times(4)).writeValueAsString(Mockito.any());
    verify(this.bulkUpdateServiceUtil).getFinalDataList(Mockito.anyMap(), anyList(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    verify(fileStorageService).getFileData(Mockito.any(),Mockito.any());
    BulkUpdateServiceUtil.removeDirectory(bulkUpdateQueue.getBulkProcessCode());
    verify(objectMapper,times(2)).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
  }

  @Test
  public void setFinalStatusAndNotificationOnInstantPickupDeleteTest() throws Exception {
    bulkProcess.setNotes("");
    List<BulkProcessData> bulkProcessData = getBulkProcessData();
    bulkProcessData.forEach(i -> i.setErrorMessage("System error"));
    bulkProcessData.forEach(i -> i.setStatus(BulkProcessData.STATUS_FAIL));
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Mockito.anyString(),
        Mockito.any(BulkProcess.class))).thenReturn(bulkProcessData);
    AuditTrailInfo auditTrailInfo = new AuditTrailInfo();
    auditTrailInfo.setFileName("test.xlsx");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        auditTrailInfo);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(BulkCncUpsertErrorDTO.class))).thenReturn(
        new BulkCncUpsertErrorDTO(ITEM_SKU, PICKUP_POINT_CODE, "Error"));
    bulkProcess.setTotalCount(4);
    bulkProcess.setDescription("1 row updated");
    when(fileStorageService.getDownloadLink(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn("file");
    instantPickupProductBulkDeleteServiceBean.setFinalStatusAndNotificationOnInstantPickupDelete(bulkProcess,
        Constant.STORE_ID);

    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(bulkUpdateServiceUtil).updateBulkDeleteCncProductFinalStatus(eq(bulkProcess), any(BulkUpdateQueue.class),
        eq(0), eq(4), anyList(), any(BulkUpdateErrorCounter.class),
        anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
    verify(notificationService).sendNotificationWithErrorFileGenerated(Mockito.any(),
      Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(fileStorageService).getDownloadLink(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString());
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
  }

  private List<BulkProcessData> getBulkProcessData() throws Exception {
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
    bulkProcessData4.setInputErrorCount(1);
    bulkProcessData4.setRowNumber(1);
    return Arrays.asList(bulkProcessData1, bulkProcessData2, bulkProcessData3, bulkProcessData4);
  }

  @Test
  public void setFinalStatusAndNotificationOnInstantPickupDeleteAllSuccessTest() throws Exception {
    bulkProcess.setNotes("");
    List<BulkProcessData> dataList = getBulkProcessData();
    for (BulkProcessData bulkProcessData : dataList) {
      bulkProcessData.setStatus(BulkProcessData.STATUS_SUCCESS);
      bulkProcessData.setBulkRequestData(new ObjectMapper().writeValueAsString(new LinkedHashMap<>()));
    }
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Mockito.anyString(),
        Mockito.any(BulkProcess.class))).thenReturn(dataList);
    AuditTrailInfo auditTrailInfo = new AuditTrailInfo();
    auditTrailInfo.setFileName("test.xlsx");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        auditTrailInfo);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(BulkCncUpsertErrorDTO.class))).thenReturn(
        new BulkCncUpsertErrorDTO(ITEM_SKU, PICKUP_POINT_CODE, "Error"));
    bulkProcess.setTotalCount(4);
    bulkProcess.setDescription("All updated");
    instantPickupProductBulkDeleteServiceBean.setFinalStatusAndNotificationOnInstantPickupDelete(bulkProcess,
        Constant.STORE_ID);

    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(bulkUpdateServiceUtil).updateBulkDeleteCncProductFinalStatus(eq(bulkProcess), any(BulkUpdateQueue.class),
        eq(4), eq(4), anyList(), any(BulkUpdateErrorCounter.class),
        anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
    BulkUpdateServiceUtil.removeDirectory(bulkProcess.getBulkProcessCode());
    verify(notificationService).sendNotificationWithErrorFileGenerated(Mockito.any(),
      Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyBoolean());
  }

  @Test
  public void setFinalStatusAndNotificationOnInstantPickupDeleteAllFailureTest() throws Exception {
    bulkProcess.setNotes("");
    List<BulkProcessData> dataList = getBulkProcessData();
    for (BulkProcessData bulkProcessData : dataList) {
      bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
      bulkProcessData.setErrorMessage(Constant.SYSTEM_ERROR);
      bulkProcessData.setBulkRequestData(new ObjectMapper().writeValueAsString(new LinkedHashMap<>()));
    }
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(Mockito.anyString(),
        Mockito.any(BulkProcess.class))).thenReturn(dataList);
    AuditTrailInfo auditTrailInfo = new AuditTrailInfo();
    auditTrailInfo.setFileName("test.xlsx");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class))).thenReturn(
        auditTrailInfo);
    when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(BulkCncUpsertErrorDTO.class))).thenReturn(
        new BulkCncUpsertErrorDTO(ITEM_SKU, PICKUP_POINT_CODE, "Error"));
    when(fileStorageService.getDownloadLink(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn("file");
    bulkProcess.setTotalCount(4);
    bulkProcess.setDescription("1 row updated");
    instantPickupProductBulkDeleteServiceBean.setFinalStatusAndNotificationOnInstantPickupDelete(bulkProcess,
        Constant.STORE_ID);

    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    verify(bulkUpdateServiceUtil).updateBulkDeleteCncProductFinalStatus(eq(bulkProcess), any(BulkUpdateQueue.class),
        eq(0), eq(4), anyList(), any(BulkUpdateErrorCounter.class),
        anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
    BulkUpdateServiceUtil.removeDirectory(bulkProcess.getBulkProcessCode());
    verify(notificationService).sendNotificationWithErrorFileGenerated(Mockito.any(),
      Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(fileStorageService).getDownloadLink(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString());
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
  }


@Test
public void testProcessEvent_WithWarehouseValidation_EligibleMerchant() throws Exception {
    ReflectionTestUtils.setField(instantPickupProductBulkDeleteServiceBean, "validateWarehouseVariantDeletionEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkDeleteServiceBean,
      "supportedMerchantsForWarehouseStockValidation",
      Constant.MERCHANT_TYPE_CM.concat(Constants.COMMA).concat(Constant.CC_MERCHANT));
  bulkProcess.setNotes("");
  bulkProcess.setStoreId(STORE_ID);
  ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO =
    new ListRequestDTO<>();
  InventoryDetailInfoRequestDTO infoRequestDTO = new InventoryDetailInfoRequestDTO();
  infoRequestDTO.setWebMerchantCode(BUSINESS_PARTNER_CODE);
  infoRequestDTO.setPickupPointCode(PICKUP_POINT_CODE_2);
  infoRequestDTO.setWebItemSku(ITEM_SKU_2);
  inventoryDetailInfoRequestDTO.setList(Collections.singletonList(infoRequestDTO));
  InventoryDetailInfoResponseDTO inventoryDetailInfoResponseDTO =
    new InventoryDetailInfoResponseDTO();
  inventoryDetailInfoResponseDTO.setWarehouseInventoryResponseList(Collections.singletonList(
    WarehouseInventoryResponseDTO.builder().availableStock(9).build()));
  WebInventoryResponseDTO webInventoryResponse = new WebInventoryResponseDTO();
  webInventoryResponse.setWebItemSku(ITEM_SKU_2);
  webInventoryResponse.setPickupPointCode(PICKUP_POINT_CODE_2);
  inventoryDetailInfoResponseDTO.setWebInventoryResponse(webInventoryResponse);
  bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  bulkProcessDataList.get(0).setBulkRequestData(new ObjectMapper().writeValueAsString(new LinkedHashMap<>()));
  when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
  when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
    RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
    bulkProcessDataList);
  when(productBusinessPartnerRepository.deleteOfflineItems(eq(STORE_ID), anyList(),
    any(AuditTrailInfo.class))).thenReturn(deleteOfflineItemDetailResponses);
  when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
    bulkProcessDataList.get(0));
  Mockito.when(inventoryOutboundServiceBean.findDetailByWebMerchantCodeAndWebItemSku(
      any()))
    .thenReturn(Collections.singletonList(inventoryDetailInfoResponseDTO));
  Map<String, String> row = new LinkedHashMap<>();
  row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU_2);
  row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE_2);
  row.put(ROW_NUMBER, "1");
  when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    ProfileResponse profile = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType(Constant.MERCHANT_TYPE_CM);
    profile.setCompany(company);
    profile.setFbbActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenReturn(profile);
    instantPickupProductBulkDeleteServiceBean.processEvent(bulkUpdateEventModel);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE);
    verify(inventoryOutboundServiceBean).findDetailByWebMerchantCodeAndWebItemSku(any());
  verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
  verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
    RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
  verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
  verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
  verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
  verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
  verify(objectMapper).writeValueAsString(any());
}

  @Test
  public void testProcessEvent_WithWarehouseValidation_EligibleMerchant_wrongL5() throws Exception {
    ReflectionTestUtils.setField(instantPickupProductBulkDeleteServiceBean, "validateWarehouseVariantDeletionEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkDeleteServiceBean,
      "supportedMerchantsForWarehouseStockValidation",
      Constant.MERCHANT_TYPE_CM.concat(Constants.COMMA).concat(Constant.CC_MERCHANT));
    bulkProcess.setNotes("");
    bulkProcess.setStoreId(STORE_ID);
    ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO =
      new ListRequestDTO<>();
    InventoryDetailInfoRequestDTO infoRequestDTO = new InventoryDetailInfoRequestDTO();
    infoRequestDTO.setWebMerchantCode(BUSINESS_PARTNER_CODE);
    infoRequestDTO.setPickupPointCode(PICKUP_POINT_CODE_2);
    infoRequestDTO.setWebItemSku(ITEM_SKU_2);
    inventoryDetailInfoRequestDTO.setList(Collections.singletonList(infoRequestDTO));
    InventoryDetailInfoResponseDTO inventoryDetailInfoResponseDTO =
      new InventoryDetailInfoResponseDTO();
    inventoryDetailInfoResponseDTO.setWarehouseInventoryResponseList(Collections.singletonList(
      WarehouseInventoryResponseDTO.builder().availableStock(9).build()));
    WebInventoryResponseDTO webInventoryResponse = new WebInventoryResponseDTO();
    webInventoryResponse.setWebItemSku(ITEM_SKU);
    webInventoryResponse.setPickupPointCode(PICKUP_POINT_CODE_2);
    inventoryDetailInfoResponseDTO.setWebInventoryResponse(webInventoryResponse);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcessDataList.get(0).setBulkRequestData(new ObjectMapper().writeValueAsString(new LinkedHashMap<>()));
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(productBusinessPartnerRepository.deleteOfflineItems(eq(STORE_ID), anyList(),
      any(AuditTrailInfo.class))).thenReturn(deleteOfflineItemDetailResponses);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Mockito.when(inventoryOutboundServiceBean.findDetailByWebMerchantCodeAndWebItemSku(
        any()))
      .thenReturn(Collections.singletonList(inventoryDetailInfoResponseDTO));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU_2);
    row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE_2);
    row.put(ROW_NUMBER, "1");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    ProfileResponse profile = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType(Constant.MERCHANT_TYPE_CM);
    profile.setCompany(company);
    profile.setFbbActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString()))
      .thenReturn(profile);
    instantPickupProductBulkDeleteServiceBean.processEvent(bulkUpdateEventModel);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE);
    verify(inventoryOutboundServiceBean).findDetailByWebMerchantCodeAndWebItemSku(any());
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(productBusinessPartnerRepository).deleteOfflineItems(eq(STORE_ID), anyList(),
      Mockito.isNull());
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
  }

  @Test
  public void testProcessEvent_WithWarehouseValidation_EligibleMerchantSwitchOff() throws Exception {
    ReflectionTestUtils.setField(instantPickupProductBulkDeleteServiceBean,
      "validateWarehouseVariantDeletionEnabled", false);
    ReflectionTestUtils.setField(instantPickupProductBulkDeleteServiceBean,
      "supportedMerchantsForWarehouseStockValidation",
      Constant.MERCHANT_TYPE_CM.concat(Constants.COMMA).concat(Constant.CC_MERCHANT));
    bulkProcess.setNotes("");
    bulkProcess.setStoreId(STORE_ID);
    ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO =
      new ListRequestDTO<>();
    InventoryDetailInfoRequestDTO infoRequestDTO = new InventoryDetailInfoRequestDTO();
    infoRequestDTO.setWebMerchantCode(BUSINESS_PARTNER_CODE);
    infoRequestDTO.setPickupPointCode(PICKUP_POINT_CODE_2);
    infoRequestDTO.setWebItemSku(ITEM_SKU_2);
    inventoryDetailInfoRequestDTO.setList(Collections.singletonList(infoRequestDTO));
    InventoryDetailInfoResponseDTO inventoryDetailInfoResponseDTO =
      new InventoryDetailInfoResponseDTO();
    inventoryDetailInfoResponseDTO.setWarehouseInventoryResponseList(Collections.singletonList(
      WarehouseInventoryResponseDTO.builder().availableStock(9).build()));
    WebInventoryResponseDTO webInventoryResponse = new WebInventoryResponseDTO();
    webInventoryResponse.setWebItemSku(ITEM_SKU_2);
    webInventoryResponse.setPickupPointCode(PICKUP_POINT_CODE_2);
    inventoryDetailInfoResponseDTO.setWebInventoryResponse(webInventoryResponse);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcessDataList.get(0).setBulkRequestData(new ObjectMapper().writeValueAsString(new LinkedHashMap<>()));
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(productBusinessPartnerRepository.deleteOfflineItems(eq(STORE_ID), anyList(),
      any(AuditTrailInfo.class))).thenReturn(deleteOfflineItemDetailResponses);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Mockito.when(inventoryOutboundServiceBean.findDetailByWebMerchantCodeAndWebItemSku(
        inventoryDetailInfoRequestDTO))
      .thenReturn(Collections.singletonList(inventoryDetailInfoResponseDTO));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU_2);
    row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE_2);
    row.put(ROW_NUMBER, "1");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    ProfileResponse profile = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType(Constant.MERCHANT_TYPE_CM);
    profile.setCompany(company);
    profile.setFbbActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString()))
      .thenReturn(profile);
    instantPickupProductBulkDeleteServiceBean.processEvent(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(productBusinessPartnerRepository).deleteOfflineItems(eq(STORE_ID), anyList(),
      Mockito.isNull());
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
  }


  @Test
  public void testProcessEvent_WithWarehouseValidation_NotEligibleMerchant() throws Exception {
    ReflectionTestUtils.setField(instantPickupProductBulkDeleteServiceBean, "validateWarehouseVariantDeletionEnabled", true);
    ReflectionTestUtils.setField(instantPickupProductBulkDeleteServiceBean,
      "supportedMerchantsForWarehouseStockValidation",
      Constant.MERCHANT_TYPE_CM.concat(Constants.COMMA).concat(Constant.CC_MERCHANT));
    bulkProcess.setNotes("");
    bulkProcess.setStoreId(STORE_ID);
    ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO =
      new ListRequestDTO<>();
    InventoryDetailInfoRequestDTO infoRequestDTO = new InventoryDetailInfoRequestDTO();
    infoRequestDTO.setWebMerchantCode(BUSINESS_PARTNER_CODE);
    infoRequestDTO.setPickupPointCode(PICKUP_POINT_CODE_2);
    infoRequestDTO.setWebItemSku(ITEM_SKU_2);
    inventoryDetailInfoRequestDTO.setList(Collections.singletonList(infoRequestDTO));
    InventoryDetailInfoResponseDTO inventoryDetailInfoResponseDTO =
      new InventoryDetailInfoResponseDTO();
    inventoryDetailInfoResponseDTO.setWarehouseInventoryResponseList(Collections.singletonList(
      WarehouseInventoryResponseDTO.builder().availableStock(9).build()));
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcessDataList.get(0).setBulkRequestData(new ObjectMapper().writeValueAsString(new LinkedHashMap<>()));
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
      bulkProcessDataList);
    when(productBusinessPartnerRepository.deleteOfflineItems(eq(STORE_ID), anyList(),
      any(AuditTrailInfo.class))).thenReturn(deleteOfflineItemDetailResponses);
    when(bulkProcessDataService.saveOperation(Mockito.any(BulkProcessData.class))).thenReturn(
      bulkProcessDataList.get(0));
    Mockito.when(inventoryOutboundServiceBean.findDetailByWebMerchantCodeAndWebItemSku(
        inventoryDetailInfoRequestDTO))
      .thenReturn(Collections.singletonList(inventoryDetailInfoResponseDTO));
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU_2);
    row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE_2);
    row.put(ROW_NUMBER, "1");
    when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    ProfileResponse profile = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType(Constant.TD_MERCHANT);
    profile.setCompany(company);
    profile.setFbbActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString()))
      .thenReturn(profile);
    instantPickupProductBulkDeleteServiceBean.processEvent(bulkUpdateEventModel);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, RANDOM_BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID,
      RANDOM_BULK_PROCESS_CODE, bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(productBusinessPartnerRepository).deleteOfflineItems(eq(STORE_ID), anyList(),
      Mockito.isNull());
    verify(this.bulkProcessDataService).saveOperation(Mockito.any(BulkProcessData.class));
    verify(this.bulkProcessDataService).saveBulkProcessData(anyList());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(AuditTrailInfo.class));
  }

}
