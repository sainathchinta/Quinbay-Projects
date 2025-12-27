package com.gdn.mta.bulk.service;


import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.io.FileUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
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
import org.slf4j.MDC;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.dto.BulkUpdateErrorDTO;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.models.BulkArchiveRequest;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;

public class BulkArchiveServiceImplTest {

  private static final String BUSINESS_PARTNER_CODE = "TOQ-16110";
  private static final String DEFAULT_BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String DEFAULT_STORE_ID = "storeId";
  private static final String DEFAULT_USERNAME = "username";
  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String PRODUCT_SKU = "TOQ-16110-00001";
  private static final String PRODUCT_SKU_2 = "TOQ-16110-00002";
  private static final String PRODUCT_SKU_3 = "TOA-16000-00002";
  private static final String PRODUCT_NAME = "productName";
  private static final String PRODUCT_NAME_2 = "productName2";
  private static final String PRODUCT_NAME_3 = "productName3";
  private static final String REQUEST_ID = "requestId";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String BULK_PROCESS_CODE_1 = "bulkProcessCode1";
  private static final String BULK_PROCESS_CODE_2 = "bulkProcessCode2";
  private static final String FILE_NAME = "test.xlsx";
  private static final String DEFAULT_NOTIFICATION_EXCHANGE = "notificationExchange";
  private static final String DEFAULT_NOTIFICATION_ROUTING_KEY = "notificationRoutingKey";

  private BulkProcess bulkProcess = new BulkProcess();
  private SXSSFWorkbook hssfWorkbook = new SXSSFWorkbook();
  private Sheet testSheet = hssfWorkbook.createSheet();
  private BulkArchiveRequest bulkArchiveRequest =
    BulkArchiveRequest.builder().productSku(PRODUCT_SKU).productName(PRODUCT_NAME).build();
  private BulkProcessData bulkProcessData = new BulkProcessData();
  private BulkProcessData bulkProcessData1 = new BulkProcessData();
  private BulkProcessData bulkProcessData2 = new BulkProcessData();
  private BulkArchiveRequest bulkArchiveRequest1;
  private BulkArchiveRequest bulkArchiveRequest2;
  private ProfileResponse profileResponse = new ProfileResponse();
  private BulkUpdateErrorDTO bulkUpdateErrorDTO;
  private BulkUpdateErrorDTO bulkUpdateErrorDTO2;
  List<BulkProcess> updatedProcessList = new ArrayList<>();
  public static final String FILE_PATH = "file-path";
  public static final String DESCRIPTION =
    "test.xlsx. Product successfully archived: 1. Failed archived: 2. Please check your product and try again.";
  public static final String DESCRIPTION_IN = "test.xlsx. Produk berhasil diarsipkan: 1. Gagal "
    + "diarsipkan: 2. Silakan cek produk Anda dan coba lagi.";

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Mock
  private NotificationService notificationService;

  @Mock
  private FileStorageService fileStorageService;

  @InjectMocks
  private BulkArchiveServiceImpl bulkArchiveServiceImpl;

  @Captor
  private ArgumentCaptor<List<BulkProcessData>> bulkProcessDataArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(bulkArchiveServiceImpl, "rowAccessSize", 100);
    bulkArchiveRequest =
      BulkArchiveRequest.builder().productSku(PRODUCT_SKU).productName(PRODUCT_NAME).build();
    bulkArchiveRequest1 =
      BulkArchiveRequest.builder().productSku(PRODUCT_SKU_2).productName(PRODUCT_NAME_2).build();
    bulkArchiveRequest2 =
      BulkArchiveRequest.builder().productSku(PRODUCT_SKU_3).productName(PRODUCT_NAME_3).build();

    generateDataForSheet(testSheet);
    bulkProcess.setRequestId(REQUEST_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcess.setDescription(FILE_NAME);

    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setInternationalFlag(false);

    bulkUpdateErrorDTO = BulkUpdateErrorDTO.builder().reason(Constant.ERROR_PRODUCT_SKU_ARCHIVE)
      .productSku(PRODUCT_SKU_2).productName(PRODUCT_NAME_2).build();
    bulkUpdateErrorDTO2 = BulkUpdateErrorDTO.builder().reason(Constant.ERROR_PRODUCT_SKU_ARCHIVE)
      .productSku(PRODUCT_SKU_3).productName(PRODUCT_NAME_3).build();

    bulkProcessData1.setSystemErrorCount(1);
    bulkProcessData2.setInputErrorCount(1);
    bulkProcessData2.setSystemErrorCount(0);
    bulkProcessData1.setInputErrorCount(0);
    File bulkArchiveEmptyPath = new File(
      ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE_1 + File.separator);
    bulkArchiveEmptyPath.mkdirs();
    File bulkArchiveNonEmptyFolders = new File(
      ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE + File.separator);
    bulkArchiveNonEmptyFolders.mkdirs();
    File bulkArchiveSingleRowFolders = new File(
      ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE_2 + File.separator);
    bulkArchiveSingleRowFolders.mkdirs();
    XSSFWorkbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("test");
    sheet.createRow(0);
    FileOutputStream fileOut = new FileOutputStream(
      ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE_1 + File.separator
        + BULK_PROCESS_CODE_1 + Constant.DOT + Constant.FILE_TYPE_XLSX);
    workbook.write(fileOut);
    fileOut.close();

    XSSFWorkbook workbookWithData = new XSSFWorkbook();
    Sheet sheetWithData = workbookWithData.createSheet("test");
    generateDataForSheet(sheetWithData);
    FileOutputStream fileOutWithData = new FileOutputStream(
      ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE + File.separator
        + BULK_PROCESS_CODE + Constant.DOT + Constant.FILE_TYPE_XLSX);
    workbookWithData.write(fileOutWithData);
    fileOutWithData.close();

    XSSFWorkbook workbookWithSingleRow = new XSSFWorkbook();
    Sheet sheetWithSingleRow = workbookWithSingleRow.createSheet("test");
    generateDataForSheet(sheetWithSingleRow);
    FileOutputStream fileOutSingleRow = new FileOutputStream(
      ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE_2 + File.separator
        + BULK_PROCESS_CODE_2 + Constant.DOT + Constant.FILE_TYPE_XLSX);
    sheetWithSingleRow.removeRow(sheetWithSingleRow.getRow(2));
    workbookWithSingleRow.write(fileOutSingleRow);
    fileOutSingleRow.close();
  }

  private void generateDataForSheet(Sheet testSheet) {
    Row headerRow = testSheet.createRow(0);
    Cell productSku = headerRow.createCell(0);
    productSku.setCellValue(BulkParameters.BLIBLI_PRODUCT_SKU);
    Cell productName = headerRow.createCell(1);
    productName.setCellValue(BulkParameters.PARENT_PRODUCT_NAME);
    Row dataRow = testSheet.createRow(1);
    Cell productSkuData = dataRow.createCell(0);
    productSkuData.setCellValue(PRODUCT_SKU);
    Cell productNameData = dataRow.createCell(1);
    productNameData.setCellValue(PRODUCT_NAME);
    Row dataRow1 = testSheet.createRow(2);
    Cell productSkuData1 = dataRow1.createCell(0);
    productSkuData1.setCellValue(PRODUCT_SKU);
    Cell productNameData1 = dataRow1.createCell(1);
    productNameData1.setCellValue(PRODUCT_NAME);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(bulkProcessService);
    Mockito.verifyNoMoreInteractions(bulkProcessDataService);
    Mockito.verifyNoMoreInteractions(productBusinessPartnerRepository);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(businessPartnerRepository);
    Mockito.verifyNoMoreInteractions(bulkUpdateServiceUtil);
    Mockito.verifyNoMoreInteractions(notificationService);
    FileUtils.deleteDirectory(new File(ProcessorUtils.BULK_UPDATE_DIR));
  }

  @Test
  public void processArchiveEventTest() throws Exception {
    bulkProcessData.setBulkRequestData("");
    bulkProcessData1.setBulkRequestData("");
    bulkProcessData2.setBulkRequestData("");
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    BulkUpdateEventModel bulkUpdateEventModel =
      BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE)
        .storeId(DEFAULT_STORE_ID).rowNumbers(Arrays.asList(1, 2)).build();
    Mockito.when(
        bulkProcessService.findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(eq(DEFAULT_STORE_ID),
          eq(DEFAULT_BULK_PROCESS_CODE), Mockito.anyList(), eq(BulkProcessData.STATUS_PENDING)))
      .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(Mockito.anyList()))
      .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    Mockito.when(this.objectMapper.readValue(Mockito.anyString(), Mockito.any(Class.class)))
      .thenReturn(bulkArchiveRequest).thenReturn(bulkArchiveRequest1)
      .thenReturn(bulkArchiveRequest2);
    /*Mockito.when(this.productBusinessPartnerRepository.bulkArchiveProductSkus(Mockito.anyString(),
        Mockito.anyString(), eq(true),
        eq(new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2))), Mockito.anyMap()))
      .thenReturn(Arrays.asList(PRODUCT_SKU_2));*/
    this.bulkArchiveServiceImpl.processArchiveEvent(bulkUpdateEventModel);
    Mockito.verify(bulkProcessService)
      .findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
      .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(eq(DEFAULT_STORE_ID),
        eq(DEFAULT_BULK_PROCESS_CODE), Mockito.anyList(), eq(BulkProcessData.STATUS_PENDING));
    Mockito.verify(this.objectMapper, times(3))
      .readValue(Mockito.anyString(), Mockito.any(Class.class));
    Mockito.verify(this.productBusinessPartnerRepository)
      .bulkArchiveProductSkus(null, DEFAULT_REQUEST_ID, true,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2)), new HashMap<>());
    Mockito.verify(this.bulkProcessDataService).saveAndReturnBulkProcessData(Mockito.anyList());
    Mockito.verify(this.bulkProcessDataService)
      .saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());
    List<String> bulkProcessDataStatus =
      this.bulkProcessDataArgumentCaptor.getValue().stream().map(BulkProcessData::getStatus)
        .collect(Collectors.toList());
    Assertions.assertTrue(bulkProcessDataStatus.contains(BulkProcessData.STATUS_SUCCESS));
    Assertions.assertTrue(bulkProcessDataStatus.contains(BulkProcessData.STATUS_FAIL));
  }

  @Test
  public void processArchiveEvent_errorOnArchiveTest() throws Exception {
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcessData.setBulkRequestData("");
    bulkProcessData1.setBulkRequestData("");
    bulkProcessData2.setBulkRequestData("");
    BulkUpdateEventModel bulkUpdateEventModel =
      BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE)
        .storeId(DEFAULT_STORE_ID).rowNumbers(Arrays.asList(1, 2)).build();
    Mockito.when(
        bulkProcessService.findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(eq(DEFAULT_STORE_ID),
          eq(DEFAULT_BULK_PROCESS_CODE), Mockito.anyList(), eq(BulkProcessData.STATUS_PENDING)))
      .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    Mockito.when(this.bulkProcessDataService.saveAndReturnBulkProcessData(Mockito.anyList()))
      .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    Mockito.when(this.objectMapper.readValue(Mockito.anyString(), Mockito.any(Class.class)))
      .thenReturn(bulkArchiveRequest).thenReturn(bulkArchiveRequest1)
      .thenReturn(bulkArchiveRequest2);
    Mockito.when(this.productBusinessPartnerRepository.bulkArchiveProductSkus(Mockito.anyString(),
        Mockito.anyString(), eq(true),
        eq(new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2))), Mockito.anyMap()))
      .thenThrow(ApplicationException.class);
    this.bulkArchiveServiceImpl.processArchiveEvent(bulkUpdateEventModel);
    Mockito.verify(bulkProcessService)
      .findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
      .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(eq(DEFAULT_STORE_ID),
        eq(DEFAULT_BULK_PROCESS_CODE), Mockito.anyList(), eq(BulkProcessData.STATUS_PENDING));
    Mockito.verify(this.objectMapper, times(3))
      .readValue(Mockito.anyString(), Mockito.any(Class.class));
    Mockito.verify(this.productBusinessPartnerRepository)
      .bulkArchiveProductSkus(null, DEFAULT_REQUEST_ID, true,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2)), new HashMap<>());
    Mockito.verify(this.bulkProcessDataService).saveAndReturnBulkProcessData(Mockito.anyList());
    Mockito.verify(this.bulkProcessDataService)
      .saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());
    List<String> bulkProcessDataStatus =
      this.bulkProcessDataArgumentCaptor.getValue().stream().map(BulkProcessData::getStatus)
        .collect(Collectors.toList());
    Assertions.assertTrue(bulkProcessDataStatus.contains(BulkProcessData.STATUS_SUCCESS));
    Assertions.assertTrue(bulkProcessDataStatus.contains(BulkProcessData.STATUS_FAIL));
  }

  @Test
  public void processArchiveEvent_emptyBulkDataListTest() throws Exception {
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    BulkUpdateEventModel bulkUpdateEventModel =
      BulkUpdateEventModel.builder().bulkProcessCode(DEFAULT_BULK_PROCESS_CODE)
        .storeId(DEFAULT_STORE_ID).rowNumbers(Arrays.asList(1, 2)).build();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    Mockito.when(
        bulkProcessService.findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(eq(DEFAULT_STORE_ID),
          eq(DEFAULT_BULK_PROCESS_CODE), Mockito.anyList(), eq(BulkProcessData.STATUS_PENDING)))
      .thenReturn(Collections.EMPTY_LIST);
    this.bulkArchiveServiceImpl.processArchiveEvent(bulkUpdateEventModel);
    Mockito.verify(bulkProcessService)
      .findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService)
      .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(eq(DEFAULT_STORE_ID),
        eq(DEFAULT_BULK_PROCESS_CODE), Mockito.anyList(), eq(BulkProcessData.STATUS_PENDING));
  }

  @Test
  public void addProcessDataAndUpdateProcessTest() throws Exception {
    bulkArchiveServiceImpl.addProcessDataAndUpdateProcess(DEFAULT_STORE_ID, testSheet, bulkProcess);
    Mockito.verify(this.bulkProcessDataService)
      .saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());
    Mockito.verify(this.bulkProcessService).saveOperation(bulkProcess);
    Mockito.verify(objectMapper).writeValueAsString(bulkArchiveRequest);
    Assertions.assertEquals(1, bulkProcessDataArgumentCaptor.getValue().size());
    Assertions.assertEquals(BULK_PROCESS_CODE,
      bulkProcessDataArgumentCaptor.getValue().get(0).getBulkProcessCode());
    Assertions.assertEquals(REQUEST_ID,
      bulkProcessDataArgumentCaptor.getValue().get(0).getRequestId());
    Assertions.assertEquals(BulkProcessData.STATUS_PENDING,
      bulkProcessDataArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(BulkProcess.STATUS_READY_TO_PROCESS, bulkProcess.getStatus());
    Assertions.assertEquals(Integer.valueOf(1), bulkProcess.getTotalCount());
    Assertions.assertEquals(PRODUCT_SKU, bulkProcessDataArgumentCaptor.getValue().get(0).getParentProduct());
  }

  @Test
  public void addProcessDataAndUpdateProcess_exceptionOnParseTest() throws Exception {
    Mockito.doThrow(JsonProcessingException.class).when(objectMapper).writeValueAsString(bulkArchiveRequest);
    bulkArchiveServiceImpl.addProcessDataAndUpdateProcess(DEFAULT_STORE_ID, testSheet, bulkProcess);
    Mockito.verify(this.bulkProcessDataService)
      .saveBulkProcessData(bulkProcessDataArgumentCaptor.capture());
    Mockito.verify(this.bulkProcessService).saveOperation(bulkProcess);
    Mockito.verify(objectMapper).writeValueAsString(bulkArchiveRequest);
    Assertions.assertEquals(0, bulkProcessDataArgumentCaptor.getValue().size());
    Assertions.assertEquals(BulkProcess.STATUS_READY_TO_PROCESS, bulkProcess.getStatus());
  }

  @Test
  public void setFinalStatusAndSendNotificationOnBulkArchiveTest() throws Exception {
    bulkProcessData.setStatus(BulkProcessData.STATUS_SUCCESS);
    bulkProcessData1.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData1.setErrorMessage(Constant.ERROR_PRODUCT_SKU_ARCHIVE);
    bulkProcessData2.setErrorMessage(Constant.ERROR_PRODUCT_SKU_ARCHIVE);
    bulkProcessData1.setBulkRequestData("");
    bulkProcessData2.setBulkRequestData("");
    Mockito.when(this.bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID,
      bulkProcess)).thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      bulkProcess.getBusinessPartnerCode())).thenReturn(profileResponse);
    Mockito.when(this.objectMapper.readValue(Mockito.anyString(), eq(BulkArchiveRequest.class)))
      .thenReturn(bulkArchiveRequest1).thenReturn(bulkArchiveRequest2);
    Mockito.when(fileStorageService.getDownloadLink(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn(FILE_PATH);
    bulkArchiveServiceImpl.setFinalStatusAndSendNotificationOnBulkArchive(bulkProcess,
      DEFAULT_STORE_ID);
    Mockito.verify(this.bulkProcessDataService)
      .findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(this.businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProcess.getBusinessPartnerCode());
    Mockito.verify(this.bulkUpdateServiceUtil)
      .updateBulkProcessNotes(Mockito.anyList(),
        eq(DEFAULT_STORE_ID), eq(BULK_PROCESS_CODE), eq(bulkProcess), eq(false), eq(null));
    Mockito.verify(this.objectMapper, times(2)).readValue(Mockito.anyString(),
      eq(BulkArchiveRequest.class));
    Assertions.assertTrue(BulkProcess.STATUS_PARTIALLY_DONE.equals(bulkProcess.getStatus()));
    verify(notificationService).sendNotificationWithErrorFileGenerated(bulkProcess,
      DESCRIPTION_IN.concat(FILE_PATH), false, false);
  }

  @Test
  public void setFinalStatusAndSendNotificationOnBulkArchive_successOnlyTest() throws Exception {
    File file = new File(
      ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE_1 + File.separator
        + BULK_PROCESS_CODE_1 + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    file.mkdirs();
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE_1);
    bulkProcessData.setStatus(BulkProcessData.STATUS_SUCCESS);
    Mockito.when(this.bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID,
      bulkProcess)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      bulkProcess.getBusinessPartnerCode())).thenReturn(profileResponse);
    Mockito.when(this.objectMapper.readValue(Mockito.anyString(), eq(BulkArchiveRequest.class)))
      .thenReturn(bulkArchiveRequest1).thenReturn(bulkArchiveRequest2);
    Mockito.when(fileStorageService.getDownloadLink(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn(FILE_PATH);
    bulkArchiveServiceImpl.setFinalStatusAndSendNotificationOnBulkArchive(bulkProcess,
      DEFAULT_STORE_ID);
    Mockito.verify(this.bulkProcessDataService)
      .findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(this.businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProcess.getBusinessPartnerCode());
    Mockito.verify(this.bulkUpdateServiceUtil)
      .updateBulkProcessNotes(Mockito.anyList(),
        eq(DEFAULT_STORE_ID), eq(BULK_PROCESS_CODE_1), eq(bulkProcess), eq(false), eq(null));
    Assertions.assertTrue(BulkProcess.STATUS_FINISHED.equals(bulkProcess.getStatus()));
    verify(notificationService).sendNotificationWithErrorFileGenerated(Mockito.any(),
      Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyBoolean());
  }

  @Test
  public void setFinalStatusAndSendNotificationOnBulkArchive_abortedTest() throws Exception {
    bulkProcessData1.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData1.setErrorMessage(Constant.ERROR_PRODUCT_SKU_ARCHIVE);
    bulkProcessData2.setErrorMessage(Constant.ERROR_PRODUCT_SKU_ARCHIVE);
    bulkProcessData1.setBulkRequestData("");
    bulkProcessData2.setBulkRequestData("");
    Mockito.when(this.bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID,
      bulkProcess)).thenReturn(Arrays.asList(bulkProcessData1, bulkProcessData2));
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      bulkProcess.getBusinessPartnerCode())).thenReturn(profileResponse);
    Mockito.when(this.objectMapper.readValue(Mockito.anyString(), eq(BulkArchiveRequest.class)))
      .thenReturn(bulkArchiveRequest1).thenReturn(bulkArchiveRequest2);
    Mockito.when(fileStorageService.getDownloadLink(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn(FILE_PATH);
    bulkArchiveServiceImpl.setFinalStatusAndSendNotificationOnBulkArchive(bulkProcess,
      DEFAULT_STORE_ID);
    Mockito.verify(this.bulkProcessDataService)
      .findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(this.businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProcess.getBusinessPartnerCode());
    Mockito.verify(this.bulkUpdateServiceUtil)
      .updateBulkProcessNotes(Mockito.anyList(),
        eq(DEFAULT_STORE_ID), eq(BULK_PROCESS_CODE), eq(bulkProcess), eq(false), eq(null));
    Mockito.verify(this.objectMapper, times(2)).readValue(Mockito.anyString(),
      eq(BulkArchiveRequest.class));
    Assertions.assertTrue(BulkProcess.STATUS_ABORTED.equals(bulkProcess.getStatus()));
    verify(notificationService).sendNotificationWithErrorFileGenerated(Mockito.any(),
      Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyBoolean());
  }

  @Test
  public void setFinalStatusAndSendNotificationOnBulkArchive_exceptionTest() {
    Mockito.when(this.bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID,
      bulkProcess)).thenThrow(ApplicationRuntimeException.class);
    bulkArchiveServiceImpl.setFinalStatusAndSendNotificationOnBulkArchive(bulkProcess,
      DEFAULT_STORE_ID);
    Mockito.verify(this.bulkProcessDataService)
      .findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
    Assertions.assertTrue(BulkProcess.STATUS_ABORTED.equals(bulkProcess.getStatus()));
  }

  @Test
  public void setFinalStatusAndSendNotificationOnBulkArchive_internationalMerchantTest()
    throws Exception {
    profileResponse.getCompany().setInternationalFlag(true);
    bulkProcessData.setStatus(BulkProcessData.STATUS_SUCCESS);
    bulkProcessData1.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData1.setErrorMessage(Constant.ERROR_PRODUCT_SKU_ARCHIVE);
    bulkProcessData2.setErrorMessage(Constant.ERROR_PRODUCT_SKU_ARCHIVE);
    bulkProcessData1.setBulkRequestData("");
    bulkProcessData2.setBulkRequestData("");
    Mockito.when(this.bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID,
      bulkProcess)).thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      bulkProcess.getBusinessPartnerCode())).thenReturn(profileResponse);
    Mockito.when(this.objectMapper.readValue(Mockito.anyString(), eq(BulkArchiveRequest.class)))
      .thenReturn(bulkArchiveRequest1).thenReturn(bulkArchiveRequest2);
    Mockito.when(fileStorageService.getDownloadLink(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn(FILE_PATH);
    bulkArchiveServiceImpl.setFinalStatusAndSendNotificationOnBulkArchive(bulkProcess,
      DEFAULT_STORE_ID);
    Mockito.verify(this.bulkProcessDataService)
      .findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(this.businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProcess.getBusinessPartnerCode());
    Mockito.verify(this.bulkUpdateServiceUtil)
      .updateBulkProcessNotes(Mockito.anyList(),
        eq(DEFAULT_STORE_ID), eq(BULK_PROCESS_CODE), eq(bulkProcess), eq(false), eq(null));
    Mockito.verify(this.objectMapper, times(2)).readValue(Mockito.anyString(),
      eq(BulkArchiveRequest.class));
    Assertions.assertTrue(BulkProcess.STATUS_PARTIALLY_DONE.equals(bulkProcess.getStatus()));
    verify(notificationService).sendNotificationWithErrorFileGenerated(bulkProcess,
      DESCRIPTION.concat(FILE_PATH), false, false);
  }

  @Test
  public void setFinalStatusAndSendNotificationOnBulkArchive_readValueExceptionTest()
    throws Exception {
    profileResponse.getCompany().setInternationalFlag(true);
    bulkProcessData.setStatus(BulkProcessData.STATUS_SUCCESS);
    bulkProcessData1.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData1.setErrorMessage(Constant.ERROR_PRODUCT_SKU_ARCHIVE);
    bulkProcessData2.setErrorMessage(Constant.ERROR_PRODUCT_SKU_ARCHIVE);
    bulkProcessData1.setBulkRequestData("");
    bulkProcessData2.setBulkRequestData("");
    Mockito.when(this.bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID,
      bulkProcess)).thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      bulkProcess.getBusinessPartnerCode())).thenReturn(profileResponse);
    Mockito.when(this.objectMapper.readValue(Mockito.anyString(), eq(BulkArchiveRequest.class)))
      .thenReturn(bulkArchiveRequest1).thenThrow(JsonProcessingException.class);
    Mockito.when(fileStorageService.getDownloadLink(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn(FILE_PATH);
    bulkArchiveServiceImpl.setFinalStatusAndSendNotificationOnBulkArchive(bulkProcess,
      DEFAULT_STORE_ID);
    Mockito.verify(this.bulkProcessDataService)
      .findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(this.businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProcess.getBusinessPartnerCode());
    Mockito.verify(this.bulkUpdateServiceUtil)
      .updateBulkProcessNotes(Mockito.anyList(),
        eq(DEFAULT_STORE_ID), eq(BULK_PROCESS_CODE), eq(bulkProcess), eq(false), eq(null));
    Mockito.verify(this.objectMapper, times(2)).readValue(Mockito.anyString(),
      eq(BulkArchiveRequest.class));
    Assertions.assertTrue(BulkProcess.STATUS_PARTIALLY_DONE.equals(bulkProcess.getStatus()));
    verify(notificationService).sendNotificationWithErrorFileGenerated(bulkProcess,
      DESCRIPTION.concat(FILE_PATH), false, false);
  }

  @Test
  public void setFinalStatusAndSendNotificationOnBulkArchive_singleRowTest() throws Exception {
    bulkProcessData.setStatus(BulkProcessData.STATUS_SUCCESS);
    bulkProcessData1.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData2.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData1.setErrorMessage(Constant.ERROR_PRODUCT_SKU_ARCHIVE);
    bulkProcessData2.setErrorMessage(Constant.ERROR_PRODUCT_SKU_ARCHIVE);
    bulkProcessData1.setBulkRequestData("");
    bulkProcessData2.setBulkRequestData("");
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE_2);
    Mockito.when(this.bulkProcessDataService.findByStoreIdAndBulkProcess(DEFAULT_STORE_ID,
      bulkProcess)).thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      bulkProcess.getBusinessPartnerCode())).thenReturn(profileResponse);
    Mockito.when(this.objectMapper.readValue(Mockito.anyString(), eq(BulkArchiveRequest.class)))
      .thenReturn(bulkArchiveRequest1).thenReturn(bulkArchiveRequest2);
    Mockito.when(fileStorageService.getDownloadLink(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString())).thenReturn(FILE_PATH);
    bulkArchiveServiceImpl.setFinalStatusAndSendNotificationOnBulkArchive(bulkProcess,
      DEFAULT_STORE_ID);
    Mockito.verify(this.bulkProcessDataService)
      .findByStoreIdAndBulkProcess(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(this.businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProcess.getBusinessPartnerCode());
    Mockito.verify(this.bulkUpdateServiceUtil)
      .updateBulkProcessNotes(Mockito.anyList(),
        eq(DEFAULT_STORE_ID), eq(BULK_PROCESS_CODE_2), eq(bulkProcess), eq(false), eq(null));
    Mockito.verify(this.objectMapper, times(2)).readValue(Mockito.anyString(),
      eq(BulkArchiveRequest.class));
    Assertions.assertTrue(BulkProcess.STATUS_PARTIALLY_DONE.equals(bulkProcess.getStatus()));
    verify(notificationService).sendNotificationWithErrorFileGenerated(bulkProcess,
      DESCRIPTION_IN.concat(FILE_PATH), false, false);
  }

  @Test
  public void addProcessDataAndUpdateProcess_emptySheetTest() throws Exception {
    testSheet.removeRow(testSheet.getRow(2));
    testSheet.removeRow(testSheet.getRow(1));
    bulkArchiveServiceImpl.addProcessDataAndUpdateProcess(DEFAULT_STORE_ID, testSheet, bulkProcess);
    Mockito.verify(this.bulkProcessService).saveOperation(bulkProcess);
    Assertions.assertEquals(BulkProcess.STATUS_PROCESSED, bulkProcess.getStatus());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getTotalCount());
  }

  @Test
  public void updateStatusForFailedArchivalTest() {
    Map<String, BulkProcessData> productSkuBulkProcessDataMap = new HashMap<>();
    productSkuBulkProcessDataMap.put(PRODUCT_SKU, bulkProcessData);
    Map<String, String> productSkuErrorMessageMap = new HashMap<>();
    productSkuErrorMessageMap.put(PRODUCT_SKU, "Product is already archived");
    bulkArchiveServiceImpl.updateStatusForFailedArchival(new ArrayList<>(), List.of(PRODUCT_SKU),
      productSkuBulkProcessDataMap, productSkuErrorMessageMap);
    Mockito.verify(this.bulkProcessService, times(0)).saveOperation(bulkProcess);
  }
}