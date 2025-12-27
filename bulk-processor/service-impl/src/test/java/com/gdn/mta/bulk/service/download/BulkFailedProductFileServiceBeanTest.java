package com.gdn.mta.bulk.service.download;


import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkDownloadEntityStatus;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadDTO;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.helper.BulkProductProcessHelper;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.repository.BulkDownloadAuditRepository;
import com.gdn.mta.bulk.service.BulkDownloadService;
import com.gdn.mta.bulk.service.BulkProcessDataService;
import com.gdn.mta.bulk.service.FileStorageService;
import com.gdn.mta.bulk.service.NotificationService;
import com.gdn.mta.bulk.service.PCBOutboundService;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.partners.bulk.util.BulkCnCreationHeaderNames;
import com.gdn.partners.bulk.util.Constant;
import org.apache.commons.lang3.StringUtils;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;
import com.gdn.mta.bulk.util.POIUtil;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

public class BulkFailedProductFileServiceBeanTest {

  private static final Long ID = 1L;
  private static final String CODE = "code";
  private static final String USERNAME = "username";
  private static final String DESCRIPTION = "description";
  private static final String PRODUCT_LEVEL3_PROCESSOR_DIR = "ProductLevel3Processor";
  private final static String DEFAULT_STORE_ID = "10001";

  @InjectMocks
  private BulkFailedProductFileServiceBean service;

  @Mock
  private BulkProductProcessHelper helper;
  @Mock
  private BulkDownloadAuditRepository bulkDownloadAuditRepository;
  @Mock
  private Workbook workbook;

  @Mock
  private NotificationService notificationService;

  @Mock
  private PCBOutboundService pcbOutboundService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private BulkDownloadService bulkDownloadService;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private ObjectMapper objectMapper;

  @TempDir
  Path tempFolder;

  @Captor
  private ArgumentCaptor<List<List<String>>> listOfListOfStringsArgumentCaptor;

  private List<List<Object>> rawExcelHeaders;
  private List<List<List<Object>>> rawExcelDatas;
  private List<List<Object>> invalidRows;
  private BulkProcess bulkProcess;
  private BulkDownloadEntity bulkDownloadEntity;
  private File folder;
  private File file;
  private CategoryDetailResponse category;

  @AfterEach
  public void breakdown() {
    verifyNoMoreInteractions(helper);
    verifyNoMoreInteractions(bulkDownloadAuditRepository);
    verifyNoMoreInteractions(notificationService);
    verifyNoMoreInteractions(fileStorageService);
    verifyNoMoreInteractions(bulkDownloadService);
    verifyNoMoreInteractions(bulkProcessDataService);
  }

  public void setBulkProcess() {
    bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(CODE);
    bulkProcess.setBusinessPartnerCode(CODE);
    bulkProcess.setStoreId(DEFAULT_STORE_ID);
    bulkProcess.setCreatedBy(USERNAME);
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setDescription(DESCRIPTION);
    bulkProcess.setEndDate(new Date());
    bulkProcess.setBulkProcessNotes(new ArrayList<>());
    bulkProcess.getBulkProcessNotes().add(new BulkProcessNotes());
    bulkProcess.getBulkProcessNotes().add(new BulkProcessNotes());
    bulkProcess.setInternationalMerchant(false);
  }

  public void setBulkDownloadEntity() {
    bulkDownloadEntity = new BulkDownloadEntity();
    bulkDownloadEntity.setBusinessPartnerCode(CODE);
    bulkDownloadEntity.setCreatedBy(USERNAME);
    bulkDownloadEntity.setCreatedDate(new Date());
    bulkDownloadEntity.setDescription(DESCRIPTION);
    bulkDownloadEntity.setEntityType(BulkProcessEntity.FAILED_PRODUCT.toString());
    bulkDownloadEntity.setFileName(CODE + ".xls");
    bulkDownloadEntity.setMarkForDelete(false);
    bulkDownloadEntity.setRequestId(CODE);
    bulkDownloadEntity.setStatus(BulkDownloadEntityStatus.STATUS_SUCCESS.getStatusValue());
    bulkDownloadEntity.setId(ID);
  }

  private CategoryDetailResponse categoryDetailResponseBuilder() {
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCatalog(new CatalogResponse());
    CategoryAttributeResponse categoryAttributePredefined = new CategoryAttributeResponse();
    categoryAttributePredefined.setMarkForDelete(false);
    categoryAttributePredefined.setStoreId(DEFAULT_STORE_ID);
    categoryAttributePredefined.setAttribute(attributeResponsePredefinedBuilder(false));

    CategoryAttributeResponse categoryAttributePredefined2 = new CategoryAttributeResponse();
    categoryAttributePredefined2.setMarkForDelete(false);
    categoryAttributePredefined2.setStoreId(DEFAULT_STORE_ID);
    categoryAttributePredefined2.setAttribute(attributeResponsePredefinedMFDBuilder(false));

    CategoryAttributeResponse categoryAttributePredefined3 = new CategoryAttributeResponse();
    categoryAttributePredefined3.setMarkForDelete(true);
    categoryAttributePredefined3.setStoreId(DEFAULT_STORE_ID);
    categoryAttributePredefined3.setAttribute(attributeResponsePredefinedBuilder(false));

    CategoryAttributeResponse categoryAttributePredefined4 = new CategoryAttributeResponse();
    categoryAttributePredefined4.setMarkForDelete(false);
    categoryAttributePredefined4.setStoreId(DEFAULT_STORE_ID);
    categoryAttributePredefined4.setAttribute(attributeResponsePredefinedBuilderGaransi(false));

    CategoryAttributeResponse categoryAttributeDescriptive = new CategoryAttributeResponse();
    categoryAttributeDescriptive.setMarkForDelete(false);
    categoryAttributeDescriptive.setStoreId(DEFAULT_STORE_ID);
    categoryAttributeDescriptive.setAttribute(attributeResponseDescriptiveBuilder(false));

    CategoryAttributeResponse categoryAttributeDefining = new CategoryAttributeResponse();
    categoryAttributeDefining.setMarkForDelete(false);
    categoryAttributeDefining.setStoreId(DEFAULT_STORE_ID);
    categoryAttributeDefining.setAttribute(attributeResponseDefiningBuilder(false));

    CategoryAttributeResponse categoryAttributeDefining2 = new CategoryAttributeResponse();
    categoryAttributeDefining2.setMarkForDelete(false);
    categoryAttributeDefining2.setStoreId(DEFAULT_STORE_ID);
    categoryAttributeDefining2.setAttribute(attributeResponseDefiningMFDBuilder(false));

    CategoryAttributeResponse categoryAttributeDefining3 = new CategoryAttributeResponse();
    categoryAttributeDefining3.setMarkForDelete(true);
    categoryAttributeDefining3.setStoreId(DEFAULT_STORE_ID);
    categoryAttributeDefining3.setAttribute(attributeResponseDefiningBuilder(false));

    categoryDetailResponse.setCategoryAttributes(new ArrayList<CategoryAttributeResponse>());
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributePredefined);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributePredefined2);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributePredefined3);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributePredefined4);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributeDescriptive);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributeDefining);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributeDefining2);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributeDefining3);
    return categoryDetailResponse;
  }

  private CategoryDetailResponse categoryDetailResponseBuilder2() {
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCatalog(new CatalogResponse());
    CategoryAttributeResponse categoryAttributePredefined = new CategoryAttributeResponse();
    categoryAttributePredefined.setMarkForDelete(false);
    categoryAttributePredefined.setStoreId(DEFAULT_STORE_ID);
    categoryAttributePredefined.setAttribute(attributeResponsePredefinedBuilder(true));

    CategoryAttributeResponse categoryAttributePredefined2 = new CategoryAttributeResponse();
    categoryAttributePredefined2.setMarkForDelete(false);
    categoryAttributePredefined2.setStoreId(DEFAULT_STORE_ID);
    categoryAttributePredefined2.setAttribute(attributeResponsePredefinedMFDBuilder(true));

    CategoryAttributeResponse categoryAttributePredefined3 = new CategoryAttributeResponse();
    categoryAttributePredefined3.setMarkForDelete(true);
    categoryAttributePredefined3.setStoreId(DEFAULT_STORE_ID);
    categoryAttributePredefined3.setAttribute(attributeResponsePredefinedBuilder(true));

    CategoryAttributeResponse categoryAttributePredefined4 = new CategoryAttributeResponse();
    categoryAttributePredefined4.setMarkForDelete(false);
    categoryAttributePredefined4.setStoreId(DEFAULT_STORE_ID);
    categoryAttributePredefined4.setAttribute(attributeResponsePredefinedBuilderGaransi(true));

    CategoryAttributeResponse categoryAttributeDescriptive = new CategoryAttributeResponse();
    categoryAttributeDescriptive.setMarkForDelete(false);
    categoryAttributeDescriptive.setStoreId(DEFAULT_STORE_ID);
    categoryAttributeDescriptive.setAttribute(attributeResponseDescriptiveBuilder(true));

    CategoryAttributeResponse categoryAttributeDefining = new CategoryAttributeResponse();
    categoryAttributeDefining.setMarkForDelete(false);
    categoryAttributeDefining.setStoreId(DEFAULT_STORE_ID);
    categoryAttributeDefining.setAttribute(attributeResponseDefiningBuilder(true));

    CategoryAttributeResponse categoryAttributeDefining2 = new CategoryAttributeResponse();
    categoryAttributeDefining2.setMarkForDelete(false);
    categoryAttributeDefining2.setStoreId(DEFAULT_STORE_ID);
    categoryAttributeDefining2.setAttribute(attributeResponseDefiningMFDBuilder(true));

    CategoryAttributeResponse categoryAttributeDefining3 = new CategoryAttributeResponse();
    categoryAttributeDefining3.setMarkForDelete(true);
    categoryAttributeDefining3.setStoreId(DEFAULT_STORE_ID);
    categoryAttributeDefining3.setAttribute(attributeResponseDefiningBuilder(true));

    CategoryAttributeResponse categoryAttributePredefined5 = new CategoryAttributeResponse();
    categoryAttributePredefined5.setMarkForDelete(false);
    categoryAttributePredefined5.setStoreId(DEFAULT_STORE_ID);
    categoryAttributePredefined5.setAttribute(attributeResponsePredefinedBuilderFamilyColour(true));

    categoryDetailResponse.setCategoryAttributes(new ArrayList<CategoryAttributeResponse>());
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributePredefined);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributePredefined2);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributePredefined3);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributePredefined4);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributePredefined5);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributeDescriptive);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributeDefining);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributeDefining2);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributeDefining3);
    return categoryDetailResponse;
  }

  private AttributeResponse attributeResponsePredefinedBuilderFamilyColour(boolean setEnName) {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attr-predefined-id-family-colour");
    attributeResponse.setName("Family Colour");
    if(setEnName) attributeResponse.setNameEnglish("Family Colour");
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    attributeResponse.setSkuValue(false);
    return attributeResponse;
  }

  private AttributeResponse attributeResponsePredefinedBuilderGaransi(boolean setEnName) {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attr-predefined-id-garansi");
    attributeResponse.setName("Garansi");
    if(setEnName) attributeResponse.setNameEnglish("Garansi");
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    attributeResponse.setSkuValue(false);
    return attributeResponse;
  }

  private AttributeResponse attributeResponsePredefinedBuilder(boolean setEnName) {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attr-predefined-id");
    attributeResponse.setName("Brand");
    if(setEnName) attributeResponse.setNameEnglish("Brand");
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    attributeResponse.setSkuValue(false);
    return attributeResponse;
  }


  private AttributeResponse attributeResponsePredefinedMFDBuilder(boolean setEnName) {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attr-predefined-id1");
    attributeResponse.setName("x1");
    if(setEnName) attributeResponse.setNameEnglish("x1");
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    attributeResponse.setSkuValue(false);
    attributeResponse.setMarkForDelete(true);
    return attributeResponse;
  }

  private AttributeResponse attributeResponseDescriptiveBuilder(boolean setEnName) {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attr-descriptive-id");
    attributeResponse.setName("Resolusi Layar");
    if(setEnName) attributeResponse.setNameEnglish("Resolusi Layar");
    attributeResponse.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    attributeResponse.setSkuValue(false);
    return attributeResponse;
  }

  private AttributeResponse attributeResponseDefiningBuilder(boolean setEnName) {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attr-defining-id");
    attributeResponse.setName("Warna");
    if(setEnName) attributeResponse.setNameEnglish("Warna");
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    attributeResponse.setSkuValue(false);
    attributeResponse.setAllowedAttributeValues(new ArrayList<AllowedAttributeValueResponse>());
    attributeResponse.getAllowedAttributeValues().add(allowedAttributeValueResponseBuilder());
    return attributeResponse;
  }

  private AttributeResponse attributeResponseDefiningMFDBuilder(boolean setEnName) {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attr-defining-id1");
    attributeResponse.setName("Colour");
    if(setEnName) attributeResponse.setNameEnglish("Colour");
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    attributeResponse.setSkuValue(false);
    attributeResponse.setMarkForDelete(true);
    attributeResponse.setAllowedAttributeValues(new ArrayList<AllowedAttributeValueResponse>());
    attributeResponse.getAllowedAttributeValues().add(allowedAttributeValueResponseBuilder());
    return attributeResponse;
  }

  private AllowedAttributeValueResponse allowedAttributeValueResponseBuilder() {
    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setId("allowed-attr-id");
    allowedAttributeValueResponse.setValue("Putih");
    return allowedAttributeValueResponse;
  }



  public void setRawExcelData() {
    rawExcelDatas = new ArrayList<>();

    List<List<Object>> contents = new ArrayList<>();
    List<Object> row1 = new ArrayList<>();
    List<Object> row2 = new ArrayList<>();
    contents.add(row1);
    contents.add(row2);
    rawExcelDatas.add(contents);

    row1.add("test");
    row1.add("0");

    row2.add("hahe");
    row2.add(1);

  }

  public void setRawExcelHeader() {
    rawExcelHeaders = new ArrayList<>();
    List<Object> contents = new ArrayList<>();
    contents.add("test");
    contents.add(1);
    rawExcelHeaders.add(contents);
  }

  public void setInvalidRows(){
    invalidRows = new ArrayList<>();
    List<Object> row1 = new ArrayList<>();

    row1.add("test");
    row1.add("0");
    row1.add("1");
    row1.add("2");
    row1.add("3");
    row1.add("4");
    row1.add("5");
    row1.add("6");
    row1.add("7");
    row1.add("8");
    row1.add("9");
    invalidRows.add(row1);
  }

  private static List<List<Object>> getInputRowData(String fileName) throws Exception {
    List<List<Object>> newList = new ArrayList<>();
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + fileName);
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    LinkedHashMap<String, Object> dataMap = objectMapper.readValue(inputStream, typeRef);
    List<Object> rowData = new ArrayList<>(dataMap.values());
    rowData.add("Alat Musik->Aksesoris Alat Musik->Strings");
    rowData.add("C3-Id");
    rowData.add("1+C3-Id");
    rowData.add(10);
    newList.add(rowData);
    return newList;
  }

  public void setFolder() throws IOException {
    folder = Files.createDirectory(tempFolder.resolve("tempFolder")).toFile();
    file = Files.createFile(tempFolder.resolve("tempFile.txt")).toFile();
  }

  public void setWhen() throws IOException {
    when(bulkDownloadAuditRepository.save(any(BulkDownloadEntity.class))).thenReturn(
        bulkDownloadEntity);
    when(helper.generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
        eq(BulkParameters.DATA_SHEET), eq(true)))
        .thenReturn(workbook);
    when(helper.getFailedProductDirectory(CODE)).thenReturn(folder.getAbsolutePath());
    when(helper.getFilePath(folder.getAbsolutePath(), CODE + ".xls")).thenReturn(
        file.getAbsolutePath());
  }

  @BeforeEach
  public void setup() throws IOException {
    initMocks(this);

    setInvalidRows();
    setRawExcelHeader();
    setRawExcelData();
    setBulkProcess();
    setBulkDownloadEntity();
    setFolder();
    setWhen();
  }

  @Test
  public void testCreateFile() throws Exception {
    service.createFile(rawExcelHeaders, bulkProcess, invalidRows, BulkParameters.DATA_SHEET, true);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper).generateDataSheetWithHSSFWorkBook(listOfListOfStringsArgumentCaptor.capture(),
        Mockito.<List<List<String>>>any(), eq(BulkParameters.DATA_SHEET), eq(true));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    Assertions.assertEquals(rawExcelHeaders.get(0).get(0),
        listOfListOfStringsArgumentCaptor.getAllValues().get(0).get(0).get(0));
    Assertions.assertEquals(String.valueOf(rawExcelHeaders.get(0).get(1)),
        listOfListOfStringsArgumentCaptor.getAllValues().get(0).get(0).get(1));
  }

  @Test
  public void testCreateFileWithError() throws Exception {
    List<Object> contents = new ArrayList<>();
    contents.add("Notes");
    contents.add(1);
    rawExcelHeaders.add(contents);
    service.createFile(rawExcelHeaders, bulkProcess, invalidRows, BulkParameters.DATA_SHEET, true);
    verify(bulkDownloadAuditRepository).save(Mockito.any(BulkDownloadEntity.class));
    verify(helper).generateDataSheetWithHSSFWorkBook(listOfListOfStringsArgumentCaptor.capture(),
        Mockito.<List<List<String>>>any(), eq(BulkParameters.DATA_SHEET), eq(true));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    Assertions.assertEquals(rawExcelHeaders.get(0).get(0),
        listOfListOfStringsArgumentCaptor.getAllValues().get(0).get(0).get(0));
    Assertions.assertEquals(String.valueOf(rawExcelHeaders.get(0).get(1)),
        listOfListOfStringsArgumentCaptor.getAllValues().get(0).get(0).get(1));
  }

  @Test
  public void testCreateFile_NullHeader() throws ApplicationException, IOException {
    Assertions.assertThrows(ApplicationException.class,
        () -> service.createFile(null, bulkProcess, invalidRows,
            BulkParameters.DATA_SHEET, true));
  }

  @Test
  public void testCreateFile_EmptyHeader() throws ApplicationException, IOException {
    Assertions.assertThrows(ApplicationException.class,
        () -> service.createFile(new ArrayList<>(), bulkProcess, invalidRows,
            BulkParameters.DATA_SHEET, true));
  }

  @Test
  public void testCreateFile_SaveWithException() throws Exception {
    when(bulkDownloadAuditRepository.save(any(BulkDownloadEntity.class))).thenThrow(
        new RuntimeException());

    try {
      service.createFile(rawExcelHeaders, bulkProcess, invalidRows, BulkParameters.DATA_SHEET, true);
    } catch (Exception e) {
      verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
      verify(helper).generateDataSheetWithHSSFWorkBook(listOfListOfStringsArgumentCaptor.capture(),
          Mockito.<List<List<String>>>any(), eq(BulkParameters.DATA_SHEET), eq(true));
    }
  }

  private Calendar getLastWeekDate() {
    Date date = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(date);
    cal.add(Calendar.DATE, -7);
    return cal;
  }

  private File createLastWeekTempDirectory() throws IOException {
    File tempDirectory = Files.createFile(tempFolder.resolve("tempFilea.txt")).toFile();
    tempDirectory.mkdir();
    Calendar cal = getLastWeekDate();
    Path tempDeleted = Paths.get(tempDirectory.getAbsolutePath());
    Files.setAttribute(tempDeleted, "lastModifiedTime", FileTime.fromMillis(cal.getTimeInMillis()));
    return tempDirectory;
  }

  @Test
  public void testDeleteFileWithModifiedDateLastWeek() throws ApplicationException, IOException {
    String tempDirectoryPath = createLastWeekTempDirectory().getParent();
    when(helper.getFailedProductDirectory(null)).thenReturn(tempDirectoryPath);
    int result = service.deleteFileWithModifiedDateLastWeek();
    verify(helper).getFailedProductDirectory(null);
    Assertions.assertFalse(result >= 1);
  }

  @Test
  public void testDeleteFileWithModifiedDateLastWeek_Exception() throws ApplicationException,
      IOException {
//    File tempFile = tempFolder.newFile();
    File tempFile = Files.createFile(tempFolder.resolve("tempFile2.txt")).toFile();
    Path tempPath = Paths.get(tempFile.toURI());
    Calendar cal = getLastWeekDate();
    Files.setAttribute(tempPath, "lastModifiedTime", FileTime.fromMillis(cal.getTimeInMillis()));
    when(helper.getFailedProductDirectory(null)).thenReturn(tempFile.getParent());
    int result = service.deleteFileWithModifiedDateLastWeek();
    verify(helper).getFailedProductDirectory(null);
    Assertions.assertTrue(result == 0);
  }

  @Test
  public void testGetDownloadLinkHtml() {
    String result = service.getDownloadLinkHtml("test");
    Assertions.assertTrue(result.contains("test"));
  }

  @Test
  public void TestCreateFileAndSendNotification() throws Exception {
    bulkProcess.setNotes(Constant.GENERIC);
    when(helper
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false))).thenReturn(workbook);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());

  }

  @Test
  public void TestCreateFileAndSendNotificationBundling() throws Exception {
    ReflectionTestUtils.setField(service, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(service, "productBundlingEligibleMerchantTypes", "TD");
    bulkProcess.setNotes(Constant.GENERIC);
    when(helper
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false))).thenReturn(workbook);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "TD", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
        Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());

  }

  @Test
  public void TestCreateFileAndSendNotificationMPPSwitchEnabled() throws Exception {
    bulkProcess.setNotes(Constant.GENERIC);
    when(helper
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false))).thenReturn(workbook);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());

  }

  @Test
  public void TestCreateFileAndSendNotificationPureDelivery() throws Exception {
    bulkProcess.setNotes(Constant.GENERIC);
    when(helper
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false))).thenReturn(workbook);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void TestCreateFileAndSendNotificationCnc() throws Exception {
    bulkProcess.setNotes(Constant.GENERIC);
    when(helper
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false))).thenReturn(workbook);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.DELIVERY_AND_CNC, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
        Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void TestCreateFileAndSendNotificationBfb() throws Exception {
    bulkProcess.setNotes(Constant.GENERIC);
    when(helper
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false))).thenReturn(workbook);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowDataBfb"),
        MerchantStatusType.BFB, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
        Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void createFileAndSendNotificationBfbTest() throws Exception {
    bulkProcess.setNotes(Constant.GENERIC);
    when(helper
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false))).thenReturn(workbook);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowDataBfb"),
        MerchantStatusType.BFB, "", true);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
        Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void TestCreateFileAndSendNotificationBfbAndCnc() throws Exception {
    bulkProcess.setNotes(Constant.GENERIC);
    when(helper
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false))).thenReturn(workbook);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowDataBfb"),
        MerchantStatusType.BFB_AND_CNC, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
        Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void TestCreateFileAndSendNotificationMPPSwitchEnabledInternationalMerchant() throws Exception {
    bulkProcess.setNotes(Constant.GENERIC);
    bulkProcess.setInternationalMerchant(true);
    when(helper
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false))).thenReturn(workbook);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());

  }

  @Test
  public void TestCreateFileAndSendNotificationPureDeliveryInternationalMerchant()  throws Exception {
    bulkProcess.setNotes(Constant.GENERIC);
    bulkProcess.setInternationalMerchant(true);
    when(helper
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false))).thenReturn(workbook);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());

  }

  @Test
  public void TestCreateFileAndSendNotificationDeliverAndCncInternationalMerchant() throws Exception {
    bulkProcess.setNotes(Constant.GENERIC);
    bulkProcess.setInternationalMerchant(true);
    when(
        helper.generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false))).thenReturn(workbook);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.DELIVERY_AND_CNC, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper).generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(),
        Mockito.<List<List<String>>>any(), anyString(), eq(false));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void TestCreateFileAndSendNotificationBfbInternationalMerchant() throws Exception {
    bulkProcess.setNotes(Constant.GENERIC);
    bulkProcess.setInternationalMerchant(true);
    when(
        helper.generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false))).thenReturn(workbook);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowDataBfb"),
        MerchantStatusType.BFB, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper).generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(),
        Mockito.<List<List<String>>>any(), anyString(), eq(false));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void TestCreateFileAndSendNotificationBfbAndCncInternationalMerchant() throws Exception {
    bulkProcess.setNotes(Constant.GENERIC);
    bulkProcess.setInternationalMerchant(true);
    when(
        helper.generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(false))).thenReturn(workbook);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowDataBfb"),
        MerchantStatusType.BFB_AND_CNC, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper).generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(),
        Mockito.<List<List<String>>>any(), anyString(), eq(false));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void TestCreateFileAndSendNotification_null() throws Exception {
    service.createFileAndSendNotification(bulkProcess, null, MerchantStatusType.PURE_DELIVERY, "", false);
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void createFileAndSendNotificationCnErrorDetailsTest() throws Exception {
    when(helper.generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
        anyString(), eq(true))).thenReturn(workbook);
    CategoryDetailResponse categoryDetailResponse = categoryDetailResponseBuilder();
    when(pcbOutboundService.getCategoryDetailResponse(any())).thenReturn(categoryDetailResponse);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(true));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void createFileAndSendNotificationCnErrorDetailsMPPSwitchEnabledTest() throws Exception {
    when(helper.generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
        anyString(), eq(true))).thenReturn(workbook);
    CategoryDetailResponse categoryDetailResponse = categoryDetailResponseBuilder();
    when(pcbOutboundService.getCategoryDetailResponse(any())).thenReturn(categoryDetailResponse);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(true));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void createFileAndSendNotificationCnErrorDetailsMPPSwitchEnabledInternationalMerchantTest() throws Exception {
    bulkProcess.setInternationalMerchant(true);
    when(helper.generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
        anyString(), eq(true))).thenReturn(workbook);
    CategoryDetailResponse categoryDetailResponse = categoryDetailResponseBuilder();
    when(pcbOutboundService.getCategoryDetailResponse(any())).thenReturn(categoryDetailResponse);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(true));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void createFileAndSendNotificationCnErrorDetailsMPPSwitchEnabledCNCTest() throws Exception {
    when(helper.generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
        anyString(), eq(true))).thenReturn(workbook);
    CategoryDetailResponse categoryDetailResponse = categoryDetailResponseBuilder();
    when(pcbOutboundService.getCategoryDetailResponse(any())).thenReturn(categoryDetailResponse);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(true));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void createFileAndSendNotificationCnErrorDetailsMPPSwitchEnabledCNCInternationalMerchantTest() throws Exception {
    bulkProcess.setInternationalMerchant(true);
    when(helper.generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
        anyString(), eq(true))).thenReturn(workbook);
    CategoryDetailResponse categoryDetailResponse = categoryDetailResponseBuilder();
    when(pcbOutboundService.getCategoryDetailResponse(any())).thenReturn(categoryDetailResponse);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(true));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void createFileAndSendNotificationCnErrorDetailsFewCasesTest() throws Exception {
    when(helper.generateDataSheetWithHSSFWorkBook(listOfListOfStringsArgumentCaptor.capture(),
        Mockito.<List<List<String>>>any(), anyString(), eq(true))).thenReturn(workbook);
    CategoryDetailResponse categoryDetailResponse = categoryDetailResponseBuilder2();
    when(pcbOutboundService.getCategoryDetailResponse(any())).thenReturn(categoryDetailResponse);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper).generateDataSheetWithHSSFWorkBook(listOfListOfStringsArgumentCaptor.capture(),
        Mockito.<List<List<String>>>any(), anyString(), eq(true));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
    Assertions.assertTrue(listOfListOfStringsArgumentCaptor.getAllValues().get(0).get(0)
        .contains(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID));
  }

  @Test
  public void createFileAndSendNotificationCnErrorDetailsFewCasesInstoreTest() throws Exception {
    when(helper.generateDataSheetWithHSSFWorkBook(listOfListOfStringsArgumentCaptor.capture(),
        Mockito.<List<List<String>>>any(), anyString(), eq(true))).thenReturn(workbook);
    CategoryDetailResponse categoryDetailResponse = categoryDetailResponseBuilder2();
    when(pcbOutboundService.getCategoryDetailResponse(any())).thenReturn(categoryDetailResponse);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", true);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper).generateDataSheetWithHSSFWorkBook(listOfListOfStringsArgumentCaptor.capture(),
        Mockito.<List<List<String>>>any(), anyString(), eq(true));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
    Assertions.assertTrue(listOfListOfStringsArgumentCaptor.getAllValues().get(0).get(0)
        .contains(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID));
  }

  @Test
  public void createFileAndSendNotificationCnErrorDetailsInternationalMerchantFewCasesTest() throws Exception {
    when(helper.generateDataSheetWithHSSFWorkBook(listOfListOfStringsArgumentCaptor.capture(),
        Mockito.<List<List<String>>>any(), anyString(), eq(true))).thenReturn(workbook);
    CategoryDetailResponse categoryDetailResponse = categoryDetailResponseBuilder2();
    when(pcbOutboundService.getCategoryDetailResponse(any())).thenReturn(categoryDetailResponse);
    bulkProcess.setInternationalMerchant(true);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper).generateDataSheetWithHSSFWorkBook(listOfListOfStringsArgumentCaptor.capture(),
        Mockito.<List<List<String>>>any(), anyString(), eq(true));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
    Assertions.assertTrue(listOfListOfStringsArgumentCaptor.getAllValues().get(0).get(0)
        .contains(BulkCnCreationHeaderNames.VARIANT_IMAGE_EN));
  }
  @Test
  public void createFileAndSendNotificationCnErrorDetailsInternationalMerchantFewCasesInstoreTest() throws Exception {
    when(helper.generateDataSheetWithHSSFWorkBook(listOfListOfStringsArgumentCaptor.capture(),
        Mockito.<List<List<String>>>any(), anyString(), eq(true))).thenReturn(workbook);
    CategoryDetailResponse categoryDetailResponse = categoryDetailResponseBuilder2();
    when(pcbOutboundService.getCategoryDetailResponse(any())).thenReturn(categoryDetailResponse);
    bulkProcess.setInternationalMerchant(true);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", true);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper).generateDataSheetWithHSSFWorkBook(listOfListOfStringsArgumentCaptor.capture(),
        Mockito.<List<List<String>>>any(), anyString(), eq(true));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
    Assertions.assertTrue(listOfListOfStringsArgumentCaptor.getAllValues().get(0).get(0)
        .contains(BulkCnCreationHeaderNames.VARIANT_IMAGE_EN));
  }


  @Test
  public void createFileAndSendNotificationCnErrorDetailsInternationalMerchantTest() throws Exception {
    when(helper.generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
        anyString(), eq(true))).thenReturn(workbook);
    CategoryDetailResponse categoryDetailResponse = categoryDetailResponseBuilder();
    when(pcbOutboundService.getCategoryDetailResponse(any())).thenReturn(categoryDetailResponse);
    bulkProcess.setInternationalMerchant(true);
    service.createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
        MerchantStatusType.PURE_DELIVERY, "", false);
    verify(bulkDownloadAuditRepository).save(any(BulkDownloadEntity.class));
    verify(helper)
        .generateDataSheetWithHSSFWorkBook(Mockito.<List<List<String>>>any(), Mockito.<List<List<String>>>any(),
            anyString(), eq(true));
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(),
      Mockito.anyString());
    verify(fileStorageService).getFailedProductDownloadLink(Mockito.anyString());
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), Mockito.anyString());
  }

  @Test
  public void createFileAndSendNotificationCnErrorDetailsMPPSwitchEnabledExceptionTest()
    throws Exception {
    CategoryDetailResponse categoryDetailResponse = categoryDetailResponseBuilder();
    when(pcbOutboundService.getCategoryDetailResponse(anyString()))
      .thenReturn(categoryDetailResponse);
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.pcbOutboundService)
      .getCategoryDetailResponse(any());
    service
      .createFileAndSendNotification(bulkProcess, getInputRowData("generateInputRowData"),
          MerchantStatusType.PURE_DELIVERY, "", false);
    verify(pcbOutboundService).getCategoryDetailResponse(any());
  }

  @Test
  public void testCreateErrorFileForConvertedUploadProcess() throws Exception {
    bulkProcess.setTotalCount(1);
    ReflectionTestUtils.setField(service, "staticBaseUrl", "baseUrl");
    List<BulkProcessData> failedData = new ArrayList<>();
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setRowNumber(1);
    bulkProcessData.setErrorMessage("Test error message");
    bulkProcessData.setBulkRequestData(
      "{\"Kategori*\":\"Test Category\",\"C2\":\"Test C2\",\"CN\":\"Test CN\"}");
    failedData.add(bulkProcessData);

    UnifiedBulkDownloadDTO unifiedBulkDownloadDTO = new UnifiedBulkDownloadDTO();
    unifiedBulkDownloadDTO.setDestinationFileByteFile("test".getBytes());

    when(bulkDownloadService.downloadProductUnifiedTemplate(anyString(), anyString(), anyString(),
      any())).thenReturn(unifiedBulkDownloadDTO);

    try (MockedStatic<POIUtil> mockedPOIUtil = Mockito.mockStatic(POIUtil.class)) {
      byte[] mockProcessedBytes = "processed".getBytes();
      mockedPOIUtil.when(
        () -> POIUtil.processExcelFileWithErrors(Mockito.any(byte[].class), Mockito.anyList(),
          Mockito.anyString(), Mockito.any(), Mockito.anyBoolean())).thenReturn(mockProcessedBytes);

      service.createErrorFileForConvertedUploadProcess(5, bulkProcess, failedData,
        Constant.TD_MERCHANT);

      verify(bulkDownloadService).downloadProductUnifiedTemplate(anyString(), anyString(),
        anyString(), any());
      verify(fileStorageService).uploadFileToBulkBucket(any(), any());
      verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), anyString());
    }
  }

  @Test
  public void testCreateErrorFileForConvertedUploadProcessWithMultipleFailedData()
    throws Exception {
    ReflectionTestUtils.setField(service, "staticBaseUrl", "baseUrl");
    List<BulkProcessData> failedData = new ArrayList<>();

    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setRowNumber(1);
    bulkProcessData1.setErrorMessage("Error message 1");
    bulkProcessData1.setBulkRequestData("{\"Kategori*\":\"Category 1\"}");
    failedData.add(bulkProcessData1);

    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setRowNumber(2);
    bulkProcessData2.setErrorMessage("Error message 2");
    bulkProcessData2.setBulkRequestData("{\"Kategori*\":\"Category 2\"}");
    failedData.add(bulkProcessData2);

    UnifiedBulkDownloadDTO unifiedBulkDownloadDTO = new UnifiedBulkDownloadDTO();
    unifiedBulkDownloadDTO.setDestinationFileByteFile("test".getBytes());

    when(bulkDownloadService.downloadProductUnifiedTemplate(anyString(), anyString(), anyString(),
      any())).thenReturn(unifiedBulkDownloadDTO);

    try (MockedStatic<POIUtil> mockedPOIUtil = Mockito.mockStatic(POIUtil.class)) {
      byte[] mockProcessedBytes = "processed".getBytes();
      mockedPOIUtil.when(
          () -> POIUtil.processExcelFileWithErrors(any(byte[].class), Mockito.anyList(),
            anyString(), Mockito.any(), Mockito.anyBoolean()))
        .thenReturn(mockProcessedBytes);
      bulkProcess.setTotalCount(100);

      service.createErrorFileForConvertedUploadProcess(5, bulkProcess, failedData,
        Constant.TD_MERCHANT);

      verify(bulkDownloadService).downloadProductUnifiedTemplate(anyString(), anyString(),
        anyString(), any());
      verify(fileStorageService).uploadFileToBulkBucket(anyString(), any());
      verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
        eq(NotificationType.BULK_UPLOADED.getValue()), anyString());
    }
  }

  @Test
  public void testCreateErrorFileForConvertedUploadProcessWithNullBulkRequestData() {
    bulkProcess.setTotalCount(1);
    service.createErrorFileForConvertedUploadProcess(5, bulkProcess, new ArrayList<>(),
      Constant.TD_MERCHANT);
    verify(notificationService).sendBulkUploadedNotification(eq(bulkProcess),
      eq(NotificationType.BULK_UPLOADED.getValue()), eq(StringUtils.EMPTY));
    verifyNoMoreInteractions(bulkDownloadService);
    verifyNoMoreInteractions(fileStorageService);
  }

  @Test
  public void testCreateErrorFileForConvertedUploadProcessWithException() throws Exception {
    bulkProcess.setTotalCount(1);
    List<BulkProcessData> failedData = new ArrayList<>();
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setRowNumber(1);
    bulkProcessData.setErrorMessage("Test error message");
    bulkProcessData.setBulkRequestData("{\"Kategori*\":\"Test Category\"}");
    failedData.add(bulkProcessData);

    UnifiedBulkDownloadDTO unifiedBulkDownloadDTO = new UnifiedBulkDownloadDTO();
    unifiedBulkDownloadDTO.setDestinationFileByteFile("test".getBytes());

    Mockito.doThrow(ApplicationRuntimeException.class).when(this.bulkDownloadService)
      .downloadProductUnifiedTemplate(anyString(), anyString(), anyString(), any());


    service.createErrorFileForConvertedUploadProcess(5, bulkProcess, failedData, Constant.TD_MERCHANT);

    verify(bulkDownloadService).downloadProductUnifiedTemplate(anyString(), anyString(),
      anyString(), any());
    verifyNoMoreInteractions(fileStorageService);
    verifyNoMoreInteractions(notificationService);
  }
}
