package com.gdn.mta.bulk.service;


import static com.gdn.mta.bulk.service.FileStorageServiceBean.START_DATE_FORMAT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.awt.image.BufferedImage;
import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.zip.ZipInputStream;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.util.IOUtils;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.apache.xmlbeans.XmlException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.config.QRCodeProperties;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.dto.BulkInternalProcessDTO;
import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.GenericTemplateFileType;
import com.gdn.mta.bulk.dto.QrCodeRowInfo;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.factory.BulkProcessHelperFactory;
import com.gdn.mta.bulk.helper.BulkInternalFailedProductsProcessHelper;
import com.gdn.mta.bulk.helper.BulkOrderProcessHelper;
import com.gdn.mta.bulk.models.AuditTrailInfo;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.FileType;
import com.gdn.mta.bulk.models.UploadImageRequest;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.InternalProcessFailedProductsDownloadRequest;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.NotificationTypeConstant;
import com.google.cloud.PageImpl;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.springframework.web.multipart.MultipartFile;

public class FileStorageServiceBeanTest {
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_REQUEST_ID = "123";
  private static final String fileName1 = "file-name";
  private static final String FILE_PATH = "file-path";
  private static final String FILE_NAME = "file-name";
  private static final String PRODUCT_CODE = "productCode";
  private static final String DEFAULT_BULK_PROCESS_CODE = UUID.randomUUID().toString();
  private static final String BULK_ARCHIVE = "BulkArchive.xlsx";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String BULK_PROCESS_TYPE = "bulkProcessType";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String CREATED_BY = "createdBy";
  private static final String UPDATED_BY = "updatedBy";
  private static final String PRODUCT_LEVEL3_PROCESSOR_DIR = "ProductLevel3Processor";
  private static final String TEMPLATE_FILE = "general_template.xlsx";
  private static final String DEFAULT_EXCEPTION_MESSAGE = "EXCEPTION MESSAGE";
    private static final String URL_IMAGE = "https://i.imgur.com/xCx7lQG.png";
  private static final String URL_IMAGE_INVALID_EXTENSION = "https://i.imgur.com/qR77BXg.gif";
  private static final ClassLoader CLASS_LOADER = ClassLoader.getSystemClassLoader();

  private static final String BASE_DIRECTORY = CLASS_LOADER.getResource(StringUtils.EMPTY).getPath();
  private static final String GENERIC_TEMPLATE_1_PATH =
    BASE_DIRECTORY + "/ExcelTemplate//Blibli-mass-upload-template-1.xlsm";
  private static final String PRODUCT_CODE_FILE_NAME = "productCode/fileName.jpeg";
  private static final String BLIBLI_MASS_TEMPLATE_LOCATION = "blibliMassTemplateLocation";
  private static final String BLIBLI_UNIFIED_TEMPLATE = "Blibli-mass-base-template.xlsx";
  private static final String BLIBLI_UNIFIED_UPLOAD_TEMPLATE = "Blibli-mass-upload-template.xlsx";
  private static final String BUCKET_NAME = "bucket";
  private static final String RESTRICTED_KEYWORD_UPDATE_TEMPLATE_PATH =
      "src/test/resources/RestrictedKeywordUpdate/restricted-keyword-bulk-add.xlsx";
  private static final String RESTRICTED_KEYWORD_UPDATE_TEMPLATE_ERROR_PATH =
      "src/test/resources/RestrictedKeywordUpdate/error/restricted-keyword-bulk-add.xlsx";
  private static final String QR_TEMPLATE_PATH = "/qrTemplates/";
  private static final String GCS_BUCKET = "bucket-test";
  private static final String REQUEST_ID = UUID.randomUUID().toString();
  private BulkProcess bulkProcess;

  @InjectMocks
  private FileStorageServiceBean fileStorageServiceBean;

  @Mock
  private GCSService gcsService;

  @Mock
  private Bucket bulkBucket;

  @Mock
  private Bucket pricingBulkBucket;

  @Mock
  private Bucket finalImageBucket;

  @Mock
  private SystemParameter systemParameter;

  @Mock
  private Blob blob;

  @Mock
  private BulkProcessHelperFactory bulkProcessHelperFactory;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private QRCodeProperties qrCodeProperties;

  private BulkUpdateProcessDTO bulkUpdateProcessDTO;

  private BulkUpdateQueue bulkUpdateQueue;

  private BulkInternalUploadRequestDTO bulkInternalUploadRequestDTO;

  private BulkDownloadRequest bulkDownloadRequest;

  @BeforeEach
  public void init() throws UnsupportedEncodingException {
    MockitoAnnotations.initMocks(this);
    bulkUpdateProcessDTO = new BulkUpdateProcessDTO();
    bulkUpdateProcessDTO.setBulkProcessType("Update");
    bulkUpdateProcessDTO.setBusinessPartnerCode("TOA-14961");
    bulkUpdateProcessDTO.setFileName("test.xlsx");
    bulkUpdateProcessDTO.setUpdatedBy("developer");
    bulkUpdateProcessDTO.setClientHost("localhost");
    bulkUpdateProcessDTO.setFileContent("Hello".getBytes(StandardCharsets.UTF_8));
    bulkUpdateProcessDTO.setPrivilegedMap(new HashMap<String, Boolean>());
    bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(UUID.randomUUID().toString());
    bulkUpdateQueue.setBusinessPartnerCode("TOA-14961");
    bulkUpdateQueue.setBulkProcessType("Update");
    bulkUpdateQueue.setClientHost("localhost");
    bulkUpdateQueue.setFileName("test.xlsx");
    bulkUpdateQueue.setPrivilegedMap(new HashMap<String, Boolean>());
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setUpdatedBy("developer");

    bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_GENERIC.name());
    bulkProcess.setStoreId(DEFAULT_STORE_ID);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedBy(CREATED_BY);
    bulkProcess.setRequestId(DEFAULT_REQUEST_ID);
    bulkProcess.setUpdatedBy(UPDATED_BY);

    bulkInternalUploadRequestDTO = new BulkInternalUploadRequestDTO();
    bulkInternalUploadRequestDTO.setFileName(BULK_ARCHIVE);
    bulkInternalUploadRequestDTO.setInternalProcessRequestCode(BULK_PROCESS_CODE);

    bulkDownloadRequest = new BulkDownloadRequest.BulkRequestBuilder()
        .fileType(FileType.XLSX).build();
    bulkDownloadRequest.setDirectDownload(true);
    bulkDownloadRequest.setExceptionMsg(DEFAULT_EXCEPTION_MESSAGE);
    bulkDownloadRequest.setRequestId(DEFAULT_REQUEST_ID);
    bulkDownloadRequest.setFilename(FILE_NAME);

    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBulkBucketName", BUCKET_NAME);
    ReflectionTestUtils.setField(fileStorageServiceBean, "allowedImageTypes", "image/jpeg,"
      + "image/png,image/jpg");
    ReflectionTestUtils.setField(fileStorageServiceBean, "finalImageDirectory", "finalDir");
    ReflectionTestUtils.setField(fileStorageServiceBean, "finalMediumImageDirectory", "mediumDir");
    ReflectionTestUtils.setField(fileStorageServiceBean, "finalThumbnailImageDirectory", "thumbDir");
    ReflectionTestUtils.setField(fileStorageServiceBean, "pathPrefix", "prefix");
  }

  @AfterEach
  public void finalizeTest() throws IOException {
    Files.deleteIfExists(
      Paths.get(BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE));
    Files.deleteIfExists(Paths.get(BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + TEMPLATE_FILE));
  }

  private Map<String, String> getFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("BulkUpdate" + File.separator + "BulkUpdate.xlsx"))), StandardCharsets.UTF_8);
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getFile(String fileName) throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(
        Thread.currentThread().getContextClassLoader().getResourceAsStream("BulkUpdate" + File.separator + fileName))),
        StandardCharsets.UTF_8);
    files.put("xlsx", excelData);
    return files;
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

  @Test
  public void createBulkFileGCSEnabled() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkUpdateProcessDTO.setBulkProcessType("ProductCreationUpload");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.createBulkFile(bulkUpdateProcessDTO,
      bulkUpdateQueue.getBulkProcessCode(), fileName1);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void createBulkFileCampaignErrorFileGCSEnabled() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkUpdateProcessDTO.setBulkProcessType("CampaignError");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.createBulkFile(bulkUpdateProcessDTO,
      bulkUpdateQueue.getBulkProcessCode(), fileName1);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void createBulkFile() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    Map<String, String> files = this.getFiles();
    bulkUpdateProcessDTO.setBulkProcessType("ProductLevel3");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkUpdateQueue.getBulkProcessCode() + File.separator
        + bulkUpdateQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    fileStorageServiceBean.createBulkFile(bulkUpdateProcessDTO,
        bulkUpdateQueue.getBulkProcessCode(), fileName1);
  }

  @Test
  public void getFileDataGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType("Archive");
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    fileStorageServiceBean.getFileData(bulkUpdateQueue, bulkProcess);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test
  public void getFileDataByFileNameTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType("Archive");
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    fileStorageServiceBean.getFileDataByFileName(bulkUpdateQueue.getFileName(), bulkProcess);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test
  public void getFileDataByFileName_bulkProcessNullTest() throws Exception {
    Assertions.assertThrows(RuntimeException.class,
        () -> fileStorageServiceBean.getFileDataByFileName(bulkUpdateQueue.getFileName(), null));
  }

  @Test
  public void getFileDataTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    BulkProcess bulkProcess = this.getBulkProcess();
    bulkProcess.setBulkProcessType("Archive");
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkProcess.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkProcess.getBulkProcessCode() + File.separator + bulkProcess.getBulkProcessCode()
        + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    fileStorageServiceBean.getFileData(bulkUpdateQueue, bulkProcess);
  }

  @Test
  @Disabled
  public void getFileDataNullBulkProcessTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    BulkProcess bulkProcess = null;
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_UPDATE_DIR + bulkProcess.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_UPDATE_DIR + bulkProcess.getBulkProcessCode() + File.separator + bulkProcess.getBulkProcessCode()
        + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Assertions.assertThrows(RuntimeException.class,
        () -> fileStorageServiceBean.getFileData(bulkUpdateQueue, bulkProcess));
  }

  @Test
  public void getFileDataGCSEnabledNullBulkProcessTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    BulkProcess bulkProcess = null;
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Assertions.assertThrows(RuntimeException.class,
        () -> fileStorageServiceBean.getFileData(bulkUpdateQueue, bulkProcess));
  }

  @Test
  public void downloadFileFromGcsTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkProcess.setBulkProcessType("ProductCreationUpload");
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn("Hello".getBytes(
        StandardCharsets.UTF_8));
    byte[] data = fileStorageServiceBean.downloadFile(bulkProcess,"xlsx");
    Assertions.assertNotNull(data);
  }

  @Test
  public void downloadFileFromGcsArchivalTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkProcess.setBulkProcessType("Archive");
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn("Hello".getBytes(
        StandardCharsets.UTF_8));
    byte[] data = fileStorageServiceBean.downloadFile(bulkProcess,"xlsx");
    Assertions.assertNotNull(data);
  }

  @Test
  public void downloadFileCampaignGCSTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkProcess.setBulkProcessType("Campaign");
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn("Hello".getBytes(
        StandardCharsets.UTF_8));
    byte[] data = fileStorageServiceBean.downloadFile(bulkProcess,"xlsx");
    Assertions.assertNotNull(data);
  }

  @Test
  public void uploadUpsertFileGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkUpdateProcessDTO.setBulkProcessType("InstantPickupProductUpsert");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean
      .createBulkFile(bulkUpdateProcessDTO, bulkUpdateQueue.getBulkProcessCode(), fileName1);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void productSuspensionFileGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkUpdateProcessDTO.setBulkProcessType("suspension");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean
        .createBulkFile(bulkUpdateProcessDTO, bulkUpdateQueue.getBulkProcessCode(), fileName1);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void internalFileGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkUpdateProcessDTO.setBulkProcessType("internal");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean
        .createBulkFile(bulkUpdateProcessDTO, bulkUpdateQueue.getBulkProcessCode(), fileName1);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void configGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkUpdateProcessDTO.setBulkProcessType("config");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean
        .createBulkFile(bulkUpdateProcessDTO, bulkUpdateQueue.getBulkProcessCode(), fileName1);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }


  @Test
  public void uploadUpsertFileTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    bulkUpdateProcessDTO.setBulkProcessType("InstantPickupProductUpsert");
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(
      ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
      ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + bulkUpdateQueue.getBulkProcessCode()
        + File.separator + bulkUpdateQueue.getBulkProcessCode()
        + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    fileStorageServiceBean
      .createBulkFile(bulkUpdateProcessDTO, bulkUpdateQueue.getBulkProcessCode(), fileName1);
    File fileCreated = new File(
      ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + bulkUpdateQueue.getBulkProcessCode()
        + File.separator + bulkUpdateQueue.getBulkProcessCode()
        + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    Assertions.assertNotNull(fileCreated);
  }

  @Test
  public void uploadDeleteUpsertFileGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkUpdateProcessDTO.setBulkProcessType("InstantPickupProductDelete");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean
      .createBulkFile(bulkUpdateProcessDTO, bulkUpdateQueue.getBulkProcessCode(), fileName1);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void uploadDeleteUpsertFileTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    bulkUpdateProcessDTO.setBulkProcessType("InstantPickupProductDelete");
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(
      ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + bulkUpdateQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
      ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + bulkUpdateQueue.getBulkProcessCode()
        + File.separator + bulkUpdateQueue.getBulkProcessCode()
        + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    fileStorageServiceBean
      .createBulkFile(bulkUpdateProcessDTO, bulkUpdateQueue.getBulkProcessCode(), fileName1);
    File fileCreated = new File(
      ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + bulkUpdateQueue.getBulkProcessCode()
        + File.separator + bulkUpdateQueue.getBulkProcessCode()
        + ProcessorUtils.FILETYPE_XLSX_EXCEL);
    Assertions.assertNotNull(fileCreated);
  }

  @Test
  public void uploadSubjectToVatCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkUpdateProcessDTO.setBulkProcessType("SubjectToVat");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean
      .createBulkFile(bulkUpdateProcessDTO, bulkUpdateQueue.getBulkProcessCode(), fileName1);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void uploadSubjectToVatErrorTest() throws Exception {
    bulkUpdateProcessDTO.setBulkProcessType("SubjectToVatError");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean
            .createBulkFile(bulkUpdateProcessDTO, bulkUpdateQueue.getBulkProcessCode(), fileName1);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void uploadProductLevel3GCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkUpdateProcessDTO.setBulkProcessType("ProductLevel3");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean
      .createBulkFile(bulkUpdateProcessDTO, bulkUpdateQueue.getBulkProcessCode(), fileName1);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void uploadDefaultCaseTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkUpdateProcessDTO.setBulkProcessType("Recategorization");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean
      .createBulkFile(bulkUpdateProcessDTO, bulkUpdateQueue.getBulkProcessCode(), fileName1);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  private void mockFile(String filePath) throws Exception {
    Map<String, String> files = this.getFilesForCateogry();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, excelFile);
  }


  private Map<String, String> getFilesForCateogry() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(org.apache.commons.io.IOUtils.toByteArray(
      Thread.currentThread().getContextClassLoader().getResourceAsStream(
        PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "Android_template_new_cn.xlsm"))), StandardCharsets.UTF_8);
    files.put("xlsx", excelData);
    return files;
  }

  @Test
  public void getFilePrefixMasterProductGCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsMasterProductDownloadPath", FILE_PATH);
    String result = fileStorageServiceBean.getFilePrefix(BulkProcessType.MASTER_PRODUCT.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixUncategorisedSkuGCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsUncategorisedSkuPath", FILE_PATH);
    String result = fileStorageServiceBean.getFilePrefix(BulkProcessType.UNCATEGORISED_SKU.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixMasterSkuAnchorsDownloadTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsMasterSkuAnchorsDownloadPath",
      FILE_PATH);
    String result = fileStorageServiceBean.getFilePrefix(
      BulkProcessType.MASTER_SKU_IN_REVIEW_DOWNLOAD.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixMasterSkuItemsDownloadTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsMasterSkuItemsDownloadPath",
      FILE_PATH);
    String result = fileStorageServiceBean.getFilePrefix(
      BulkProcessType.MASTER_SKU_REVIEW_ITEMS_DOWNLOAD.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixAutoApprovedProductsTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsAutoApprovedProductsDownloadPath",
        FILE_PATH);
    String result = fileStorageServiceBean.getFilePrefix(
        BulkProcessType.AUTO_APPROVED_PRODUCTS_DOWNLOAD.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixIprProductsTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsIprProductsDownloadPath",
        FILE_PATH);
    String result = fileStorageServiceBean.getFilePrefix(
        BulkProcessType.IPR_PRODUCTS_DOWNLOAD_ALL.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixCopyProductGCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsStoreCopyDownloadPath", FILE_PATH);
    String result = fileStorageServiceBean.getFilePrefix(BulkProcessType.STORE_COPY.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixSuspensionGCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsSuspensionProductDownloadPath", FILE_PATH);
    String result = fileStorageServiceBean.getFilePrefix(BulkProcessType.PRODUCT_SUSPENSION.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixMasterProductGCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    String result = fileStorageServiceBean.getFilePrefix(BulkProcessType.MASTER_PRODUCT.getValue());
    Assertions.assertEquals(result, EmailConstants.MASTER_PRODUCT_DOWNLOAD_FILE_PREFIX);
  }

  @Test
  public void getFilePrefixUncategorisedSkuGCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    String result = fileStorageServiceBean.getFilePrefix(BulkProcessType.UNCATEGORISED_SKU.getValue());
    Assertions.assertEquals(result, EmailConstants.UNCATEGORISED_SKU_DOWNLOAD_FILE_PREFIX);
  }

  @Test
  public void getFilePrefixCopyProductGCSTrueFalse() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    String result = fileStorageServiceBean.getFilePrefix(BulkProcessType.STORE_COPY.getValue());
    Assertions.assertEquals(result, EmailConstants.STORE_COPY_FILE_PREFIX);
  }

  @Test
  public void getFilePrefixSuspensionGCSFalseest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    String result = fileStorageServiceBean.getFilePrefix(BulkProcessType.PRODUCT_SUSPENSION.getValue());
    Assertions.assertEquals(result, EmailConstants.BULK_PRODUCT_SUSPENSION_FILE_PREFIX);
  }

  @Test
  public void filePreFixDefaultCaseTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    String result = fileStorageServiceBean.getFilePrefix(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Assertions.assertEquals(result, StringUtils.EMPTY);
  }

  @Test
  public void uploadInstoreGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkUpdateProcessDTO.setBulkProcessType("Instore");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean
      .createBulkFile(bulkUpdateProcessDTO, bulkUpdateQueue.getBulkProcessCode(), fileName1);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void uploadMppFailedGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkUpdateProcessDTO.setBulkProcessType("FailedUpsertMppProduct");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean
      .createBulkFile(bulkUpdateProcessDTO, bulkUpdateQueue.getBulkProcessCode(), fileName1);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void createBulkInternalGCSEnabledTest() throws Exception {
    InternalProcessFailedProductsDownloadRequest request = new InternalProcessFailedProductsDownloadRequest();
    BulkInternalProcessDTO bulkInternalProcessDTO = new BulkInternalProcessDTO();
    request.setRequestId(DEFAULT_REQUEST_ID);
    bulkInternalProcessDTO.setDirectoryPath(ProcessorUtils.DATA_BASE_DIR);
    bulkInternalProcessDTO.setFilepath(ProcessorUtils.DATA_BASE_DIR + FILE_PATH);
    bulkInternalProcessDTO.setFinalWorkbook(new XSSFWorkbook());
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    Mockito.when(bulkProcessHelperFactory.getHelper(request)).thenReturn(new BulkInternalFailedProductsProcessHelper());
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean
        .createBulkInternalFile(request, bulkInternalProcessDTO, "process-type");
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void createBulkInternalTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    BulkDownloadRequest request = new BulkDownloadRequest();
    BulkInternalProcessDTO bulkInternalProcessDTO = new BulkInternalProcessDTO();
    request.setRequestId(DEFAULT_REQUEST_ID);
    bulkInternalProcessDTO.setFilepath(ProcessorUtils.DATA_BASE_DIR + FILE_PATH);
    bulkInternalProcessDTO.setDirectoryPath(ProcessorUtils.DATA_BASE_DIR);
    bulkInternalProcessDTO.setFinalWorkbook(new XSSFWorkbook());
    fileStorageServiceBean
        .createBulkInternalFile(request, bulkInternalProcessDTO, "process-type");
  }

  @Test
  public void getDownloadLinkTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    String link = fileStorageServiceBean
      .getDownloadLink(FILE_PATH, "FailedUpsertMppProduct", BULK_PROCESS_CODE, FILE_NAME);
    Assertions.assertNotNull(link);
  }

  @Test
  public void getDownloadLinkGCSTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    String link = fileStorageServiceBean
      .getDownloadLink(FILE_PATH, "FailedUpsertMppProduct", BULK_PROCESS_CODE, FILE_NAME);
    Assertions.assertNotNull(link);
  }

  @Test
  public void getMtaDownloadPathTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    String filePath = fileStorageServiceBean.getFailedProductDownloadLink(BULK_PROCESS_CODE);
    Assertions.assertEquals("static/failed-products/bulkProcessCode/bulkProcessCode.xls", filePath);
  }

  @Test
  public void getMtaDownloadPathGCSEnabledTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsDownloadFailedProductsPath",
      "failedProducts/");
    String filePath = fileStorageServiceBean.getFailedProductDownloadLink(BULK_PROCESS_CODE);
    Assertions.assertEquals("static/failedProducts/bulkProcessCode/bulkProcessCode.xls", filePath);
  }

  @Test
  public void getFailedProductGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkUpdateProcessDTO.setBulkProcessType("ProductCreationFailedDir");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean
      .createBulkFile(bulkUpdateProcessDTO, bulkUpdateQueue.getBulkProcessCode(), fileName1);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void getFileDataWithInternalUploadRequestWithGcsTest() throws Exception{
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.DELETE_BRAND_AUTHORISATION);
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString()))
        .thenReturn(excelFile);
    try {
      Assertions.assertThrows(Exception.class,
          () -> fileStorageServiceBean.getFileDataWithInternalUploadRequest(
              bulkInternalUploadRequestDTO));
    } finally {
      Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
    }
  }

  @Test()
  public void getFileDataWithInternalUploadRequestWithGcs_Test() throws Exception{
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.SUSPEND);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Sheet data =
      fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test
  public void getFileDataWithInternalUploadRequestBulkRebateTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.BULK_PRICE_REBATE);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Sheet data =
        fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test()
  public void getFileDataWithInternalUploadRequestWithGcs_recatTest() throws Exception{
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Sheet data =
      fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test
  public void getFileDataWithInternalUploadRequestBulkProductTypeTaggingTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Sheet data =
      fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test
  public void getFileDataWithInternalUploadRequestBulkSkuLevelRebateTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsPricingBucketName", BUCKET_NAME);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.BULK_SKU_LEVEL_REBATE);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test()
  public void getFileDataWithInternalUploadRequestWithGcsBulkPriceUpdateTest() throws Exception{
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Sheet data =
        fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test()
  public void getFileDataWithInternalUploadRequestWithGcs_VendorBulkTest() throws Exception{
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Sheet data =
      fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test()
  public void getFileDataWithInternalUploadRequestWithGcs_RESTRICTED_KEYWORD_UPSERT_Test() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Sheet data = fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test()
  public void getFileDataWithInternalUploadRequestWithGcs_BrandAuthAdd_Test() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.BRAND_AUTH_ADD);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Sheet data = fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }
  @Test()
  public void getFileDataWithInternalUploadRequestWithGcs_BrandAuthDelete_Test() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Sheet data = fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test()
  public void getFileDataWithInternalUploadRequestWithGcs_RESTRICTED_KEYWORD_DELETE_Test() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Sheet data = fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }


  @Test
  public void getFileDataWithInternalUploadRequestWithGcs_internalUpload() throws Exception{
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.INTERNAL_BULK_UPLOAD);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Sheet data =
      fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test()
  public void getFileDataWithInternalUploadRequestWithGcs_SalesCategoryTest() throws Exception{
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Sheet data =
      fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test()
  public void getFileDataWithInternalUploadRequestWithGcs_StoreCopyTest() throws Exception{
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.STORE_COPY);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Sheet data =
      fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test()
  public void getFileDataWithInternalUploadRequestWithGcs_configurationTest() throws Exception{
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.CONFIGURATION);
    bulkInternalUploadRequestDTO.setRelativePath(FILE_PATH.concat(".xlsx"));
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    Sheet data =
      fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString());
  }

  @Test
  public void getFileDataWithInternalUpload_FileStore() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.CONFIGURATION);
    bulkInternalUploadRequestDTO.setFileName(BULK_ARCHIVE);
    bulkInternalUploadRequestDTO.setRelativePath(ProcessorUtils.BULK_CONFIGURATION_DIR
      + bulkInternalUploadRequestDTO.getInternalProcessRequestCode() + Constant.SLASH
      + BULK_ARCHIVE);
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_CONFIGURATION_DIR + bulkInternalUploadRequestDTO.getInternalProcessRequestCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_CONFIGURATION_DIR + bulkInternalUploadRequestDTO.getInternalProcessRequestCode() + File.separator
      + bulkInternalUploadRequestDTO.getFileName(), excelFile);
    fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
  }

  @Test
  public void getFileDataWithInternalUpload_recatFileStore() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION);
    bulkInternalUploadRequestDTO.setFileName(BULK_ARCHIVE);
    bulkInternalUploadRequestDTO.setRelativePath(ProcessorUtils.BULK_RECAT_DIR
      + bulkInternalUploadRequestDTO.getInternalProcessRequestCode() + Constant.SLASH
      + BULK_ARCHIVE);
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_RECAT_DIR + bulkInternalUploadRequestDTO.getInternalProcessRequestCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_RECAT_DIR + bulkInternalUploadRequestDTO.getInternalProcessRequestCode() + File.separator
      + bulkInternalUploadRequestDTO.getFileName(), excelFile);
    fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
  }

  @Test
  public void getFileDataWithInternalUpload_storeCopyFileStore() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.STORE_COPY);
    bulkInternalUploadRequestDTO.setFileName(BULK_ARCHIVE);
    bulkInternalUploadRequestDTO.setRelativePath(ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS
      + bulkInternalUploadRequestDTO.getInternalProcessRequestCode() + Constant.SLASH
      + BULK_ARCHIVE);
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS + bulkInternalUploadRequestDTO.getInternalProcessRequestCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS + bulkInternalUploadRequestDTO.getInternalProcessRequestCode() + File.separator
      + bulkInternalUploadRequestDTO.getFileName(), excelFile);
    fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
  }

  @Test
  public void getFileDataWithInternalUpload_SalesCategoryTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE);
    bulkInternalUploadRequestDTO.setFileName(BULK_ARCHIVE);
    bulkInternalUploadRequestDTO.setRelativePath(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR
      + bulkInternalUploadRequestDTO.getInternalProcessRequestCode() + Constant.SLASH
      + BULK_ARCHIVE);
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR + bulkInternalUploadRequestDTO.getInternalProcessRequestCode());
    ProcessorUtils.createFile(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR + bulkInternalUploadRequestDTO.getInternalProcessRequestCode() + File.separator
      + bulkInternalUploadRequestDTO.getFileName(), excelFile);
    fileStorageServiceBean.getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
  }

  @Test
  public void downloadGenericTemplateFileTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "unifiedTemplateDirectory",
      ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS);
    mockFile(ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS + Constant.SLASH + TEMPLATE_FILE);
    byte[] result = fileStorageServiceBean.downloadGenericTemplateFile(TEMPLATE_FILE);
    Assertions.assertNotNull(result);
  }

  @Test
  public void downloadGenericTemplateFileGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsMassTemplateLocation", FILE_PATH);
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn(excelFile);
    byte[] result = fileStorageServiceBean.downloadGenericTemplateFile(TEMPLATE_FILE);
    Assertions.assertEquals(excelFile,result);
  }

  @Test
  public void createGenericBulkFileTest() throws Exception {
    String expectedFilePath = "target/x-bulk/storeCopyUploads//bpCode/general_template.xlsm";
    ReflectionTestUtils.setField(fileStorageServiceBean, "unifiedTemplateDirectory", ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS);
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String filePath =
      fileStorageServiceBean.createGenericBulkFile(BUSINESS_PARTNER_CODE, false, BulkInternalProcessType.GENERIC_FILE_GENERATION, excelFile);
    Assertions.assertEquals(expectedFilePath, filePath);
  }

  @Test
  public void createGenericBulkFileBulkVersioningTest() throws Exception {
    String expectedFilePath = "target/x-bulk/storeCopyUploads//bpCode/general_template_1.2.xlsm";
    ReflectionTestUtils.setField(fileStorageServiceBean, "unifiedTemplateDirectory",
        ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS);
    ReflectionTestUtils.setField(fileStorageServiceBean, "bulkExcelVersioningEn", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "bulkGenericExcelVersion", "1.1");
    ReflectionTestUtils.setField(fileStorageServiceBean, "bulkGenericInstoreExcelVersion", "1.2");
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String filePath = fileStorageServiceBean.createGenericBulkFile(BUSINESS_PARTNER_CODE, true,
        BulkInternalProcessType.GENERIC_FILE_GENERATION, excelFile);
    Assertions.assertEquals(expectedFilePath, filePath);
  }

  @Test
  public void uploadUnmappedSkuGCSEnabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.uploadUnmappedSku(new XSSFWorkbook(), REQUEST_ID);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void uploadUnmappedSkuTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    fileStorageServiceBean.uploadUnmappedSku(new XSSFWorkbook(), REQUEST_ID);
  }

  @Test
  public void createGenericBulkFileGcsEnabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsMassTemplateLocation", FILE_PATH);
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String filePath =
      fileStorageServiceBean.createGenericBulkFile(BUSINESS_PARTNER_CODE, false, BulkInternalProcessType.GENERIC_FILE_GENERATION, excelFile);
    Assertions.assertEquals(FILE_PATH + Constant.SLASH + BUSINESS_PARTNER_CODE + Constant.SLASH
      + Constant.GENERAL_TEMPLATE, filePath);
  }

  @Test
  public void createGenericBulkFileBundlingTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsMassTemplateLocation", FILE_PATH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsMassBundlingTemplatePath", "bundling");
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String filePath = fileStorageServiceBean.createGenericBulkFile(BUSINESS_PARTNER_CODE, false,
        BulkInternalProcessType.GENERIC_FILE_GENERATION, excelFile);
    Assertions.assertEquals(
        FILE_PATH + Constant.SLASH + BUSINESS_PARTNER_CODE + Constant.SLASH
            + Constant.GENERAL_TEMPLATE, filePath);
  }

  @Test
  public void createGenericBulkFileBundlingBulkVersioningTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsMassTemplateLocation", FILE_PATH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsMassBundlingTemplatePath", "bundling");
    ReflectionTestUtils.setField(fileStorageServiceBean, "bulkGenericExcelVersion", "1.1");
    ReflectionTestUtils.setField(fileStorageServiceBean, "bulkExcelVersioningEn", true);
    Map<String, String> files = this.getFile(BULK_ARCHIVE);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String filePath = fileStorageServiceBean.createGenericBulkFile(BUSINESS_PARTNER_CODE, false,
        BulkInternalProcessType.GENERIC_FILE_GENERATION, excelFile);
    Assertions.assertEquals(
        FILE_PATH + Constant.SLASH + BUSINESS_PARTNER_CODE + Constant.SLASH
            + Constant.NEW_GENERAL_TEMPLATE + Constant.UNDERSCORE + "1.1"
            + Constant.NEW_GENERAL_TEMPLATE_EXTENSION, filePath);
  }

  @Test
  public void generateFileGcsEnableTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.PRODUCT);
    Mockito.when(bulkProcessHelperFactory.getHelper(bulkDownloadRequest)).thenReturn(new BulkOrderProcessHelper());
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq("xbulk/downloadOrders/123/file-name"), Mockito.any())).thenReturn(blob);
    this.fileStorageServiceBean.generateFile(bulkDownloadRequest, excelFile);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq("xbulk/downloadOrders/123/bulk-update-product-template.xlsx"), Mockito.any());
  }

  @Test
  public void generateCampaignFileGcsEnableTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.CAMPAIGN_PRODUCT);
    bulkDownloadRequest.setMerchantId(BUSINESS_PARTNER_CODE);
    Mockito.when(bulkProcessHelperFactory.getHelper(bulkDownloadRequest)).thenReturn(new BulkOrderProcessHelper());
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));

    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq("xbulk/downloadOrders/123/file-name"), Mockito.any())).thenReturn(blob);
    this.fileStorageServiceBean.generateFile(bulkDownloadRequest, excelFile);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq("xbulk/downloadOrders/123/produk-promo-bpCode.xlsx"), Mockito.any());
  }

  @Test
  public void generateCampaignFileGcsEnablePriceRecommendationTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.BULK_PRICE_RECOMMENDATION);
    bulkDownloadRequest.setMerchantId(BUSINESS_PARTNER_CODE);
    Mockito.when(bulkProcessHelperFactory.getHelper(bulkDownloadRequest)).thenReturn(new BulkOrderProcessHelper());
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq("xbulk/downloadOrders/123/file-name"), Mockito.any())).thenReturn(blob);
    this.fileStorageServiceBean.generateFile(bulkDownloadRequest, excelFile);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq("xbulk/downloadOrders/123/bulk-price-information.xlsx"), Mockito.any());
  }

  @Test
  public void generateOrderFileGcsEnableTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.ORDER);
    Mockito.when(bulkProcessHelperFactory.getHelper(bulkDownloadRequest)).thenReturn(new BulkOrderProcessHelper());
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));

    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq("xbulk/downloadOrders/123/bulk-order-download-template.csv"), Mockito.any())).thenReturn(blob);
    this.fileStorageServiceBean.generateFile(bulkDownloadRequest, excelFile);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq("xbulk/downloadOrders/123/bulk-order-download-template.csv"), Mockito.any());
  }

  @Test
  public void generateFileGcsDisableTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    Mockito.when(bulkProcessHelperFactory.getHelper(bulkDownloadRequest)).thenReturn(new BulkOrderProcessHelper());
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    this.fileStorageServiceBean.generateFile(bulkDownloadRequest, excelFile);
  }

  @Test
  public void getEmailPrefixTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    Assertions.assertEquals("xbulk", fileStorageServiceBean.getEmailPrefix());

    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    Assertions.assertEquals("x-bulk", fileStorageServiceBean.getEmailPrefix());
  }

  @Test
  public void getNotificationDetailPathTest() throws Exception {
    Mockito.when(bulkProcessHelperFactory.getHelper(bulkDownloadRequest)).thenReturn(new BulkOrderProcessHelper());
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.MASTER_PRODUCT);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    Assertions.assertEquals("xbulk/downloadOrders/123/file-name", fileStorageServiceBean.getNotificationDetailPath(bulkDownloadRequest));

    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    Assertions.assertEquals(bulkDownloadRequest.getRequestId(),
        fileStorageServiceBean.getNotificationDetailPath(bulkDownloadRequest));
  }

  @Test
  public void getNotificationDetailPathBasicInfoTest() throws Exception {
    Mockito.when(bulkProcessHelperFactory.getHelper(bulkDownloadRequest)).thenReturn(new BulkOrderProcessHelper());
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.PRODUCT_BASIC_INFO);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    Assertions.assertEquals("xbulk/downloadOrders/123/bulk-basic-info-update-template.xlsx",
        fileStorageServiceBean.getNotificationDetailPath(bulkDownloadRequest));
  }

  @Test
  public void uploadImageFilesToSourceLocationGcsEnableTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    Map<String, String> images = ImmutableMap.of("image1", "image1");
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + "/mta/source");
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + "/mta/source/image1", excelFile);
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + "image1")).thenReturn(excelFile);
    fileStorageServiceBean.uploadImageFilesToSourceLocation(images.entrySet().iterator().next(), bulkProcess,
        ProcessorUtils.DATA_BASE_DIR + "/mta/source");
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + "image1");
  }

  @Test
  public void uploadImageFilesToSourceLocationGcsDisableest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    Map<String, String> images = ImmutableMap.of("image1", "image1");
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(
        ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator + ProcessorUtils.DATA_RAW_DIR);
    ProcessorUtils.createFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator + ProcessorUtils.DATA_RAW_DIR
            + File.separator + "image1", excelFile);
    fileStorageServiceBean.uploadImageFilesToSourceLocation(images.entrySet().iterator().next(), bulkProcess,
        ProcessorUtils.DATA_BASE_DIR + "/mta/source");
  }

  @Test
  public void uploadImageFilesToSourceLocationGcsEnableUploadingToGcsTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "saveImageRawFolderToGcs", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "sourceImageDirectory", "source-image/catalog-image/");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    Map<String, String> images = ImmutableMap.of(PRODUCT_CODE_FILE_NAME, PRODUCT_CODE_FILE_NAME);
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + "/mta/source");
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + "/mta/source/image1", excelFile);
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + PRODUCT_CODE_FILE_NAME)).thenReturn(excelFile);
    fileStorageServiceBean.uploadImageFilesToSourceLocation(images.entrySet().iterator().next(), bulkProcess,
        ProcessorUtils.DATA_BASE_DIR + "/mta/source");
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + PRODUCT_CODE_FILE_NAME);
  }

  @Test
  public void decompressFileGcsEnableTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean,"maxBufferSize", 16384);
    String zipData = new String(Base64.encodeBase64(org.apache.commons.io.IOUtils.toByteArray(
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
        StandardCharsets.UTF_8);
    byte[] zipImageFile = Base64.decodeBase64(zipData);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq(
            "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
                + Constant.SLASH + "image-1.jpg"),
        Mockito.any())).thenReturn(blob);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq(
            "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
                + Constant.SLASH + "image-2.jpg"),
        Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.decompressFile(bulkProcess.getBulkProcessCode(), zipImageFile);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq(
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR + Constant.SLASH + "image-1.jpg"), Mockito.any());
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq(
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR + Constant.SLASH + "image-2.jpg"), Mockito.any());
  }

  @Test
  public void decompressFileGcsDisableTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    ReflectionTestUtils.setField(fileStorageServiceBean,"maxBufferSize", 16384);
    String zipData = new String(Base64.encodeBase64(org.apache.commons.io.IOUtils.toByteArray(
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
        StandardCharsets.UTF_8);
    byte[] zipImageFile = Base64.decodeBase64(zipData);
    fileStorageServiceBean.decompressFile(bulkProcess.getBulkProcessCode(), zipImageFile);
  }

  @Test
  public void decompressFileGcsEnableWithDynamicBufferTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean,"maxBufferSize", 16384);
    String zipData = new String(Base64.encodeBase64(org.apache.commons.io.IOUtils.toByteArray(
      Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
        StandardCharsets.UTF_8);
    byte[] zipImageFile = Base64.decodeBase64(zipData);
    ZipInputStream zipInputStream = mock(ZipInputStream.class);
    Mockito.when(zipInputStream.read(Mockito.any(byte[].class))).thenReturn(4, -1);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq(
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
          + Constant.SLASH + "image-1.jpg"),
      Mockito.any())).thenReturn(blob);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq(
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
          + Constant.SLASH + "image-2.jpg"),
      Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.decompressFile(bulkProcess.getBulkProcessCode(), zipImageFile);
    zipInputStream.close();
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq(
      "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR + Constant.SLASH + "image-1.jpg"), Mockito.any());
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq(
      "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR + Constant.SLASH + "image-2.jpg"), Mockito.any());
  }

  @Test
  public void downloadAndValidateProductCreationImagesGcsEnabledTest() throws IOException {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    BulkUploadErrorCounter bulkUploadErrorCounter = new BulkUploadErrorCounter();
    bulkUploadErrorCounter.setImage(97);
    List<String> images =
        Arrays.asList("image1.jpeg", "image2.gif", "image3.jpg", "image4.jpeg", "image4.jpeg", "image5.jpeg");
    byte[] image2File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image2.gif"));
    byte[] image3File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image3.jpg"));
    byte[] image4File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image4.jpeg"));
    byte[] image5File = {1};
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(0))).thenReturn(null);
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(1))).thenReturn(image2File);
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(2))).thenReturn(image3File);
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(3))).thenReturn(image4File);
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(5))).thenReturn(image5File);

    fileStorageServiceBean.downloadAndValidateProductCreationImages(bulkProcess, bulkUploadErrorCounter,
        new HashMap<>(), new StringBuilder(), images, "0", 10000000, true);

    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(0));
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(1));
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(2));
    Mockito.verify(gcsService, times(2)).downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(3));
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(5));
  }


  @Test
  public void downloadAndValidateProductCreationImagesGcsEnabledContainginResverseMapTest() throws IOException {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    List<String> images = Arrays.asList("image1.jpeg", "image2.gif", "image3.jpg");
    byte[] image2File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image2.gif"));
    byte[] image3File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image3.jpg"));
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(0))).thenReturn(null);
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(1))).thenReturn(image2File);
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(2))).thenReturn(image3File);

    fileStorageServiceBean.downloadAndValidateProductCreationImages(bulkProcess, new BulkUploadErrorCounter(),
        ImmutableMap.of("image1.jpeg", "image1.jpeg", "image2.gif", "image2.gif", "image3.jpg", "image3.jpg"), new StringBuilder(), images, "0",
        1000, true);

    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(0));
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(1));
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + images.get(2));
  }

  @Test
  public void downloadAndValidateProductCreationImagesGcsEnabledExceptionTest() throws IOException {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    List<String> images = Arrays.asList("image1.jpeg", "image2.gif", "image3.jpg");
    Map<String, String> imageAndImageUrlReverseMap = mock(Map.class);
    Mockito.when(imageAndImageUrlReverseMap.containsKey("image1.jpeg")).thenReturn(true);
    byte[] image2File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
      .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image2.gif"));
    byte[] image3File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
      .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image3.jpg"));
    doThrow(ApplicationRuntimeException.class).when(gcsService).downloadFile(BUCKET_NAME,
      "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
        + Constant.SLASH + images.get(0));
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
      "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
        + Constant.SLASH + images.get(1))).thenReturn(image2File);
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
      "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
        + Constant.SLASH + images.get(2))).thenReturn(image3File);

    boolean result = fileStorageServiceBean.downloadAndValidateProductCreationImages(bulkProcess,
      new BulkUploadErrorCounter(),
      ImmutableMap.of("image1.jpeg", "image1.jpeg", "image2.gif", "image2.gif", "image3.jpg",
        "image3.jpg"), new StringBuilder(), images, "0", Integer.MIN_VALUE, false);
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
      "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
        + Constant.SLASH + images.get(0));
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
      "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
        + Constant.SLASH + images.get(1));
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
      "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
        + Constant.SLASH + images.get(2));
  }

  @Test
  public void downloadAndValidateProductCreationImagesGcsEnabledMapContainsTest() throws IOException {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    List<String> images = Arrays.asList("image1.jpeg", "image2.gif", "image3.jpg");
    Map<String, String> imageAndImageUrlReverseMap = mock(Map.class);
    Mockito.when(imageAndImageUrlReverseMap.containsKey("image1.jpeg")).thenReturn(true);
    byte[] image2File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
      .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image2.gif"));
    byte[] image3File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
      .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image3.jpg"));
    doThrow(ApplicationRuntimeException.class).when(gcsService).downloadFile(BUCKET_NAME,
      "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
        + Constant.SLASH + images.get(0));
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
      "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
        + Constant.SLASH + images.get(1))).thenReturn(image2File);
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
      "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
        + Constant.SLASH + images.get(2))).thenReturn(image3File);

    boolean result = fileStorageServiceBean.downloadAndValidateProductCreationImages(bulkProcess,
      new BulkUploadErrorCounter(),
      ImmutableMap.of("image2.gif", "image2.gif", "image3.jpg",
        "image3.jpg"), new StringBuilder(), images, "0", Integer.MIN_VALUE, false);
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
      "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
        + Constant.SLASH + images.get(0));
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
      "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
        + Constant.SLASH + images.get(1));
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
      "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
        + Constant.SLASH + images.get(2));
  }

  @Test
  public void downloadAndValidateProductCreationImagesGcsDisabledTest() throws IOException {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    BulkUploadErrorCounter bulkUploadErrorCounter = new BulkUploadErrorCounter();
    bulkUploadErrorCounter.setImage(97);
    List<String> images =
        Arrays.asList("image1.jpeg", "image2.gif", "image3.jpg", "image4.jpeg", "image4.jpeg", "image5.jpeg");
    byte[] image2File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image2.gif"));
    ProcessorUtils.createFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator + ProcessorUtils.DATA_RAW_DIR
            + File.separator + "image2.gif", image2File);
    byte[] image3File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image3.jpg"));
    ProcessorUtils.createFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator + ProcessorUtils.DATA_RAW_DIR
            + File.separator + "image3.jpg", image3File);
    byte[] image4File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image4.jpeg"));
    ProcessorUtils.createFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator + ProcessorUtils.DATA_RAW_DIR
            + File.separator + "image4.jpeg", image4File);
    byte[] image5File = {1};
    ProcessorUtils.createFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator + ProcessorUtils.DATA_RAW_DIR
            + File.separator + "image5.jpeg", image5File);

    fileStorageServiceBean.downloadAndValidateProductCreationImages(bulkProcess, bulkUploadErrorCounter,
        new HashMap<>(), new StringBuilder(), images, "0", 1000, true);
  }

  @Test
  public void downloadAndValidateProductCreationImagesGcsDisabledContainginResverseMapTest() throws IOException {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    BulkUploadErrorCounter bulkUploadErrorCounter = new BulkUploadErrorCounter();
    bulkUploadErrorCounter.setImage(97);
    List<String> images = Arrays.asList("image1.jpeg", "image2.gif", "image3.jpg");
    byte[] image2File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image2.gif"));
    ProcessorUtils.createFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator + ProcessorUtils.DATA_RAW_DIR
            + File.separator + "image2.gif", image2File);
    byte[] image3File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image3.jpg"));
    ProcessorUtils.createFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator + ProcessorUtils.DATA_RAW_DIR
            + File.separator + "image3.jpg", image3File);

    fileStorageServiceBean.downloadAndValidateProductCreationImages(bulkProcess, bulkUploadErrorCounter,
        ImmutableMap.of("image1.jpeg", "image1.jpeg", "image2.gif", "image2.gif", "image3.jpg", "image3.jpg"),
        new StringBuilder(), images, "0", 1000, true);
  }

  @Test
  public void downloadImagesGcsEnableTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    Map<String, String> imagesToDownload =
        ImmutableMap.of(URL_IMAGE, "1", URL_IMAGE_INVALID_EXTENSION, "2");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq(
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + "image1.png"), Mockito.any())).thenReturn(blob);

    fileStorageServiceBean.downloadImages(bulkProcess.getBulkProcessCode(), imagesToDownload, 2000, 2000,
        new HashSet<>(), new ArrayList<>(), new BulkUploadErrorCounter(), new StringBuilder(), true);

    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq(
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + "image1.png"), Mockito.any());
  }

  @Test
  public void downloadImagesGcsEnableErrorTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    Map<String, String> imagesToDownload =
        ImmutableMap.of(URL_IMAGE, "1", URL_IMAGE_INVALID_EXTENSION, "2");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq(
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + "image1.png"), Mockito.any())).thenThrow(ApplicationRuntimeException.class);

    fileStorageServiceBean.downloadImages(bulkProcess.getBulkProcessCode(), imagesToDownload, 2000, 2000,
        new HashSet<>(), new ArrayList<>(), new BulkUploadErrorCounter(), new StringBuilder(), true);

    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq(
        "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + "image1.png"), Mockito.any());
  }

  @Test
  public void downloadImagesGcsDisabledTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    Map<String, String> imagesToDownload =
        ImmutableMap.of(URL_IMAGE, "image1.jpg", URL_IMAGE_INVALID_EXTENSION, "image2.gif");

    fileStorageServiceBean.downloadImages(bulkProcess.getBulkProcessCode(), imagesToDownload, 2000, 2000,
        new HashSet<>(), new ArrayList<>(), new BulkUploadErrorCounter(), new StringBuilder(), true);
  }

  @Test
  public void getNotificationTypeTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    Assertions.assertEquals(NotificationTypeConstant.VENDOR_BULK_DOWNLOADED_V2,
        fileStorageServiceBean.getNotificationType(BulkProcessEntity.PRODUCT_VENDOR));
    Assertions.assertEquals(NotificationTypeConstant.BULK_DOWNLOADED_V2,
        fileStorageServiceBean.getNotificationType(BulkProcessEntity.PRODUCT));

    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    Assertions.assertEquals(NotificationTypeConstant.VENDOR_BULK_DOWNLOADED,
        fileStorageServiceBean.getNotificationType(BulkProcessEntity.PRODUCT_VENDOR));
    Assertions.assertEquals(NotificationType.BULK_DOWNLOADED.getValue(),
        fileStorageServiceBean.getNotificationType(BulkProcessEntity.PRODUCT));
  }

  @Test
  public void downloadFileErrorCampaignGCSTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    bulkProcess.setBulkProcessType("CampaignError");
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn("Hello".getBytes(
        StandardCharsets.UTF_8));
    byte[] data = fileStorageServiceBean.downloadFile(bulkProcess,"xlsx");
    Assertions.assertNotNull(data);
  }

  @Test
  public void campaignErrorFilePathTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    String link = fileStorageServiceBean
      .campaignErrorFilePath(FILE_PATH, "FailedUpsertMppProduct", BULK_PROCESS_CODE, FILE_NAME);
    Assertions.assertNotNull(link);
  }

  @Test
  public void campaignErrorFilePathGcsFalseTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    String link = fileStorageServiceBean
      .campaignErrorFilePath(FILE_PATH, "FailedUpsertMppProduct", BULK_PROCESS_CODE, FILE_NAME);
    Assertions.assertNotNull(link);
  }
 @Test
 public void writeGenericTemplateTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    fileStorageServiceBean.writeGenericTemplate(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + bulkProcess.getBulkProcessCode()
      + File.separator + bulkProcess.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL,
      new XSSFWorkbook());
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(),Mockito.any());
  }

  @Test
  public void writeGenericTemplateFileStoreTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", false);
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation", BLIBLI_MASS_TEMPLATE_LOCATION);
    mockFile(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + TEMPLATE_FILE);
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
    ProcessorUtils.createFile(
      BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + TEMPLATE_FILE, excelFile);
    fileStorageServiceBean.writeGenericTemplate(TEMPLATE_FILE, new XSSFWorkbook());
  }

  @Test
  public void writeGenericTemplateFileStoreExceptionTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    fileStorageServiceBean.writeGenericTemplate(
      BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + TEMPLATE_FILE,
      new XSSFWorkbook());
  }

  @Test
  public void getUnifiedBaseTemplateTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation",
        BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsMassTemplateLocation",
        BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFile",
        BLIBLI_UNIFIED_TEMPLATE);
    mockFile(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH
        + BLIBLI_UNIFIED_UPLOAD_TEMPLATE);
    mockFile(
        BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE);
    XSSFWorkbook workBook = null;
    try (InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
      workBook = new XSSFWorkbook(is);
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      workBook.write(baos);
      byte[] xls = baos.toByteArray();
      ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
      ProcessorUtils.createFile(
          BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE,
          xls);
      ProcessorUtils.createFile(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH
          + BLIBLI_UNIFIED_UPLOAD_TEMPLATE, xls);
      Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString()))
          .thenReturn(xls);
      fileStorageServiceBean.getUnifiedBaseTemplate(BLIBLI_UNIFIED_TEMPLATE,
          BLIBLI_UNIFIED_UPLOAD_TEMPLATE, true);
      Mockito.verify(gcsService).downloadFile(Mockito.anyString(), Mockito.anyString());
    }
  }

  @Test
  public void getUnifiedBaseTemplate_NoFileDataTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation", BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFile",
      BLIBLI_UNIFIED_TEMPLATE.replace(".xlsx","-1.xlsx"));
    mockFile( BASE_DIRECTORY  +BLIBLI_MASS_TEMPLATE_LOCATION+ Constant.SLASH + BLIBLI_UNIFIED_UPLOAD_TEMPLATE);
    mockFile( BASE_DIRECTORY  +BLIBLI_MASS_TEMPLATE_LOCATION+ Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE);
    XSSFWorkbook workBook = null;
    try(InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
      workBook = new XSSFWorkbook(is);
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      workBook.write(baos);
      byte[] xls = baos.toByteArray();
      byte[] empty = new byte[0];
      ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
      ProcessorUtils.createFile(
          BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE,
          xls);
      ProcessorUtils.createFile(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH
          + BLIBLI_UNIFIED_UPLOAD_TEMPLATE, xls);
      doThrow(ApplicationRuntimeException.class).when(gcsService)
          .downloadFile(BUCKET_NAME, "blibliMassTemplateLocation/Blibli-mass-upload-template.xlsx");
      Mockito.when(gcsService.downloadFile(BUCKET_NAME,
          "blibliMassTemplateLocation/Blibli-mass" + "-base-template.xlsx")).thenReturn(xls);
      fileStorageServiceBean.getUnifiedBaseTemplate(BLIBLI_UNIFIED_TEMPLATE,
          BLIBLI_UNIFIED_UPLOAD_TEMPLATE, false);
      Mockito.verify(gcsService, times(2)).downloadFile(Mockito.anyString(), Mockito.anyString());
    }
  }

 @Test
 public void isCategoryTemplateFileExistTest() throws Exception {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"categoryTemplateLocation",
      BLIBLI_MASS_TEMPLATE_LOCATION+Constant.SLASH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation", BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFile", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
   XSSFWorkbook workBook = null;
   try(InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
     workBook = new XSSFWorkbook(is);
     ByteArrayOutputStream baos = new ByteArrayOutputStream();
     workBook.write(baos);
     byte[] xls = baos.toByteArray();
     ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
     ProcessorUtils.createFile(
         BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE,
         xls);
     ProcessorUtils.createFile(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH
         + BLIBLI_UNIFIED_UPLOAD_TEMPLATE, xls);
     Mockito.when(
             gcsService.copyBlobInChunks(Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
         .thenReturn(blob);
     fileStorageServiceBean.isCategoryTemplateFileExist(BLIBLI_UNIFIED_UPLOAD_TEMPLATE,
         BLIBLI_UNIFIED_UPLOAD_TEMPLATE, BLIBLI_UNIFIED_TEMPLATE, BLIBLI_UNIFIED_TEMPLATE);
   }
  }

  @Test
  public void isCategoryTemplateFileExist_FileStoreTest() throws Exception {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", false);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "categoryTemplateLocation",
      BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation", BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFile", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
    XSSFWorkbook workBook = null;
    try(InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
      workBook = new XSSFWorkbook(is);
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      workBook.write(baos);
      byte[] xls = baos.toByteArray();
      ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
      ProcessorUtils.createFile(
          BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE,
          xls);
      ProcessorUtils.createFile(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH
          + BLIBLI_UNIFIED_UPLOAD_TEMPLATE, xls);
      fileStorageServiceBean.isCategoryTemplateFileExist(BLIBLI_UNIFIED_UPLOAD_TEMPLATE,
          BLIBLI_UNIFIED_UPLOAD_TEMPLATE, BLIBLI_UNIFIED_TEMPLATE, BLIBLI_UNIFIED_TEMPLATE);
    }
  }

  @Test
  public void isCategoryTemplateFileExist_NoCopyNeededTest() throws Exception {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"categoryTemplateLocation",
      BLIBLI_MASS_TEMPLATE_LOCATION+Constant.SLASH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation", BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFile", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
    XSSFWorkbook workBook = null;
    try(InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
      workBook = new XSSFWorkbook(is);
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      workBook.write(baos);
      byte[] xls = baos.toByteArray();
      ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
      ProcessorUtils.createFile(
          BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE,
          xls);
      ProcessorUtils.createFile(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH
          + BLIBLI_UNIFIED_UPLOAD_TEMPLATE, xls);
      Mockito.when(gcsService.isFileExists(Mockito.anyString(), Mockito.anyString()))
          .thenReturn(true);
      fileStorageServiceBean.isCategoryTemplateFileExist(BLIBLI_UNIFIED_UPLOAD_TEMPLATE,
          BLIBLI_UNIFIED_UPLOAD_TEMPLATE, BLIBLI_UNIFIED_TEMPLATE, BLIBLI_UNIFIED_TEMPLATE);
    }
  }

  @Test
  public void isCategoryTemplateFileExistWithExceptionTest() throws Exception {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"categoryTemplateLocation",
      BLIBLI_MASS_TEMPLATE_LOCATION+Constant.SLASH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation", BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFile", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
    XSSFWorkbook workBook = null;
    try(InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
      workBook = new XSSFWorkbook(is);
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      workBook.write(baos);
      byte[] xls = baos.toByteArray();
      ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
      ProcessorUtils.createFile(
          BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE,
          xls);
      ProcessorUtils.createFile(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH
          + BLIBLI_UNIFIED_UPLOAD_TEMPLATE, xls);
      ProcessorUtils.createFile(
          BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + "Blibli-mass-base"
              + "-template.xlsx", xls);
      Mockito.when(gcsService.copyBlobInChunks(Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString())).thenReturn(null);
      Assertions.assertThrows(Exception.class,
          () -> fileStorageServiceBean.isCategoryTemplateFileExist(BLIBLI_UNIFIED_UPLOAD_TEMPLATE,
              BLIBLI_UNIFIED_UPLOAD_TEMPLATE, BLIBLI_UNIFIED_TEMPLATE, BLIBLI_UNIFIED_TEMPLATE));
    }
  }

  @Test
  public void isCategoryTemplateFileExistWithFileStoreExceptionTest() throws Exception {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"categoryTemplateLocation",
      BLIBLI_MASS_TEMPLATE_LOCATION+Constant.SLASH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation", BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFile", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
    XSSFWorkbook workBook = null;
    try(InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
      workBook = new XSSFWorkbook(is);
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      workBook.write(baos);
      byte[] xls = baos.toByteArray();
      ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
      ProcessorUtils.createFile(
          BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE,
          xls);
      ProcessorUtils.createFile(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH
          + BLIBLI_UNIFIED_UPLOAD_TEMPLATE, xls);
      ProcessorUtils.createFile(
          BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE,
          xls);
      Mockito.when(gcsService.copyBlobInChunks(Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString())).thenReturn(null);
      Assertions.assertThrows(Exception.class,
          () -> fileStorageServiceBean.isCategoryTemplateFileExist(BLIBLI_UNIFIED_UPLOAD_TEMPLATE,
              BLIBLI_UNIFIED_UPLOAD_TEMPLATE, BLIBLI_UNIFIED_TEMPLATE, BLIBLI_UNIFIED_TEMPLATE));
    }
  }

  @Test
  public void uploadRegeneratedTemplatesTest() throws Exception {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFile", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFileEnglish", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"categoryTemplateLocation",
      BLIBLI_MASS_TEMPLATE_LOCATION+Constant.SLASH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation", BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    try(InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
      XSSFWorkbook workBook = new XSSFWorkbook(is);
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      workBook.write(baos);
      byte[] xls = baos.toByteArray();
      byte[] xlsEn = baos.toByteArray();
      ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
      ProcessorUtils.createFile(
          BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE,
          xls);
      ProcessorUtils.createFile(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH
          + BLIBLI_UNIFIED_UPLOAD_TEMPLATE, xls);
      Mockito.when(
              gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
          .thenReturn(blob);
      fileStorageServiceBean.uploadRegeneratedTemplates(xls, xlsEn,
          GenericTemplateFileType.PURE_DELIVERY_FILE.name(), false);
    }
  }

  @Test
  public void uploadRegeneratedTemplatesFileStoreTest() throws Exception {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", false);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFile", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFileEnglish", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"categoryTemplateLocation",
      BLIBLI_MASS_TEMPLATE_LOCATION+Constant.SLASH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation", BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    try(InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
      XSSFWorkbook workBook = new XSSFWorkbook(is);
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      workBook.write(baos);
      byte[] xls = baos.toByteArray();
      byte[] xlsEn = baos.toByteArray();
      ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
      ProcessorUtils.createFile(
          BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE,
          xls);
      ProcessorUtils.createFile(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH
          + BLIBLI_UNIFIED_UPLOAD_TEMPLATE, xls);
      Mockito.when(
              gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
          .thenReturn(blob);
      fileStorageServiceBean.uploadRegeneratedTemplates(xls, xlsEn,
          GenericTemplateFileType.PURE_DELIVERY_FILE.name(), false);
    }
  }

  @Test
  public void uploadRegeneratedTemplatesFileStoreForCategoryTempTest() throws Exception {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", false);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFile", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFileEnglish", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"categoryTemplateLocation",
      BLIBLI_MASS_TEMPLATE_LOCATION+Constant.SLASH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation", BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    try(InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
      XSSFWorkbook workBook = new XSSFWorkbook(is);
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      workBook.write(baos);
      byte[] xls = baos.toByteArray();
      byte[] xlsEn = baos.toByteArray();
      ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
      ProcessorUtils.createFile(
          BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE,
          xls);
      ProcessorUtils.createFile(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH
          + BLIBLI_UNIFIED_UPLOAD_TEMPLATE, xls);
      Mockito.when(
              gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
          .thenReturn(blob);
      fileStorageServiceBean.uploadRegeneratedTemplates(xls, xlsEn,
          GenericTemplateFileType.DEFAULT_FILE.name(), true);
    }
  }

  @Test
  public void uploadRegeneratedTemplatesFileStoreForCategoryTempGdnEnableTest() throws Exception {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFile", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFileEnglish", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"categoryTemplateLocation",
        BLIBLI_MASS_TEMPLATE_LOCATION+Constant.SLASH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation", BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    try(InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
      XSSFWorkbook workBook = new XSSFWorkbook(is);
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      workBook.write(baos);
      byte[] xls = baos.toByteArray();
      byte[] xlsEn = baos.toByteArray();
      ProcessorUtils.createDirectories(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION);
      ProcessorUtils.createFile(
          BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH + BLIBLI_UNIFIED_TEMPLATE,
          xls);
      ProcessorUtils.createFile(BASE_DIRECTORY + BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH
          + BLIBLI_UNIFIED_UPLOAD_TEMPLATE, xls);
      Mockito.when(
              gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
          .thenReturn(blob);
      fileStorageServiceBean.uploadRegeneratedTemplates(xls, xlsEn,
          GenericTemplateFileType.DEFAULT_FILE.name(), true);
      Mockito.verify(gcsService, times(2))
          .uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    }
  }

  @Test
  public void regenerateMasterBrandValuesSheetTest() throws IOException, XmlException, InvalidFormatException {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFile",
      BLIBLI_UNIFIED_TEMPLATE+BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFileEnglish",
      BLIBLI_UNIFIED_TEMPLATE+BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"categoryTemplateLocation",
      BLIBLI_MASS_TEMPLATE_LOCATION+Constant.SLASH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation", BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    try(InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
      XSSFWorkbook workBook = new XSSFWorkbook(is);
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      workBook.write(baos);
      byte[] xls = baos.toByteArray();
      byte[] xlsEn = baos.toByteArray();
      Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString()))
          .thenReturn(xls);
      Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString()))
          .thenReturn(xlsEn);
      fileStorageServiceBean.regenerateMasterBrandValuesSheet(
          GenericTemplateFileType.PURE_DELIVERY_FILE.name());
    }
  }

  @Test
  public void regenerateMasterBrandValuesSheet_fileStoreTest() throws IOException, XmlException,
      InvalidFormatException {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", false);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFile", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFileEnglish", BLIBLI_UNIFIED_TEMPLATE);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"categoryTemplateLocation",
      BLIBLI_MASS_TEMPLATE_LOCATION+Constant.SLASH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation",
      BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    try(InputStream is = new BufferedInputStream(new FileInputStream(GENERIC_TEMPLATE_1_PATH))) {
      XSSFWorkbook workBook = new XSSFWorkbook(is);
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      workBook.write(baos);
      byte[] xls = baos.toByteArray();
      byte[] xlsEn = baos.toByteArray();
      Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString()))
          .thenReturn(xls);
      Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString()))
          .thenReturn(xlsEn);
      fileStorageServiceBean.regenerateMasterBrandValuesSheet(
          GenericTemplateFileType.PURE_DELIVERY_FILE.name());
    }
  }

  @Test
  public void getCategoryTemplateFilePathTest(){
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", false);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"categoryTemplateLocation",
      BLIBLI_MASS_TEMPLATE_LOCATION+Constant.SLASH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation", BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    String categoryTemplateFilePath = fileStorageServiceBean.getCategoryTemplateFilePath();
    Assertions.assertEquals(BLIBLI_MASS_TEMPLATE_LOCATION+Constant.SLASH,categoryTemplateFilePath);
  }

  @Test
  public void getCategoryTemplateFilePathForGCSTest(){
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"categoryTemplateLocation",
      BLIBLI_MASS_TEMPLATE_LOCATION+Constant.SLASH);
    ReflectionTestUtils.setField(fileStorageServiceBean, "blibliMassTemplateLocation", BLIBLI_MASS_TEMPLATE_LOCATION);
    ReflectionTestUtils.setField(this.fileStorageServiceBean,"gcsMassTemplateLocation",BLIBLI_MASS_TEMPLATE_LOCATION);
    String categoryTemplateFilePath = fileStorageServiceBean.getCategoryTemplateFilePath();
    Assertions.assertEquals(BLIBLI_MASS_TEMPLATE_LOCATION,categoryTemplateFilePath);
  }


  @Test
  public void isMassTemplateExistsTest() {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsMassTemplateLocation",
      BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "unifiedUploadTemplateFile", BLIBLI_UNIFIED_TEMPLATE);
    Mockito.when(gcsService.isFileExists(Mockito.anyString(),Mockito.anyString())).thenReturn(true);
    boolean massTemplateFileExist = fileStorageServiceBean.isMassTemplateFileExist(FILE_NAME);
    Assertions.assertTrue(massTemplateFileExist);
  }

  @Test
  public void isFileExistsTest(){
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", false);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "categoryTemplateLocation",
      BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH);
    bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
      .internalProcessRequestCode(bulkProcess.getBulkProcessCode()).fileName(fileName1)
      .bulkInternalProcessType(BulkInternalProcessType.SUSPEND).relativePath(FILE_PATH).build();
    Mockito.when(gcsService.isFileExists(Mockito.anyString(),Mockito.anyString())).thenReturn(true);
    Boolean fileExists = fileStorageServiceBean.isFileExists(bulkInternalUploadRequestDTO);
    Assertions.assertTrue(fileExists);
  }

  @Test
  public void isFileExists_PricingBucketTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsPricingBucketName", BUCKET_NAME);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "categoryTemplateLocation",
      BLIBLI_MASS_TEMPLATE_LOCATION + Constant.SLASH);
    bulkInternalUploadRequestDTO = BulkInternalUploadRequestDTO.builder()
      .internalProcessRequestCode(bulkProcess.getBulkProcessCode()).fileName(fileName1)
      .bulkInternalProcessType(BulkInternalProcessType.BULK_SKU_LEVEL_REBATE).relativePath(FILE_PATH).build();
    Mockito.when(gcsService.isFileExists(Mockito.anyString(),Mockito.anyString())).thenReturn(true);
    Boolean fileExists = fileStorageServiceBean.isFileExists(bulkInternalUploadRequestDTO);
    Assertions.assertTrue(fileExists);
  }

  @Test
  public void isFileExistsFilePathTest(){
    fileStorageServiceBean.checkIfFileExistsByFilePath(FILE_PATH);
  }

  @Test
  public void getBasePathSuspensionErrorTest() {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", false);
    fileStorageServiceBean.getBasePath(BulkProcessType.SUSPENSION_ERROR.getValue());
  }


  @Test
  public void getBasePathExternalErrorTest() {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", false);
    fileStorageServiceBean.getBasePath(BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue());
  }

  @Test
  public void getBasePathForPriceRebateTest(){
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsUploadPath", "/xbulk/upload/");
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    String path = fileStorageServiceBean.getBasePath(BulkProcessType.BULK_PRICE_REBATE.getValue());
    Assertions.assertEquals("/xbulk/upload/rebate/", path);
  }

  @Test
  public void getBasePathSuspensionErrorGcsEnabledTest() {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    fileStorageServiceBean.getBasePath(BulkProcessType.SUSPENSION_ERROR.getValue());
  }

  @Test
  public void getBasePathQrGenerationGcsDisabledTest() {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", false);
    fileStorageServiceBean.getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }

  @Test
  public void getBasePathQrGenerationGcsEnabledTest() {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    fileStorageServiceBean.getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }

  @Test
  public void getBasePathConfigErrorTest() {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", false);
    fileStorageServiceBean.getBasePath(BulkProcessType.CONFIGURATION_ERROR.getValue());
  }

  @Test
  public void getBasePathConfigErrorGcsEnabledTest() {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    fileStorageServiceBean.getBasePath(BulkProcessType.CONFIGURATION_ERROR.getValue());
  }

  @Test
  public void getBasePathRecatErrorTest() {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", false);
    fileStorageServiceBean.getBasePath(BulkProcessType.RECAT_ERROR.getValue());
  }

  @Test
  public void getBasePathRecatErrorGcsEnabledTest() {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    fileStorageServiceBean.getBasePath(BulkProcessType.RECAT_ERROR.getValue());
  }

  @Test
  public void moveTmpImageToProductImageGcsDisabledTest() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ReflectionTestUtils.setField(fileStorageServiceBean, "saveImageRawFolderToGcs", false);
    Mockito.when(systemParameter.getMtaApiTmpImage()).thenReturn(BASE_DIRECTORY + "/mtaApiTemp/");
    Mockito.when(systemParameter.getMtaImageSource()).thenReturn(BASE_DIRECTORY + "/mtaSourceTemp/");
    ProcessorUtils.createDirectories(
        BASE_DIRECTORY + "/mtaApiTemp" + Constant.SLASH + DEFAULT_REQUEST_ID + Constant.SLASH);
    ProcessorUtils.createDirectories(
        BASE_DIRECTORY + "/mtaSourceTemp" + Constant.SLASH + PRODUCT_CODE + Constant.SLASH);
    ProcessorUtils.createFile(
        BASE_DIRECTORY + "/mtaApiTemp" + Constant.SLASH + DEFAULT_REQUEST_ID + Constant.SLASH + FILE_NAME, excelFile);

    fileStorageServiceBean.moveTmpImageToProductImage(DEFAULT_REQUEST_ID, ImmutableSet.of(FILE_NAME), PRODUCT_CODE);
  }

  @Test
  public void moveTmpImageToProductImageGcsEnabledTest() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ReflectionTestUtils.setField(fileStorageServiceBean, "saveImageRawFolderToGcs", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "sourceImageDirectory", BASE_DIRECTORY);
    Mockito.when(systemParameter.getMtaApiTmpImage()).thenReturn(BASE_DIRECTORY + "/mtaApiTemp/");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.any(),
        Mockito.eq((BASE_DIRECTORY + Constant.SLASH + PRODUCT_CODE + Constant.SLASH + FILE_NAME).replace("//","/")), Mockito.any())).thenReturn(blob);
    ProcessorUtils.createDirectories(
        BASE_DIRECTORY + "/mtaSourceTemp" + Constant.SLASH + PRODUCT_CODE + Constant.SLASH);
    ProcessorUtils.createFile(
        BASE_DIRECTORY + "/mtaApiTemp" + Constant.SLASH + DEFAULT_REQUEST_ID + Constant.SLASH + FILE_NAME, excelFile);

    fileStorageServiceBean.moveTmpImageToProductImage(DEFAULT_REQUEST_ID, ImmutableSet.of(FILE_NAME), PRODUCT_CODE);
    Mockito.verify(gcsService)
        .uploadCreatedFile(Mockito.any(), Mockito.eq((BASE_DIRECTORY + Constant.SLASH + PRODUCT_CODE + Constant.SLASH + FILE_NAME).replace("//","/")),
            Mockito.any());

  }

  @Test
  public void getBasePathStoreCopyTest() {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", false);
    fileStorageServiceBean.getBasePath(BulkProcessType.STORE_COPY_TEMPLATE.getValue());
  }

  @Test
  public void getBasePathStoreCopyGcsEnabledTest() {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    fileStorageServiceBean.getBasePath(BulkProcessType.STORE_COPY_TEMPLATE.getValue());
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsArchiveErrorUploadPath", FILE_PATH);
    String filePath = fileStorageServiceBean.getBasePath(BulkProcessType.ARCHIVE_ERROR.getValue());
    Assertions.assertEquals(FILE_PATH, filePath);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsDeletePpErrorUploadPath",
      FILE_NAME);
    filePath = fileStorageServiceBean.getBasePath(BulkProcessType.PICKUP_POINT_DELETE_ERROR.getValue());
    Assertions.assertEquals(FILE_NAME, filePath);
  }

  @Test
  public void getBasePathForProductTypeTaggingTest(){
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsUploadPath", "/xbulk/upload/");
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    String path = fileStorageServiceBean.getBasePath(BulkProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.getValue());
    Assertions.assertEquals("/xbulk/upload/productTypeTaggingUpdate/", path);
  }



  @Test
  public void getErrorFileLinkForWorkOrderUploadsTest() {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsWorkOrderErrorUploadPath", FILE_PATH);
    String filePath = fileStorageServiceBean.getBasePath(BulkProcessType.WORK_ORDER_ERROR.getValue());
    Assertions.assertEquals(FILE_PATH, filePath);
  }

  @Test
  public void generateRestrictedKeywordErrorFileTest() throws Exception {
    List<Pair<Integer, String>> excelRowNumberAndErrorMessageMapping =
        Arrays.asList(Pair.of(1, BulkProcessValidationErrorMessages.CATEGORY_NOT_FOUND_ERROR_MESSAGE_ID),
            Pair.of(5, BulkProcessValidationErrorMessages.CATEGORY_NOT_FOUND_ERROR_MESSAGE_ID));
    byte[] sheet = Files.readAllBytes(Path.of(RESTRICTED_KEYWORD_UPDATE_TEMPLATE_PATH));
    Mockito.when(gcsService.downloadFile(BUCKET_NAME, RESTRICTED_KEYWORD_UPDATE_TEMPLATE_PATH)).thenReturn(sheet);
    Mockito.when(
        gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq(RESTRICTED_KEYWORD_UPDATE_TEMPLATE_ERROR_PATH),
            Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.downloadFileAndGenerateErrorFile(RESTRICTED_KEYWORD_UPDATE_TEMPLATE_PATH,
        RESTRICTED_KEYWORD_UPDATE_TEMPLATE_ERROR_PATH, excelRowNumberAndErrorMessageMapping, null);
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME, RESTRICTED_KEYWORD_UPDATE_TEMPLATE_PATH);
    Mockito.verify(gcsService)
        .uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.eq(RESTRICTED_KEYWORD_UPDATE_TEMPLATE_ERROR_PATH),
            Mockito.any());
  }

  @Test
  public void generateRestrictedKeywordErrorFile_pricingBucketTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsPricingBucketName", BUCKET_NAME);
    List<Pair<Integer, String>> excelRowNumberAndErrorMessageMapping =
        Arrays.asList(Pair.of(1, BulkProcessValidationErrorMessages.CATEGORY_NOT_FOUND_ERROR_MESSAGE_ID),
            Pair.of(5, BulkProcessValidationErrorMessages.CATEGORY_NOT_FOUND_ERROR_MESSAGE_ID));
    byte[] sheet = Files.readAllBytes(Path.of(RESTRICTED_KEYWORD_UPDATE_TEMPLATE_PATH));
    Mockito.when(gcsService.downloadFile(BUCKET_NAME, RESTRICTED_KEYWORD_UPDATE_TEMPLATE_PATH)).thenReturn(sheet);
    Mockito.when(
        gcsService.uploadCreatedFile(Mockito.eq(pricingBulkBucket), Mockito.eq(RESTRICTED_KEYWORD_UPDATE_TEMPLATE_ERROR_PATH),
            Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.downloadFileAndGenerateErrorFile(RESTRICTED_KEYWORD_UPDATE_TEMPLATE_PATH,
        RESTRICTED_KEYWORD_UPDATE_TEMPLATE_ERROR_PATH,
        excelRowNumberAndErrorMessageMapping, BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME, RESTRICTED_KEYWORD_UPDATE_TEMPLATE_PATH);
    Mockito.verify(gcsService)
        .uploadCreatedFile(Mockito.eq(pricingBulkBucket), Mockito.eq(RESTRICTED_KEYWORD_UPDATE_TEMPLATE_ERROR_PATH),
            Mockito.any());
  }

  @Test
  public void testGetErrorFileLinkForListing_ProductCreationUpload() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setTotalCount(120);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsDownloadFailedProductsPath",
      "failedProducts/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    Assertions.assertEquals("static/failedProducts/bulkProces/bulkProces.xls", result);
  }

  @Test
  public void testGetErrorFileLinkForListing_ProductExternalCreationUpload()
    throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setTotalCount(120);
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setBulkProcessType(BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue());
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsDownloadFailedProductsPath",
      "failedProducts/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    Assertions.assertTrue(result.contains("static/failedProducts"));
  }

  @Test
  public void ProductBasicInfoUploadTest() throws JsonProcessingException, ParseException {
    ReflectionTestUtils.setField(fileStorageServiceBean,"errorFileNameSize",8);
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_BASIC_INFO.getValue());
    bulkProcess.setBulkProcessCode("123456789-0987654321");
    bulkProcess.setTotalCount(120);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsDownloadFailedProductsPath",
      "failedProducts/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    Assertions.assertEquals("static/target/x-bulk/downloadFailedProducts/12345678/12345678.xlsx", result);
  }

  @Test
  public void testGetErrorFileLinkForListing_ForSubjectToVat() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    bulkProcess.setTotalCount(120);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    when(gcsService.isFileExists(Mockito.anyString(), Mockito.anyString())).thenReturn(true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsSubjectToVatErrorPath", "failedProducts/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    Assertions.assertEquals("static/failedProducts/bulkProc/bulkProc.xlsx", result);
  }

  @Test
  public void testGetErrorFileLinkForListing_ProductCreationUploadWithSmallCode() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    bulkProcess.setBulkProcessCode("BP123");
    bulkProcess.setTotalCount(120);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsDownloadFailedProductsPath",
      "failedProducts/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    Assertions.assertEquals("static/failedProducts/BP123/BP123.xls", result);
  }

  @Test
  public void testGetErrorFileLinkForListing_SubjectToVatErrorPathTest() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    bulkProcess.setBulkProcessCode("BP123");
    when(gcsService.isFileExists(Mockito.anyString(),Mockito.anyString())).thenReturn(true);
    bulkProcess.setTotalCount(120);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsSubjectToVatErrorPath",
            "vatError/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    Assertions.assertEquals("static/vatError/BP123/BP123.xlsx", result);
  }

  @Test
  public void testGetErrorFileLinkForListing_SubjectToVatErrorPathFileNotExistsInGCSTest() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    bulkProcess.setBulkProcessCode("BP123");
    bulkProcess.setTotalCount(120);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsSubjectToVatErrorPath",
            "vatError/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    Assertions.assertEquals(StringUtils.EMPTY
            , result);
  }

  @Test
  public void testGetErrorFileLinkForListing_ProductCreationUploadPriority1() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setTotalCount(4);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsDownloadFailedProductsPath",
      "failedProducts/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    Assertions.assertEquals("static/failedProducts/bulkProces/bulkProces.xls", result);
  }

  @Test
  public void testGetErrorFileLinkForListing_ProductCreationUploadPriority2() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setTotalCount(4);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue());
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsDownloadFailedProductsPath",
      "failedProducts/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    Assertions.assertEquals("static/failedProducts/bulkProces/bulkProces.xls", result);
  }

  @Test
  public void testGetErrorFileLinkForListing_ProductLevel3() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkProcess.setBulkProcessCode("BP456");
    bulkProcess.setStartDate(new Date());
    bulkProcess.setTotalCount(4);
    bulkProcess.setRequestId(DEFAULT_REQUEST_ID);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "comparisonDate", "2023-07-16T16:01:05.091");
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsDownloadFailedProductsPath", "downloadFailedProducts/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    String expected = "static/downloadFailedProducts/BP456/BP456.xlsx";
    Assertions.assertEquals(expected, result);
  }

  @Test
  public void testGetErrorFileLinkForListing_campaignUploads() throws JsonProcessingException,
    ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.CAMPAIGN.getValue());
    bulkProcess.setBulkProcessCode("BP456456-90898989");
    bulkProcess.setStartDate(new Date());
    bulkProcess.setTotalCount(4);
    bulkProcess.setRequestId(DEFAULT_REQUEST_ID);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "comparisonDate", "2023-07-16T16:01:05.091");
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsCampaignErrorUploadPath",
      "downloadFailedCampaignProducts/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    String expected = "<a href=\"static/downloadFailedCampaignProducts/BP456456/BP456456.xlsx\">Download</a>";
    Assertions.assertEquals(expected, result);
  }

  @Test
  public void testGetErrorFileLinkForListing_ProductLevel3BeforeDeployment() throws JsonProcessingException,
    ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkProcess.setBulkProcessCode("BP456");
    bulkProcess.setTotalCount(4);
    SimpleDateFormat simpleDateFormat = new SimpleDateFormat(START_DATE_FORMAT);
    Date startDate = simpleDateFormat.parse("2023-07-10T16:01:05.091");
    bulkProcess.setStartDate(startDate);
    bulkProcess.setRequestId(DEFAULT_REQUEST_ID);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "comparisonDate", "2023-07-16T16:01:05.091");
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsDownloadFailedProductsPath", "downloadFailedProducts/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    String expected = "static/downloadFailedProducts/123/123.xlsx";
    Assertions.assertEquals(expected, result);
  }

  @Test
  public void testGetErrorFileLinkForListing_ProductLevel3AfterDeployment() throws JsonProcessingException,
    ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkProcess.setBulkProcessCode("BP456");
    bulkProcess.setTotalCount(4);
    SimpleDateFormat simpleDateFormat = new SimpleDateFormat(START_DATE_FORMAT);
    Date startDate = simpleDateFormat.parse("2023-07-19T00:00:00.091");
    bulkProcess.setStartDate(startDate);
    bulkProcess.setRequestId(DEFAULT_REQUEST_ID);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "comparisonDate", "2023-07-16T16:01:05.091");
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsDownloadFailedProductsPath", "downloadFailedProducts/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    String expected = "static/downloadFailedProducts/BP456/BP456.xlsx";
    Assertions.assertEquals(expected, result);
  }

  @Test
  public void testGetErrorFileLinkForListing_ProductLevel3_Priority() throws JsonProcessingException,
    ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue());
    bulkProcess.setBulkProcessCode("BP456");
    bulkProcess.setTotalCount(4);
    bulkProcess.setStartDate(new Date());
    bulkProcess.setRequestId(DEFAULT_REQUEST_ID);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "comparisonDate", "2023-07-16T16:01:05.091");
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsDownloadFailedProductsPath", "downloadFailedProducts/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    String expected = "static/downloadFailedProducts/BP456/BP456.xlsx";
    Assertions.assertEquals(expected, result);
  }

  @Test
  public void testGetErrorFileLinkForListing_ProductLevel3_Priority2() throws JsonProcessingException,
    ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue());
    bulkProcess.setBulkProcessCode("BP456");
    bulkProcess.setTotalCount(120);
    bulkProcess.setStartDate(new Date());
    bulkProcess.setRequestId(DEFAULT_REQUEST_ID);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "comparisonDate", "2023-07-16T16:01:05.091");
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsDownloadFailedProductsPath", "downloadFailedProducts/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    String expected = "static/downloadFailedProducts/BP456/BP456.xlsx";
    Assertions.assertEquals(expected, result);
  }

  @Test
  public void testGetErrorFileLinkForListing_EANProductLevel4_AfterDeployment() throws JsonProcessingException,
      ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue());
    bulkProcess.setBulkProcessCode("BP456");
    bulkProcess.setTotalCount(4);
    SimpleDateFormat simpleDateFormat = new SimpleDateFormat(START_DATE_FORMAT);
    Date startDate = simpleDateFormat.parse("2023-07-19T00:00:00.091");
    bulkProcess.setStartDate(startDate);
    bulkProcess.setRequestId(DEFAULT_REQUEST_ID);
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "comparisonDate", "2023-07-16T16:01:05.091");
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsUpdateEanPath", "updateEan/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    String expected = "static/updateEan/BP456/BP456.xlsx";
    Assertions.assertEquals(expected, result);
  }

  @Test
  public void testGetErrorFileLinkForListing_InstantPickupProductUpsert() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    bulkProcess.setBulkProcessCode("BP456");
    bulkProcess.setTotalCount(4);
    bulkProcess.setStartDate(new Date());
    bulkProcess.setRequestId(DEFAULT_REQUEST_ID);
    bulkProcess.setNotes("{\"fileName\":\"sample.xlsx\"}");
    Mockito.when(objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class))
      .thenReturn(AuditTrailInfo.builder().fileName("sample.xlsx").build());
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "comparisonDate", "2023-07-16T16:01:05.091");
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsFailedMppUploadPath", "mppFailed/");

    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    String expected = "static/mppFailed/BP456/BP456.xlsx";
    Assertions.assertEquals(expected, result);
  }

  @Test
  public void testGetErrorFileLinkForListing_InstantPickupProductUpsert_Nofile() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    bulkProcess.setBulkProcessCode("BP456");
    bulkProcess.setStartDate(new Date());
    bulkProcess.setRequestId(DEFAULT_REQUEST_ID);
    bulkProcess.setNotes("{\"fileName\":\"sample.xlsx\"}");
    Mockito.when(objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class))
      .thenReturn(AuditTrailInfo.builder().fileName("sample.xlsx").build());
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageServiceBean, "comparisonDate", "2023-07-16T16:01:05.091");
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsFailedMppUploadPath", "mppFailed/");

    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    String expected = "";
    Assertions.assertEquals(expected, result);
  }
  @Test
  public void testGetErrorFileLinkForListing_UnknownBulkProcessType() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.CAMPAIGN.getValue());
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    Assertions.assertEquals("", result);
  }

  @Test
  public void testGetErrorFileLinkForListing_UpsertDeleteBulkProcessType() throws JsonProcessingException,
    ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue());
    bulkProcess.setTotalCount(10);
    bulkProcess.setBulkProcessCode("BP-1234");
    bulkProcess.setStartDate(new Date());
    ReflectionTestUtils.setField(fileStorageServiceBean, "mppDeleteFileExistDate", "2023-07-16T16:01:05"
      + ".091");
    ReflectionTestUtils.setField(fileStorageServiceBean, "errorFileNameSize", 6);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsDeletePpErrorUploadPath", "mppFailed"
      + "/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    String expected = "static/mppFailed/BP-123/BP-123.xlsx";
    Assertions.assertEquals(expected, result);
  }


  @Test
  public void testGetErrorFileLinkForListing_UpsertDeleteFileNotExistingBulkProcessType() throws JsonProcessingException,
    ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue());
    bulkProcess.setTotalCount(10);
    bulkProcess.setBulkProcessCode("BP-1234");
    bulkProcess.setStartDate(new Date());
    LocalDateTime futureDateTime = LocalDateTime.now().plusHours(3);
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS");
    String futureDateString = futureDateTime.format(formatter);
    ReflectionTestUtils.setField(fileStorageServiceBean, "mppDeleteFileExistDate", futureDateString);
    ReflectionTestUtils.setField(fileStorageServiceBean, "errorFileNameSize", 6);
    ReflectionTestUtils.setField(fileStorageServiceBean, "staticBaseUrl", "static");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsDeletePpErrorUploadPath", "mppFailed"
      + "/");
    String result = fileStorageServiceBean.getErrorFileLinkForListing(bulkProcess);
    Assertions.assertEquals(StringUtils.EMPTY, result);
  }

  /*@Test
  public void downloadAndValidateProductCreationInvalidImagesGcsEnabledTest() throws IOException {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsEnabled", true);
    BulkUploadErrorCounter bulkUploadErrorCounter = new BulkUploadErrorCounter();
    bulkUploadErrorCounter.setImage(97);
    List<String> images =
            Arrays.asList("image1.pdf", "image2.gif", "image3.excel", "image4", "image4.jpeg", "image5.jpeg");
    byte[] image2File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image2.gif"));
    byte[] image3File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image3.jpg"));
    byte[] image4File = IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "image4.jpeg"));
    byte[] image5File = {1};
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
            "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
                    + Constant.SLASH + images.get(0))).thenReturn(null);
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
            "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
                    + Constant.SLASH + images.get(1))).thenReturn(image2File);
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
            "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
                    + Constant.SLASH + images.get(2))).thenReturn(image3File);
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
            "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
                    + Constant.SLASH + images.get(3))).thenReturn(image4File);
    Mockito.when(gcsService.downloadFile(BUCKET_NAME,
            "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
                    + Constant.SLASH + images.get(5))).thenReturn(image5File);

    fileStorageServiceBean.downloadAndValidateProductCreationImages(bulkProcess, bulkUploadErrorCounter,
            new HashMap<>(), new StringBuilder(), images, "0", 10000000, true);

    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
            "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
                    + Constant.SLASH + images.get(0));
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
            "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
                    + Constant.SLASH + images.get(1));
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
            "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
                    + Constant.SLASH + images.get(2));
    Mockito.verify(gcsService, times(2)).downloadFile(BUCKET_NAME,
            "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
                    + Constant.SLASH + images.get(3));
    Mockito.verify(gcsService).downloadFile(BUCKET_NAME,
            "xbulk" + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
                    + Constant.SLASH + images.get(5));
  }*/

  @Test
  public void fetchQRCodeTemplateSuccessTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsMassTemplateLocation", FILE_PATH);
    QrCodeRowInfo qrCodeRowInfo =
        QrCodeRowInfo.builder().qrGenerationType(AllowedQRGenerationType.STORE.getValue())
            .templateSize("A4").isDarkTheme(true).qrPerPage(2).build();
    Map<String, String> templateMap = new HashMap<>();
    templateMap.put("STORE-A4-DARK-2", FILE_NAME);
    when(qrCodeProperties.getQrConfigToTemplateName()).thenReturn(templateMap);
    when(gcsService.downloadFile(BUCKET_NAME, FILE_PATH + QR_TEMPLATE_PATH + FILE_NAME))
        .thenReturn(FILE_NAME.getBytes(Charset.defaultCharset()));
    String template = fileStorageServiceBean.fetchQRCodeTemplate(qrCodeRowInfo);
    Assertions.assertEquals(FILE_NAME, template);
    verify(gcsService).downloadFile(BUCKET_NAME, FILE_PATH + QR_TEMPLATE_PATH + FILE_NAME);
  }

  @Test
  public void fetchQRCodeTemplateNotFoundTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsMassTemplateLocation", FILE_PATH);
    QrCodeRowInfo qrCodeRowInfo =
        QrCodeRowInfo.builder().qrGenerationType(AllowedQRGenerationType.STORE.getValue())
            .templateSize("A4").isDarkTheme(true).qrPerPage(2).build();
    Map<String, String> templateMap = new HashMap<>();
    templateMap.put("not-STORE-A4-DARK-2", FILE_NAME);
    when(qrCodeProperties.getQrConfigToTemplateName()).thenReturn(templateMap);
    try {
      fileStorageServiceBean.fetchQRCodeTemplate(qrCodeRowInfo);
    } catch (RuntimeException e) {
      verifyNoInteractions(gcsService);
    }
  }

  @Test
  public void uploadFileToGcsTest() throws Exception {
    InputStream inputStream = new ByteArrayInputStream(FILE_NAME.getBytes(Charset.defaultCharset()));
    fileStorageServiceBean.uploadFileToGcs(FILE_PATH, MediaType.APPLICATION_PDF_VALUE, inputStream);
    verify(gcsService).uploadCreatedFileStream(Mockito.any(Bucket.class), Mockito.eq(FILE_PATH),
        Mockito.eq(MediaType.APPLICATION_PDF_VALUE), Mockito.eq(inputStream));
  }

  @Test
  public void downloadFileFromGcsBucketTest() throws Exception {
    Mockito.when(gcsService.downloadFile(BUCKET_NAME, FILE_PATH))
        .thenReturn(FILE_NAME.getBytes(Charset.defaultCharset()));
    fileStorageServiceBean.downloadFileFromGcs(FILE_PATH);
    verify(gcsService).downloadFile(BUCKET_NAME, FILE_PATH);
  }

  @Test
  public void listFilesAtGcsDirectoryTest() throws Exception {
    Mockito.when(gcsService.listFilesAtDirectory(BUCKET_NAME, FILE_PATH))
        .thenReturn(null);
    fileStorageServiceBean.listFilesAtGcsDirectory(FILE_PATH);
    verify(gcsService).listFilesAtDirectory(BUCKET_NAME, FILE_PATH);
  }

  @Test
  public void replaceFilesAtGcsDirectoryTest() throws Exception {
    InputStream inputStream = new ByteArrayInputStream(FILE_NAME.getBytes(Charset.defaultCharset()));
    when(gcsService.listFilesAtDirectory(BUCKET_NAME, FILE_PATH)).thenReturn(
        new PageImpl<>(null, null, Collections.singleton(blob)));
    when(gcsService.uploadCreatedFileStream(bulkBucket, FILE_PATH + Constant.SLASH + FILE_NAME,
        MediaType.APPLICATION_PDF_VALUE, inputStream)).thenReturn(blob);

    fileStorageServiceBean.replaceFilesAtGcsDirectory(FILE_PATH, FILE_NAME,
        MediaType.APPLICATION_PDF_VALUE, inputStream);
    verify(gcsService).listFilesAtDirectory(BUCKET_NAME, FILE_PATH);
    verify(gcsService).uploadCreatedFileStream(bulkBucket, FILE_PATH + Constant.SLASH + FILE_NAME,
        MediaType.APPLICATION_PDF_VALUE, inputStream);
    verify(gcsService).deleteFilesByBlobIds(anyList());
  }

  @Test
  public void replaceFilesAtGcsDirectoryContainsDirectoryTest() throws Exception {
    InputStream inputStream = new ByteArrayInputStream(FILE_NAME.getBytes(Charset.defaultCharset()));
    when(blob.isDirectory()).thenReturn(true);
    when(gcsService.listFilesAtDirectory(BUCKET_NAME, FILE_PATH)).thenReturn(
        new PageImpl<>(null, null, Collections.singleton(blob)));
    when(gcsService.uploadCreatedFileStream(bulkBucket, FILE_PATH + Constant.SLASH + FILE_NAME,
        MediaType.APPLICATION_PDF_VALUE, inputStream))
        .thenReturn(blob);

    fileStorageServiceBean.replaceFilesAtGcsDirectory(FILE_PATH, FILE_NAME,
        MediaType.APPLICATION_PDF_VALUE, inputStream);
    verify(gcsService).listFilesAtDirectory(BUCKET_NAME, FILE_PATH);
    verify(gcsService).uploadCreatedFileStream(bulkBucket, FILE_PATH + Constant.SLASH + FILE_NAME,
        MediaType.APPLICATION_PDF_VALUE, inputStream);
    verify(gcsService).deleteFilesByBlobIds(anyList());
  }

  @Test
  public void replaceFilesAtGcsDirectoryUploadFailedTest() throws Exception {
    InputStream inputStream = new ByteArrayInputStream(FILE_NAME.getBytes(Charset.defaultCharset()));
    when(gcsService.listFilesAtDirectory(BUCKET_NAME, FILE_PATH)).thenReturn(
        new PageImpl<>(null, null, Collections.singleton(blob)));

    fileStorageServiceBean.replaceFilesAtGcsDirectory(FILE_PATH, FILE_NAME,
        MediaType.APPLICATION_PDF_VALUE, inputStream);
    verify(gcsService).listFilesAtDirectory(BUCKET_NAME, FILE_PATH);
    verify(gcsService).uploadCreatedFileStream(bulkBucket, FILE_PATH + Constant.SLASH + FILE_NAME,
        MediaType.APPLICATION_PDF_VALUE, inputStream);
  }

  @Test
  public void uploadGenericTemplateToGcsTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsMassTemplateLocation", "");
    ReflectionTestUtils.setField(fileStorageServiceBean, "unifiedUploadTemplateFile", "");
    ReflectionTestUtils.setField(fileStorageServiceBean, "unifiedUploadTemplateFileEnglish", "");
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
        .thenReturn(blob);
    fileStorageServiceBean.uploadGenericTemplateToGcs(new byte[] {}, new byte[] {},
        GenericTemplateFileType.PURE_DELIVERY_FILE.name());
    Mockito.verify(gcsService, times(2)).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void downloadBaseTemplateForPriceRecommendationTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsMassTemplateLocation", "template");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBulkBucketName", "bucket");
    Mockito.when(gcsService.downloadFile("bucket", "template/BulkPriceRecommendationBaseTemplate.xlsx")).thenReturn(new byte[100]);
    fileStorageServiceBean.downloadBaseTemplateForPriceRecommendation();
    Mockito.verify(gcsService).downloadFile("bucket", "template/BulkPriceRecommendationBaseTemplate.xlsx");
  }

  @Test
  public void uploadToPricingBucketTest() throws Exception {
    fileStorageServiceBean.uploadToPricingBucket(FILE_PATH, new byte[] {});
    Mockito.verify(gcsService).uploadCreatedFile(pricingBulkBucket, FILE_PATH, new byte[] {});
  }

  @Test
  public void getDownloadLinkForNeedRevisionDeletionTest(){
    fileStorageServiceBean.getDownloadLinkForNeedRevisionDeletion(FILE_PATH,"deletProcessCode");
  }

  @Test
  public void downloadBaseTemplateForBulkBasicInfoUpdateTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsMassTemplateLocation", "template");
    ReflectionTestUtils.setField(fileStorageServiceBean, "gcsBulkBucketName", "bucket");
    Mockito.when(gcsService.downloadFile("bucket", "template/file-name"))
        .thenReturn(new byte[1]);
    fileStorageServiceBean.downloadBaseTemplateForBulkBasicInfoUpdate(FILE_NAME);
    Mockito.verify(gcsService).downloadFile("bucket", "template/file-name");
  }

  @Test
  void testGetActiveResizedImagePathForGcsEnabled() {
    String imageName = "test.jpg";
    String mediumImageType = Constant.MEDIUM_IMAGE;
    String otherImageType = "thumbnail";
    String mediumResult = fileStorageServiceBean.getActiveResizedImagePathForGcsEnabled(mediumImageType, imageName);
    fileStorageServiceBean.getActiveResizedImagePathForGcsEnabled(otherImageType, imageName);
    Assertions.assertTrue(mediumResult.contains("finalDir"));
    Assertions.assertTrue(mediumResult.contains("mediumDir"));
  }

  @Test
  void testUploadFullActiveImage() throws Exception {
    UploadImageRequest request = new UploadImageRequest();
    request.setImageFileName("image test.jpg");
    request.setBytes(new byte[] {1, 2, 3});
    Blob mockBlob = mock(Blob.class);
    when(gcsService.uploadCreatedFile(eq(finalImageBucket), anyString(), any(byte[].class))).thenReturn(mockBlob);
    fileStorageServiceBean.uploadFullActiveImage(request, PRODUCT_CODE);
    verify(gcsService).uploadCreatedFileWithMimeType(eq(finalImageBucket), anyString(), any(byte[].class),
        eq(PRODUCT_CODE));
  }


  @Test
  void testUploadResizeActiveImageFile() throws Exception {
    String param = Constant.MEDIUM_IMAGE;
    String imageName = "test.jpg";
    int width = 100;
    int height = 100;
    byte[] imageData = new byte[] {1, 2, 3};
    BufferedImage mockImage = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);
    Blob mockBlob = mock(Blob.class);
    when(gcsService.uploadCreatedFile(eq(finalImageBucket), anyString(), any(byte[].class))).thenReturn(mockBlob);
    fileStorageServiceBean.uploadResizeActiveImageFile(param, imageName, width, height, imageData, PRODUCT_CODE);
    verify(gcsService).uploadCreatedFileWithMimeType(eq(finalImageBucket), anyString(), any(byte[].class),
        eq(PRODUCT_CODE));
  }

  @Test
  void getListOfMultipartFile_returnsEmptyList_whenFileNamesEmpty() throws Exception {
    List<MultipartFile> result =
        fileStorageServiceBean.getListOfMultipartFile("user1", Collections.emptyList());
    Assertions.assertTrue(result.isEmpty());
  }

  @Test
  void getListOfMultipartFile_returnsListWithFiles_whenFilesExist() throws Exception {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsUploadPath", "/xbulk/upload/");
    byte[] content1 = "content1".getBytes();
    byte[] content2 = "content2".getBytes();
    String fileName1 = "file1.txt";
    String fileName2 = "file2.txt";
    String user = "user1";
    when(gcsService.downloadFile(eq(BUCKET_NAME),
        eq("/xbulk/upload" + "/" + user + "/" + fileName1))).thenReturn(content1);
    when(gcsService.downloadFile(eq(BUCKET_NAME),
        eq("/xbulk/upload" + "/" + user + "/" + fileName2))).thenReturn(content2);
    List<MultipartFile> result =
        fileStorageServiceBean.getListOfMultipartFile(user, Arrays.asList(fileName1, fileName2));
    Assertions.assertEquals(2, result.size());
    Assertions.assertEquals(fileName1, result.get(0).getName());
    Assertions.assertEquals(content1.length, result.get(0).getBytes().length);
    Assertions.assertEquals(fileName2, result.get(1).getName());
    Assertions.assertEquals(content2.length, result.get(1).getBytes().length);
  }

  @Test
  void getListOfMultipartFile_skipsNullFiles() throws Exception {
    String fileName1 = "file1.txt";
    String fileName2 = "file2.txt";
    String user = "user1";
    when(gcsService.downloadFile(eq(BUCKET_NAME), anyString())).thenReturn(null)
        .thenReturn("some".getBytes());
    List<MultipartFile> result =
        fileStorageServiceBean.getListOfMultipartFile(user, Arrays.asList(fileName1, fileName2));
    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals(fileName2, result.get(0).getName());
  }

  @Test
  void generateFileName_returnsCorrectPath() {
    ReflectionTestUtils.setField(this.fileStorageServiceBean, "gcsUploadPath", "/xbulk/upload/");
    String fileName = "file.txt";
    String user = "user1";
    String expected = "/xbulk/upload/" + user + "/" + fileName;
    String actual = fileStorageServiceBean.generateFileName(fileName, user);
    Assertions.assertEquals(expected, actual);
  }

  @Test
  public void testOpenGcsInputStream_Success() {
    // --- Arrange ---
    BulkUpdateQueue queue = new BulkUpdateQueue();
    queue.setFileName("products.xlsx");

    BulkProcess process = new BulkProcess();
    process.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    process.setBulkProcessCode("BPCODE123");

    String expectedFilePath = "UPDATE/BPCODE123/BPCODE123.xlsx";

    InputStream mockStream = new ByteArrayInputStream("dummy-data".getBytes());
    when(gcsService.openFileStream(anyString(), anyString())).thenReturn(mockStream);

    // --- Act ---
    InputStream result = fileStorageServiceBean.openGcsInputStream(queue, process);

    // --- Assert ---
    Assertions.assertNotNull(result);
    verify(gcsService).openFileStream(anyString(), anyString());
  }

  @Test
  public void testOpenGcsInputStream_NullBulkProcess_ThrowsException() {
    BulkUpdateQueue queue = new BulkUpdateQueue();
    queue.setFileName("file.xlsx");
    Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> fileStorageServiceBean.openGcsInputStream(queue, null));
  }
}
