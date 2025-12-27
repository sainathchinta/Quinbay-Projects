package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.mta.bulk.config.QRCodeProperties;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.QrCodeRowInfo;
import com.gdn.mta.bulk.dto.QrCodeRowItemInfo;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.io.File;
import java.io.FileInputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public class QrCodeFinalizeServiceImplTest {
  private static final String STORE_ID = "store-id";
  private static final String BULK_PROCESS_CODE = "bulk-process-code";
  private static final String ZIP_FILE_URL = "https://blibli.com/123";
  private static final String PATH_PREFIX = "src/test/resources/";
  private static final String TEMP_DIRECTORY = PATH_PREFIX + BulkProcessType.QR_GENERATION.getValue()
      + File.separator + BULK_PROCESS_CODE;
  private static final String PERSISTED_DIRECTORY = PATH_PREFIX + BulkProcessType.QR_GENERATION.getValue()
      + File.separator + "persisted";

  private static final String ERROR_MESSAGE = "Error Message";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String MERCHANT_NAME = "merchantName";
  private static final String ITEM_SKU = "itemSku";
  private static final String PICKUPPOINT_CODE = "pickupPointCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PICKUPPOINT_NAME = "pickupPointName";
  private static final String QR_GENERATION_TYPE_STORE = "STORE";
  private static final String QR_GENERATION_TYPE_ITEM_PICKUPPOINT = "ITEM_PICKUP_POINT";
  private static final String QR_GENERATION_TYPE_PRODUCT = "PRODUCT";
  private static final String QR_GENERATION_TYPE_ITEM = "ITEM";
  private static final String APPLICATION_ZIP_FORMAT = "application/zip";
  private static final String QR_ZIP_FORMAT = "qr-codes.zip";
  private static final String MERGED_FILEPATH = "src/test/resources/QrGeneration/bulk-process-code";
  private static final String MERGED_FILE_NAME = "qr-codes.pdf";
  private static final String GCS_FILEPATH = "QrGenerationbulk-process-code";

  @InjectMocks
  private QrCodeFinalizeServiceImpl qrCodeFinalizeService;
  
  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private PdfUtilityService pdfUtilityService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private NotificationService notificationService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private QRCodeProperties qrCodeProperties;

  @Mock
  private BulkProcessService bulkProcessService;

  private BulkProcess bulkProcess;
  private BulkProcessData failedBulkProcessDataForStore;
  private BulkProcessData failedBulkProcessDataForItemPickupPoint;
  private BulkProcessData failedBulkProcessDataForProduct;
  private BulkProcessData failedBulkProcessDataForItem;
  private QrCodeRowInfo qrCodeRowInfo;
  private QrCodeRowInfo qrCodeRowInfo2;
  private QrCodeRowInfo qrCodeRowInfo3;
  private QrCodeRowInfo qrCodeRowInfo4;
  private QrCodeRowItemInfo qrCodeRowItemInfo;
  private String qrCodeRowInfoAsString;
  private String qrCodeRowInfo2AsString;
  private String qrCodeRowInfo3AsString;
  private String qrCodeRowInfo4AsString;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    this.qrCodeRowItemInfo = new QrCodeRowItemInfo();
    qrCodeRowItemInfo.setItemSku(ITEM_SKU);
    qrCodeRowItemInfo.setPickupPointCode(PICKUPPOINT_CODE);
    qrCodeRowItemInfo.setProductSku(PRODUCT_SKU);
    qrCodeRowItemInfo.setPickupPointName(PICKUPPOINT_NAME);

    this.qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setMerchantCode(MERCHANT_CODE);
    qrCodeRowInfo.setMerchantName(MERCHANT_NAME);
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    qrCodeRowInfo.setQrGenerationType(QR_GENERATION_TYPE_STORE);

    this.qrCodeRowInfoAsString = qrCodeRowInfo.toString();

    this.qrCodeRowInfo2 = new QrCodeRowInfo();
    qrCodeRowInfo2.setMerchantCode(MERCHANT_CODE);
    qrCodeRowInfo2.setMerchantName(MERCHANT_NAME);
    qrCodeRowInfo2.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    qrCodeRowInfo2.setQrGenerationType(QR_GENERATION_TYPE_ITEM_PICKUPPOINT);

    this.qrCodeRowInfo2AsString = qrCodeRowInfo2.toString();

    this.qrCodeRowInfo3 = new QrCodeRowInfo();
    qrCodeRowInfo3.setMerchantCode(MERCHANT_CODE);
    qrCodeRowInfo3.setMerchantName(MERCHANT_NAME);
    qrCodeRowInfo3.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    qrCodeRowInfo3.setQrGenerationType(QR_GENERATION_TYPE_PRODUCT);

    this.qrCodeRowInfo3AsString = qrCodeRowInfo3.toString();

    this.qrCodeRowInfo4 = new QrCodeRowInfo();
    qrCodeRowInfo4.setMerchantCode(MERCHANT_CODE);
    qrCodeRowInfo4.setMerchantName(MERCHANT_NAME);
    qrCodeRowInfo4.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    qrCodeRowInfo4.setQrGenerationType(QR_GENERATION_TYPE_ITEM);

    this.qrCodeRowInfo4AsString = qrCodeRowInfo4.toString();


    this.failedBulkProcessDataForStore = new BulkProcessData();
    this.failedBulkProcessDataForStore.setBulkProcessCode(BULK_PROCESS_CODE);
    this.failedBulkProcessDataForStore.setStatus(BulkProcessData.STATUS_FAIL);
    this.failedBulkProcessDataForStore.setErrorMessage(ERROR_MESSAGE);
    this.failedBulkProcessDataForStore.setBulkRequestData(qrCodeRowInfoAsString);

    this.failedBulkProcessDataForItemPickupPoint = new BulkProcessData();
    this.failedBulkProcessDataForItemPickupPoint.setBulkProcessCode(BULK_PROCESS_CODE);
    this.failedBulkProcessDataForItemPickupPoint.setStatus(BulkProcessData.STATUS_FAIL);
    this.failedBulkProcessDataForItemPickupPoint.setErrorMessage(ERROR_MESSAGE);
    this.failedBulkProcessDataForItemPickupPoint.setBulkRequestData(qrCodeRowInfo2AsString);

    this.failedBulkProcessDataForProduct = new BulkProcessData();
    this.failedBulkProcessDataForProduct.setBulkProcessCode(BULK_PROCESS_CODE);
    this.failedBulkProcessDataForProduct.setStatus(BulkProcessData.STATUS_FAIL);
    this.failedBulkProcessDataForProduct.setErrorMessage(ERROR_MESSAGE);
    this.failedBulkProcessDataForProduct.setBulkRequestData(qrCodeRowInfo3AsString);

    this.failedBulkProcessDataForItem = new BulkProcessData();
    this.failedBulkProcessDataForItem.setBulkProcessCode(BULK_PROCESS_CODE);
    this.failedBulkProcessDataForItem.setStatus(BulkProcessData.STATUS_FAIL);
    this.failedBulkProcessDataForItem.setErrorMessage(ERROR_MESSAGE);
    this.failedBulkProcessDataForItem.setBulkRequestData(qrCodeRowInfo4AsString);

    this.bulkProcess = new BulkProcess();
    this.bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    this.bulkProcess.setDescription(AllowedQRGenerationType.STORE.getValue());
    this.bulkProcess.setBulkProcessType(BulkProcessType.QR_GENERATION.getValue());

    when(qrCodeProperties.getLocalTempDirectory()).thenReturn(PATH_PREFIX);
    when(fileStorageService.getBasePath(BulkProcessType.QR_GENERATION.getValue()))
        .thenReturn(BulkProcessType.QR_GENERATION.getValue());
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(bulkProcessDataService, pdfUtilityService, fileStorageService,
        notificationService, objectMapper, qrCodeProperties, bulkProcessService);
  }

  @Test
  public void setFinalStatusAndSendNotificationOnQRGeneration_success() throws Exception {
    Path tempPdfPath = Path.of(TEMP_DIRECTORY + "/qr-codes.pdf");
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + "/qr-codes.pdf");
    if (!Files.exists(tempPdfPath)) {
      Files.createDirectories(tempPdfPath);
    }
    Files.copy(persistedPdfPath, tempPdfPath, StandardCopyOption.REPLACE_EXISTING);

    Path outputFilePath = Path.of(TEMP_DIRECTORY + "/qr-codes.zip");
    try {
      Files.deleteIfExists(outputFilePath);

      when(bulkProcessDataService.getFailedDataForProcessedFile(STORE_ID,
          BULK_PROCESS_CODE)).thenReturn(Collections.emptyList());
      when(fileStorageService.replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class))).thenReturn(ZIP_FILE_URL);

      qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(STORE_ID,
          this.bulkProcess);

      verify(qrCodeProperties).getLocalTempDirectory();
      verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
      verify(bulkProcessDataService).getFailedDataForProcessedFile(STORE_ID, BULK_PROCESS_CODE);
      verify(pdfUtilityService).generateMergedPdfFileFromPdfFilesAtGcsDirectory(anyString(),
          anyString(), anyString());
      verify(fileStorageService).replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class));
      verify(notificationService).sendGenerateQrCodeNotification(this.bulkProcess, ZIP_FILE_URL);
      verifyNoInteractions(bulkProcessService);
    } finally {
      Files.deleteIfExists(outputFilePath);
    }
  }

  @Test
  public void setFinalStatusAndSendNotificationOnQRGeneration_addToBag() throws Exception {
    Path tempPdfPath = Path.of(TEMP_DIRECTORY + "/qr-codes.pdf");
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + "/qr-codes.pdf");
    List<String> statusList = new ArrayList<>();
    statusList.add(BulkProcessData.STATUS_FAIL);
    statusList.add(BulkProcessData.STATUS_SUCCESS);
    if (!Files.exists(tempPdfPath)) {
      Files.createDirectories(tempPdfPath);
    }
    bulkProcess.setDescription(AllowedQRGenerationType.ADD_TO_BAG.getValue());
    Files.copy(persistedPdfPath, tempPdfPath, StandardCopyOption.REPLACE_EXISTING);

    Path outputFilePath = Path.of(TEMP_DIRECTORY + "/qr-codes.zip");
    try {
      Files.deleteIfExists(outputFilePath);
      Map<String, String> errorMap = new HashMap<>();
      errorMap.put(ITEM_SKU, PICKUPPOINT_NAME);
      BulkProcessData bulkProcessData = new BulkProcessData();

      this.qrCodeRowInfo4 = new QrCodeRowInfo();
      qrCodeRowInfo4.setMerchantCode(MERCHANT_CODE);
      qrCodeRowInfo4.setMerchantName(MERCHANT_NAME);
      qrCodeRowInfo4.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.getValue());
      qrCodeRowInfo4.setRowItems(Collections.singletonList(qrCodeRowItemInfo));

      this.qrCodeRowInfo4AsString = qrCodeRowInfo4.toString();

      bulkProcessData.setBulkRequestData(qrCodeRowInfo4AsString);
      bulkProcessData.setErrorMessage("errorMessage");

      List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
      bulkProcessDataList.add(bulkProcessData);

      when(bulkProcessDataService.getFailedDataForProcessedFileInStatusIn(STORE_ID,
          bulkProcess.getBulkProcessCode(), statusList)).thenReturn(bulkProcessDataList);
      qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.getValue());
      when(objectMapper.readValue(anyString(), eq(QrCodeRowInfo.class))).thenReturn(qrCodeRowInfo);
      when(objectMapper.readValue(anyString(),  any(TypeReference.class))).thenReturn(errorMap);
      when(fileStorageService.replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class))).thenReturn(ZIP_FILE_URL);

      qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(STORE_ID,
          this.bulkProcess);

      verify(qrCodeProperties).getLocalTempDirectory();
      verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
      verify(bulkProcessDataService).getFailedDataForProcessedFileInStatusIn(STORE_ID,
          bulkProcess.getBulkProcessCode(), statusList);
      verify(pdfUtilityService).generateMergedPdfFileFromPdfFilesAtGcsDirectory(anyString(),
          anyString(), anyString());
      verify(objectMapper).readValue(anyString(), eq(QrCodeRowInfo.class));
      verify(objectMapper).readValue(anyString(),  any(TypeReference.class));
      verify(fileStorageService).replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class));
      verify(notificationService).sendGenerateQrCodeNotification(this.bulkProcess, ZIP_FILE_URL);
      verifyNoInteractions(bulkProcessService);
    } finally {
      Files.deleteIfExists(outputFilePath);
    }
  }

  @Test
  public void setFinalStatusAndSendNotificationOnQRGeneration_addToBag_aborted_state() throws Exception {
    Path tempPdfPath = Path.of(TEMP_DIRECTORY + "/qr-codes.pdf");
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + "/qr-codes.pdf");
    List<String> statusList = new ArrayList<>();
    statusList.add(BulkProcessData.STATUS_FAIL);
    statusList.add(BulkProcessData.STATUS_SUCCESS);
    if (!Files.exists(tempPdfPath)) {
      Files.createDirectories(tempPdfPath);
    }
    bulkProcess.setDescription(AllowedQRGenerationType.ADD_TO_BAG.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
    Files.copy(persistedPdfPath, tempPdfPath, StandardCopyOption.REPLACE_EXISTING);

    Path outputFilePath = Path.of(TEMP_DIRECTORY + "/qr-codes.zip");
    try {
      Files.deleteIfExists(outputFilePath);
      Map<String, String> errorMap = new HashMap<>();
      errorMap.put(ITEM_SKU, PICKUPPOINT_NAME);
      BulkProcessData bulkProcessData = new BulkProcessData();

      this.qrCodeRowInfo4 = new QrCodeRowInfo();
      qrCodeRowInfo4.setMerchantCode(MERCHANT_CODE);
      qrCodeRowInfo4.setMerchantName(MERCHANT_NAME);
      qrCodeRowInfo4.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.getValue());
      qrCodeRowInfo4.setRowItems(Collections.singletonList(qrCodeRowItemInfo));

      this.qrCodeRowInfo4AsString = qrCodeRowInfo4.toString();

      bulkProcessData.setBulkRequestData(qrCodeRowInfo4AsString);
      bulkProcessData.setErrorMessage("errorMessage");

      List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
      bulkProcessDataList.add(bulkProcessData);

      when(bulkProcessDataService.getFailedDataForProcessedFileInStatusIn(STORE_ID,
          bulkProcess.getBulkProcessCode(), statusList)).thenReturn(bulkProcessDataList);
      qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.getValue());
      when(objectMapper.readValue(anyString(), eq(QrCodeRowInfo.class))).thenReturn(qrCodeRowInfo);
      when(objectMapper.readValue(anyString(),  any(TypeReference.class))).thenReturn(errorMap);
      when(fileStorageService.replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class))).thenReturn(ZIP_FILE_URL);

      qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(STORE_ID,
          this.bulkProcess);

      verify(qrCodeProperties).getLocalTempDirectory();
      verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
      verify(bulkProcessDataService).getFailedDataForProcessedFileInStatusIn(STORE_ID,
          bulkProcess.getBulkProcessCode(), statusList);
      verify(objectMapper).readValue(anyString(), eq(QrCodeRowInfo.class));
      verify(objectMapper).readValue(anyString(),  any(TypeReference.class));
      verify(fileStorageService).replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class));
      verify(notificationService).sendGenerateQrCodeNotification(this.bulkProcess, ZIP_FILE_URL);
      verifyNoInteractions(bulkProcessService);
    } finally {
      Files.deleteIfExists(outputFilePath);
    }
  }

  @Test
  public void setFinalStatusAndSendNotificationOnQRGeneration_addToBag_EmptyList() throws Exception {
    Path tempPdfPath = Path.of(TEMP_DIRECTORY + "/qr-codes.pdf");
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + "/qr-codes.pdf");
    List<String> statusList = new ArrayList<>();
    statusList.add(BulkProcessData.STATUS_FAIL);
    statusList.add(BulkProcessData.STATUS_SUCCESS);
    if (!Files.exists(tempPdfPath)) {
      Files.createDirectories(tempPdfPath);
    }
    bulkProcess.setDescription(AllowedQRGenerationType.ADD_TO_BAG.getValue());
    Files.copy(persistedPdfPath, tempPdfPath, StandardCopyOption.REPLACE_EXISTING);

    Path outputFilePath = Path.of(TEMP_DIRECTORY + "/qr-codes.zip");
    try {
      Files.deleteIfExists(outputFilePath);
      Map<String, String> errorMap = new HashMap<>();
      errorMap.put(ITEM_SKU, PICKUPPOINT_NAME);
      BulkProcessData bulkProcessData = new BulkProcessData();

      this.qrCodeRowInfo4 = new QrCodeRowInfo();
      qrCodeRowInfo4.setMerchantCode(MERCHANT_CODE);
      qrCodeRowInfo4.setMerchantName(MERCHANT_NAME);
      qrCodeRowInfo4.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.getValue());
      qrCodeRowInfo4.setRowItems(Collections.singletonList(qrCodeRowItemInfo));

      this.qrCodeRowInfo4AsString = qrCodeRowInfo4.toString();

      bulkProcessData.setBulkRequestData(qrCodeRowInfo4AsString);

      List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
      bulkProcessDataList.add(bulkProcessData);

      when(bulkProcessDataService.getFailedDataForProcessedFileInStatusIn(STORE_ID,
          bulkProcess.getBulkProcessCode(), statusList)).thenReturn(bulkProcessDataList);
      qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.getValue());
      when(objectMapper.readValue(anyString(), eq(QrCodeRowInfo.class))).thenReturn(qrCodeRowInfo);
      when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(errorMap);
      when(fileStorageService.replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class))).thenReturn(ZIP_FILE_URL);

      qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(STORE_ID,
          this.bulkProcess);

      verify(qrCodeProperties).getLocalTempDirectory();
      verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
      verify(bulkProcessDataService).getFailedDataForProcessedFileInStatusIn(STORE_ID,
          bulkProcess.getBulkProcessCode(), statusList);
      verify(pdfUtilityService).generateMergedPdfFileFromPdfFilesAtGcsDirectory(GCS_FILEPATH,
          MERGED_FILEPATH, MERGED_FILE_NAME);
      verify(fileStorageService).replaceFilesAtGcsDirectory(eq(GCS_FILEPATH), eq(QR_ZIP_FORMAT),
          eq(APPLICATION_ZIP_FORMAT), any(FileInputStream.class));
      verify(notificationService).sendGenerateQrCodeNotification(this.bulkProcess, ZIP_FILE_URL);
      verifyNoInteractions(bulkProcessService);
    } finally {
      Files.deleteIfExists(outputFilePath);
    }
  }

  @Test
  public void setFinalStatusAndSendNotificationOnQRGeneration_addToBag_invalidMap()
      throws Exception {
    Path tempPdfPath = Path.of(TEMP_DIRECTORY + "/qr-codes.pdf");
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + "/qr-codes.pdf");
    if (!Files.exists(tempPdfPath)) {
      Files.createDirectories(tempPdfPath);
    }
    bulkProcess.setDescription(AllowedQRGenerationType.ADD_TO_BAG.getValue());
    Files.copy(persistedPdfPath, tempPdfPath, StandardCopyOption.REPLACE_EXISTING);

    Path outputFilePath = Path.of(TEMP_DIRECTORY + "/qr-codes.zip");
    try {
      Files.deleteIfExists(outputFilePath);
      Map<String, String> errorMap = new HashMap<>();
      errorMap.put(ITEM_SKU, PICKUPPOINT_NAME);
      BulkProcessData bulkProcessData = new BulkProcessData();

      this.qrCodeRowInfo4 = new QrCodeRowInfo();
      qrCodeRowInfo4.setMerchantCode(MERCHANT_CODE);
      qrCodeRowInfo4.setMerchantName(MERCHANT_NAME);
      qrCodeRowInfo4.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.getValue());
      qrCodeRowInfo4.setRowItems(Collections.singletonList(qrCodeRowItemInfo));

      this.qrCodeRowInfo4AsString = qrCodeRowInfo4.toString();

      bulkProcessData.setBulkRequestData(qrCodeRowInfo4AsString);
      bulkProcessData.setErrorMessage("errorMessage");

      List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
      bulkProcessDataList.add(bulkProcessData);

      when(bulkProcessDataService.getFailedDataForProcessedFileInStatusIn(anyString(), anyString(), anyList())).thenReturn(bulkProcessDataList);
      qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.getValue());
      when(objectMapper.readValue(anyString(), eq(QrCodeRowInfo.class))).thenReturn(qrCodeRowInfo);
      when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenThrow(
          JsonProcessingException.class);
      when(fileStorageService.replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class))).thenReturn(ZIP_FILE_URL);

      qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(STORE_ID,
          this.bulkProcess);

      verify(qrCodeProperties).getLocalTempDirectory();
      verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
      verify(bulkProcessDataService).getFailedDataForProcessedFileInStatusIn(anyString(), anyString(), anyList());
      verify(objectMapper).readValue(anyString(), eq(QrCodeRowInfo.class));
      verify(objectMapper).readValue(anyString(), any(TypeReference.class));
      verify(notificationService).sendGenerateQrCodeFailedNotification(null, "ADD_TO_BAG");
      verify(bulkProcessService).saveBulkProcess(any());
    } finally {
      Files.deleteIfExists(outputFilePath);
    }
  }

  @Test
  public void setFinalStatusAndSendNotificationOnQRGeneration_addToBag_emptyErrorMap() throws Exception {
    Path tempPdfPath = Path.of(TEMP_DIRECTORY + "/qr-codes.pdf");
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + "/qr-codes.pdf");
    if (!Files.exists(tempPdfPath)) {
      Files.createDirectories(tempPdfPath);
    }
    bulkProcess.setDescription(AllowedQRGenerationType.ADD_TO_BAG.getValue());
    Files.copy(persistedPdfPath, tempPdfPath, StandardCopyOption.REPLACE_EXISTING);

    Path outputFilePath = Path.of(TEMP_DIRECTORY + "/qr-codes.zip");
    try {
      Files.deleteIfExists(outputFilePath);
      Map<String, String> errorMap = new HashMap<>();
      errorMap.put(ITEM_SKU, PICKUPPOINT_NAME);
      BulkProcessData bulkProcessData = new BulkProcessData();

      this.qrCodeRowInfo4 = new QrCodeRowInfo();
      qrCodeRowInfo4.setMerchantCode(MERCHANT_CODE);
      qrCodeRowInfo4.setMerchantName(MERCHANT_NAME);
      qrCodeRowInfo4.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.getValue());
      qrCodeRowInfo4.setRowItems(Collections.singletonList(qrCodeRowItemInfo));

      this.qrCodeRowInfo4AsString = qrCodeRowInfo4.toString();

      bulkProcessData.setBulkRequestData(qrCodeRowInfo4AsString);
      bulkProcessData.setErrorMessage("errorMessage");

      List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
      bulkProcessDataList.add(bulkProcessData);

      when(bulkProcessDataService.getFailedDataForProcessedFileInStatusIn(anyString(), anyString(), anyList())).thenReturn(bulkProcessDataList);
      qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.getValue());
      when(objectMapper.readValue(anyString(), eq(QrCodeRowInfo.class))).thenReturn(qrCodeRowInfo);
      when(objectMapper.readValue(anyString(),  any(TypeReference.class))).thenReturn(null);
      when(fileStorageService.replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class))).thenReturn(ZIP_FILE_URL);

      qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(STORE_ID,
          this.bulkProcess);

      verify(qrCodeProperties).getLocalTempDirectory();
      verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
      verify(bulkProcessDataService).getFailedDataForProcessedFileInStatusIn(anyString(), anyString(), anyList());
      verify(pdfUtilityService).generateMergedPdfFileFromPdfFilesAtGcsDirectory(anyString(),
          anyString(), anyString());
      verify(objectMapper).readValue(anyString(), eq(QrCodeRowInfo.class));
      verify(objectMapper).readValue(anyString(),  any(TypeReference.class));
      verify(fileStorageService).replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class));
      verify(notificationService).sendGenerateQrCodeNotification(this.bulkProcess, ZIP_FILE_URL);
      verifyNoInteractions(bulkProcessService);
    } finally {
      Files.deleteIfExists(outputFilePath);
    }
  }

  @Test
  public void setFinalStatusAndSendNotificationOnQRGeneration_addToBag_withoutErrorMap() throws Exception {
    Path tempPdfPath = Path.of(TEMP_DIRECTORY + "/qr-codes.pdf");
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + "/qr-codes.pdf");
    if (!Files.exists(tempPdfPath)) {
      Files.createDirectories(tempPdfPath);
    }
    bulkProcess.setDescription(AllowedQRGenerationType.ADD_TO_BAG.getValue());
    Files.copy(persistedPdfPath, tempPdfPath, StandardCopyOption.REPLACE_EXISTING);

    Path outputFilePath = Path.of(TEMP_DIRECTORY + "/qr-codes.zip");
    try {
      Files.deleteIfExists(outputFilePath);
      Map<String, String> errorMap = new HashMap<>();
      BulkProcessData bulkProcessData = new BulkProcessData();

      this.qrCodeRowInfo4 = new QrCodeRowInfo();
      qrCodeRowInfo4.setMerchantCode(MERCHANT_CODE);
      qrCodeRowInfo4.setMerchantName(MERCHANT_NAME);
      qrCodeRowInfo4.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.getValue());
      qrCodeRowInfo4.setRowItems(Collections.singletonList(qrCodeRowItemInfo));

      this.qrCodeRowInfo4AsString = qrCodeRowInfo4.toString();

      bulkProcessData.setBulkRequestData(qrCodeRowInfo4AsString);
      bulkProcessData.setErrorMessage("{}");

      List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
      bulkProcessDataList.add(bulkProcessData);

      when(bulkProcessDataService.getFailedDataForProcessedFileInStatusIn(anyString(), anyString(), anyList())).thenReturn(new ArrayList<>());
      qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.getValue());
      when(fileStorageService.replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class))).thenReturn(ZIP_FILE_URL);

      qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(STORE_ID,
          this.bulkProcess);

      verify(qrCodeProperties).getLocalTempDirectory();
      verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
      verify(bulkProcessDataService).getFailedDataForProcessedFileInStatusIn(anyString(), anyString(), anyList());
      verify(pdfUtilityService).generateMergedPdfFileFromPdfFilesAtGcsDirectory(anyString(),
          anyString(), anyString());
      verify(fileStorageService).replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class));
      verify(notificationService).sendGenerateQrCodeNotification(this.bulkProcess, ZIP_FILE_URL);
      verifyNoInteractions(bulkProcessService);
    } finally {
      Files.deleteIfExists(outputFilePath);
    }
  }

  @Test
  public void setFinalStatusAndSendNotificationOnQRGeneration_abortedProcess() throws Exception {
      bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
      qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(STORE_ID,
          this.bulkProcess);
    verify(notificationService).sendGenerateQrCodeFailedNotification(bulkProcess.getBusinessPartnerCode(),
        bulkProcess.getDescription());
  }

  @Test
  public void setFinalStatusAndSendNotificationOnQRGeneration_exception() throws Exception {
    Path tempPdfPath = Path.of(TEMP_DIRECTORY + "/qr-codes.pdf");
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + "/qr-codes.pdf");
    if (!Files.exists(tempPdfPath)) {
      Files.createDirectories(tempPdfPath);
    }
    Files.copy(persistedPdfPath, tempPdfPath, StandardCopyOption.REPLACE_EXISTING);

    Path outputFilePath = Path.of(TEMP_DIRECTORY + "/qr-codes.zip");
    try {
      Files.deleteIfExists(outputFilePath);

      when(bulkProcessDataService.getFailedDataForProcessedFile(STORE_ID,
          BULK_PROCESS_CODE)).thenReturn(Collections.emptyList());
      when(fileStorageService.replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class))).thenReturn(StringUtils.EMPTY);

      qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(STORE_ID,
          this.bulkProcess);

      verify(qrCodeProperties).getLocalTempDirectory();
      verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
      verify(bulkProcessDataService).getFailedDataForProcessedFile(STORE_ID, BULK_PROCESS_CODE);
      verify(pdfUtilityService).generateMergedPdfFileFromPdfFilesAtGcsDirectory(anyString(),
          anyString(), anyString());
      verify(fileStorageService).replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class));
      verify(bulkProcessService).saveBulkProcess(bulkProcess);
      verify(this.notificationService).sendGenerateQrCodeFailedNotification(bulkProcess.getBusinessPartnerCode(), bulkProcess.getDescription());
    } finally {
      Files.deleteIfExists(outputFilePath);
    }
  }

  @Test
  public void setFinalStatusAndSendNotificationOnQRGeneration_errorsExistForStore() throws Exception {
    Path tempPdfPath = Path.of(TEMP_DIRECTORY + "/qr-codes.pdf");
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + "/qr-codes.pdf");
    if (!Files.exists(tempPdfPath)) {
      Files.createDirectories(tempPdfPath);
    }
    Files.copy(persistedPdfPath, tempPdfPath, StandardCopyOption.REPLACE_EXISTING);

    Path outputFilePath = Path.of(TEMP_DIRECTORY + "/qr-codes.zip");
    try {
      Files.deleteIfExists(outputFilePath);

      when(bulkProcessDataService.getFailedDataForProcessedFile(STORE_ID, BULK_PROCESS_CODE))
          .thenReturn(Collections.singletonList(failedBulkProcessDataForStore));
      when(objectMapper.readValue(qrCodeRowInfoAsString, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
      when(fileStorageService.replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class))).thenReturn(ZIP_FILE_URL);

      qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(STORE_ID,
          this.bulkProcess);

      verify(qrCodeProperties).getLocalTempDirectory();
      verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
      verify(objectMapper).readValue(qrCodeRowInfoAsString, QrCodeRowInfo.class);
      verify(bulkProcessDataService).getFailedDataForProcessedFile(STORE_ID, BULK_PROCESS_CODE);
      verify(pdfUtilityService).generateMergedPdfFileFromPdfFilesAtGcsDirectory(anyString(),
          anyString(), anyString());
      verify(fileStorageService).replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class));
      verify(notificationService).sendGenerateQrCodeNotification(this.bulkProcess, ZIP_FILE_URL);
      verifyNoInteractions(bulkProcessService);
    } finally {
      Files.deleteIfExists(outputFilePath);
    }
  }

  @Test
  public void setFinalStatusAndSendNotificationOnQRGeneration_errorsExistForItemPickupPoint() throws Exception {
    Path tempPdfPath = Path.of(TEMP_DIRECTORY + "/qr-codes.pdf");
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + "/qr-codes.pdf");
    if (!Files.exists(tempPdfPath)) {
      Files.createDirectories(tempPdfPath);
    }
    Files.copy(persistedPdfPath, tempPdfPath, StandardCopyOption.REPLACE_EXISTING);

    Path outputFilePath = Path.of(TEMP_DIRECTORY + "/qr-codes.zip");
    try {
      Files.deleteIfExists(outputFilePath);

      when(bulkProcessDataService.getFailedDataForProcessedFile(STORE_ID, BULK_PROCESS_CODE))
          .thenReturn(Collections.singletonList(failedBulkProcessDataForItemPickupPoint));
      when(objectMapper.readValue(qrCodeRowInfo2AsString, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo2);
      when(fileStorageService.replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class))).thenReturn(ZIP_FILE_URL);

      qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(STORE_ID,
          this.bulkProcess);

      verify(qrCodeProperties).getLocalTempDirectory();
      verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
      verify(objectMapper).readValue(qrCodeRowInfo2AsString, QrCodeRowInfo.class);
      verify(bulkProcessDataService).getFailedDataForProcessedFile(STORE_ID, BULK_PROCESS_CODE);
      verify(pdfUtilityService).generateMergedPdfFileFromPdfFilesAtGcsDirectory(anyString(),
          anyString(), anyString());
      verify(fileStorageService).replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class));
      verify(notificationService).sendGenerateQrCodeNotification(this.bulkProcess, ZIP_FILE_URL);
      verifyNoInteractions(bulkProcessService);
    } finally {
      Files.deleteIfExists(outputFilePath);
    }
  }

  @Test
  public void setFinalStatusAndSendNotificationOnQRGeneration_errorsExistForProduct() throws Exception {
    Path tempPdfPath = Path.of(TEMP_DIRECTORY + "/qr-codes.pdf");
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + "/qr-codes.pdf");
    if (!Files.exists(tempPdfPath)) {
      Files.createDirectories(tempPdfPath);
    }
    Files.copy(persistedPdfPath, tempPdfPath, StandardCopyOption.REPLACE_EXISTING);

    Path outputFilePath = Path.of(TEMP_DIRECTORY + "/qr-codes.zip");
    try {
      Files.deleteIfExists(outputFilePath);

      when(bulkProcessDataService.getFailedDataForProcessedFile(STORE_ID, BULK_PROCESS_CODE))
          .thenReturn(Collections.singletonList(failedBulkProcessDataForProduct));
      when(objectMapper.readValue(qrCodeRowInfo3AsString, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo3);
      when(fileStorageService.replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class))).thenReturn(ZIP_FILE_URL);

      qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(STORE_ID,
          this.bulkProcess);

      verify(qrCodeProperties).getLocalTempDirectory();
      verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
      verify(objectMapper).readValue(qrCodeRowInfo3AsString, QrCodeRowInfo.class);
      verify(bulkProcessDataService).getFailedDataForProcessedFile(STORE_ID, BULK_PROCESS_CODE);
      verify(pdfUtilityService).generateMergedPdfFileFromPdfFilesAtGcsDirectory(anyString(),
          anyString(), anyString());
      verify(fileStorageService).replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class));
      verify(notificationService).sendGenerateQrCodeNotification(this.bulkProcess, ZIP_FILE_URL);
      verifyNoInteractions(bulkProcessService);
    } finally {
      Files.deleteIfExists(outputFilePath);
    }
  }

  @Test
  public void setFinalStatusAndSendNotificationOnQRGeneration_errorsExistForItem() throws Exception {
    Path tempPdfPath = Path.of(TEMP_DIRECTORY + "/qr-codes.pdf");
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + "/qr-codes.pdf");
    bulkProcess.setDescription(AllowedQRGenerationType.PRODUCT.getValue());
    if (!Files.exists(tempPdfPath)) {
      Files.createDirectories(tempPdfPath);
    }
    Files.copy(persistedPdfPath, tempPdfPath, StandardCopyOption.REPLACE_EXISTING);

    Path outputFilePath = Path.of(TEMP_DIRECTORY + "/qr-codes.zip");
    try {
      Files.deleteIfExists(outputFilePath);

      when(bulkProcessDataService.getFailedDataForProcessedFile(STORE_ID, BULK_PROCESS_CODE))
          .thenReturn(Collections.singletonList(failedBulkProcessDataForItem));
      when(objectMapper.readValue(qrCodeRowInfo4AsString, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo4);
      when(fileStorageService.replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class))).thenReturn(ZIP_FILE_URL);

      qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(STORE_ID,
          this.bulkProcess);

      verify(qrCodeProperties).getLocalTempDirectory();
      verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
      verify(objectMapper).readValue(qrCodeRowInfo4AsString, QrCodeRowInfo.class);
      verify(bulkProcessDataService).getFailedDataForProcessedFile(STORE_ID, BULK_PROCESS_CODE);
      verify(pdfUtilityService).generateMergedPdfFileFromPdfFilesAtGcsDirectory(anyString(),
          anyString(), anyString());
      verify(fileStorageService).replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class));
      verify(notificationService).sendGenerateQrCodeNotification(this.bulkProcess, ZIP_FILE_URL);
      verifyNoInteractions(bulkProcessService);
    } finally {
      Files.deleteIfExists(outputFilePath);
    }
  }

  @Test
  public void setFinalStatusAndSendNotificationOnQRGeneration_errorsExist_jsonProcessingException() throws Exception {
    Path tempPdfPath = Path.of(TEMP_DIRECTORY + "/qr-codes.pdf");
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + "/qr-codes.pdf");
    if (!Files.exists(tempPdfPath)) {
      Files.createDirectories(tempPdfPath);
    }
    Files.copy(persistedPdfPath, tempPdfPath, StandardCopyOption.REPLACE_EXISTING);

    Path outputFilePath = Path.of(TEMP_DIRECTORY + "/qr-codes.zip");
    try {
      Files.deleteIfExists(outputFilePath);

      when(bulkProcessDataService.getFailedDataForProcessedFile(STORE_ID, BULK_PROCESS_CODE))
          .thenReturn(Collections.singletonList(failedBulkProcessDataForItem));
      when(objectMapper.readValue(qrCodeRowInfo4AsString, QrCodeRowInfo.class)).thenThrow(
          JsonProcessingException.class);
      when(fileStorageService.replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class))).thenReturn(ZIP_FILE_URL);

      qrCodeFinalizeService.setFinalStatusAndSendNotificationOnQRGeneration(STORE_ID,
          this.bulkProcess);

      verify(qrCodeProperties).getLocalTempDirectory();
      verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
      verify(objectMapper).readValue(qrCodeRowInfo4AsString, QrCodeRowInfo.class);
      verify(bulkProcessDataService).getFailedDataForProcessedFile(STORE_ID, BULK_PROCESS_CODE);
      verify(pdfUtilityService).generateMergedPdfFileFromPdfFilesAtGcsDirectory(anyString(),
          anyString(), anyString());
      verify(fileStorageService).replaceFilesAtGcsDirectory(anyString(), anyString(),
          anyString(), any(FileInputStream.class));
      verify(notificationService).sendGenerateQrCodeNotification(this.bulkProcess, ZIP_FILE_URL);
      verifyNoInteractions(bulkProcessService);
    } finally {
      Files.deleteIfExists(outputFilePath);
    }
  }
}
