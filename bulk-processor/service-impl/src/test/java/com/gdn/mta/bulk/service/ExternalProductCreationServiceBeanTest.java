package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BrandAndCategoryPredictionRequest;
import com.gdn.mta.bulk.dto.BulkProcessExternalUploadRequest;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessImageQC;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.PlatformConfig;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;

public class ExternalProductCreationServiceBeanTest {

  private static final String ZIP_FILE_NAME = "zip file";
  private static final String BULK_PROCESS_CODE = "Bulk_process_code";

  @InjectMocks
  private ExternalProductCreationServiceBean externalProductCreationServiceBean;

  @Mock
  private ProductLevel3ProcessorServiceBean productLevel3ProcessorServiceBean;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private BulkProcessRepository bulkProcessRepository;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private FileStorageServiceBean fileStorageServiceBean;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private BulkProcessImageService bulkProcessImageService;

  @Mock
  private BulkProcessImageQCService bulkProcessImageQCService;

  @Mock
  private NotificationService notificationService;

  BulkProcessExternalUploadRequest request = null;
  PlatformConfig platformConfig;

  @BeforeEach
  public void initializeTest() throws JsonProcessingException {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(externalProductCreationServiceBean, "maxProductCount", 100000);
    request = new BulkProcessExternalUploadRequest();
    request.setZipFileName(ZIP_FILE_NAME);
    platformConfig = new ObjectMapper().readValue(
      "[\n" + "  {\n" + "    \"platform\": \"shopee\",\n" + "    \"joinKey\": \"Kode Produk\",\n"
        + "    \"sheetConfigs\": [\n" + "      {\n" + "        \"sheetType\": \"basicInfo\",\n"
        + "        \"sheetIndex\": 0,\n" + "        \"maxRows\": 10000,\n"
        + "        \"maxColumns\": 50,\n" + "        \"headerRowIndex\": 2,\n"
        + "        \"dataStartRowIndex\": 6,\n" + "        \"columns\": [\n" + "          {\n"
        + "            \"sourceColumn\": \"Kode Produk\",\n"
        + "            \"destinationColumn\": \"productId\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Nama Produk\",\n"
        + "            \"destinationColumn\": \"Nama Produk*\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n"
        + "            \"sourceColumn\": \"Deskripsi Produk\",\n"
        + "            \"destinationColumn\": \"Deskripsi*\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          }\n" + "        ]\n" + "      },\n" + "      {\n"
        + "        \"sheetType\": \"media\",\n" + "        \"sheetIndex\": 0,\n"
        + "        \"maxRows\": 5000,\n" + "        \"maxColumns\": 2000,\n"
        + "        \"headerRowIndex\": 2,\n" + "        \"dataStartRowIndex\": 6,\n"
        + "        \"columns\": [\n" + "          {\n"
        + "            \"sourceColumn\": \"Kode Produk\",\n"
        + "            \"destinationColumn\": \"productId\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Kategori\",\n"
        + "            \"destinationColumn\": \"External category\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Foto Sampul\",\n"
        + "            \"destinationColumn\": \"Foto-1*\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Foto Produk 1\",\n"
        + "            \"destinationColumn\": \"Foto-2\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": false\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Foto Produk 2\",\n"
        + "            \"destinationColumn\": \"Foto-3\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": false\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Foto Produk 3\",\n"
        + "            \"destinationColumn\": \"Foto-4\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": false\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Foto Produk 4\",\n"
        + "            \"destinationColumn\": \"Foto-5\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": false\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Foto Produk 5\",\n"
        + "            \"destinationColumn\": \"Foto-6\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": false\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Foto Produk 6\",\n"
        + "            \"destinationColumn\": \"Foto-7\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": false\n"
        + "          }\n" + "        ]\n" + "      },\n" + "      {\n"
        + "        \"sheetType\": \"shipping\",\n" + "        \"sheetIndex\": 0,\n"
        + "        \"maxRows\": 30000,\n" + "        \"maxColumns\": 200,\n"
        + "        \"headerRowIndex\": 3,\n" + "        \"dataStartRowIndex\": 6,\n"
        + "        \"columns\": [\n" + "          {\n"
        + "            \"sourceColumn\": \"Kode Produk\",\n"
        + "            \"destinationColumn\": \"productId\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Berat Produk/g\",\n"
        + "            \"destinationColumn\": \"Berat (gram)*\",\n"
        + "            \"dataType\": \"NUMBER\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Panjang\",\n"
        + "            \"destinationColumn\": \"Panjang (cm)*\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Lebar\",\n"
        + "            \"destinationColumn\": \"Lebar (cm)*\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Tinggi\",\n"
        + "            \"destinationColumn\": \"Tinggi (cm)*\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          }\n" + "        ]\n" + "      },\n" + "      {\n"
        + "        \"sheetType\": \"sales\",\n" + "        \"sheetIndex\": 0,\n"
        + "        \"maxRows\": 20000,\n" + "        \"maxColumns\": 30,\n"
        + "        \"headerRowIndex\": 2,\n" + "        \"dataStartRowIndex\": 6,\n"
        + "        \"columns\": [\n" + "          {\n"
        + "            \"sourceColumn\": \"Kode Produk\",\n"
        + "            \"destinationColumn\": \"productId\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Kode Variasi\",\n"
        + "            \"destinationColumn\": \"variantId\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Nama Variasi\",\n"
        + "            \"destinationColumn\": \"Variasi\",\n"
        + "            \"dataType\": \"NUMBER\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"SKU\",\n"
        + "            \"destinationColumn\": \"Seller SKU\",\n"
        + "            \"dataType\": \"NUMBER\",\n" + "            \"mandatory\": false\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Harga\",\n"
        + "            \"destinationColumn\": \"Harga Penjualan (Rp)*\",\n"
        + "            \"dataType\": \"NUMBER\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Stok\",\n"
        + "            \"destinationColumn\": \"Available Stock*\",\n"
        + "            \"dataType\": \"NUMBER\",\n" + "            \"mandatory\": true\n"
        + "          }\n" + "        ]\n" + "      }\n" + "    ]\n" + "  }\n" + "]",
      new com.fasterxml.jackson.core.type.TypeReference<List<PlatformConfig>>() {
      }).get(0);
  }

  @AfterEach
  public void after() {
    Mockito.verifyNoMoreInteractions(this.bulkProcessService,
      this.productLevel3ProcessorServiceBean, this.kafkaTopicProperties, this.kafkaProducer,
      this.bulkProcessRepository, this.businessPartnerRepository, this.fileStorageServiceBean,
      this.systemParameterConfigService, this.objectMapper, this.bulkProcessDataService,
      this.bulkProcessImageService, this.bulkProcessImageQCService, this.notificationService);
  }

  @Test
  public void preProcessTest() throws Exception {
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    // 1. zipFileName blank → exception
    Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> externalProductCreationServiceBean.preProcess(Constant.STORE_ID, Constant.REQUEST_ID,
        Constant.USER_NAME, request));
    // 2. bulkProcessCode blank → exception
    request.setZipFileName(ZIP_FILE_NAME);
    Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> externalProductCreationServiceBean.preProcess(Constant.STORE_ID, Constant.REQUEST_ID,
        Constant.USER_NAME, request));
    // 3. files empty → exception
    request.setBulkProcessCode(ZIP_FILE_NAME);
    Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> externalProductCreationServiceBean.preProcess(Constant.STORE_ID, Constant.REQUEST_ID,
        Constant.USER_NAME, request));

    // 4. businessPartnerCode blank → exception
    request.setFiles(Map.of(ZIP_FILE_NAME, ZIP_FILE_NAME));
    Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> externalProductCreationServiceBean.preProcess(Constant.STORE_ID, Constant.REQUEST_ID,
        Constant.USER_NAME, request));

    // 5. Happy path
    request.setBusinessPartnerCode(ZIP_FILE_NAME);

    BulkProcess bulkProcess = new BulkProcess();
    Mockito.when(productLevel3ProcessorServiceBean.createBulkProcess(Mockito.eq(Constant.STORE_ID),
      Mockito.eq(Constant.REQUEST_ID), Mockito.eq(ZIP_FILE_NAME),
      Mockito.eq(BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue()), Mockito.eq(ZIP_FILE_NAME),
      Mockito.eq(ZIP_FILE_NAME), Mockito.anyMap())).thenReturn(bulkProcess);

    Mockito.when(kafkaTopicProperties.getBulkExternalCreateEvent()).thenReturn("topic-test");
    // Call the method
    externalProductCreationServiceBean.preProcess(Constant.STORE_ID, Constant.REQUEST_ID,
      Constant.USER_NAME, request);
    // Verify bulk process saved
    Mockito.verify(productLevel3ProcessorServiceBean)
      .createBulkProcess(Mockito.eq(Constant.STORE_ID), Mockito.eq(Constant.REQUEST_ID),
        Mockito.eq(ZIP_FILE_NAME), Mockito.eq(BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue()),
        Mockito.eq(ZIP_FILE_NAME), Mockito.eq(ZIP_FILE_NAME), Mockito.anyMap());
    Mockito.verify(bulkProcessService).saveBulkProcess(bulkProcess);
    // Verify Kafka event sent
    Mockito.verify(kafkaProducer).send("topic-test", ZIP_FILE_NAME, request);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkExternalCreateEvent();
  }

  @Test
  public void processTest_GeneralException() throws Exception {
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    request.setStoreId(Constant.STORE_ID);
    request.setBulkProcessCode("TEST_CODE");
    request.setZipFileName("test.zip");

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBusinessPartnerCode("BP001");
    bulkProcess.setCreatedDate(new Date());

    Mockito.when(
      bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        Constant.STORE_ID, "TEST_CODE", BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);

    ProfileResponse businessPartner = new ProfileResponse();
    businessPartner.setMerchantStatus("ACTIVE");
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001"))
      .thenReturn(businessPartner);

    byte[] zipData = "test zip data".getBytes();
    Mockito.when(fileStorageServiceBean.downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP))
      .thenReturn(zipData);

    SystemParameterConfig config = new SystemParameterConfig();
    config.setValue("[{\"platform\":\"shopee\",\"sheetConfigs\":[]}]");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES)).thenReturn(config);

    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);
    externalProductCreationServiceBean.process(request);

    Mockito.verify(bulkProcessService, Mockito.times(2))
      .saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(Constant.STORE_ID, "TEST_CODE",
        BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001");
    Mockito.verify(fileStorageServiceBean).downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES);
    Mockito.verify(objectMapper).readValue(anyString(), any(TypeReference.class));
  }

  @Test
  public void processTest() throws Exception {

    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    request.setStoreId(Constant.STORE_ID);
    request.setBulkProcessCode(BULK_PROCESS_CODE);
    request.setZipFileName(BULK_PROCESS_CODE + Constant.FILE_TYPE_ZIP);
    request.setUsername(Constant.USER_NAME);
    request.setFiles(Map.of("PRODUCT_INFO", "NAPOCUT.xlsx"));

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBusinessPartnerCode(BULK_PROCESS_CODE);
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);

    Mockito.when(
      bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        Constant.STORE_ID, BULK_PROCESS_CODE, BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);

    ProfileResponse businessPartner = new ProfileResponse();
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      bulkProcess.getBusinessPartnerCode())).thenReturn(businessPartner);

    byte[] zipData =
      Files.readAllBytes(Paths.get("src/test/resources/externalCreationUpload" + "/NAPOCUT.zip"));
    Mockito.when(fileStorageServiceBean.downloadFile(Mockito.eq(bulkProcess),
      Mockito.eq(Constant.FILE_TYPE_ZIP))).thenReturn(zipData);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue("[{\"platform\":\"Shopee\",\"sheetConfigs\":[]}]");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES))
      .thenReturn(systemParameterConfig);
    bulkProcess.setCreatedDate(new Date());
    ObjectMapper mockMapper = Mockito.mock(ObjectMapper.class);
    Mockito.when(mockMapper.readValue(Mockito.anyString(), any(TypeReference.class)))
      .thenReturn(List.of(platformConfig));

    externalProductCreationServiceBean.process(request);

    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(Constant.STORE_ID,
        BULK_PROCESS_CODE, BulkProcess.STATUS_PENDING);
    Mockito.verify(bulkProcessService, Mockito.times(1))
      .saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, BULK_PROCESS_CODE);

  }

  @Test
  public void processTest_ShopeeConfigFound() throws Exception {
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    request.setStoreId(Constant.STORE_ID);
    request.setBulkProcessCode("TEST_CODE");
    request.setZipFileName("test.zip");

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBusinessPartnerCode("BP001");

    Mockito.when(
      bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        Constant.STORE_ID, "TEST_CODE", BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);

    ProfileResponse businessPartner = new ProfileResponse();
    businessPartner.setMerchantStatus("ACTIVE");
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001"))
      .thenReturn(businessPartner);

    byte[] zipData =
      Files.readAllBytes(Paths.get("src/test/resources/externalCreationUpload" + "/NAPOCUT.zip"));
    Mockito.when(fileStorageServiceBean.downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP))
      .thenReturn(zipData);

    SystemParameterConfig config = new SystemParameterConfig();
    config.setValue("[{\"platform\":\"other\",\"sheetConfigs\":[]}]");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES)).thenReturn(config);
    Mockito.when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenReturn(List.of(platformConfig));
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);
    Map<String, String> excelFileNameToTypeMap = new HashMap<>();
    excelFileNameToTypeMap.put("basicInfo", "Basic Shopee.xlsx");
    excelFileNameToTypeMap.put("sales", "Info Sales Shopee.xlsx");
    excelFileNameToTypeMap.put("shipping", "info shipping napocut.xlsx");
    excelFileNameToTypeMap.put("media", "Media Shopee.xlsx");
    request.setFiles(excelFileNameToTypeMap);
    bulkProcess.setCreatedDate(new Date());
    externalProductCreationServiceBean.process(request);
    Mockito.verify(bulkProcessService, Mockito.times(2))
      .saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(Constant.STORE_ID, "TEST_CODE",
        BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001");
    Mockito.verify(fileStorageServiceBean).downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES);
    Mockito.verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    Mockito.verify(bulkProcessService)
      .publishBulkExternalImageQCDownloadEventModel(any(), anyMap());
    Mockito.verify(bulkProcessImageQCService).saveBulkProcessImageQc(anyList());
  }

  @Test
  public void processTest_ShopeeConfigFoundExceedCount() throws Exception {
    ReflectionTestUtils.setField(externalProductCreationServiceBean, "maxProductCount", 1);
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    request.setStoreId(Constant.STORE_ID);
    request.setBulkProcessCode("TEST_CODE");
    request.setZipFileName("test.zip");

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBusinessPartnerCode("BP001");

    Mockito.when(
      bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        Constant.STORE_ID, "TEST_CODE", BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);

    ProfileResponse businessPartner = new ProfileResponse();
    businessPartner.setMerchantStatus("ACTIVE");
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001"))
      .thenReturn(businessPartner);

    byte[] zipData =
      Files.readAllBytes(Paths.get("src/test/resources/externalCreationUpload" + "/NAPOCUT.zip"));
    Mockito.when(fileStorageServiceBean.downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP))
      .thenReturn(zipData);

    SystemParameterConfig config = new SystemParameterConfig();
    config.setValue("[{\"platform\":\"other\",\"sheetConfigs\":[]}]");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES)).thenReturn(config);
    Mockito.when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenReturn(List.of(platformConfig));
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);
    Map<String, String> excelFileNameToTypeMap = new HashMap<>();
    excelFileNameToTypeMap.put("basicInfo", "Basic Shopee.xlsx");
    excelFileNameToTypeMap.put("sales", "Info Sales Shopee.xlsx");
    excelFileNameToTypeMap.put("shipping", "info shipping napocut.xlsx");
    bulkProcess.setCreatedDate(new Date());
    excelFileNameToTypeMap.put("media", "Media Shopee.xlsx");
    request.setFiles(excelFileNameToTypeMap);
    externalProductCreationServiceBean.process(request);
    Mockito.verify(bulkProcessService, Mockito.times(2))
      .saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(Constant.STORE_ID, "TEST_CODE",
        BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001");
    Mockito.verify(fileStorageServiceBean).downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES);
    Mockito.verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    Mockito.verify(notificationService)
      .sendBulkUploadedNotification(any(), anyString(), anyString());
  }

  @Test
  public void processTest_ShopeeConfigMismatch() throws Exception {
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    request.setStoreId(Constant.STORE_ID);
    request.setBulkProcessCode("TEST_CODE");
    request.setZipFileName("test.zip");

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBusinessPartnerCode("BP001");

    Mockito.when(
      bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        Constant.STORE_ID, "TEST_CODE", BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);

    ProfileResponse businessPartner = new ProfileResponse();
    businessPartner.setMerchantStatus("ACTIVE");
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001"))
      .thenReturn(businessPartner);

    byte[] zipData =
      Files.readAllBytes(Paths.get("src/test/resources/externalCreationUpload" + "/mismatch.zip"));
    Mockito.when(fileStorageServiceBean.downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP))
      .thenReturn(zipData);

    SystemParameterConfig config = new SystemParameterConfig();
    config.setValue("[{\"platform\":\"other\",\"sheetConfigs\":[]}]");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES)).thenReturn(config);
    Mockito.when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenReturn(List.of(platformConfig));
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);
    Map<String, String> excelFileNameToTypeMap = new HashMap<>();
    excelFileNameToTypeMap.put("basicInfo", "Basic Shopee.xlsx");
    excelFileNameToTypeMap.put("sales", "Info Sales Shopee.xlsx");
    excelFileNameToTypeMap.put("shipping", "info shipping napocut.xlsx");
    bulkProcess.setCreatedDate(new Date());
    excelFileNameToTypeMap.put("media", "Media Shopee.xlsx");
    request.setFiles(excelFileNameToTypeMap);
    externalProductCreationServiceBean.process(request);
    Mockito.verify(bulkProcessService, Mockito.times(2))
      .saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(Constant.STORE_ID, "TEST_CODE",
        BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001");
    Mockito.verify(fileStorageServiceBean).downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES);
    Mockito.verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    Mockito.verify(notificationService)
      .sendBulkUploadedNotification(any(), anyString(), anyString());
  }

  @Test
  public void processShouldThrowExceptionWhenBulkProcessNotFound() {
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    request.setStoreId(Constant.STORE_ID);
    request.setBulkProcessCode(Constant.FILE_TYPE_ZIP);
    request.setZipFileName(Constant.FILE_TYPE_ZIP);
    request.setUsername(Constant.USER_NAME);

    Mockito.when(
      bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        Constant.STORE_ID, Constant.FILE_TYPE_ZIP, BulkProcess.STATUS_PENDING)).thenReturn(null);

    Assertions.assertThrows(ApplicationException.class,
      () -> externalProductCreationServiceBean.process(request));

    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(Constant.STORE_ID,
        Constant.FILE_TYPE_ZIP, BulkProcess.STATUS_PENDING);
  }

  @Test
  public void processShouldThrowExceptionWhenBulkProcessNotFoundZipNull() throws Exception {
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    request.setStoreId(Constant.STORE_ID);
    request.setBulkProcessCode(Constant.FILE_TYPE_ZIP);
    request.setZipFileName(Constant.FILE_TYPE_ZIP);
    request.setUsername(Constant.USER_NAME);
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setBusinessPartnerCode("BP001");
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
          Constant.STORE_ID, Constant.FILE_TYPE_ZIP, BulkProcess.STATUS_PENDING))
      .thenReturn(bulkProcess);
    ProfileResponse businessPartner = new ProfileResponse();
    byte[] zipData = null;
    Mockito.when(
        fileStorageServiceBean.downloadFile(Mockito.any(), Mockito.eq(Constant.FILE_TYPE_ZIP)))
      .thenReturn(zipData);
    businessPartner.setMerchantStatus("ACTIVE");
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001"))
      .thenReturn(businessPartner);
    externalProductCreationServiceBean.process(request);
    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(Constant.STORE_ID,
        Constant.FILE_TYPE_ZIP, BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001");
    Mockito.verify(notificationService)
      .sendBulkUploadedNotification(any(), anyString(), anyString());
    Mockito.verify(fileStorageServiceBean).downloadFile(any(), any());
    Mockito.verify(bulkProcessService, Mockito.times(2))
      .saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processBulkProcessImagesShouldSaveAndPublish() {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(ZIP_FILE_NAME);
    bulkProcess.setBulkProcessType(BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue());
    Set<String> imageUrls = Set.of("https://image1.jpg", "https://image2.jpg");
    SystemParameterConfig config = new SystemParameterConfig();
    config.setValue("1");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
      Constant.IMAGE_DOWNLOAD_BATCH_SIZE)).thenReturn(config);

    Mockito.doNothing().when(bulkProcessImageService).saveBulkProcessImage(Mockito.anyList());
    Mockito.doNothing().when(bulkProcessService)
      .publishBulkExternalImageDownloadEventModel(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList());
    externalProductCreationServiceBean.processBulkProcessImages(imageUrls, bulkProcess);
//    Assertions.assertEquals(BulkProcess.STATUS_IN_PROGRESS, bulkProcess.getStatus());
    Mockito.verify(bulkProcessImageService).saveBulkProcessImage(Mockito.anyList());
    Mockito.verify(bulkProcessService, Mockito.atLeastOnce())
      .publishBulkExternalImageDownloadEventModel(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
      Constant.IMAGE_DOWNLOAD_BATCH_SIZE);
  }

  @Test
  public void processBulkProcessImagesShouldNotSaveWhenEmpty() {
    BulkProcess bulkProcess = new BulkProcess();
    Set<String> imageUrls = Set.of();
    externalProductCreationServiceBean.processBulkProcessImages(imageUrls, bulkProcess);
    Mockito.verifyNoInteractions(bulkProcessImageService);
    Mockito.verifyNoInteractions(bulkProcessService);
  }
  @Test
  public void processBulkProcessImageQcForUniqueProductShouldSaveAndPublish() {
    BulkProcess bulkProcess = new BulkProcess();
    Map<String, BrandAndCategoryPredictionRequest> productMap = Map.of("p1", new BrandAndCategoryPredictionRequest());
    List<BulkProcessImageQC> qcList = new ArrayList<>();

    BulkProcessImageQC bulkProcessImageQC = new BulkProcessImageQC();
    Mockito.doNothing().when(bulkProcessImageQCService).saveBulkProcessImageQc(Mockito.anyList());
    Mockito.doNothing().when(bulkProcessService)
      .publishBulkExternalImageQCDownloadEventModel(Mockito.eq(bulkProcess), Mockito.eq(productMap));
    externalProductCreationServiceBean.processBulkProcessImageQcForUniqueProduct(productMap, bulkProcess, qcList);
    Assertions.assertFalse(qcList.isEmpty());
    Mockito.verify(bulkProcessImageQCService).saveBulkProcessImageQc(Mockito.anyList());
    Mockito.verify(bulkProcessService).publishBulkExternalImageQCDownloadEventModel(bulkProcess, productMap);
  }
  @Test
  public void processBulkProcessImageQcForUniqueProductShouldNotSaveWhenEmpty() {
    BulkProcess bulkProcess = new BulkProcess();
    Map<String, BrandAndCategoryPredictionRequest> productMap = Map.of();
    List<BulkProcessImageQC> qcList = new ArrayList<>();

    externalProductCreationServiceBean.processBulkProcessImageQcForUniqueProduct(productMap, bulkProcess, qcList);

    Mockito.verifyNoInteractions(bulkProcessImageQCService);
    Mockito.verifyNoInteractions(bulkProcessService);
  }

  @Test
  public void processTestValidationExceptionWithNewNotificationEnabled() throws Exception {
    ReflectionTestUtils.setField(externalProductCreationServiceBean,
      "newNotificationForHeaderValidationError", true);
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    request.setStoreId(Constant.STORE_ID);
    request.setBulkProcessCode("TEST_CODE");
    request.setZipFileName("test.zip");

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBusinessPartnerCode("BP001");
    bulkProcess.setCreatedDate(new Date());

    Mockito.when(
      bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        Constant.STORE_ID, "TEST_CODE", BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);

    ProfileResponse businessPartner = new ProfileResponse();
    businessPartner.setMerchantStatus("ACTIVE");
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001"))
      .thenReturn(businessPartner);

    byte[] zipData = "test zip data".getBytes();
    Mockito.when(fileStorageServiceBean.downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP))
      .thenReturn(zipData);

    SystemParameterConfig config = new SystemParameterConfig();
    config.setValue("[{\"platform\":\"shopee\",\"sheetConfigs\":[]}]");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES)).thenReturn(config);

    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);

    Mockito.when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenThrow(new com.gdn.mta.bulk.ValidationException("VALIDATION", "Validation error"));

    externalProductCreationServiceBean.process(request);

    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(Constant.STORE_ID, "TEST_CODE",
        BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001");
    Mockito.verify(fileStorageServiceBean).downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES);
    Mockito.verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    Mockito.verify(notificationService)
      .sendBulkUploadedNotificationWithoutErrorSheet(any(), anyString());
    Mockito.verify(notificationService, Mockito.never())
      .sendBulkUploadedNotification(any(), anyString(), anyString());
    Mockito.verify(bulkProcessService, Mockito.times(2))
      .saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processTestValidationExceptionWithNewNotificationDisabled() throws Exception {
    ReflectionTestUtils.setField(externalProductCreationServiceBean,
      "newNotificationForHeaderValidationError", false);
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    request.setStoreId(Constant.STORE_ID);
    request.setBulkProcessCode("TEST_CODE");
    request.setZipFileName("test.zip");

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBusinessPartnerCode("BP001");
    bulkProcess.setCreatedDate(new Date());

    Mockito.when(
      bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        Constant.STORE_ID, "TEST_CODE", BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);

    ProfileResponse businessPartner = new ProfileResponse();
    businessPartner.setMerchantStatus("ACTIVE");
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001"))
      .thenReturn(businessPartner);

    byte[] zipData = "test zip data".getBytes();
    Mockito.when(fileStorageServiceBean.downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP))
      .thenReturn(zipData);

    SystemParameterConfig config = new SystemParameterConfig();
    config.setValue("[{\"platform\":\"shopee\",\"sheetConfigs\":[]}]");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES)).thenReturn(config);

    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);

    Mockito.when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenThrow(new com.gdn.mta.bulk.ValidationException("VALIDATION", "Validation error"));

    externalProductCreationServiceBean.process(request);

    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(Constant.STORE_ID, "TEST_CODE",
        BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001");
    Mockito.verify(fileStorageServiceBean).downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES);
    Mockito.verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    Mockito.verify(notificationService, Mockito.never())
      .sendBulkUploadedNotificationWithoutErrorSheet(any(), anyString());
    Mockito.verify(notificationService)
      .sendBulkUploadedNotification(any(), anyString(), anyString());
    Mockito.verify(bulkProcessService, Mockito.times(2))
      .saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processTestGeneralExceptionDoesNotSendNotification() throws Exception {
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    request.setStoreId(Constant.STORE_ID);
    request.setBulkProcessCode("TEST_CODE");
    request.setZipFileName("test.zip");

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBusinessPartnerCode("BP001");
    bulkProcess.setCreatedDate(new Date());

    Mockito.when(
      bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        Constant.STORE_ID, "TEST_CODE", BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);

    ProfileResponse businessPartner = new ProfileResponse();
    businessPartner.setMerchantStatus("ACTIVE");
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001"))
      .thenReturn(businessPartner);

    byte[] zipData = "test zip data".getBytes();
    Mockito.when(fileStorageServiceBean.downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP))
      .thenReturn(zipData);

    SystemParameterConfig config = new SystemParameterConfig();
    config.setValue("[{\"platform\":\"shopee\",\"sheetConfigs\":[]}]");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES)).thenReturn(config);

    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);

    Mockito.when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenThrow(new RuntimeException("General error"));

    externalProductCreationServiceBean.process(request);

    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(Constant.STORE_ID, "TEST_CODE",
        BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(Constant.STORE_ID, "BP001");
    Mockito.verify(fileStorageServiceBean).downloadFile(bulkProcess, Constant.FILE_TYPE_ZIP);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.EXTERNAL_MARKETPLACE_SHEET_RULES);
    Mockito.verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    Mockito.verify(notificationService, Mockito.never())
      .sendBulkUploadedNotificationWithoutErrorSheet(any(), anyString());
    Mockito.verify(notificationService, Mockito.never())
      .sendBulkUploadedNotification(any(), anyString(), anyString());
    Mockito.verify(bulkProcessService, Mockito.times(2))
      .saveOperation(Mockito.any(BulkProcess.class));
  }

}
