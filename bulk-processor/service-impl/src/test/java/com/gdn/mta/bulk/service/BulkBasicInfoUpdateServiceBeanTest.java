package com.gdn.mta.bulk.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.argThat;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.config.KafkaPublisher;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkBasicInfoRequest;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.dto.product.BulkBasicInfoVideoDownloadResponseModel;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessImage;
import com.gdn.mta.bulk.entity.BulkProcessVideo;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.ImageDownloadResult;
import com.gdn.mta.bulk.models.ProductBasicDetail;
import com.gdn.mta.bulk.models.download.responsedata.ProductBasicResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductImageAndVideoResponse;
import com.gdn.mta.bulk.models.ProductL3Response;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.request.BulkBasicInfoUpdateRequest;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.ExcelHeaderNames;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.PageInfo;
import com.google.api.services.youtube.model.VideoListResponse;

@ExtendWith(MockitoExtension.class)
class BulkBasicInfoUpdateServiceBeanTest {

  public static final String REGEX = "(?<=watch\\?v=|/videos/|embed\\/|youtu"
      + ".be\\/|\\/v\\/|\\/e\\/|watch\\?v%3D|watch\\?feature=player_embedded&v=|%2Fvideos%2F"
      + "|embed%\u200C\u200B2F|youtu.be%2F|%2Fv%2F|shorts\\/)[^#\\&\\?\\n]*";
  @Mock
  private KafkaPublisher kafkaPublisher;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private BulkProcessRepository bulkProcessRepository;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private BulkProcessImageService bulkProcessImageService;

  @Mock
  private BulkProcessVideoService bulkProcessVideoService;

  @Mock
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private YouTube youTube;

  @Mock
  private YouTube.Videos videos;

  @Mock
  private YouTube.Videos.List list;

  private VideoListResponse videoListResponse;

  @InjectMocks
  private BulkBasicInfoUpdateServiceBean service;

  @Mock
  private NotificationService notificationService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private PCBOutboundService pcbOutboundService;

  @Mock
  private FileStorageServiceBean fileStorageServiceBean;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private ProductLevel3BulkUpdateServiceBean productLevel3BulkUpdateServiceBean;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private PBPOutboundService pbpOutboundService;

  private BulkBasicInfoRequest request;
  private static final String STORE_ID = "10001";
  private static final String REQUEST_ID = "req123";
  private static final String FILE_NAME = "test.csv";
  private static final String BUSINESS_PARTNER_CODE = "BP123";
  private static final String PRIORITY_1_TOPIC = "priority1Topic";
  private static final String PRIORITY_2_TOPIC = "priority2Topic";
  private static final String BULK_PROCESS_CODE = "BPC-123";
  private static final List<Integer> ROW_NUMBERS = Arrays.asList(1, 2);
  private static final String VIDEO_LIST = "snippet,contentDetails,statistics";
  private static final String HEADER_VALIDATION_ERROR =
      "File tidak valid. Silakan download template baru dan coba lagi.";
  private static final String VIDEO_URL = "VIDEO URL";
  private static final String VIDEO_URL_2 = "VIDEO URL 2";
  private static final String PRODUCT_SKU = "Product Sku";
  private BulkBasicInfoRequest bulkBasicInfoRequest;
  private BulkProcess bulkProcess;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.initMocks(this);
    request = new BulkBasicInfoRequest();
    request.setFileName(FILE_NAME);
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkBasicInfoRequest = new BulkBasicInfoRequest();
    bulkBasicInfoRequest.setStoreId(STORE_ID);
    bulkBasicInfoRequest.setBulkProcessCode(STORE_ID);
    bulkBasicInfoRequest.setInstoreSeller(true);
    ReflectionTestUtils.setField(service, "updateProductBasicInfoDownloadLink", REQUEST_ID);
    videoListResponse = new VideoListResponse();
    PageInfo pageInfo = new PageInfo();
    pageInfo.setTotalResults(2);
    videoListResponse.setPageInfo(pageInfo);
    productLevel3BulkUpdateServiceBean.setBusinessPartnerRepository(businessPartnerRepository);
    ReflectionTestUtils.setField(service, "youTubeDataApiKey", "apiKey");
    ReflectionTestUtils.setField(service, "videoStaticBaseUrlPrefix", "dsgshsaggrh");
    ReflectionTestUtils.setField(service, "imageStaticBaseUrlPrefix", "dsgshsaggrh");
    bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_BASIC_INFO.getValue());
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(kafkaTopicProperties);
    verifyNoMoreInteractions(kafkaPublisher);
    verifyNoMoreInteractions(bulkProcessRepository);
  }


  @Test
  void whenProcessingTrustedSeller_shouldPublishToPriority1Queue() {
    request.setTrustedSeller(true);
    when(kafkaTopicProperties.getBulkBasicInfoUploadPriority1Event()).thenReturn(PRIORITY_1_TOPIC);
    service.preProcessBulkBasicInfoUpdate(STORE_ID, REQUEST_ID, request);
    verify(bulkProcessRepository).save(any(BulkProcess.class));
    verify(kafkaPublisher).send(PRIORITY_1_TOPIC, request);
    verify(kafkaTopicProperties).getBulkBasicInfoUploadPriority1Event();
    verifyNoMoreInteractions(bulkProcessRepository);
    verifyNoMoreInteractions(kafkaPublisher);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  void whenProcessingNonTrustedSeller_shouldPublishToPriority2Queue() {
    request.setTrustedSeller(false);
    when(kafkaTopicProperties.getBulkBasicInfoUploadPriority2Event()).thenReturn(PRIORITY_2_TOPIC);
    service.preProcessBulkBasicInfoUpdate(STORE_ID, REQUEST_ID, request);
    verify(bulkProcessRepository).save(any(BulkProcess.class));
    verify(kafkaPublisher).send(PRIORITY_2_TOPIC, request);
    verify(kafkaTopicProperties).getBulkBasicInfoUploadPriority2Event();
    verifyNoMoreInteractions(bulkProcessRepository);
    verifyNoMoreInteractions(kafkaPublisher);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  void whenBulkProcessSaveFails_shouldThrowException() {
    doThrow(new RuntimeException("Database error")).when(bulkProcessRepository).save(any(BulkProcess.class));
    assertThrows(RuntimeException.class, () -> service.preProcessBulkBasicInfoUpdate(STORE_ID, REQUEST_ID, request));
    verify(bulkProcessRepository).save(any(BulkProcess.class));
    verifyNoInteractions(kafkaPublisher);
    verifyNoInteractions(kafkaTopicProperties);
    verifyNoMoreInteractions(bulkProcessRepository);
  }

  @Test
  void whenKafkaPublishFails_shouldThrowException() {
    request.setTrustedSeller(true);
    when(kafkaTopicProperties.getBulkBasicInfoUploadPriority1Event()).thenReturn(PRIORITY_1_TOPIC);
    doThrow(new RuntimeException("Kafka error")).when(kafkaPublisher).send(anyString(), any(BulkBasicInfoRequest.class));
    assertThrows(RuntimeException.class, () -> service.preProcessBulkBasicInfoUpdate(STORE_ID, REQUEST_ID, request));
    verify(bulkProcessRepository).save(any(BulkProcess.class));
    verify(kafkaPublisher).send(PRIORITY_1_TOPIC, request);
    verify(kafkaTopicProperties).getBulkBasicInfoUploadPriority1Event();
    verifyNoMoreInteractions(bulkProcessRepository);
    verifyNoMoreInteractions(kafkaPublisher);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  void whenNullRequestProvided_shouldThrowException() {
    assertThrows(NullPointerException.class, () -> service.preProcessBulkBasicInfoUpdate(STORE_ID, REQUEST_ID, null));
    verifyNoInteractions(bulkProcessRepository);
    verifyNoInteractions(kafkaPublisher);
    verifyNoInteractions(kafkaTopicProperties);
  }

  @Test
  void whenProcessingRequest_shouldSetCorrectBulkProcessFields() {
    request.setTrustedSeller(true);
    when(kafkaTopicProperties.getBulkBasicInfoUploadPriority1Event()).thenReturn(PRIORITY_1_TOPIC);
    service.preProcessBulkBasicInfoUpdate(STORE_ID, REQUEST_ID, request);
    verify(bulkProcessRepository).save(any(BulkProcess.class));
    verify(kafkaPublisher).send(PRIORITY_1_TOPIC, request);
    verify(kafkaTopicProperties).getBulkBasicInfoUploadPriority1Event();
    verifyNoMoreInteractions(bulkProcessRepository);
    verifyNoMoreInteractions(kafkaPublisher);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void validateBulkUpdateRequest_WhenEmptyHeaders_ThrowsException() throws IOException {
    // Setup request
    BulkBasicInfoRequest request = new BulkBasicInfoRequest();
    request.setStoreId("store1");
    request.setBulkProcessCode("code1");
    request.setBusinessPartnerCode("partner1");

    // Create actual workbook and sheet
    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Test Sheet");
    Row headerRow = sheet.createRow(0); // Create header row at index 0

    // Setup mocks
    BulkProcess bulkProcess = new BulkProcess();
    when(fileStorageServiceBean.getFileDataForBasicInfo(any(), any())).thenReturn(sheet);

    // Test and verify
    ApplicationRuntimeException exception =
        assertThrows(ApplicationRuntimeException.class, () -> service.validateBulkUpdateRequest(request, bulkProcess));

    verify(fileStorageServiceBean).getFileDataForBasicInfo(request, bulkProcess);
  }

  @Test
  public void validateBulkUpdateRequestNullTest() throws IOException {
    // Setup request
    BulkBasicInfoRequest request = new BulkBasicInfoRequest();
    request.setStoreId("store1");
    request.setBulkProcessCode("code1");
    request.setBusinessPartnerCode("partner1");
    // Create actual workbook and sheet
    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Test Sheet");
    // Test and verify
    ApplicationRuntimeException exception =
        assertThrows(ApplicationRuntimeException.class, () -> service.validateBulkUpdateRequest(request, null));
  }

  @Test
  public void validateBulkUpdateRequest_WhenInvalidHeaders_ThrowsException() throws IOException {
    // Setup request
    BulkBasicInfoRequest request = new BulkBasicInfoRequest();
    request.setStoreId("store1");
    request.setBulkProcessCode("code1");
    request.setInstoreSeller(false);

    // Create actual workbook and sheet with header
    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Test Sheet");
    Row headerRow = sheet.createRow(0);
    Cell headerCell = headerRow.createCell(0);
    headerCell.setCellValue("InvalidHeader");

    // Setup mocks
    BulkProcess bulkProcess = new BulkProcess();
    when(fileStorageServiceBean.getFileDataForBasicInfo(any(), any())).thenReturn(sheet);

    // Test and verify
    ApplicationRuntimeException exception =
        assertThrows(ApplicationRuntimeException.class, () -> service.validateBulkUpdateRequest(request, bulkProcess));

    verify(fileStorageServiceBean).getFileDataForBasicInfo(request, bulkProcess);
  }

  @Test
  void processBulkUpdate_Success() throws IOException {
    BulkBasicInfoRequest request = new BulkBasicInfoRequest();
    request.setStoreId("store1");
    request.setBulkProcessCode("code1");
    request.setInstoreSeller(false);

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);

    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Test");

    Row headerRow = sheet.createRow(0);
    List<String> headers = BulkParameters.BASIC_INFO_BASE_HEADER_LIST;

    for (int i = 0; i < headers.size(); i++) {
      Cell cell = headerRow.createCell(i);
      cell.setCellValue(headers.get(i));
    }
    String[][] sampleData =
        {{"SKU123", "Product1", "Brand1", "Category1", "Description1", "main1.jpg", "photo2.jpg", "photo3.jpg",
            "photo4.jpg", "photo5.jpg", "photo6.jpg", "photo7.jpg", "photo8.jpg", "video1.mp4", "REGULAR", "10", "20",
            "30", "1", "1.5", "size1.jpg", "1.0"},
            {"SKU456", "Product2", "Brand2", "Category2", "Description2", "main2.jpg", "photo2.jpg", "photo3.jpg",
                "photo4.jpg", "photo5.jpg", "photo6.jpg", "photo7.jpg", "photo8.jpg", "video2.mp4", "REGULAR", "15",
                "25", "35", "2", "2.5", "size2.jpg", "1.5"},
            {"SKU789", "Product3", "Brand3", "Category3", "Description3", "main3.jpg", "photo2.jpg", "photo3.jpg",
                "photo4.jpg", "photo5.jpg", "photo6.jpg", "photo7.jpg", "photo8.jpg", "video3.mp4", "REGULAR", "12",
                "22", "32", "1.2", "1.7", "size3.jpg", "1.2"},
            {"SKU101", "Product4", "Brand4", "Category4", "Description4", "main4.jpg", "photo2.jpg", "photo3.jpg",
                "photo4.jpg", "photo5.jpg", "photo6.jpg", "photo7.jpg", "photo8.jpg", "video4.mp4", "REGULAR", "18",
                "28", "38", "1.8", "2.3", "size4.jpg", "1.8"},
            {"SKU102", "Product5", "Brand5", "Category5", "Description5", "main5.jpg", "photo2.jpg", "photo3.jpg",
                "photo4.jpg", "photo5.jpg", "photo6.jpg", "photo7.jpg", "photo8.jpg", "video5.mp4", "REGULAR", "14",
                "24", "34", "1.4", "1.9", "size5.jpg", "1.4"}};

    for (int i = 0; i < sampleData.length; i++) {
      Row dataRow = sheet.createRow(i + 1);
      for (int j = 0; j < sampleData[i].length; j++) {
        dataRow.createCell(j).setCellValue(sampleData[i][j]);
      }
    }

    SystemParameterConfig maxSizeConfig = new SystemParameterConfig();
    maxSizeConfig.setValue("100");

    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(anyString(), anyString(),
        eq(BulkProcess.STATUS_PENDING))).thenReturn(bulkProcess);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(fileStorageServiceBean.getFileDataForBasicInfo(any(), any())).thenReturn(sheet);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(anyString(),
        eq(SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_BASIC_INFO_MAXIMUM_SIZE))).thenReturn(maxSizeConfig);

    service.processBulkUpdate(request);

    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(anyString(),
        anyString(), eq(BulkProcess.STATUS_PENDING));
    verify(bulkProcessRepository, times(1)).save(any(BulkProcess.class));
    verify(fileStorageServiceBean).getFileDataForBasicInfo(eq(request), any());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(anyString(),
        eq(SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_BASIC_INFO_MAXIMUM_SIZE));
    assertEquals(2, bulkProcess.getTotalCount());
    assertNotNull(bulkProcess.getStartDate());
  }

  @Test
  void processBulkUpdate_Fail_MaxRows() throws IOException {
    BulkBasicInfoRequest request = new BulkBasicInfoRequest();
    request.setStoreId("store1");
    request.setBulkProcessCode("code1");
    request.setInstoreSeller(false);

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);

    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Test");

    Row headerRow = sheet.createRow(0);
    List<String> headers = BulkParameters.BASIC_INFO_BASE_HEADER_LIST;

    for (int i = 0; i < headers.size(); i++) {
      Cell cell = headerRow.createCell(i);
      cell.setCellValue(headers.get(i));
    }
    String[][] sampleData =
        {{"SKU123", "Product1", "Brand1", "Category1", "Description1", "main1.jpg", "photo2.jpg",
            "photo3.jpg", "photo4.jpg", "photo5.jpg", "photo6.jpg", "photo7.jpg", "photo8.jpg",
            "video1.mp4", "REGULAR", "10", "20", "30", "1", "1.5", "size1.jpg", "1.0"},
            {"SKU456", "Product2", "Brand2", "Category2", "Description2", "main2.jpg", "photo2.jpg",
                "photo3.jpg", "photo4.jpg", "photo5.jpg", "photo6.jpg", "photo7.jpg", "photo8.jpg",
                "video2.mp4", "REGULAR", "15", "25", "35", "2", "2.5", "size2.jpg", "1.5"},
            {"SKU789", "Product3", "Brand3", "Category3", "Description3", "main3.jpg", "photo2.jpg",
                "photo3.jpg", "photo4.jpg", "photo5.jpg", "photo6.jpg", "photo7.jpg", "photo8.jpg",
                "video3.mp4", "REGULAR", "12", "22", "32", "1.2", "1.7", "size3.jpg", "1.2"},
            {"SKU101", "Product4", "Brand4", "Category4", "Description4", "main4.jpg", "photo2.jpg",
                "photo3.jpg", "photo4.jpg", "photo5.jpg", "photo6.jpg", "photo7.jpg", "photo8.jpg",
                "video4.mp4", "REGULAR", "18", "28", "38", "1.8", "2.3", "size4.jpg", "1.8"},
            {"SKU102", "Product5", "Brand5", "Category5", "Description5", "main5.jpg", "photo2.jpg",
                "photo3.jpg", "photo4.jpg", "photo5.jpg", "photo6.jpg", "photo7.jpg", "photo8.jpg",
                "video5.mp4", "REGULAR", "14", "24", "34", "1.4", "1.9", "size5.jpg", "1.4"}};

    for (int i = 0; i < sampleData.length; i++) {
      Row dataRow = sheet.createRow(i + 1);
      for (int j = 0; j < sampleData[i].length; j++) {
        dataRow.createCell(j).setCellValue(sampleData[i][j]);
      }
    }

    SystemParameterConfig maxSizeConfig = new SystemParameterConfig();
    maxSizeConfig.setValue("1");

    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
        anyString(), anyString(), eq(BulkProcess.STATUS_PENDING))).thenReturn(bulkProcess);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(fileStorageServiceBean.getFileDataForBasicInfo(any(), any())).thenReturn(sheet);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(anyString(),
        eq(SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_BASIC_INFO_MAXIMUM_SIZE))).thenReturn(
        maxSizeConfig);
    try {
      service.processBulkUpdate(request);
    } finally {
      verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
          anyString(), anyString(), eq(BulkProcess.STATUS_PENDING));
      verify(bulkProcessRepository, times(1)).save(any(BulkProcess.class));
      verify(fileStorageServiceBean).getFileDataForBasicInfo(eq(request), any());
      verify(systemParameterConfigService).findValueByStoreIdAndVariable(anyString(),
          eq(SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_BASIC_INFO_MAXIMUM_SIZE));
    }
  }

  @Test
  void processBulkUpdate_SuccessEmpty() throws IOException {
    // Setup request
    BulkBasicInfoRequest request = new BulkBasicInfoRequest();
    request.setStoreId("store1");
    request.setBulkProcessCode("code1");
    request.setInstoreSeller(false);

    // Create bulk process
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);

    // Create actual workbook with test data
    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Test");

    // Create headers row
    Row headerRow = sheet.createRow(0);
    List<String> headers = BulkParameters.BASIC_INFO_BASE_HEADER_LIST;

    // Add each header to the row
    for (int i = 0; i < headers.size(); i++) {
      Cell cell = headerRow.createCell(i);
      cell.setCellValue(headers.get(i));
    }

    // Create sample data row
    Row dataRow = sheet.createRow(4);
    String[] sampleData = {"SKU123", // BLIBLI_PRODUCT_SKU_HEADER
        "Product Parent", // PARENT_PRODUCT_NAME_HEADER
        "Brand1", // BRAND_HEADER
        "Category1", // CATEGORY_HEADER
        "Description", // DESCRIPTIONS
        "main.jpg", // MAIN_PHOTO
        "photo2.jpg", // COMMON_PHOTO_2
        "photo3.jpg", // COMMON_PHOTO_3
        "photo4.jpg", // COMMON_PHOTO_4
        "photo5.jpg", // COMMON_PHOTO_5
        "photo6.jpg", // COMMON_PHOTO_6
        "photo7.jpg", // COMMON_PHOTO_7
        "photo8.jpg", // COMMON_PHOTO_8
        "video.mp4", // VIDEO_URL
        "REGULAR", // SHIPPING_TYPE
        "10", // LENGTH_HEADER
        "20", // WIDTH_HEADER
        "30", // HEIGHT_HEADER
        "1", // ACTUAL_WEIGHT
        "1.5", // SHIPPING_WEIGHT
        "size.jpg", // SIZE_CHART
        "1.0" // VOLUME_FACTOR
    };

    for (int i = 0; i < sampleData.length; i++) {
      Cell cell = dataRow.createCell(i);
      cell.setCellValue(sampleData[i]);
    }

    // Setup mocks
    SystemParameterConfig maxSizeConfig = new SystemParameterConfig();
    maxSizeConfig.setValue("1");

    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(anyString(), anyString(),
        eq(BulkProcess.STATUS_PENDING))).thenReturn(bulkProcess);
    when(bulkProcessRepository.save(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(fileStorageServiceBean.getFileDataForBasicInfo(any(), any())).thenReturn(sheet);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(anyString(),
        eq(SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_BASIC_INFO_MAXIMUM_SIZE))).thenReturn(maxSizeConfig);

    // Execute
    service.processBulkUpdate(request);

    // Verify
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(anyString(),
        anyString(), eq(BulkProcess.STATUS_PENDING));
    verify(bulkProcessRepository, times(1)).save(any(BulkProcess.class));
    verify(fileStorageServiceBean).getFileDataForBasicInfo(eq(request), any());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(anyString(),
        eq(SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_BASIC_INFO_MAXIMUM_SIZE));
    assertNotNull(bulkProcess.getStartDate());
  }

  @Test
  void setErrorMessageAndGetDownloadLink_AllConditions() {
    // Setup
    BulkProcess bulkProcess = new BulkProcess();
    String downloadLinkTemplate = "template-link";
    when(fileStorageServiceBean.getDownloadLinkHtml(REQUEST_ID)).thenReturn("download-link");

    // Test case 1: Header mismatch
    String result1 = service.setErrorMessageAndGetDownloadLink(bulkProcess, BulkUpdateServiceUtil.HEADER_MISMATCH,
        StringUtils.EMPTY);
    assertEquals("download-link", result1);
    assertEquals(HEADER_VALIDATION_ERROR, bulkProcess.getDescription());
    verify(fileStorageServiceBean).getDownloadLinkHtml(REQUEST_ID);

    // Reset
    bulkProcess.setDescription(null);

    // Test case 2: Maximum row error
    String errorWithCategory = ErrorCategory.VALIDATION.getMessage() + ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN;
    String result2 = service.setErrorMessageAndGetDownloadLink(bulkProcess, errorWithCategory, downloadLinkTemplate);
    assertEquals(downloadLinkTemplate, result2);
    assertEquals(ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN, bulkProcess.getDescription());

    // Reset
    bulkProcess.setDescription(null);

    // Test case 3: File blank error
    errorWithCategory = ErrorCategory.VALIDATION.getMessage() + BulkUpdateServiceUtil.FILE_BLANK_ERROR;
    String result3 = service.setErrorMessageAndGetDownloadLink(bulkProcess, errorWithCategory, downloadLinkTemplate);
    assertEquals(downloadLinkTemplate, result3);
    assertEquals(BulkUpdateServiceUtil.FILE_BLANK_ERROR, bulkProcess.getDescription());

    // Test case 4: Other error message
    String result4 = service.setErrorMessageAndGetDownloadLink(bulkProcess, "Some other error", downloadLinkTemplate);
    assertEquals(downloadLinkTemplate, result4);
    assertEquals(BulkUpdateServiceUtil.FILE_BLANK_ERROR, bulkProcess.getDescription());

    verify(fileStorageServiceBean, times(1)).getDownloadLinkHtml(anyString());
  }

  @Test
  void generateBulkProcessData_AllScenarios() throws IOException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId("store1");
    bulkProcess.setBulkProcessType("type1");
    List<Map<String, String>> productData = new ArrayList<>();
    Map<String, String> product1 = new HashMap<>();
    product1.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU1");
    Map<String, String> product2 = new HashMap<>();
    product2.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU1");
    Map<String, String> product3 = new HashMap<>();
    product3.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU3");
    productData.add(product1);
    productData.add(product2);
    productData.add(product3);
    SystemParameterConfig trustedSellerConfig = new SystemParameterConfig();
    trustedSellerConfig.setValue("10");
    SystemParameterConfig regularMaxConfig = new SystemParameterConfig();
    regularMaxConfig.setValue("50");
    SystemParameterConfig regularMinConfig = new SystemParameterConfig();
    regularMinConfig.setValue("20");
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(trustedSellerConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(regularMaxConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(regularMinConfig);
    ProductBasicResponse productResponse = new ProductBasicResponse();
    productResponse.setB2cActivated(true);
    productResponse.setForceReview(false);
    productResponse.setArchived(false);
    productResponse.setTakenDown(false);
    productResponse.setMarkForDelete(false);
    when(xProductOutboundService.getProductBasicInfo("store1", "SKU3", true)).thenReturn(productResponse);
    when(objectMapper.writeValueAsString(any(Map.class))).thenReturn("{}");
    service.generateBulkProcessData(bulkProcess, productData, bulkBasicInfoRequest);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    verify(xProductOutboundService).getProductBasicInfo("store1", "SKU3", true);
    verify(objectMapper, times(3)).writeValueAsString(any(Map.class));
    verify(bulkProcessDataService).saveBulkProcessData(argThat(list -> list.size() == 3));
    verify(bulkProcessService).saveOperation(bulkProcess);
    assertEquals(BulkProcess.STATUS_PUBLISHED, bulkProcess.getStatus());
  }

  @Test
  void generateBulkProcessData_AllValidScenarios() throws IOException {
    ReflectionTestUtils.setField(service,"validateYoutubeUrlSwitchEn",true);
    ReflectionTestUtils.setField(service, "youtubeRegex", REGEX);
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId("store1");
    bulkProcess.setBulkProcessType("type1");
    List<Map<String, String>> productData = new ArrayList<>();
    Map<String, String> product3 = new HashMap<>();
    product3.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU3");
    product3.put(BulkParameters.PARENT_PRODUCT_NAME_HEADER, "SKU3");
    product3.put(BulkParameters.INSTORE, "1");
    product3.put(BulkParameters.DESCRIPTIONS, PRODUCT_SKU);
    product3.put(BulkParameters.MAIN_PHOTO, PRODUCT_SKU);
    product3.put(BulkParameters.SHIPPING_TYPE, BulkParameters.SHIPPED_BY_BLIBLI);
    product3.put(BulkParameters.VIDEO_URL, "https://www.youtube.com/watch?v=P1xAhgKTqDA");
    productData.add(product3);
    SystemParameterConfig trustedSellerConfig = new SystemParameterConfig();
    trustedSellerConfig.setValue("10");
    SystemParameterConfig imageDownloadConfig = new SystemParameterConfig();
    imageDownloadConfig.setValue("10");
    SystemParameterConfig regularMaxConfig = new SystemParameterConfig();
    regularMaxConfig.setValue("50");
    SystemParameterConfig regularMinConfig = new SystemParameterConfig();
    regularMinConfig.setValue("20");
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(trustedSellerConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.IMAGE_DOWNLOAD_BATCH_SIZE)).thenReturn(imageDownloadConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(regularMaxConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(regularMinConfig);
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    Mockito.when(list.execute()).thenReturn(videoListResponse);
    ProductBasicResponse productResponse = new ProductBasicResponse();
    productResponse.setProductCode(PRODUCT_SKU);
    productResponse.setB2cActivated(false);
    productResponse.setForceReview(false);
    productResponse.setArchived(false);
    productResponse.setTakenDown(false);
    productResponse.setMarkForDelete(false);
    productResponse.setSyncProduct(true);
    when(xProductOutboundService.getProductBasicInfo("store1", "SKU3", true)).thenReturn(productResponse);
    when(objectMapper.writeValueAsString(any(Map.class))).thenReturn("{}");
    ProductImageAndVideoResponse productImageAndVideoResponse = new ProductImageAndVideoResponse();
    when(pcbOutboundService.getBasicInfoFromPCB(List.of(PRODUCT_SKU))).thenReturn(
        List.of(productImageAndVideoResponse));
    try {
      service.generateBulkProcessData(bulkProcess, productData, bulkBasicInfoRequest);
    } catch (Exception e) {
    } finally {
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
      verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID,
          Constant.IMAGE_DOWNLOAD_BATCH_SIZE);
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
      verify(xProductOutboundService).getProductBasicInfo("store1", "SKU3", true);
      verify(objectMapper).writeValueAsString(any(Map.class));
    }
  }

  @Test
  void generateBulkProcessData_InActiveYoutubeLink() throws IOException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId("store1");
    bulkProcess.setBulkProcessType("type1");
    List<Map<String, String>> productData = new ArrayList<>();
    Map<String, String> product3 = new HashMap<>();
    product3.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU3");
    product3.put(BulkParameters.PARENT_PRODUCT_NAME_HEADER, "SKU3");
    product3.put(BulkParameters.INSTORE, "1");
    product3.put(BulkParameters.DESCRIPTIONS, PRODUCT_SKU);
    product3.put(BulkParameters.MAIN_PHOTO, PRODUCT_SKU);
    product3.put(BulkParameters.SHIPPING_TYPE, BulkParameters.SHIPPED_BY_BLIBLI);
    product3.put(BulkParameters.VIDEO_URL,
        "https://storage.googleapis.com/blibli-reels-video-final-bucket/videos/final/BLG-70029"
            + "/5dad8788-bc4d-4158-8826-64994fad14d2"
            +
            "/Nafas_Segar_18_jam_dengan_Pepsodent_Herbal_dari_Sahur_hingga_Buka_Puasa_compressed"
            + ".mp4");
    productData.add(product3);
    SystemParameterConfig trustedSellerConfig = new SystemParameterConfig();
    trustedSellerConfig.setValue("10");
    SystemParameterConfig regularMaxConfig = new SystemParameterConfig();
    regularMaxConfig.setValue("50");
    SystemParameterConfig regularMinConfig = new SystemParameterConfig();
    regularMinConfig.setValue("20");
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(trustedSellerConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(regularMaxConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(regularMinConfig);
    ProductBasicResponse productResponse = new ProductBasicResponse();
    productResponse.setProductCode(PRODUCT_SKU);
    productResponse.setB2cActivated(false);
    productResponse.setForceReview(false);
    productResponse.setArchived(false);
    productResponse.setTakenDown(false);
    productResponse.setMarkForDelete(false);
    when(xProductOutboundService.getProductBasicInfo("store1", "SKU3", true)).thenReturn(productResponse);
    when(objectMapper.writeValueAsString(any(Map.class))).thenReturn("{}");
    ProductImageAndVideoResponse productImageAndVideoResponse = new ProductImageAndVideoResponse();
    when(pcbOutboundService.getBasicInfoFromPCB(List.of(PRODUCT_SKU))).thenReturn(
        List.of(productImageAndVideoResponse));
    try {
      service.generateBulkProcessData(bulkProcess, productData, bulkBasicInfoRequest);
    } catch (Exception e) {
    } finally {
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
      verify(xProductOutboundService).getProductBasicInfo("store1", "SKU3", true);
      verify(objectMapper).writeValueAsString(any(Map.class));
      assertEquals(BulkProcess.STATUS_PUBLISHED, bulkProcess.getStatus());
    }
  }

  @Test
  void generateBulkProcessData_AllValidScenarios_Without_Video() throws IOException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId("store1");
    bulkProcess.setBulkProcessType("type1");
    List<Map<String, String>> productData = new ArrayList<>();
    Map<String, String> product3 = new HashMap<>();
    product3.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU3");
    product3.put(BulkParameters.PARENT_PRODUCT_NAME_HEADER, "SKU3");
    product3.put(BulkParameters.INSTORE, "1");
    product3.put(BulkParameters.DESCRIPTIONS, PRODUCT_SKU);
    product3.put(BulkParameters.MAIN_PHOTO, PRODUCT_SKU);
    product3.put(BulkParameters.SHIPPING_TYPE, BulkParameters.SHIPPED_BY_BLIBLI);
    productData.add(product3);
    SystemParameterConfig trustedSellerConfig = new SystemParameterConfig();
    trustedSellerConfig.setValue("10");
    SystemParameterConfig regularMaxConfig = new SystemParameterConfig();
    regularMaxConfig.setValue("50");
    SystemParameterConfig regularMinConfig = new SystemParameterConfig();
    regularMinConfig.setValue("20");
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(trustedSellerConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(regularMaxConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(regularMinConfig);
    ProductBasicResponse productResponse = new ProductBasicResponse();
    productResponse.setProductCode(PRODUCT_SKU);
    productResponse.setB2cActivated(false);
    productResponse.setForceReview(false);
    productResponse.setArchived(false);
    productResponse.setTakenDown(false);
    productResponse.setMarkForDelete(false);
    productResponse.setSyncProduct(true);
    when(xProductOutboundService.getProductBasicInfo("store1", "SKU3", true)).thenReturn(productResponse);
    when(objectMapper.writeValueAsString(any(Map.class))).thenReturn("{}");
    ProductImageAndVideoResponse productImageAndVideoResponse = new ProductImageAndVideoResponse();
    when(pcbOutboundService.getBasicInfoFromPCB(List.of(PRODUCT_SKU))).thenReturn(
        List.of(productImageAndVideoResponse));
    try {
      service.generateBulkProcessData(bulkProcess, productData, bulkBasicInfoRequest);
    } catch (Exception e) {
    } finally {
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
      verify(xProductOutboundService).getProductBasicInfo("store1", "SKU3", true);
      verify(objectMapper).writeValueAsString(any(Map.class));
      assertEquals(BulkProcess.STATUS_READY_TO_PROCESS, bulkProcess.getStatus());
    }
  }

  @Test
  void generateBulkProcessData_InValidScenarios() throws JsonProcessingException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId("store1");
    bulkProcess.setBulkProcessType("type1");
    List<Map<String, String>> productData = new ArrayList<>();
    Map<String, String> product3 = new HashMap<>();
    product3.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU3");
    product3.put(BulkParameters.PARENT_PRODUCT_NAME_HEADER, "SKU3");
    product3.put(BulkParameters.INSTORE, PRODUCT_SKU);
    product3.put(BulkParameters.DESCRIPTIONS, PRODUCT_SKU);
    product3.put(BulkParameters.MAIN_PHOTO, PRODUCT_SKU);
    product3.put(BulkParameters.SHIPPING_TYPE, BulkParameters.SHIPPED_BY_BLIBLI);
    product3.put(BulkParameters.VIDEO_URL, VIDEO_URL);
    productData.add(product3);
    SystemParameterConfig trustedSellerConfig = new SystemParameterConfig();
    trustedSellerConfig.setValue("10");
    SystemParameterConfig regularMaxConfig = new SystemParameterConfig();
    regularMaxConfig.setValue("50");
    SystemParameterConfig regularMinConfig = new SystemParameterConfig();
    regularMinConfig.setValue("20");
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(trustedSellerConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(regularMaxConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(regularMinConfig);
    ProductBasicResponse productResponse = new ProductBasicResponse();
    productResponse.setProductCode(PRODUCT_SKU);
    productResponse.setB2cActivated(false);
    productResponse.setForceReview(false);
    productResponse.setArchived(false);
    productResponse.setTakenDown(false);
    productResponse.setMarkForDelete(false);
    when(xProductOutboundService.getProductBasicInfo("store1", "SKU3", true)).thenReturn(productResponse);
    when(objectMapper.writeValueAsString(any(Map.class))).thenReturn("{}");
    ProductImageAndVideoResponse productImageAndVideoResponse = new ProductImageAndVideoResponse();
    when(pcbOutboundService.getBasicInfoFromPCB(List.of(PRODUCT_SKU))).thenReturn(
        List.of(productImageAndVideoResponse));
    bulkBasicInfoRequest.setProductVideoActivated(true);
    try {
      service.generateBulkProcessData(bulkProcess, productData, bulkBasicInfoRequest);
    } catch (Exception e) {
    } finally {
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
      verify(xProductOutboundService).getProductBasicInfo("store1", "SKU3", true);
      verify(objectMapper).writeValueAsString(any(Map.class));
    }
  }

  @Test
  void validateVideoUrlForYoutubeAndVideoTest() throws IOException {
    ReflectionTestUtils.setField(service, "validateYoutubeUrlSwitchEn", true);
    ReflectionTestUtils.setField(service, "youtubeRegex", REGEX);
    videoListResponse = new VideoListResponse();
    PageInfo pageInfo = new PageInfo();
    pageInfo.setTotalResults(0);
    videoListResponse.setPageInfo(pageInfo);
    when(youTube.videos()).thenReturn(videos);
    when(videos.list(VIDEO_LIST)).thenReturn(list);
    when(list.execute()).thenReturn(videoListResponse);
    Map<String, String> userData = new HashMap<>();
    userData.put(BulkParameters.VIDEO_URL, "https://www.youtube.com/watch?v=Pavan_Sai");
    Assertions.assertEquals(BulkProcessValidationErrorMessages.INVALID_YOUTUBE_URL,
        service.validateVideoUrlForYoutubeAndVideo(new BulkBasicInfoRequest(), userData,
            new ProductImageAndVideoResponse()));
  }

  @Test
  void validateVideoUrlForYoutubeAndVideoSameTest() throws IOException {
    ReflectionTestUtils.setField(service, "validateYoutubeUrlSwitchEn", true);
    ReflectionTestUtils.setField(service, "youtubeRegex", REGEX);
    videoListResponse = new VideoListResponse();
    PageInfo pageInfo = new PageInfo();
    pageInfo.setTotalResults(0);
    videoListResponse.setPageInfo(pageInfo);
    ProductImageAndVideoResponse productImageAndVideoResponse = new ProductImageAndVideoResponse();
    productImageAndVideoResponse.setUrl("https://www.youtube.com/watch?v=Pavan_Sai");
    Map<String, String> userData = new HashMap<>();
    userData.put(BulkParameters.VIDEO_URL, "https://www.youtube.com/watch?v=Pavan_Sai");
    Assertions.assertEquals("",
        service.validateVideoUrlForYoutubeAndVideo(new BulkBasicInfoRequest(), userData,
            productImageAndVideoResponse));
  }

  @Test
  void validateVideoUrlForYoutubeAndVideoTest1() throws IOException {
    ReflectionTestUtils.setField(service, "validateYoutubeUrlSwitchEn", true);
    ReflectionTestUtils.setField(service, "youtubeRegex", "");
    videoListResponse = new VideoListResponse();
    PageInfo pageInfo = new PageInfo();
    pageInfo.setTotalResults(0);
    videoListResponse.setPageInfo(pageInfo);
    Map<String, String> userData = new HashMap<>();
    userData.put(BulkParameters.VIDEO_URL, "https://www.youtube.com/watch?,v=Pavan_,Sai");
    ProductImageAndVideoResponse productImageAndVideoResponse = new ProductImageAndVideoResponse();
    Assertions.assertEquals("Link video harus merupakan youtube URL",
        service.validateVideoUrlForYoutubeAndVideo(new BulkBasicInfoRequest(), userData,
            productImageAndVideoResponse));
  }

  @Test
  void validateVideoUrlForYoutubeAndVideoEligibleTest1() throws IOException {
    ReflectionTestUtils.setField(service, "validateYoutubeUrlSwitchEn", true);
    ReflectionTestUtils.setField(service, "youtubeRegex", "");
    videoListResponse = new VideoListResponse();
    PageInfo pageInfo = new PageInfo();
    pageInfo.setTotalResults(0);
    videoListResponse.setPageInfo(pageInfo);
    Map<String, String> userData = new HashMap<>();
    BulkBasicInfoRequest bulkBasicInfoRequest1 = new BulkBasicInfoRequest();
    bulkBasicInfoRequest1.setProductVideoActivated(true);
    userData.put(BulkParameters.VIDEO_URL, "https://www.youtube.com/watch?,v=Pavan_,Sai");
    ProductImageAndVideoResponse productImageAndVideoResponse = new ProductImageAndVideoResponse();
    Assertions.assertEquals("",
        service.validateVideoUrlForYoutubeAndVideo(bulkBasicInfoRequest1, userData,
            productImageAndVideoResponse));
  }

  @Test
  void generateBulkProcessData_forceReviewScenarios() throws JsonProcessingException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId("store1");
    bulkProcess.setBulkProcessType("type1");
    List<Map<String, String>> productData = new ArrayList<>();
    Map<String, String> product3 = new HashMap<>();
    product3.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU3");
    product3.put(BulkParameters.PARENT_PRODUCT_NAME_HEADER, "SKU3");
    product3.put(BulkParameters.INSTORE, PRODUCT_SKU);
    product3.put(BulkParameters.DESCRIPTIONS, PRODUCT_SKU);
    product3.put(BulkParameters.MAIN_PHOTO, PRODUCT_SKU);
    product3.put(BulkParameters.SHIPPING_TYPE, BulkParameters.SHIPPED_BY_BLIBLI);
    productData.add(product3);
    SystemParameterConfig trustedSellerConfig = new SystemParameterConfig();
    trustedSellerConfig.setValue("10");
    SystemParameterConfig regularMaxConfig = new SystemParameterConfig();
    regularMaxConfig.setValue("50");
    SystemParameterConfig regularMinConfig = new SystemParameterConfig();
    regularMinConfig.setValue("20");
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(trustedSellerConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(regularMaxConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(regularMinConfig);
    ProductBasicResponse productResponse = new ProductBasicResponse();
    productResponse.setProductCode(PRODUCT_SKU);
    productResponse.setB2cActivated(false);
    productResponse.setForceReview(true);
    productResponse.setArchived(false);
    productResponse.setTakenDown(false);
    productResponse.setMarkForDelete(false);
    when(xProductOutboundService.getProductBasicInfo("store1", "SKU3", true)).thenReturn(productResponse);
    when(objectMapper.writeValueAsString(any(Map.class))).thenReturn("{}");
    ProductImageAndVideoResponse productImageAndVideoResponse = new ProductImageAndVideoResponse();
    when(pcbOutboundService.getBasicInfoFromPCB(List.of(PRODUCT_SKU))).thenReturn(
        List.of(productImageAndVideoResponse));
    try {
      service.generateBulkProcessData(bulkProcess, productData, bulkBasicInfoRequest);
    } catch (Exception e) {
    } finally {
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
      verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
          SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
      verify(xProductOutboundService).getProductBasicInfo("store1", "SKU3", true);
      verify(objectMapper).writeValueAsString(any(Map.class));
      assertEquals(BulkProcess.STATUS_PUBLISHED, bulkProcess.getStatus());
    }
  }

  @Test
  void generateBulkProcessData_AllScenariosException() throws IOException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId("store1");
    bulkProcess.setBulkProcessType("type1");
    List<Map<String, String>> productData = new ArrayList<>();
    Map<String, String> product1 = new HashMap<>();
    product1.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU1");
    Map<String, String> product2 = new HashMap<>();
    product2.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU1");
    Map<String, String> product3 = new HashMap<>();
    product3.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU3");
    productData.add(product1);
    productData.add(product2);
    productData.add(product3);
    SystemParameterConfig trustedSellerConfig = new SystemParameterConfig();
    trustedSellerConfig.setValue("10");
    SystemParameterConfig regularMaxConfig = new SystemParameterConfig();
    regularMaxConfig.setValue("50");
    SystemParameterConfig regularMinConfig = new SystemParameterConfig();
    regularMinConfig.setValue("20");
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(trustedSellerConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(regularMaxConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(regularMinConfig);
    BasicProductResponse productResponse = new BasicProductResponse();
    productResponse.setB2cActivated(true);
    productResponse.setForceReview(false);
    productResponse.setArchived(false);
    productResponse.setTakenDown(false);
    productResponse.setMarkForDelete(false);
    when(xProductOutboundService.getProductBasicInfo("store1", "SKU3", true)).thenThrow(ApplicationRuntimeException.class);
    when(objectMapper.writeValueAsString(any(Map.class))).thenReturn("{}");
    service.generateBulkProcessData(bulkProcess, productData, bulkBasicInfoRequest);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MAX_ROW_SIZE);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable("store1",
        SystemParameterConfigNames.UPDATE_REGULAR_SELLER_MIN_ROW_SIZE);
    verify(xProductOutboundService).getProductBasicInfo("store1", "SKU3", true);
    verify(objectMapper, times(3)).writeValueAsString(any(Map.class));
    verify(bulkProcessDataService).saveBulkProcessData(argThat(list -> list.size() == 3));
    verify(bulkProcessService).saveOperation(bulkProcess);
    assertEquals(BulkProcess.STATUS_PUBLISHED, bulkProcess.getStatus());
  }

  @Test
  void generateBulkProcessData_InactiveProduct() throws IOException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId("store1");

    List<Map<String, String>> productData = new ArrayList<>();
    Map<String, String> product = new HashMap<>();
    product.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU1");
    productData.add(product);

    SystemParameterConfig config = new SystemParameterConfig();
    config.setValue("10");
    when(systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(config);

    ProductBasicResponse productResponse = new ProductBasicResponse();
    productResponse.setB2cActivated(true);
    productResponse.setForceReview(true);
    when(xProductOutboundService.getProductBasicInfo("store1", "SKU1", true)).thenReturn(productResponse);
    when(objectMapper.writeValueAsString(any())).thenReturn("{}");

    service.generateBulkProcessData(bulkProcess, productData, bulkBasicInfoRequest);

    verify(bulkProcessDataService).saveBulkProcessData(anyList());
  }

  @Test
  void generateBulkProcessData_EmptyData() throws IOException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    List<Map<String, String>> productData = new ArrayList<>();

    SystemParameterConfig config = new SystemParameterConfig();
    config.setValue("10");
    when(systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(config);

    service.generateBulkProcessData(bulkProcess, productData, bulkBasicInfoRequest);

    verify(bulkProcessDataService).saveBulkProcessData(anyList());
    assertEquals(BulkProcess.STATUS_PUBLISHED, bulkProcess.getStatus());
  }

  @Test
  public void testProcessBulkBasicInfoUpdate_EmptyBulkProcessDataList() throws Exception {
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).rowNumbers(ROW_NUMBERS)
            .build();
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING)).thenReturn(Collections.emptyList());
    Mockito.when(productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository())
        .thenReturn(businessPartnerRepository);
    service.processBulkBasicInfoUpdate(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING);
    verify(bulkProcessImageService, never()).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(bulkProcessVideoService, never()).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(objectMapper, never()).readValue(anyString(), eq(BulkBasicInfoUpdateRequest.class));
    verify(bulkProcessDataService, never()).saveAndReturnBulkProcessData(anyList());
  }

  @Test
  public void testProcessBulkBasicInfoUpdate1() throws Exception {
    // Given
    ReflectionTestUtils.setField(service, "youTubeDataApiKey", "apiKey");
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).rowNumbers(ROW_NUMBERS)
            .build();

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);

    String bulkRequestData1 =
        "{\"mainPhoto\":\"http://example.com/img1.jpg\",\"commonImages\":[\"http://example" + ".com/img1.jpg\"]}";
    String bulkRequestData2 =
        "{\"mainPhoto\":\"http://example.com/img2.jpg\",\"commonImages\":[\"http://example" + ".com/img2.jpg\"]}";

    BulkProcessData data1 = new BulkProcessData();
    data1.setStoreId(STORE_ID);
    data1.setBulkProcessCode(BULK_PROCESS_CODE);
    data1.setRowNumber(1);
    data1.setStatus(BulkProcessData.STATUS_PENDING);
    data1.setBulkRequestData(bulkRequestData1);

    BulkProcessData data2 = new BulkProcessData();
    data2.setStoreId(STORE_ID);
    data2.setBulkProcessCode(BULK_PROCESS_CODE);
    data2.setRowNumber(2);
    data2.setStatus(BulkProcessData.STATUS_PENDING);
    data2.setBulkRequestData(bulkRequestData2);

    List<BulkProcessData> bulkProcessDataList = Arrays.asList(data1, data2);

    BulkProcessImage image1 = new BulkProcessImage();
    image1.setStoreId(STORE_ID);
    image1.setBulkProcessCode(BULK_PROCESS_CODE);

    BulkProcessImage image2 = new BulkProcessImage();
    image2.setStoreId(STORE_ID);
    image2.setBulkProcessCode(BULK_PROCESS_CODE);

    List<BulkProcessImage> bulkProcessImages = Arrays.asList(image1, image2);

    BulkProcessVideo video = new BulkProcessVideo();
    video.setStoreId(STORE_ID);
    video.setBulkProcessCode(BULK_PROCESS_CODE);
    video.setVideoUrl("http://example.com/video.mp4");
    video.setUploadedURL("http://example.com/video.mp4");

    List<BulkProcessVideo> bulkProcessVideo = Collections.singletonList(video);

    Map<String, String> requestMap1 = new HashMap<>();
    requestMap1.put("mainPhoto", "http://example.com/img1.jpg");
    requestMap1.put("Video", "[\"http://example.com/img1.jpg\"]");

    Map<String, String> requestMap2 = new HashMap<>();
    requestMap2.put("mainPhoto", "http://example.com/img2.jpg");
    requestMap2.put("Video", "[\"http://example.com/img2.jpg\"]");

    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(STORE_ID, bulkProcess)).thenReturn(bulkProcessImages);
    when(bulkProcessVideoService.findByStoreIdAndBulkProcess(STORE_ID, bulkProcess)).thenReturn(bulkProcessVideo);
    when(objectMapper.readValue(eq(bulkRequestData1),
        any(com.fasterxml.jackson.core.type.TypeReference.class))).thenReturn(requestMap1);
    when(objectMapper.readValue(eq(bulkRequestData2),
        any(com.fasterxml.jackson.core.type.TypeReference.class))).thenReturn(requestMap2);
    Mockito.when(productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository())
        .thenReturn(businessPartnerRepository);
    when(bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    service.processBulkBasicInfoUpdate(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(STORE_ID, bulkProcess);
    verify(bulkProcessVideoService).findByStoreIdAndBulkProcess(STORE_ID, bulkProcess);
    verify(bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(anyList());
  }

  @Test
  public void testProcessBulkBasicInfoUpdate_EmptyDataList() throws Exception {
    // Given
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).rowNumbers(ROW_NUMBERS)
            .build();

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    Mockito.when(productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository())
        .thenReturn(businessPartnerRepository);

    // Mock dependencies
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING)).thenReturn(Collections.emptyList());

    // When
    service.processBulkBasicInfoUpdate(bulkUpdateEventModel);

    // Then
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING);
    verifyNoMoreInteractions(bulkProcessImageService, bulkProcessVideoService, objectMapper, bulkProcessDataService);
  }

  @Test
  public void testProcessBulkBasicInfoUpdate_ImageValidationError() throws Exception {
    // Given
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).rowNumbers(ROW_NUMBERS)
            .build();

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);

    String bulkRequestData = "{\"mainPhoto\":\"http://example.com/invalid.jpg\"}";

    BulkProcessData data = new BulkProcessData();
    data.setStoreId(STORE_ID);
    data.setBulkProcessCode(BULK_PROCESS_CODE);
    data.setRowNumber(1);
    data.setStatus(BulkProcessData.STATUS_PENDING);
    data.setBulkRequestData(bulkRequestData);

    List<BulkProcessData> bulkProcessDataList = Collections.singletonList(data);

    BulkProcessImage image = new BulkProcessImage();
    image.setStoreId(STORE_ID);
    image.setBulkProcessCode(BULK_PROCESS_CODE);
    image.setImageURL("http://example.com/valid.jpg");
    image.setErrorMessage("Image validation failed");
    image.setCompleted(true);

    List<BulkProcessImage> bulkProcessImages = Collections.singletonList(image);

    Map<String, String> requestMap = new HashMap<>();
    requestMap.put(BulkParameters.MAIN_PHOTO, "http://example.com/invalid.jpg");
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setMasterDataProduct(new MasterDataProductDTO());
    productL3Response.setProductType(ProductType.REGULAR);
    productL3Response.setLength(0d);
    productL3Response.setWidth(0d);
    productL3Response.setHeight(0d);
    productL3Response.setWeight(0d);
    productL3Response.setShippingWeight(0d);

    // Mock dependencies
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(STORE_ID, bulkProcess)).thenReturn(bulkProcessImages);
    when(objectMapper.readValue(eq(bulkRequestData), any(TypeReference.class))).thenReturn(requestMap);

    when(xProductOutboundService.getProductL3DetailsByProductSku(
      any() )).thenReturn(productL3Response);

    Mockito.when(productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository())
        .thenReturn(businessPartnerRepository);
    // When
    service.processBulkBasicInfoUpdate(bulkUpdateEventModel);

    // Then
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(STORE_ID, bulkProcess);
    verify(objectMapper).readValue(eq(bulkRequestData), any(TypeReference.class));
    verify(bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(bulkProcessDataList);
  }

  @Test
  public void testProcessBulkBasicInfoUpdate_ImageValidationErrorNoChange() throws Exception {
    // Given
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).rowNumbers(ROW_NUMBERS)
            .build();

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);

    String bulkRequestData = "{\"mainPhoto\":\"http://example.com/invalid.jpg\"}";

    BulkProcessData data = new BulkProcessData();
    data.setStoreId(STORE_ID);
    data.setBulkProcessCode(BULK_PROCESS_CODE);
    data.setRowNumber(1);
    data.setStatus(BulkProcessData.STATUS_PENDING);
    data.setBulkRequestData(bulkRequestData);

    List<BulkProcessData> bulkProcessDataList = Collections.singletonList(data);

    List<BulkProcessImage> bulkProcessImages = new ArrayList<>();
    Map<String, String> requestMap = new HashMap<>();
    requestMap.put(BulkParameters.MAIN_PHOTO, "http://example.com/invalid.jpg");
    requestMap.put(BulkParameters.SHIPPING_TYPE, ExcelHeaderNames.SHIPPED_BY_BLIBLI_ID);
    requestMap.put(BulkParameters.SIZE_CHART, "");
    ProductL3Response productL3Response = new ProductL3Response();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductImages(
        Collections.singletonList(new MasterDataProductImageDTO(true, "http://example.com/invalid.jpg", "valid.jpg", 0)));
    productL3Response.setMasterDataProduct(masterDataProductDTO);
    productL3Response.setProductType(ProductType.REGULAR);
    productL3Response.setLength(0d);
    productL3Response.setWidth(0d);
    productL3Response.setHeight(0d);
    productL3Response.setWeight(0d);
    productL3Response.setShippingWeight(0d);

    // Mock dependencies
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(STORE_ID, bulkProcess)).thenReturn(bulkProcessImages);
    when(objectMapper.readValue(eq(bulkRequestData), any(TypeReference.class))).thenReturn(requestMap);

    when(xProductOutboundService.getProductL3DetailsByProductSku(
        any() )).thenReturn(productL3Response);

    Mockito.when(productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository())
        .thenReturn(businessPartnerRepository);
    // When
    service.processBulkBasicInfoUpdate(bulkUpdateEventModel);

    // Then
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(STORE_ID, bulkProcess);
    verify(objectMapper).readValue(eq(bulkRequestData), any(TypeReference.class));
    verify(bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(bulkProcessDataList);
  }

  @Test
  public void testProcessBulkBasicInfoUpdate_ImageValidationErrorException() throws Exception {
    // Given
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).rowNumbers(ROW_NUMBERS)
            .build();
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    String bulkRequestData = "{\"mainPhoto\":\"http://example.com/invalid.jpg\"}";
    BulkProcessData data = new BulkProcessData();
    data.setStoreId(STORE_ID);
    data.setBulkProcessCode(BULK_PROCESS_CODE);
    data.setRowNumber(1);
    data.setStatus(BulkProcessData.STATUS_PENDING);
    data.setBulkRequestData(bulkRequestData);
    List<BulkProcessData> bulkProcessDataList = Collections.singletonList(data);
    BulkProcessImage image = new BulkProcessImage();
    image.setStoreId(STORE_ID);
    image.setBulkProcessCode(BULK_PROCESS_CODE);
    image.setImageURL("http://example.com/valid.jpg");
    image.setErrorMessage("Image validation failed");
    image.setCompleted(true);
    List<BulkProcessImage> bulkProcessImages = Collections.singletonList(image);
    Map<String, String> requestMap = new HashMap<>();
    requestMap.put(BulkParameters.MAIN_PHOTO, "http://example.com/invalid.jpg");
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setMasterDataProduct(new MasterDataProductDTO());
    productL3Response.setProductType(ProductType.REGULAR);
    productL3Response.setLength(0d);
    productL3Response.setWidth(0d);
    productL3Response.setHeight(0d);
    productL3Response.setWeight(0d);
    productL3Response.setShippingWeight(0d);

    // Mock dependencies
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenThrow(new ApplicationRuntimeException());
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(STORE_ID, bulkProcess)).thenReturn(bulkProcessImages);

    Mockito.when(productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository())
        .thenReturn(businessPartnerRepository);
    // When
    service.processBulkBasicInfoUpdate(bulkUpdateEventModel);

    // Then
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(STORE_ID, bulkProcess);
    verify(bulkProcessDataService, times(1)).saveAndReturnBulkProcessData(bulkProcessDataList);
  }

  @Test
  public void testProcessBulkBasicInfoUpdate_VideoValidationError() throws Exception {
    // Given
    ReflectionTestUtils.setField(service, "youTubeDataApiKey", "apiKey");
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).rowNumbers(ROW_NUMBERS)
            .build();

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);

    String bulkRequestData = "{\"videoUrl\":\"http://example.com/invalid.mp4\"}";

    BulkProcessData data = new BulkProcessData();
    data.setStoreId(STORE_ID);
    data.setBulkProcessCode(BULK_PROCESS_CODE);
    data.setRowNumber(1);
    data.setStatus(BulkProcessData.STATUS_PENDING);
    data.setBulkRequestData(bulkRequestData);

    List<BulkProcessData> bulkProcessDataList = Collections.singletonList(data);

    BulkProcessVideo video = new BulkProcessVideo();
    video.setStoreId(STORE_ID);
    video.setBulkProcessCode(BULK_PROCESS_CODE);
    video.setUploadedURL("http://example.com/valid.mp4");
    video.setErrorMessage("Video validation failed");
    video.setCompleted(true);

    List<BulkProcessVideo> bulkProcessVideos = Collections.singletonList(video);

    Map<String, String> requestMap = new HashMap<>();
    requestMap.put(com.gdn.mta.bulk.util.BulkParameters.VIDEO_URL, "http://example.com/invalid.mp4");

    // Mock dependencies
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(bulkProcessVideoService.findByStoreIdAndBulkProcess(STORE_ID, bulkProcess)).thenReturn(bulkProcessVideos);
    when(objectMapper.readValue(eq(bulkRequestData), any(TypeReference.class))).thenReturn(requestMap);
    when(bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    Mockito.when(productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository())
        .thenReturn(businessPartnerRepository);
    // When
    service.processBulkBasicInfoUpdate(bulkUpdateEventModel);

    // Then
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING);
    verify(bulkProcessVideoService).findByStoreIdAndBulkProcess(STORE_ID, bulkProcess);
    verify(objectMapper).readValue(eq(bulkRequestData), any(TypeReference.class));
    verify(bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(bulkProcessDataList);
  }

  @Test
  public void testProcessBulkBasicInfoUpdate_SystemError() throws Exception {
    // Given
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).rowNumbers(ROW_NUMBERS)
            .build();

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);

    String bulkRequestData = "invalid json";

    BulkProcessData data = new BulkProcessData();
    data.setStoreId(STORE_ID);
    data.setBulkProcessCode(BULK_PROCESS_CODE);
    data.setRowNumber(1);
    data.setStatus(BulkProcessData.STATUS_PENDING);
    data.setBulkRequestData(bulkRequestData);

    List<BulkProcessData> bulkProcessDataList = Collections.singletonList(data);

    // Mock dependencies
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(objectMapper.readValue(eq(bulkRequestData), any(TypeReference.class))).thenThrow(
        new JsonProcessingException("Invalid JSON") {
        });
    Mockito.when(productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository())
        .thenReturn(businessPartnerRepository);


    // When
    service.processBulkBasicInfoUpdate(bulkUpdateEventModel);

    // Then
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(eq(bulkRequestData), any(TypeReference.class));
    verify(bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(bulkProcessDataList);
  }

  @Test
  public void testProcessBulkBasicInfoUpdate_SuccessWithImagesOnly() throws Exception {
    // Given
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).rowNumbers(ROW_NUMBERS)
            .build();

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);

    String bulkRequestData =
        "{\"mainPhoto\":\"http://example.com/image.jpg\",\"commonPhoto2\":\"http://example" + ".com/image2.jpg\"}";

    BulkProcessData data = new BulkProcessData();
    data.setStoreId(STORE_ID);
    data.setBulkProcessCode(BULK_PROCESS_CODE);
    data.setRowNumber(1);
    data.setStatus(BulkProcessData.STATUS_PENDING);
    data.setBulkRequestData(bulkRequestData);

    List<BulkProcessData> bulkProcessDataList = Collections.singletonList(data);

    BulkProcessImage image1 = new BulkProcessImage();
    image1.setStoreId(STORE_ID);
    image1.setBulkProcessCode(BULK_PROCESS_CODE);
    image1.setImageURL("http://example.com/image.jpg");
    image1.setCompleted(true);

    BulkProcessImage image2 = new BulkProcessImage();
    image2.setStoreId(STORE_ID);
    image2.setBulkProcessCode(BULK_PROCESS_CODE);
    image2.setImageURL("http://example.com/image2.jpg");
    image2.setCompleted(true);

    List<BulkProcessImage> bulkProcessImages = Arrays.asList(image1, image2);

    Map<String, String> requestMap = new HashMap<>();
    requestMap.put(BulkParameters.MAIN_PHOTO, "http://example.com/image.jpg");
    requestMap.put(BulkParameters.COMMON_PHOTO_2, "http://example.com/image2.jpg");

    // Mock dependencies
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(STORE_ID, bulkProcess)).thenReturn(bulkProcessImages);
    when(objectMapper.readValue(eq(bulkRequestData), any(TypeReference.class))).thenReturn(requestMap);
    Mockito.when(productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository())
        .thenReturn(businessPartnerRepository);
    when(bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);


    // When
    service.processBulkBasicInfoUpdate(bulkUpdateEventModel);

    // Then
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(STORE_ID, bulkProcess);
    verify(objectMapper).readValue(eq(bulkRequestData), any(TypeReference.class));
    verify(bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(bulkProcessDataList);
  }

  @Test
  public void testProcessBulkBasicInfoUpdate_SuccessWithVideoOnly() throws Exception {
    // Given
    ReflectionTestUtils.setField(service, "youTubeDataApiKey", "apiKey");
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).rowNumbers(ROW_NUMBERS)
            .build();

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);

    String bulkRequestData = "{\"videoUrl\":\"http://example.com/video.mp4\"}";

    BulkProcessData data = new BulkProcessData();
    data.setStoreId(STORE_ID);
    data.setBulkProcessCode(BULK_PROCESS_CODE);
    data.setRowNumber(1);
    data.setStatus(BulkProcessData.STATUS_PENDING);
    data.setBulkRequestData(bulkRequestData);

    List<BulkProcessData> bulkProcessDataList = Collections.singletonList(data);
    BulkProcessVideo video = new BulkProcessVideo();
    video.setStoreId(STORE_ID);
    video.setBulkProcessCode(BULK_PROCESS_CODE);
    video.setUploadedURL("http://example.com/video.mp4");
    video.setCompleted(true);
    List<BulkProcessVideo> bulkProcessVideos = Collections.singletonList(video);
    Map<String, String> requestMap = new HashMap<>();
    requestMap.put(BulkParameters.VIDEO_URL, "http://example.com/video.mp4");
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(bulkProcessVideoService.findByStoreIdAndBulkProcess(STORE_ID, bulkProcess)).thenReturn(bulkProcessVideos);
    when(objectMapper.readValue(eq(bulkRequestData), any(TypeReference.class))).thenReturn(requestMap);
    Mockito.when(productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository())
        .thenReturn(businessPartnerRepository);
    when(bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    service.processBulkBasicInfoUpdate(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING);
    verify(bulkProcessVideoService).findByStoreIdAndBulkProcess(STORE_ID, bulkProcess);
    verify(objectMapper).readValue(eq(bulkRequestData), any(TypeReference.class));
    verify(bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(bulkProcessDataList);
  }

  @Test
  public void testProcessBulkBasicInfoUpdate_VideoExistsWithErrors() throws Exception {
    // Given
    ReflectionTestUtils.setField(service, "youTubeDataApiKey", "apiKey");
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).rowNumbers(ROW_NUMBERS)
            .build();

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);

    String bulkRequestData = "{\"videoUrl\":\"http://example.com/video.mp4\"}";

    BulkProcessData data = new BulkProcessData();
    data.setStoreId(STORE_ID);
    data.setBulkProcessCode(BULK_PROCESS_CODE);
    data.setRowNumber(1);
    data.setStatus(BulkProcessData.STATUS_PENDING);
    data.setBulkRequestData(bulkRequestData);

    List<BulkProcessData> bulkProcessDataList = Collections.singletonList(data);

    // Video exists but has errors (completed=true and has error message)
    BulkProcessVideo video = new BulkProcessVideo();
    video.setStoreId(STORE_ID);
    video.setBulkProcessCode(BULK_PROCESS_CODE);
    video.setUploadedURL("http://example.com/video.mp4");
    video.setErrorMessage("Video format not supported");
    video.setCompleted(true);

    List<BulkProcessVideo> bulkProcessVideos = Collections.singletonList(video);

    Map<String, String> requestMap = new HashMap<>();
    requestMap.put(BulkParameters.VIDEO_URL, "http://example.com/video.mp4");

    // Mock dependencies
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(bulkProcessVideoService.findByStoreIdAndBulkProcess(STORE_ID, bulkProcess)).thenReturn(bulkProcessVideos);
    when(objectMapper.readValue(eq(bulkRequestData), any(TypeReference.class))).thenReturn(requestMap);
    Mockito.when(productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository())
        .thenReturn(businessPartnerRepository);
    when(bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    // When
    service.processBulkBasicInfoUpdate(bulkUpdateEventModel);

    // Then
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING);
    verify(bulkProcessVideoService).findByStoreIdAndBulkProcess(STORE_ID, bulkProcess);
    verify(objectMapper).readValue(eq(bulkRequestData), any(TypeReference.class));
    verify(bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(bulkProcessDataList);
  }

  @Test
  public void testProcessBulkBasicInfoUpdate_ImageExistsWithErrors() throws Exception {
    // Given
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).rowNumbers(ROW_NUMBERS)
            .build();

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);

    String bulkRequestData = "{\"mainPhoto\":\"http://example.com/image.jpg\"}";

    BulkProcessData data = new BulkProcessData();
    data.setStoreId(STORE_ID);
    data.setBulkProcessCode(BULK_PROCESS_CODE);
    data.setRowNumber(1);
    data.setStatus(BulkProcessData.STATUS_PENDING);
    data.setBulkRequestData(bulkRequestData);

    List<BulkProcessData> bulkProcessDataList = Collections.singletonList(data);

    // Image exists but has errors (completed=true and has error message)
    BulkProcessImage image = new BulkProcessImage();
    image.setStoreId(STORE_ID);
    image.setBulkProcessCode(BULK_PROCESS_CODE);
    image.setImageURL("http://example.com/image.jpg");
    image.setErrorMessage("Image resolution too low");
    image.setCompleted(true);

    List<BulkProcessImage> bulkProcessImages = Collections.singletonList(image);

    Map<String, String> requestMap = new HashMap<>();
    requestMap.put(BulkParameters.MAIN_PHOTO, "http://example.com/image.jpg");

    // Mock dependencies
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(STORE_ID, bulkProcess)).thenReturn(bulkProcessImages);
    when(objectMapper.readValue(eq(bulkRequestData), any(TypeReference.class))).thenReturn(requestMap);
    Mockito.when(productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository())
        .thenReturn(businessPartnerRepository);
    when(bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);


    // When
    service.processBulkBasicInfoUpdate(bulkUpdateEventModel);

    // Then
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(STORE_ID, bulkProcess);
    verify(objectMapper).readValue(eq(bulkRequestData), any(TypeReference.class));
    verify(bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(bulkProcessDataList);
  }

  @Test
  public void testProcessBulkBasicInfoUpdate_PublishBasicInfoUpdateEvent() throws Exception {
    // Given
    ReflectionTestUtils.setField(service, "youTubeDataApiKey", "apiKey");
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).rowNumbers(ROW_NUMBERS)
            .build();

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);

    String bulkRequestData = "{\"videoUrl\":\"http://example.com/video.mp4\"}";

    BulkProcessData data = new BulkProcessData();
    data.setStoreId(STORE_ID);
    data.setBulkProcessCode(BULK_PROCESS_CODE);
    data.setRowNumber(1);
    data.setStatus(BulkProcessData.STATUS_PENDING);
    data.setBulkRequestData(bulkRequestData);

    List<BulkProcessData> bulkProcessDataList = Collections.singletonList(data);

    // Video exists and is valid
    BulkProcessVideo video = new BulkProcessVideo();
    video.setStoreId(STORE_ID);
    video.setBulkProcessCode(BULK_PROCESS_CODE);
    video.setUploadedURL("http://example.com/video.mp4");
    video.setCompleted(true);

    List<BulkProcessVideo> bulkProcessVideos = Collections.singletonList(video);

    Map<String, String> requestMap = new HashMap<>();
    requestMap.put(BulkParameters.VIDEO_URL, "http://example.com/video.mp4");

    // Mock dependencies
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(bulkProcessVideoService.findByStoreIdAndBulkProcess(STORE_ID, bulkProcess)).thenReturn(bulkProcessVideos);
    when(objectMapper.readValue(eq(bulkRequestData), any(TypeReference.class))).thenReturn(requestMap);
    Mockito.when(productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository())
        .thenReturn(businessPartnerRepository);
    when(bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    service.processBulkBasicInfoUpdate(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING);
    verify(bulkProcessVideoService).findByStoreIdAndBulkProcess(STORE_ID, bulkProcess);
    verify(objectMapper).readValue(eq(bulkRequestData), any(TypeReference.class));
    verify(bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(bulkProcessDataList);
  }

  @Test
  public void testProcessBulkBasicInfoUpdate_YoutubeValidTest() throws Exception {
    // Given
    ReflectionTestUtils.setField(service, "youTubeDataApiKey", "apiKey");
    ReflectionTestUtils.setField(service,"validateYoutubeUrlSwitchEn",true);
    ReflectionTestUtils.setField(service, "youtubeRegex", REGEX);
    BulkUpdateEventModel bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).rowNumbers(ROW_NUMBERS)
            .build();

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);

    String bulkRequestData = "{\"videoUrl\":\"https://www.youtube.com/watch?v=P1xAhgKTqDA\",\"Tipe pengiriman\":\"REGULAR\"}";

    BulkProcessData data = new BulkProcessData();
    data.setStoreId(STORE_ID);
    data.setBulkProcessCode(BULK_PROCESS_CODE);
    data.setRowNumber(1);
    data.setStatus(BulkProcessData.STATUS_PENDING);
    data.setBulkRequestData(bulkRequestData);

    List<BulkProcessData> bulkProcessDataList = Collections.singletonList(data);

    // Video exists and is valid
    BulkProcessVideo video = new BulkProcessVideo();
    video.setStoreId(STORE_ID);
    video.setBulkProcessCode(BULK_PROCESS_CODE);
    video.setUploadedURL("https://www.youtube.com/watch?v=P1xAhgKTqDA");
    video.setCompleted(true);

    List<BulkProcessVideo> bulkProcessVideos = Collections.singletonList(video);

    Map<String, String> requestMap = new HashMap<>();
    requestMap.put(BulkParameters.VIDEO_URL, "https://www.youtube.com/watch?v=P1xAhgKTqDA");
    requestMap.put(BulkParameters.SHIPPING_TYPE, "REGULAR");

    // Mock dependencies
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING)).thenReturn(bulkProcessDataList);
    when(bulkProcessVideoService.findByStoreIdAndBulkProcess(STORE_ID, bulkProcess)).thenReturn(bulkProcessVideos);
    when(objectMapper.readValue(eq(bulkRequestData), any(TypeReference.class))).thenReturn(requestMap);
    Mockito.when(productLevel3BulkUpdateServiceBean.getBusinessPartnerRepository())
        .thenReturn(businessPartnerRepository);

    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setOfficial(true);
    profileResponse.setBopisFlag(true);
    profileResponse.setBigProductFlag(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, null)).thenReturn(profileResponse);

    ProductL3Response response = new ProductL3Response();

    response.setProductSku("SKU-12345");
    response.setProductCode("PROD-001");
    response.setProductType(ProductType.BIG_PRODUCT); // Replace with your enum
    response.setMarkForDelete(false);
    response.setOff2OnChannelActive(true);

    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    // set fields inside masterDataProduct if needed
    response.setMasterDataProduct(masterDataProduct);

    response.setLength(25.0);
    response.setWidth(15.0);
    response.setHeight(5.0);
    response.setWeight(1.2);
    response.setShippingWeight(1.5);
    response.setSizeChartCode("SIZE-001");
    response.setVideoUrl("https://example.com/video.mp4");
    response.setUrl("https://example.com/product-url");


    when(xProductOutboundService.getProductL3DetailsByProductSku(null)).thenReturn(response);
    when(bulkProcessDataService.saveAndReturnBulkProcessData(anyList())).thenReturn(bulkProcessDataList);
    service.processBulkBasicInfoUpdate(bulkUpdateEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        ROW_NUMBERS, BulkProcessData.STATUS_PENDING);
    verify(bulkProcessVideoService).findByStoreIdAndBulkProcess(STORE_ID, bulkProcess);
    verify(objectMapper).readValue(eq(bulkRequestData), any(TypeReference.class));
    verify(bulkProcessDataService, times(2)).saveAndReturnBulkProcessData(bulkProcessDataList);
  }

  @Test
  public void testMapNewVideoUrlsToProductCodes() {
    ReflectionTestUtils.setField(service, "youtubeRegex", REGEX);
    Map<String, List<String>> videoUrlToProductCodesMap = new HashMap<>();
    Map<String, String> userData = new HashMap<>();
    userData.put(BulkParameters.VIDEO_URL, VIDEO_URL);
    ProductImageAndVideoResponse productImageAndVideoResponse = Mockito.mock(ProductImageAndVideoResponse.class);
    Mockito.when(productImageAndVideoResponse.getVideo()).thenReturn("https://example.com/video2.mp4");
    service.mapNewVideoUrlsToProductCodes(videoUrlToProductCodesMap, userData, PRODUCT_SKU,
        productImageAndVideoResponse);
    assertTrue(videoUrlToProductCodesMap.containsKey(VIDEO_URL));
    assertEquals(1, videoUrlToProductCodesMap.get(VIDEO_URL).size());
    assertEquals(PRODUCT_SKU, videoUrlToProductCodesMap.get(VIDEO_URL).get(0));
  }

  @Test
  void testMapNewVideoUrlsToProductCodes_BlankUploadedUrl() {
    Map<String, List<String>> videoUrlToProductCodesMap = new HashMap<>();
    Map<String, String> userData = new HashMap<>();
    userData.put(BulkParameters.VIDEO_URL, "  ");
    ProductImageAndVideoResponse response = Mockito.mock(ProductImageAndVideoResponse.class);
    Mockito.when(response.getVideo()).thenReturn("https://existing.com/video.mp4");
    service.mapNewVideoUrlsToProductCodes(videoUrlToProductCodesMap, userData, PRODUCT_SKU, response);
    assertTrue(videoUrlToProductCodesMap.isEmpty());
  }

  @Test
  void testMapNewVideoUrlsToProductCodes_YoutubeUrl() throws IOException {
    ReflectionTestUtils.setField(service, "youtubeRegex", REGEX);
    Map<String, List<String>> videoUrlToProductCodesMap = new HashMap<>();
    Map<String, String> userData = new HashMap<>();
    userData.put(BulkParameters.VIDEO_URL, "https://www.youtube.com/watch?v=abc123");
    ProductImageAndVideoResponse response = Mockito.mock(ProductImageAndVideoResponse.class);
    Mockito.when(response.getVideo()).thenReturn(null);
    service.mapNewVideoUrlsToProductCodes(videoUrlToProductCodesMap, userData, PRODUCT_SKU, response);
    assertTrue(videoUrlToProductCodesMap.isEmpty());
  }

  @Test
  void testMapNewVideoUrlsToProductCodes_NoExistingVideo_AddsToMap() {
    ReflectionTestUtils.setField(service, "youtubeRegex", REGEX);
    Map<String, List<String>> videoUrlToProductCodesMap = new HashMap<>();
    Map<String, String> userData = new HashMap<>();
    userData.put(BulkParameters.VIDEO_URL, "https://cdn.store.com/videos/video.mp4");
    ProductImageAndVideoResponse response = Mockito.mock(ProductImageAndVideoResponse.class);
    Mockito.when(response.getVideo()).thenReturn("");
    service.mapNewVideoUrlsToProductCodes(videoUrlToProductCodesMap, userData, PRODUCT_SKU, response);
    assertTrue(videoUrlToProductCodesMap.containsKey("https://cdn.store.com/videos/video.mp4"));
  }

  @Test
  void testMapNewVideoUrlsToProductCodes_AlreadyPresent_SkipAdding() {
    ReflectionTestUtils.setField(service, "youtubeRegex", REGEX);
    Map<String, List<String>> videoUrlToProductCodesMap = new HashMap<>();
    Map<String, String> userData = new HashMap<>();
    String uploadedUrl = "video123.mp4";
    userData.put(BulkParameters.VIDEO_URL, uploadedUrl);
    ProductImageAndVideoResponse response = Mockito.mock(ProductImageAndVideoResponse.class);
    Mockito.when(response.getVideo()).thenReturn("https://cdn.store.com/video123.mp4");
    service.mapNewVideoUrlsToProductCodes(videoUrlToProductCodesMap, userData, PRODUCT_SKU, response);
    assertFalse(videoUrlToProductCodesMap.containsKey(uploadedUrl));
  }

  @Test
  void testMapNewVideoUrlsToProductCodes_NotAlreadyPresent_AddsToMap() {
    ReflectionTestUtils.setField(service, "youtubeRegex", REGEX);
    Map<String, List<String>> videoUrlToProductCodesMap = new HashMap<>();
    Map<String, String> userData = new HashMap<>();
    String uploadedUrl = "https://cdn.store.com/video123.mp4";
    userData.put(BulkParameters.VIDEO_URL, uploadedUrl);
    ProductImageAndVideoResponse response = Mockito.mock(ProductImageAndVideoResponse.class);
    Mockito.when(response.getVideo()).thenReturn("https://cdn.store.com/another_video.mp4");
    service.mapNewVideoUrlsToProductCodes(videoUrlToProductCodesMap, userData, PRODUCT_SKU, response);
    assertTrue(videoUrlToProductCodesMap.containsKey(uploadedUrl));
    assertEquals(1, videoUrlToProductCodesMap.get(uploadedUrl).size());
    assertEquals(PRODUCT_SKU, videoUrlToProductCodesMap.get(uploadedUrl).get(0));
  }

  @Test
  void testSaveAndPublishBulkBasicInfoVideoDownloads() {
    Map<String, List<String>> videoUrlToProductCodesMap = new HashMap<>();
    videoUrlToProductCodesMap.put("https://example.com/video1.mp4", Arrays.asList("SKU1", "SKU2"));
    List<BulkProcessVideo> bulkProcessVideoList = Collections.singletonList(Mockito.mock(BulkProcessVideo.class));
    SystemParameterConfig mockParameter = new SystemParameterConfig();
    mockParameter.setValue("2");
    bulkProcess.setBusinessPartnerCode("");
    service.saveAndPublishBulkBasicInfoVideoDownloads(bulkProcess, videoUrlToProductCodesMap, bulkProcessVideoList);
    Mockito.verify(bulkProcessVideoService).saveAllBulkProcessVideo(bulkProcessVideoList);
    Mockito.verify(bulkProcessService)
        .publishBulkBasicInfoVideoDownloadEventModel(BULK_PROCESS_CODE, BulkProcessType.PRODUCT_BASIC_INFO.getValue(),
            "https://example.com/video1.mp4", "");
  }

  @Test
  void testSaveAndPublishBulkBasicInfoImageDownloads() {
    Map<String, List<String>> imageUrlToProductCodesMap = new HashMap<>();
    imageUrlToProductCodesMap.put("https://example.com/image1.jpg", Arrays.asList("SKU1", "SKU2"));
    List<BulkProcessImage> bulkProcessImageList = Collections.singletonList(Mockito.mock(BulkProcessImage.class));
    SystemParameterConfig mockParameter = new SystemParameterConfig();
    mockParameter.setValue("2");
    Mockito.when(
            systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.IMAGE_DOWNLOAD_BATCH_SIZE))
        .thenReturn(mockParameter);
    service.saveAndPublishBulkBasicInfoImageDownloads(bulkProcess, imageUrlToProductCodesMap, bulkProcessImageList);
    Mockito.verify(bulkProcessImageService).saveBulkProcessImage(bulkProcessImageList);
    Mockito.verify(bulkProcessService)
        .publishBulkBasicInfoImageDownloadEventModel(BULK_PROCESS_CODE, BulkProcessType.PRODUCT_BASIC_INFO.getValue(),
            Collections.singletonList("https://example.com/image1.jpg"));
  }

  @Test
  void testSetBulkProcessStatusWhenImageOrVideoListIsNotEmpty() {
    bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_BASIC_INFO.getValue());
    service.setBulkProcessStatus(bulkProcess, new ArrayList<>(), new ArrayList<>());
    assertEquals(BulkProcess.STATUS_READY_TO_PROCESS, bulkProcess.getStatus());
    service.setBulkProcessStatus(bulkProcess, List.of(new BulkProcessImage()), new ArrayList<>());
    assertEquals(BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING, bulkProcess.getStatus());
    service.setBulkProcessStatus(bulkProcess, List.of(new BulkProcessImage()), List.of((new BulkProcessVideo())));
    assertEquals(BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING, bulkProcess.getStatus());
    service.setBulkProcessStatus(bulkProcess, new ArrayList<>(), List.of((new BulkProcessVideo())));
    assertEquals(BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING, bulkProcess.getStatus());
  }

  @Test
  void processBulkUpdate_BulkProcessIsNull_ThrowsApplicationRuntimeException() {
    BulkBasicInfoRequest request = new BulkBasicInfoRequest();
    request.setStoreId("store1");
    request.setBulkProcessCode("code1");
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(eq("store1"), eq("code1"),
        eq(BulkProcess.STATUS_PENDING))).thenReturn(null);
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class, () -> {
      service.processBulkUpdate(request);
    });
    assertTrue(exception.getMessage().contains("code1 is already processed or being processed"));
    verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(eq("store1"),
        eq("code1"), eq(BulkProcess.STATUS_PENDING));
  }

  @Test
  public void processBulkProcessVideoUpdate_success() {
    String storeId = "store123";
    String bulkProcessCode = "bulkCode123";
    String originalUrl = "http://video.url/video.mp4";
    BulkBasicInfoVideoDownloadResponseModel responseModel = new BulkBasicInfoVideoDownloadResponseModel();
    responseModel.setOriginalUrl(originalUrl);
    Map<String, String> additionalFields = new HashMap<>();
    additionalFields.put(Constant.BULK_PROCESS_CODE, bulkProcessCode);
    responseModel.setAdditionalFields(additionalFields);
    BulkProcessVideo bulkProcessVideo = new BulkProcessVideo();
    Mockito.when(
            bulkProcessVideoService.findByStoreIdAndBulkProcessCodeAndUploadedURL(storeId, bulkProcessCode, originalUrl))
        .thenReturn(bulkProcessVideo);
    Mockito.when(bulkProcessVideoService.saveBulkProcessVideo(bulkProcessVideo)).thenReturn(bulkProcessVideo);
    service.processBulkProcessVideoUpdate(storeId, responseModel);
    Mockito.verify(bulkProcessVideoService)
        .findByStoreIdAndBulkProcessCodeAndUploadedURL(storeId, bulkProcessCode, originalUrl);
    Mockito.verify(bulkProcessVideoService).saveBulkProcessVideo(bulkProcessVideo);
  }

  @Test
  public void processBulkProcessVideoUpdate_null() {
    String storeId = "store123";
    String bulkProcessCode = "bulkCode123";
    String originalUrl = "http://video.url/video.mp4";
    BulkBasicInfoVideoDownloadResponseModel responseModel = new BulkBasicInfoVideoDownloadResponseModel();
    responseModel.setOriginalUrl(originalUrl);
    Map<String, String> additionalFields = new HashMap<>();
    additionalFields.put(Constant.BULK_PROCESS_CODE, bulkProcessCode);
    responseModel.setAdditionalFields(additionalFields);
    Mockito.when(
            bulkProcessVideoService.findByStoreIdAndBulkProcessCodeAndUploadedURL(storeId, bulkProcessCode, originalUrl))
        .thenReturn(null);
    service.processBulkProcessVideoUpdate(storeId, responseModel);
    Mockito.verify(bulkProcessVideoService)
        .findByStoreIdAndBulkProcessCodeAndUploadedURL(storeId, bulkProcessCode, originalUrl);
  }

  @Test
  public void processBulkProcessVideoUpdateNoBpCode() {
    String storeId = "store123";
    String bulkProcessCode = "bulkCode123";
    String originalUrl = "http://video.url/video.mp4";
    BulkBasicInfoVideoDownloadResponseModel responseModel = new BulkBasicInfoVideoDownloadResponseModel();
    responseModel.setOriginalUrl(originalUrl);
    Map<String, String> additionalFields = new HashMap<>();
    responseModel.setAdditionalFields(additionalFields);
    service.processBulkProcessVideoUpdate(storeId, responseModel);
    additionalFields.put(Constant.BULK_PROCESS_CODE, null);
    service.processBulkProcessVideoUpdate(storeId, responseModel);
  }

  @Test
  void testDownloadImages() throws Exception {
    String bulkProcessCode = "BULK123";
    List<String> imageDownloadList = List.of("http://image.url/img1.jpg");
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessImage image = new BulkProcessImage();
    image.setImageURL("http://image.url/img1.jpg");
    image.setSequence(1);
    image.setNotes("[{\"someField\":\"someValue\"}]");
    List<BulkProcessImage> imageList = List.of(image);
    List<ProductBasicDetail> productDetails = List.of(new ProductBasicDetail());
    ImageDownloadResult result = new ImageDownloadResult();
    result.setDownloadSuccess(true);
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(Constant.STORE_ID,
        bulkProcessCode)).thenReturn(bulkProcess);
    when(bulkProcessImageService.findByBulkProcessCodeAndImageUrl(bulkProcess, imageDownloadList)).thenReturn(
        imageList);
    when(objectMapper.readValue(eq(image.getNotes()), any(TypeReference.class))).thenReturn(productDetails);
    when(fileStorageServiceBean.downloadImageFileValidateAndUploadToGCS(eq(bulkProcessCode), anyMap(), anyList(), any(),
        any())).thenReturn(result);
    service.downloadImages(bulkProcessCode, imageDownloadList);
    verify(bulkProcessImageService).saveBulkProcessImage(imageList);
  }

  @Test
  void testDownloadImagesWithExceptionInDownload() throws Exception {
    String bulkProcessCode = "BULK123";
    List<String> imageDownloadList = List.of("http://image.url/img1.jpg");
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessImage image = new BulkProcessImage();
    image.setImageURL("http://image.url/img1.jpg");
    image.setSequence(1);
    image.setNotes("[{\"someField\":\"someValue\"}]");
    List<BulkProcessImage> imageList = List.of(image);
    List<ProductBasicDetail> productDetails = List.of(new ProductBasicDetail());
    when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(Constant.STORE_ID,
        bulkProcessCode)).thenReturn(bulkProcess);
    when(bulkProcessImageService.findByBulkProcessCodeAndImageUrl(bulkProcess, imageDownloadList)).thenReturn(
        imageList);
    when(objectMapper.readValue(eq(image.getNotes()), any(TypeReference.class))).thenReturn(productDetails);
    when(fileStorageServiceBean.downloadImageFileValidateAndUploadToGCS(eq(bulkProcessCode), anyMap(), anyList(), any(),
        eq(productDetails))).thenThrow(new RuntimeException("Download error"));
    service.downloadImages(bulkProcessCode, imageDownloadList);
    verify(bulkProcessImageService).saveBulkProcessImage(imageList);
  }

}