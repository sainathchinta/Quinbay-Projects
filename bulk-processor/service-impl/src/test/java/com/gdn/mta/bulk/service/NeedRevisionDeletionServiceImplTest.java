package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.DeleteProductRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityResponse;
import com.gda.mta.product.dto.response.ProductCodeAndNameDetails;
import com.gda.mta.product.dto.response.ProductCodeAndNameResponseList;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletion;
import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletionData;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.repository.BulkNeedRevisionDeletionDataRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.NeedRevisionDeletionRepository;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.anyList;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

public class NeedRevisionDeletionServiceImplTest {

  public static final String BULK_NEED_REVISION_POST_LIVE_DELETION =
    "Bulk Need Revision postLive Deletion";
  @InjectMocks
  private NeedRevisionDeletionServiceImpl needRevisionDeletionService;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private NeedRevisionDeletionRepository needRevisionDeletionRepository;

  @Mock
  private BulkNeedRevisionDeletionDataRepository needRevisionDeletionDataRepository;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Captor
  private ArgumentCaptor<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<BulkNeedRevisionDeletionData>> bulkNeedRevisionDeletionDataArgumentCaptor;

  @Mock
  private BulkNeedRevisionDeletionDataRepository bulkNeedRevisionDeletionDataRepository;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private NotificationService notificationService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private PBPOutboundService pbpOutboundService;

  @Mock
  private ObjectMapper objectMapper;


  private static final int BATCH_SIZE = 50;
  private static final String GCS_PATH = "mock/gcs/path";
  private static final String ALLOWED_MERCHANT_TYPES = "CM";
  private static final String STATES = "PENDING,IN_PROGRESS";
  private static final String REQUEST_ID = "requestId";
  private static final String PRODUCT_NAME = "productName";
  private SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
  public static final String NEED_REVISION_DELETION_TYPE = "NEED_REVISION_DELETION";
  public static final String NEED_REVISION_DELETION_EVENT = "com.gdn.x.bulk.need.revision.deletion";
  private static final String BUSINESS_PARTNER_CODE = "RAM-70107";
  private static final String STORE_ID = "storeId";
  private static final String DELETION_PROCESS_CODE = "DEL-1234";
  private static final String PRODUCT_CODE = "MTA-123456";
  private static final String PRODUCT_CODE_2 = "MTA-000001";
  private BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
  private BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();

  @BeforeEach
  public void setup() throws Exception {
    initMocks(this);
    ReflectionTestUtils.setField(needRevisionDeletionService, "batchSizeForFetchingNrDeletion", BATCH_SIZE);
    ReflectionTestUtils.setField(needRevisionDeletionService, "gcsNeedRevisionDeletionUploadFilePath", GCS_PATH);
    ReflectionTestUtils.setField(needRevisionDeletionService,
        "autoNeedRevisionDeletionMerchantTypes", ALLOWED_MERCHANT_TYPES);
    ReflectionTestUtils.setField(needRevisionDeletionService, "batchSizeToPublishDataEntries", 5);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType("CM");
    profileResponse.setCompany(companyDTO);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeList(Mockito.any(), Mockito.anyInt(),
            Mockito.anyInt())).thenReturn(List.of(profileResponse));
    Mockito.when(kafkaTopicProperties.getNeedRevisionDeletionEvent())
        .thenReturn(NEED_REVISION_DELETION_EVENT);
    bulkNeedRevisionDeletionData.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletionData.setProductCode(PRODUCT_CODE);

    bulkNeedRevisionDeletion.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletion.setStoreId(STORE_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productRepository, needRevisionDeletionRepository,
        needRevisionDeletionDataRepository, businessPartnerRepository);
  }

  @Test
  public void testPopulateNRBusinessPartnerCodes_withValidData() {
    String storeId = "store-123";
    String requestId = "req-456";
    List<String> businessPartnerCodeList = Arrays.asList("partner-1", "partner-2");
    when(productRepository.fetchNRBusinessPartnerCodes(storeId, requestId)).thenReturn(businessPartnerCodeList);
    needRevisionDeletionService.populateNRBusinessPartnerCodes(storeId, requestId);
    verify(productRepository, times(1)).fetchNRBusinessPartnerCodes(storeId, requestId);
    verify(needRevisionDeletionRepository).saveAll(anyList());
  }

  @Test
  public void testPopulateNRBusinessPartnerCodes_withEmptyData() {
    String storeId = "store-123";
    String requestId = "req-456";
    when(productRepository.fetchNRBusinessPartnerCodes(storeId, requestId)).thenReturn(Collections.emptyList());
    needRevisionDeletionService.populateNRBusinessPartnerCodes(storeId, requestId);
    verify(productRepository, times(1)).fetchNRBusinessPartnerCodes(storeId, requestId);
    verify(needRevisionDeletionRepository, times(0)).saveAll(any());
  }

  @Test
  public void testPopulateNRBusinessPartnerCodes_withNullData() {
    String storeId = "store-123";
    String requestId = "req-456";
    when(productRepository.fetchNRBusinessPartnerCodes(storeId, requestId)).thenReturn(null);
    needRevisionDeletionService.populateNRBusinessPartnerCodes(storeId, requestId);
    verify(productRepository, times(1)).fetchNRBusinessPartnerCodes(storeId, requestId);
    verify(needRevisionDeletionRepository, times(0)).saveAll(any());
  }

  @Test
  void testSendNotificationForNeedRevisionDeletion() throws Exception {
    ReflectionTestUtils.setField(needRevisionDeletionService, "fetchPublishedCountForNeedRevisionDeletion", BATCH_SIZE);
    Page<BulkNeedRevisionDeletion> mockPage = mock(Page.class);
    BulkNeedRevisionDeletion deletionEntity = new BulkNeedRevisionDeletion();
    deletionEntity.setDeletionProcessCode("process123");
    deletionEntity.setTotalCount(10);
    deletionEntity.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
    BulkNeedRevisionDeletion deletionEntity1 = new BulkNeedRevisionDeletion();
    deletionEntity1.setDeletionProcessCode("122");
    deletionEntity1.setTotalCount(10);
    deletionEntity1.setStatus(BulkNeedRevisionDeletion.STATUS_PUBLISHED);
    when(mockPage.getContent()).thenReturn(Collections.singletonList(deletionEntity));
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(anyString(), anyString(), any())).thenReturn(
        mockPage);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusOrderByCreatedDateAsc(anyString(), anyString(),
        any())).thenReturn(Collections.singletonList(deletionEntity1));
    when(needRevisionDeletionDataRepository.findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(anyString(),
        any(),any())).thenReturn(new ArrayList<>());
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData.setEligibleForDeletion(true);
    bulkNeedRevisionDeletionData.setStatus(BulkNeedRevisionDeletion.STATUS_PROCESSED);
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData2 = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData2.setEligibleForDeletion(true);
    bulkNeedRevisionDeletionData2.setStatus(BulkNeedRevisionDeletion.STATUS_FAIL);
    ProductCodeAndNameDetails productCodeAndNameDetails = new ProductCodeAndNameDetails();
    productCodeAndNameDetails.setProductName("product123");
    ObjectMapper objectMapper1 = new ObjectMapper();
    bulkNeedRevisionDeletionData.setProductData(objectMapper1.writeValueAsString(productCodeAndNameDetails));
    bulkNeedRevisionDeletionData2.setProductData(objectMapper1.writeValueAsString(productCodeAndNameDetails));
    when(bulkNeedRevisionDeletionDataRepository.findByStoreIdAndDeletionProcessCode(anyString(),
        anyString())).thenReturn(Arrays.asList(bulkNeedRevisionDeletionData, bulkNeedRevisionDeletionData2));
    doNothing().when(fileStorageService).uploadFileToBulkBucket(anyString(), any());
    doNothing().when(notificationService).sendNeedRevisionDeletionNotification(any(), anyString(), anyInt(), anyInt());
    needRevisionDeletionService.sendNotificationForNeedRevisionDeletion("store123", "req456");
    verify(needRevisionDeletionRepository, times(1)).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
        anyString(), anyString(), any());
    verify(notificationService, times(1)).sendNeedRevisionDeletionNotification(any(), any(), anyInt(), anyInt());
    verify(needRevisionDeletionRepository,Mockito.times(2)).saveAll(any());
    verify(needRevisionDeletionRepository).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(anyString(), anyString(), any());
    verify(needRevisionDeletionRepository).findByStoreIdAndStatusOrderByCreatedDateAsc(anyString(), anyString(), any());
    verify(needRevisionDeletionDataRepository,times(2)).findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(anyString(),any(),any());
  }

  @Test
  void testSendNotificationForNeedRevisionDeletionStatusFinishedTest() throws Exception {
    ReflectionTestUtils.setField(needRevisionDeletionService, "fetchPublishedCountForNeedRevisionDeletion", BATCH_SIZE);
    Page<BulkNeedRevisionDeletion> mockPage = mock(Page.class);
    BulkNeedRevisionDeletion deletionEntity = new BulkNeedRevisionDeletion();
    deletionEntity.setDeletionProcessCode("process123");
    deletionEntity.setTotalCount(10);
    deletionEntity.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
    when(mockPage.getContent()).thenReturn(Collections.singletonList(deletionEntity));
    BulkNeedRevisionDeletion deletionEntity1 = new BulkNeedRevisionDeletion();
    deletionEntity1.setDeletionProcessCode("122");
    deletionEntity1.setTotalCount(10);
    deletionEntity1.setStatus(BulkNeedRevisionDeletion.STATUS_PUBLISHED);
    when(mockPage.getContent()).thenReturn(Collections.singletonList(deletionEntity));
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(anyString(),
        anyString(), any())).thenReturn(mockPage);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusOrderByCreatedDateAsc(anyString(), anyString(),
        any())).thenReturn(Collections.singletonList(deletionEntity1));
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData.setEligibleForDeletion(true);
    bulkNeedRevisionDeletionData.setStatus(BulkNeedRevisionDeletion.STATUS_PROCESSED);
    bulkNeedRevisionDeletionData.setDeletionProcessCode("bulkProcessCode");
    ProductCodeAndNameDetails productCodeAndNameDetails = new ProductCodeAndNameDetails();
    productCodeAndNameDetails.setProductName("product123");
    productCodeAndNameDetails.setProductCode("dd");
    ObjectMapper objectMapper = new ObjectMapper();
    bulkNeedRevisionDeletionData.setProductData(objectMapper.writeValueAsString(productCodeAndNameDetails));
    when(needRevisionDeletionDataRepository.findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(anyString(),
        anyList(),any())).thenReturn(Arrays.asList(bulkNeedRevisionDeletionData));
    when(bulkNeedRevisionDeletionDataRepository.findByStoreIdAndDeletionProcessCode(anyString(),
        anyString())).thenReturn(Arrays.asList(bulkNeedRevisionDeletionData));
    doNothing().when(fileStorageService).uploadFileToBulkBucket(anyString(), any());
    doNothing().when(notificationService).sendNeedRevisionDeletionNotification(any(), anyString(), anyInt(), anyInt());
    needRevisionDeletionService.sendNotificationForNeedRevisionDeletion("store123", "req456");
    verify(notificationService, times(1)).sendNeedRevisionDeletionNotification(any(), any(), anyInt(), anyInt());
    verify(needRevisionDeletionRepository, times(2)).saveAll(any());
    verify(needRevisionDeletionRepository, times(1)).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
        anyString(), anyString(), any());
    verify(needRevisionDeletionRepository, Mockito.times(2)).saveAll(any());
    verify(needRevisionDeletionRepository).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(anyString(),
        anyString(), any());
    verify(needRevisionDeletionRepository).findByStoreIdAndStatusOrderByCreatedDateAsc(anyString(), anyString(), any());
    verify(needRevisionDeletionDataRepository,times(2)).findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(anyString(), any(),any());
  }

  @Test
  void testSendNotificationForNeedRevisionDeletionZeroRowsTest() throws Exception {
    ReflectionTestUtils.setField(needRevisionDeletionService, "fetchPublishedCountForNeedRevisionDeletion", BATCH_SIZE);
    Page<BulkNeedRevisionDeletion> mockPage = mock(Page.class);
    BulkNeedRevisionDeletion deletionEntity = new BulkNeedRevisionDeletion();
    deletionEntity.setDeletionProcessCode("process123");
    deletionEntity.setTotalCount(10);
    deletionEntity.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
    when(mockPage.getContent()).thenReturn(Collections.singletonList(deletionEntity));
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData.setEligibleForDeletion(true);
    bulkNeedRevisionDeletionData.setStatus(BulkNeedRevisionDeletion.STATUS_PROCESSED);
    bulkNeedRevisionDeletionData.setDeletionProcessCode("bulkProcessCode");
    ProductCodeAndNameDetails productCodeAndNameDetails = new ProductCodeAndNameDetails();
    productCodeAndNameDetails.setProductName("product123");
    productCodeAndNameDetails.setProductCode("dd");
    ObjectMapper objectMapper = new ObjectMapper();
    bulkNeedRevisionDeletionData.setProductData(objectMapper.writeValueAsString(productCodeAndNameDetails));
    BulkNeedRevisionDeletion deletionEntity1 = new BulkNeedRevisionDeletion();
    deletionEntity1.setDeletionProcessCode("122");
    deletionEntity1.setTotalCount(10);
    deletionEntity1.setStatus(BulkNeedRevisionDeletion.STATUS_PUBLISHED);
    when(mockPage.getContent()).thenReturn(Collections.singletonList(deletionEntity));
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(anyString(),
        anyString(), any())).thenReturn(mockPage);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusOrderByCreatedDateAsc(anyString(), anyString(),
        any())).thenReturn(Collections.singletonList(deletionEntity1));
    when(needRevisionDeletionDataRepository.findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(anyString(),
        anyList(),any())).thenReturn(Arrays.asList(bulkNeedRevisionDeletionData));
    doNothing().when(fileStorageService).uploadFileToBulkBucket(anyString(), any());
    doNothing().when(notificationService).sendNeedRevisionDeletionNotification(any(), anyString(), anyInt(), anyInt());
    needRevisionDeletionService.sendNotificationForNeedRevisionDeletion("store123", "req456");
    verify(notificationService, times(1)).sendNeedRevisionDeletionNotification(any(), any(), anyInt(), anyInt());
    verify(needRevisionDeletionRepository,times(2)).saveAll(any());
    verify(needRevisionDeletionRepository, times(1)).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
        anyString(), anyString(), any());
    verify(needRevisionDeletionRepository).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(anyString(),
        anyString(), any());
    verify(needRevisionDeletionRepository).findByStoreIdAndStatusOrderByCreatedDateAsc(anyString(), anyString(), any());
    verify(needRevisionDeletionDataRepository,times(2)).findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(anyString(), any(),any());
  }

  @Test
  void testSendNotificationForNeedRevisionDeletionStatusAbortedTest() throws Exception {
    ReflectionTestUtils.setField(needRevisionDeletionService, "fetchPublishedCountForNeedRevisionDeletion", BATCH_SIZE);
    Page<BulkNeedRevisionDeletion> mockPage = mock(Page.class);
    BulkNeedRevisionDeletion deletionEntity = new BulkNeedRevisionDeletion();
    deletionEntity.setDeletionProcessCode("process123");
    deletionEntity.setTotalCount(1);
    deletionEntity.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
    when(mockPage.getContent()).thenReturn(Collections.singletonList(deletionEntity));
    when(needRevisionDeletionRepository.findByStoreIdAndProcessTypeAndStatus(anyString(), anyString(), anyString(),
        any())).thenReturn(mockPage);
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData.setEligibleForDeletion(true);
    bulkNeedRevisionDeletionData.setStatus(BulkNeedRevisionDeletion.STATUS_FAIL);
    bulkNeedRevisionDeletionData.setDeletionProcessCode("bulkProcessCode");
    ProductCodeAndNameDetails productCodeAndNameDetails = new ProductCodeAndNameDetails();
    productCodeAndNameDetails.setProductName("product123");
    ObjectMapper objectMapper1 = new ObjectMapper();
    bulkNeedRevisionDeletionData.setProductData(objectMapper1.writeValueAsString(productCodeAndNameDetails));
    BulkNeedRevisionDeletion deletionEntity1 = new BulkNeedRevisionDeletion();
    deletionEntity1.setDeletionProcessCode("122");
    deletionEntity1.setTotalCount(10);
    deletionEntity1.setStatus(BulkNeedRevisionDeletion.STATUS_PUBLISHED);
    when(mockPage.getContent()).thenReturn(Collections.singletonList(deletionEntity));
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(anyString(),
        anyString(), any())).thenReturn(mockPage);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusOrderByCreatedDateAsc(anyString(), anyString(),
        any())).thenReturn(Collections.singletonList(deletionEntity1));
    when(needRevisionDeletionDataRepository.findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(anyString(),
        anyList(),any())).thenReturn(Arrays.asList(bulkNeedRevisionDeletionData));
    doNothing().when(fileStorageService).uploadFileToBulkBucket(anyString(), any());
    doNothing().when(notificationService).sendNeedRevisionDeletionNotification(any(), anyString(), anyInt(), anyInt());
    needRevisionDeletionService.sendNotificationForNeedRevisionDeletion("store123", "req456");
    verify(needRevisionDeletionRepository, times(1)).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
        anyString(), anyString(), any());
    verify(notificationService, times(1)).sendNeedRevisionDeletionNotification(any(), any(), anyInt(), anyInt());
    verify(needRevisionDeletionRepository,times(2)).saveAll(any());
    verify(needRevisionDeletionRepository, times(1)).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
        anyString(), anyString(), any());
    verify(needRevisionDeletionRepository).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(anyString(),
        anyString(), any());
    verify(needRevisionDeletionRepository).findByStoreIdAndStatusOrderByCreatedDateAsc(anyString(), anyString(), any());
    verify(needRevisionDeletionDataRepository,times(2)).findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(anyString(), any(),any());
  }



  @Test
  public void testProcessNeedRevisionDeletion_withEmptyList()
      throws ApplicationException, JsonProcessingException {
    Integer fetchProcessCountForDeletion = 5;

    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, fetchProcessCountForDeletion))).thenReturn(Collections.emptyList());

    needRevisionDeletionService.processNeedRevisionDeletion(STORE_ID, fetchProcessCountForDeletion);

    verify(needRevisionDeletionRepository, times(0)).save(any());
    verify(needRevisionDeletionDataRepository, times(0)).saveAll(any());
    verify(
      needRevisionDeletionRepository).findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, 5));
  }

  @Test
  public void testProcessNeedRevisionDeletion_withValidData()
      throws ApplicationException, JsonProcessingException {
    Integer fetchProcessCountForDeletion = 5;
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
    bulkNeedRevisionDeletion.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletion.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkNeedRevisionDeletion.setStatus(BulkNeedRevisionDeletion.STATUS_IN_PROGRESS);
    DeleteProductRequest deleteProductRequest = new DeleteProductRequest();
    deleteProductRequest.setProductCode(PRODUCT_CODE);
    deleteProductRequest.setNotes(BULK_NEED_REVISION_POST_LIVE_DELETION);
    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionList =
      List.of(bulkNeedRevisionDeletion);
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletionData.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
    bulkNeedRevisionDeletionData.setProductCode(PRODUCT_CODE);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, fetchProcessCountForDeletion))).thenReturn(bulkNeedRevisionDeletionList);
    NeedRevisionEligibilityResponse needRevisionEligibilityResponse =
      new NeedRevisionEligibilityResponse();
    needRevisionEligibilityResponse.setEligibleForDeletion(true);
    needRevisionEligibilityResponse.setProductCode(PRODUCT_CODE);
    when(needRevisionDeletionDataRepository.findByStoreIdAndDeletionProcessCode(eq(STORE_ID),
      eq(DELETION_PROCESS_CODE)))
      .thenReturn(List.of(bulkNeedRevisionDeletionData));
    NeedRevisionEligibilityRequest needRevisionEligibilityRequest =
      NeedRevisionEligibilityRequest.builder().productCode(PRODUCT_CODE)
        .businessPartnerCode(BUSINESS_PARTNER_CODE).build();
    when(productRepository.getEligibilityForNeedRevisionDeletion(eq(STORE_ID),
      eq(List.of(needRevisionEligibilityRequest))))
      .thenReturn(List.of(needRevisionEligibilityResponse));
    when(productRepository.deleteProductCollection(anyString(), any(), anyBoolean())).thenReturn(true);
    when(needRevisionDeletionRepository.save(Mockito.any())).thenReturn(bulkNeedRevisionDeletion);
    needRevisionDeletionService.processNeedRevisionDeletion(STORE_ID, fetchProcessCountForDeletion);
    verify(
      needRevisionDeletionRepository).findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, 5));
    verify(needRevisionDeletionRepository).save(Mockito.any());
    verify(needRevisionDeletionDataRepository).findByStoreIdAndDeletionProcessCode(STORE_ID,
        DELETION_PROCESS_CODE);
  }

  @Test
  public void testProcessNeedRevisionDeletion_withValidPBPStatusNotDeletedData()
      throws ApplicationException, JsonProcessingException {
    Integer fetchProcessCountForDeletion = 5;
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
    bulkNeedRevisionDeletion.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletion.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkNeedRevisionDeletion.setStatus(BulkNeedRevisionDeletion.STATUS_IN_PROGRESS);
    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionList = List.of(bulkNeedRevisionDeletion);
    DeleteProductRequest deleteProductRequest = new DeleteProductRequest();
    deleteProductRequest.setProductCode(PRODUCT_CODE);
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletionData.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
    bulkNeedRevisionDeletionData.setProductCode(PRODUCT_CODE);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, fetchProcessCountForDeletion))).thenReturn(bulkNeedRevisionDeletionList);
    NeedRevisionEligibilityResponse needRevisionEligibilityResponse =
      new NeedRevisionEligibilityResponse();
    needRevisionEligibilityResponse.setEligibleForDeletion(true);
    needRevisionEligibilityResponse.setProductCode(PRODUCT_CODE);
    when(needRevisionDeletionDataRepository.findByStoreIdAndDeletionProcessCode(STORE_ID,
      DELETION_PROCESS_CODE)).thenReturn(List.of(bulkNeedRevisionDeletionData));
    NeedRevisionEligibilityRequest needRevisionEligibilityRequest =
      NeedRevisionEligibilityRequest.builder().productCode(PRODUCT_CODE)
        .businessPartnerCode(BUSINESS_PARTNER_CODE).build();
    when(productRepository.getEligibilityForNeedRevisionDeletion(anyString(),
     anyList())).thenReturn(
      List.of(needRevisionEligibilityResponse));
    when(productRepository.deleteProductCollection(eq(STORE_ID), eq(deleteProductRequest),
      eq(false))).thenReturn(
      false);
    when(needRevisionDeletionRepository.save(Mockito.any())).thenReturn(bulkNeedRevisionDeletion);
    needRevisionDeletionService.processNeedRevisionDeletion(STORE_ID, fetchProcessCountForDeletion);
    verify(
      needRevisionDeletionRepository).findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, 5));
    verify(needRevisionDeletionRepository).save(Mockito.any());
    verify(needRevisionDeletionDataRepository).findByStoreIdAndDeletionProcessCode(STORE_ID,
        DELETION_PROCESS_CODE);
  }

  @Test
  public void testProcessNeedRevisionDeletion_withValidPBPStatusNotEligibleData()
      throws ApplicationException, JsonProcessingException {
    Integer fetchProcessCountForDeletion = 5;
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
    bulkNeedRevisionDeletion.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletion.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkNeedRevisionDeletion.setStatus(BulkNeedRevisionDeletion.STATUS_IN_PROGRESS);
    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionList = List.of(bulkNeedRevisionDeletion);
    DeleteProductRequest deleteProductRequest = new DeleteProductRequest();
    deleteProductRequest.setProductCode(PRODUCT_CODE);
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletionData.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
    bulkNeedRevisionDeletionData.setProductCode(PRODUCT_CODE);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, fetchProcessCountForDeletion))).thenReturn(bulkNeedRevisionDeletionList);
    NeedRevisionEligibilityResponse needRevisionEligibilityResponse =
      new NeedRevisionEligibilityResponse();
    needRevisionEligibilityResponse.setEligibleForDeletion(false);
    needRevisionEligibilityResponse.setProductCode(PRODUCT_CODE);
    when(needRevisionDeletionDataRepository.findByStoreIdAndDeletionProcessCode(STORE_ID,
      DELETION_PROCESS_CODE)).thenReturn(List.of(bulkNeedRevisionDeletionData));
    NeedRevisionEligibilityRequest needRevisionEligibilityRequest =
      NeedRevisionEligibilityRequest.builder().productCode(PRODUCT_CODE)
        .businessPartnerCode(BUSINESS_PARTNER_CODE).build();
    when(productRepository.getEligibilityForNeedRevisionDeletion(STORE_ID, List.of(needRevisionEligibilityRequest))).thenReturn(
      List.of(needRevisionEligibilityResponse));
    when(productRepository.deleteProductCollection(STORE_ID, deleteProductRequest, false)).thenReturn(
      false);
    when(needRevisionDeletionRepository.save(Mockito.any())).thenReturn(bulkNeedRevisionDeletion);
    needRevisionDeletionService.processNeedRevisionDeletion(STORE_ID, fetchProcessCountForDeletion);
    verify(
      needRevisionDeletionRepository).findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, 5));
    verify(needRevisionDeletionRepository).save(Mockito.any());
    verify(needRevisionDeletionDataRepository).findByStoreIdAndDeletionProcessCode(STORE_ID,
        DELETION_PROCESS_CODE);
  }


  @Test
  public void testProcessNeedRevisionDeletion_withValidFalseData()
      throws ApplicationException, JsonProcessingException {
    Integer fetchProcessCountForDeletion = 5;
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
    bulkNeedRevisionDeletion.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletion.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkNeedRevisionDeletion.setStatus(BulkNeedRevisionDeletion.STATUS_IN_PROGRESS);
    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionList =
      List.of(bulkNeedRevisionDeletion);
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletionData.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
    bulkNeedRevisionDeletionData.setProductCode(PRODUCT_CODE);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, fetchProcessCountForDeletion))).thenReturn(bulkNeedRevisionDeletionList);
    NeedRevisionEligibilityResponse needRevisionEligibilityResponse =
      new NeedRevisionEligibilityResponse();
    needRevisionEligibilityResponse.setEligibleForDeletion(false);
    needRevisionEligibilityResponse.setProductCode(PRODUCT_CODE);
    when(needRevisionDeletionDataRepository.findByStoreIdAndDeletionProcessCode(STORE_ID, DELETION_PROCESS_CODE))
      .thenReturn(List.of(bulkNeedRevisionDeletionData));
    NeedRevisionEligibilityRequest needRevisionEligibilityRequest =
      NeedRevisionEligibilityRequest.builder().productCode(PRODUCT_CODE)
        .businessPartnerCode(BUSINESS_PARTNER_CODE).build();
    when(productRepository.getEligibilityForNeedRevisionDeletion(STORE_ID, List.of(needRevisionEligibilityRequest)))
      .thenReturn(List.of(needRevisionEligibilityResponse));
    when(needRevisionDeletionRepository.save(Mockito.any())).thenReturn(bulkNeedRevisionDeletion);
    needRevisionDeletionService.processNeedRevisionDeletion(STORE_ID, fetchProcessCountForDeletion);
    verify(
      needRevisionDeletionRepository).findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, 5));
    verify(needRevisionDeletionRepository).save(Mockito.any());
    verify(needRevisionDeletionDataRepository).findByStoreIdAndDeletionProcessCode(STORE_ID,
        DELETION_PROCESS_CODE);
  }


  @Test
  public void testProcessNeedRevisionDeletion_withInValidData()
      throws ApplicationException {
    int fetchProcessCountForDeletion = 5;
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
    bulkNeedRevisionDeletion.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletion.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkNeedRevisionDeletion.setStatus(BulkNeedRevisionDeletion.STATUS_IN_PROGRESS);
    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionList =
      List.of(bulkNeedRevisionDeletion);
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletionData.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
    bulkNeedRevisionDeletionData.setProductCode(PRODUCT_CODE);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, fetchProcessCountForDeletion))).thenReturn(bulkNeedRevisionDeletionList);
    when(needRevisionDeletionRepository.save(Mockito.any())).thenReturn(bulkNeedRevisionDeletion);
    NeedRevisionEligibilityResponse needRevisionEligibilityResponse =
      new NeedRevisionEligibilityResponse();
    needRevisionEligibilityResponse.setEligibleForDeletion(true);
    needRevisionEligibilityResponse.setProductCode(PRODUCT_CODE);
    when(needRevisionDeletionDataRepository.findByStoreIdAndDeletionProcessCode(STORE_ID, DELETION_PROCESS_CODE))
      .thenReturn(List.of(bulkNeedRevisionDeletionData));
    NeedRevisionEligibilityRequest needRevisionEligibilityRequest =
      NeedRevisionEligibilityRequest.builder().productCode(PRODUCT_CODE)
        .businessPartnerCode(BUSINESS_PARTNER_CODE).build();
    when(productRepository.getEligibilityForNeedRevisionDeletion(STORE_ID, List.of(needRevisionEligibilityRequest)))
      .thenReturn(List.of(needRevisionEligibilityResponse));

    needRevisionDeletionService.processNeedRevisionDeletion(STORE_ID, fetchProcessCountForDeletion);
    verify(
      needRevisionDeletionRepository).findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, 5));
    verify(needRevisionDeletionDataRepository).findByStoreIdAndDeletionProcessCode(STORE_ID,
        DELETION_PROCESS_CODE);
    verify(needRevisionDeletionRepository).save(Mockito.any());
  }

  @Test
  public void testProcessNeedRevisionDeletion_withInValidDataAndNotEligible()
      throws ApplicationException, JsonProcessingException {
    Integer fetchProcessCountForDeletion = 5;
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
    bulkNeedRevisionDeletion.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletion.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkNeedRevisionDeletion.setStatus(BulkNeedRevisionDeletion.STATUS_IN_PROGRESS);
    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionList =
      List.of(bulkNeedRevisionDeletion);
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletionData.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
    bulkNeedRevisionDeletionData.setProductCode(PRODUCT_CODE);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, fetchProcessCountForDeletion))).thenReturn(bulkNeedRevisionDeletionList);
    NeedRevisionEligibilityResponse needRevisionEligibilityResponse =
      new NeedRevisionEligibilityResponse();
    needRevisionEligibilityResponse.setEligibleForDeletion(null);
    needRevisionEligibilityResponse.setProductCode(PRODUCT_CODE);
    when(needRevisionDeletionDataRepository.findByStoreIdAndDeletionProcessCode(STORE_ID, DELETION_PROCESS_CODE))
      .thenReturn(List.of(bulkNeedRevisionDeletionData));
    NeedRevisionEligibilityRequest needRevisionEligibilityRequest =
      NeedRevisionEligibilityRequest.builder().productCode(PRODUCT_CODE)
        .businessPartnerCode(BUSINESS_PARTNER_CODE).build();
    when(productRepository.getEligibilityForNeedRevisionDeletion(STORE_ID, List.of(needRevisionEligibilityRequest)))
      .thenReturn(List.of(needRevisionEligibilityResponse));
    when(needRevisionDeletionRepository.save(Mockito.any())).thenReturn(bulkNeedRevisionDeletion);
    needRevisionDeletionService.processNeedRevisionDeletion(STORE_ID, fetchProcessCountForDeletion);
    verify(
      needRevisionDeletionRepository).findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, 5));
    verify(needRevisionDeletionRepository).save(Mockito.any());
    verify(needRevisionDeletionDataRepository).findByStoreIdAndDeletionProcessCode(STORE_ID,
        DELETION_PROCESS_CODE);
  }

  @Test
  public void testProcessNeedRevisionDeletion_withNullEligibilityResponse()
      throws ApplicationException, JsonProcessingException {
    Integer fetchProcessCountForDeletion = 5;
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
    bulkNeedRevisionDeletion.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletion.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkNeedRevisionDeletion.setStatus(BulkNeedRevisionDeletion.STATUS_IN_PROGRESS);
    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionList =
      List.of(bulkNeedRevisionDeletion);
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletionData.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
    bulkNeedRevisionDeletionData.setProductCode(PRODUCT_CODE);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, fetchProcessCountForDeletion))).thenReturn(bulkNeedRevisionDeletionList);

    when(needRevisionDeletionDataRepository.findByStoreIdAndDeletionProcessCode(STORE_ID, DELETION_PROCESS_CODE))
      .thenReturn(List.of(bulkNeedRevisionDeletionData));

    when(productRepository.getEligibilityForNeedRevisionDeletion(anyString(), anyList()))
      .thenReturn(Collections.emptyList());
    when(needRevisionDeletionRepository.save(Mockito.any())).thenReturn(bulkNeedRevisionDeletion);
    needRevisionDeletionService.processNeedRevisionDeletion(STORE_ID, fetchProcessCountForDeletion);
    verify(
      needRevisionDeletionRepository).findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, 5));
    verify(needRevisionDeletionRepository).save(Mockito.any());
    verify(needRevisionDeletionDataRepository).findByStoreIdAndDeletionProcessCode(STORE_ID,
        DELETION_PROCESS_CODE);
  }

  @Test
  public void testProcessNeedRevisionDeletion_withNotEligibleProductForDeletion()
      throws ApplicationException, JsonProcessingException {
    Integer fetchProcessCountForDeletion = 5;
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
    bulkNeedRevisionDeletion.setDeletionProcessCode(DELETION_PROCESS_CODE);
    bulkNeedRevisionDeletion.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkNeedRevisionDeletion.setStatus(BulkNeedRevisionDeletion.STATUS_IN_PROGRESS);
    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletionList =
      List.of(bulkNeedRevisionDeletion);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, fetchProcessCountForDeletion))).thenReturn(bulkNeedRevisionDeletionList);

    when(needRevisionDeletionDataRepository.findByStoreIdAndDeletionProcessCode(eq(STORE_ID),
      anyString()))
      .thenReturn(List.of(new BulkNeedRevisionDeletionData()));

    NeedRevisionEligibilityResponse eligibilityResponse = new NeedRevisionEligibilityResponse();
    eligibilityResponse.setEligibleForDeletion(false);
    when(productRepository.getEligibilityForNeedRevisionDeletion(eq(STORE_ID), anyList()))
      .thenReturn(List.of(eligibilityResponse));
    when(needRevisionDeletionRepository.save(Mockito.any())).thenReturn(bulkNeedRevisionDeletion);
    needRevisionDeletionService.processNeedRevisionDeletion(STORE_ID, fetchProcessCountForDeletion);
    verify(
      needRevisionDeletionRepository).findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
      STORE_ID, BulkNeedRevisionDeletion.STATUS_IN_PROGRESS, NEED_REVISION_DELETION_TYPE,
      PageRequest.of(0, 5));
    verify(needRevisionDeletionRepository).save(Mockito.any());
    verify(needRevisionDeletionDataRepository).findByStoreIdAndDeletionProcessCode(STORE_ID,
        DELETION_PROCESS_CODE);
  }


  @Test
  public void testFetchProductsOfABusinessPartner_Success() throws Exception {
    String businessPartnerBatchSize = "10";
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(businessPartnerBatchSize);

    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletions = new ArrayList<>();
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
    bulkNeedRevisionDeletion.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkNeedRevisionDeletions.add(bulkNeedRevisionDeletion);

    Page<BulkNeedRevisionDeletion> page = new PageImpl<>(bulkNeedRevisionDeletions);
    ProductCodeAndNameDetails productCodeAndNameDetails = new ProductCodeAndNameDetails();
    productCodeAndNameDetails.setProductCode(PRODUCT_CODE);
    productCodeAndNameDetails.setProductName(PRODUCT_NAME);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        "nr_deletion_fetch_products_with_bp_codes_size")).thenReturn(systemParameterConfig);
    when(
        needRevisionDeletionRepository.findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
            eq(STORE_ID), eq(BulkNeedRevisionDeletion.STATUS_PENDING),
            any(PageRequest.class))).thenReturn(page);

    ProductCodeAndNameResponseList productResponseList = new ProductCodeAndNameResponseList();
    productResponseList.setProducts(Collections.singletonList(productCodeAndNameDetails));
    when(pbpOutboundService.getProductDetailsOfBusinessPartnerFromPBP(eq(STORE_ID), eq(REQUEST_ID),
        eq(BUSINESS_PARTNER_CODE))).thenReturn(productResponseList);
    when(needRevisionDeletionRepository.saveAll(Mockito.anyList())).thenReturn(
        bulkNeedRevisionDeletions);

    needRevisionDeletionService.fetchProductsOfABusinessPartner(STORE_ID, REQUEST_ID);

    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        "nr_deletion_fetch_products_with_bp_codes_size");
    verify(
        needRevisionDeletionRepository).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
        eq(STORE_ID), eq(BulkNeedRevisionDeletion.STATUS_PENDING), any(PageRequest.class));
    verify(pbpOutboundService).getProductDetailsOfBusinessPartnerFromPBP(eq(STORE_ID),
        eq(REQUEST_ID), eq(BUSINESS_PARTNER_CODE));
    verify(bulkNeedRevisionDeletionDataRepository).saveAll(anyList());
    verify(needRevisionDeletionRepository, times(3)).saveAll(anyList());
    verify(objectMapper).writeValueAsString(Mockito.any());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeList(Mockito.any(),
        Mockito.anyInt(), Mockito.anyInt());
  }


  @Test
  void testFetchProductsOfABusinessPartner_productCodeAndNameResponseListNull_Test()
      throws Exception {
    String businessPartnerBatchSize = "10";
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(businessPartnerBatchSize);

    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletions = new ArrayList<>();
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
    bulkNeedRevisionDeletion.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkNeedRevisionDeletions.add(bulkNeedRevisionDeletion);

    Page<BulkNeedRevisionDeletion> page = new PageImpl<>(bulkNeedRevisionDeletions);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        "nr_deletion_fetch_products_with_bp_codes_size")).thenReturn(systemParameterConfig);
    when(
        needRevisionDeletionRepository.findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
            eq(STORE_ID), eq(BulkNeedRevisionDeletion.STATUS_PENDING),
            any(PageRequest.class))).thenReturn(page);

    when(pbpOutboundService.getProductDetailsOfBusinessPartnerFromPBP(eq(STORE_ID), eq(REQUEST_ID),
        eq(BUSINESS_PARTNER_CODE))).thenReturn(null);
    when(needRevisionDeletionRepository.saveAll(Mockito.anyList())).thenReturn(
        bulkNeedRevisionDeletions);

    needRevisionDeletionService.fetchProductsOfABusinessPartner(STORE_ID, REQUEST_ID);

    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        "nr_deletion_fetch_products_with_bp_codes_size");
    verify(
        needRevisionDeletionRepository).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
        eq(STORE_ID), eq(BulkNeedRevisionDeletion.STATUS_PENDING), any(PageRequest.class));
    verify(pbpOutboundService).getProductDetailsOfBusinessPartnerFromPBP(eq(STORE_ID),
        eq(REQUEST_ID), eq(BUSINESS_PARTNER_CODE));
    verify(needRevisionDeletionRepository, times(3)).saveAll(anyList());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeList(Mockito.any(),
        Mockito.anyInt(), Mockito.anyInt());
  }

  @Test
  void testFetchProductsOfABusinessPartner_productCodeAndNameResponseListEmpty() throws Exception {
    String businessPartnerBatchSize = "10";
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(businessPartnerBatchSize);

    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletions = new ArrayList<>();
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
    bulkNeedRevisionDeletion.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkNeedRevisionDeletions.add(bulkNeedRevisionDeletion);

    Page<BulkNeedRevisionDeletion> page = new PageImpl<>(bulkNeedRevisionDeletions);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        "nr_deletion_fetch_products_with_bp_codes_size")).thenReturn(systemParameterConfig);
    when(
        needRevisionDeletionRepository.findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
            eq(STORE_ID), eq(BulkNeedRevisionDeletion.STATUS_PENDING),
            any(PageRequest.class))).thenReturn(page);

    ProductCodeAndNameResponseList productResponseList = new ProductCodeAndNameResponseList();
    productResponseList.setProducts(new ArrayList<>());
    when(pbpOutboundService.getProductDetailsOfBusinessPartnerFromPBP(eq(STORE_ID), eq(REQUEST_ID),
        eq(BUSINESS_PARTNER_CODE))).thenReturn(productResponseList);
    when(needRevisionDeletionRepository.saveAll(Mockito.anyList())).thenReturn(
        bulkNeedRevisionDeletions);

    needRevisionDeletionService.fetchProductsOfABusinessPartner(STORE_ID, REQUEST_ID);

    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        "nr_deletion_fetch_products_with_bp_codes_size");
    verify(
        needRevisionDeletionRepository).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
        eq(STORE_ID), eq(BulkNeedRevisionDeletion.STATUS_PENDING), any(PageRequest.class));
    verify(pbpOutboundService).getProductDetailsOfBusinessPartnerFromPBP(eq(STORE_ID),
        eq(REQUEST_ID), eq(BUSINESS_PARTNER_CODE));
    verify(needRevisionDeletionRepository, times(3)).saveAll(anyList());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeList(Mockito.any(),
        Mockito.anyInt(), Mockito.anyInt());
  }

  @Test
  void testFetchProductsOfABusinessPartner_Failure() throws Exception {
    String businessPartnerBatchSize = "10";
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(businessPartnerBatchSize);

    List<BulkNeedRevisionDeletion> bulkNeedRevisionDeletions = new ArrayList<>();
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
    bulkNeedRevisionDeletion.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkNeedRevisionDeletions.add(bulkNeedRevisionDeletion);

    Page<BulkNeedRevisionDeletion> page = new PageImpl<>(bulkNeedRevisionDeletions);
    ProductCodeAndNameDetails productCodeAndNameDetails = new ProductCodeAndNameDetails();
    productCodeAndNameDetails.setProductCode(PRODUCT_CODE);
    productCodeAndNameDetails.setProductName(PRODUCT_NAME);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        "nr_deletion_fetch_products_with_bp_codes_size")).thenReturn(systemParameterConfig);
    when(
        needRevisionDeletionRepository.findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
            eq(STORE_ID), eq(BulkNeedRevisionDeletion.STATUS_PENDING),
            any(PageRequest.class))).thenReturn(page);
    ProductCodeAndNameResponseList productResponseList = new ProductCodeAndNameResponseList();
    productResponseList.setProducts(Collections.singletonList(productCodeAndNameDetails));
    Mockito.doThrow(new ApplicationRuntimeException()).when(pbpOutboundService)
        .getProductDetailsOfBusinessPartnerFromPBP(eq(STORE_ID), eq(REQUEST_ID),
            eq(BUSINESS_PARTNER_CODE));
    when(needRevisionDeletionRepository.saveAll(Mockito.anyList())).thenReturn(
        bulkNeedRevisionDeletions);
    needRevisionDeletionService.fetchProductsOfABusinessPartner(STORE_ID, REQUEST_ID);

    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        "nr_deletion_fetch_products_with_bp_codes_size");
    verify(
        needRevisionDeletionRepository).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
        eq(STORE_ID), eq(BulkNeedRevisionDeletion.STATUS_PENDING), any(PageRequest.class));
    verify(pbpOutboundService).getProductDetailsOfBusinessPartnerFromPBP(eq(STORE_ID),
        eq(REQUEST_ID), eq(BUSINESS_PARTNER_CODE));
    verify(needRevisionDeletionRepository, times(3)).saveAll(anyList());
    verify(businessPartnerRepository).filterByBusinessPartnerCodeList(Mockito.any(),
        Mockito.anyInt(), Mockito.anyInt());
  }

  @Test
  public void saveBulkNeedRevisionDeletionTest() {
    needRevisionDeletionService.saveBulkNeedRevisionDeletion(
        BulkNeedRevisionDeletion.builder().deletionProcessCode(DELETION_PROCESS_CODE).build());
    Mockito.verify(needRevisionDeletionRepository).save(Mockito.any());
  }

  @Test
  public void fetchNeedRevisionDeletionByDeletionProcessCodeTest() {
    needRevisionDeletionService.fetchNeedRevisionDeletionByDeletionProcessCode(STORE_ID,
        DELETION_PROCESS_CODE);
    Mockito.verify(needRevisionDeletionRepository)
        .findByStoreIdAndDeletionProcessCode(STORE_ID, DELETION_PROCESS_CODE);
  }

  @Test
  public void performEligibilityCheckAndProcessDataDeletionTest() throws ApplicationException {
    DeleteProductRequest deleteRequest = new DeleteProductRequest();
    deleteRequest.setProductCode(PRODUCT_CODE);
    deleteRequest.setProductName(PRODUCT_NAME);
    deleteRequest.setNotes(Constant.BULK_NEED_REVISION_POST_LIVE_DELETION);
    DeleteProductRequest deleteRequest2 = new DeleteProductRequest();
    deleteRequest2.setProductCode(PRODUCT_CODE_2);
    deleteRequest2.setProductName(PRODUCT_NAME);
    deleteRequest2.setNotes(Constant.BULK_NEED_REVISION_POST_LIVE_DELETION);
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData2 = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData2.setProductCode(PRODUCT_CODE_2);
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData3 = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData3.setProductCode(PRODUCT_CODE_2 + "3");
    NeedRevisionEligibilityResponse needRevisionEligibilityResponse =
        new NeedRevisionEligibilityResponse();
    needRevisionEligibilityResponse.setProductCode(PRODUCT_CODE);
    needRevisionEligibilityResponse.setProductName(PRODUCT_NAME);
    needRevisionEligibilityResponse.setEligibleForDeletion(true);
    NeedRevisionEligibilityResponse needRevisionEligibilityResponse2 =
        new NeedRevisionEligibilityResponse();
    needRevisionEligibilityResponse2.setProductCode(PRODUCT_CODE_2);
    needRevisionEligibilityResponse2.setEligibleForDeletion(true);
    needRevisionEligibilityResponse2.setProductName(PRODUCT_NAME);

    NeedRevisionEligibilityResponse needRevisionEligibilityResponse3 =
        new NeedRevisionEligibilityResponse();
    needRevisionEligibilityResponse3.setProductCode(PRODUCT_CODE_2 + "3");
    needRevisionEligibilityResponse3.setEligibleForDeletion(false);
    needRevisionEligibilityResponse3.setProductName(PRODUCT_NAME);
    Mockito.when(productRepository.getEligibilityForNeedRevisionDeletion(Mockito.anyString(),
        Mockito.anyList())).thenReturn(
        Arrays.asList(needRevisionEligibilityResponse, needRevisionEligibilityResponse2,
            needRevisionEligibilityResponse3));
    Mockito.when(productRepository.deleteProductCollection(Mockito.anyString(), Mockito.any(),
            Mockito.anyBoolean())).thenReturn(true).thenThrow(new ApplicationRuntimeException())
        .thenReturn(false);
    needRevisionDeletionService.performEligibilityCheckAndProcessDataDeletion(STORE_ID,
        Arrays.asList(bulkNeedRevisionDeletionData, bulkNeedRevisionDeletionData2,
            bulkNeedRevisionDeletionData3, new BulkNeedRevisionDeletionData()),
        BUSINESS_PARTNER_CODE);
    Mockito.verify(needRevisionDeletionDataRepository).saveAll(Mockito.anyList());
    Mockito.verify(productRepository)
        .getEligibilityForNeedRevisionDeletion(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(productRepository, times(2))
        .deleteProductCollection(Mockito.anyString(), Mockito.any(), Mockito.anyBoolean());
  }

  @Test
  public void performEligibilityCheckAndProcessDataDeletionTest_Status_Fail() throws ApplicationException {
    DeleteProductRequest deleteRequest = new DeleteProductRequest();
    deleteRequest.setProductCode(PRODUCT_CODE);
    deleteRequest.setProductName(PRODUCT_NAME);
    deleteRequest.setNotes(Constant.BULK_NEED_REVISION_POST_LIVE_DELETION);
    NeedRevisionEligibilityResponse needRevisionEligibilityResponse =
        new NeedRevisionEligibilityResponse();
    needRevisionEligibilityResponse.setProductCode(PRODUCT_CODE);
    needRevisionEligibilityResponse.setProductName(PRODUCT_NAME);
    needRevisionEligibilityResponse.setEligibleForDeletion(true);
    Mockito.when(productRepository.getEligibilityForNeedRevisionDeletion(Mockito.anyString(),
        Mockito.anyList())).thenReturn(Arrays.asList(needRevisionEligibilityResponse));
    Mockito.when(productRepository.deleteProductCollection(Mockito.anyString(), Mockito.any(),
        Mockito.anyBoolean())).thenReturn(false);
    needRevisionDeletionService.performEligibilityCheckAndProcessDataDeletion(STORE_ID,
        Arrays.asList(bulkNeedRevisionDeletionData), BUSINESS_PARTNER_CODE);
    Mockito.verify(needRevisionDeletionDataRepository).saveAll(Mockito.anyList());
    Mockito.verify(productRepository)
        .getEligibilityForNeedRevisionDeletion(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(productRepository)
        .deleteProductCollection(Mockito.anyString(), Mockito.any(), Mockito.anyBoolean());
  }

  @Test
  void testSendNotificationForNeedRevisionDeletionCheckingFinishedStateTest() throws Exception {
    ReflectionTestUtils.setField(needRevisionDeletionService, "fetchPublishedCountForNeedRevisionDeletion", BATCH_SIZE);
    Page<BulkNeedRevisionDeletion> mockPage = mock(Page.class);
    BulkNeedRevisionDeletion deletionEntity = new BulkNeedRevisionDeletion();
    deletionEntity.setDeletionProcessCode("process123");
    deletionEntity.setTotalCount(1);
    deletionEntity.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
    when(mockPage.getContent()).thenReturn(Collections.singletonList(deletionEntity));
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData.setEligibleForDeletion(true);
    bulkNeedRevisionDeletionData.setStatus(BulkNeedRevisionDeletion.STATUS_FINISHED);
    bulkNeedRevisionDeletionData.setDeletionProcessCode("bulkProcessCode");
    ProductCodeAndNameDetails productCodeAndNameDetails = new ProductCodeAndNameDetails();
    productCodeAndNameDetails.setProductName("product123");
    ObjectMapper objectMapper1 = new ObjectMapper();
    bulkNeedRevisionDeletionData.setProductData(objectMapper1.writeValueAsString(productCodeAndNameDetails));
    BulkNeedRevisionDeletion deletionEntity1 = new BulkNeedRevisionDeletion();
    deletionEntity1.setDeletionProcessCode("122");
    deletionEntity1.setTotalCount(10);
    deletionEntity1.setStatus(BulkNeedRevisionDeletion.STATUS_PUBLISHED);
    when(mockPage.getContent()).thenReturn(Collections.singletonList(deletionEntity));
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(anyString(),
        anyString(), any())).thenReturn(mockPage);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusOrderByCreatedDateAsc(anyString(), anyString(),
        any())).thenReturn(Collections.singletonList(deletionEntity1));
    when(needRevisionDeletionDataRepository.findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(anyString(),
        anyList(),any())).thenReturn(Arrays.asList(bulkNeedRevisionDeletionData));
    doNothing().when(fileStorageService).uploadFileToBulkBucket(anyString(), any());
    doNothing().when(notificationService).sendNeedRevisionDeletionNotification(any(), anyString(), anyInt(), anyInt());
    needRevisionDeletionService.sendNotificationForNeedRevisionDeletion("store123", "req456");
    verify(notificationService, times(1)).sendNeedRevisionDeletionNotification(any(), any(), anyInt(), anyInt());
    verify(needRevisionDeletionRepository, times(1)).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
        anyString(), anyString(), any());
    verify(notificationService, times(1)).sendNeedRevisionDeletionNotification(any(), any(), anyInt(), anyInt());
    verify(needRevisionDeletionRepository, times(2)).saveAll(any());
    verify(needRevisionDeletionRepository, times(1)).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
        anyString(), anyString(), any());
    verify(needRevisionDeletionRepository).findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(anyString(),
        anyString(), any());
    verify(needRevisionDeletionRepository).findByStoreIdAndStatusOrderByCreatedDateAsc(anyString(), anyString(), any());
    verify(needRevisionDeletionDataRepository,times(2)).findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(anyString(), any(),any());
  }

  @Test
  void testSendNotificationForNeedRevisionDeletionCheckingTest() throws Exception {
    ReflectionTestUtils.setField(needRevisionDeletionService, "fetchPublishedCountForNeedRevisionDeletion", BATCH_SIZE);
    Page<BulkNeedRevisionDeletion> mockPage = mock(Page.class);
    BulkNeedRevisionDeletion deletionEntity = new BulkNeedRevisionDeletion();
    deletionEntity.setDeletionProcessCode("process123");
    deletionEntity.setTotalCount(1);
    deletionEntity.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
    when(mockPage.getContent()).thenReturn(Collections.singletonList(deletionEntity));
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData.setEligibleForDeletion(true);
    bulkNeedRevisionDeletionData.setStatus(BulkNeedRevisionDeletion.STATUS_FINISHED);
    bulkNeedRevisionDeletionData.setDeletionProcessCode("bulkProcessCode");
    ProductCodeAndNameDetails productCodeAndNameDetails = new ProductCodeAndNameDetails();
    productCodeAndNameDetails.setProductName("product123");
    ObjectMapper objectMapper1 = new ObjectMapper();
    bulkNeedRevisionDeletionData.setProductData(objectMapper1.writeValueAsString(productCodeAndNameDetails));
    BulkNeedRevisionDeletion deletionEntity1 = new BulkNeedRevisionDeletion();
    deletionEntity1.setDeletionProcessCode("bulkProcessCode");
    deletionEntity1.setTotalCount(10);
    deletionEntity1.setStatus(BulkNeedRevisionDeletion.STATUS_PUBLISHED);
    when(mockPage.getContent()).thenReturn(Collections.singletonList(deletionEntity));
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(anyString(),
        anyString(), any())).thenReturn(mockPage);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusOrderByCreatedDateAsc(anyString(), anyString(),
        any())).thenReturn(Collections.singletonList(deletionEntity1));
    when(needRevisionDeletionDataRepository.findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(anyString(),
        anyList(),any())).thenReturn(Arrays.asList(bulkNeedRevisionDeletionData));
    doNothing().when(fileStorageService).uploadFileToBulkBucket(anyString(), any());
    doNothing().when(notificationService).sendNeedRevisionDeletionNotification(any(), anyString(), anyInt(), anyInt());
    needRevisionDeletionService.sendNotificationForNeedRevisionDeletion("store123", "req456");
    verify(needRevisionDeletionRepository).findByStoreIdAndStatusOrderByCreatedDateAsc(anyString(), anyString(), any());
    verify(needRevisionDeletionDataRepository,times(2)).findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(anyString(), any(),any());
  }

  @Test
  void testSendNotificationForNeedRevisionDeletionCheckingTest1() throws Exception {
    ReflectionTestUtils.setField(needRevisionDeletionService, "fetchPublishedCountForNeedRevisionDeletion", BATCH_SIZE);
    Page<BulkNeedRevisionDeletion> mockPage = mock(Page.class);
    BulkNeedRevisionDeletion deletionEntity = new BulkNeedRevisionDeletion();
    deletionEntity.setDeletionProcessCode("process123");
    deletionEntity.setTotalCount(1);
    deletionEntity.setStatus(BulkNeedRevisionDeletion.STATUS_PENDING);
    when(mockPage.getContent()).thenReturn(Collections.singletonList(deletionEntity));
    BulkNeedRevisionDeletion deletionEntity1 = new BulkNeedRevisionDeletion();
    deletionEntity1.setDeletionProcessCode("bulkProcessCode");
    bulkNeedRevisionDeletionData.setDeletionProcessCode("bulkProcessCode");
    deletionEntity1.setTotalCount(10);
    deletionEntity1.setStatus(BulkNeedRevisionDeletion.STATUS_PUBLISHED);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
        anyString(), anyString(), any())).thenReturn(mockPage);
    when(needRevisionDeletionRepository.findByStoreIdAndStatusOrderByCreatedDateAsc(
        anyString(), anyString(), any())).thenReturn(Collections.singletonList(deletionEntity1));
    when(needRevisionDeletionDataRepository.findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(
        anyString(), anyList(),any())).thenReturn(Arrays.asList(bulkNeedRevisionDeletionData));
    doNothing().when(fileStorageService).uploadFileToBulkBucket(anyString(), any());
    doNothing().when(notificationService).sendNeedRevisionDeletionNotification(any(), anyString(), anyInt(), anyInt());
    needRevisionDeletionService.sendNotificationForNeedRevisionDeletion("store123", "req456");
    verify(needRevisionDeletionRepository).findByStoreIdAndStatusOrderByCreatedDateAsc(anyString(), anyString(), any());
    verify(needRevisionDeletionDataRepository,times(2)).findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(anyString(), any(),any());
  }

  @Test
  public void saveBulkNeedRevisionDeletionDataTest() {
    Mockito.when(needRevisionDeletionDataRepository.saveAll(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    needRevisionDeletionService.saveBulkNeedRevisionDeletionData(
        List.of(bulkNeedRevisionDeletionData));
    Mockito.verify(needRevisionDeletionDataRepository).saveAll(Mockito.anyList());
  }
}
