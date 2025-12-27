package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadDTO;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang.StringUtils;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.util.IOUtils;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.mta.bulk.BulkDownloadEntityStatus;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.UnifiedBulkDownloadException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkDownloadFileContentDTO;
import com.gdn.mta.bulk.dto.BulkDownloadMailRecipient;
import com.gdn.mta.bulk.dto.BulkDownloadProductDTO;
import com.gdn.mta.bulk.dto.BulkInternalPendingRequestResponse;
import com.gdn.mta.bulk.dto.CampaignBulkDownloadRequest;
import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.entity.BulkDownloadQueue;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.entity.UnifiedBulkDownloadEvent;
import com.gdn.mta.bulk.entity.constants.BulkProcessConstant;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.CampaignProductDownloadRequest;
import com.gdn.mta.bulk.repository.BulkDownloadAuditRepository;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.google.common.collect.ImmutableMap;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.google.common.collect.ImmutableList;
import org.apache.poi.xssf.streaming.SXSSFSheet;

/**
 * Created by virajjasani on 09/09/16.
 */
public class BulkDownloadServiceBeanTest {

  private static final Integer PRODUCT_SIZE = 10;
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String EMAIL_CC_INVALID_USERNAME = "developertest";
  private static final String DEFAULT_USERNAME = "developer@tester.com";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BPCODE";
  private static final String CAMPAIGN_CODE = "campaign_code";
  private static final String DEFAULT_REQUEST_ID = "REQUEST_ID";
  private static final String DEFAULT_FILE_NAME = "FILE_NAME";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final Map<String, Boolean> PRIVILEGE_MAP =
      ImmutableMap.<String, Boolean>builder().put("isPrivilegedToReadProductType", true)
          .put("isPrivilegedToReadPickupPoint", true).put("isPrivilegedToReadPrice", true)
          .put("isPrivilegedToReadAvailableStock", true)
          .put("isPrivilegedToReadDisplayBuyable", true).put("isPrivilegedToReadO2O", true)
          .build();
  private static final String BRAND = "Brand";
  private static final String DOWNLOAD_IN_PROGRESS = "IN_PROGRESS";
  private static final String DOWNLOAD_COMPLETE = "COMPLETE";
  private static final ClassLoader CLASS_LOADER = ClassLoader.getSystemClassLoader();
  private static final String BASE_DIRECTORY = CLASS_LOADER.getResource(StringUtils.EMPTY).getPath();
  private static final String UNIFIED_BASE_DIRECTORY = BASE_DIRECTORY + "/ExcelTemplate/";
  private static final String UNIFIED_TEMPLATE_GENERAL_PATH =
      UNIFIED_BASE_DIRECTORY + "BPCODE/general-template.xlsm";
  private static final String PICKUP_POINT_SHEET = "merchant_input";
  private static final int PICKUP_POINT_SHEET_INDEX = 7;
  private static final String BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM =
      "Blibli-mass-upload-template-delivery.xlsm,Blibli-mass-upload-template-cnc.xlsm,Blibli-mass-upload-bfb-template.xlsm,Blibli-mass-upload-bfbAndCnc-template.xlsm,Blibli-mass-upload-template-delivery.xlsm,Blibli-mass-upload-template-cnc.xlsm,Blibli-mass-upload-bfb-template.xlsm,Blibli-mass-upload-bfbAndCnc-template.xlsm,Blibli-mass-upload-template-delivery-instore.xlsm,Blibli-mass-upload-template-cnc-instore.xlsm,Blibli-mass-upload-bfb-instore-template.xlsm,Blibli-mass-upload-bfbAndCnc-instore-template.xlsm,Blibli-mass-upload-template-delivery-instore.xlsm";
  private static final String BLIBLI_MASS_UPLOAD_TEMPLATE_EN_XLSM =
      "Blibli-mass-upload-template-en-delivery.xlsm,Blibli-mass-upload-template-en-cnc.xlsm,Blibli-mass-upload-bfb-template-en.xlsm,Blibli-mass-upload-bfbAndCnc-template-en.xlsm,Blibli-mass-upload-template-delivery.xlsm,Blibli-mass-upload-template-cnc.xlsm,Blibli-mass-upload-bfb-template.xlsm,Blibli-mass-upload-bfbAndCnc-template.xlsm,Blibli-mass-upload-template-en-instore-delivery.xlsm,Blibli-mass-upload-template-en-instore-cnc.xlsm,Blibli-mass-upload-bfb-template-en-instore.xlsm,Blibli-mass-upload-bfbAndCnc-template-en-instore.xlsm,Blibli-mass-upload-template-delivery-instore.xlsm";
  public static final String MINUTE_VALUE = "60";
  public static final String FILE_PATH = "file-path";
  public static final String PICKUP_POINT_CODE = "pickup-point-code";
  public static final String PICKUP_POINT_NAME = "pickup-point-name";
  public static final String BLIBLI = "BLIBLI";

  private BulkDownloadQueue bulkDownloadQueue;
  private BulkDownloadEntity bulkDownloadEntity;
  private CampaignBulkDownloadRequest campaignBulkDownloadRequest;
  private CampaignProductDownloadRequest campaignProductDownloadRequest;
  private CampaignProductDownloadRequest campaignProductDownloadRequest1;
  private ProfileResponse businessPartner;
  private BulkDownloadMailRecipient bulkDownloadMailRecipient;
  private Page<ProductLevel3SummaryResponse> gdnSku;
  private BulkDownloadProductLevel3Response resultSummary;
  private ProductLevel3SummaryRequest request;
  private Workbook workbook;
  private List<PickupPointDTO> pickupPointDTOList;
  private List<PickupPointResponse> pickupPointResponseList;
  private PickupPointResponse pickupPointResponse;
  private static final List<String> HEADERLIST =
      ImmutableList.<String>builder().add("Blibli SKU").add("Nama Produk").add("SKU Code")
          .add("Seller SKU").add("Harga (Rp)").add("Harga Penjualan (Rp)").add("Stok")
          .add("Tipe Penanganan").add("Toko/Gudang").add("Ditampilkan").add("Dapat Dibeli")
          .add("Offline To Online").build();

  private ProfileResponse profileResponse;
  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse1;
  private SystemParameterConfig categorySheetConfig;
  private SystemParameterConfig brandSheetConfig;
  private UnifiedBulkDownloadEvent unifiedBulkDownloadEvent;
  private SystemParameterConfig brandSheetAppendInReviewBrandConfig;

  @InjectMocks
  private BulkDownloadServiceBean bulkDownloadServiceBean;

  @Mock
  private BulkDownloadAuditRepository bulkDownloadAuditRepository;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @Mock
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Mock
  private POIUtil poiUtil;

  @Mock
  private MailDeliveryService mailDeliveryService;

  @Mock
  private BulkProcessRepository bulkProcessRepository;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private PCBOutboundServiceBean pcbOutboundService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private UnifiedBulkDownloadService unifiedBulkDownloadService;

  @Mock
  private NotificationService notificationService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private PickupPointService pickupPointService;

  @Mock
  private ExcelEditHelperService excelEditHelperService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    request = new ProductLevel3SummaryRequest();
    bulkDownloadQueue = new BulkDownloadQueue();
    bulkDownloadQueue.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkDownloadQueue.setPrivilegedMap(PRIVILEGE_MAP);
    bulkDownloadQueue.setProductSize(PRODUCT_SIZE);
    bulkDownloadQueue.setRequestId(DEFAULT_REQUEST_ID);
    bulkDownloadQueue.setEmailTo(DEFAULT_USERNAME);
    bulkDownloadEntity = new BulkDownloadEntity();
    bulkDownloadEntity.setFileName(DEFAULT_FILE_NAME);
    bulkDownloadEntity.setCreatedBy(DEFAULT_USERNAME);
    bulkDownloadMailRecipient = new BulkDownloadMailRecipient();
    bulkDownloadMailRecipient.setEmailTo(DEFAULT_USERNAME);
    bulkDownloadMailRecipient.setEmailCc(DEFAULT_USERNAME);
    bulkDownloadQueue.setRequest(request);
    businessPartner = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setOfflineToOnlineFlag(true);
    companyDTO.setInventoryFulfillment("BL");
    businessPartner.setCompany(companyDTO);
    businessPartner.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointDTOList = new ArrayList<>();
    pickupPointResponseList = new ArrayList<>();
    businessPartner.setPickupPoints(pickupPointDTOList);
    workbook = new SXSSFWorkbook();
    gdnSku = new PageImpl<>(new ArrayList<>());

    resultSummary = new BulkDownloadProductLevel3Response(gdnSku.getContent(), new HashMap<String, String>());
    PickupPointDTO pickupPointDto = new PickupPointDTO();
    pickupPointDto.setCode(PICKUP_POINT_CODE);
    pickupPointDto.setName(PICKUP_POINT_NAME);
    pickupPointDTOList.add(pickupPointDto);
    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    profileResponse.setMerchantStatus("ACTIVE");
    profileResponse.setPickupPoints(pickupPointDTOList);
    profileResponse.setAllCategory(true);
    companyDTO.setInternationalFlag(false);
    profileResponse.setCompany(companyDTO);

    predefinedAllowedAttributeValueResponse1 = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse1.setValue(BRAND);

    categorySheetConfig = new SystemParameterConfig();
    categorySheetConfig.setVariable("category_sheet_regenerate_time");
    categorySheetConfig.setUpdatedDate(new Date());

    brandSheetConfig = new SystemParameterConfig();
    brandSheetConfig.setVariable("brand_sheet_regenerate_time");
    brandSheetConfig.setUpdatedDate(new Date());

    unifiedBulkDownloadEvent = new UnifiedBulkDownloadEvent();
    unifiedBulkDownloadEvent.setDownloadStatus("IN_PROGRESS");
    unifiedBulkDownloadEvent.setBrandUpdated(true);
    unifiedBulkDownloadEvent.setPickupPointUpdated(true);
    unifiedBulkDownloadEvent.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    unifiedBulkDownloadEvent.setLastDownloadedTime(new Date(25));

    brandSheetAppendInReviewBrandConfig = new SystemParameterConfig();
    brandSheetAppendInReviewBrandConfig.setValue("true");

    ReflectionTestUtils.setField(bulkDownloadServiceBean, "unifiedTemplateDirectory",
        UNIFIED_BASE_DIRECTORY);
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "unifiedTemplateFile",
        BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "unifiedTemplateFileEnglish",
        BLIBLI_MASS_UPLOAD_TEMPLATE_EN_XLSM);
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "fileStore", CLASS_LOADER.getResource("").getPath());

    campaignBulkDownloadRequest = CampaignBulkDownloadRequest.builder().campaignCode(CAMPAIGN_CODE).page(0).build();
    CampaignItemSummaryRequest campaignItemSummaryRequest = new CampaignItemSummaryRequest();
    campaignItemSummaryRequest.setMerchantCode("TOQ-15779");
    campaignProductDownloadRequest =
        CampaignProductDownloadRequest.builder().campaignItemSummaryRequest(campaignItemSummaryRequest).build();
    campaignProductDownloadRequest1 =
        CampaignProductDownloadRequest.builder().campaignItemSummaryRequest(campaignItemSummaryRequest)
            .campaignCode(CAMPAIGN_CODE).build();

    ReflectionTestUtils.setField(bulkDownloadServiceBean, "multiPickupPointEnabled", false);

    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setName(PICKUP_POINT_NAME);
    pickupPointResponseList.add(pickupPointResponse);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "pickupPointNameConcat", false);
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "ppNameDelimiter", "||");
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(bulkDownloadAuditRepository);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(businessPartnerRepository);
    Mockito.verifyNoMoreInteractions(productLevel3Repository);
    Mockito.verifyNoMoreInteractions(bulkDownloadServiceBeanUtil);
    Mockito.verifyNoMoreInteractions(mailDeliveryService);
    Mockito.verifyNoMoreInteractions(poiUtil);
    Mockito.verifyNoMoreInteractions(mailDeliveryService);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(notificationService);
    Mockito.verifyNoMoreInteractions(excelEditHelperService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void preProcess() throws Exception {
    bulkDownloadServiceBean
        .preProcess(DEFAULT_REQUEST_ID, PRIVILEGE_MAP, DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_SIZE,request,
            DEFAULT_USERNAME, bulkDownloadMailRecipient);
    Mockito.verify(bulkDownloadAuditRepository).save(Mockito.any(BulkDownloadEntity.class));
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProductDownloadEvent()), Mockito.any(BulkDownloadQueue.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkProductDownloadEvent();
  }

  @Test
  public void test_postProcess_invalidCC() throws Exception {
    bulkDownloadMailRecipient.setEmailCc(EMAIL_CC_INVALID_USERNAME);
    test_postProcess_common();
  }

  @Test
  public void postProcessTest() throws Exception {
    test_postProcess_common();
  }
  
  @Test
  public void postProcessTest_exception() throws Exception {
    Mockito.when(bulkDownloadAuditRepository.findByRequestId(DEFAULT_REQUEST_ID)).thenReturn(
        bulkDownloadEntity);
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(
            DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(null);
    try {
      bulkDownloadServiceBean.postProcess(bulkDownloadQueue);
    } catch (Exception e) {
      Mockito.verify(bulkDownloadAuditRepository).findByRequestId(DEFAULT_REQUEST_ID);
      Mockito.verify(businessPartnerRepository)
          .filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
              DEFAULT_BUSINESS_PARTNER_CODE);
      Mockito.verify(bulkDownloadAuditRepository, Mockito.times(2))
      .save(Mockito.any(BulkDownloadEntity.class));
      Mockito.verify(notificationService).sendDownloadNotification(bulkDownloadEntity);
    }
  }

  private void test_postProcess_common() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setPickupPoints(pickupPointDTOList);
    profileResponse.setCompany(CompanyDTO.builder().inventoryFulfillment("BL").offlineToOnlineFlag(true).build());
    profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.when(bulkDownloadAuditRepository.findByRequestId(DEFAULT_REQUEST_ID))
        .thenReturn(bulkDownloadEntity);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(businessPartner);
    Pageable pageable = PageRequest.of(0, 50);
    Mockito.when(productLevel3Repository
        .findSummaryByFilterForBulkDownload(DEFAULT_BUSINESS_PARTNER_CODE, pageable, request))
        .thenReturn(resultSummary);
    Mockito
        .when(poiUtil.generateXLFile(HEADERLIST, new ArrayList<List<String>>(),
          pickupPointDTOList))
        .thenReturn(
            (SXSSFWorkbook) workbook);
    bulkDownloadServiceBean.postProcess(bulkDownloadQueue);
    Mockito.verify(bulkDownloadAuditRepository).findByRequestId(DEFAULT_REQUEST_ID);
    Mockito.verify(bulkDownloadAuditRepository, Mockito.times(2))
        .save(Mockito.any(BulkDownloadEntity.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3Repository)
        .findSummaryByFilterForBulkDownload(DEFAULT_BUSINESS_PARTNER_CODE, pageable, request);
    Mockito.verify(bulkDownloadServiceBeanUtil).getAllProductValues(eq(PRIVILEGE_MAP), eq(true),
      eq(new ArrayList<ProductLevel3SummaryResponse>()), eq(true),
      Mockito.any(ProfileResponse.class), Mockito.anyList());
    Mockito.verify(bulkDownloadServiceBeanUtil)
        .generateValidationForWorkbook(workbook, 1, 8, true, Constant.PICKUP_POINT_SHEET);
    Mockito.verify(poiUtil).generateXLFile(HEADERLIST, new ArrayList<List<String>>(), pickupPointDTOList);
    Mockito.verify(mailDeliveryService)
        .sendBulkDownloadEmail(anyString(), anyString(), anyString(), anyMap(), anyString(),
            anyString(), any(BulkDownloadMailRecipient.class));
    Mockito.verify(notificationService).sendDownloadNotification(bulkDownloadEntity);
  }

  @Test
  public void test_postProcessWithEmptyResponse() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setPickupPoints(pickupPointDTOList);
    profileResponse.setCompany(CompanyDTO.builder().inventoryFulfillment("BL").offlineToOnlineFlag(true).build());
    profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    resultSummary = new BulkDownloadProductLevel3Response(new ArrayList<>(), new HashMap<>());
    Mockito.when(bulkDownloadAuditRepository.findByRequestId(DEFAULT_REQUEST_ID))
        .thenReturn(bulkDownloadEntity);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(businessPartner);
    Pageable pageable = PageRequest.of(0, 50);
    Mockito.when(productLevel3Repository
        .findSummaryByFilterForBulkDownload(DEFAULT_BUSINESS_PARTNER_CODE, pageable, request))
        .thenReturn(resultSummary);
    Mockito
        .when(poiUtil.generateXLFile(HEADERLIST, new ArrayList<List<String>>(),
          pickupPointDTOList))
        .thenReturn(
            (SXSSFWorkbook) workbook);
    bulkDownloadServiceBean.postProcess(bulkDownloadQueue);
    Mockito.verify(bulkDownloadAuditRepository).findByRequestId(DEFAULT_REQUEST_ID);
    Mockito.verify(bulkDownloadAuditRepository, Mockito.times(2))
        .save(Mockito.any(BulkDownloadEntity.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3Repository)
        .findSummaryByFilterForBulkDownload(DEFAULT_BUSINESS_PARTNER_CODE, pageable, request);
    Mockito.verify(bulkDownloadServiceBeanUtil).getAllProductValues(eq(PRIVILEGE_MAP), eq(true),
      eq(new ArrayList<ProductLevel3SummaryResponse>()), eq(true),
      Mockito.any(ProfileResponse.class), Mockito.anyList());
    Mockito.verify(bulkDownloadServiceBeanUtil)
        .generateValidationForWorkbook(workbook, 1, 8, true, Constant.PICKUP_POINT_SHEET);
    Mockito.verify(poiUtil).generateXLFile(HEADERLIST, new ArrayList<List<String>>(), pickupPointDTOList);
    Mockito.verify(mailDeliveryService)
        .sendBulkDownloadEmail(anyString(), anyString(), anyString(), anyMap(), anyString(),
            anyString(), any(BulkDownloadMailRecipient.class));
    Mockito.verify(notificationService).sendDownloadNotification(bulkDownloadEntity);
  }

  @Test
  public void test_postProcessWithNullResponse() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setPickupPoints(pickupPointDTOList);
    profileResponse.setCompany(CompanyDTO.builder().inventoryFulfillment("BL").offlineToOnlineFlag(true).build());
    profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    resultSummary = new BulkDownloadProductLevel3Response(new ArrayList<>(), new HashMap<>());
    Mockito.when(bulkDownloadAuditRepository.findByRequestId(DEFAULT_REQUEST_ID))
        .thenReturn(bulkDownloadEntity);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(businessPartner);
    Pageable pageable = PageRequest.of(0, 50);
    Mockito.when(productLevel3Repository
        .findSummaryByFilterForBulkDownload(DEFAULT_BUSINESS_PARTNER_CODE, pageable, request))
        .thenReturn(null);
    Mockito
        .when(poiUtil.generateXLFile(HEADERLIST, new ArrayList<List<String>>(),
          pickupPointDTOList))
        .thenReturn(
            (SXSSFWorkbook) workbook);
    bulkDownloadServiceBean.postProcess(bulkDownloadQueue);
    Mockito.verify(bulkDownloadAuditRepository).findByRequestId(DEFAULT_REQUEST_ID);
    Mockito.verify(bulkDownloadAuditRepository, Mockito.times(2))
        .save(Mockito.any(BulkDownloadEntity.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3Repository)
        .findSummaryByFilterForBulkDownload(DEFAULT_BUSINESS_PARTNER_CODE, pageable, request);
    Mockito.verify(bulkDownloadServiceBeanUtil).getAllProductValues(eq(PRIVILEGE_MAP), eq(true),
      eq(new ArrayList<ProductLevel3SummaryResponse>()), eq(true),
      Mockito.any(ProfileResponse.class), Mockito.anyList());
    Mockito.verify(bulkDownloadServiceBeanUtil)
        .generateValidationForWorkbook(workbook, 1, 8, true, Constant.PICKUP_POINT_SHEET);
    Mockito.verify(poiUtil).generateXLFile(HEADERLIST, new ArrayList<List<String>>(), pickupPointDTOList);
    Mockito.verify(mailDeliveryService)
        .sendBulkDownloadEmail(anyString(), anyString(), anyString(), anyMap(), anyString(),
            anyString(), any(BulkDownloadMailRecipient.class));
    Mockito.verify(notificationService).sendDownloadNotification(bulkDownloadEntity);
  }

  @Test
  public void getBulkDownloadProductByRequestIdTest() throws Exception {
    Mockito.when(bulkDownloadAuditRepository.findByRequestId(DEFAULT_REQUEST_ID))
        .thenReturn(bulkDownloadEntity);
    bulkDownloadServiceBean.getBulkDownloadProductByRequestId(DEFAULT_REQUEST_ID);
    Mockito.verify(bulkDownloadAuditRepository).findByRequestId(anyString());
  }

  @Test
  public void getFileContentsTest() throws Exception {
    bulkDownloadServiceBean.getFileContents(DEFAULT_REQUEST_ID);
  }

  @Test
  public void getFileContents_fileDoesntExist_response_file_notFound() throws Exception {
    BulkDownloadProductDTO productDTO = new BulkDownloadProductDTO();
    productDTO.setFileName("test-download.csv");
    productDTO.setEntityType(BulkProcessEntity.ORDER.toString());
    BulkDownloadFileContentDTO fileContents = bulkDownloadServiceBean.getFileContents(productDTO);
    Assertions.assertFalse(fileContents.isFileAvailable());
  }


  @Test
  public void getFileContents_entityNotFound_response_file_notFound() throws Exception {
    BulkDownloadProductDTO productDTO = new BulkDownloadProductDTO();
    productDTO.setFileName("test-download.csv");
    productDTO.setEntityType("random-entity");
    BulkDownloadFileContentDTO fileContents = bulkDownloadServiceBean.getFileContents(productDTO);
    Assertions.assertFalse(fileContents.isFileAvailable());
  }

  @Test
  public void getBulkProcessProductFile_BulkUpdateTrueTest() throws Exception {
    BulkDownloadFileContentDTO fileContentDTO = new BulkDownloadFileContentDTO();
    fileContentDTO.setFileAvailable(true);
    fileContentDTO.setFileContent("test".getBytes());
    fileContentDTO.setRequestId(BULK_PROCESS_CODE);
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkUpdate(true);
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath
        + File.separator + BULK_PROCESS_CODE
        + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Mockito.when(this.bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(eq(DEFAULT_STORE_ID),
        eq(BULK_PROCESS_CODE))).thenReturn(bulkProcess);
    BulkDownloadFileContentDTO response = this.bulkDownloadServiceBean.getBulkProcessProductFile(DEFAULT_STORE_ID,BULK_PROCESS_CODE);
    Mockito.verify(this.bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(
      eq(DEFAULT_STORE_ID), eq(BULK_PROCESS_CODE));
    ProcessorUtils.deleteFile(directoryPath);
    Assertions.assertTrue(response.isFileAvailable());
  }
  
  @Test
  public void getBulkProcessProductFile_WhenBulkProcessIsNotFound() throws Exception {
    Mockito.when(this.bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(eq(DEFAULT_STORE_ID),
        eq(BULK_PROCESS_CODE))).thenReturn(null);
    try{
      Assertions.assertThrows(ApplicationException.class,
          () -> this.bulkDownloadServiceBean.getBulkProcessProductFile(DEFAULT_STORE_ID,
              BULK_PROCESS_CODE));
    } catch(Exception e){
      Mockito.verify(this.bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(eq(DEFAULT_STORE_ID), eq(BULK_PROCESS_CODE));
    }
  }
  
  @Test
  public void getBulkProcessProductFile_BulkUpdateFalseTest() throws Exception {
    BulkDownloadFileContentDTO fileContentDTO = new BulkDownloadFileContentDTO();
    fileContentDTO.setFileAvailable(true);
    fileContentDTO.setFileContent("test".getBytes());
    fileContentDTO.setRequestId(BULK_PROCESS_CODE);
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkUpdate(false);
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.DATA_BASE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath
        + File.separator + BULK_PROCESS_CODE
        + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    Mockito.when(this.bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(bulkProcess);
    
    BulkDownloadFileContentDTO response = this.bulkDownloadServiceBean.getBulkProcessProductFile(DEFAULT_STORE_ID,BULK_PROCESS_CODE);
    
    Mockito.verify(this.bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(
      eq(DEFAULT_STORE_ID), eq(BULK_PROCESS_CODE));
    ProcessorUtils.deleteFile(directoryPath);
    Assertions.assertTrue(response.isFileAvailable());
  }

  public void getBulkProcessProductFile_fileDoesntExistTest() throws Exception {
    BulkDownloadFileContentDTO fileContentDTO = new BulkDownloadFileContentDTO();
    fileContentDTO.setFileAvailable(false);
    fileContentDTO.setFileContent("test".getBytes());
    fileContentDTO.setRequestId("BULK_PROCESS_CODE");
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkUpdate(false);
    Mockito.when(this.bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(eq(DEFAULT_STORE_ID),
        eq(BULK_PROCESS_CODE))).thenReturn(bulkProcess);
    BulkDownloadFileContentDTO response = this.bulkDownloadServiceBean.getBulkProcessProductFile(DEFAULT_STORE_ID,BULK_PROCESS_CODE);
    Mockito.verify(this.bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(
      eq(DEFAULT_STORE_ID), eq(BULK_PROCESS_CODE));

  }

  @Test
  public void generateWholeSaleErrorWorkbookBulkDownload() throws Exception {
    List<String> row1 = new ArrayList<>();
    row1.add("SKU_1");
    row1.add("REASON_1");
    List<String> row2 = new ArrayList<>();
    row2.add("SKU_2");
    row2.add("REASON_2");
    List<List<String>> xlData = new ArrayList<>();
    xlData.add(row1);
    xlData.add(row2);
    List<String> headerList = new ArrayList<>();
    headerList.add(BulkParameters.ITEM_SKU);
    headerList.add(BulkParameters.PICKUP_POINT_HEADER);
    headerList.add(BulkParameters.REASON);
    Mockito.when(poiUtil.generateXLFileForWholesale(headerList, xlData)).thenReturn((SXSSFWorkbook) workbook);
    Mockito.when(fileStorageService
      .getDownloadLink(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(FILE_PATH);
    String path = bulkDownloadServiceBean.generateWholeSaleErrorWorkbookBulkDownload(DEFAULT_REQUEST_ID, xlData);
    Assertions.assertEquals(FILE_PATH, path);
    Mockito.verify(fileStorageService)
      .getDownloadLink(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    Mockito.verify(poiUtil).generateXLFileForWholesale(headerList, xlData);
  }

  @Test
  public void generateEanErrorWorkBookBulkDownload() throws Exception {
    List<String> row1 = new ArrayList<>();
    row1.add("Product Name 1");
    row1.add("SKU_1");
    row1.add("Error Reason 1");
    List<String> row2 = new ArrayList<>();
    row2.add("Product Name 2");
    row2.add("SKU_2");
    row2.add("Error Reason 2");
    List<List<String>> xlData = new ArrayList<>();
    xlData.add(row1);
    xlData.add(row2);
    List<String> headerList = new ArrayList<>();
    headerList.add(BulkParameters.NAMA_PRODUK);
    headerList.add(BulkParameters.ITEM_SKU);
    headerList.add(BulkParameters.REASON);
    Mockito.when(poiUtil.generateXLFileForWholesale(headerList, xlData)).thenReturn((SXSSFWorkbook) workbook);
    Mockito.doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    Mockito.when(fileStorageService
      .getDownloadLink(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(FILE_PATH);
    String path = bulkDownloadServiceBean.generateEanErrorWorkBookBulkDownload(BULK_PROCESS_CODE, xlData);
    Assertions.assertEquals(FILE_PATH, path);
    Mockito.verify(poiUtil).generateXLFileForWholesale(headerList, xlData);
    Mockito.verify(fileStorageService).createBulkFile(Mockito.any(), eq(BULK_PROCESS_CODE), Mockito.anyString());
    Mockito.verify(fileStorageService)
      .getDownloadLink(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }


  private Map<String, String> getFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread()
            .getContextClassLoader()
            .getResourceAsStream("BulkUpdate" + File.separator + "BulkUpdate.xlsx"))), "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }

  @Test
  public void downloadProductUnifiedTemplateTest() throws Exception {
    Mockito.when(
        this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    unifiedBulkDownloadEvent.setDownloadStatus("COMPLETE");
    Mockito.when(
        unifiedBulkDownloadService.getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_IN_PROGRESS, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_COMPLETE, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito
        .when(this.pcbOutboundService.getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true))
        .thenReturn(Arrays.asList(predefinedAllowedAttributeValueResponse1));
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP)).thenReturn(categorySheetConfig);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME))
        .thenReturn(brandSheetConfig);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SHEET_APPENDING_INREVIEW_BRAND_FLAG))
        .thenReturn(brandSheetAppendInReviewBrandConfig);
    Mockito.when(fileStorageService.isFileExists(Mockito.any())).thenReturn(Boolean.TRUE);
    Mockito.when(excelEditHelperService.getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(
      UnifiedBulkDownloadDTO.builder().filePath(FILE_PATH).build());
    this.bulkDownloadServiceBean
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            Collections.emptySet());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(unifiedBulkDownloadService)
        .getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.pcbOutboundService)
        .getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true);
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME);
    Mockito.verify(excelEditHelperService).getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    XSSFWorkbook workBook;
    if (new File(UNIFIED_TEMPLATE_GENERAL_PATH).exists()) {
      try (InputStream is = new BufferedInputStream(new FileInputStream(UNIFIED_TEMPLATE_GENERAL_PATH))) {
        workBook = new XSSFWorkbook(is);
        XSSFSheet sheet = workBook.getSheet(PICKUP_POINT_SHEET);
        Assertions.assertEquals(PICKUP_POINT_SHEET_INDEX, workBook.getSheetIndex(PICKUP_POINT_SHEET));
        Assertions.assertEquals(Constant.PICKUP_POINT_SHEET_HEADER, sheet.getRow(0).getCell(0).getStringCellValue());
        Assertions.assertEquals(Constant.PICKUP_POINT_CODE_HEADER, sheet.getRow(2).getCell(0).getStringCellValue());
      }
    }
  }

  @Test
  public void downloadProductUnifiedTemplateSellerTypeTDTest() throws Exception {
    Mockito.when(
            this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    unifiedBulkDownloadEvent.setDownloadStatus("COMPLETE");
    profileResponse.getCompany().setMerchantType("TD");
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "productBundlingEligibleMerchantTypes", "TD");
    Mockito.when(
            unifiedBulkDownloadService.getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_IN_PROGRESS, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_COMPLETE, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito
        .when(this.pcbOutboundService.getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true))
        .thenReturn(Arrays.asList(predefinedAllowedAttributeValueResponse1));
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP)).thenReturn(categorySheetConfig);
    Mockito.when(this.systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME))
        .thenReturn(brandSheetConfig);
    Mockito.when(this.systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SHEET_APPENDING_INREVIEW_BRAND_FLAG))
        .thenReturn(brandSheetAppendInReviewBrandConfig);
    Mockito.when(fileStorageService.isFileExists(Mockito.any())).thenReturn(Boolean.TRUE);
    Mockito.when(
            excelEditHelperService.getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
                Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(UnifiedBulkDownloadDTO.builder().filePath(FILE_PATH).build());
    this.bulkDownloadServiceBean
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, Collections.emptySet());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(unifiedBulkDownloadService)
        .getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.pcbOutboundService)
        .getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true);
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME);
    Mockito.verify(excelEditHelperService).getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    XSSFWorkbook workBook;
    if (new File(UNIFIED_TEMPLATE_GENERAL_PATH).exists()) {
      try (InputStream is = new BufferedInputStream(new FileInputStream(UNIFIED_TEMPLATE_GENERAL_PATH))) {
        workBook = new XSSFWorkbook(is);
        XSSFSheet sheet = workBook.getSheet(PICKUP_POINT_SHEET);
        Assertions.assertEquals(PICKUP_POINT_SHEET_INDEX, workBook.getSheetIndex(PICKUP_POINT_SHEET));
        Assertions.assertEquals(Constant.PICKUP_POINT_SHEET_HEADER, sheet.getRow(0).getCell(0).getStringCellValue());
        Assertions.assertEquals(Constant.PICKUP_POINT_CODE_HEADER, sheet.getRow(2).getCell(0).getStringCellValue());
      }
    }
  }

  @Test
  public void downloadProductUnifiedTemplateSellerTypeTDInstoreTest() throws Exception {
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "instoreNewFlowEnabled", true);
    Mockito.when(
            this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    unifiedBulkDownloadEvent.setDownloadStatus("COMPLETE");
    profileResponse.getCompany().setMerchantType("TD");
    profileResponse.getCompany().setOfflineToOnlineFlag(false);
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "productBundlingEligibleMerchantTypes", "TD");
    Mockito.when(
            unifiedBulkDownloadService.getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_IN_PROGRESS, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_COMPLETE, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito
        .when(this.pcbOutboundService.getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true))
        .thenReturn(Arrays.asList(predefinedAllowedAttributeValueResponse1));
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP)).thenReturn(categorySheetConfig);
    Mockito.when(this.systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME))
        .thenReturn(brandSheetConfig);
    Mockito.when(this.systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SHEET_APPENDING_INREVIEW_BRAND_FLAG))
        .thenReturn(brandSheetAppendInReviewBrandConfig);
    Mockito.when(fileStorageService.isFileExists(Mockito.any())).thenReturn(Boolean.TRUE);
    Mockito.when(
            excelEditHelperService.getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
                Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(UnifiedBulkDownloadDTO.builder().filePath(FILE_PATH).build());
    this.bulkDownloadServiceBean
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, Collections.emptySet());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(unifiedBulkDownloadService)
        .getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.pcbOutboundService)
        .getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true);
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME);
    Mockito.verify(excelEditHelperService).getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    XSSFWorkbook workBook;
    if (new File(UNIFIED_TEMPLATE_GENERAL_PATH).exists()) {
      try (InputStream is = new BufferedInputStream(new FileInputStream(UNIFIED_TEMPLATE_GENERAL_PATH))) {
        workBook = new XSSFWorkbook(is);
        XSSFSheet sheet = workBook.getSheet(PICKUP_POINT_SHEET);
        Assertions.assertEquals(PICKUP_POINT_SHEET_INDEX, workBook.getSheetIndex(PICKUP_POINT_SHEET));
        Assertions.assertEquals(Constant.PICKUP_POINT_SHEET_HEADER, sheet.getRow(0).getCell(0).getStringCellValue());
        Assertions.assertEquals(Constant.PICKUP_POINT_CODE_HEADER, sheet.getRow(2).getCell(0).getStringCellValue());
      }
    }
  }

  @Test
  public void downloadProductUnifiedTemplateSellerTypeTDInstoreSellerTest() throws Exception {
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "instoreNewFlowEnabled", true);
    Mockito.when(
            this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    unifiedBulkDownloadEvent.setDownloadStatus("COMPLETE");
    profileResponse.getCompany().setMerchantType("TD");
    profileResponse.getCompany().setOfflineToOnlineFlag(true);
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "productBundlingEligibleMerchantTypes", "TD");
    Mockito.when(
            unifiedBulkDownloadService.getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_IN_PROGRESS, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_COMPLETE, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito
        .when(this.pcbOutboundService.getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true))
        .thenReturn(Arrays.asList(predefinedAllowedAttributeValueResponse1));
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP)).thenReturn(categorySheetConfig);
    Mockito.when(this.systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME))
        .thenReturn(brandSheetConfig);
    Mockito.when(this.systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SHEET_APPENDING_INREVIEW_BRAND_FLAG))
        .thenReturn(brandSheetAppendInReviewBrandConfig);
    Mockito.when(fileStorageService.isFileExists(Mockito.any())).thenReturn(Boolean.TRUE);
    Mockito.when(
            excelEditHelperService.getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
                Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(UnifiedBulkDownloadDTO.builder().filePath(FILE_PATH).build());
    this.bulkDownloadServiceBean
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, Collections.emptySet());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(unifiedBulkDownloadService)
        .getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.pcbOutboundService)
        .getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true);
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME);
    Mockito.verify(excelEditHelperService).getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    XSSFWorkbook workBook;
    if (new File(UNIFIED_TEMPLATE_GENERAL_PATH).exists()) {
      try (InputStream is = new BufferedInputStream(new FileInputStream(UNIFIED_TEMPLATE_GENERAL_PATH))) {
        workBook = new XSSFWorkbook(is);
        XSSFSheet sheet = workBook.getSheet(PICKUP_POINT_SHEET);
        Assertions.assertEquals(PICKUP_POINT_SHEET_INDEX, workBook.getSheetIndex(PICKUP_POINT_SHEET));
        Assertions.assertEquals(Constant.PICKUP_POINT_SHEET_HEADER, sheet.getRow(0).getCell(0).getStringCellValue());
        Assertions.assertEquals(Constant.PICKUP_POINT_CODE_HEADER, sheet.getRow(2).getCell(0).getStringCellValue());
      }
    }
  }

  @Test
  public void downloadProductUnifiedTemplateEnglishTest() throws Exception {
    List<String> salesChannel = new ArrayList<>();
    salesChannel.add("BLIBLI");
    profileResponse.getCompany().setSalesChannel(salesChannel);
    profileResponse.getCompany().setInternationalFlag(true);
    profileResponse.getCompany().setCncActivated(true);
    Mockito.when(
        this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    unifiedBulkDownloadEvent.setDownloadStatus("COMPLETE");
    Mockito.when(
        unifiedBulkDownloadService.getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_IN_PROGRESS, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_COMPLETE, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito
        .when(this.pcbOutboundService.getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true))
        .thenReturn(Arrays.asList(predefinedAllowedAttributeValueResponse1));
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP)).thenReturn(categorySheetConfig);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME))
        .thenReturn(brandSheetConfig);
    Mockito.when(fileStorageService.isFileExists(Mockito.any())).thenReturn(Boolean.TRUE);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SHEET_APPENDING_INREVIEW_BRAND_FLAG))
        .thenReturn(brandSheetAppendInReviewBrandConfig);
    Mockito.when(
        excelEditHelperService.getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.anyBoolean(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(),
          Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
      .thenReturn(UnifiedBulkDownloadDTO.builder().filePath(FILE_PATH).build());
    this.bulkDownloadServiceBean
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            Collections.emptySet());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(unifiedBulkDownloadService)
        .getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.pcbOutboundService)
        .getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true);
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME);
    Mockito.verify(excelEditHelperService).getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    XSSFWorkbook workBook;
    if (new File(UNIFIED_TEMPLATE_GENERAL_PATH).exists()) {
      try (InputStream is = new BufferedInputStream(new FileInputStream(UNIFIED_TEMPLATE_GENERAL_PATH))) {
        workBook = new XSSFWorkbook(is);
        XSSFSheet sheet = workBook.getSheet(PICKUP_POINT_SHEET);
        Assertions.assertEquals(PICKUP_POINT_SHEET_INDEX, workBook.getSheetIndex(PICKUP_POINT_SHEET));
        Assertions.assertEquals(Constant.PICKUP_POINT_SHEET_HEADER, sheet.getRow(0).getCell(0).getStringCellValue());
        Assertions.assertEquals(Constant.PICKUP_POINT_CODE_HEADER, sheet.getRow(2).getCell(0).getStringCellValue());
      }
    }
  }

  @Test
  public void downloadProductUnifiedTemplateWithNullResponseTest() throws Exception {
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "unifiedTemplateDirectory", "./src/test/resources/ExcelTemplate2/");
    Mockito.when(
        this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    unifiedBulkDownloadEvent.setDownloadStatus("COMPLETE");
    Mockito.when(
        unifiedBulkDownloadService.getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_IN_PROGRESS, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_COMPLETE, DEFAULT_BUSINESS_PARTNER_CODE, null);

    Mockito
        .when(this.pcbOutboundService.getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true))
        .thenReturn(Arrays.asList(predefinedAllowedAttributeValueResponse1));
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP)).thenReturn(categorySheetConfig);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME))
        .thenReturn(brandSheetConfig);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BRAND_SHEET_APPENDING_INREVIEW_BRAND_FLAG))
        .thenReturn(brandSheetAppendInReviewBrandConfig);
    Mockito.when(
        excelEditHelperService.getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.anyBoolean(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(),
          Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
      .thenReturn(UnifiedBulkDownloadDTO.builder().filePath(FILE_PATH).build());
    this.bulkDownloadServiceBean
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            Collections.emptySet());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(unifiedBulkDownloadService)
        .getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(excelEditHelperService).getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void downloadProductUnifiedTemplateNoInReviewBrandTest() throws Exception {
    Mockito.when(
        this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito
        .when(this.pcbOutboundService.getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true))
        .thenReturn(null);
    unifiedBulkDownloadEvent.setDownloadStatus("COMPLETE");
    Mockito.when(
        unifiedBulkDownloadService.getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_IN_PROGRESS, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_COMPLETE, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP)).thenReturn(categorySheetConfig);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME))
        .thenReturn(brandSheetConfig);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SHEET_APPENDING_INREVIEW_BRAND_FLAG))
        .thenReturn(brandSheetAppendInReviewBrandConfig);
    Mockito.when(fileStorageService.isFileExists(Mockito.any())).thenReturn(Boolean.TRUE);
    Mockito.when(
        excelEditHelperService.getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.anyBoolean(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(),
          Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
      .thenReturn(UnifiedBulkDownloadDTO.builder().filePath(FILE_PATH).build());
    this.bulkDownloadServiceBean
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            Collections.emptySet());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(unifiedBulkDownloadService)
        .getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.pcbOutboundService)
        .getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true);
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME);
    Mockito.verify(excelEditHelperService).getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void downloadProductUnifiedTemplateUnifiedBulkDownloadExceptionTest() throws Exception {
    Mockito.when(
        unifiedBulkDownloadService.getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.bulkDownloadServiceBean.downloadProductUnifiedTemplate(DEFAULT_STORE_ID,
              DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, Collections.emptySet()));
    } finally {
      Mockito.verify(unifiedBulkDownloadService)
          .getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void downloadProductUnifiedTemplateFirstRequestTest() throws Exception {
    Mockito.when(
        this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
        unifiedBulkDownloadService.getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(null);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_IN_PROGRESS, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_COMPLETE, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito
        .when(this.pcbOutboundService.getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true))
        .thenReturn(Arrays.asList(predefinedAllowedAttributeValueResponse1));
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SHEET_APPENDING_INREVIEW_BRAND_FLAG))
        .thenReturn(brandSheetAppendInReviewBrandConfig);
    Mockito.when(
        excelEditHelperService.getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.anyBoolean(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(),
          Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
      .thenReturn(UnifiedBulkDownloadDTO.builder().filePath(FILE_PATH).build());
    this.bulkDownloadServiceBean
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            Collections.emptySet());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(unifiedBulkDownloadService)
        .getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.pcbOutboundService)
        .getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true);
    Mockito.verify(excelEditHelperService).getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void downloadProductUnifiedTemplateSecondRequestTest() throws Exception {
    Mockito.when(
        this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    unifiedBulkDownloadEvent.setDownloadStatus("COMPLETE");
    unifiedBulkDownloadEvent.setLastDownloadedTime(new Date());
    unifiedBulkDownloadEvent.setPickupPointUpdated(false);
    unifiedBulkDownloadEvent.setBrandUpdated(false);
    Mockito.when(
        unifiedBulkDownloadService.getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_IN_PROGRESS, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_COMPLETE, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito
        .when(this.pcbOutboundService.getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true))
        .thenReturn(Arrays.asList(predefinedAllowedAttributeValueResponse1));
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP)).thenReturn(categorySheetConfig);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME))
        .thenReturn(brandSheetConfig);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SHEET_APPENDING_INREVIEW_BRAND_FLAG))
        .thenReturn(brandSheetAppendInReviewBrandConfig);
    Mockito.when(fileStorageService.isFileExists(Mockito.any())).thenReturn(Boolean.TRUE);
    Mockito.when(
        excelEditHelperService.getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.anyBoolean(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(),
          Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
      .thenReturn(UnifiedBulkDownloadDTO.builder().filePath(FILE_PATH).build());
    this.bulkDownloadServiceBean
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            Collections.emptySet());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(unifiedBulkDownloadService)
        .getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.pcbOutboundService)
        .getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true);
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME);
    Mockito.verify(excelEditHelperService).getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void downloadProductUnifiedTemplateExceptionTest() throws Exception {
    Mockito.when(
        this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenThrow(new Exception());
    unifiedBulkDownloadEvent.setDownloadStatus("COMPLETE");
    Mockito.when(
        unifiedBulkDownloadService.getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.bulkDownloadServiceBean.downloadProductUnifiedTemplate(DEFAULT_STORE_ID,
              DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, Collections.emptySet()));
    } finally {
      Mockito.verify(unifiedBulkDownloadService)
          .getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
      Mockito.verify(this.businessPartnerRepository)
          .filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void downloadProductUnifiedTemplateAppendInReviewBrandIsFalseTest() throws Exception {
    Mockito.when(
        this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito
        .when(this.pcbOutboundService.getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true))
        .thenReturn(null);
    unifiedBulkDownloadEvent.setDownloadStatus("COMPLETE");
    Mockito.when(
        unifiedBulkDownloadService.getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_IN_PROGRESS, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_COMPLETE, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP)).thenReturn(categorySheetConfig);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME))
        .thenReturn(brandSheetConfig);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SHEET_APPENDING_INREVIEW_BRAND_FLAG))
        .thenReturn(brandSheetAppendInReviewBrandConfig);
    Mockito.when(fileStorageService.isFileExists(Mockito.any())).thenReturn(Boolean.TRUE);
    Mockito.when(
        excelEditHelperService.getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.anyBoolean(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(),
          Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
      .thenReturn(UnifiedBulkDownloadDTO.builder().filePath(FILE_PATH).build());
    this.bulkDownloadServiceBean
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            Collections.emptySet());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
      DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(unifiedBulkDownloadService)
        .getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME);
    Mockito.verify(excelEditHelperService).getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void downloadProductUnifiedTemplateAppendInReviewBrandIsFalseBundleInEligibleTest() throws Exception {
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "productBundlingEligibleMerchantTypes", "ABC");
    Mockito.when(
            this.businessPartnerRepository.filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito
        .when(this.pcbOutboundService.getBrandSuggestions(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE, true, true))
        .thenReturn(null);
    unifiedBulkDownloadEvent.setDownloadStatus("COMPLETE");
    Mockito.when(
            unifiedBulkDownloadService.getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_IN_PROGRESS, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.doNothing().when(unifiedBulkDownloadService)
        .updateStatusAndLastDownloadTime(DOWNLOAD_COMPLETE, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP)).thenReturn(categorySheetConfig);
    Mockito.when(this.systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME))
        .thenReturn(brandSheetConfig);
    Mockito.when(this.systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SHEET_APPENDING_INREVIEW_BRAND_FLAG))
        .thenReturn(brandSheetAppendInReviewBrandConfig);
    Mockito.when(fileStorageService.isFileExists(Mockito.any())).thenReturn(Boolean.TRUE);
    Mockito.when(
        excelEditHelperService.getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.anyBoolean(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(),
          Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
      .thenReturn(UnifiedBulkDownloadDTO.builder().filePath(FILE_PATH).build());
    this.bulkDownloadServiceBean
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            Collections.emptySet());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(unifiedBulkDownloadService)
        .getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BRAND_SCHEDULER_RUN_TIME);
    Mockito.verify(excelEditHelperService).getProductUnifiedTemplate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void countNumberOfCampaignDownloads () throws Exception {
    bulkDownloadEntity.setDescription(
        "{\"bulkProcessEntity\":\"CAMPAIGN_PRODUCT\",\"campaignItemSummaryRequest\":{\"merchantCode\":\"TOQ-15779\","
            + "\"categories\":[],\"brands\":[],\"keyword\":\"\",\"itemSku\":\"\"},\"privilegedMap\":null,\"page\":0,\"size\""
            + ":0,\"promoType\":null,\"recommendedWeek\":null,\"campaignCode\":null,\"timestamp\":0,\"requestId\":"
            + "\"7dcf1b5d-d206-495a-a8dc-de76a4804073\",\"downloadType\":\"ALL\",\"fileType\":\"XLSX\",\"bulkProcessEntity\""
            + ":\"CAMPAIGN_PRODUCT\"}");
    BulkDownloadEntity bulkDownloadEntity1 = new BulkDownloadEntity();
    bulkDownloadEntity1.setDescription("CampaignProductDownloadRequest");
    BulkDownloadEntity bulkDownloadEntity2 = new BulkDownloadEntity();
    bulkDownloadEntity2.setDescription(
        "{\"bulkProcessEntity\":\"CAMPAIGN_PRODUCT\",\"campaignItemSummaryRequest\":{\"merchantCode\":\"TOQ-15779\","
            + "\"categories\":[],\"brands\":[],\"keyword\":\"\",\"itemSku\":\"\"},\"privilegedMap\":null,\"page\":0,\"size\""
            + ":0,\"promoType\":null,\"recommendedWeek\":null,\"campaignCode\":\"campaign_code\",\"timestamp\":0,\"requestId\":"
            + "\"7dcf1b5d-d206-495a-a8dc-de76a4804073\",\"downloadType\":\"ALL\",\"fileType\":\"XLSX\",\"bulkProcessEntity\""
            + ":\"CAMPAIGN_PRODUCT\"}");
    BulkDownloadEntity bulkDownloadEntity3 = new BulkDownloadEntity();
    bulkDownloadEntity3.setDescription("{}");
    BulkDownloadEntity bulkDownloadEntity4 = new BulkDownloadEntity();
    bulkDownloadEntity4
        .setDescription("{\"bulkProcessEntity\":\"CAMPAIGN_PRODUCT\",\"campaignItemSummaryRequest\":{..");
    BulkDownloadEntity bulkDownloadEntity5 = new BulkDownloadEntity();
    bulkDownloadEntity5.setDescription("{");
    Mockito.when(bulkDownloadAuditRepository
        .findByEntityTypeAndBusinessPartnerCodeAndStatusIn(BulkProcessEntity.CAMPAIGN_PRODUCT.name(),
            DEFAULT_BUSINESS_PARTNER_CODE, Arrays.asList(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
                BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()))).thenReturn(Arrays
        .asList(bulkDownloadEntity, new BulkDownloadEntity(), bulkDownloadEntity1, bulkDownloadEntity2,
            bulkDownloadEntity3, bulkDownloadEntity4, bulkDownloadEntity5));
    Mockito
        .when(objectMapper.readValue(eq(bulkDownloadEntity.getDescription()), Mockito.any(TypeReference.class)))
        .thenReturn(campaignProductDownloadRequest);
    Mockito.when(
        objectMapper.readValue(eq(bulkDownloadEntity2.getDescription()), Mockito.any(TypeReference.class)))
        .thenReturn(campaignProductDownloadRequest1);
    this.bulkDownloadServiceBean
        .countNumberOfCampaignDownloads(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, campaignBulkDownloadRequest);
    Mockito.verify(bulkDownloadAuditRepository)
        .findByEntityTypeAndBusinessPartnerCodeAndStatusIn(BulkProcessEntity.CAMPAIGN_PRODUCT.name(),
            DEFAULT_BUSINESS_PARTNER_CODE, Arrays.asList(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
                BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));
    Mockito.verify(objectMapper)
        .readValue(eq(bulkDownloadEntity.getDescription()), Mockito.any(TypeReference.class));
    Mockito.verify(objectMapper)
        .readValue(eq(bulkDownloadEntity2.getDescription()), Mockito.any(TypeReference.class));
    Mockito.verify(objectMapper)
        .readValue(eq(bulkDownloadEntity3.getDescription()), Mockito.any(TypeReference.class));
  }

  @Test
  public void countNumberOfCampaignDownloads_Empty () throws Exception {
    Mockito.when(bulkDownloadAuditRepository
        .findByEntityTypeAndBusinessPartnerCodeAndStatusIn(BulkProcessEntity.CAMPAIGN_PRODUCT.name(),
            DEFAULT_BUSINESS_PARTNER_CODE, Arrays.asList(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
                BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()))).thenReturn(Arrays.asList());
    this.bulkDownloadServiceBean
        .countNumberOfCampaignDownloads(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, campaignBulkDownloadRequest);
    Mockito.verify(bulkDownloadAuditRepository)
        .findByEntityTypeAndBusinessPartnerCodeAndStatusIn(BulkProcessEntity.CAMPAIGN_PRODUCT.name(),
            DEFAULT_BUSINESS_PARTNER_CODE, Arrays.asList(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
                BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));
  }

  @Test
  public void countPendingRequestsByUsernameAndDownloadType() throws Exception {
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "maxBulkRequests", 1);
    Mockito.when(
            bulkDownloadAuditRepository.countByEntityTypeAndBusinessPartnerCodeAndStatusInAndCreatedByAndMarkForDeleteFalse(
                BulkProcessEntity.STORE_COPY_PRODUCTS.name(), DEFAULT_BUSINESS_PARTNER_CODE,
                Arrays.asList(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()), DEFAULT_USERNAME))
        .thenReturn(1);
    BulkInternalPendingRequestResponse bulkInternalPendingRequestResponse =
        this.bulkDownloadServiceBean.countPendingRequestsByUsernameAndDownloadType(DEFAULT_BUSINESS_PARTNER_CODE,
            DEFAULT_USERNAME, BulkProcessEntity.STORE_COPY_PRODUCTS.name());
    Mockito.verify(bulkDownloadAuditRepository)
        .countByEntityTypeAndBusinessPartnerCodeAndStatusInAndCreatedByAndMarkForDeleteFalse(
            BulkProcessEntity.STORE_COPY_PRODUCTS.name(), DEFAULT_BUSINESS_PARTNER_CODE,
            Arrays.asList(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
                BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()), DEFAULT_USERNAME);
    Assertions.assertFalse(bulkInternalPendingRequestResponse.isBulkInternalStatusFlag());
  }

  @Test
  public void countPendingRequestsByUsernameAndDownloadType1() throws Exception {
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "maxBulkRequests", 1);
    Mockito.when(
            bulkDownloadAuditRepository.countByEntityTypeAndBusinessPartnerCodeAndStatusInAndCreatedByAndMarkForDeleteFalse(
                BulkProcessEntity.STORE_COPY_PRODUCTS.name(), DEFAULT_BUSINESS_PARTNER_CODE,
                Arrays.asList(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()), DEFAULT_USERNAME))
        .thenReturn(0);
    BulkInternalPendingRequestResponse bulkInternalPendingRequestResponse =
        this.bulkDownloadServiceBean.countPendingRequestsByUsernameAndDownloadType(DEFAULT_BUSINESS_PARTNER_CODE,
            DEFAULT_USERNAME, BulkProcessEntity.STORE_COPY_PRODUCTS.name());
    Mockito.verify(bulkDownloadAuditRepository)
        .countByEntityTypeAndBusinessPartnerCodeAndStatusInAndCreatedByAndMarkForDeleteFalse(
            BulkProcessEntity.STORE_COPY_PRODUCTS.name(), DEFAULT_BUSINESS_PARTNER_CODE,
            Arrays.asList(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(),
                BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()), DEFAULT_USERNAME);
    Assertions.assertTrue(bulkInternalPendingRequestResponse.isBulkInternalStatusFlag());
  }

  @Test
  public void clearInProgressDownloadsTest() throws Exception {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MINUTE_VALUE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.STORE_COPY_ABORT_IN_PROGRESS_DOWNLOADS_BULK_DOWNLOAD_ENTITY))
        .thenReturn(systemParameterConfig);
    bulkDownloadServiceBean.clearInProgressDownloads(DEFAULT_STORE_ID, BulkProcessEntity.STORE_COPY_PRODUCTS.name(),
        BulkDownloadServiceBean.IN_PROGRESS);
    Mockito.verify(bulkDownloadAuditRepository)
        .updateStatusInProgressBulkDownloadEntityToAborted(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.STORE_COPY_ABORT_IN_PROGRESS_DOWNLOADS_BULK_DOWNLOAD_ENTITY);
  }

  @Test
  public void getMerchantTypePureDeliveryTest() {
    MerchantStatusType merchantType = this.bulkDownloadServiceBean.setMerchantType(profileResponse);
    Assertions.assertEquals(merchantType.getType(), MerchantStatusType.PURE_DELIVERY.getType());
  }
  @Test
  public void getMerchantTypeCncTest() {
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setSalesChannel(Collections.singletonList(BLIBLI));
    MerchantStatusType merchantType = this.bulkDownloadServiceBean.setMerchantType(profileResponse);
    Assertions.assertEquals(merchantType.getType(), MerchantStatusType.DELIVERY_AND_CNC.getType());
  }
  @Test
  public void getMerchantTypeBfbCncTest() {
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setSalesChannel(Collections.singletonList(BulkProcessConstant.B2B_SELLER_CHANNEL));
    MerchantStatusType merchantType = this.bulkDownloadServiceBean.setMerchantType(profileResponse);
    Assertions.assertEquals(merchantType.getType(), MerchantStatusType.BFB_AND_CNC.getType());
  }
  @Test
  public void getMerchantTypeBfbTest() {
    profileResponse.getCompany().setCncActivated(false);
    profileResponse.getCompany().setSalesChannel(Collections.singletonList(BulkProcessConstant.B2B_SELLER_CHANNEL));
    MerchantStatusType merchantType = this.bulkDownloadServiceBean.setMerchantType(profileResponse);
    Assertions.assertEquals(merchantType.getType(), MerchantStatusType.BFB.getType());
  }

  @Test
  public void getMerchantTypeCompanyNullTest() {
    profileResponse.setCompany(null);
    MerchantStatusType merchantType = this.bulkDownloadServiceBean.setMerchantType(profileResponse);
    Assertions.assertEquals(merchantType.getType(), MerchantStatusType.PURE_DELIVERY.getType());
  }

  @Test
  public void generateWorkbookBulkDownload_pricePrivilegeFalse() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    profileResponse.setMerchantStatus("ACTIVE");
    profileResponse.setPickupPoints(pickupPointDTOList);
    profileResponse.setAllCategory(true);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(false);
    companyDTO.setOfflineToOnlineFlag(false);
    profileResponse.setCompany(companyDTO);

    Map<String, Boolean> privilegedMap = new HashMap<>(PRIVILEGE_MAP);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_PRICE, false);

    SXSSFWorkbook mockWorkbook = Mockito.mock(SXSSFWorkbook.class);
    SXSSFSheet mockSheet = Mockito.mock(SXSSFSheet.class);
    Mockito.when(mockWorkbook.getSheetAt(0)).thenReturn(mockSheet);
    Mockito.when(poiUtil.generateXLFile(Mockito.anyList(), Mockito.anyList(), Mockito.anyList()))
        .thenReturn(mockWorkbook);

    bulkDownloadServiceBean.generateWorkbookBulkDownload(profileResponse, privilegedMap, PRODUCT_SIZE, request,
        new HashMap<>(), true);

    Mockito.verify(poiUtil).generateXLFile(Mockito.anyList(), Mockito.anyList(), Mockito.anyList());
    Mockito.verify(bulkDownloadServiceBeanUtil)
        .generateValidationForWorkbook(Mockito.any(SXSSFWorkbook.class), Mockito.anyInt(), Mockito.anyInt(),
            Mockito.anyBoolean(), Mockito.anyString());
    Mockito.verify(productLevel3Repository)
        .findSummaryByFilterForBulkDownload(eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.any(Pageable.class),
            eq(request));
    Mockito.verify(bulkDownloadServiceBeanUtil)
        .getAllProductValues(eq(privilegedMap), eq(false), Mockito.anyList(), eq(true), eq(profileResponse),
            Mockito.anyList());
  }

  @Test
  public void generateWorkbookBulkDownload_pickupPointPrivilegeFalse() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    profileResponse.setMerchantStatus("ACTIVE");
    profileResponse.setPickupPoints(pickupPointDTOList);
    profileResponse.setAllCategory(true);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(false);
    companyDTO.setOfflineToOnlineFlag(false);
    profileResponse.setCompany(companyDTO);

    Map<String, Boolean> privilegedMap = new HashMap<>(PRIVILEGE_MAP);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_PICKUP_POINT, false);

    SXSSFWorkbook mockWorkbook = Mockito.mock(SXSSFWorkbook.class);
    SXSSFSheet mockSheet = Mockito.mock(SXSSFSheet.class);
    Mockito.when(mockWorkbook.getSheetAt(0)).thenReturn(mockSheet);
    Mockito.when(poiUtil.generateXLFile(Mockito.anyList(), Mockito.anyList(), Mockito.anyList()))
        .thenReturn(mockWorkbook);


    bulkDownloadServiceBean.generateWorkbookBulkDownload(profileResponse, privilegedMap, PRODUCT_SIZE, request,
        new HashMap<>(), true);

    Mockito.verify(poiUtil).generateXLFile(Mockito.anyList(), Mockito.anyList(), Mockito.anyList());
    Mockito.verify(bulkDownloadServiceBeanUtil)
        .generateValidationForWorkbook(Mockito.any(SXSSFWorkbook.class), Mockito.anyInt(), Mockito.anyInt(),
            Mockito.anyBoolean(), Mockito.anyString());
    Mockito.verify(productLevel3Repository)
        .findSummaryByFilterForBulkDownload(eq(DEFAULT_BUSINESS_PARTNER_CODE), Mockito.any(Pageable.class),
            eq(request));
    Mockito.verify(bulkDownloadServiceBeanUtil)
        .getAllProductValues(eq(privilegedMap), eq(false), Mockito.anyList(), eq(true), eq(profileResponse),
            Mockito.anyList());
  }

  @Test
  public void checkForProductFlags_allPrivilegesFalse() {
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_PRICE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_STOCK, false);
    BulkDownloadServiceBean.checkForProductFlags(privilegedMap, false, new ArrayList<>());
    Assertions.assertFalse(privilegedMap.get(BulkParameters.PRIVILEGE_READ_PRICE));
    Assertions.assertFalse(privilegedMap.get(BulkParameters.PRIVILEGE_READ_PICKUP_POINT));
    Assertions.assertFalse(privilegedMap.get(BulkParameters.PRIVILEGE_READ_STOCK));
  }

  @Test
  public void checkForProductFlags_missingPrivileges() {
    Map<String, Boolean> privilegedMap = new HashMap<>();
    BulkDownloadServiceBean.checkForProductFlags(privilegedMap, false, new ArrayList<>());
  }

  @Test
  public void testCheckForStockAndProductType_DefaultFalse() {
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "preOrderQuotaFeatureSwitch", false);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    List<String> headerList = new ArrayList<>();
    bulkDownloadServiceBean.checkForStockAndProductType(privilegedMap, headerList, false);
    Assertions.assertTrue(headerList.isEmpty());
  }

  @Test
  public void testCheckForStockAndProductType_stock() {
    ReflectionTestUtils.setField(bulkDownloadServiceBean, "preOrderQuotaFeatureSwitch", true);
    Map<String, Boolean> privilegedMap = new HashMap<>(1);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_STOCK, true);
    List<String> headerList = new ArrayList<>();
    bulkDownloadServiceBean.checkForStockAndProductType(privilegedMap, headerList, true);
    Assertions.assertFalse(headerList.isEmpty());
  }
}
