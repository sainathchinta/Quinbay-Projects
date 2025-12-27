package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.repository.BulkProcessCustomRepository;
import com.gdn.mta.bulk.repository.BulkProcessDataRepository;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.CategoryRepository;
import com.gdn.mta.bulk.service.download.BulkFailedProductFileService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

public class BulkProcessServiceWrapperImplTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_REQUEST_ID = "request-id";
  private static final String DEFAULT_BULK_PROCESS_CODE = "defaultBulkProcessCode";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00106";
  private static final String DEFAULT_CATEGORY_CODE = "CA-00106";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String PARENT_PRODUCT = "parentProduct";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";

  private static final String BULK_PROCESS_DATA =
      "{\"Kategori*\":\"Alat Musik\",\n\"Kategori1\":\"Aksesoris Alat Musik\",\n\"Kategori2\":\"Strings\",\n\"Nama"
          + "Produk*\":\"Product name\",\n\"Seller SKU\":\"Seller Sku 1\",\n\"Deskripsi*\":\"Description\","
          + "\n\"Keunggulan produk\":\"USP\",\n\"Merek*\":\"ABC\",\n\"Warna*\":\"Black\",\n\"Family Colour\":\"Hitam\","
          + "\n\"Ukuran\":\"\",\n\"Variasi\":\"\",\n\"Parent\":\"1\",\n\"Foto-1*\":\"image-1.jpg\",\n\"Foto-2\":\"\",\n"
          + "\"Foto-3\":\"\",\n\"Fotor-4\":\"\",\n\"Foto-5\":\"\",\n\"Foto-6\":\"\",\n\"Foto-7\":\"\",\n\"Foto-8\":\"\","
          + "\n\"Url Video\":\"URL Video\",\n\"Tipe Penanganan*\":\"Melalui partner logistik Blibli\",\n\"Panjang (cm)*"
          + "\":\"10\",\n\"Lebar (cm)*\":\"10\",\n\"Tinggi (cm)*\":\"10\",\n\"Berat (gram)*\":\"100\",\n\"Harga Penjualan "
          + "(Rp)*\":\"54321\",\n\"Available Stock*\":\"2\",\n\"Pilih Attribut\":\"Fungsi\",\n\"Pilih value\":\"Fungsi value"
          + "\",\n\"Pilih Attribut1\":\"Lain-lain\",\n\"Pilih value1\":\"Lain - lian value\",\n\"Pilih Attribut2\":\"Dimensi"
          + "Produk\",\n\"Pilih value2\":\"Dimensi value\",\n\"Pilih Attribut3\":\"Berat\",\n\"Pilih value3\":\"Berat value"
          + "\",\n\"Pilih Attribut4\":\"Material\",\n\"Pilih value4\":\"material value\"}";

  private BulkProcessData bulkProcessData;
  private ConfigurationStatusRequest configurationStatusRequest;
  private GdnRestListResponse<ConfigurationStatusResponse> configurationStatusResponse;
  private SystemParameterConfig creationBatchSize;

  @InjectMocks
  private BulkProcessServiceWrapperImpl bulkProcessServiceWrapper;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private BulkProcessCustomRepository bulkProcessCustomRepository;

  @Mock
  private BulkProcessRepository bulkProcessRepository;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private BulkProcessDataRepository bulkProcessDataRepository;

  @Mock
  private BulkFailedProductFileService bulkFailedProductFileService;

  @Mock
  private PCBOutboundService pcbOutboundService;

  @Mock
  private CategoryRepository categoryRepository;

  @Mock
  private BulkUpdateService bulkUpdateService;

  @Mock
  private BulkArchiveService bulkArchiveService;

  @Mock
  private BulkDeleteService bulkDeleteService;

  @Mock
  private BulkUpsertService bulkUpsertService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private QrCodeFinalizeService qrCodeFinalizeService;

  @Mock
  private EANProductLevel4BulkUpdateService eanProductLevel4BulkUpdateService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(systemParameterConfigService);
    Mockito.verifyNoMoreInteractions(bulkProcessService);
    Mockito.verifyNoMoreInteractions(bulkFailedProductFileService);
    Mockito.verifyNoMoreInteractions(bulkProcessCustomRepository);
    Mockito.verifyNoMoreInteractions(bulkProcessRepository);
    Mockito.verifyNoMoreInteractions(bulkProcessDataService);
    Mockito.verifyNoMoreInteractions(pcbOutboundService);
    Mockito.verifyNoMoreInteractions(categoryRepository);
    Mockito.verifyNoMoreInteractions(bulkDeleteService);
    Mockito.verifyNoMoreInteractions(bulkUpsertService);
    Mockito.verifyNoMoreInteractions(bulkArchiveService);
    Mockito.verifyNoMoreInteractions(eanProductLevel4BulkUpdateService);
    Mockito.verifyNoMoreInteractions(bulkUpdateService);
    Mockito.verifyNoMoreInteractions(bulkProcessDataRepository);
    Mockito.verifyNoMoreInteractions(qrCodeFinalizeService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  private ConfigurationStatusResponse getConfigurationStatusResponse(String status) {
    ConfigurationStatusResponse configurationStatusResponse =
        ConfigurationStatusResponse.builder().reviewConfig(status).build();
    return configurationStatusResponse;
  }

  private List<CategoryResponse> getCategoryResponsesWithNullActivationInterval(Integer internalActivationInterval) {
    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setParentCategoryId(null);
    categoryResponse1.setInternalActivationInterval(internalActivationInterval);
    CategoryResponse categoryResponse2 = new CategoryResponse();
    categoryResponse2.setParentCategoryId("id");
    return Arrays.asList(categoryResponse1, categoryResponse2);
  }

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    bulkProcessData = new BulkProcessData();
    bulkProcessData.setBulkProcessId(getBulkProcess().get(0).getId());
    bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS);
    creationBatchSize = new SystemParameterConfig("bulk_creation_batch_size", "10", "desc");
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE))
        .thenReturn(creationBatchSize);
    Mockito.when(systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_DELETE_BATCH_SIZE))
        .thenReturn(creationBatchSize);
    Mockito.when(systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_UPDATE_BATCH_SIZE))
        .thenReturn(creationBatchSize);
    when(systemParameterConfigService
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_PRIORITY_1)).thenReturn(creationBatchSize);
    when(systemParameterConfigService
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_PRIORITY_2)).thenReturn(creationBatchSize);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_CONVERTED_UPLOAD)).thenReturn(
      creationBatchSize);
    Mockito.when(systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_UPSERT_BATCH_SIZE))
        .thenReturn(creationBatchSize);
    Mockito.when(systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_CAMPAIGN_BATCH_SIZE))
        .thenReturn(creationBatchSize);
    Mockito.when(systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_INSTORE_BATCH_SIZE))
        .thenReturn(creationBatchSize);
    Mockito.when(systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_ARCHIVE_BATCH_SIZE))
        .thenReturn(creationBatchSize);
    Mockito.when(systemParameterConfigService
            .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_VAT_UPDATE_BATCH_SIZE))
        .thenReturn(creationBatchSize);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE))
        .thenReturn(creationBatchSize);
    SystemParameterConfig creationOrderBy = new SystemParameterConfig("bulk_creation_batch_size", "true", "desc");
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY))
        .thenReturn(creationOrderBy);
    configurationStatusRequest = ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
        .categoryCode(DEFAULT_CATEGORY_CODE).build();
    configurationStatusResponse =
        new GdnRestListResponse<>(Arrays.asList(getConfigurationStatusResponse("Pre-live")), new PageMetaData(),
            DEFAULT_REQUEST_ID);
    Mockito.when(pcbOutboundService
        .getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_BULK_PROCESS_CODE,
            DEFAULT_USERNAME, Arrays.asList(configurationStatusRequest))).thenReturn(configurationStatusResponse);
    Mockito.when(categoryRepository.filterCategoryHierarchyByCategoryCode(DEFAULT_STORE_ID, DEFAULT_CATEGORY_CODE))
        .thenReturn(getCategoryResponsesWithNullActivationInterval(null));
    ReflectionTestUtils.setField(bulkProcessServiceWrapper, "internalActivationPeriod", 120);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_EXTERNAL_UPLOAD)).thenReturn(
      creationBatchSize);
  }

  private List<BulkProcess> getBulkProcess() {
    Date date = Calendar.getInstance().getTime();
    List<BulkProcess> bulkProcess = new ArrayList<BulkProcess>();
    bulkProcess.add(
        new BulkProcess(DEFAULT_BULK_PROCESS_CODE, "ProductLevel3", DEFAULT_BUSINESS_PARTNER_CODE, null, null,
            BulkProcess.STATUS_PENDING, null, new ArrayList<BulkProcessNotes>()));
    bulkProcess.add(
        new BulkProcess(UUID.randomUUID().toString(), "ProductLevel3", DEFAULT_BUSINESS_PARTNER_CODE, date, null,
            BulkProcess.STATUS_IN_PROGRESS, null, new ArrayList<BulkProcessNotes>()));
    for (BulkProcess bulkProcessItem : bulkProcess) {
      bulkProcessItem.setInternationalMerchant(false);
      bulkProcessItem.setStoreId(DEFAULT_STORE_ID);
      bulkProcessItem.setCreatedBy(DEFAULT_USERNAME);
      bulkProcessItem.setCreatedDate(date);
      bulkProcessItem.setUpdatedBy(DEFAULT_USERNAME);
      bulkProcessItem.setUpdatedDate(date);
      bulkProcessItem.setSuccessCount(1);
      bulkProcessItem.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
      bulkProcessItem.setNotes(Constant.GENERIC);
      bulkProcessItem.setDescription("general_template (1).xlsm. proses validasi berlangsung");
    }
    return bulkProcess;
  }

  private ProfileResponse getProfileResponse() {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    List<PickupPointDTO> pickupPointList = new ArrayList<PickupPointDTO>();
    PickupPointDTO pickupPoint = new PickupPointDTO();
    pickupPoint.setCode(PICKUP_POINT_CODE);
    pickupPointList.add(pickupPoint);
    profileResponse.setPickupPoints(pickupPointList);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(false);
    companyDTO.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    profileResponse.setCompany(companyDTO);
    return profileResponse;
  }

  @Test
  public void processReadyToProcessDataTest() throws Exception {
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setNotes(Constant.GENERIC);
    bulkProcessList.get(0).setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    verify(bulkProcessService).publishBulkProductCreationEvent(DEFAULT_STORE_ID, bulkProcessList.get(0),
      BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void convertedUploadProcessReadyToProcessDataTest() throws Exception {
    bulkProcessData = BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10)
      .status(BulkProcessData.STATUS_PENDING).bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2)
      .systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setNotes(Constant.GENERIC);
    bulkProcessList.get(0)
      .setBulkProcessType(BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue());
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10,
      BulkProcess.STATUS_READY_TO_PROCESS,
      BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue())).thenReturn(
      Arrays.asList(bulkProcessList.get(0)));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID,
      BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue());
    verify(bulkProcessService).publishBulkProductCreationEvent(DEFAULT_STORE_ID,
      bulkProcessList.get(0), BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue());
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
      .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_CONVERTED_UPLOAD);
  }

  @Test
  public void externalUploadProcessReadyToProcessDataTest() throws Exception {
    bulkProcessData = BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10)
      .status(BulkProcessData.STATUS_PENDING).bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2)
      .systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setNotes(Constant.GENERIC);
    bulkProcessList.get(0)
      .setBulkProcessType(BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue());
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10,
      BulkProcess.STATUS_READY_TO_PROCESS,
      BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue())).thenReturn(
      Arrays.asList(bulkProcessList.get(0)));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID,
      BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue());
    verify(bulkProcessService).publishBulkProductCreationEvent(DEFAULT_STORE_ID,
      bulkProcessList.get(0), BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue());
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
      .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_EXTERNAL_UPLOAD);
  }

  @Test
  public void processReadyToProcessDataFor_Priority1Test() throws Exception {
    bulkProcessData =
      BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
        .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setNotes(Constant.GENERIC);
    bulkProcessList.get(0).setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
      BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID,
      BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    verify(bulkProcessService).publishBulkProductCreationEvent(DEFAULT_STORE_ID, bulkProcessList.get(0),
      BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
      .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_PRIORITY_1);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessDataFor_Priority2Test() throws Exception {
    bulkProcessData =
      BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
        .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setNotes(Constant.GENERIC);
    bulkProcessList.get(0).setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue());
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
      BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID,
      BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue());
    verify(bulkProcessService).publishBulkProductCreationEvent(DEFAULT_STORE_ID, bulkProcessList.get(0),
      BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue());
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
      .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_PRIORITY_2);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessData_vatUpdateTest() throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.SUBJECT_TO_VAT.getValue())).thenReturn(Arrays.asList(bulkProcess));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID, BulkProcessType.SUBJECT_TO_VAT.getValue());
    verify(bulkProcessService).publishVatUpdateEvent(DEFAULT_STORE_ID, bulkProcess);
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.SUBJECT_TO_VAT.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_VAT_UPDATE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessData_DeleteItemPickupPointTest() throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_DELETE_PICKUP_POINT_READY_TO_PROCESS_BATCH_SIZE))
      .thenReturn(creationBatchSize);
    bulkProcess.setBulkProcessType(BulkProcessType.DELETE_PICKUP_POINT.getValue());
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.DELETE_PICKUP_POINT.getValue())).thenReturn(Arrays.asList(bulkProcess));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID, BulkProcessType.DELETE_PICKUP_POINT.getValue());
    verify(bulkProcessService).publishBulkDeleteItemPickupPointEvent(DEFAULT_STORE_ID, bulkProcess);
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.DELETE_PICKUP_POINT.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_DELETE_PICKUP_POINT_READY_TO_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessData_ASSEMBLY_UPLOADTest() throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.ASSEMBLY_REQUEST.getValue());
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
      BulkProcessType.ASSEMBLY_REQUEST.getValue())).thenReturn(Arrays.asList(bulkProcess));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_WORK_ORDER_UPLOAD_BATCH_SIZE))
      .thenReturn(creationBatchSize);
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID,
      BulkProcessType.ASSEMBLY_REQUEST.getValue());
    verify(bulkProcessService).publishBulkWorkOrderUploadEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessCustomRepository)
      .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.ASSEMBLY_REQUEST.getValue());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_WORK_ORDER_UPLOAD_BATCH_SIZE);
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessData_DISASSEMBLY_UPLOADTest() throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.DISASSEMBLY_REQUEST.getValue());
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
      BulkProcessType.DISASSEMBLY_REQUEST.getValue())).thenReturn(Arrays.asList(bulkProcess));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_WORK_ORDER_UPLOAD_BATCH_SIZE))
      .thenReturn(creationBatchSize);
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID,
      BulkProcessType.DISASSEMBLY_REQUEST.getValue());
    verify(bulkProcessService).publishBulkWorkOrderUploadEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessCustomRepository)
      .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.DISASSEMBLY_REQUEST.getValue());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_WORK_ORDER_UPLOAD_BATCH_SIZE);
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessData_DISASSEMBLY_TransferRequestTest() throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.TRANSFER_REQUEST.getValue());
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
      BulkProcessType.TRANSFER_REQUEST.getValue())).thenReturn(Arrays.asList(bulkProcess));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_WORK_ORDER_UPLOAD_BATCH_SIZE))
      .thenReturn(creationBatchSize);
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID,
      BulkProcessType.TRANSFER_REQUEST.getValue());
    verify(bulkProcessService).publishBulkWorkOrderUploadEvent(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessCustomRepository)
      .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.TRANSFER_REQUEST.getValue());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_WORK_ORDER_UPLOAD_BATCH_SIZE);
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }


  @Test
  public void processReadyToProcessDataWithNullUpdatedListTest() throws Exception {
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setNotes(Constant.GENERIC);
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue())).thenReturn(new ArrayList<>());
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessDataBulkUpdateTest() throws Exception {
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.get(0).setBulkUpdate(true);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.PRODUCT_LEVEL_3.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID, BulkProcessType.PRODUCT_LEVEL_3.getValue());
    verify(bulkProcessService).publishBulkUpdateEvent(DEFAULT_STORE_ID, bulkProcessList.get(0));
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.PRODUCT_LEVEL_3.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_UPDATE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessDataBulkUpdatePriority1Test() throws Exception {
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue());
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.get(0).setBulkUpdate(true);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue()))
        .thenReturn(Arrays.asList(bulkProcessList.get(0)));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_UPDATE_PRIORITY_1_BATCH_SIZE))
        .thenReturn(creationBatchSize);
    bulkProcessServiceWrapper
        .processReadyToProcessData(DEFAULT_STORE_ID, BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue());
    verify(bulkProcessService).publishBulkUpdateEvent(DEFAULT_STORE_ID, bulkProcessList.get(0));
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_READY_TO_PROCESS_UPDATE_PRIORITY_1_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessDataBulkUpdatePriority2Test() throws Exception {
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue());
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.get(0).setBulkUpdate(true);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue()))
        .thenReturn(Arrays.asList(bulkProcessList.get(0)));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_UPDATE_PRIORITY_2_BATCH_SIZE))
        .thenReturn(creationBatchSize);
    bulkProcessServiceWrapper
        .processReadyToProcessData(DEFAULT_STORE_ID, BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue());
    verify(bulkProcessService).publishBulkUpdateEvent(DEFAULT_STORE_ID, bulkProcessList.get(0));
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_READY_TO_PROCESS_UPDATE_PRIORITY_2_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessDataBulkUpdateFalseTest() throws Exception {
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.get(0).setBulkUpdate(false);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.PRODUCT_LEVEL_3.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID, BulkProcessType.PRODUCT_LEVEL_3.getValue());
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.PRODUCT_LEVEL_3.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_UPDATE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessDataEANProductLevel4BulkUpdateTest() throws Exception {
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setBulkProcessType(BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue());
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.get(0).setBulkUpdate(true);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID, BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue());
    verify(bulkProcessService).publishBulkUpdateEANEvent(DEFAULT_STORE_ID, bulkProcessList.get(0));
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessDataEANProductLevel4BulkUpdateFalseTest() throws Exception {
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setBulkProcessType(BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue());
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.get(0).setBulkUpdate(false);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID, BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue());
    verify(bulkProcessService, Mockito.never()).publishBulkUpdateEANEvent(any(), any());
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessCampaignUploadTest() throws Exception {
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setBulkProcessType(BulkProcessType.CAMPAIGN.getValue());
    bulkProcessList.get(0).setTotalCount(10);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.CAMPAIGN.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID, BulkProcessType.CAMPAIGN.getValue());
    verify(bulkProcessService).publishCampaignUploadEvent(DEFAULT_STORE_ID, bulkProcessList.get(0));
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.CAMPAIGN.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_CAMPAIGN_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessEmptyBulkProcessListTest() throws Exception {
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue())).thenReturn(new ArrayList<>());
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }
  @Test
  public void processReadyToProcessDataTestZeroParam() throws Exception {
    creationBatchSize = new SystemParameterConfig("bulk_creation_batch_size", "0", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE)).thenReturn(creationBatchSize);
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE);
  }

  @Test
  public void processProcessedDataTest() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessDataService.getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        Arrays.asList(bulkProcessData));
    doNothing().when(this.bulkFailedProductFileService)
        .createFileAndSendNotification(eq(getBulkProcess().get(0)), anyList(), Mockito.any(MerchantStatusType.class),
            Mockito.anyString(), eq(false));
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    verify(bulkProcessDataService).getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    Mockito.verify(bulkFailedProductFileService)
        .createFileAndSendNotification(any(), anyList(), Mockito.any(MerchantStatusType.class), Mockito.anyString(),
            eq(false));
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(pcbOutboundService)
        .getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_BULK_PROCESS_CODE,
            DEFAULT_USERNAME, Arrays.asList(configurationStatusRequest));
    Mockito.verify(categoryRepository).filterCategoryHierarchyByCategoryCode(DEFAULT_STORE_ID, DEFAULT_CATEGORY_CODE);
  }

  @Test
  public void processProcessedExtrnalUploadDataTest() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setMerchantType("TD");
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
      Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
      BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
        .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessDataService.getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
      Arrays.asList(bulkProcessData));
    doNothing().when(this.bulkFailedProductFileService)
      .createFileAndSendNotification(eq(getBulkProcess().get(0)), anyList(), Mockito.any(MerchantStatusType.class),
        Mockito.anyString(), eq(false));
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
      BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
      BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue());
    verify(bulkProcessDataService).getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    Mockito.verify(bulkFailedProductFileService)
      .createErrorFileForConvertedUploadProcess(anyInt(), any(), anyList(), Mockito.anyString());
    Mockito.verify(bulkProcessCustomRepository)
      .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.EXTERNAL_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processProcessedDataMppSwitchEnabledTest() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    ReflectionTestUtils.setField(bulkProcessServiceWrapper, "multiPickupPointEnabled", true);
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(getProfileResponse());
    when(bulkProcessDataService.getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        Arrays.asList(bulkProcessData));
    doNothing().when(this.bulkFailedProductFileService)
        .createFileAndSendNotification(eq(getBulkProcess().get(0)), anyList(),Mockito.any(MerchantStatusType.class), Mockito.anyString(),
            eq(false));
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    verify(bulkProcessDataService).getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    Mockito.verify(bulkFailedProductFileService)
        .createFileAndSendNotification(any(), anyList(),Mockito.any(MerchantStatusType.class), Mockito.anyString(),
            eq(false));
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(pcbOutboundService)
        .getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_BULK_PROCESS_CODE,
            DEFAULT_USERNAME, Arrays.asList(configurationStatusRequest));
    Mockito.verify(categoryRepository).filterCategoryHierarchyByCategoryCode(DEFAULT_STORE_ID, DEFAULT_CATEGORY_CODE);
  }

  @Test
  public void processProcessedDataMppSwitchEnabledCNCTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessServiceWrapper, "multiPickupPointEnabled", true);
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    when(bulkProcessDataService.getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        Arrays.asList(bulkProcessData));
    doNothing().when(this.bulkFailedProductFileService)
        .createFileAndSendNotification(eq(getBulkProcess().get(0)), anyList(), Mockito.any(MerchantStatusType.class),
            Mockito.anyString(), eq(false));
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    verify(bulkProcessDataService).getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    Mockito.verify(bulkFailedProductFileService)
        .createFileAndSendNotification(any(), anyList(),Mockito.any(MerchantStatusType.class), Mockito.anyString(),
            eq(false));
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(pcbOutboundService)
        .getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_BULK_PROCESS_CODE,
            DEFAULT_USERNAME, Arrays.asList(configurationStatusRequest));
    Mockito.verify(categoryRepository).filterCategoryHierarchyByCategoryCode(DEFAULT_STORE_ID, DEFAULT_CATEGORY_CODE);
  }

  @Test
  public void processProcessedDataTestZeroParam() throws Exception {
    creationBatchSize = new SystemParameterConfig("bulk_creation_batch_size", "0", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE)).thenReturn(creationBatchSize);
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
  }

  @Test
  public void processProcessedDataTest_PostLive() throws Exception {
    Mockito.when(pcbOutboundService.getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        DEFAULT_BULK_PROCESS_CODE, DEFAULT_USERNAME, Arrays.asList(configurationStatusRequest))).thenReturn(null);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    Mockito.when(categoryRepository.filterCategoryHierarchyByCategoryCode(DEFAULT_STORE_ID, DEFAULT_CATEGORY_CODE))
        .thenReturn(getCategoryResponsesWithNullActivationInterval(120));
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setInternationalMerchant(true);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessDataService.getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        Arrays.asList(bulkProcessData));
    doNothing().when(this.bulkFailedProductFileService)
        .createFileAndSendNotification(eq(getBulkProcess().get(0)), anyList(), Mockito.any(MerchantStatusType.class),
            Mockito.anyString(), eq(false));
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    verify(bulkProcessDataService).getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    Mockito.verify(bulkFailedProductFileService)
        .createFileAndSendNotification(any(), anyList(), Mockito.any(MerchantStatusType.class), Mockito.anyString(),
            eq(false));
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(pcbOutboundService)
        .getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_BULK_PROCESS_CODE,
            DEFAULT_USERNAME, Arrays.asList(configurationStatusRequest));
    Mockito.verify(categoryRepository).filterCategoryHierarchyByCategoryCode(DEFAULT_STORE_ID, DEFAULT_CATEGORY_CODE);
  }

  @Test
  public void processProcessedDataTest_PostLive_Exception() throws Exception {
    configurationStatusResponse.setSuccess(false);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            DEFAULT_BULK_PROCESS_CODE, DEFAULT_USERNAME, Arrays.asList(configurationStatusRequest)))
        .thenReturn(configurationStatusResponse);
    Mockito.when(categoryRepository.filterCategoryHierarchyByCategoryCode(DEFAULT_STORE_ID, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setInternationalMerchant(true);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessDataService.getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        Arrays.asList(bulkProcessData));
    doNothing().when(this.bulkFailedProductFileService)
        .createFileAndSendNotification(eq(getBulkProcess().get(0)), anyList(),Mockito.any(MerchantStatusType.class), Mockito.anyString(),
            eq(false));
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    verify(bulkProcessDataService).getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(bulkFailedProductFileService)
        .createFileAndSendNotification(any(), anyList(), Mockito.any(MerchantStatusType.class), Mockito.anyString(),
            eq(false));
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(pcbOutboundService)
        .getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_BULK_PROCESS_CODE,
            DEFAULT_USERNAME, Arrays.asList(configurationStatusRequest));
    Mockito.verify(categoryRepository).filterCategoryHierarchyByCategoryCode(DEFAULT_STORE_ID, DEFAULT_CATEGORY_CODE);
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
  }

  @Test
  public void processBlpBulkProcessEntries_allSuccess() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.remove(1);
    when(bulkProcessDataService.getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        Arrays.asList());
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    verify(bulkProcessDataService).getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    Mockito.verify(bulkFailedProductFileService)
        .createFileAndSendNotification(any(), anyList(),Mockito.any(MerchantStatusType.class), Mockito.anyString(),
            eq(false));
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processBlpBulkProcessEntriesupdatedListNull_allSuccess() throws Exception {
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.remove(1);
    when(bulkProcessDataService.getFailedDataForProcessedFile(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE)).thenReturn(
        Arrays.asList());
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue())).thenReturn(new ArrayList<>());
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(new ArrayList());
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkProcessService).saveBulkProcessList(Mockito.anyList());
  }

  @Test
  public void processProcessedDataTest_ForUpdate() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.PRODUCT_LEVEL_3.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        new ArrayList<>());
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_LEVEL_3.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.PRODUCT_LEVEL_3.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkUpdateService).setFinalStatusAndNotificationOnUpdate(bulkProcessList.get(0), DEFAULT_STORE_ID);
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    Mockito.verify(bulkProcessDataService).getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void processProcessedDataTestForPriority1Update() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue());
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        new ArrayList<>());
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkUpdateService).setFinalStatusAndNotificationOnUpdate(bulkProcessList.get(0), DEFAULT_STORE_ID);
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    Mockito.verify(bulkProcessDataService).getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void processProcessedDataTestForPriority2Update() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue());
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        new ArrayList<>());
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkUpdateService).setFinalStatusAndNotificationOnUpdate(bulkProcessList.get(0), DEFAULT_STORE_ID);
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    Mockito.verify(bulkProcessDataService).getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void processProcessedDataTest_ForCampaignUpload() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setBulkProcessType(BulkProcessType.CAMPAIGN.getValue());
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.CAMPAIGN.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        new ArrayList<>());
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID, BulkProcessType.CAMPAIGN.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED, BulkProcessType.CAMPAIGN.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkUpdateService)
        .setFinalStatusAndNotificationOnCampaignUpload(bulkProcessList.get(0), DEFAULT_STORE_ID);
    Mockito.verify(bulkProcessDataService).getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
  }

  @Test
  public void processProcessedDataTest_ForEANProductLevel4() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setBulkProcessType(BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue());
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        new ArrayList<>());
    doNothing().when(eanProductLevel4BulkUpdateService).setFinalStatusAndNotificationOnEANUpdate(Mockito.anyString(), Mockito.any(BulkProcess.class));
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID, BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED, BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(eanProductLevel4BulkUpdateService)
        .setFinalStatusAndNotificationOnEANUpdate(DEFAULT_STORE_ID, bulkProcessList.get(0));
    Mockito.verify(bulkProcessDataService).getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
  }

  @Test
  public void processProcessedDataTest_defaultTest() throws Exception {
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setBulkProcessType("");
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.CAMPAIGN.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(new ArrayList<>());
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID, BulkProcessType.CAMPAIGN.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.CAMPAIGN.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkProcessService).saveBulkProcessList(Mockito.anyList());
  }

  @Test
  public void processReadyToProcessInstantPickupUpsertTest() throws Exception {
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    bulkProcessList.get(0).setTotalCount(10);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID,
        BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    verify(bulkProcessService).publishBulkInstantPickupItemUpsertEvent(DEFAULT_STORE_ID, bulkProcessList.get(0));
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_UPSERT_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    verify(bulkProcessRepository).saveAll(anyList());
    verify(bulkProcessService).publishBulkInstantPickupItemUpsertEvent(DEFAULT_STORE_ID, bulkProcessList.get(0));
  }

  @Test
  public void processReadyToProcessVatUpdateTest() throws Exception {
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    bulkProcessList.get(0).setTotalCount(10);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.SUBJECT_TO_VAT.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID,
        BulkProcessType.SUBJECT_TO_VAT.getValue());
    verify(bulkProcessService).publishVatUpdateEvent(DEFAULT_STORE_ID, bulkProcessList.get(0));
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.SUBJECT_TO_VAT.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_VAT_UPDATE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    verify(bulkProcessRepository).saveAll(anyList());
    verify(bulkProcessService).publishVatUpdateEvent(DEFAULT_STORE_ID, bulkProcessList.get(0));
  }

  @Test
  public void processReadyToProcessDataInstoreTest() throws Exception {
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setNotes(Constant.GENERIC);
    bulkProcessList.get(0).setBulkProcessType(BulkProcessType.IN_STORE.getValue());
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(0).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.IN_STORE.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID, BulkProcessType.IN_STORE.getValue());
    verify(bulkProcessService).publishInstoreUpdateEvent(DEFAULT_STORE_ID, bulkProcessList.get(0));
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.IN_STORE.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_INSTORE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processReadyToProcessInstantPickupDeleteTest() throws Exception {
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(0).setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcessList.get(0).setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue());
    bulkProcessList.get(0).setTotalCount(10);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID,
        BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue());
    verify(bulkProcessService).publishBulkInstantPickupItemDeleteEvent(DEFAULT_STORE_ID, bulkProcessList.get(0));
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_DELETE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processProcessedDataTest_ForInstoreUpload() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setBulkProcessType(BulkProcessType.IN_STORE.getValue());
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.IN_STORE.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        new ArrayList<>());
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID, BulkProcessType.IN_STORE.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.IN_STORE.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkUpdateService)
        .setFinalStatusAndNotificationOnInstoreUpload(bulkProcessList.get(0), DEFAULT_STORE_ID);
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    Mockito.verify(bulkProcessDataService).getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void processProcessedDataTest_ForVatUpdate() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setBulkProcessType(BulkProcessType.SUBJECT_TO_VAT.getValue());
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.SUBJECT_TO_VAT.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        new ArrayList<>());
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID, BulkProcessType.SUBJECT_TO_VAT.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.SUBJECT_TO_VAT.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkUpdateService)
        .setFinalStatusAndNotificationOnVatBulkUpdate(bulkProcessList.get(0), DEFAULT_STORE_ID);
    Mockito.verify(bulkProcessDataService).getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
  }

  @Test
  public void processReadyToProcessData_bulkArchiveTest() throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.ARCHIVE.getValue());
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.ARCHIVE.getValue())).thenReturn(Arrays.asList(bulkProcess));
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID, BulkProcessType.ARCHIVE.getValue());
    verify(bulkProcessService).publishArchiveProductEvent(DEFAULT_STORE_ID, bulkProcess);
    verify(bulkProcessRepository).saveAll(anyList());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
            BulkProcessType.ARCHIVE.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_ARCHIVE_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processProcessedDataTest_ForArchive() throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.ARCHIVE.getValue());
    bulkProcess.setStoreId(DEFAULT_STORE_ID);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.ARCHIVE.getValue())).thenReturn(Arrays.asList(bulkProcess));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(Arrays.asList(bulkProcess));
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        new ArrayList<>());
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID, BulkProcessType.ARCHIVE.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED, BulkProcessType.ARCHIVE.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkArchiveService).setFinalStatusAndSendNotificationOnBulkArchive(bulkProcess, DEFAULT_STORE_ID);
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    Mockito.verify(bulkProcessDataService).getFailedDataForProcessedFile(Mockito.anyString(), Mockito.any());
  }

  @Test
  public void processProcessedDataTest_ForInstantPickupUpsert() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        Arrays.asList(bulkProcessData));
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkUpsertService)
        .setFinalStatusAndNotificationOnInstantPickupUpsert(bulkProcessList.get(0), DEFAULT_STORE_ID);
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    Mockito.verify(bulkProcessDataService).getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void processProcessedDataTest_ForInstantPickupDelete() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue());
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        new ArrayList<>());
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkDeleteService)
        .setFinalStatusAndNotificationOnInstantPickupDelete(bulkProcessList.get(0), DEFAULT_STORE_ID);
    Mockito.verify(bulkProcessDataService).getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
  }

  @Test
  public void processProcessedDataTest_ForPickupPointDelete() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
        BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
            .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setBulkProcessType(BulkProcessType.DELETE_PICKUP_POINT.getValue());
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.DELETE_PICKUP_POINT.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        new ArrayList<>());

    Mockito.when(bulkProcessDataService.findByStoreIdAndBulkProcess(Mockito.eq(DEFAULT_STORE_ID), Mockito.any()))
        .thenReturn(new ArrayList<>());
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.DELETE_PICKUP_POINT.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.DELETE_PICKUP_POINT.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkUpdateService)
        .sendFinalStatusEventForPickupPointDelete(DEFAULT_STORE_ID, bulkProcessList.get(0), new ArrayList<>());
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    Mockito.verify(bulkProcessDataService).findByStoreIdAndBulkProcess(Mockito.any(),Mockito.any());
  }

  @Test
  public void processProcessedDataTest_ForQRGeneration() throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(DEFAULT_STORE_ID);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.QR_GENERATION.getValue())).thenReturn(Collections.singletonList(bulkProcess));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(Collections.singletonList(bulkProcess));
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        new ArrayList<>());

    Mockito.when(bulkProcessDataService.findByStoreIdAndBulkProcess(Mockito.eq(DEFAULT_STORE_ID), Mockito.any()))
        .thenReturn(new ArrayList<>());
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.QR_GENERATION.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.QR_GENERATION.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(qrCodeFinalizeService)
        .setFinalStatusAndSendNotificationOnQRGeneration(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
  }

  @Test
  public void processProcessedDataTest_ForBasicInfoUpdateNotificationTest() throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(DEFAULT_STORE_ID);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
        Mockito.anyString())).thenReturn(profileResponse);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.PRODUCT_BASIC_INFO.getValue())).thenReturn(Collections.singletonList(bulkProcess));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(Collections.singletonList(bulkProcess));
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        new ArrayList<>());

    Mockito.when(bulkProcessDataService.findByStoreIdAndBulkProcess(Mockito.eq(DEFAULT_STORE_ID), Mockito.any()))
        .thenReturn(new ArrayList<>());
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID,
        BulkProcessType.PRODUCT_BASIC_INFO.getValue());
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
            BulkProcessType.PRODUCT_BASIC_INFO.getValue());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkUpdateService)
        .setFinalStatusAndNotificationOnBasicInfoUpdate(DEFAULT_STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
  }

  @Test
  public void processReadyToProcessInvalidTypeTest() throws Exception {
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS,
        BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue())).thenReturn(new ArrayList<>());
    bulkProcessServiceWrapper.processReadyToProcessData(DEFAULT_STORE_ID, "Recategorization");
    Mockito.verify(bulkProcessCustomRepository)
        .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_READY_TO_PROCESS, "Recategorization");
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_READY_TO_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
  }

  @Test
  public void processProcessedDataAssemblyRequestUploadTest() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
     when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
      Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
      BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
        .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setBulkProcessType(BulkProcessType.ASSEMBLY_REQUEST.getValue());
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
      BulkProcessType.ASSEMBLY_REQUEST.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
      new ArrayList<>());
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(any(), any())).thenReturn(Collections.singletonList(bulkProcessData));
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID, BulkProcessType.ASSEMBLY_REQUEST.getValue());
    Mockito.verify(bulkProcessCustomRepository)
      .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.ASSEMBLY_REQUEST.getValue());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkUpdateService)
      .setFinalStatusAndNotificationForWorkOrderUpload(DEFAULT_STORE_ID ,bulkProcessList.get(0) ,
        Collections.singletonList(bulkProcessData));
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(any(), any());
  }

  @Test
  public void processProcessedDataDisAssemblyRequestUploadTest() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
      Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
      BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
        .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_PROCESSED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setBulkProcessType(BulkProcessType.DISASSEMBLY_REQUEST.getValue());
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
      BulkProcessType.DISASSEMBLY_REQUEST.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
      new ArrayList<>());
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(any(), any())).thenReturn(Collections.singletonList(bulkProcessData));
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID, BulkProcessType.DISASSEMBLY_REQUEST.getValue());
    Mockito.verify(bulkProcessCustomRepository)
      .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.DISASSEMBLY_REQUEST.getValue());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkUpdateService)
      .setFinalStatusAndNotificationForWorkOrderUpload(DEFAULT_STORE_ID ,bulkProcessList.get(0) ,
        Collections.singletonList(bulkProcessData));
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(any(), any());
  }

  @Test
  public void processProcessedDataTransferRequestUploadTest() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
      Mockito.anyString())).thenReturn(profileResponse);
    bulkProcessData =
      BulkProcessData.builder().parentProduct(PARENT_PRODUCT).rowNumber(10).status(BulkProcessData.STATUS_PENDING)
        .bulkRequestData(BULK_PROCESS_DATA).inputErrorCount(2).systemErrorCount(3).build();
    List<BulkProcess> bulkProcessList = getBulkProcess();
    bulkProcessList.get(1).setStatus(BulkProcess.STATUS_FINISHED);
    bulkProcessList.get(1).setTotalCount(10);
    bulkProcessList.get(1).setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcessList.get(1).setBulkProcessType(BulkProcessType.TRANSFER_REQUEST.getValue());
    bulkProcessList.get(1).setNotes(DEFAULT_CATEGORY_CODE);
    bulkProcessList.remove(0);
    when(bulkProcessCustomRepository.findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
      BulkProcessType.TRANSFER_REQUEST.getValue())).thenReturn(Arrays.asList(bulkProcessList.get(0)));
    when(bulkProcessService.saveBulkProcessList(Mockito.anyList())).thenReturn(bulkProcessList);
    when(bulkProcessDataService.getFailedDataForProcessedFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
      new ArrayList<>());
    when(bulkProcessDataService.findByStoreIdAndBulkProcess(any(), any())).thenReturn(Collections.singletonList(bulkProcessData));
    bulkProcessServiceWrapper.processProcessedData(DEFAULT_STORE_ID, BulkProcessType.TRANSFER_REQUEST.getValue());
    Mockito.verify(bulkProcessCustomRepository)
      .findBlpToProcess(DEFAULT_STORE_ID, true, 10, BulkProcess.STATUS_PROCESSED,
        BulkProcessType.TRANSFER_REQUEST.getValue());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_PROCESSED_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.BULK_CREATION_PROCESS_ORDER_BY);
    Mockito.verify(bulkUpdateService)
      .setFinalStatusAndNotificationForWorkOrderUpload(DEFAULT_STORE_ID ,bulkProcessList.get(0) ,
        Collections.singletonList(bulkProcessData));
    Mockito.verify(bulkProcessService, times(2)).saveBulkProcessList(Mockito.anyList());
    verify(bulkProcessDataService).findByStoreIdAndBulkProcess(any(), any());
  }

  @Test
  public void testAbortStruckProcessesByProcessTypes() {
    List<String> bulkProcessTypes = Arrays.asList(BulkProcessType.CAMPAIGN.name(),BulkProcessType.ASSEMBLY_REQUEST.name());
    Map<String, Integer> expectedAbortTimeMap = new HashMap<>();
    expectedAbortTimeMap.put(BulkProcessType.CAMPAIGN.name(), 10);
    expectedAbortTimeMap.put(BulkProcessType.ASSEMBLY_REQUEST.name(), 200);

    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConfigNames.CAMPAIGN_STRUCK_PROCESS_IN_MINUTES))
      .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.CAMPAIGN_STRUCK_PROCESS_IN_MINUTES,
        String.valueOf(10), "desc"));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.ASSEMBLY_REQUEST_STRUCK_PROCESS_IN_MINUTES))
      .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.CAMPAIGN_STRUCK_PROCESS_IN_MINUTES,
        String.valueOf(200), "desc"));

    bulkProcessServiceWrapper.abortStruckProcessesByProcessTypes(DEFAULT_STORE_ID, bulkProcessTypes);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.ASSEMBLY_REQUEST_STRUCK_PROCESS_IN_MINUTES);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConfigNames.CAMPAIGN_STRUCK_PROCESS_IN_MINUTES);
    verify(bulkProcessService).abortStruckProcessesByProcessTypes(DEFAULT_STORE_ID, expectedAbortTimeMap);

  }
}
