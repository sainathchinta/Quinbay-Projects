package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyIterable;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.ProductDetailsRequest;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.repository.BulkProcessDataRepository;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemL5ListingResponse;

public class BulkProcessDataServiceBeanTest {

  private static final String STORE_ID = "10001";
  private static final String BULK_PROCESS_CODE = "blp";
  private static final String PARENT_PRODUCT = "parentProduct";
  private static final String PRODUCT_LEVEL3_PROCESSOR_DIR = "ProductLevel3Processor";
  private BulkProcessData bulkProcessData = new BulkProcessData();
  private BulkProcess bulkProcess = new BulkProcess();
  private static final String STATUS = "PENDING";
  private final static String REQUEST_ID = "REQUEST_ID";
  private final static String MERCHANT_CODE = "MERCHANT_CODE";
  private final static String QR_GENERATION_TYPE = "STORE";
  private final static String TEMPLATE_SIZE = "2";
  private final static String BULK_PROCESS_ID = "BULK_PROCESS_ID";
  private final static String PP_CODE_1 = "PP-001";
  private final static String PP_CODE_2 = "PP-002";
  private final static String PP_CODE_3 = "PP-003";
  private final static String PP_CODE_4 = "PP-004";
  private final static String PP_CODE_5 = "PP-005";
  private final static String PP_CODE_6 = "PP-006";
  private final static String PP_CODE_7 = "PP-007";
  private final static String PRODUCT_SKU = "PRODUCT_SKU";
  private final static String PRODUCT_SKU1 = "PRODUCT_SKU1";
  private final static String ITEM_SKU = "ITEM_SKU";
  private final static String PRICE = "10";

  private DownloadQRCodeRequest downloadQRCodeRequest;
  private List<ProductDetailsRequest> productDetailsRequestList ;
  ProductDetailsRequest productDetailsRequest;
  ProductDetailsRequest productDetailsRequest1;
  private ObjectMapper mapper;
  private Pageable pageable = PageRequest.of(0, 10);

  @InjectMocks
  BulkProcessDataServiceBean bulkProcessDataServiceBean;

  @Mock
  BulkProcessDataRepository bulkProcessDataRepository;

  @Mock
  SystemParameterConfigService systemParameterConfigService;

  @Mock
  private PickupPointService pickupPointService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);

    mapper = new ObjectMapper();

    downloadQRCodeRequest = new DownloadQRCodeRequest();
    downloadQRCodeRequest.setRequestId(REQUEST_ID);
    downloadQRCodeRequest.setStoreId(STORE_ID);
    downloadQRCodeRequest.setMerchantCode(MERCHANT_CODE);
    downloadQRCodeRequest.setRequestId(REQUEST_ID);
    downloadQRCodeRequest.setStoreId(STORE_ID);
    downloadQRCodeRequest.setAllStores(true);
    downloadQRCodeRequest.setQrPerPage(2);
    downloadQRCodeRequest.setMerchantCode(MERCHANT_CODE);
    downloadQRCodeRequest.setIsDarkTheme(Boolean.FALSE);
    downloadQRCodeRequest.setPrintPrice(true);
    downloadQRCodeRequest.setQrGenerationType(QR_GENERATION_TYPE);
    downloadQRCodeRequest.setTemplateSize(TEMPLATE_SIZE);

    productDetailsRequestList = new ArrayList<>();
    productDetailsRequest = new ProductDetailsRequest();
    productDetailsRequest1 = new ProductDetailsRequest();

    ReflectionTestUtils.setField(bulkProcessDataServiceBean, "fetchItemPickupPointSize", 1);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(bulkProcessDataRepository);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(pickupPointService);
    Mockito.verifyNoMoreInteractions(xProductOutboundService);
  }

  @Test
  public void findByStoreIdAndBulkProcessCodeTest() throws Exception {
    bulkProcessDataServiceBean.findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessDataRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(Constant.STORE_ID, bulkProcess.getBulkProcessCode());
  }

  @Test
  public void saveOperation() {
    Mockito.when(bulkProcessDataRepository.findById(bulkProcessData.getId())).thenReturn(Optional.of(bulkProcessData));
    Mockito.when(bulkProcessDataRepository.save(bulkProcessData)).thenReturn(bulkProcessData);
    this.bulkProcessDataServiceBean.saveOperation(bulkProcessData);
    Mockito.verify(bulkProcessDataRepository).findById(bulkProcessData.getId());
    Mockito.verify(bulkProcessDataRepository).save(bulkProcessData);
  }

  @Test
  public void findByBulkProcessCodeAndParentProduct() throws Exception {
    Mockito.when(
            bulkProcessDataRepository.findByStoreIdAndBulkProcessCodeAndParentProductAndStatusAndMarkForDeleteFalse(
                STORE_ID, bulkProcess.getBulkProcessCode(), PARENT_PRODUCT, BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData));
    this.bulkProcessDataServiceBean.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess,
        PARENT_PRODUCT, BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataRepository)
        .findByStoreIdAndBulkProcessCodeAndParentProductAndStatusAndMarkForDeleteFalse(STORE_ID,
            bulkProcess.getBulkProcessCode(), PARENT_PRODUCT, BulkProcessData.STATUS_PENDING);
  }

  @Test
  public void findByBulkProcessIdAndParentProductAndStatusTest() throws Exception {
    bulkProcessDataServiceBean.findByBulkProcessIdAndParentProductAndStatus(STORE_ID,
        BULK_PROCESS_ID, PARENT_PRODUCT, BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataRepository)
        .findByStoreIdAndBulkProcessIdAndParentProductAndStatusAndMarkForDeleteFalse(STORE_ID,
            BULK_PROCESS_ID, PARENT_PRODUCT, BulkProcessData.STATUS_PENDING);
  }

  @Test
  public void getFailedDataForProcessedFile() throws Exception {
    Mockito.when(bulkProcessDataRepository
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcessData.STATUS_FAIL)).thenReturn(Arrays.asList(bulkProcessData));
    this.bulkProcessDataServiceBean.getFailedDataForProcessedFile(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcessData.STATUS_FAIL);
  }

  @Test
  public void getFailedDataForProcessedFileInStatusIn() throws Exception {
    List<String> statusList = new ArrayList<>();
    statusList.add(BulkProcessData.STATUS_FAIL);
    statusList.add(BulkProcessData.STATUS_SUCCESS);
    Mockito.when(bulkProcessDataRepository
        .findByStoreIdAndBulkProcessCodeAndStatusInAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            statusList)).thenReturn(Arrays.asList(bulkProcessData));
    this.bulkProcessDataServiceBean.getFailedDataForProcessedFileInStatusIn(STORE_ID, BULK_PROCESS_CODE, statusList);
    Mockito.verify(bulkProcessDataRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusInAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            statusList);
  }

  @Test
  public void getDistinctParentProduct() throws Exception {
    Mockito.when(bulkProcessDataRepository
        .getDistinctParentForBlpCode(STORE_ID, BULK_PROCESS_CODE, BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(PARENT_PRODUCT));
    this.bulkProcessDataServiceBean.getDistinctParentProduct(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataRepository)
        .getDistinctParentForBlpCode(STORE_ID, BULK_PROCESS_CODE, BulkProcessData.STATUS_PENDING);
  }

  @Test
  public void deleteDataByUpdatedDate() {
    Mockito.doNothing().when(bulkProcessDataRepository).deleteByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    this.bulkProcessDataServiceBean.deleteDataByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataRepository).deleteByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
  }

  @Test
  public void saveBulkProcessData() {
    Mockito.when(bulkProcessDataRepository.saveAll(Arrays.asList(bulkProcessData)))
        .thenReturn(Arrays.asList(bulkProcessData));
    this.bulkProcessDataServiceBean.saveBulkProcessData(Arrays.asList(bulkProcessData));
    Mockito.verify(bulkProcessDataRepository).saveAll(Arrays.asList(bulkProcessData));
  }

  @Test
  public void saveBulkProcessOperationData() {
    Mockito.when(bulkProcessDataRepository.saveAll(Arrays.asList(bulkProcessData)))
      .thenReturn(Arrays.asList(bulkProcessData));
    this.bulkProcessDataServiceBean.saveOperationBulkProcessData(Arrays.asList(bulkProcessData));
    Mockito.verify(bulkProcessDataRepository).saveAll(Arrays.asList(bulkProcessData));
  }

  @Test
  public void saveAndReturnBulkProcessData() {
    Mockito.when(bulkProcessDataRepository.saveAll(Arrays.asList(bulkProcessData)))
        .thenReturn(Arrays.asList(bulkProcessData));
    this.bulkProcessDataServiceBean.saveBulkProcessData(Arrays.asList(bulkProcessData));
    Mockito.verify(bulkProcessDataRepository).saveAll(Arrays.asList(bulkProcessData));
  }

  @Test
  public void updatePendingImageDownloadsTest() {
    Date date = new Date();
    Mockito.doNothing().when(bulkProcessDataRepository)
        .updatePendingProcesses(STORE_ID, Arrays.asList(BULK_PROCESS_CODE), date, Constant.SYSTEM_ERROR);
    this.bulkProcessDataServiceBean.updatePendingProcesses(STORE_ID, Arrays.asList(BULK_PROCESS_CODE), date);
    Mockito.verify(bulkProcessDataRepository)
        .updatePendingProcesses(STORE_ID, Arrays.asList(BULK_PROCESS_CODE), date, Constant.SYSTEM_ERROR);
  }

  @Test
  public void findByStoreIdAndBulkProcessCodeAndRowNumberIn() throws Exception {
    Mockito.when(
        bulkProcessDataRepository.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
            Arrays.asList(1), STATUS)).thenReturn(Arrays.asList(bulkProcessData));
    this.bulkProcessDataServiceBean.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE,
        Arrays.asList(1), STATUS);
    Mockito.verify(bulkProcessDataRepository)
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(STORE_ID, BULK_PROCESS_CODE, Arrays.asList(1), STATUS);
  }

  @Test
  public void findByStoreIdAndBulkProcessCodeAndStatusTest() throws Exception {
    Mockito.when(bulkProcessDataRepository.findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, BULK_PROCESS_CODE, STATUS,
        pageable)).thenReturn(new PageImpl<>(Arrays.asList(bulkProcessData)));
    this.bulkProcessDataServiceBean.findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, BULK_PROCESS_CODE, STATUS,
        pageable);
    Mockito.verify(bulkProcessDataRepository)
        .findByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, BULK_PROCESS_CODE, STATUS, pageable);
  }

  @Test
  public void findRowNumberByStoreIdAndBulkProcessCodeAndStatusTest() throws Exception {
    Mockito.when(bulkProcessDataRepository
        .findRowNumberByStoreIdAndBulkProcessCodeAndStatus(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(Arrays.asList(1, 2));
    this.bulkProcessDataServiceBean
        .findRowNumberByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, BULK_PROCESS_CODE, STATUS);
    Mockito.verify(bulkProcessDataRepository)
        .findRowNumberByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, BULK_PROCESS_CODE, STATUS);
  }

  @Test
  public void getPendingBulkProcessCodesTest() throws Exception {
    Mockito.when(bulkProcessDataRepository.getPendingBulkProcessCodes(STORE_ID, Arrays.asList(BULK_PROCESS_CODE)))
        .thenReturn(new ArrayList<>());
    this.bulkProcessDataServiceBean.getPendingBulkProcessCodes(STORE_ID, Arrays.asList(BULK_PROCESS_CODE));
    Mockito.verify(bulkProcessDataRepository).getPendingBulkProcessCodes(STORE_ID, Arrays.asList(BULK_PROCESS_CODE));
  }

  @Test
  public void getRowNumberAndParentProductByStoreIdAndBulkProcessCodeAndStatusTest() {
    bulkProcessDataServiceBean
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, BULK_PROCESS_CODE, STATUS);
    Mockito.verify(bulkProcessDataRepository)
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, BULK_PROCESS_CODE, STATUS);
  }

  @Test
  public void abortPendingBulkProcessBeforeTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.ABORT_PENDING_BULK_PROCESS_LIMIT);
    systemParameterConfig.setValue(String.valueOf(20));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.ABORT_PENDING_BULK_PROCESS_LIMIT))
      .thenReturn(systemParameterConfig);
    Mockito.doNothing().when(bulkProcessDataRepository)
      .updateStatusInPendingOrInProgressBulkProcessToAborted(any(Date.class));
    bulkProcessDataServiceBean.abortPendingBulkProcessDataBeforeOrById(STORE_ID, StringUtils.EMPTY);
    Mockito.verify(bulkProcessDataRepository).updateStatusInPendingOrInProgressBulkProcessToAborted(any(Date.class));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.ABORT_PENDING_BULK_PROCESS_LIMIT);
  }

  @Test
  public void abortPendingBulkProcessWithIdTest() {
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(SystemParameterConfigNames.ABORT_PENDING_BULK_PROCESS_LIMIT);
    systemParameterConfig.setValue(String.valueOf(20));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.ABORT_PENDING_BULK_PROCESS_LIMIT))
      .thenReturn(systemParameterConfig);
    Mockito.doNothing().when(bulkProcessDataRepository)
      .updateBulkProcessDataStatusToFailById(BULK_PROCESS_CODE);
    bulkProcessDataServiceBean.abortPendingBulkProcessDataBeforeOrById(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataRepository).updateBulkProcessDataStatusToFailById(BULK_PROCESS_CODE);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.ABORT_PENDING_BULK_PROCESS_LIMIT);
  }

  @Test
  public void downloadQRCodeSuccess() throws Exception {
    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    when(objectMapper.readValue(mapper.writeValueAsString(downloadQRCodeRequest),
        DownloadQRCodeRequest.class)).thenReturn(downloadQRCodeRequest);
    when(pickupPointService.getPickupPointSummaryFilter(anyInt(), any()))
        .thenReturn(constructPickupPointResponse());
    bulkProcessDataServiceBean.saveRequestInBulkProcessData(downloadQRCodeRequest, bulkProcessData);
    verify(pickupPointService).getPickupPointSummaryFilter(anyInt(), any());
    verify(bulkProcessDataRepository, times(1)).saveAll(anyIterable());
  }

  @Test
  public void downloadQRCode_defaultStoreSelectionSuccess() throws Exception {
    downloadQRCodeRequest.setProductDetailsRequestList(null);
    downloadQRCodeRequest.setAllStores(false);
    downloadQRCodeRequest.setQrGenerationType(AllowedQRGenerationType.STORE.getValue());
    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    when(objectMapper.readValue(mapper.writeValueAsString(downloadQRCodeRequest),
        DownloadQRCodeRequest.class)).thenReturn(downloadQRCodeRequest);
    when(pickupPointService.getPickupPointSummaryFilter(anyInt(), any()))
        .thenReturn(constructPickupPointResponse());
    bulkProcessDataServiceBean.saveRequestInBulkProcessData(downloadQRCodeRequest, bulkProcessData);
    verify(bulkProcessDataRepository, times(1)).saveAll(anyIterable());
  }

  @Test
  public void downloadQRCodeSuccessPPThrowsException() throws Exception {

    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);

    when(objectMapper.readValue(mapper.writeValueAsString(downloadQRCodeRequest),
        DownloadQRCodeRequest.class)).thenReturn(downloadQRCodeRequest);
    when(pickupPointService.getPickupPointSummaryFilter(anyInt(), any()))
        .thenReturn(new ArrayList<>());
    try {
      bulkProcessDataServiceBean.saveRequestInBulkProcessData(downloadQRCodeRequest, bulkProcessData);
    }catch (Exception e){
      verify(pickupPointService).getPickupPointSummaryFilter(anyInt(), any());
    }
  }

  @Test
  public void downloadQRCodeSuccessWithAllStoreFalse() throws Exception {

    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);

    List<ProductDetailsRequest> list = new ArrayList<>();

    ProductDetailsRequest productDetailsRequest = ProductDetailsRequest.builder()
        .pickupPointCode("PP-001")
        .build();
    list.add(productDetailsRequest);
    downloadQRCodeRequest.setAllStores(false);
    downloadQRCodeRequest.setProductDetailsRequestList(list);

    bulkProcessDataServiceBean.saveRequestInBulkProcessData(downloadQRCodeRequest, bulkProcessData);
    verify(bulkProcessDataRepository, times(1)).saveAll(anyIterable());
  }

  @Test
  public void downloadQRCodeSuccessWithQtyPerPage0() throws Exception {

    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);

    downloadQRCodeRequest.setQrPerPage(0);

    when(objectMapper.readValue(mapper.writeValueAsString(downloadQRCodeRequest),
        DownloadQRCodeRequest.class)).thenReturn(downloadQRCodeRequest);
    try {
      bulkProcessDataServiceBean.saveRequestInBulkProcessData(downloadQRCodeRequest,
          bulkProcessData);
    } catch (Exception e) {

    }
  }


  @Test
  public void downloadQRCodeSuccessForProductTypeSuccess() throws Exception {
    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    downloadQRCodeRequest.setQrGenerationType(Constant.PRODUCT);
    productDetailsRequest.setProductSku(PRODUCT_SKU);
    downloadQRCodeRequest.setProductDetailsRequestList(
        Arrays.asList(productDetailsRequest, new ProductDetailsRequest()));
    when(objectMapper.readValue(mapper.writeValueAsString(downloadQRCodeRequest),
        DownloadQRCodeRequest.class)).thenReturn(downloadQRCodeRequest);
    bulkProcessDataServiceBean.saveRequestInBulkProcessData(downloadQRCodeRequest, bulkProcessData);
    verify(bulkProcessDataRepository, times(1)).saveAll(anyIterable());
  }

  @Test
  public void downloadQRCodeSuccessForProductTypeEmptyListTest() throws Exception {
    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    downloadQRCodeRequest.setQrGenerationType(Constant.PRODUCT);
    when(objectMapper.readValue(mapper.writeValueAsString(downloadQRCodeRequest),
        DownloadQRCodeRequest.class)).thenReturn(downloadQRCodeRequest);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkProcessDataServiceBean.saveRequestInBulkProcessData(downloadQRCodeRequest,
              bulkProcessData));
    } finally {
      verify(bulkProcessDataRepository, times(0)).saveAll(anyIterable());
    }
  }

  @Test
  public void downloadQRCodeSuccessForItemTypeEmptyListTest() throws Exception {
    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    downloadQRCodeRequest.setQrGenerationType(Constant.ITEM);
    when(objectMapper.readValue(mapper.writeValueAsString(downloadQRCodeRequest),
        DownloadQRCodeRequest.class)).thenReturn(downloadQRCodeRequest);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkProcessDataServiceBean.saveRequestInBulkProcessData(downloadQRCodeRequest,
              bulkProcessData));
    } finally {
      verify(bulkProcessDataRepository, times(0)).saveAll(anyIterable());
    }
  }

  @Test
  public void downloadQRCodeSuccessForItemTypeSuccessTest() throws Exception {
    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    downloadQRCodeRequest.setQrGenerationType(Constant.ITEM);
    productDetailsRequest.setItemSku(ITEM_SKU);
    productDetailsRequest1.setProductSku(PRODUCT_SKU);
    downloadQRCodeRequest.setProductDetailsRequestList(Arrays.asList(productDetailsRequest, productDetailsRequest1, new ProductDetailsRequest()));
    when(objectMapper.readValue(mapper.writeValueAsString(downloadQRCodeRequest),
        DownloadQRCodeRequest.class)).thenReturn(downloadQRCodeRequest);
    List<ItemBasicDetailV2Response> basicDetailV2ResponseList = new ArrayList<>();
    basicDetailV2ResponseList.add(new ItemBasicDetailV2Response());
    when(xProductOutboundService.getItemBasicDetails(PRODUCT_SKU)).thenReturn(basicDetailV2ResponseList);
    bulkProcessDataServiceBean.saveRequestInBulkProcessData(downloadQRCodeRequest, bulkProcessData);
    verify(bulkProcessDataRepository, times(1)).saveAll(anyIterable());
    verify(xProductOutboundService).getItemBasicDetails(PRODUCT_SKU);
  }

  @Test
  public void downloadQRCodeSuccessForItemTypeEmptyResponseFromXproducttTest() throws Exception {
    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    downloadQRCodeRequest.setQrGenerationType(Constant.ITEM);
    when(objectMapper.readValue(mapper.writeValueAsString(downloadQRCodeRequest),
        DownloadQRCodeRequest.class)).thenReturn(downloadQRCodeRequest);
    productDetailsRequest.setItemSku(ITEM_SKU);
    productDetailsRequest1.setProductSku(PRODUCT_SKU);
    downloadQRCodeRequest.setProductDetailsRequestList(Arrays.asList(productDetailsRequest, productDetailsRequest1));
    when(xProductOutboundService.getItemBasicDetails(PRODUCT_SKU)).thenReturn(new ArrayList<>());
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkProcessDataServiceBean.saveRequestInBulkProcessData(downloadQRCodeRequest,
              bulkProcessData));
    } finally {
      verify(xProductOutboundService).getItemBasicDetails(PRODUCT_SKU);
    }
  }

  @Test
  public void downloadQRCodeSuccessForItemVariantTypeEmptyListTest() throws Exception {
    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    downloadQRCodeRequest.setQrGenerationType(Constant.ITEM_PICKUP_POINT);
    when(objectMapper.readValue(mapper.writeValueAsString(downloadQRCodeRequest),
        DownloadQRCodeRequest.class)).thenReturn(downloadQRCodeRequest);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkProcessDataServiceBean.saveRequestInBulkProcessData(downloadQRCodeRequest,
              bulkProcessData));
    } finally {
      verify(bulkProcessDataRepository, times(0)).saveAll(anyIterable());
    }
  }

  @Test
  public void downloadQRCodeSuccessForItemVariantTypeSuccessTest() throws Exception {
    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    downloadQRCodeRequest.setQrGenerationType(Constant.ITEM_PICKUP_POINT);
    productDetailsRequest.setItemSku(ITEM_SKU);
    productDetailsRequest.setPickupPointCode(PP_CODE_1);
    productDetailsRequest1.setProductSku(PRODUCT_SKU);
    downloadQRCodeRequest.setProductDetailsRequestList(Arrays.asList(productDetailsRequest, productDetailsRequest1, new ProductDetailsRequest()));
    when(objectMapper.readValue(mapper.writeValueAsString(downloadQRCodeRequest),
        DownloadQRCodeRequest.class)).thenReturn(downloadQRCodeRequest);
    List<ItemL5ListingResponse> basicDetailV2ResponseList = new ArrayList<>();
    basicDetailV2ResponseList.add(new ItemL5ListingResponse());
    when(xProductOutboundService.getItemL5Details(PRODUCT_SKU, false, 0, 1)).thenReturn(
        new PageImpl<ItemL5ListingResponse>(basicDetailV2ResponseList) {
        });
    bulkProcessDataServiceBean.saveRequestInBulkProcessData(downloadQRCodeRequest, bulkProcessData);
    verify(bulkProcessDataRepository, times(1)).saveAll(anyIterable());
    verify(xProductOutboundService).getItemL5Details(PRODUCT_SKU, false, 0, 1);
  }

  @Test
  public void downloadQRCodeSuccessForItemPickupPointTypeSuccessTest() throws Exception {
    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    downloadQRCodeRequest.setQrGenerationType(Constant.ITEM_PICKUP_POINT);
    productDetailsRequest.setItemSku(ITEM_SKU);
    productDetailsRequest.setPickupPointCode(PP_CODE_1);
    productDetailsRequest.setProductPrice(PRICE);
    productDetailsRequest1.setProductSku(PRODUCT_SKU);
    downloadQRCodeRequest.setProductDetailsRequestList(Arrays.asList(productDetailsRequest, productDetailsRequest1, new ProductDetailsRequest()));
    when(objectMapper.readValue(mapper.writeValueAsString(downloadQRCodeRequest),
        DownloadQRCodeRequest.class)).thenReturn(downloadQRCodeRequest);
    List<ItemL5ListingResponse> basicDetailV2ResponseList = new ArrayList<>();
    basicDetailV2ResponseList.add(new ItemL5ListingResponse());
    when(xProductOutboundService.getItemL5Details(PRODUCT_SKU, false, 0, 1)).thenReturn(
        new PageImpl<ItemL5ListingResponse>(basicDetailV2ResponseList) {
        });
    bulkProcessDataServiceBean.saveRequestInBulkProcessData(downloadQRCodeRequest, bulkProcessData);
    verify(bulkProcessDataRepository, times(1)).saveAll(anyIterable());
    verify(xProductOutboundService).getItemL5Details(PRODUCT_SKU, false, 0, 1);
  }

  private List<PickupPointResponse> constructPickupPointResponse() {

    PickupPointResponse pickupPointResponse = PickupPointResponse.builder()
        .code(PP_CODE_1)
        .name(PP_CODE_1)
        .build();
    PickupPointResponse pickupPointResponse1 = PickupPointResponse.builder()
        .code(PP_CODE_2)
        .name(PP_CODE_2)
        .build();
    PickupPointResponse pickupPointResponse2 = PickupPointResponse.builder()
        .code(PP_CODE_3)
        .name(PP_CODE_3)
        .build();
    PickupPointResponse pickupPointResponse3 = PickupPointResponse.builder()
        .code(PP_CODE_4)
        .name(PP_CODE_4)
        .build();
    PickupPointResponse pickupPointResponse4 = PickupPointResponse.builder()
        .code(PP_CODE_5)
        .name(PP_CODE_5)
        .build();
    PickupPointResponse pickupPointResponse5 = PickupPointResponse.builder()
        .code(PP_CODE_6)
        .name(PP_CODE_6)
        .build();
    PickupPointResponse pickupPointResponse6 = PickupPointResponse.builder()
        .code(PP_CODE_7)
        .name(PP_CODE_7)
        .build();
    List<PickupPointResponse> list = new ArrayList<>();
    list.add(pickupPointResponse);
    list.add(pickupPointResponse1);
    list.add(pickupPointResponse2);
    list.add(pickupPointResponse3);
    list.add(pickupPointResponse4);
    list.add(pickupPointResponse5);
    list.add(pickupPointResponse6);

    return list;
  }

  @Test
  public void downloadQRCodeSuccessWithAllProducts() throws Exception {

    BulkProcess bulkProcessData = new BulkProcess();
    bulkProcessData.setId(BULK_PROCESS_ID);
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);

    downloadQRCodeRequest.setAllStores(false);
    downloadQRCodeRequest.setQrGenerationType(Constant.ALL_PRODUCTS);

    bulkProcessDataServiceBean.saveRequestInBulkProcessData(downloadQRCodeRequest, bulkProcessData);
    verify(bulkProcessDataRepository, times(1)).saveAll(anyIterable());
  }

  @Test
  public void getBulkProcessDataDTOByStoreIdAndBulkProcessCodeAndStatusTest() throws Exception{
    bulkProcessDataServiceBean
      .getBulkProcessDataDTOByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, BULK_PROCESS_CODE, STATUS);
    verify(bulkProcessDataRepository).getBulkProcessDataDTOByStoreIdAndBulkProcessCodeAndStatus(STORE_ID, BULK_PROCESS_CODE, STATUS);

  }

  @Test
  public void findBulkProcessDataByIdTest() {
    bulkProcessDataServiceBean.findBulkProcessDataById(STORE_ID);
    verify(bulkProcessDataRepository).findById(STORE_ID);
  }

  @Test
  public void updateStatusToFailByBulkProcessCodeAndStatusIn(){
    bulkProcessDataServiceBean.updateStatusToFailByBulkProcessCodeAndStatusIn(BULK_PROCESS_CODE,
      Collections.singletonList(STATUS));
    verify(bulkProcessDataRepository).updateStatusToFailByBulkProcessCodeAndStatusIn(BULK_PROCESS_CODE, Collections.singletonList(STATUS));

  }

  @Test
  public void findByStoreIdAndBulkProcessCodeAndStatusAndIdentifierTest() {
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcessData.setParentProduct(PRODUCT_SKU);
    Mockito.when(bulkProcessDataRepository.findByStoreIdAndBulkProcessCodeAndStatusAndIdentifierNotNull(STORE_ID,
        BULK_PROCESS_CODE, STATUS)).thenReturn(Collections.singletonList(bulkProcessData));
    bulkProcessDataServiceBean.findByStoreIdAndBulkProcessCodeAndStatusAndIdentifier(STORE_ID, BULK_PROCESS_CODE,
        STATUS);
    Mockito.verify(bulkProcessDataRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndIdentifierNotNull(STORE_ID, BULK_PROCESS_CODE, STATUS);
  }
}