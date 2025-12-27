package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoResponseDTO;
import com.gdn.mta.bulk.dto.inventory.WebInventoryResponseDTO;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.service.util.BeanUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.dto.common.ListRequestDTO;
import org.apache.commons.lang3.StringUtils;
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
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigStockRequest;
import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gdn.common.base.entity.GdnBaseEntity;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.DormantSellerProductStatus;
import com.gdn.mta.bulk.DormantSellerStatus;
import com.gdn.mta.bulk.SellerProcessType;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.DormantSellerProductUpdateRequest;
import com.gdn.mta.bulk.entity.DormantSellerEvent;
import com.gdn.mta.bulk.entity.DormantSellerProduct;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.DormantSellerItemDetail;
import com.gdn.mta.bulk.repository.DormantSellerEventRepository;
import com.gdn.mta.bulk.repository.DormantSellerProductRepository;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.response.B2BResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;

public class DormantSellerServiceBeanTest {

  private static final String BUSINESS_PARTNER_CODE = "RAM-70107";
  private static final String STORE_ID = "storeId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final int PAGE = 0;
  private static final int SIZE = 50;
  private static final int TOTAL_RECORDS = 52;
  private static final int TOTAL_RECORDS_SINGLE_PAGE = 2;
  private static final String ITEM_SKU = "itemSku";
  private static final String DORMANT_SELLER_EVENT_ID = "dormantSellerEventId";
  private static final String ITEM_SKU_1 = "itemSku1";
  private static final String PICKUP_POINT_CODE = "ppCode";
  private static final int ONE_COUNT = 1;
  private static final String PROCESS_TYPE = "processType";
  private static final String EMAIL_OBJECT = "obj";
  private static final String OBJECT = "object";

  private Pageable pageable = PageRequest.of(PAGE, SIZE);
  private Pageable oneSizePageRequest = PageRequest.of(PAGE, ONE_COUNT);
  private Pageable nextPageable = PageRequest.of(PAGE + 1, SIZE);
  private ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
  private ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
  private InProgressProductResponse inProgressProductResponse = new InProgressProductResponse();
  private SystemParameterConfig dbFetchSizeConfig = new SystemParameterConfig();
  private SystemParameterConfig dormantSellerDateConfig = new SystemParameterConfig();
  private SystemParameterConfig maxLimitConfig = new SystemParameterConfig();
  private SystemParameterConfig sellerFetchBatchSize = new SystemParameterConfig();
  private DormantSellerProduct dormantSellerProduct;
  private DormantSellerProduct dormantSellerProduct1;
  private DormantSellerEvent dormantSellerEvent;
  private DormantSellerItemDetail dormantSellerItemDetail;
  private DormantSellerProductUpdateRequest dormantSellerProductUpdateRequest;
  private DormantSellerProductUpdateRequest dormantSellerProductUpdateRequest1;
  private Pageable sellerEventPage = PageRequest.of(0, SIZE);
  private Page<ProductLevel3SummaryResponse> products;
  private SystemParameterConfig sellerStatusBatchSize = new SystemParameterConfig();
  private ItemPickupPointListingRequest itemPickupPointListingRequest =
    new ItemPickupPointListingRequest();
  private ItemPickupPointListingResponse itemPickupPointListingResponse =
    new ItemPickupPointListingResponse();
  private ItemPickupPointListingResponse itemPickupPointListingResponse1 =
    new ItemPickupPointListingResponse();
  private InventoryDetailInfoResponseDTO inventoryDetailInfoResponseDTO =
    new InventoryDetailInfoResponseDTO();
  private InventoryDetailInfoResponseDTO inventoryDetailInfoResponseDTO1 =
    new InventoryDetailInfoResponseDTO();

  @InjectMocks
  private DormantSellerServiceBean dormantSellerServiceBean;

  @Mock
  private DormantSellerEventRepository dormantSellerEventRepository;

  @Mock
  private DormantSellerProductRepository dormantSellerProductRepository;

  @Mock
  private PBPOutboundService pbpOutboundService;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private MailDeliveryService mailDeliveryService;

  @Captor
  private ArgumentCaptor<DormantSellerEvent> dormantSellerEventArgumentCaptor;

  @Captor
  private ArgumentCaptor<DormantSellerProduct> dormantSellerProductArgumentCaptor;

  @Captor
  private ArgumentCaptor<List> dormantSellerProductRepositoryArgumentCaptor =
      ArgumentCaptor.forClass(List.class);

  @Mock
  private ProductLevel3Repository level3Repository;

  @Mock
  private InventoryOutboundService inventoryOutboundService;

  @BeforeEach
  public void initialize() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(dormantSellerServiceBean, "dormantSellerRetryStatusList",
        List.of(DormantSellerStatus.FETCHED.name()));
    ReflectionTestUtils.setField(dormantSellerServiceBean, "dormantSellerNotifyStatusList",
        List.of(DormantSellerStatus.FETCHED.name()));
    ReflectionTestUtils.setField(dormantSellerServiceBean, "dormantSellerRetryPageSize", 10);
    ReflectionTestUtils.setField(dormantSellerServiceBean, "dormantSellerRetryThreshold", 3);
    ReflectionTestUtils.setField(dormantSellerServiceBean, "dormantSellerFailureNotificationMail", Constant.SYSTEM);
    itemSummaryRequest.setArchived(false);
    itemSummaryRequest.setMerchantCode(BUSINESS_PARTNER_CODE);
    dbFetchSizeConfig.setValue(String.valueOf(SIZE));
    maxLimitConfig.setValue(String.valueOf(TOTAL_RECORDS));
    dormantSellerDateConfig.setValue(String.valueOf(TOTAL_RECORDS));
    sellerFetchBatchSize.setValue(String.valueOf(SIZE));
    dormantSellerItemDetail =
        DormantSellerItemDetail.builder().itemStatus(DormantSellerProductStatus.ACTIVE.name()).itemSku(ITEM_SKU)
            .businessPartnerCode(BUSINESS_PARTNER_CODE).build();

    dormantSellerProduct =
        DormantSellerProduct.builder().itemSku(ITEM_SKU).productStatus(DormantSellerProductStatus.ACTIVE.name())
            .status(DormantSellerStatus.IN_PROGRESS.name()).businessPartnerCode(BUSINESS_PARTNER_CODE).build();
    dormantSellerProduct1 =
      DormantSellerProduct.builder().itemSku(ITEM_SKU_1).productStatus(DormantSellerProductStatus.ACTIVE.name())
        .status(DormantSellerStatus.PENDING.name()).businessPartnerCode(BUSINESS_PARTNER_CODE).build();
    dormantSellerEvent = DormantSellerEvent.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).build();
    dormantSellerEvent.setId(GdnBaseEntity.ID);

    dormantSellerProductUpdateRequest =
      DormantSellerProductUpdateRequest.builder().itemSku(ITEM_SKU).status(DormantSellerStatus.PENDING.name()).build();
    dormantSellerProductUpdateRequest1 =
      DormantSellerProductUpdateRequest.builder().itemSku(ITEM_SKU_1).status(DormantSellerStatus.COMPLETED.name()).build();

    products = new PageImpl<>(new ArrayList<>());
    sellerStatusBatchSize.setValue(String.valueOf(SIZE));
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE))
      .thenReturn(sellerStatusBatchSize);

    itemPickupPointListingRequest.setProductSku(ITEM_SKU);
    itemPickupPointListingRequest.setPickupPointCodes(Collections.emptySet());
    itemPickupPointListingRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse1.setItemSku(ITEM_SKU_1);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingResponse1.setItemSku(ITEM_SKU_1);

    inventoryDetailInfoResponseDTO.setWebItemSku(ITEM_SKU);
    WebInventoryResponseDTO webInventoryResponseDTO = new WebInventoryResponseDTO();
    webInventoryResponseDTO.setPickupPointCode(PICKUP_POINT_CODE);
    webInventoryResponseDTO.setAvailableStock(10);
    inventoryDetailInfoResponseDTO.setWebInventoryResponse(webInventoryResponseDTO);
    when(dormantSellerProductRepository.save(Mockito.any())).thenReturn(dormantSellerProduct);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(dormantSellerEventRepository);
    Mockito.verifyNoMoreInteractions(dormantSellerProductRepository);
    Mockito.verifyNoMoreInteractions(pbpOutboundService);
    Mockito.verifyNoMoreInteractions(xProductOutboundService);
    Mockito.verifyNoMoreInteractions(systemParameterConfigService);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(inventoryOutboundService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(applicationContext);
    Mockito.verifyNoMoreInteractions(mailDeliveryService);
  }

  @Test
  public void testProcessDormantSellerDeactivateTest() throws ApplicationException {
    dormantSellerServiceBean.processSellerDeactivate(BUSINESS_PARTNER_CODE, SellerProcessType.DORMANT.name());
    Mockito.verify(dormantSellerEventRepository).save(Mockito.any(DormantSellerEvent.class));
  }

  @Test
  public void testProcessDormantSellerDeactivateEmptyCodeTest() throws ApplicationException {
    Assertions.assertThrows(ApplicationException.class,
        () -> dormantSellerServiceBean.processSellerDeactivate(StringUtils.EMPTY,
            SellerProcessType.DORMANT.name()));
  }

  @Test
  public void processPendingDormantSellerEventTest() {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE)).thenReturn(sellerFetchBatchSize);
    Mockito.when(this.dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID,
      DormantSellerStatus.PENDING.name(), sellerEventPage)).thenReturn(
      new PageImpl<>(Arrays.asList(DormantSellerEvent.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).build()),
        sellerEventPage, TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(xProductOutboundService.getItemSummaryByFilter(REQUEST_ID, USERNAME, pageable, itemSummaryRequest))
      .thenReturn(new PageImpl<>(Collections.singletonList(itemSummaryResponse), pageable, TOTAL_RECORDS));
    Mockito.when(xProductOutboundService.getItemSummaryByFilter(REQUEST_ID, USERNAME, nextPageable, itemSummaryRequest))
      .thenReturn(new PageImpl<>(Collections.singletonList(itemSummaryResponse), nextPageable, TOTAL_RECORDS));
    Mockito.when(
        this.pbpOutboundService.fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
          Mockito.any(Pageable.class))).thenReturn(
        new PageImpl<>(Arrays.asList(inProgressProductResponse), PageRequest.of(PAGE, SIZE), TOTAL_RECORDS_SINGLE_PAGE))
      .thenReturn(new PageImpl<>(Collections.emptyList(), PageRequest.of(PAGE + 1, SIZE), TOTAL_RECORDS_SINGLE_PAGE));
    dormantSellerServiceBean.processPendingDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE);
    Mockito.verify(this.dormantSellerEventRepository)
      .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, DormantSellerStatus.PENDING.name(), sellerEventPage);
    Mockito.verify(this.xProductOutboundService, times(2))
      .getItemSummaryByFilter(eq(REQUEST_ID), eq(USERNAME), Mockito.any(Pageable.class), eq(itemSummaryRequest));
    Mockito.verify(dormantSellerProductRepository, times(2)).saveAll(Mockito.anyList());
    Mockito.verify(this.pbpOutboundService, times(2))
      .fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
        Mockito.any(Pageable.class));
    Mockito.verify(dormantSellerEventRepository).save(Mockito.any(DormantSellerEvent.class));
  }

  @Test
  public void processPendingDormantSellerEvent_exceptionTest() {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE)).thenReturn(sellerFetchBatchSize);
    Mockito.when(this.dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID,
      DormantSellerStatus.PENDING.name())).thenThrow(ApplicationRuntimeException.class);
    dormantSellerServiceBean.processPendingDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE);
    Mockito.verify(this.dormantSellerEventRepository)
      .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, DormantSellerStatus.PENDING.name(), sellerEventPage);
  }

  @Test
  public void processPendingDormantSellerEvent_singleItemsPageTest() {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE)).thenReturn(sellerFetchBatchSize);
    Mockito.when(this.dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID,
      DormantSellerStatus.PENDING.name(), sellerEventPage)).thenReturn(
      new PageImpl<>(Arrays.asList(DormantSellerEvent.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).build()),
        sellerEventPage, TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(xProductOutboundService.getItemSummaryByFilter(REQUEST_ID, USERNAME, pageable, itemSummaryRequest))
      .thenReturn(
        new PageImpl<>(Collections.singletonList(itemSummaryResponse), pageable, TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(
        this.pbpOutboundService.fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
          Mockito.any(Pageable.class))).thenReturn(
        new PageImpl<>(Arrays.asList(inProgressProductResponse), PageRequest.of(PAGE, SIZE), TOTAL_RECORDS_SINGLE_PAGE))
      .thenReturn(new PageImpl<>(Collections.emptyList(), PageRequest.of(PAGE + 1, SIZE), TOTAL_RECORDS_SINGLE_PAGE));
    dormantSellerServiceBean.processPendingDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE);
    Mockito.verify(this.dormantSellerEventRepository)
      .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, DormantSellerStatus.PENDING.name(), sellerEventPage);
    Mockito.verify(this.xProductOutboundService)
      .getItemSummaryByFilter(eq(REQUEST_ID), eq(USERNAME), Mockito.any(Pageable.class), eq(itemSummaryRequest));
    Mockito.verify(dormantSellerProductRepository, times(2)).saveAll(Mockito.anyList());
    Mockito.verify(this.pbpOutboundService, times(2))
      .fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
        Mockito.any(Pageable.class));
    Mockito.verify(dormantSellerEventRepository).save(Mockito.any(DormantSellerEvent.class));
  }

  @Test
  public void updateViewConfigForItemsOfDormantSellerTest() {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_FETCH_BATCH_SIZE)).thenReturn(dbFetchSizeConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_UPDATE_MAX_SIZE)).thenReturn(maxLimitConfig);
    Mockito.when(this.dormantSellerProductRepository.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
        DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), pageable))
      .thenReturn(new PageImpl<>(Collections.singletonList(dormantSellerProduct), pageable, 1));
    Mockito.when(this.dormantSellerProductRepository.saveAll(Arrays.asList(dormantSellerProduct)))
      .thenReturn(Arrays.asList(dormantSellerProduct));
    this.dormantSellerServiceBean.updateViewConfigForItemsOfDormantSeller(STORE_ID, REQUEST_ID,
      USERNAME, SellerProcessType.DORMANT.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_UPDATE_MAX_SIZE);
    Mockito.verify(this.dormantSellerProductRepository).findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
      DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), pageable);
    Mockito.verify(this.kafkaProducer)
      .send(eq(kafkaTopicProperties.getDormantSellerItemSkuViewConfigUpdate()),
        Mockito.any());
    Mockito.verify(this.dormantSellerProductRepository,
      times(2)).saveAll(Arrays.asList(dormantSellerProduct));
    Mockito.verify(kafkaTopicProperties, times(2)).getDormantSellerItemSkuViewConfigUpdate();
  }

  @Test
  public void updateViewConfigForItemsOfSuspendSellerTest() {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.SUSPEND_SELLER_PRODUCT_FETCH_BATCH_SIZE)).thenReturn(dbFetchSizeConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.SUSPEND_SELLER_PRODUCT_UPDATE_MAX_SIZE)).thenReturn(maxLimitConfig);
    Mockito.when(this.dormantSellerProductRepository.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
        DormantSellerStatus.PENDING.name(), SellerProcessType.SUSPEND.name(), pageable))
      .thenReturn(new PageImpl<>(Collections.singletonList(dormantSellerProduct), pageable, 1));
    Mockito.when(this.dormantSellerProductRepository.saveAll(Arrays.asList(dormantSellerProduct)))
      .thenReturn(Arrays.asList(dormantSellerProduct));
    this.dormantSellerServiceBean.updateViewConfigForItemsOfDormantSeller(STORE_ID, REQUEST_ID,
      USERNAME, SellerProcessType.SUSPEND.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.SUSPEND_SELLER_PRODUCT_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.SUSPEND_SELLER_PRODUCT_UPDATE_MAX_SIZE);
    Mockito.verify(this.dormantSellerProductRepository).findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
      DormantSellerStatus.PENDING.name(), SellerProcessType.SUSPEND.name(), pageable);
    Mockito.verify(this.kafkaProducer)
      .send(eq(kafkaTopicProperties.getDormantSellerItemSkuViewConfigUpdate()),
        Mockito.any());
    Mockito.verify(this.dormantSellerProductRepository,
      times(2)).saveAll(Arrays.asList(dormantSellerProduct));
    Mockito.verify(kafkaTopicProperties, times(2)).getDormantSellerItemSkuViewConfigUpdate();
  }

  @Test
  public void updateViewConfigForItemsOfTerminatedSellerTest() {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.TERMINATED_SELLER_PRODUCT_FETCH_BATCH_SIZE)).thenReturn(dbFetchSizeConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.TERMINATED_SELLER_PRODUCT_UPDATE_MAX_SIZE)).thenReturn(maxLimitConfig);
    Mockito.when(this.dormantSellerProductRepository.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
        DormantSellerStatus.PENDING.name(), SellerProcessType.TERMINATED.name(), pageable))
      .thenReturn(new PageImpl<>(Collections.singletonList(dormantSellerProduct), pageable, 1));
    Mockito.when(this.dormantSellerProductRepository.saveAll(Arrays.asList(dormantSellerProduct)))
      .thenReturn(Arrays.asList(dormantSellerProduct));
    this.dormantSellerServiceBean.updateViewConfigForItemsOfDormantSeller(STORE_ID, REQUEST_ID,
      USERNAME, SellerProcessType.TERMINATED.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.TERMINATED_SELLER_PRODUCT_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.TERMINATED_SELLER_PRODUCT_UPDATE_MAX_SIZE);
    Mockito.verify(this.dormantSellerProductRepository).findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
      DormantSellerStatus.PENDING.name(), SellerProcessType.TERMINATED.name(), pageable);
    Mockito.verify(this.kafkaProducer)
      .send(eq(kafkaTopicProperties.getDormantSellerItemSkuViewConfigUpdate()),
        Mockito.any());
    Mockito.verify(this.dormantSellerProductRepository,
      times(2)).saveAll(Arrays.asList(dormantSellerProduct));
    Mockito.verify(kafkaTopicProperties, times(2)).getDormantSellerItemSkuViewConfigUpdate();
  }

  @Test
  public void updateViewConfigForItemsOfInvalidTypeSellerTest() {
    this.dormantSellerServiceBean.updateViewConfigForItemsOfDormantSeller(STORE_ID, REQUEST_ID,
      USERNAME, USERNAME);
  }

  @Test
  public void updateViewConfigForItemsOfDormantSeller_multiplePageTest() {
    pageable = PageRequest.of(0, ONE_COUNT);
    dbFetchSizeConfig.setValue(String.valueOf(ONE_COUNT));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_FETCH_BATCH_SIZE)).thenReturn(dbFetchSizeConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_UPDATE_MAX_SIZE)).thenReturn(maxLimitConfig);
    Mockito.when(this.dormantSellerProductRepository.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
        DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), pageable))
      .thenReturn(new PageImpl<>(Arrays.asList(dormantSellerProduct), pageable, 2))
      .thenReturn(new PageImpl<>(Arrays.asList(dormantSellerProduct), pageable, 1));
    Mockito.when(this.dormantSellerProductRepository.saveAll(Arrays.asList(dormantSellerProduct)))
      .thenReturn(Arrays.asList(dormantSellerProduct));
    this.dormantSellerServiceBean.updateViewConfigForItemsOfDormantSeller(STORE_ID, REQUEST_ID,
      USERNAME, SellerProcessType.DORMANT.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_UPDATE_MAX_SIZE);
    Mockito.verify(this.dormantSellerProductRepository, times(2))
      .findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
        DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), pageable);
    Mockito.verify(this.kafkaProducer, times(2))
      .send(eq(kafkaTopicProperties.getDormantSellerItemSkuViewConfigUpdate()),
        Mockito.any());
    Mockito.verify(this.dormantSellerProductRepository, times(4)).saveAll(Mockito.anyList());
    Mockito.verify(kafkaTopicProperties, times(3)).getDormantSellerItemSkuViewConfigUpdate();
  }

  @Test
  public void updateViewConfigForItemsOfDormantSeller_maxLimitReachedTest() {
    maxLimitConfig.setValue(String.valueOf(1));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_FETCH_BATCH_SIZE)).thenReturn(dbFetchSizeConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_UPDATE_MAX_SIZE)).thenReturn(maxLimitConfig);
    Mockito.when(this.dormantSellerProductRepository.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
        DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), oneSizePageRequest))
      .thenReturn(new PageImpl<>(Arrays.asList(dormantSellerProduct), oneSizePageRequest, 2));
    Mockito.when(this.dormantSellerProductRepository.saveAll(Arrays.asList(dormantSellerProduct)))
      .thenReturn(Arrays.asList(dormantSellerProduct));
    this.dormantSellerServiceBean.updateViewConfigForItemsOfDormantSeller(STORE_ID, REQUEST_ID,
      USERNAME, SellerProcessType.DORMANT.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_UPDATE_MAX_SIZE);
    Mockito.verify(this.dormantSellerProductRepository)
      .findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
        DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), oneSizePageRequest);
    Mockito.verify(this.dormantSellerProductRepository,
      times(2)).saveAll(Arrays.asList(dormantSellerProduct));
    Mockito.verify(this.kafkaProducer)
      .send(eq(kafkaTopicProperties.getDormantSellerItemSkuViewConfigUpdate()), Mockito.any());
    Mockito.verify(this.dormantSellerProductRepository, times(2)).saveAll(Mockito.anyList());
    Mockito.verify(kafkaTopicProperties, times(2)).getDormantSellerItemSkuViewConfigUpdate();
  }

  @Test
  public void updateViewConfigForItemsOfDormantSeller_exceptionTest() {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_FETCH_BATCH_SIZE)).thenThrow(ApplicationRuntimeException.class);
    this.dormantSellerServiceBean.updateViewConfigForItemsOfDormantSeller(STORE_ID, REQUEST_ID,
      USERNAME, SellerProcessType.DORMANT.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_FETCH_BATCH_SIZE);
  }

  @Test
  public void updateProductItemViewConfigItemSkuEmptyTest() {
    DormantSellerItemDetail dormantSellerItemDetail = DormantSellerItemDetail.builder().build();
    Assertions.assertThrows(ApplicationException.class,
        () -> dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail));
  }

  @Test
  public void updateProductItemViewConfigStatusEmptyTest() {
    DormantSellerItemDetail dormantSellerItemDetail = DormantSellerItemDetail.builder().itemSku(ITEM_SKU).build();
    Assertions.assertThrows(ApplicationException.class,
        () -> dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail));
  }

  @Test
  public void updateProductItemViewConfigTest() throws Exception {
    DormantSellerItemDetail dormantSellerItemDetail =
        DormantSellerItemDetail.builder().itemSku(ITEM_SKU).itemStatus(DormantSellerProductStatus.ACTIVE.name())
            .processType(SellerProcessType.DORMANT.name()).dormantSellerEventId(DORMANT_SELLER_EVENT_ID).build();
    when(dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
        dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
        dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(
        dormantSellerProduct);
    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);
    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(dormantSellerItemDetail.getItemSku(),
            DormantSellerStatus.IN_PROGRESS.name(), dormantSellerItemDetail.getProcessType(),
            dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(xProductOutboundService)
        .updateItemPickupPointViewConfigWithProductStatus(anyString(), any(ItemPickupPointViewConfigBaseRequest.class));
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
  }

  @Test
  public void updateProductItemViewConfigXproductExceptionTest() throws Exception {
    DormantSellerItemDetail dormantSellerItemDetail =
        DormantSellerItemDetail.builder().itemSku(ITEM_SKU).itemStatus(DormantSellerProductStatus.ACTIVE.name())
          .processType(SellerProcessType.DORMANT.name()).build();
    Mockito.doThrow(new ApplicationException()).when(xProductOutboundService)
        .updateItemPickupPointViewConfigWithProductStatus(anyString(), any(ItemPickupPointViewConfigBaseRequest.class));
    when(dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
        dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
        dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(
        dormantSellerProduct);
    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);
    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(dormantSellerItemDetail.getItemSku(),
            DormantSellerStatus.IN_PROGRESS.name(), dormantSellerItemDetail.getProcessType(),
            dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(xProductOutboundService)
        .updateItemPickupPointViewConfigWithProductStatus(anyString(), any(ItemPickupPointViewConfigBaseRequest.class));
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
    Assertions.assertEquals(DormantSellerStatus.FAILED.name(), dormantSellerProduct.getStatus());
  }

  @Test
  public void updateProductItemViewConfigInProgressTest() throws Exception {
    DormantSellerItemDetail dormantSellerItemDetail =
        DormantSellerItemDetail.builder().itemSku(ITEM_SKU).processType(SellerProcessType.TERMINATED.toString())
            .itemStatus(DormantSellerProductStatus.IN_PROGRESS.name()).dormantSellerEventId(DORMANT_SELLER_EVENT_ID).build();
    when(dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
        dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
        dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(
        dormantSellerProduct);

    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);

    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(dormantSellerItemDetail.getItemSku(),
            DormantSellerStatus.IN_PROGRESS.name(), dormantSellerItemDetail.getProcessType(),
            dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
    Mockito.verify(pbpOutboundService).updateProductItemViewConfig(anyString(), any());
    Mockito.verify(pbpOutboundService).deleteTerminatedSellerProducts(anyString());
  }

  @Test
  public void updateProductItemViewConfigInProgressNewFlowTest() throws Exception {
    ReflectionTestUtils.setField(dormantSellerServiceBean, "terminationSellerNewFlow", true);
    DormantSellerItemDetail dormantSellerItemDetail =
        DormantSellerItemDetail.builder().itemSku(ITEM_SKU).processType(SellerProcessType.TERMINATED.toString())
            .itemStatus(DormantSellerProductStatus.IN_PROGRESS.name()).dormantSellerEventId(DORMANT_SELLER_EVENT_ID)
            .build();
    when(
        dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
            dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
            dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(),
            dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(dormantSellerProduct);

    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);

    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
            dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
            dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(),
            dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
    Mockito.verify(pbpOutboundService).deleteTerminatedSellerProducts(anyString());
  }

  @Test
  public void updateProductItemViewConfigInProgressNotTerminatedTest() throws Exception {
    DormantSellerItemDetail dormantSellerItemDetail =
        DormantSellerItemDetail.builder().itemSku(ITEM_SKU).processType(SellerProcessType.DORMANT.toString())
            .itemStatus(DormantSellerProductStatus.IN_PROGRESS.name()).dormantSellerEventId(DORMANT_SELLER_EVENT_ID).build();
    when(dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
        dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
        dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(
        dormantSellerProduct);

    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);

    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(dormantSellerItemDetail.getItemSku(),
            DormantSellerStatus.IN_PROGRESS.name(), dormantSellerItemDetail.getProcessType(),
            dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
    Mockito.verify(pbpOutboundService).updateProductItemViewConfig(anyString(), any());
  }

  @Test
  public void updateProductItemViewConfigInProgressNotTerminatedSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(dormantSellerServiceBean, "terminationSellerNewFlow", true);
    DormantSellerItemDetail dormantSellerItemDetail =
        DormantSellerItemDetail.builder().itemSku(ITEM_SKU).processType(SellerProcessType.DORMANT.toString())
            .itemStatus(DormantSellerProductStatus.IN_PROGRESS.name()).dormantSellerEventId(DORMANT_SELLER_EVENT_ID)
            .build();
    when(
        dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
            dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
            dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(),
            dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(dormantSellerProduct);

    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);

    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
            dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
            dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(),
            dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
    Mockito.verify(pbpOutboundService).updateProductItemViewConfig(anyString(), any());
  }

  @Test
  public void updateProductItemViewConfigInProgressPBPExceptionTest() throws Exception {
    DormantSellerItemDetail dormantSellerItemDetail =
        DormantSellerItemDetail.builder().itemSku(ITEM_SKU).itemStatus(DormantSellerProductStatus.IN_PROGRESS.name())
            .processType(SellerProcessType.DORMANT.name()).dormantSellerEventId(DORMANT_SELLER_EVENT_ID).build();
    when(dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
        dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
        dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(
        dormantSellerProduct);
    ProductLevel3ViewConfigStockRequest productLevel3ViewConfigStockRequest = new ProductLevel3ViewConfigStockRequest();
    productLevel3ViewConfigStockRequest.setDisplay(false);
    productLevel3ViewConfigStockRequest.setBuyable(false);
    productLevel3ViewConfigStockRequest.setCncActive(false);

    Mockito.doThrow(new NullPointerException()).when(pbpOutboundService)
        .updateProductItemViewConfig(dormantSellerItemDetail.getItemSku(), productLevel3ViewConfigStockRequest);

    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);

    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(dormantSellerItemDetail.getItemSku(),
            DormantSellerStatus.IN_PROGRESS.name(), dormantSellerItemDetail.getProcessType(),
            dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
    Mockito.verify(pbpOutboundService).updateProductItemViewConfig(anyString(), any());
  }

  @Test
  public void updateDormantSellerStatusTest() {
    Mockito.when(
      dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
        eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable))).thenReturn(new PageImpl<>(
      Arrays.asList(
        DormantSellerEvent.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).build())));
    dormantSellerServiceBean.updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(dormantSellerEventRepository).findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
      eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable));
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(dormantSellerProductRepository).findByDormantSellerEventIdAndMarkForDeleteFalse(Mockito.isNull());
    Mockito.verify(dormantSellerEventRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void updateDormantSellerStatus_exceptionTest() {
    Mockito.when(this.systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE))
      .thenThrow(ApplicationRuntimeException.class);
    dormantSellerServiceBean.updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE);
  }

  @Test
  public void updateDormantSellerStatusCompleteTest() {
    dormantSellerProduct.setStatus(DormantSellerStatus.COMPLETED.name());
    Mockito.when(
        dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
          eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable)))
      .thenReturn(new PageImpl<>(Arrays.asList(dormantSellerEvent)));
    Mockito.when(
        dormantSellerProductRepository.findByDormantSellerEventIdAndMarkForDeleteFalse(anyString()))
      .thenReturn(Arrays.asList(dormantSellerProduct));
    dormantSellerServiceBean.updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(dormantSellerEventRepository)
      .findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
        eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable));
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(dormantSellerProductRepository)
      .findByDormantSellerEventIdAndMarkForDeleteFalse(anyString());
    Mockito.verify(dormantSellerEventRepository).saveAll(Mockito.anyList());
    Mockito.verify(dormantSellerProductRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void updateDormantSellerStatusFailedTest() {
    dormantSellerProduct.setStatus(DormantSellerStatus.FAILED.name());
    Mockito.when(
        dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
          eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable)))
      .thenReturn(new PageImpl<>(Arrays.asList(dormantSellerEvent)));
    Mockito.when(
        dormantSellerProductRepository.findByDormantSellerEventIdAndMarkForDeleteFalse(anyString()))
      .thenReturn(Arrays.asList(dormantSellerProduct));
    dormantSellerServiceBean.updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(dormantSellerEventRepository)
      .findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
        eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable));
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(dormantSellerProductRepository).findByDormantSellerEventIdAndMarkForDeleteFalse(anyString());
    Mockito.verify(dormantSellerEventRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void updateDormantSellerStatusPartialCompletedTest() {
    dormantSellerProduct.setStatus(DormantSellerStatus.FAILED.name());
    List<DormantSellerProduct> dormantSellerProductList = new ArrayList<>();
    dormantSellerProductList.add(dormantSellerProduct);
    dormantSellerProductList.add(
        DormantSellerProduct.builder().status(DormantSellerStatus.COMPLETED.name()).build());
    dormantSellerProductList.add(
        DormantSellerProduct.builder().status(DormantSellerStatus.COMPLETED.name()).build());
    Mockito.when(
        dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
          eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable)))
      .thenReturn(new PageImpl(Arrays.asList(dormantSellerEvent)));
    Mockito.when(dormantSellerProductRepository.findByDormantSellerEventIdAndMarkForDeleteFalse(anyString()))
        .thenReturn(dormantSellerProductList);
    dormantSellerServiceBean.updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(dormantSellerEventRepository)
      .findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
        eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable));
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(dormantSellerProductRepository).findByDormantSellerEventIdAndMarkForDeleteFalse(anyString());
    Mockito.verify(dormantSellerEventRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void updateDormantSellerStatusPartialCompletedTest_Not_Failed() {
    List<DormantSellerProduct> dormantSellerProductList = new ArrayList<>();
    dormantSellerProductList.add(
        DormantSellerProduct.builder().status(DormantSellerStatus.COMPLETED.name()).build());
    dormantSellerProductList.add(
        DormantSellerProduct.builder().status(DormantSellerStatus.COMPLETED.name()).build());
    Mockito.when(
        dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
          eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable)))
      .thenReturn(new PageImpl<>(Arrays.asList(dormantSellerEvent)));
    Mockito.when(dormantSellerProductRepository.findByDormantSellerEventIdAndMarkForDeleteFalse(anyString()))
        .thenReturn(dormantSellerProductList);
    dormantSellerServiceBean.updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(dormantSellerEventRepository)
      .findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
        eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable));
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(dormantSellerProductRepository).findByDormantSellerEventIdAndMarkForDeleteFalse(anyString());
    Mockito.verify(dormantSellerEventRepository).saveAll(Mockito.anyList());
    Mockito.verify(dormantSellerProductRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void updateDormantSellerStatusPendingTest() {
    List<DormantSellerProduct> dormantSellerProductList = new ArrayList<>();
    dormantSellerProductList.add(
      DormantSellerProduct.builder().status(DormantSellerStatus.COMPLETED.name()).build());
    dormantSellerProductList.add(
      DormantSellerProduct.builder().status(DormantSellerStatus.PENDING.name()).build());
    Mockito.when(
        dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
          eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable)))
      .thenReturn(new PageImpl<>(Arrays.asList(dormantSellerEvent)));
    Mockito.when(dormantSellerProductRepository.findByDormantSellerEventIdAndMarkForDeleteFalse(anyString()))
      .thenReturn(dormantSellerProductList);
    dormantSellerServiceBean.updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(dormantSellerEventRepository)
      .findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
        eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable));
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(dormantSellerProductRepository).findByDormantSellerEventIdAndMarkForDeleteFalse(anyString());
  }

  @Test
  public void updateDormantSellerStatusFetchedTest() {
    List<DormantSellerProduct> dormantSellerProductList = new ArrayList<>();
    dormantSellerProductList.add(
        DormantSellerProduct.builder().status(DormantSellerStatus.COMPLETED.name()).build());
    dormantSellerProductList.add(
        DormantSellerProduct.builder().status(DormantSellerStatus.FETCHED.name()).build());
    Mockito.when(
            dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
                eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable)))
        .thenReturn(new PageImpl<>(Arrays.asList(dormantSellerEvent)));
    Mockito.when(dormantSellerProductRepository.findByDormantSellerEventIdAndMarkForDeleteFalse(anyString()))
        .thenReturn(dormantSellerProductList);
    dormantSellerServiceBean.updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(dormantSellerEventRepository)
        .findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
            eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable));
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(dormantSellerProductRepository).findByDormantSellerEventIdAndMarkForDeleteFalse(anyString());
  }

  @Test
  public void updateDormantSellerStatusInProgressTest() {
    List<DormantSellerProduct> dormantSellerProductList = new ArrayList<>();
    dormantSellerProductList.add(
      DormantSellerProduct.builder().status(DormantSellerStatus.COMPLETED.name()).build());
    dormantSellerProductList.add(
      DormantSellerProduct.builder().status(DormantSellerStatus.IN_PROGRESS.name()).build());
    Mockito.when(
        dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
          eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable)))
      .thenReturn(new PageImpl<>(Arrays.asList(dormantSellerEvent)));
    Mockito.when(dormantSellerProductRepository.findByDormantSellerEventIdAndMarkForDeleteFalse(anyString()))
      .thenReturn(dormantSellerProductList);
    dormantSellerServiceBean.updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(dormantSellerEventRepository)
      .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, DormantSellerStatus.IN_PROGRESS.name(),
        pageable);
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(dormantSellerProductRepository).findByDormantSellerEventIdAndMarkForDeleteFalse(anyString());
  }

  @Test
  public void updateDormantSellerStatusPartiallyCompletedTest() {
    List<DormantSellerProduct> dormantSellerProductList = new ArrayList<>();
    dormantSellerProductList.add(
      DormantSellerProduct.builder().status(DormantSellerStatus.COMPLETED.name()).build());
    dormantSellerProductList.add(
      DormantSellerProduct.builder().status(DormantSellerStatus.FAILED.name()).build());
    Mockito.when(
        dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
          eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable)))
      .thenReturn(new PageImpl<>(Arrays.asList(dormantSellerEvent)));
    Mockito.when(dormantSellerProductRepository.findByDormantSellerEventIdAndMarkForDeleteFalse(anyString()))
      .thenReturn(dormantSellerProductList);
    dormantSellerServiceBean.updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(dormantSellerEventRepository)
      .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, DormantSellerStatus.IN_PROGRESS.name(),
        pageable);
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(dormantSellerProductRepository).findByDormantSellerEventIdAndMarkForDeleteFalse(anyString());
    Mockito.verify(dormantSellerEventRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void updateDormantSellerStatusNoEventTest() {
    Mockito.when(
        dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
          eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable)))
      .thenReturn(new PageImpl<>(Arrays.asList()));
    dormantSellerServiceBean.updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(dormantSellerEventRepository)
      .findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
        eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable));
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE);
  }

  @Test
  public void updateDormantSellerStatusCompleteNoActiveProductTest() {
    dormantSellerProduct.setStatus(DormantSellerStatus.COMPLETED.name());
    Mockito.when(
        dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
          eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable)))
      .thenReturn(new PageImpl<>(Arrays.asList(dormantSellerEvent)));
    Mockito.when(dormantSellerProductRepository.findByDormantSellerEventIdAndMarkForDeleteFalse(anyString()))
        .thenReturn(Arrays.asList());
    dormantSellerServiceBean.updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
    Mockito.verify(dormantSellerEventRepository)
      .findByStoreIdAndStatusAndMarkForDeleteFalse(eq(STORE_ID),
        eq(DormantSellerStatus.IN_PROGRESS.name()), eq(pageable));
    Mockito.verify(this.systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE);
    Mockito.verify(dormantSellerProductRepository).findByDormantSellerEventIdAndMarkForDeleteFalse(anyString());
    Mockito.verify(dormantSellerEventRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void deleteDormantSellerProductTest() {
    Mockito.when(dormantSellerProductRepository.findByUpdatedDateAndMarkForDeleteTrue(any(), any())).thenReturn(
        new PageImpl<>(Collections.singletonList(dormantSellerProduct), PageRequest.of(PAGE, SIZE), TOTAL_RECORDS));
    dormantSellerServiceBean.deleteDormantSellerProduct(STORE_ID, REQUEST_ID, USERNAME, 10, 100, 30);
    Mockito.verify(dormantSellerProductRepository, times(100)).findByUpdatedDateAndMarkForDeleteTrue(any(), any());
    Mockito.verify(dormantSellerProductRepository, times(100)).deleteAll(anyList());
  }

  @Test
  public void deleteDormantSellerProductNoProductsTest() {
    Mockito.when(dormantSellerProductRepository.findByUpdatedDateAndMarkForDeleteTrue(any(), any()))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(PAGE, SIZE), TOTAL_RECORDS));
    dormantSellerServiceBean.deleteDormantSellerProduct(STORE_ID, REQUEST_ID, USERNAME, 10, 100, 30);
    Mockito.verify(dormantSellerProductRepository).findByUpdatedDateAndMarkForDeleteTrue(any(), any());
  }

  @Test
  public void overrideDormantSellerProductStatus_emtpyTest() {
    dormantSellerServiceBean.overrideDormantSellerProductStatus(STORE_ID, REQUEST_ID, USERNAME, Collections.emptyList());
  }

  @Test
  public void overrideDormantSellerProductStatus() {
    Mockito.when(this.dormantSellerProductRepository.findByItemSkuIn(Arrays.asList(ITEM_SKU, ITEM_SKU_1)))
      .thenReturn(Arrays.asList(dormantSellerProduct, dormantSellerProduct1));
    dormantSellerServiceBean.overrideDormantSellerProductStatus(STORE_ID, REQUEST_ID, USERNAME,
      Arrays.asList(dormantSellerProductUpdateRequest, dormantSellerProductUpdateRequest1));
    Mockito.verify(this.dormantSellerProductRepository).findByItemSkuIn(Arrays.asList(ITEM_SKU, ITEM_SKU_1));
    Mockito.verify(this.dormantSellerProductRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void updateDormantSellerEventTest() {
    Mockito.when(dormantSellerEventRepository.findByStoreIdAndBusinessPartnerCodeIn(STORE_ID,
        Arrays.asList(BUSINESS_PARTNER_CODE))).thenReturn(Arrays.asList(dormantSellerEvent));
    dormantSellerServiceBean.updateDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME,
        DormantSellerStatus.IN_PROGRESS.name(), Arrays.asList(BUSINESS_PARTNER_CODE), false);
    Mockito.verify(dormantSellerEventRepository).findByStoreIdAndBusinessPartnerCodeIn(STORE_ID,
        Arrays.asList(BUSINESS_PARTNER_CODE));
    Mockito.verify(dormantSellerEventRepository).saveAll(anyList());
  }

  @Test
  public void updateDormantSellerEvent_Abort_Test() {
    ReflectionTestUtils.setField(dormantSellerServiceBean, "dormantSellerAbortStatuses",
        List.of(DormantSellerStatus.IN_PROGRESS.name(), DormantSellerStatus.PENDING.name()));
    ReflectionTestUtils.setField(dormantSellerServiceBean, "dormantSellerAbortProcesses",
        List.of(SellerProcessType.DORMANT.name()));
    DormantSellerProduct dormantSellerProduct = new DormantSellerProduct();
    dormantSellerProduct.setStatus(DormantSellerStatus.IN_PROGRESS.name());
    dormantSellerProduct.setMarkForDelete(false);
    Mockito.when(
            dormantSellerProductRepository.findByStoreIdAndStatusInAndProcessTypeInAndMarkForDeleteFalseAndUpdatedDateBefore(
                eq(STORE_ID),
                eq(List.of(DormantSellerStatus.IN_PROGRESS.name(),
                    DormantSellerStatus.PENDING.name())),
                eq(List.of(SellerProcessType.DORMANT.name())), Mockito.any()))
        .thenReturn(List.of(dormantSellerProduct));

    Mockito.when(dormantSellerEventRepository.findByStoreIdAndBusinessPartnerCodeIn(STORE_ID,
        Arrays.asList(BUSINESS_PARTNER_CODE))).thenReturn(Arrays.asList(dormantSellerEvent));
    dormantSellerServiceBean.updateDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME,
        DormantSellerStatus.IN_PROGRESS.name(), Arrays.asList(BUSINESS_PARTNER_CODE), true);
    Mockito.verify(dormantSellerProductRepository)
        .saveAll(dormantSellerProductRepositoryArgumentCaptor.capture());
    List<DormantSellerProduct> capturedProducts =
        dormantSellerProductRepositoryArgumentCaptor.getValue();
    Assertions.assertEquals(1, capturedProducts.size());
    DormantSellerProduct captured = capturedProducts.get(0);
    Assertions.assertEquals(DormantSellerStatus.FAILED.name(), captured.getStatus());
    Mockito.verify(dormantSellerProductRepository)
        .findByStoreIdAndStatusInAndProcessTypeInAndMarkForDeleteFalseAndUpdatedDateBefore(
            eq(STORE_ID),
            eq(List.of(DormantSellerStatus.IN_PROGRESS.name(), DormantSellerStatus.PENDING.name())),
            eq(List.of(SellerProcessType.DORMANT.name())), Mockito.any());
  }

  @Test
  public void processResignSellerEventTest_emptyBusinessPartner() {
    Assertions.assertThrows(ApplicationException.class,
        () -> dormantSellerServiceBean.processResignSellerEvent(STORE_ID, StringUtils.EMPTY));
  }

  @Test
  public void processResignSellerEventTest() throws Exception {
    dormantSellerServiceBean.processResignSellerEvent(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(this.dormantSellerEventRepository)
      .save(dormantSellerEventArgumentCaptor.capture());
    Assertions.assertEquals(SellerProcessType.SUSPEND.name(),
      dormantSellerEventArgumentCaptor.getValue().getProcessType());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
      dormantSellerEventArgumentCaptor.getValue().getBusinessPartnerCode());
  }

  @Test
  public void findByStoreIdAndStatusAndMarkForDeleteFalseTest() {
    dormantSellerServiceBean.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
      DormantSellerStatus.PENDING.name(), PROCESS_TYPE, TOTAL_RECORDS_SINGLE_PAGE);
    Mockito.verify(this.dormantSellerEventRepository)
      .findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
        DormantSellerStatus.PENDING.name(), PROCESS_TYPE,
          PageRequest.of(0, TOTAL_RECORDS_SINGLE_PAGE));
  }

  @Test
  public void saveCollectionInputTest() {
    dormantSellerServiceBean.saveCollectionInput(Arrays.asList(dormantSellerProduct));
    Mockito.verify(this.dormantSellerProductRepository).saveAll(Arrays.asList(dormantSellerProduct));
  }

  @Test
  public void upsertDormantSellerEventTest() {
    dormantSellerServiceBean.upsertDormantSellerEvent(dormantSellerEvent);
    Mockito.verify(this.dormantSellerEventRepository).save(dormantSellerEvent);
  }

  @Test
  public void updateProductItemViewConfig_suspendSellerTest() throws Exception {
    DormantSellerItemDetail dormantSellerItemDetail =
      DormantSellerItemDetail.builder().itemSku(ITEM_SKU)
        .itemStatus(DormantSellerProductStatus.ACTIVE.name())
        .processType(SellerProcessType.SUSPEND.name()).dormantSellerEventId(DORMANT_SELLER_EVENT_ID).build();
    when(dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
        dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
        dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(
        dormantSellerProduct);
    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);
    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(dormantSellerItemDetail.getItemSku(),
            DormantSellerStatus.IN_PROGRESS.name(), dormantSellerItemDetail.getProcessType(),
            dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(xProductOutboundService).archiveByProductSku(ITEM_SKU, true, true);
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
  }

  @Test
  public void updateProductItemViewConfig_suspendSellerExceptionTest() throws Exception {
    DormantSellerProduct dormantSellerProduct2 = new DormantSellerProduct();
    dormantSellerProduct2.setStatus(DormantSellerStatus.PROCESSING.name());
    BeanUtils.copyProperties(dormantSellerProduct, dormantSellerProduct2);
    when(dormantSellerProductRepository.save(Mockito.any())).thenReturn(dormantSellerProduct2)
        .thenReturn(dormantSellerProduct);
    DormantSellerItemDetail dormantSellerItemDetail =
      DormantSellerItemDetail.builder().itemSku(ITEM_SKU)
        .itemStatus(DormantSellerProductStatus.ACTIVE.name())
        .processType(SellerProcessType.SUSPEND.name()).dormantSellerEventId(DORMANT_SELLER_EVENT_ID).build();
    Mockito.doThrow(ApplicationRuntimeException.class).when(xProductOutboundService).archiveByProductSku(ITEM_SKU,
      true, true);
    when(dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
        dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
        dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(
        dormantSellerProduct);
    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);
    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(dormantSellerItemDetail.getItemSku(),
            DormantSellerStatus.IN_PROGRESS.name(), dormantSellerItemDetail.getProcessType(),
            dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(xProductOutboundService).archiveByProductSku(ITEM_SKU, true, true);
    Mockito.verify(dormantSellerProductRepository, times(2)).save(dormantSellerProductArgumentCaptor.capture());
    Assertions.assertEquals(DormantSellerStatus.PROCESSING.name(),
        dormantSellerProductArgumentCaptor.getAllValues().get(0).getStatus());
  }

  @Test
  public void updateProductItemViewConfig_suspendSeller_Duplicate_Event_Test() throws Exception {
    when(
        dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
            dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
            dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(),
            dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(null);
    Assertions.assertThrows(ApplicationException.class,
        () -> dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail));
    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
            dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
            dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(),
            dormantSellerItemDetail.getDormantSellerEventId());
  }

  @Test
  public void updateProductItemViewConfig_unknownTypeTest() throws Exception {
    DormantSellerItemDetail dormantSellerItemDetail =
      DormantSellerItemDetail.builder().itemSku(ITEM_SKU)
        .itemStatus(DormantSellerProductStatus.ACTIVE.name()).dormantSellerEventId(DORMANT_SELLER_EVENT_ID).build();
    when(dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
        dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
        dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(
        dormantSellerProduct);
    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);
    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(dormantSellerItemDetail.getItemSku(),
            DormantSellerStatus.IN_PROGRESS.name(), dormantSellerItemDetail.getProcessType(),
            dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(dormantSellerProductRepository, times(2)).save(dormantSellerProductArgumentCaptor.capture());
    Assertions.assertEquals(DormantSellerStatus.FAILED.name(),
      dormantSellerProductArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void updateProductItemViewConfigTerminatedMerchant() throws Exception {
    dormantSellerItemDetail.setProcessType(SellerProcessType.TERMINATED.name());
    dormantSellerItemDetail.setDormantSellerEventId(DORMANT_SELLER_EVENT_ID);
    when(dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
        dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
        dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(
        dormantSellerProduct);
    Mockito.when(
        this.xProductOutboundService.getItemPickupPointList(pageable, itemPickupPointListingRequest))
      .thenReturn(new PageImpl<>(
        Arrays.asList(itemPickupPointListingResponse, itemPickupPointListingResponse1), PageRequest.of(PAGE, SIZE), SIZE));
    Mockito.when(this.inventoryOutboundService.findDetailByWebMerchantCodeAndWebItemSku(
        Mockito.any(ListRequestDTO.class)))
      .thenReturn(Arrays.asList(inventoryDetailInfoResponseDTO, inventoryDetailInfoResponseDTO1));
    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);
    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(dormantSellerItemDetail.getItemSku(),
            DormantSellerStatus.IN_PROGRESS.name(), dormantSellerItemDetail.getProcessType(),
            dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(this.xProductOutboundService)
      .getItemPickupPointList(pageable, itemPickupPointListingRequest);
    Mockito.verify(this.inventoryOutboundService).findDetailByWebMerchantCodeAndWebItemSku(
        Mockito.any(ListRequestDTO.class));
    Mockito.verify(pbpOutboundService).listingUpdate(any(ProductLevel3QuickEditV2Request.class), anyString());
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
    Mockito.verify(pbpOutboundService).deleteTerminatedSellerProducts(anyString());
  }

  @Test
  public void updateProductItemViewConfigTerminatedNewFlowSharedProductMerchant() throws Exception {
    ReflectionTestUtils.setField(dormantSellerServiceBean, "terminationSellerNewFlow", true);
    dormantSellerItemDetail.setProcessType(SellerProcessType.TERMINATED.name());
    dormantSellerItemDetail.setDormantSellerEventId(DORMANT_SELLER_EVENT_ID);
    when(
        dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
            dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
            dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(),
            dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(dormantSellerProduct);
    Mockito.when(this.xProductOutboundService.getItemPickupPointList(pageable, itemPickupPointListingRequest))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPointListingResponse, itemPickupPointListingResponse1),
            PageRequest.of(PAGE, SIZE), SIZE));
    Mockito.when(
            this.inventoryOutboundService.findDetailByWebMerchantCodeAndWebItemSku(Mockito.any(ListRequestDTO.class)))
        .thenReturn(Arrays.asList(inventoryDetailInfoResponseDTO, inventoryDetailInfoResponseDTO1));
    Mockito.when(xProductOutboundService.isSharedProduct(Constant.STORE_ID,
        dormantSellerItemDetail.getItemSku(), dormantSellerItemDetail.getBusinessPartnerCode()))
      .thenReturn(true);
    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);
    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
            dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
            dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(),
            dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(this.xProductOutboundService).getItemPickupPointList(pageable, itemPickupPointListingRequest);
    Mockito.verify(this.inventoryOutboundService)
        .findDetailByWebMerchantCodeAndWebItemSku(Mockito.any(ListRequestDTO.class));
    Mockito.verify(pbpOutboundService).listingUpdate(any(ProductLevel3QuickEditV2Request.class), anyString());
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
    Mockito.verify(pbpOutboundService).deleteTerminatedSellerProducts(anyString());
    Mockito.verify(xProductOutboundService)
      .isSharedProduct(Constant.STORE_ID, dormantSellerItemDetail.getItemSku(),
        dormantSellerItemDetail.getBusinessPartnerCode());
  }

  @Test
  public void updateProductItemViewConfigTerminatedNewFlowNonSharedProductMerchant() throws Exception {
    ReflectionTestUtils.setField(dormantSellerServiceBean, "terminationSellerNewFlow", true);
    dormantSellerItemDetail.setProcessType(SellerProcessType.TERMINATED.name());
    dormantSellerItemDetail.setDormantSellerEventId(DORMANT_SELLER_EVENT_ID);
    Mockito.when(xProductOutboundService.isSharedProduct(Constant.STORE_ID,
        dormantSellerItemDetail.getItemSku(), dormantSellerItemDetail.getBusinessPartnerCode()))
      .thenReturn(false);
    when(
        dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
            dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
            dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(),
            dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(dormantSellerProduct);
    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);
    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
            dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
            dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(),
            dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
    Mockito.verify(pbpOutboundService).deleteTerminatedSellerProducts(anyString());
    Mockito.verify(xProductOutboundService)
      .isSharedProduct(Constant.STORE_ID, dormantSellerItemDetail.getItemSku(),
        dormantSellerItemDetail.getBusinessPartnerCode());
  }

  @Test
  public void updateProductItemViewConfigTerminatedNewFlowNonSharedProductMerchantError() throws Exception {
    ReflectionTestUtils.setField(dormantSellerServiceBean, "terminationSellerNewFlow", true);
    dormantSellerItemDetail.setProcessType(SellerProcessType.TERMINATED.name());
    dormantSellerItemDetail.setDormantSellerEventId(DORMANT_SELLER_EVENT_ID);
    Mockito.when(xProductOutboundService.isSharedProduct(Constant.STORE_ID,
        dormantSellerItemDetail.getItemSku(), dormantSellerItemDetail.getBusinessPartnerCode()))
      .thenReturn(false);
    when(
        dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
            dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
            dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(),
            dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(dormantSellerProduct);
    Mockito.doThrow(RuntimeException.class).when(pbpOutboundService).deleteTerminatedSellerProducts(anyString());
    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);
    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
            dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
            dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(),
            dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
    Mockito.verify(pbpOutboundService).deleteTerminatedSellerProducts(anyString());
    Mockito.verify(xProductOutboundService)
      .isSharedProduct(Constant.STORE_ID, dormantSellerItemDetail.getItemSku(),
        dormantSellerItemDetail.getBusinessPartnerCode());
  }

  @Test
  public void updateProductItemViewConfigTerminatedMerchantB2bFieldsNotEmpty() throws Exception {
    dormantSellerItemDetail.setProcessType(SellerProcessType.TERMINATED.name());
    when(dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
        dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
        dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(
        dormantSellerProduct);
    B2BResponse b2bFieldsResponse = new B2BResponse();
    b2bFieldsResponse.setManaged(false);
    b2bFieldsResponse.setBasePrice(Double.valueOf(22));
    b2bFieldsResponse.setManaged(false);
    itemPickupPointListingResponse1.setB2bFields(b2bFieldsResponse);
    Mockito.when(
            this.xProductOutboundService.getItemPickupPointList(pageable, itemPickupPointListingRequest))
        .thenReturn(new PageImpl<>(
            Arrays.asList(itemPickupPointListingResponse, itemPickupPointListingResponse1), PageRequest.of(PAGE, SIZE), SIZE));
    Mockito.when(this.inventoryOutboundService.findDetailByWebMerchantCodeAndWebItemSku(
            Mockito.any(ListRequestDTO.class)))
        .thenReturn(Arrays.asList(inventoryDetailInfoResponseDTO, inventoryDetailInfoResponseDTO1));
    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);
    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(dormantSellerItemDetail.getItemSku(),
            DormantSellerStatus.IN_PROGRESS.name(), dormantSellerItemDetail.getProcessType(),
            dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(this.xProductOutboundService)
        .getItemPickupPointList(pageable, itemPickupPointListingRequest);
    Mockito.verify(this.inventoryOutboundService).findDetailByWebMerchantCodeAndWebItemSku(
        Mockito.any(ListRequestDTO.class));
    Mockito.verify(pbpOutboundService).listingUpdate(any(ProductLevel3QuickEditV2Request.class), anyString());
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
    Mockito.verify(pbpOutboundService).deleteTerminatedSellerProducts(anyString());
  }

  @Test
  public void updateProductItemViewConfigTerminatedMerchantWithEmptyResponseTest() throws Exception {
    dormantSellerItemDetail.setProcessType(SellerProcessType.TERMINATED.name());
    dormantSellerItemDetail.setDormantSellerEventId(DORMANT_SELLER_EVENT_ID);

    when(dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
        dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
        dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(
        dormantSellerProduct);
    Mockito.when(
        this.xProductOutboundService.getItemPickupPointList(pageable, itemPickupPointListingRequest))
      .thenReturn(new PageImpl<>(Collections.EMPTY_LIST, PageRequest.of(PAGE, SIZE), SIZE));
    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);
    Mockito.verify(dormantSellerProductRepository)
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(dormantSellerItemDetail.getItemSku(),
            DormantSellerStatus.IN_PROGRESS.name(), dormantSellerItemDetail.getProcessType(),
            dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(this.xProductOutboundService)
      .getItemPickupPointList(pageable, itemPickupPointListingRequest);
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
  }

  @Test
  public void updateProductItemViewConfigTerminatedMerchantMultiplePageTest() throws Exception {
    dormantSellerItemDetail.setProcessType(SellerProcessType.TERMINATED.name());
    dormantSellerItemDetail.setDormantSellerEventId(DORMANT_SELLER_EVENT_ID);
    when(dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
      dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
      dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(
      dormantSellerProduct);
    Mockito.when(this.xProductOutboundService.getItemPickupPointList(Mockito.any(),
      eq(itemPickupPointListingRequest))).thenReturn(
      new PageImpl<>(Arrays.asList(itemPickupPointListingResponse, itemPickupPointListingResponse1), PageRequest.of(PAGE, SIZE), SIZE + SIZE)).thenReturn(
      new PageImpl<>(Arrays.asList(itemPickupPointListingResponse, itemPickupPointListingResponse1), PageRequest.of(PAGE + 1, SIZE), SIZE + SIZE));
    Mockito.when(this.inventoryOutboundService.findDetailByWebMerchantCodeAndWebItemSku(
        Mockito.any(ListRequestDTO.class)))
      .thenReturn(Arrays.asList(inventoryDetailInfoResponseDTO, inventoryDetailInfoResponseDTO1));
    dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);
    Mockito.verify(dormantSellerProductRepository)
      .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(dormantSellerItemDetail.getItemSku(),
        DormantSellerStatus.IN_PROGRESS.name(), dormantSellerItemDetail.getProcessType(),
        dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId());
    Mockito.verify(this.xProductOutboundService, times(2)).getItemPickupPointList(Mockito.any(),
        eq(itemPickupPointListingRequest));
    Mockito.verify(this.inventoryOutboundService, times(2)).findDetailByWebMerchantCodeAndWebItemSku(
      Mockito.any(ListRequestDTO.class));
    Mockito.verify(pbpOutboundService, times(2)).listingUpdate(any(ProductLevel3QuickEditV2Request.class), anyString());
    Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
    Mockito.verify(pbpOutboundService).deleteTerminatedSellerProducts(anyString());
  }

  @Test
  public void updateProductItemViewConfigTerminatedMerchant_exceptionTest() throws Exception {
    dormantSellerItemDetail.setProcessType(SellerProcessType.TERMINATED.name());
    dormantSellerItemDetail.setDormantSellerEventId(DORMANT_SELLER_EVENT_ID);
    when(dormantSellerProductRepository.findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
      dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
      dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId())).thenReturn(
      dormantSellerProduct);
    Mockito.when(
        this.xProductOutboundService.getItemPickupPointList(pageable, itemPickupPointListingRequest))
      .thenThrow(ApplicationRuntimeException.class);
    try {
      dormantSellerServiceBean.updateProductItemViewConfig(dormantSellerItemDetail);
    } finally {
      Mockito.verify(dormantSellerProductRepository).findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
        dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
        dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getDormantSellerEventId());
      Mockito.verify(this.xProductOutboundService)
        .getItemPickupPointList(pageable, itemPickupPointListingRequest);
      Mockito.verify(dormantSellerProductRepository, times(2)).save(any(DormantSellerProduct.class));
    }
  }

  @Test
  public void findByBusinessPartnerCodeAndProcessTypeTest() {
    Mockito.when(dormantSellerEventRepository.findFirstByBusinessPartnerCodeAndProcessType(BUSINESS_PARTNER_CODE, PROCESS_TYPE)).thenReturn(new DormantSellerEvent());
    dormantSellerServiceBean.findByBusinessPartnerCodeAndProcessType(BUSINESS_PARTNER_CODE, PROCESS_TYPE);
    Mockito.verify(dormantSellerEventRepository).findFirstByBusinessPartnerCodeAndProcessType(BUSINESS_PARTNER_CODE, PROCESS_TYPE);
  }

  @Test
  public void findByBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalseTest() {
    Mockito.when(dormantSellerEventRepository
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(anyString(), anyString(), anyString()))
        .thenReturn(new ArrayList<>());
    dormantSellerServiceBean
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(STORE_ID, BUSINESS_PARTNER_CODE,
            PROCESS_TYPE);
    Mockito.verify(dormantSellerEventRepository)
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(anyString(), anyString(), anyString());
  }

  @Test
  public void saveDormantSellerEventsTest() {
    Mockito.when(dormantSellerEventRepository.saveAll(anyList())).thenReturn(new ArrayList<>());
    dormantSellerServiceBean.saveDormantSellerEvents(new ArrayList<>());
    Mockito.verify(dormantSellerEventRepository).saveAll(anyList());
  }

  @Test
  public void retryDormantSellerTestNoRecordsFound() {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.DORMANT_SELLER_RETRY_THRESHOLD_TIME_IN_MINUTES)).thenReturn(dormantSellerDateConfig);
    Mockito.when(
        dormantSellerEventRepository.findByStoreIdAndStatesInAndUpdatedDateAndMarkForDeleteFalse(Mockito.eq(STORE_ID),
            Mockito.anyList(), Mockito.any(Date.class), Mockito.any(Pageable.class))).thenReturn(Page.empty());
    dormantSellerServiceBean.retryDormantSeller(STORE_ID);
    Mockito.verify(dormantSellerEventRepository)
        .findByStoreIdAndStatesInAndUpdatedDateAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.anyList(),
            Mockito.any(Date.class), Mockito.any(Pageable.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Mockito.eq(STORE_ID),
        Mockito.eq(SystemParameterConfigNames.DORMANT_SELLER_RETRY_THRESHOLD_TIME_IN_MINUTES));
  }

  @Test
  public void retryDormantSellerTestRecordsFound() {
    Mockito.when(applicationContext.getBean(DormantSellerServiceBean.class)).thenReturn(dormantSellerServiceBean);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.DORMANT_SELLER_RETRY_THRESHOLD_TIME_IN_MINUTES)).thenReturn(dormantSellerDateConfig);
    Page<DormantSellerEvent> page =
        new PageImpl<>(Collections.singletonList(dormantSellerEvent), PageRequest.of(0, 1), 2);
    Mockito.when(
            dormantSellerEventRepository.findByStoreIdAndStatesInAndUpdatedDateAndMarkForDeleteFalse(Mockito.eq(STORE_ID),
                Mockito.anyList(), Mockito.any(Date.class), Mockito.any(Pageable.class))).thenReturn(page)
        .thenReturn(Page.empty());
    dormantSellerServiceBean.retryDormantSeller(STORE_ID);
    Mockito.verify(dormantSellerEventRepository, times(2))
        .findByStoreIdAndStatesInAndUpdatedDateAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.anyList(),
            Mockito.any(Date.class), Mockito.any(Pageable.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Mockito.eq(STORE_ID),
        Mockito.eq(SystemParameterConfigNames.DORMANT_SELLER_RETRY_THRESHOLD_TIME_IN_MINUTES));
    Mockito.verify(applicationContext).getBean(DormantSellerServiceBean.class);
    Mockito.verify(dormantSellerEventRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void retryDormantSellerTestRecordsAboveThresholdFound() {
    Mockito.when(applicationContext.getBean(DormantSellerServiceBean.class)).thenReturn(dormantSellerServiceBean);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.DORMANT_SELLER_RETRY_THRESHOLD_TIME_IN_MINUTES)).thenReturn(dormantSellerDateConfig);
    dormantSellerEvent.setRetryCount(5);
    Page<DormantSellerEvent> page =
        new PageImpl<>(Collections.singletonList(dormantSellerEvent), PageRequest.of(0, 1), 2);
    Mockito.when(
            dormantSellerEventRepository.findByStoreIdAndStatesInAndUpdatedDateAndMarkForDeleteFalse(Mockito.eq(STORE_ID),
                Mockito.anyList(), Mockito.any(Date.class), Mockito.any(Pageable.class))).thenReturn(page)
        .thenReturn(Page.empty());
    dormantSellerServiceBean.retryDormantSeller(STORE_ID);
    Mockito.verify(dormantSellerEventRepository, times(2))
        .findByStoreIdAndStatesInAndUpdatedDateAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.anyList(),
            Mockito.any(Date.class), Mockito.any(Pageable.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Mockito.eq(STORE_ID),
        Mockito.eq(SystemParameterConfigNames.DORMANT_SELLER_RETRY_THRESHOLD_TIME_IN_MINUTES));
  }

  @Test
  public void notifyDormantSellerTestRecordsFound() {
    Mockito.when(applicationContext.getBean(DormantSellerServiceBean.class)).thenReturn(dormantSellerServiceBean);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.DORMANT_SELLER_NOTIFY_THRESHOLD_TIME_IN_MINUTES))
        .thenReturn(dormantSellerDateConfig);
    Page<DormantSellerEvent> page =
        new PageImpl<>(Collections.singletonList(dormantSellerEvent), PageRequest.of(0, 1), 2);
    Mockito.when(
            dormantSellerEventRepository.findByStoreIdAndStatesInAndUpdatedDateAfterAndMarkForDeleteFalse(Mockito.eq(STORE_ID),
                Mockito.anyList(), Mockito.any(Date.class), Mockito.any(Pageable.class))).thenReturn(page)
        .thenReturn(Page.empty());
    dormantSellerServiceBean.notifyStuckDormantProcess(STORE_ID);
    Mockito.verify(dormantSellerEventRepository, times(2))
        .findByStoreIdAndStatesInAndUpdatedDateAfterAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.anyList(),
            Mockito.any(Date.class), Mockito.any(Pageable.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Mockito.eq(STORE_ID),
        Mockito.eq(SystemParameterConfigNames.DORMANT_SELLER_NOTIFY_THRESHOLD_TIME_IN_MINUTES));
    Map<String, Object> mailObjectWrapper = new HashMap<>();
    Map<String, Object> emailObject = new HashMap<>();
    mailObjectWrapper.put(OBJECT, List.of(dormantSellerEvent));
    emailObject.put(EMAIL_OBJECT, mailObjectWrapper);
    Mockito.verify(mailDeliveryService).sendMail(emailObject, EmailConstants.DORMANT_SELLER_NOTIFICATION,
        EmailConstants.DORMANT_SELLER_STUCK_PROCESS, Constant.SYSTEM, Constant.SYSTEM);
  }

  @Test
  public void notifyDormantSellerTestNoRecordsFound() {
    Mockito.when(applicationContext.getBean(DormantSellerServiceBean.class)).thenReturn(dormantSellerServiceBean);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.DORMANT_SELLER_NOTIFY_THRESHOLD_TIME_IN_MINUTES))
        .thenReturn(dormantSellerDateConfig);
    Mockito.when(
        dormantSellerEventRepository.findByStoreIdAndStatesInAndUpdatedDateAfterAndMarkForDeleteFalse(Mockito.eq(STORE_ID),
            Mockito.anyList(), Mockito.any(Date.class), Mockito.any(Pageable.class))).thenReturn(Page.empty());
    dormantSellerServiceBean.notifyStuckDormantProcess(STORE_ID);
    Mockito.verify(dormantSellerEventRepository)
        .findByStoreIdAndStatesInAndUpdatedDateAfterAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.anyList(),
            Mockito.any(Date.class), Mockito.any(Pageable.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Mockito.eq(STORE_ID),
        Mockito.eq(SystemParameterConfigNames.DORMANT_SELLER_NOTIFY_THRESHOLD_TIME_IN_MINUTES));
  }

}