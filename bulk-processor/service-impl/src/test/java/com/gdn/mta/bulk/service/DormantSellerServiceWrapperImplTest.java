package com.gdn.mta.bulk.service;

import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.DormantSellerStatus;
import com.gdn.mta.bulk.SellerProcessType;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.entity.DormantSellerEvent;
import com.gdn.mta.bulk.entity.DormantSellerProduct;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
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
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;

public class DormantSellerServiceWrapperImplTest {

  private static final String STORE_ID = "storeId";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String PRODUCT_SKU = "productSku";
  private static final int PAGE = 0;
  private static final int SIZE = 50;
  private static final int TOTAL_RECORDS = 52;
  private static final long TOTAL_RECORDS_SINGLE_PAGE = 2;
  private static final int SELLER_FETCH_LIMIT = 1;
  private static final int ONE_COUNT = 1;

  private Pageable pageable = PageRequest.of(PAGE, SIZE);
  private Pageable oneSizePageRequest = PageRequest.of(PAGE, ONE_COUNT);
  private Pageable nextPageable = PageRequest.of(PAGE + 1, SIZE);
  private SystemParameterConfig sellerFetchBatchSize = new SystemParameterConfig();
  private Pageable sellerEventPage = PageRequest.of(0, 50);
  private ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
  private ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
  private InProgressProductResponse inProgressProductResponse = new InProgressProductResponse();
  private ProductL3SummaryResponse productL3SummaryResponse = new ProductL3SummaryResponse();
  private ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
  private DormantSellerEvent dormantSellerEvent;
  private ProfileResponse profileResponse;
  private List<DormantSellerEvent> dormantSellerEventList = new ArrayList<>();


  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private DormantSellerService dormantSellerService;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private PBPOutboundService pbpOutboundService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @InjectMocks
  private DormantSellerServiceWrapperImpl dormantSellerServiceWrapperImpl;

  @Captor
  private ArgumentCaptor<DormantSellerEvent> dormantSellerEventArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    sellerFetchBatchSize.setValue(String.valueOf(SELLER_FETCH_LIMIT));
    itemSummaryRequest.setArchived(false);
    itemSummaryRequest.setMerchantCode(BUSINESS_PARTNER_CODE);

    productL3SummaryResponse.setProductSku(PRODUCT_SKU);

    productSummaryRequest.setArchived(false);
    productSummaryRequest.setMerchantCode(BUSINESS_PARTNER_CODE);

    dormantSellerEvent =
      DormantSellerEvent.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).build();
    dormantSellerEventList.add(dormantSellerEvent);

    profileResponse = new ProfileResponse();
    profileResponse.setMerchantStatus(Constant.INACTIVE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(systemParameterConfigService);
    Mockito.verifyNoMoreInteractions(dormantSellerService);
    Mockito.verifyNoMoreInteractions(xProductOutboundService);
    Mockito.verifyNoMoreInteractions(pbpOutboundService);
  }

  @Test
  public void processPendingDormantSellerEventTest() throws Exception {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setDormantFlag(true);
    profileResponse.setCompany(companyDTO);
    dormantSellerEventList.forEach(
        dormantSellerEvent -> dormantSellerEvent.setStatus(DormantSellerStatus.FETCHED.name()));
    Mockito.when(dormantSellerService.saveDormantSellerEvents(Mockito.anyList()))
        .thenReturn(dormantSellerEventList);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE)).thenReturn(sellerFetchBatchSize);
    Mockito.when(
        this.dormantSellerService.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
          DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), SELLER_FETCH_LIMIT))
      .thenReturn(
        new PageImpl<>(dormantSellerEventList, sellerEventPage, TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(this.dormantSellerService.upsertDormantSellerEvent(dormantSellerEvent))
      .thenReturn(dormantSellerEvent);
    Mockito.when(
        xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, PAGE, SIZE,
            REQUEST_ID, USERNAME)).thenReturn(
        new PageImpl<>(Collections.singletonList(productL3SummaryResponse), pageable, TOTAL_RECORDS));
    Mockito.when(
        xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, PAGE + 1, SIZE,
            REQUEST_ID, USERNAME)).thenReturn(
        new PageImpl<>(Collections.singletonList(productL3SummaryResponse), nextPageable,
            TOTAL_RECORDS));
    Mockito.when(
        this.pbpOutboundService.fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
          Mockito.any(Pageable.class))).thenReturn(
        new PageImpl<>(Arrays.asList(inProgressProductResponse), PageRequest.of(PAGE, SIZE), TOTAL_RECORDS_SINGLE_PAGE))
      .thenReturn(new PageImpl<>(Collections.emptyList(), PageRequest.of(PAGE + 1, SIZE), TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    dormantSellerServiceWrapperImpl.processPendingDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME,
      SellerProcessType.DORMANT.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE);
    Mockito.verify(this.dormantSellerService)
      .findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
        DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), SELLER_FETCH_LIMIT);
    Mockito.verify(this.xProductOutboundService, times(2))
        .getProductL3SummaryResponse(eq(productSummaryRequest), Mockito.anyInt(), eq(SIZE),
            eq(REQUEST_ID), eq(USERNAME));
    Mockito.verify(this.pbpOutboundService)
      .fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
        Mockito.any(Pageable.class));
    Mockito.verify(this.dormantSellerService,
      times(2)).saveCollectionInput(Mockito.anyList());
    Mockito.verify(this.dormantSellerService, times(1))
      .upsertDormantSellerEvent(dormantSellerEvent);
    Mockito.verify(dormantSellerService).saveDormantSellerEvents(Mockito.anyList());
  }

  @Test
  public void processPendingDormantSellerEventEmptyTest() throws Exception {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setDormantFlag(true);
    profileResponse.setCompany(companyDTO);
    dormantSellerEventList.forEach(
        dormantSellerEvent -> dormantSellerEvent.setStatus(DormantSellerStatus.FETCHED.name()));
    Mockito.when(dormantSellerService.saveDormantSellerEvents(Mockito.anyList())).thenReturn(dormantSellerEventList);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE)).thenReturn(sellerFetchBatchSize);
    Mockito.when(this.dormantSellerService.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
            DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), SELLER_FETCH_LIMIT))
        .thenReturn(new PageImpl<>(dormantSellerEventList, sellerEventPage, TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(this.dormantSellerService.upsertDormantSellerEvent(dormantSellerEvent)).thenReturn(dormantSellerEvent);
    Mockito.when(
            xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, PAGE, SIZE, REQUEST_ID, USERNAME))
        .thenReturn(new PageImpl<>(Collections.emptyList(), pageable, 0));
    Mockito.when(this.pbpOutboundService.fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME),
            eq(BUSINESS_PARTNER_CODE), Mockito.any(Pageable.class)))
        .thenReturn(new PageImpl<>(Collections.emptyList(), PageRequest.of(PAGE, SIZE), 0));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    dormantSellerServiceWrapperImpl.processPendingDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME,
        SellerProcessType.DORMANT.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE);
    Mockito.verify(this.dormantSellerService)
        .findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID, DormantSellerStatus.PENDING.name(),
            SellerProcessType.DORMANT.name(), SELLER_FETCH_LIMIT);
    Mockito.verify(this.xProductOutboundService)
        .getProductL3SummaryResponse(eq(productSummaryRequest), Mockito.anyInt(), eq(SIZE), eq(REQUEST_ID),
            eq(USERNAME));
    Mockito.verify(this.pbpOutboundService)
        .fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
            Mockito.any(Pageable.class));
    Mockito.verify(this.dormantSellerService, times(1)).upsertDormantSellerEvent(dormantSellerEvent);
    Mockito.verify(dormantSellerService).saveDormantSellerEvents(Mockito.anyList());
  }

  @Test
  public void processPendingDormantSellerEventActiveTest() throws Exception {
    ReflectionTestUtils.setField(dormantSellerServiceWrapperImpl,
        "removeArchiveFilterInSellerTerminationFlow", true);
    profileResponse.setMerchantStatus("ACTIVE");
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setDormantFlag(true);
    profileResponse.setCompany(companyDTO);
    dormantSellerEventList.forEach(
        dormantSellerEvent -> dormantSellerEvent.setStatus(DormantSellerStatus.FETCHED.name()));
    Mockito.when(dormantSellerService.saveDormantSellerEvents(Mockito.anyList()))
        .thenReturn(dormantSellerEventList);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE)).thenReturn(sellerFetchBatchSize);
    Mockito.when(
            this.dormantSellerService.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
                DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), SELLER_FETCH_LIMIT))
        .thenReturn(
            new PageImpl<>(dormantSellerEventList, sellerEventPage, TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(this.dormantSellerService.upsertDormantSellerEvent(dormantSellerEvent))
        .thenReturn(dormantSellerEvent);
    Mockito.when(
        xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, PAGE, SIZE,
            REQUEST_ID, USERNAME)).thenReturn(
        new PageImpl<>(Collections.singletonList(productL3SummaryResponse), pageable, TOTAL_RECORDS));
    Mockito.when(
        xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, PAGE + 1, SIZE,
            REQUEST_ID, USERNAME)).thenReturn(
        new PageImpl<>(Collections.singletonList(productL3SummaryResponse), nextPageable,
            TOTAL_RECORDS));
    Mockito.when(
            this.pbpOutboundService.fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
                Mockito.any(Pageable.class))).thenReturn(
            new PageImpl<>(Arrays.asList(inProgressProductResponse), PageRequest.of(PAGE, SIZE), TOTAL_RECORDS_SINGLE_PAGE))
        .thenReturn(new PageImpl<>(Collections.emptyList(), PageRequest.of(PAGE + 1, SIZE), TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    dormantSellerServiceWrapperImpl.processPendingDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME,
        SellerProcessType.DORMANT.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE);
    Mockito.verify(this.dormantSellerService)
        .findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
            DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), SELLER_FETCH_LIMIT);
    Mockito.verify(this.xProductOutboundService, times(2))
        .getProductL3SummaryResponse(eq(productSummaryRequest), Mockito.anyInt(), eq(SIZE),
            eq(REQUEST_ID), eq(USERNAME));
    Mockito.verify(this.pbpOutboundService)
        .fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
            Mockito.any(Pageable.class));
    Mockito.verify(this.dormantSellerService,
        times(2)).saveCollectionInput(Mockito.anyList());
    Mockito.verify(this.dormantSellerService, times(1))
        .upsertDormantSellerEvent(dormantSellerEvent);
    Mockito.verify(dormantSellerService).saveDormantSellerEvents(Mockito.anyList());
  }

  @Test
  public void processPendingDormantSellerEvent_exceptionTest() {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE)).thenThrow(ApplicationRuntimeException.class);
    dormantSellerServiceWrapperImpl.processPendingDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME,
      SellerProcessType.DORMANT.name());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE);
  }

  @Test
  public void processPendingDormantSellerEvent_suspendSellerTest() throws Exception {
    ReflectionTestUtils.setField(dormantSellerServiceWrapperImpl,
        "removeArchiveFilterInSellerTerminationFlow", true);
    dormantSellerEventList.forEach(
        dormantSellerEvent -> dormantSellerEvent.setStatus(DormantSellerStatus.FETCHED.name()));
    Mockito.when(dormantSellerService.saveDormantSellerEvents(Mockito.anyList()))
        .thenReturn(dormantSellerEventList);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.SUSPEND_SELLER_EVENT_FETCH_BATCH_SIZE)).thenReturn(sellerFetchBatchSize);
    Mockito.when(
        this.dormantSellerService.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
          DormantSellerStatus.PENDING.name(), SellerProcessType.SUSPEND.name(), SELLER_FETCH_LIMIT))
      .thenReturn(
        new PageImpl<>(dormantSellerEventList, sellerEventPage, TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(
      xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, PAGE, SIZE,
        REQUEST_ID, USERNAME)).thenReturn(
      new PageImpl<>(Collections.singletonList(productL3SummaryResponse), pageable, TOTAL_RECORDS));
    Mockito.when(
      xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, PAGE + 1, SIZE,
        REQUEST_ID, USERNAME)).thenReturn(
      new PageImpl<>(Collections.singletonList(productL3SummaryResponse), nextPageable,
        TOTAL_RECORDS));
    Mockito.when(this.dormantSellerService.upsertDormantSellerEvent(dormantSellerEvent))
      .thenReturn(dormantSellerEvent);
    Mockito.when(
        this.pbpOutboundService.fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
          Mockito.any(Pageable.class))).thenReturn(
        new PageImpl<>(Arrays.asList(inProgressProductResponse), PageRequest.of(PAGE, SIZE), TOTAL_RECORDS_SINGLE_PAGE))
      .thenReturn(new PageImpl<>(Collections.emptyList(), PageRequest.of(PAGE + 1, SIZE), TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    dormantSellerServiceWrapperImpl.processPendingDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME,
      SellerProcessType.SUSPEND.name());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.SUSPEND_SELLER_EVENT_FETCH_BATCH_SIZE);
    Mockito.verify(this.dormantSellerService)
      .findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
        DormantSellerStatus.PENDING.name(), SellerProcessType.SUSPEND.name(), SELLER_FETCH_LIMIT);
    Mockito.verify(this.xProductOutboundService, times(2))
      .getProductL3SummaryResponse(eq(productSummaryRequest), Mockito.anyInt(), eq(SIZE),
        eq(REQUEST_ID), eq(USERNAME));
    Mockito.verify(this.pbpOutboundService)
      .fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
        Mockito.any(Pageable.class));
    Mockito.verify(this.dormantSellerService,
      times(2)).saveCollectionInput(Mockito.anyList());
    Mockito.verify(this.dormantSellerService,
      times(1)).upsertDormantSellerEvent(dormantSellerEventArgumentCaptor.capture());
    Mockito.verify(dormantSellerService).saveDormantSellerEvents(Mockito.anyList());
  }


  @Test
  public void processPendingDormantSellerEvent_suspendSellerActiveDormantSellerTest() throws Exception {
    dormantSellerEventList.forEach(
        dormantSellerEvent -> dormantSellerEvent.setStatus(DormantSellerStatus.FETCHED.name()));
    Mockito.when(dormantSellerService.saveDormantSellerEvents(Mockito.anyList()))
        .thenReturn(dormantSellerEventList);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.SUSPEND_SELLER_EVENT_FETCH_BATCH_SIZE)).thenReturn(sellerFetchBatchSize);
    Mockito.when(
            this.dormantSellerService.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
                DormantSellerStatus.PENDING.name(), SellerProcessType.SUSPEND.name(), SELLER_FETCH_LIMIT))
        .thenReturn(
            new PageImpl<>(dormantSellerEventList, sellerEventPage, TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(
        xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, PAGE, SIZE,
            REQUEST_ID, USERNAME)).thenReturn(
        new PageImpl<>(Collections.singletonList(productL3SummaryResponse), pageable, TOTAL_RECORDS));
    Mockito.when(
        xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, PAGE + 1, SIZE,
            REQUEST_ID, USERNAME)).thenReturn(
        new PageImpl<>(Collections.singletonList(productL3SummaryResponse), nextPageable,
            TOTAL_RECORDS));
    Mockito.when(this.dormantSellerService.upsertDormantSellerEvent(dormantSellerEvent))
        .thenReturn(dormantSellerEvent);
    Mockito.when(
            this.pbpOutboundService.fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
                Mockito.any(Pageable.class))).thenReturn(
            new PageImpl<>(Arrays.asList(inProgressProductResponse), PageRequest.of(PAGE, SIZE), TOTAL_RECORDS_SINGLE_PAGE))
        .thenReturn(new PageImpl<>(Collections.emptyList(), PageRequest.of(PAGE + 1, SIZE), TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    dormantSellerServiceWrapperImpl.processPendingDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME,
        SellerProcessType.SUSPEND.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.SUSPEND_SELLER_EVENT_FETCH_BATCH_SIZE);
    Mockito.verify(this.dormantSellerService)
        .findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
            DormantSellerStatus.PENDING.name(), SellerProcessType.SUSPEND.name(), SELLER_FETCH_LIMIT);
    Mockito.verify(this.xProductOutboundService, times(2))
        .getProductL3SummaryResponse(eq(productSummaryRequest), Mockito.anyInt(), eq(SIZE),
            eq(REQUEST_ID), eq(USERNAME));
    Mockito.verify(this.pbpOutboundService)
        .fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
            Mockito.any(Pageable.class));
    Mockito.verify(this.dormantSellerService,
        times(2)).saveCollectionInput(Mockito.anyList());
    Mockito.verify(this.dormantSellerService,
        times(1)).upsertDormantSellerEvent(dormantSellerEventArgumentCaptor.capture());
    Mockito.verify(dormantSellerService).saveDormantSellerEvents(Mockito.anyList());
  }

  @Test
  public void processPendingSellerEventTerminatedMerchantTest() throws Exception {
    ReflectionTestUtils.setField(dormantSellerServiceWrapperImpl,
        "removeArchiveFilterInSellerTerminationFlow", true);
    productSummaryRequest.setArchived(null);
    dormantSellerEventList.forEach(
        dormantSellerEvent -> dormantSellerEvent.setStatus(DormantSellerStatus.FETCHED.name()));
    Mockito.when(dormantSellerService.saveDormantSellerEvents(Mockito.anyList()))
        .thenReturn(dormantSellerEventList);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TERMINATED_SELLER_EVENT_FETCH_BATCH_SIZE)).thenReturn(sellerFetchBatchSize);
    Mockito.when(
      this.dormantSellerService.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
        DormantSellerStatus.PENDING.name(), SellerProcessType.TERMINATED.name(),
        SELLER_FETCH_LIMIT)).thenReturn(
      new PageImpl<>(dormantSellerEventList, sellerEventPage, TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(this.dormantSellerService.upsertDormantSellerEvent(dormantSellerEvent))
      .thenReturn(dormantSellerEvent);
    Mockito.when(
        xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, PAGE, SIZE,
            REQUEST_ID, USERNAME)).thenReturn(
        new PageImpl<>(Collections.singletonList(productL3SummaryResponse), pageable, TOTAL_RECORDS));
    Mockito.when(
        xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, PAGE + 1, SIZE,
            REQUEST_ID, USERNAME)).thenReturn(
        new PageImpl<>(Collections.singletonList(productL3SummaryResponse), nextPageable,
            TOTAL_RECORDS));
    Mockito.when(
            this.pbpOutboundService.fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
                Mockito.any(Pageable.class))).thenReturn(
            new PageImpl<>(Arrays.asList(inProgressProductResponse), PageRequest.of(PAGE, SIZE), TOTAL_RECORDS_SINGLE_PAGE))
        .thenReturn(new PageImpl<>(Collections.emptyList(), PageRequest.of(PAGE + 1, SIZE), TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    dormantSellerServiceWrapperImpl.processPendingDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME,
        SellerProcessType.TERMINATED.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TERMINATED_SELLER_EVENT_FETCH_BATCH_SIZE);
    Mockito.verify(this.dormantSellerService)
        .findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
            DormantSellerStatus.PENDING.name(), SellerProcessType.TERMINATED.name(), SELLER_FETCH_LIMIT);
    Mockito.verify(this.xProductOutboundService, times(2))
        .getProductL3SummaryResponse(eq(productSummaryRequest), Mockito.anyInt(), eq(SIZE),
            eq(REQUEST_ID), eq(USERNAME));
    Mockito.verify(this.pbpOutboundService)
        .fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
            Mockito.any(Pageable.class));
    Mockito.verify(this.dormantSellerService,
        times(2)).saveCollectionInput(Mockito.anyList());
    Mockito.verify(this.dormantSellerService,
      times(1)).upsertDormantSellerEvent(dormantSellerEventArgumentCaptor.capture());
    Mockito.verify(dormantSellerService).saveDormantSellerEvents(Mockito.anyList());
  }

  @Test
  public void processPendingSellerEventTerminatedMerchantActiveTest() throws Exception {
    profileResponse.setMerchantStatus("ACTIVE");
    dormantSellerEventList.forEach(
        dormantSellerEvent -> dormantSellerEvent.setStatus(DormantSellerStatus.FETCHED.name()));
    Mockito.when(dormantSellerService.saveDormantSellerEvents(Mockito.anyList()))
        .thenReturn(dormantSellerEventList);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TERMINATED_SELLER_EVENT_FETCH_BATCH_SIZE)).thenReturn(sellerFetchBatchSize);
    Mockito.when(
        this.dormantSellerService.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
            DormantSellerStatus.PENDING.name(), SellerProcessType.TERMINATED.name(),
            SELLER_FETCH_LIMIT)).thenReturn(
        new PageImpl<>(dormantSellerEventList, sellerEventPage, TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(this.dormantSellerService.upsertDormantSellerEvent(dormantSellerEvent))
        .thenReturn(dormantSellerEvent);
    Mockito.when(
        xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, PAGE, SIZE,
            REQUEST_ID, USERNAME)).thenReturn(
        new PageImpl<>(Collections.singletonList(productL3SummaryResponse), pageable, TOTAL_RECORDS));
    Mockito.when(
        xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, PAGE + 1, SIZE,
            REQUEST_ID, USERNAME)).thenReturn(
        new PageImpl<>(Collections.singletonList(productL3SummaryResponse), nextPageable,
            TOTAL_RECORDS));
    Mockito.when(
            this.pbpOutboundService.fetchInProgressProductsByMerchantCode(eq(REQUEST_ID), eq(USERNAME), eq(BUSINESS_PARTNER_CODE),
                Mockito.any(Pageable.class))).thenReturn(
            new PageImpl<>(Arrays.asList(inProgressProductResponse), PageRequest.of(PAGE, SIZE), TOTAL_RECORDS_SINGLE_PAGE))
        .thenReturn(new PageImpl<>(Collections.emptyList(), PageRequest.of(PAGE + 1, SIZE), TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    dormantSellerServiceWrapperImpl.processPendingDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME,
        SellerProcessType.TERMINATED.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TERMINATED_SELLER_EVENT_FETCH_BATCH_SIZE);
    Mockito.verify(this.dormantSellerService)
        .findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
            DormantSellerStatus.PENDING.name(), SellerProcessType.TERMINATED.name(), SELLER_FETCH_LIMIT);
    Mockito.verify(this.dormantSellerService).upsertDormantSellerEvent(dormantSellerEventArgumentCaptor.capture());
    Assertions.assertEquals(DormantSellerStatus.SKIPPED.name(),
        dormantSellerEventArgumentCaptor.getValue().getStatus());
    Mockito.verify(dormantSellerService).saveDormantSellerEvents(Mockito.anyList());
  }



  @Test
  public void processPendingDormantSellerEventActiveSellerTest() throws Exception {
    profileResponse.setMerchantStatus(Constant.ACTIVE);
    dormantSellerEventList.forEach(
        dormantSellerEvent -> dormantSellerEvent.setStatus(DormantSellerStatus.FETCHED.name()));
    Mockito.when(dormantSellerService.saveDormantSellerEvents(Mockito.anyList()))
        .thenReturn(dormantSellerEventList);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE)).thenReturn(sellerFetchBatchSize);
    Mockito.when(
            this.dormantSellerService.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
                DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), SELLER_FETCH_LIMIT))
        .thenReturn(
            new PageImpl<>(dormantSellerEventList, sellerEventPage, TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(this.dormantSellerService.upsertDormantSellerEvent(dormantSellerEvent))
        .thenReturn(dormantSellerEvent);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    dormantSellerServiceWrapperImpl.processPendingDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME,
        SellerProcessType.DORMANT.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE);
    Mockito.verify(this.dormantSellerService)
        .findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
            DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), SELLER_FETCH_LIMIT);
    Mockito.verify(this.dormantSellerService, times(1))
        .upsertDormantSellerEvent(dormantSellerEvent);
    Mockito.verify(dormantSellerService).saveDormantSellerEvents(Mockito.anyList());
  }

  @Test
  public void processPendingDormantSellerEventActiveSellerExceptionTest() throws Exception {
    dormantSellerEventList.forEach(
        dormantSellerEvent -> dormantSellerEvent.setStatus(DormantSellerStatus.FETCHED.name()));
    Mockito.when(dormantSellerService.saveDormantSellerEvents(Mockito.anyList()))
        .thenReturn(dormantSellerEventList);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE)).thenReturn(sellerFetchBatchSize);
    Mockito.when(
            this.dormantSellerService.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
                DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), SELLER_FETCH_LIMIT))
        .thenReturn(
            new PageImpl<>(dormantSellerEventList, sellerEventPage, TOTAL_RECORDS_SINGLE_PAGE));
    Mockito.when(this.dormantSellerService.upsertDormantSellerEvent(dormantSellerEvent))
        .thenReturn(dormantSellerEvent);
    Mockito.doThrow(RuntimeException.class).when(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    dormantSellerServiceWrapperImpl.processPendingDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME,
        SellerProcessType.DORMANT.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE);
    Mockito.verify(this.dormantSellerService)
        .findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(STORE_ID,
            DormantSellerStatus.PENDING.name(), SellerProcessType.DORMANT.name(), SELLER_FETCH_LIMIT);
    Mockito.verify(this.dormantSellerService, times(1))
        .upsertDormantSellerEvent(dormantSellerEventArgumentCaptor.capture());
    Mockito.verify(dormantSellerService).saveDormantSellerEvents(Mockito.anyList());
    Assertions.assertEquals(dormantSellerEventArgumentCaptor.getValue().getStatus(),
        DormantSellerStatus.FAILED.name());
  }

}