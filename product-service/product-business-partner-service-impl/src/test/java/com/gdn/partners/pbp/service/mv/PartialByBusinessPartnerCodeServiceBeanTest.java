package com.gdn.partners.pbp.service.mv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.service.mv.indexing.PartialByBusinessPartnerCodeServiceBean;
import com.gdn.partners.pbp.service.mv.indexing.PartialByBusinessPartnerCodeServiceBean.ProductLevel3AggregatorPartialIndexer;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorService;
import com.gdn.partners.pbp.util.ProductAggregatorIndexingUtil;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;

public class PartialByBusinessPartnerCodeServiceBeanTest {

  private static final String CHANNEL_ID = "channel";

  private static final String CLIENT_ID = "client";

  private static final String STORE_ID = "store";

  private static final String USERNAME = "username";

  private static final String REQUEST_ID = "requestid";

  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";

  @InjectMocks
  private PartialByBusinessPartnerCodeServiceBean instance;

  @Mock
  private ProductLevel3AggregatorService productLevel3AggregatorService;

  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @Mock
  private ApplicationProperties applicationProperties;

  @Mock
  private ProductAggregatorIndexingUtil productAggregatorIndexingUtil;
  
  @Mock
  private ThreadPoolTaskExecutor productLevel3AggregatorPartialIndexer;

  private MandatoryRequestParam mandatoryRequestParam;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    mandatoryRequestParam = MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID,
        CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, null);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productAggregatorIndexingUtil);
    Mockito.verifyNoMoreInteractions(this.productLevel3AggregatorService);
  }

  private Page<ItemSummaryResponse> getProductItemSummaryResponse() {
    List<ItemSummaryResponse> contents = new ArrayList<>();
    int x = 0;
    while (x < 5) {
      ItemSummaryResponse content = new ItemSummaryResponse();
      content.setItemSku("sku" + x);
      contents.add(content);
      x++;
    }
    Page<ItemSummaryResponse> response = new PageImpl<>(contents, PageRequest.of(0, 5), 10);
    return response;
  }

  private Page<ItemSummaryResponse> getProductItemSummaryMoreThanPageResponse() {
    List<ItemSummaryResponse> contents = new ArrayList<>();
    int x = 0;
    while (x < 10) {
      ItemSummaryResponse content = new ItemSummaryResponse();
      content.setItemSku("sku" + x);
      contents.add(content);
      x++;
    }
    Page<ItemSummaryResponse> response = new PageImpl<>(contents, PageRequest.of(0, 5), 20);
    return response;
  }

  @Test
  public void doIndexingTest() throws Exception {
    Map<String, Object> parameter = new HashMap<String, Object>();
    parameter.put(PartialByBusinessPartnerCodeServiceBean.BUSINESS_PARTNER_CODES,
        null);
    Mockito.when(this.applicationProperties.getBatchPartialIndexingSize()).thenReturn(5);
    Mockito.when(this.productAggregatorIndexingUtil.isCurrentIndexingRunning())
        .thenReturn(Boolean.FALSE);
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
//    Mockito
//        .when(
//            this.productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class),
//                Mockito.any(PageRequest.class), Mockito.any(SortOrder.class)))
//        .thenReturn(this.getProductItemSummaryResponse());
//    Mockito.doNothing().when(this.productLevel3AggregatorService).update(Mockito.anyString(),
//        Mockito.anyString());
    Mockito.when(this.productLevel3AggregatorPartialIndexer.getActiveCount()).thenReturn(0);
    Mockito.when(this.productLevel3AggregatorPartialIndexer.getCorePoolSize()).thenReturn(5);
    this.instance.doIndexing(mandatoryRequestParam, parameter, false);
    Mockito.verify(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.verify(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
    Mockito.verify(this.productAggregatorIndexingUtil).isCurrentIndexingRunning();
//    Mockito.verify(this.productLevel3AggregatorService, Mockito.times(10))
//        .update(Mockito.anyString(), Mockito.anyString());
//    Mockito.verify(this.productLevel3Repository, Mockito.times(2)).findSummaryByFilter(
//        Mockito.any(ItemSummaryRequest.class), Mockito.any(PageRequest.class),
//        Mockito.any(SortOrder.class));
  }

  @Test
  public void doIndexingForceTest() throws Exception {
    Map<String, Object> parameter = new HashMap<String, Object>();
    parameter.put(PartialByBusinessPartnerCodeServiceBean.BUSINESS_PARTNER_CODES,
        Arrays.asList(BUSINESS_PARTNER_CODE));
    Mockito.when(this.applicationProperties.getBatchPartialIndexingSize()).thenReturn(5);
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
//    Mockito
//        .when(
//            this.productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class),
//                Mockito.any(PageRequest.class), Mockito.any(SortOrder.class)))
//        .thenReturn(this.getProductItemSummaryResponse());
//    Mockito.doNothing().when(this.productLevel3AggregatorService).update(Mockito.anyString(),
//        Mockito.anyString());
    Mockito.when(this.productLevel3AggregatorPartialIndexer.getActiveCount()).thenReturn(0);
    Mockito.when(this.productLevel3AggregatorPartialIndexer.getCorePoolSize()).thenReturn(5);
    this.instance.doIndexing(mandatoryRequestParam, parameter, true);
    Mockito.verify(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.verify(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
//    Mockito.verify(this.productLevel3AggregatorService, Mockito.times(10))
//        .update(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void doIndexingRunningTest() throws Exception {
    Map<String, Object> parameter = new HashMap<String, Object>();
    parameter.put(PartialByBusinessPartnerCodeServiceBean.BUSINESS_PARTNER_CODES,
        BUSINESS_PARTNER_CODE);
    Mockito.when(this.productAggregatorIndexingUtil.isCurrentIndexingRunning())
        .thenReturn(Boolean.TRUE);
    this.instance.doIndexing(mandatoryRequestParam, parameter, false);
    Mockito.verify(this.productAggregatorIndexingUtil).isCurrentIndexingRunning();
  }

  @Test
  public void doIndexingTotalPageMoreThanCurrentPageTest() throws Exception {
    Mockito.when(this.applicationProperties.getBatchPartialIndexingSize()).thenReturn(5);
    Mockito
        .when(
            this.productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class),
                Mockito.any(PageRequest.class), Mockito.any(SortOrder.class)))
        .thenReturn(this.getProductItemSummaryMoreThanPageResponse());
    Mockito.doNothing().when(this.productLevel3AggregatorService).update(Mockito.anyString(),
        Mockito.anyString());
    ExecutorService executorService = Executors.newSingleThreadExecutor();
    ProductLevel3AggregatorPartialIndexer productLevel3AggregatorPartialIndexer =
        this.instance.new ProductLevel3AggregatorPartialIndexer(this.applicationProperties,
            this.productLevel3AggregatorService, this.mandatoryRequestParam,
            PartialByBusinessPartnerCodeServiceBeanTest.BUSINESS_PARTNER_CODE);
    executorService.execute(productLevel3AggregatorPartialIndexer);
    Thread.sleep(1000);
    executorService.shutdown();
    Mockito.verify(this.productLevel3AggregatorService, Mockito.times(40)).update(Mockito.anyString(),
        Mockito.anyString());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void doIndexingExceptionWhenGetProductSummaryTest() throws Exception {
    Mockito.when(this.applicationProperties.getBatchPartialIndexingSize()).thenReturn(5);
    Mockito
        .when(
            this.productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class),
                Mockito.any(PageRequest.class), Mockito.any(SortOrder.class)))
        .thenThrow(Exception.class);
    Mockito.doNothing().when(this.productLevel3AggregatorService).update(Mockito.anyString(),
        Mockito.anyString());
    ExecutorService executorService = Executors.newSingleThreadExecutor();
    ProductLevel3AggregatorPartialIndexer productLevel3AggregatorPartialIndexer =
        this.instance.new ProductLevel3AggregatorPartialIndexer(this.applicationProperties,
            this.productLevel3AggregatorService, this.mandatoryRequestParam,
            PartialByBusinessPartnerCodeServiceBeanTest.BUSINESS_PARTNER_CODE);
    executorService.execute(productLevel3AggregatorPartialIndexer);
    Thread.sleep(1000);
    executorService.shutdown();
    Mockito.verify(this.productLevel3Repository, Mockito.times(1)).findSummaryByFilter(
        Mockito.any(ItemSummaryRequest.class), Mockito.any(PageRequest.class),
        Mockito.any(SortOrder.class));
  }

  @Test
  public void doIndexingExceptionWhenUpsertTest() throws Exception {
    Mockito.when(this.applicationProperties.getBatchPartialIndexingSize()).thenReturn(5);
    Mockito
        .when(
            this.productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class),
                Mockito.any(PageRequest.class), Mockito.any(SortOrder.class)))
        .thenReturn(this.getProductItemSummaryResponse());
    Mockito.doThrow(Exception.class).when(this.productLevel3AggregatorService)
        .update(Mockito.anyString(), Mockito.anyString());
    ExecutorService executorService = Executors.newSingleThreadExecutor();
    ProductLevel3AggregatorPartialIndexer productLevel3AggregatorPartialIndexer =
        this.instance.new ProductLevel3AggregatorPartialIndexer(this.applicationProperties,
            this.productLevel3AggregatorService, this.mandatoryRequestParam,
            PartialByBusinessPartnerCodeServiceBeanTest.BUSINESS_PARTNER_CODE);
    executorService.execute(productLevel3AggregatorPartialIndexer);
    Thread.sleep(1000);
    executorService.shutdown();
    Mockito.verify(this.productLevel3AggregatorService, Mockito.times(10))
        .update(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.productLevel3Repository, Mockito.times(2)).findSummaryByFilter(
        Mockito.any(ItemSummaryRequest.class), Mockito.any(PageRequest.class),
        Mockito.any(SortOrder.class));
  }


}
