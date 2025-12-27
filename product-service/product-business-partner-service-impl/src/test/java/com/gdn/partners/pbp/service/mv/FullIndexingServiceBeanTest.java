package com.gdn.partners.pbp.service.mv;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.service.mv.indexing.FullIndexingServiceBean;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorService;
import com.gdn.partners.pbp.util.ProductAggregatorIndexingUtil;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;

public class FullIndexingServiceBeanTest {

  private static final String CHANNEL_ID = "channel";
  private static final String CLIENT_ID = "client";
  private static final String STORE_ID = "store";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestid";

  @InjectMocks
  private FullIndexingServiceBean fullIndexingServiceBean;

  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @Mock
  private ApplicationProperties applicationProperties;

  @Mock
  private ProductLevel3AggregatorService productLevel3AggregatorService;

  @Mock
  private ProductAggregatorIndexingUtil productAggregatorIndexingUtil;

  private MandatoryRequestParam mandatoryRequestParam;

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

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, null);
  }

  @Test
  public void doIndexingTest() throws Exception {
    Mockito.when(this.productAggregatorIndexingUtil.isCurrentIndexingRunning()).thenReturn(Boolean.FALSE);
    Mockito.when(this.productLevel3Repository
        .findSummaryByFilter(Mockito.any(ItemSummaryRequest.class), Mockito.any(PageRequest.class),
            Mockito.any(SortOrder.class))).thenReturn(this.getProductItemSummaryResponse());
    Mockito.when(this.applicationProperties.getBatchFullIndexingSize()).thenReturn(5);
    Map<String, Object> requestMap = new HashMap<>();
    this.fullIndexingServiceBean.doIndexing(mandatoryRequestParam, requestMap, false);
  }

  @Test
  public void doIndexingTest_ExceptionOnUpdate() throws Exception {
    Mockito.when(this.productAggregatorIndexingUtil.isCurrentIndexingRunning()).thenReturn(Boolean.FALSE);
    Mockito.when(this.productLevel3Repository
        .findSummaryByFilter(Mockito.any(ItemSummaryRequest.class), Mockito.any(PageRequest.class),
            Mockito.any(SortOrder.class))).thenReturn(this.getProductItemSummaryResponse());
    Mockito.when(this.applicationProperties.getBatchFullIndexingSize()).thenReturn(5);
    Mockito.doThrow(Exception.class).when(this.productLevel3AggregatorService)
        .update(Mockito.anyString(), Mockito.anyString());
    Map<String, Object> requestMap = new HashMap<>();
    this.fullIndexingServiceBean.doIndexing(mandatoryRequestParam, requestMap, false);
  }

  @Test
  public void doIndexingTest_Exception() throws Exception {
    Mockito.when(this.productAggregatorIndexingUtil.isCurrentIndexingRunning()).thenReturn(Boolean.FALSE);
    Mockito.when(this.productLevel3Repository
        .findSummaryByFilter(Mockito.any(ItemSummaryRequest.class), Mockito.any(PageRequest.class),
            Mockito.any(SortOrder.class))).thenReturn(this.getProductItemSummaryResponse());
    Map<String, Object> requestMap = new HashMap<>();
    this.fullIndexingServiceBean.doIndexing(mandatoryRequestParam, requestMap, true);
  }

  @Test
  public void doIndexingTest_IndexStillRunning() throws Exception {
    Mockito.when(this.productAggregatorIndexingUtil.isCurrentIndexingRunning()).thenReturn(Boolean.TRUE);
    Mockito.when(this.productLevel3Repository
        .findSummaryByFilter(Mockito.any(ItemSummaryRequest.class), Mockito.any(PageRequest.class),
            Mockito.any(SortOrder.class))).thenReturn(this.getProductItemSummaryResponse());
    Map<String, Object> requestMap = new HashMap<>();
    this.fullIndexingServiceBean.doIndexing(mandatoryRequestParam, requestMap, false);
  }
}
