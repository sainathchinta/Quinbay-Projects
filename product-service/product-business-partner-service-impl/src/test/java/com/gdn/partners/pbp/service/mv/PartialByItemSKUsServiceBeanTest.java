package com.gdn.partners.pbp.service.mv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.partners.pbp.service.mv.indexing.PartialByItemSKUsServiceBean;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorService;
import com.gdn.partners.pbp.service.redis.StringRedisService;
import com.gdn.partners.pbp.util.ProductAggregatorIndexingUtil;

public class PartialByItemSKUsServiceBeanTest {

  private static final String CHANNEL_ID = "channel";

  private static final String CLIENT_ID = "client";

  private static final String STORE_ID = "store";

  private static final String USERNAME = "username";

  private static final String REQUEST_ID = "requestid";

  @InjectMocks
  private PartialByItemSKUsServiceBean instance;

  @Mock
  private ProductAggregatorIndexingUtil productAggregatorIndexingUtil;
  
  @Mock
  private ProductLevel3AggregatorService productLevel3AggregatorService;

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

  @Test
  public void doIndexingTest() throws Exception {
    Map<String, Object> parameter = new HashMap<String, Object>();
    parameter.put("test", new ArrayList<>(Arrays.asList("test_prd1", "test_prd2")));
    parameter.put("test2", new ArrayList<>(Arrays.asList("test2_prd1", "test2_prd2")));
    Mockito.when(this.productAggregatorIndexingUtil.isCurrentIndexingRunning())
        .thenReturn(Boolean.FALSE);
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
    Mockito.doNothing().when(this.productLevel3AggregatorService).update(Mockito.anyString(),
        Mockito.anyString());
    this.instance.doIndexing(mandatoryRequestParam, parameter, false);
    Mockito.verify(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.verify(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
    Mockito.verify(this.productAggregatorIndexingUtil).isCurrentIndexingRunning();
    Mockito.verify(this.productLevel3AggregatorService, Mockito.times(4))
        .update(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void doIndexingIndexingAlreadyRunningTest() throws Exception {
    Map<String, Object> parameter = new HashMap<String, Object>();
    parameter.put("test", new ArrayList<>(Arrays.asList("test_prd1", "test_prd2")));
    parameter.put("test2", new ArrayList<>(Arrays.asList("test2_prd1", "test2_prd2")));
    Mockito.when(this.productAggregatorIndexingUtil.isCurrentIndexingRunning())
        .thenReturn(Boolean.TRUE);
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
    Mockito.doNothing().when(this.productLevel3AggregatorService).update(Mockito.anyString(),
        Mockito.anyString());
    this.instance.doIndexing(mandatoryRequestParam, parameter, false);
    Mockito.verify(this.productAggregatorIndexingUtil).isCurrentIndexingRunning();
  }

  @Test
  public void doIndexingForcedTest() throws Exception {
    Map<String, Object> parameter = new HashMap<String, Object>();
    parameter.put("test", new ArrayList<>(Arrays.asList("test_prd1", "test_prd2")));
    parameter.put("test2", new ArrayList<>(Arrays.asList("test2_prd1", "test2_prd2")));
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
    Mockito.doNothing().when(this.productLevel3AggregatorService).update(Mockito.anyString(),
        Mockito.anyString());
    this.instance.doIndexing(mandatoryRequestParam, parameter, true);
    Mockito.verify(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.verify(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
    Mockito.verify(this.productLevel3AggregatorService, Mockito.times(4))
        .update(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void doIndexingUpsertFailedTest() throws Exception {
    Map<String, Object> parameter = new HashMap<String, Object>();
    parameter.put("test", new ArrayList<>(Arrays.asList("test_prd1", "test_prd2")));
    parameter.put("test2", new ArrayList<>(Arrays.asList("test2_prd1", "test2_prd2")));
    Mockito.when(this.productAggregatorIndexingUtil.isCurrentIndexingRunning())
        .thenReturn(Boolean.FALSE);
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
    Mockito.doThrow(Exception.class).when(this.productLevel3AggregatorService)
        .update(Mockito.anyString(), Mockito.anyString());
    this.instance.doIndexing(mandatoryRequestParam, parameter, false);
    Mockito.verify(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.verify(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
    Mockito.verify(this.productAggregatorIndexingUtil).isCurrentIndexingRunning();
    Mockito.verify(this.productLevel3AggregatorService, Mockito.times(4))
        .update(Mockito.anyString(), Mockito.anyString());
  }
  
  @Test
  public void doIndexingNullParameterTest() throws Exception {
    Map<String, Object> parameter = new HashMap<String, Object>();
    parameter.put("test", new ArrayList<>(Arrays.asList("test_prd1", "test_prd2")));
    parameter.put("test2", new ArrayList<>(Arrays.asList("test2_prd1", "test2_prd2")));
    Mockito.when(this.productAggregatorIndexingUtil.isCurrentIndexingRunning())
        .thenReturn(Boolean.FALSE);
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
    this.instance.doIndexing(mandatoryRequestParam, null, false);
    Mockito.verify(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.verify(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
    Mockito.verify(this.productAggregatorIndexingUtil).isCurrentIndexingRunning();
  }
  
  @Test
  public void doIndexingProductEmptyTest() throws Exception {
    Map<String, Object> parameter = new HashMap<String, Object>();
    parameter.put("test", null);
    Mockito.when(this.productAggregatorIndexingUtil.isCurrentIndexingRunning())
        .thenReturn(Boolean.FALSE);
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.doNothing().when(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
    this.instance.doIndexing(mandatoryRequestParam, null, false);
    Mockito.verify(this.productAggregatorIndexingUtil).setIndexingRunning();
    Mockito.verify(this.productAggregatorIndexingUtil).delIndexingRunningProcess();
    Mockito.verify(this.productAggregatorIndexingUtil).isCurrentIndexingRunning();
  }

}
