package com.gdn.x.mta.distributiontask.client;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.impl.client.CloseableHttpClient;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.util.GdnHttpClientHelper;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.mta.distributiontask.client.util.TaskHistoryClientPath;
import com.gdn.x.mta.distributiontask.rest.model.response.TaskHistoryResponse;

public class TaskHistoryClientTest {
  private static final String USERNAME = "user1";
  private static final String PASSWORD = "password";
  private static final String HOST = "localhost";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String STORE_ID = "10001";
  private static final String REQUEST_ID = "123a";
  private static final String REQUEST_ID_EMPTY = "";
  private static final String PRODUCT_CODE = "productCode";
  private static final String MERCHANT_NAME = "merchant1";
  private static final String VENDOR_CODE = "v1";
  private static final String CURRENT_DATE = "2012-12-12 12:12";
  private static final String JSON_TYPE = "application/json";
  private static final int CONNECTION_TIMEOUT_IN_MS = 1;
  private static final String TASK_CODE = "TASK_CODE";
  private TaskHistoryClient taskHistoryClient;
  private GdnHttpClientHelper gdnHttpClientHelper;

  @BeforeEach
  public void setUp() throws Exception {
    new TaskHistoryClientPath();
    this.taskHistoryClient = new TaskHistoryClient(USERNAME, PASSWORD, HOST, 8080, CLIENT_ID,
        CHANNEL_ID, STORE_ID, TaskHistoryClientPath.BASE_PATH);
    TaskHistoryClient taskHistoryClientTest =
        new TaskHistoryClient(new GdnRestClientConfiguration(USERNAME, PASSWORD, HOST, 8080,
            CLIENT_ID, CHANNEL_ID, STORE_ID), TaskHistoryClientPath.BASE_PATH);
    gdnHttpClientHelper = Mockito.mock(GdnHttpClientHelper.class);
    ReflectionTestUtils.setField(taskHistoryClient, "httpClientHelper", gdnHttpClientHelper,
        GdnHttpClientHelper.class);
  }

  @AfterEach
  public void tearDown() throws Exception {}

  @Test
   void testGetProductHistories() throws Exception {
    taskHistoryClient.getProductHistories(REQUEST_ID, USERNAME, 0, 10, PRODUCT_CODE);
    Mockito.verify(gdnHttpClientHelper).invokeGetSummary(Mockito.any(),
        Mockito.eq(TaskHistoryResponse.class), Mockito.anyString(),
        Mockito.any(), Mockito.any());
    Mockito.verify(gdnHttpClientHelper).getURI(Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyString(), Mockito.any(),
        Mockito.any());
  }

  @Test
   void testGetProductHistoriesRequestIdEmpty() throws Exception {
    taskHistoryClient.getProductHistories(REQUEST_ID_EMPTY, USERNAME, 0, 10, PRODUCT_CODE);
    Mockito.verify(gdnHttpClientHelper).invokeGetSummary(Mockito.any(),
        Mockito.eq(TaskHistoryResponse.class), Mockito.anyString(),
        Mockito.any(), Mockito.any());
    Mockito.verify(gdnHttpClientHelper).getURI(Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyString(), Mockito.any(),
        Mockito.any());
  }
  
  @Test
   void testGetProductHistoriesByTaskCode() throws Exception {
    taskHistoryClient.getProductHistoriesByTaskCode(REQUEST_ID, USERNAME, 0, 10, TASK_CODE);
    Mockito.verify(gdnHttpClientHelper).invokeGetSummary(Mockito.any(),
        Mockito.eq(TaskHistoryResponse.class), Mockito.anyString(),
        Mockito.any(), Mockito.any());
    Mockito.verify(gdnHttpClientHelper).getURI(Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyString(), Mockito.any(),
        Mockito.any());
  }

}
