package com.gdn.x.mta.distributiontask.client;

import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.util.GdnHttpClientHelper;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.mta.distributiontask.request.ProductDistributionTaskRequest;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import java.net.URI;
import java.util.ArrayList;

import static org.mockito.Mockito.*;

/**
 * Created by Alok on 10/13/16.
 */
public class DistributionTaskClientTest {

  private static final String BASE_PATH = "/vendor";
  private static final String USERNAME = "user1";
  private static final String PASSWORD = "password";
  private static final String HOST = "localhost";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANNEL_ID = "channelId";
  private static final String STORE_ID = "10001";
  private static final String REQUEST_ID = "requestId";

  private GdnRestClientConfiguration gdnRestClientConfiguration;
  private GdnHttpClientHelper gdnHttpClientHelper;
  private DistributionTaskClient distributionTaskClient;

  @Mock
  private URI uri;


  @BeforeEach
  public void setUp() throws Exception {
    gdnHttpClientHelper = Mockito.mock(GdnHttpClientHelper.class);
    gdnRestClientConfiguration =
        new GdnRestClientConfiguration(USERNAME, PASSWORD, HOST, 8080, CLIENT_ID, CHANNEL_ID,
            STORE_ID);
    distributionTaskClient = new DistributionTaskClient(gdnRestClientConfiguration, BASE_PATH);
    ReflectionTestUtils.setField(distributionTaskClient, "httpClientHelper", gdnHttpClientHelper,
        GdnHttpClientHelper.class);

  }

  private void verifyGdnHttpClientHelperInteractions() throws Exception {
    Mockito.verify(gdnHttpClientHelper)
        .getURI(Mockito.anyString(), Mockito.anyInt(), Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(gdnHttpClientHelper).getClosableHttpConnectionSingleton();
  }

  @Test
   void saveProductDistributionTaskTestOk() throws Exception {
    ProductDistributionTaskRequest productDistributionTaskRequest =
        new ProductDistributionTaskRequest();
    productDistributionTaskRequest.setVendorCode("Vendor_Code");
    productDistributionTaskRequest.setProductCodes(new ArrayList<String>());
    distributionTaskClient
        .saveProductDistributionTask(null, USERNAME, productDistributionTaskRequest);
    verifyGdnHttpClientHelperInteractions();
  }

  @Test
   void saveProductDistributionTaskTestWithRequestIdOk() throws Exception {
    ProductDistributionTaskRequest productDistributionTaskRequest =
        new ProductDistributionTaskRequest();
    productDistributionTaskRequest.setVendorCode("Vendor_Code");
    productDistributionTaskRequest.setProductCodes(new ArrayList<String>());
    distributionTaskClient
        .saveProductDistributionTask(REQUEST_ID, USERNAME, productDistributionTaskRequest);
    verifyGdnHttpClientHelperInteractions();
  }

  @Test
   void removeProductFromPDT() throws Exception {
    GdnBaseRestResponse expectedResponse = new GdnBaseRestResponse();
    expectedResponse.setSuccess(true);
    expectedResponse.setRequestId(REQUEST_ID);
    RemoveProductRequest request = new RemoveProductRequest();
    request.setProductCode("MTA-123456");
    Mockito.when(gdnHttpClientHelper
        .getURI(anyString(), anyInt(), anyString(), any(MandatoryRequestParam.class), any()))
        .thenReturn(uri);
    Mockito.when(gdnHttpClientHelper
        .invokePost(any(), any(), any(Class.class),
            anyString(), any(), any())).thenReturn(expectedResponse);
    GdnBaseRestResponse response =
        distributionTaskClient.removeProductFromPDT(REQUEST_ID, USERNAME, request);
    Assertions.assertEquals(expectedResponse, response);
  }

  @AfterEach
  public void tearDown() throws Exception {

  }

}
