package com.gdn.x.mta.distributiontask.client;

import java.net.URI;
import java.util.Map;

import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.util.GdnHttpClientHelper;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.mta.distributiontask.request.DistributionTaskFilterRequest;
import com.gdn.x.mta.distributiontask.request.RejectProductRequest;
import com.gdn.x.mta.distributiontask.rest.model.constant.QCFilterState;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductResponse;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

/**
 * Created by Alok on 10/13/16.
 */
public class QCTaskClientTest {

  private static final String BASE_PATH = "/vendor";
  private static final String USERNAME = "user1";
  private static final String PASSWORD = "password";
  private static final String HOST = "localhost";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANNEL_ID = "channelId";
  private static final String STORE_ID = "10001";
  private static final String VENDOR_CODE = "v1";
  private static final String REQUEST_ID = "requestId";
  private static final String STATUS = QCFilterState.ALL.toString();

  private GdnRestClientConfiguration gdnRestClientConfiguration;
  private GdnHttpClientHelper gdnHttpClientHelper;
  private QCTaskClient qcTaskClient;
  private GdnRestListRequest gdnRestListRequest;

  @BeforeEach
  public void setUp() throws Exception {

    gdnHttpClientHelper = Mockito.mock(GdnHttpClientHelper.class);
    gdnRestClientConfiguration =
        new GdnRestClientConfiguration(USERNAME, PASSWORD, HOST, 8080, CLIENT_ID, CHANNEL_ID,
            STORE_ID);
    qcTaskClient = new QCTaskClient(gdnRestClientConfiguration, BASE_PATH);
    ReflectionTestUtils
        .setField(qcTaskClient, "httpClientHelper", gdnHttpClientHelper, GdnHttpClientHelper.class);
    gdnRestListRequest = new GdnRestListRequest();
    gdnRestListRequest.setPage(0);
    gdnRestListRequest.setRequestId(REQUEST_ID);
    gdnRestListRequest.setSize(25);
  }

  private void verifyGdnHttpClientHelperInteractions() throws Exception {
    Mockito.verify(gdnHttpClientHelper)
        .getURI(Mockito.anyString(), Mockito.anyInt(), Mockito.anyString(),
            Mockito.any(),
            Mockito.any());
    Mockito.verify(gdnHttpClientHelper).getClosableHttpConnectionSingleton();
  }

  @Test
   void filterProductTestOk() throws Exception {
    ProductListRequest productListRequest = new ProductListRequest();
    productListRequest.setBusinessPartnerCode("business-partner-code");
    qcTaskClient.filterProduct(USERNAME, gdnRestListRequest, STATUS, productListRequest);
    verifyGdnHttpClientHelperInteractions();
  }

  @Test
   void rejectProductTestOk() throws Exception {
    RejectProductRequest rejectProductRequest = new RejectProductRequest();
    qcTaskClient.rejectProduct(null, USERNAME, rejectProductRequest);
    verifyGdnHttpClientHelperInteractions();
  }

  @Test 
  public void approveProductTestOk() throws Exception {
    qcTaskClient.approveProduct(REQUEST_ID, USERNAME, "Product-Id");
  }
  
  @Test
   void moveFailedProductToQCTestOk() throws Exception {
    qcTaskClient.moveFailedProductToQC(null, USERNAME, "product-code");
    Mockito.verify(this.gdnHttpClientHelper, Mockito.times(1)).invokePostType(
        (URI) Mockito.any(), Mockito.any(),
        Mockito.any(),
        Mockito.any(),
        Mockito.eq("application/json"), Mockito.anyInt(), (Map<String, String>) Mockito.any());
  }
  
  @Test
   void countQCProductSummaryByFilter() throws Exception {
    ProductListRequest productListRequest = new ProductListRequest();
    productListRequest.setBusinessPartnerCode("business-partner-code");
    qcTaskClient.countQCProductSummaryByFilter(null, USERNAME, productListRequest);
    Mockito.verify(this.gdnHttpClientHelper, Mockito.times(1)).invokePostType(
        (URI) Mockito.any(), Mockito.any(),
        Mockito.any(),
        Mockito.any(),
        Mockito.eq("application/json"), Mockito.anyInt(), (Map<String, String>) Mockito.any());
  }

  @AfterEach public void tearDown() throws Exception {

  }

}
