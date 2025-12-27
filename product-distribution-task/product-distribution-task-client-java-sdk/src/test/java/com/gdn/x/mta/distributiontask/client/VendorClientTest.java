package com.gdn.x.mta.distributiontask.client;

import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.util.GdnHttpClientHelper;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.mta.distributiontask.request.VendorDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorCapacityResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorTaskInformationResponse;

import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;
import com.fasterxml.jackson.core.type.TypeReference;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import java.net.URI;

/**
 * Created by Alok on 10/11/16.
 */
public class VendorClientTest {

  private static final String BASE_PATH = "/vendor";
  private static final String USERNAME = "user1";
  private static final String PASSWORD = "password";
  private static final String HOST = "localhost";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANNEL_ID = "channelId";
  private static final String STORE_ID = "10001";
  private static final String REQUEST_ID = "requestId";
  public static final String APPLICATION_JSON_VALUE = "application/json";

  private GdnHttpClientHelper gdnHttpClientHelper;
  private VendorClient vendorClient;
  private GdnRestClientConfiguration gdnRestClientConfiguration;

  private GdnRestSimpleResponse<Integer> createResponse(){
    GdnRestSimpleResponse<Integer> response = new GdnRestSimpleResponse<>();
    response.setValue(0);
    return response;
  }

  @BeforeEach public void setUp() throws Exception {

    gdnHttpClientHelper = Mockito.mock(GdnHttpClientHelper.class);
    gdnRestClientConfiguration =
        new GdnRestClientConfiguration(USERNAME, PASSWORD, HOST, 8080, CLIENT_ID, CHANNEL_ID,
            STORE_ID);
    vendorClient = new VendorClient(gdnRestClientConfiguration, BASE_PATH);
    ReflectionTestUtils
        .setField(vendorClient, "httpClientHelper", gdnHttpClientHelper, GdnHttpClientHelper.class);
  }

  @Test public void createVendorTestOkWithRequestId() throws Exception {
    VendorDetailRequest vendorDetailRequest =
        new VendorDetailRequest.Builder().isAbleToReject(false).isQcRequired(false)
            .vendorCode("vendor-code").build();
    vendorClient.createVendor(REQUEST_ID, USERNAME, vendorDetailRequest);
    verifyGdnHttpClientHelperInteractions();
  }

  @Test public void createVendorTestOk() throws Exception {
    VendorDetailRequest vendorDetailRequest =
        new VendorDetailRequest.Builder().isAbleToReject(false).isQcRequired(false)
            .vendorCode("vendor-code").build();
    vendorClient.createVendor(null, USERNAME, vendorDetailRequest);
    verifyGdnHttpClientHelperInteractions();
  }

  @Test public void findVendorDetailResponseListTestOk() throws Exception {
    vendorClient.findVendorDetailResponseList(REQUEST_ID, USERNAME, 0, 25);
    verifyGdnHttpClientHelperInteractions();
  }

  @Test public void findVendorbyVendorCodeTestOk() throws Exception {
    vendorClient.findVendorbyVendorCode(REQUEST_ID, USERNAME, "vendor-code");
    verifyGdnHttpClientHelperInteractions();
  }

  @AfterEach public void tearDown() throws Exception {

  }

  private void verifyGdnHttpClientHelperInteractions() throws Exception {
    Mockito.verify(gdnHttpClientHelper)
        .getURI(Mockito.anyString(), Mockito.anyInt(), Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(gdnHttpClientHelper).getClosableHttpConnectionSingleton();
  }

  @Test
   void countVendorsCapacityTest() throws Exception {
    vendorClient.countVendorsCapacity(REQUEST_ID, USERNAME);
    Mockito.verify(gdnHttpClientHelper).invokeGetSummary(Mockito.any(),
        Mockito.eq(VendorCapacityResponse.class), Mockito.anyString(),
        Mockito.any(), Mockito.any());
    Mockito.verify(gdnHttpClientHelper).getURI(Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyString(), Mockito.any(),
        Mockito.any());
  }

  @Test
   void countVendorsTaskTest() throws Exception {
    vendorClient.countVendorsTask(REQUEST_ID, USERNAME);
    Mockito.verify(gdnHttpClientHelper).invokeGetSummary(Mockito.any(),
        Mockito.eq(VendorTaskInformationResponse.class), Mockito.anyString(),
        Mockito.any(), Mockito.any());
    Mockito.verify(gdnHttpClientHelper).getURI(Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyString(), Mockito.any(),
        Mockito.any());
  }

  @Test
   void countProductAssignToVendorTestOk() throws Exception {
    GdnRestSimpleResponse<Integer> response = new GdnRestSimpleResponse<>();
    response.setValue(0);
    Mockito.when(gdnHttpClientHelper.invokeGetType((URI) Mockito.any(),
        (TypeReference<GdnRestSimpleResponse>) Mockito.any(),
        Mockito.eq(APPLICATION_JSON_VALUE), Mockito.anyInt())).thenReturn(response);
    GdnRestSimpleResponse<Integer> clientResponse =
        vendorClient.assignedProductCountToVendor(REQUEST_ID, USERNAME, "vendor-code");
    Mockito.verify(gdnHttpClientHelper).invokeGetType((URI) Mockito.any(),
        (TypeReference<GdnRestSimpleResponse>) Mockito.any(),
        Mockito.eq(APPLICATION_JSON_VALUE), Mockito.anyInt());
    Assertions.assertEquals(createResponse().getValue(), clientResponse.getValue());
  }

  @Test public void deleteVendorTestOk() throws Exception {

    vendorClient.deleteVender(REQUEST_ID, USERNAME, "vendor-code");
    Mockito.verify(gdnHttpClientHelper)
        .getURI(Mockito.anyString(), Mockito.anyInt(), Mockito.anyString(),
            (MandatoryRequestParam) Mockito.any(),
            Mockito.anyMap());
  }

}
