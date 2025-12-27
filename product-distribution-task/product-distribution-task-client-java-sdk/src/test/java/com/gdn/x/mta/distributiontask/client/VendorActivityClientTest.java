package com.gdn.x.mta.distributiontask.client;

import java.util.Collections;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.util.GdnHttpClientHelper;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkVendorProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.BulkVendorProductActionsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorProductActionsResponse;


/**
 * Created by virajjasani on 10/10/16.
 */
public class VendorActivityClientTest {

  private static final String USERNAME = "user1";
  private static final String PASSWORD = "password";
  private static final String HOST = "localhost";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANNEL_ID = "channelId";
  private static final String STORE_ID = "10001";
  private static final String VENDOR_CODE = "v1";
  private static final String REQUEST_ID = "requestId";
  private static final String PRODUCT_CODE = "productCode";

  private GdnHttpClientHelper gdnHttpClientHelper;
  private VendorActivityClient vendorActivityClient;
  private BulkVendorProductActionsResponse bulkVendorProductActionsResponse;


  @BeforeEach
  public void setUp() throws Exception {
    gdnHttpClientHelper = Mockito.mock(GdnHttpClientHelper.class);
    vendorActivityClient =
        new VendorActivityClient(USERNAME, PASSWORD, HOST, 8080, CLIENT_ID, CHANNEL_ID, STORE_ID,
            VendorActivityClient.BASE_PATH);
    ReflectionTestUtils.setField(vendorActivityClient, "httpClientHelper", gdnHttpClientHelper,
        GdnHttpClientHelper.class);

    bulkVendorProductActionsResponse = new BulkVendorProductActionsResponse(Collections
        .singletonList(new VendorProductActionsResponse(Collections.singletonList(PRODUCT_CODE), true, null)));
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(gdnHttpClientHelper);
  }

  @Test
   void updateProductContent() throws Exception {
    vendorActivityClient.updateProductContent(REQUEST_ID, USERNAME, VENDOR_CODE,
        new DistributionProductDetailRequest());
    verifyGdnHttpClientHelperInteractions();
    Mockito.verify(gdnHttpClientHelper)
        .invokePost(Mockito.any(), Mockito.any(), Mockito.eq(DistributionProductDetailRequest.class),
            Mockito.anyString(), Mockito.any(), Mockito.any());
  }

  @Test
   void updateProductImage() throws Exception {
    vendorActivityClient.updateProductImage(REQUEST_ID, USERNAME, VENDOR_CODE,
        new DistributionProductDetailRequest());
    verifyGdnHttpClientHelperInteractions();
    Mockito.verify(gdnHttpClientHelper)
        .invokePost(Mockito.any(), Mockito.any(), Mockito.eq(DistributionProductDetailRequest.class),
            Mockito.anyString(), Mockito.any(), Mockito.any());
  }

  @Test
   void rejectProduct() throws Exception {
    vendorActivityClient.rejectProduct(null,USERNAME,VENDOR_CODE,new RejectProductVendorRequest());
    Mockito.verify(gdnHttpClientHelper)
        .invokePost(Mockito.any(), Mockito.any(), Mockito.eq(RejectProductVendorRequest.class), Mockito.anyString(),
            Mockito.any(), Mockito.any());
    verifyGdnHttpClientHelperInteractions();
  }

  @Test
   void approveProduct() throws Exception {
    vendorActivityClient.approveProduct(REQUEST_ID,USERNAME,new VendorApprovalRequest());
    verifyGdnHttpClientHelperInteractions();
    Mockito.verify(gdnHttpClientHelper)
        .invokePost(Mockito.any(), Mockito.any(), Mockito.eq(VendorApprovalRequest.class), Mockito.anyString(),
            Mockito.any(), Mockito.any());
  }

  @Test
   void approveProductNewTest() throws Exception {
    vendorActivityClient.approveProductNew(REQUEST_ID, USERNAME, Constants.TYPE_IMAGE, VENDOR_CODE,
        new DistributionProductDetailRequest());
    verifyGdnHttpClientHelperInteractions();
    Mockito.verify(gdnHttpClientHelper)
        .invokePost(Mockito.any(), Mockito.any(), Mockito.eq(VendorApprovalRequest.class), Mockito.anyString(),
            Mockito.any(), Mockito.any());
  }

  @Test
   void bulkVendorProductActionsTest() throws Exception {
    Mockito.when(gdnHttpClientHelper.invokePostType(Mockito.any(), Mockito.any(BulkVendorProductActionsRequest.class),
        Mockito.eq(BulkVendorProductActionsRequest.class),
        Mockito.any(),
        Mockito.anyString(), Mockito.anyInt(), Mockito.any()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, bulkVendorProductActionsResponse, null));
    GdnRestSingleResponse<BulkVendorProductActionsResponse> response =
        vendorActivityClient.bulkVendorProductActions(REQUEST_ID, USERNAME, new BulkVendorProductActionsRequest());
    Mockito.verify(gdnHttpClientHelper)
        .getURI(Mockito.anyString(), Mockito.anyInt(), Mockito.anyString(),
            Mockito.any(),
            Mockito.any());
    Mockito.verify(gdnHttpClientHelper)
        .invokePostType(Mockito.any(), Mockito.any(BulkVendorProductActionsRequest.class),
            Mockito.eq(BulkVendorProductActionsRequest.class),
            Mockito.any(),
            Mockito.anyString(), Mockito.anyInt(),
            Mockito.any());
    Assertions.assertEquals(PRODUCT_CODE,
        response.getValue().getVendorProductActionsResponses().get(0).getProductCode().get(0));

  }

  private void verifyGdnHttpClientHelperInteractions() throws Exception {
    Mockito.verify(gdnHttpClientHelper)
        .getURI(Mockito.anyString(), Mockito.anyInt(), Mockito.anyString(),
            (MandatoryRequestParam) Mockito.any(),
            Mockito.anyMap());
    Mockito.verify(gdnHttpClientHelper).getClosableHttpConnectionSingleton();
  }

}
