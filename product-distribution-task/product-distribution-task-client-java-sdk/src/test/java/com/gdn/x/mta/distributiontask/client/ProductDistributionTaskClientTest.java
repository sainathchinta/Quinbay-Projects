package com.gdn.x.mta.distributiontask.client;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.anyInt;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.util.GdnHttpClientHelper;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.client.util.ProductDistributionTaskClientPath;
import com.gdn.x.mta.distributiontask.request.DistributionTaskFilterRequest;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.response.ProductBusinessPartnerMapperResponse;
import com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductCodeListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.PDTProductDomainEventModelResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.ProductListResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorProductStatusResponse;

/**
 * Created by virajjasani on 18/09/16.
 */
public class ProductDistributionTaskClientTest {

  private static final String USERNAME = "user1";
  private static final String PASSWORD = "password";
  private static final String HOST = "localhost";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";
  private static final String MERCHANT_NAME = "merchant1";
  private static final String VENDOR_CODE = "v1";
  private static final String CURRENT_DATE = "2012-12-12 12:12";
  private static final String JSON_TYPE = "application/json";
  private static final String REQUEST_ID = "requestId";
  private ProductDistributionTaskClient productDistributionTaskClient;
  private ProductListRequest request;
  private DistributionTaskFilterRequest request2;
  private DistributionTaskMultipleFilterRequest multipleFilterrequest;
  private GdnHttpClientHelper gdnHttpClientHelper;
  private FilterSummaryRequest filterSummaryRequest;

  @Mock
  private URI uri;

  @Test
   void getDistributionSummaryByFilterTest() throws Exception {
    productDistributionTaskClient.getDistributionSummaryByFilter(REQUEST_ID, USERNAME, 0, 10,
        request2);

    Mockito.verify(this.gdnHttpClientHelper, Mockito.times(1)).invokePostType(
        (URI) Mockito.any(), (DistributionTaskFilterRequest) Mockito.any(),
        Mockito.eq(DistributionTaskFilterRequest.class),
        (TypeReference<GdnRestListResponse<DistributionProductResponse>>) Mockito.any(),
        Mockito.eq(JSON_TYPE), Mockito.anyInt(), (Map<String, String>) Mockito.any());
  }

  @Test
   void getDistributionSummaryByMultipleFilterTest() throws Exception {
    productDistributionTaskClient.getDistributionSummaryByMultipleFilter(REQUEST_ID, USERNAME, 0, 10, multipleFilterrequest);
    Mockito.verify(this.gdnHttpClientHelper, Mockito.times(1)).invokePostType((URI) Mockito.any(),
        (DistributionTaskMultipleFilterRequest) Mockito.any(), Mockito.eq(DistributionTaskMultipleFilterRequest
            .class), (TypeReference<GdnRestListResponse<DistributionProductResponse>>) Mockito.any(), Mockito
            .eq(JSON_TYPE), Mockito.anyInt(), (Map<String, String>) Mockito.any());
  }
  
  @Test
   void countDistributionSummaryByFilterTest() throws Exception {
    productDistributionTaskClient.countDistributionSummaryByFilter(true, true, "", USERNAME,
        request2);

    Mockito.verify(this.gdnHttpClientHelper, Mockito.times(1)).invokePostType(
        (URI) Mockito.any(), (DistributionTaskFilterRequest) Mockito.any(),
        Mockito.eq(DistributionTaskFilterRequest.class),
        (TypeReference<GdnRestSingleResponse<MapResponse>>) Mockito.any(),
        Mockito.eq(JSON_TYPE), Mockito.anyInt(), (Map<String, String>) Mockito.any());
  }

  @Test
   void getProductLisingForVendorCodeTest() throws Exception {
    productDistributionTaskClient.getProductListingForVendorCode(REQUEST_ID, USERNAME, 0, 10,
        request);
    Mockito.verify(gdnHttpClientHelper).getURI(Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyString(), Mockito.any(),
        Mockito.any());
    Mockito.verify(gdnHttpClientHelper)
        .invokePostType(Mockito.any(), Mockito.any(),
            Mockito.eq(ProductListRequest.class), Mockito.any(),
            Mockito.anyString(), Mockito.anyInt(), Mockito.any());
  }

  @Test
   void getProductListTest() throws Exception {
    productDistributionTaskClient.getProductDetails(REQUEST_ID, USERNAME, PRODUCT_CODE);
    Mockito.verify(gdnHttpClientHelper).invokeGetSingle(Mockito.any(),
        Mockito.eq(DistributionProductDetailResponse.class), Mockito.anyString(),
        Mockito.any(), Mockito.any());
    Mockito.verify(gdnHttpClientHelper).getURI(Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyString(), Mockito.any(),
        Mockito.any());
  }

  @Test public void filterProductBusinessPartnerMapperByWorkFlowStateTestOk() throws Exception {
    GdnRestListRequest gdnRestListRequest = new GdnRestListRequest();
    gdnRestListRequest.setPage(0);
    gdnRestListRequest.setSize(25);
    gdnRestListRequest.setRequestId(REQUEST_ID);
    productDistributionTaskClient
        .filterProductBusinessPartnerMapperByWorkFlowState(gdnRestListRequest, USERNAME,
            WorkflowWebState.CONTENT_APPROVED.toString(), true, "keyword");
    Mockito.verify(gdnHttpClientHelper).invokeGetSummary(Mockito.any(),
        Mockito.eq(ProductBusinessPartnerMapperResponse.class), Mockito.anyString(),
        Mockito.any(), Mockito.any());
    Mockito.verify(gdnHttpClientHelper)
        .getURI(Mockito.anyString(), Mockito.anyInt(), Mockito.anyString(),
            Mockito.any(),
            Mockito.any());
  }

  @Test
   void getDetailsForProduct() throws Exception {
    GdnRestSingleResponse<DistributionProductDetailResponse> expectedResponse =
        new GdnRestSingleResponse<DistributionProductDetailResponse>(
            new DistributionProductDetailResponse(), REQUEST_ID);
    Mockito.when(gdnHttpClientHelper
        .getURI(anyString(), anyInt(), anyString(), any(), any()))
        .thenReturn(uri);
    Mockito.when(gdnHttpClientHelper
        .invokeGetSingle(any(), any(Class.class), anyString(),
            any(), any())).thenReturn(expectedResponse);
    GdnRestSingleResponse<DistributionProductDetailResponse> response =
        productDistributionTaskClient.getDetailsForProduct(REQUEST_ID, USERNAME, PRODUCT_CODE);
    Assertions.assertEquals(expectedResponse, response);
  }

  @Test
   void countVendorProductSummaryByFilter() throws Exception {
    GdnRestSingleResponse<MapResponse> expectedResponse =
        new GdnRestSingleResponse<MapResponse>(new MapResponse(), REQUEST_ID);
    Mockito.when(gdnHttpClientHelper
        .getURI(anyString(), anyInt(), anyString(), any(), any()))
        .thenReturn(uri);
    Mockito.when(gdnHttpClientHelper
        .invokePostType(any(), any(), any(Class.class), any(),
            anyString(), anyInt(), any())).thenReturn(expectedResponse);
    GdnRestSingleResponse<MapResponse> response = productDistributionTaskClient
        .countVendorProductSummaryByFilter(REQUEST_ID, USERNAME, new ProductListRequest());
    Assertions.assertEquals(expectedResponse, response);
  }

  @Test public void countProductStatusForVendor() throws Exception {
    GdnRestSingleResponse<VendorProductStatusResponse> expectedResponse =
        new GdnRestSingleResponse<VendorProductStatusResponse>(new VendorProductStatusResponse(),
            REQUEST_ID);
    Mockito.when(gdnHttpClientHelper
            .getURI(anyString(), anyInt(), anyString(), any(), any()))
        .thenReturn(uri);
    Mockito.when(gdnHttpClientHelper
        .invokeGetSingle(any(), any(Class.class), anyString(),
            any(), any())).thenReturn(expectedResponse);
    GdnRestSingleResponse<VendorProductStatusResponse> response = productDistributionTaskClient
        .countProductStatusForVendor(REQUEST_ID, USERNAME, VENDOR_CODE);
    Assertions.assertEquals(expectedResponse, response);
  }

  @Test public void getPDTDomainModelResponseByCode() throws Exception {
    GdnRestSingleResponse<PDTProductDomainEventModelResponse> expectedResponse =
        new GdnRestSingleResponse<PDTProductDomainEventModelResponse>(
            new PDTProductDomainEventModelResponse(), REQUEST_ID);
    Mockito.when(gdnHttpClientHelper
        .getURI(anyString(), anyInt(), anyString(), any(MandatoryRequestParam.class), any()))
        .thenReturn(uri);
    Mockito.when(gdnHttpClientHelper
        .invokeGetSingle(any(), any(Class.class), anyString(),
            any(), any())).thenReturn(expectedResponse);
    GdnRestSingleResponse<PDTProductDomainEventModelResponse> response =
        productDistributionTaskClient
            .getPDTDomainModelResponseByCode(REQUEST_ID, USERNAME, PRODUCT_CODE);
    Assertions.assertEquals(expectedResponse, response);
  }

  @Test
   void getProductCodeListTest() throws Exception {
    ProductListResponse productListResponse = new ProductListResponse(Arrays.asList("test"));
    GdnRestSingleResponse<ProductListResponse> response =
        new GdnRestSingleResponse<ProductListResponse>(productListResponse, REQUEST_ID);
    Mockito.when(this.gdnHttpClientHelper
        .invokePostType(Mockito.any(), Mockito.any(),
            Mockito.eq(ProductCodeListRequest.class),
            Mockito.any(),
            Mockito.eq(JSON_TYPE), Mockito.anyInt(), Mockito.any()))
        .thenReturn(response);

    GdnRestSingleResponse<ProductListResponse> result = productDistributionTaskClient
        .getProductCodeList(REQUEST_ID, USERNAME, new ProductCodeListRequest(new ArrayList<>()));

    Mockito.verify(this.gdnHttpClientHelper, Mockito.times(1))
        .invokePostType(Mockito.any(), Mockito.any(),
            Mockito.eq(ProductCodeListRequest.class),
            Mockito.any(),
            Mockito.eq(JSON_TYPE), Mockito.anyInt(), Mockito.any());
    Assertions.assertEquals(response, result);
  }

  @BeforeEach
  public void setUp() throws Exception {
    new ProductDistributionTaskClientPath();
    GdnRestClientConfiguration config =
        new GdnRestClientConfiguration(USERNAME, PASSWORD, HOST, 8080, CLIENT_ID, CHANNEL_ID,
            STORE_ID);
    productDistributionTaskClient =
        new ProductDistributionTaskClient(config, "product-distribution-task-service");
    Assertions.assertNotNull(productDistributionTaskClient);
    productDistributionTaskClient = new ProductDistributionTaskClient(config);
    Assertions.assertNotNull(productDistributionTaskClient);
    productDistributionTaskClient = new ProductDistributionTaskClient(USERNAME, PASSWORD, HOST,
        8080, CLIENT_ID, CHANNEL_ID, STORE_ID, ProductDistributionTaskClientPath.BASE_PATH);
    Assertions.assertNotNull(productDistributionTaskClient);
    gdnHttpClientHelper = Mockito.mock(GdnHttpClientHelper.class);
    ReflectionTestUtils.setField(productDistributionTaskClient, "httpClientHelper",
        gdnHttpClientHelper, GdnHttpClientHelper.class);
    request = new ProductListRequest();
    request.setCategoryCode(CATEGORY_CODE);
    request.setVendorCode(VENDOR_CODE);
    request.setProductCode(PRODUCT_CODE);
    request.setStartDate(CURRENT_DATE);
    request.setEndDate(CURRENT_DATE);
    request.setBusinessPartnerName(MERCHANT_NAME);
  }

  @Test
   void rejectProductTest() throws Exception {
    this.productDistributionTaskClient.rejectProduct(null,USERNAME,new RejectProductListRequest());
    Mockito.verify(gdnHttpClientHelper)
        .invokePost(Mockito.any(), Mockito.any(),
            Mockito.eq(RejectProductListRequest.class), Mockito.anyString(),
            Mockito.any(),
            Mockito.any());
  }

  @Test
   void getProductListFilterTest() throws Exception {
    productDistributionTaskClient.getProductList(REQUEST_ID, USERNAME, 0, 10, filterSummaryRequest);
    Mockito.verify(gdnHttpClientHelper)
        .getURI(Mockito.anyString(), Mockito.anyInt(), Mockito.anyString(), Mockito.any(),
            Mockito.any());
    Mockito.verify(gdnHttpClientHelper)
        .invokePostType(Mockito.any(), Mockito.any(), Mockito.eq(FilterSummaryRequest.class),
            Mockito.any(), Mockito.anyString(), Mockito.anyInt(),
            Mockito.any());
  }

  @Test
   void sendProductBackToVendorTest() throws Exception {
    GdnBaseRestResponse expectedResponse = new GdnBaseRestResponse(null, null, true, REQUEST_ID);
    Mockito.when(gdnHttpClientHelper
        .invokeGetType((URI) Mockito.any(), (TypeReference<GdnBaseRestResponse>) Mockito.any(),
            Mockito.eq(ProductDistributionTaskClientPath.APPLICATION_JSON_VALUE), Mockito.anyInt()))
        .thenReturn(expectedResponse);
    GdnBaseRestResponse response =
        productDistributionTaskClient.sendProductBackToVendor(REQUEST_ID, USERNAME, PRODUCT_CODE);
    Assertions.assertEquals(expectedResponse, response);
    Mockito.verify(gdnHttpClientHelper)
        .invokeGetType((URI) Mockito.any(), (TypeReference<GdnBaseRestResponse>) Mockito.any(),
            Mockito.eq(ProductDistributionTaskClientPath.APPLICATION_JSON_VALUE), Mockito.anyInt());
  }
}
