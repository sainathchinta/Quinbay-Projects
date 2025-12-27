package com.gdn.mta.product.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ItemSkuPickupPointRequest;
import com.gda.mta.product.dto.ProductCodeAndSkuRequest;
import com.gda.mta.product.dto.response.ProductL3DetailsResponse;
import com.gda.mta.product.dto.response.ProductSkuDetailResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.service.ProductL3Service;
import com.gdn.mta.product.web.model.ProductL3ControllerPath;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.UUID;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

public class ProductL3ControllerTest {

  @InjectMocks
  private ProductL3Controller productL3Controller;

  @Mock
  private ProductL3Service productL3Service;

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_PRODUCT_SKU = "BLI-00001";
  private static final String DEFAULT_CHANNEL_ID = "api";
  private static final String DEFAULT_USERNAME = "com.gdn.mta";
  private static final String DEFAULT_CLIENT_ID = "mta";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String  BUSINESS_PARTNER_CODE = "bpCode";
  private static final String  PICKUP_POINT_CODE = "ppcCode";

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;
  @BeforeEach
  public void initialise() {
    MockitoAnnotations.initMocks(this);
    setMockMvc(MockMvcBuilders.standaloneSetup(productL3Controller)
      .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
        new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
        new MappingJackson2HttpMessageConverter()).build());

    objectMapper = new ObjectMapper();
  }

  public MockMvc getMockMvc() {
    return mockMvc;
  }

  public void setMockMvc(MockMvc mockMvc) {
    this.mockMvc = mockMvc;
  }

  @Test
  public void getL3DetailByProductSkuTest() throws Exception {
    Mockito.when(
            productL3Service.getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, false, true, true))
        .thenReturn(new ProductL3DetailsResponse());

    URI uri = new URIBuilder().setPath(
      ProductL3ControllerPath.BASE_PATH + ProductL3ControllerPath.GET_L3_PRODUCT_DETAIL
        .replaceAll("\\{productSku}", DEFAULT_PRODUCT_SKU))
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
      .addParameter("clientId", DEFAULT_CLIENT_ID)
      .addParameter("isNeedCorrection", Boolean.FALSE.toString()).build();
    mockMvc.perform(MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
      .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productL3Service)
      .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, false, true, true);
  }

  @Test
  public void getL3DetailByProductSkuExceptionTest() throws Exception {
    Mockito.when(
            productL3Service.getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, false, true, true))
        .thenThrow(Exception.class);
    URI uri = new URIBuilder().setPath(
      ProductL3ControllerPath.BASE_PATH + ProductL3ControllerPath.GET_L3_PRODUCT_DETAIL
        .replaceAll("\\{productSku}", DEFAULT_PRODUCT_SKU))
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
      .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(MockMvcRequestBuilders.get(uri))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productL3Service)
        .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, false, true, true);
  }

  @Test
  public void getItemPickupPointL3ListingTest() throws Exception {
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    Mockito.when(
        productL3Service.getItemPickupPointL3Listing(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID, 0, 1,
            itemPickupPointListingL3Request, false, false, true)).thenReturn(new PageImpl<>(new ArrayList<>()));
    URI uri =
        new URIBuilder().setPath(ProductL3ControllerPath.BASE_PATH + ProductL3ControllerPath.GET_L5_PRODUCT_DETAIL)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("page", "0").addParameter("size", "1").build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(itemPickupPointListingL3Request)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productL3Service)
        .getItemPickupPointL3Listing(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID, 0, 1,
            itemPickupPointListingL3Request, true, true, true);
  }

  @Test
  public void getItemPickupPointL3ListingExceptionTest() throws Exception {
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    Mockito.when(
        productL3Service.getItemPickupPointL3Listing(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID, 0, 1,
            itemPickupPointListingL3Request, false, true, true)).thenThrow(ApplicationRuntimeException.class);
    URI uri =
        new URIBuilder().setPath(ProductL3ControllerPath.BASE_PATH + ProductL3ControllerPath.GET_L5_PRODUCT_DETAIL)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("page", "0").addParameter("size", "1").build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(itemPickupPointListingL3Request)))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productL3Service)
        .getItemPickupPointL3Listing(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID, 0, 1,
            itemPickupPointListingL3Request, true, true, true);
  }

  @Test
  public void getL5SummaryByProductSkusTest() throws Exception {
    SimpleListStringRequest productSkus =
      new SimpleListStringRequest(Collections.singletonList(DEFAULT_PRODUCT_SKU));
    Mockito.when(
        productL3Service.getItemPickupPointByProductSkus(0, 1, Collections.singletonList(DEFAULT_PRODUCT_SKU),
          BUSINESS_PARTNER_CODE, false, null))
      .thenReturn(new PageImpl<>(new ArrayList<>()));
    URI uri =
      new URIBuilder().setPath(ProductL3ControllerPath.BASE_PATH + ProductL3ControllerPath.GET_L5_BY_PRODUCT_SKUS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("page", "0")
        .addParameter("size", "1")
        .addParameter("businessPartnerCode", BUSINESS_PARTNER_CODE).build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(productSkus)))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(productL3Service)
      .getItemPickupPointByProductSkus(0, 1, Collections.singletonList(DEFAULT_PRODUCT_SKU),
        BUSINESS_PARTNER_CODE, false, null);
  }

  @Test
  public void getL5SummaryByProductSkus_exceptionTest() throws Exception {
    SimpleListStringRequest productSkus =
      new SimpleListStringRequest(Collections.singletonList(DEFAULT_PRODUCT_SKU));
    Mockito.when(
        productL3Service.getItemPickupPointByProductSkus(0, 1, Collections.singletonList(DEFAULT_PRODUCT_SKU),
          BUSINESS_PARTNER_CODE, false, null))
      .thenThrow(Exception.class);
    URI uri =
      new URIBuilder().setPath(ProductL3ControllerPath.BASE_PATH + ProductL3ControllerPath.GET_L5_BY_PRODUCT_SKUS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("page", "0")
        .addParameter("size", "1").addParameter("businessPartnerCode", BUSINESS_PARTNER_CODE)
        .build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(productSkus)))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(productL3Service)
      .getItemPickupPointByProductSkus(0, 1, Collections.singletonList(DEFAULT_PRODUCT_SKU),
        BUSINESS_PARTNER_CODE, false, null);
  }

  @Test
  public void deleteL5ByPickupPointCodeTest() throws Exception {
    ItemSkuPickupPointRequest itemSkuPickupPointRequest =
      ItemSkuPickupPointRequest.builder().productSku(DEFAULT_PRODUCT_SKU).pickupPointCode(PICKUP_POINT_CODE).build();
      new SimpleListStringRequest(Collections.singletonList(DEFAULT_PRODUCT_SKU));
    Mockito.when(
      productL3Service.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID,
        itemSkuPickupPointRequest))
      .thenReturn((new ArrayList<>()));
    URI uri =
      new URIBuilder().setPath(ProductL3ControllerPath.BASE_PATH + ProductL3ControllerPath.DELETE_L5_BY_PICKUP_POINT_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("page", "0")
        .addParameter("size", "1")
        .addParameter("businessPartnerCode", BUSINESS_PARTNER_CODE).build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
      .contentType(MediaType.APPLICATION_JSON)
      .content(objectMapper.writeValueAsString(itemSkuPickupPointRequest)))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(productL3Service)
      .deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
  }

  @Test
  public void deleteL5ByPickupPointCodeExceptionTest() throws Exception {
    ItemSkuPickupPointRequest itemSkuPickupPointRequest =
      ItemSkuPickupPointRequest.builder().productSku(DEFAULT_PRODUCT_SKU).pickupPointCode(PICKUP_POINT_CODE).build();
    new SimpleListStringRequest(Collections.singletonList(DEFAULT_PRODUCT_SKU));
    Mockito.when(
      productL3Service.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID,
        itemSkuPickupPointRequest))
      .thenThrow(RuntimeException.class);
    URI uri =
      new URIBuilder().setPath(ProductL3ControllerPath.BASE_PATH + ProductL3ControllerPath.DELETE_L5_BY_PICKUP_POINT_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("page", "0")
        .addParameter("size", "1")
        .addParameter("businessPartnerCode", BUSINESS_PARTNER_CODE).build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
      .contentType(MediaType.APPLICATION_JSON)
      .content(objectMapper.writeValueAsString(itemSkuPickupPointRequest)))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(productL3Service)
      .deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
  }

  @Test
  public void getProductSkuDetailResponseTest() throws Exception {
    ProductCodeAndSkuRequest request = new ProductCodeAndSkuRequest();
    ProductSkuDetailResponse skuDetailResponse = new ProductSkuDetailResponse();
    request.setProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.when(productL3Service.getProductSkuDetailResponse(DEFAULT_STORE_ID, request))
      .thenReturn(skuDetailResponse);

    URI uri = new URIBuilder().setPath(
        ProductL3ControllerPath.BASE_PATH + ProductL3ControllerPath.GET_PRODUCT_SKU_DETAIL)
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
      .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(productL3Service).getProductSkuDetailResponse(DEFAULT_STORE_ID, request);
  }

  @Test
  public void getProductSkuDetailResponseExceptionTest() throws Exception {
    ProductCodeAndSkuRequest request = new ProductCodeAndSkuRequest();
    ProductSkuDetailResponse skuDetailResponse = new ProductSkuDetailResponse();
    request.setProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.when(productL3Service.getProductSkuDetailResponse(DEFAULT_STORE_ID, request))
      .thenThrow(ApplicationRuntimeException.class);
    URI uri = new URIBuilder().setPath(
        ProductL3ControllerPath.BASE_PATH + ProductL3ControllerPath.GET_PRODUCT_SKU_DETAIL)
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
      .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(productL3Service).getProductSkuDetailResponse(DEFAULT_STORE_ID, request);
  }
}
