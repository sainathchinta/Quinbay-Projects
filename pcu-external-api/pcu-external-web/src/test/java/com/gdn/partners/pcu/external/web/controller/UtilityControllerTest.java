package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.model.UtilityApiPath;
import com.gdn.partners.pcu.external.service.ProductService;
import com.gdn.partners.pcu.external.service.VideoService;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.Matchers.is;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

class UtilityControllerTest extends TestHelper {

  private static final String PRODUCT_SKU = "productSku";
  private static final String ITEM_SKU = "itemSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String VIDEO_ID = "test-video-id";
  private static final String BRAND_REQUEST_CODE = "BRAND-REQ-12345";

  @InjectMocks
  private UtilityController utilityController;

  @Mock
  private ProductService productService;

  @Mock
  private VideoService videoService;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(utilityController).build();
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(videoService);
  }

  @Test
  void republishProductToAgp() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(UtilityApiPath.BASE_PATH + UtilityApiPath.REPUBLISH_L3_TO_AGP, PRODUCT_SKU).accept(
            MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(productService).republishProductToAgp(PRODUCT_SKU);
  }

  @Test
  void republishItemToAgp() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(UtilityApiPath.BASE_PATH + UtilityApiPath.REPUBLISH_L4_TO_AGP, ITEM_SKU).accept(
            MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(productService).republishItemToAgp(ITEM_SKU);
  }

  @Test
  void republishItemPickupPointToAgp() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(UtilityApiPath.BASE_PATH + UtilityApiPath.REPUBLISH_L5_TO_AGP, ITEM_SKU, PICKUP_POINT_CODE).accept(
            MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(productService).republishItemPickupPointToAgp(ITEM_SKU, PICKUP_POINT_CODE, true);
  }

  @Test
  void retryVideoCompression_Success() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
      post(UtilityApiPath.BASE_PATH + UtilityApiPath.RETRY_VIDEO_COMPRESSION, VIDEO_ID).accept(
        MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE);

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));

    Mockito.verify(videoService).retryVideoCompression(VIDEO_ID);
  }

  @Test
  void reindexBrandSolrCollection_Success() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
      post(UtilityApiPath.BASE_PATH + UtilityApiPath.REINDEX_BRAND_COLLECTION_BY_BRAND_REQUEST_CODE,
        BRAND_REQUEST_CODE).accept(MediaType.APPLICATION_JSON_VALUE)
        .contentType(MediaType.APPLICATION_JSON_VALUE);

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));

    Mockito.verify(productService).reindexBrandCollection(BRAND_REQUEST_CODE);
  }

  @Test
  void generateFingerPrint_Success() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
      post(UtilityApiPath.BASE_PATH + UtilityApiPath.GENERATE_FINGERPRINT, VIDEO_ID).accept(
        MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE);

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));

    Mockito.verify(videoService).generateFingerPrint(VIDEO_ID);
  }
}