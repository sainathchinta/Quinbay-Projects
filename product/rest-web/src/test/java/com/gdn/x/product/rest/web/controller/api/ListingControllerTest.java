package com.gdn.x.product.rest.web.controller.api;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.gdn.x.product.enums.Constants;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.rest.web.model.ListingApiPath;
import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequestV2;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.ItemL5ListingResponse;
import com.gdn.x.product.service.api.ListingService;

public class ListingControllerTest {
  private static final String STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String PRODUCT_SKU = "productSku";

  @InjectMocks
  private ListingController listingController;

  @Mock
  private ListingService listingService;

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;
  private ItemPickupPointSummaryRequest itemPickupPointSummaryRequest;
  private ProductSummaryRequestV2 productSummaryRequestV2;
  private ItemRequestV2 itemRequestV2;
  private ItemPickupPointListingRequest itemPickupPointListingRequest;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.listingController).build();
    objectMapper = new ObjectMapper();

    itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    productSummaryRequestV2 = new ProductSummaryRequestV2();
    itemRequestV2 = new ItemRequestV2();
    itemPickupPointListingRequest = new ItemPickupPointListingRequest();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(listingService);
  }

  @Test
  public void getItemPickupPointSummaryTest() throws Exception {
    Mockito.when(listingService.getItemPickupPointSummary(STORE_ID, 0, 1, itemPickupPointSummaryRequest,
            null))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.ITEM_PICKUP_POINT_SUMMARY).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(itemPickupPointSummaryRequest)).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("page", "0").param("size", "1")).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(true));
    Mockito.verify(listingService).getItemPickupPointSummary(STORE_ID, 0, 1, itemPickupPointSummaryRequest,
        null);
  }

  @Test
  public void getItemPickupPointSummaryErrorTest() throws Exception {
    Mockito.when(listingService.getItemPickupPointSummary(STORE_ID, 0, 1, itemPickupPointSummaryRequest,
            null))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.ITEM_PICKUP_POINT_SUMMARY).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(itemPickupPointSummaryRequest))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", "0").param("size", "1"))
        .andExpect(status().isOk()).andExpect(MockMvcResultMatchers.jsonPath("$.success").value(false));
    Mockito.verify(listingService).getItemPickupPointSummary(STORE_ID, 0, 1, itemPickupPointSummaryRequest,
        null);
  }

  @Test
  public void getHalalDashboardProductsTest() throws Exception {
    HalalProductsFilterRequest halalProductsFilterRequest = new HalalProductsFilterRequest();
    Mockito.when(listingService.getHalalDashboardProductsResponses(STORE_ID, 0, 1, halalProductsFilterRequest))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.GET_HALAL_DASHBOARD_PRODUCTS).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(halalProductsFilterRequest))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", "0").param("size", "1"))
        .andExpect(status().isOk()).andExpect(MockMvcResultMatchers.jsonPath("$.success").value(true));
    Mockito.verify(listingService).getHalalDashboardProductsResponses(STORE_ID, 0, 1, halalProductsFilterRequest);
  }

  @Test
  public void getHalalDashboardProductsExceptionTest() throws Exception {
    HalalProductsFilterRequest halalProductsFilterRequest = new HalalProductsFilterRequest();
    Mockito.when(listingService.getHalalDashboardProductsResponses(STORE_ID, 0, 1, halalProductsFilterRequest))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.GET_HALAL_DASHBOARD_PRODUCTS).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(halalProductsFilterRequest))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", "0").param("size", "1"))
        .andExpect(status().isOk()).andExpect(MockMvcResultMatchers.jsonPath("$.success").value(false));
    Mockito.verify(listingService).getHalalDashboardProductsResponses(STORE_ID, 0, 1, halalProductsFilterRequest);
  }

  @Test
  public void getProductSummaryV2Test() throws Exception {
    Mockito.when(listingService.getProductSummary(STORE_ID, 0, 1, productSummaryRequestV2))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.PRODUCT_SUMMARY_V2).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(productSummaryRequestV2)).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("page", "0").param("size", "1")).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(true));
    Mockito.verify(listingService).getProductSummary(STORE_ID, 0, 1, productSummaryRequestV2);
  }

  @Test
  public void getProductSummaryV2ErrorTest() throws Exception {
    Mockito.when(listingService.getProductSummary(STORE_ID, 0, 1, productSummaryRequestV2))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.PRODUCT_SUMMARY_V2).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(productSummaryRequestV2)).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("page", "0").param("size", "1")).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(false));
    Mockito.verify(listingService).getProductSummary(STORE_ID, 0, 1, productSummaryRequestV2);
  }

  @Test
  public void getItemPickupPointsByItemSkuTest() throws Exception {
    Mockito.when(listingService.getItemPickupPointsByItemSku(STORE_ID, 0, 1, itemRequestV2, false))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.ITEM_PICKUP_POINT_BY_ITEM_SKU).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(itemRequestV2)).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("page", "0").param("size", "1")).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(true));
    Mockito.verify(listingService).getItemPickupPointsByItemSku(STORE_ID, 0, 1, itemRequestV2, false);
  }

  @Test
  public void getItemPickupPointsByItemSkuErrorTest() throws Exception {
    Mockito.when(listingService.getItemPickupPointsByItemSku(STORE_ID, 0, 1, itemRequestV2, false))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.ITEM_PICKUP_POINT_BY_ITEM_SKU).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(itemRequestV2)).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("page", "0").param("size", "1")).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(false));
    Mockito.verify(listingService).getItemPickupPointsByItemSku(STORE_ID, 0, 1, itemRequestV2, false);
  }

  @Test
  public void getItemPickupPointListTest() throws Exception {
    Mockito.when(listingService.getItemPickupPointListing(STORE_ID, 0, 1, Constants.CNC,
        itemPickupPointListingRequest)).thenReturn(new PageImpl<>(new ArrayList<>()));
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.ITEM_PICKUP_LISTING).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(itemPickupPointListingRequest)).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("page", "0").param("size", "1").param("fetchViewConfigByChannel", Constants.CNC)).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(true));
    Mockito.verify(listingService).getItemPickupPointListing(STORE_ID, 0, 1,  Constants.CNC, itemPickupPointListingRequest);
  }

  @Test
  public void getItemPickupPointErrorList() throws Exception {
    Mockito.when(listingService.getItemPickupPointListing(STORE_ID, 0, 1, Constants.CNC, itemPickupPointListingRequest))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.ITEM_PICKUP_LISTING).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(itemRequestV2)).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("page", "0").param("size", "1").param("fetchViewConfigByChannel", Constants.CNC)).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(false));
    Mockito.verify(listingService).getItemPickupPointListing(STORE_ID, 0, 1, Constants.CNC, itemPickupPointListingRequest);
  }

  @Test
  public void getItemL5DetailsTest() throws Exception {
    Mockito.when(
            listingService.getItemL5Listing(STORE_ID, null,
              Collections.emptyList(), 0, 1, false, false))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.ITEM_L5_DETAILS)
        .content(this.objectMapper.writeValueAsString(new SimpleListStringRequest())).contentType(
                MediaType.APPLICATION_JSON).param("productSku", "").param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId",
                REQUEST_ID)
            .param("username", USERNAME).param("cncActivated", "false").param("page", "0").param("size", "1"))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(true));
    Mockito.verify(listingService)
        .getItemL5Listing(STORE_ID, null, Collections.emptyList(), 0, 1, false, false);
  }

  @Test
  public void getItemL5Details_withProductSkuTest() throws Exception {
    List<String> productSkus = Collections.singletonList(PRODUCT_SKU);
    Mockito.when(
        listingService.getItemL5Listing(STORE_ID, productSkus,
          Collections.emptyList(), 0, 1, false, false))
      .thenReturn(new PageImpl<>(new ArrayList<>()));
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.ITEM_L5_DETAILS)
        .content(this.objectMapper.writeValueAsString(new SimpleListStringRequest())).contentType(
          MediaType.APPLICATION_JSON).param("productSku", PRODUCT_SKU).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId",
          REQUEST_ID)
        .param("username", USERNAME).param("cncActivated", "false").param("page", "0").param("size", "1"))
      .andExpect(status().isOk())
      .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(true));
    Mockito.verify(listingService)
      .getItemL5Listing(STORE_ID, productSkus, Collections.emptyList(), 0, 1, false, false);
  }

  @Test
  public void getItemL5Details_ExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(listingService)
        .getItemL5Listing(STORE_ID, null, Collections.emptyList(), 0, 1, false, false);
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.ITEM_L5_DETAILS)
        .content(this.objectMapper.writeValueAsString(new SimpleListStringRequest()))
        .contentType(
                MediaType.APPLICATION_JSON).param("productSku", "").param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId",
                REQUEST_ID)
            .param("username", USERNAME).param("cncActivated", "false").param("page", "0").param("size", "1"))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(false));
    Mockito.verify(listingService)
        .getItemL5Listing(STORE_ID, null, Collections.emptyList(), 0, 1, false, false);
  }

  @Test
  public void getItemL5DetailsPageSizeNullTest() throws Exception {
    List<String> productSkus = Collections.singletonList(PRODUCT_SKU);
    List<ItemL5ListingResponse> itemL5ListingResponses = new ArrayList<>();
    ItemL5ListingResponse itemL5ListingResponse = new ItemL5ListingResponse();
    itemL5ListingResponse.setItemSku("ItemSku");
    itemL5ListingResponses.add(itemL5ListingResponse);
    Mockito.when(listingService.getItemL5Listing(STORE_ID, productSkus, Collections.emptyList(), 0, null, null, false))
        .thenReturn(new PageImpl<>(itemL5ListingResponses));
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.ITEM_L5_DETAILS).content(
                this.objectMapper.writeValueAsString(new SimpleListStringRequest())).contentType(MediaType.APPLICATION_JSON)
            .param("productSku", PRODUCT_SKU).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
            .param("cncActivated", "").param("page", "0").param("size", "")).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(true));
    Mockito.verify(listingService).getItemL5Listing(STORE_ID, productSkus, Collections.emptyList(), 0, null, null, false);
  }

  @Test
  public void getItemL5DetailsPageSizeTest() throws Exception {
    List<String> productSkus = Collections.singletonList(PRODUCT_SKU);
    List<ItemL5ListingResponse> itemL5ListingResponses = new ArrayList<>();
    ItemL5ListingResponse itemL5ListingResponse = new ItemL5ListingResponse();
    itemL5ListingResponse.setItemSku("ItemSku");
    itemL5ListingResponses.add(itemL5ListingResponse);
    Mockito.when(listingService.getItemL5Listing(STORE_ID, productSkus, Collections.emptyList(), 0, 1, true, false))
        .thenReturn(new PageImpl<>(itemL5ListingResponses));
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.ITEM_L5_DETAILS).content(
                this.objectMapper.writeValueAsString(new SimpleListStringRequest())).contentType(MediaType.APPLICATION_JSON)
            .param("productSku", PRODUCT_SKU).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
            .param("cncActivated", "true").param("page", "0").param("size", "1")).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(true));
    Mockito.verify(listingService).getItemL5Listing(STORE_ID, productSkus, Collections.emptyList(), 0, 1, true, false);
  }

  @Test
  public void getItemL5DetailsPageNullTest() throws Exception {
    List<String> productSkus = Collections.singletonList(PRODUCT_SKU);
    List<ItemL5ListingResponse> itemL5ListingResponses = new ArrayList<>();
    ItemL5ListingResponse itemL5ListingResponse = new ItemL5ListingResponse();
    itemL5ListingResponse.setItemSku("ItemSku");
    itemL5ListingResponses.add(itemL5ListingResponse);
    Mockito.when(listingService.getItemL5Listing(STORE_ID, productSkus, Collections.emptyList(), null, null, null, false))
        .thenReturn(new PageImpl<>(itemL5ListingResponses));
    this.mockMvc.perform(post(ListingApiPath.BASE_PATH + ListingApiPath.ITEM_L5_DETAILS).content(
                this.objectMapper.writeValueAsString(new SimpleListStringRequest())).contentType(MediaType.APPLICATION_JSON)
            .param("productSku", PRODUCT_SKU).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
            .param("cncActivated", "").param("page", "").param("size", "")).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(true));
    Mockito.verify(listingService).getItemL5Listing(STORE_ID, productSkus, Collections.emptyList(), null, null, null, false);
  }
}
