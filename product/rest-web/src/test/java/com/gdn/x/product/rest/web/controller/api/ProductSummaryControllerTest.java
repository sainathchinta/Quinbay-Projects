package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.vo.ItemPickupPointListingUpdateRequestVo;
import com.gdn.x.product.rest.web.model.ProductSummaryControllerPath;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.service.api.ItemPickupPointSummaryService;
import com.gdn.x.product.service.util.ModelConverter;

public class ProductSummaryControllerTest {

  private MockMvc mockMvc;
  private static final String STORE_ID = "store-id";
  private static final String REQUEST_ID = "request-id";
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String USERNAME = "username";
  private static final String PRODUCT_SKU = "productSku";

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

  @InjectMocks
  private ProductSummaryController productSummaryController;

  @Mock
  private ModelConverter modelConverter;

  @Mock
  private ItemPickupPointSummaryService pickupPointService;

  private MandatoryRequestParam mandatoryRequestParam;


  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.productSummaryController).build();
    this.mandatoryRequestParam = MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID,
        CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, USERNAME);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.pickupPointService);
    verifyNoMoreInteractions(this.modelConverter);
  }

  @Test
  public void updateItemPickupPointListingTest() throws Exception {
    ItemPickupPointQuickEditRequest quickEditUpdateRequest = new ItemPickupPointQuickEditRequest();
    ItemPickupPointListingUpdateRequest itemListingUpdateRequest =
      new ItemPickupPointListingUpdateRequest();
    itemListingUpdateRequest.setProductType(ProductType.REGULAR);
    itemListingUpdateRequest.setQuickEditUpdateRequests(Arrays.asList(quickEditUpdateRequest));
    List<ItemPickupPointListingUpdateRequestVo> requestVo = new ArrayList<>();
    Mockito
      .when(modelConverter.convertToItemPickupPointListingUpdateRequestVo(itemListingUpdateRequest))
      .thenReturn(requestVo);
    this.mockMvc.perform(
      post(ProductSummaryControllerPath.SUMMARY_V2 + ProductSummaryControllerPath.LISTING_UPDATE, PRODUCT_SKU)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(OBJECT_MAPPER.writeValueAsString(itemListingUpdateRequest))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)
        .param("productSku", PRODUCT_SKU)).andExpect(status().isOk())
      .andExpect(jsonPath("$.errorCode", equalTo(null)))
      .andExpect(jsonPath("$.errorMessage", equalTo(null)))
      .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.pickupPointService)
      .validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU, ProductType.REGULAR, requestVo);
    verify(modelConverter).convertToItemPickupPointListingUpdateRequestVo(itemListingUpdateRequest);
  }

  @Test
  public void updateItemPickupPointListingExceptionTest() throws Exception {
    ItemPickupPointQuickEditRequest quickEditUpdateRequest = new ItemPickupPointQuickEditRequest();
    ItemPickupPointListingUpdateRequest itemListingUpdateRequest =
      new ItemPickupPointListingUpdateRequest();
    itemListingUpdateRequest.setProductType(ProductType.REGULAR);
    itemListingUpdateRequest.setQuickEditUpdateRequests(Arrays.asList(quickEditUpdateRequest));
    List<ItemPickupPointListingUpdateRequestVo> requestVo = new ArrayList<>();
    Mockito
      .when(modelConverter.convertToItemPickupPointListingUpdateRequestVo(itemListingUpdateRequest))
      .thenReturn(requestVo);
    Mockito.doThrow(ApplicationRuntimeException.class).when(pickupPointService)
      .validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU, ProductType.REGULAR, requestVo);
    this.mockMvc.perform(
      post(ProductSummaryControllerPath.SUMMARY_V2 + ProductSummaryControllerPath.LISTING_UPDATE, PRODUCT_SKU)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(OBJECT_MAPPER.writeValueAsString(itemListingUpdateRequest))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)
        .param("productSku", PRODUCT_SKU)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.pickupPointService)
      .validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU, ProductType.REGULAR, requestVo);
    verify(modelConverter).convertToItemPickupPointListingUpdateRequestVo(itemListingUpdateRequest);
  }
}
