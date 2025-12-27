package com.gdn.partners.product.analytics.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.model.ProductOptimisationApiPath;
import com.gdn.partners.product.analytics.service.ProductOptimisationService;
import com.gdn.partners.product.analytics.web.model.ProductCountsWebResponse;
import com.gdn.partners.product.analytics.web.model.ProductOptimisationListResponse;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationFeedbackRequest;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationListRequest;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationUpdateStatusRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.util.ArrayList;
import java.util.List;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

@ExtendWith(MockitoExtension.class)
class ProductOptimisationControllerTest {

  private static final String STORE_ID = "storeId";
  private static final String REQUEST_ID = "requestId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final int PRODUCT_COUNTS = 2;
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String USER_NAME = "username";
  private static final String SELLER_CODE = "sellerCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String STATUS = "status";

  @InjectMocks
  private ProductOptimisationController productOptimisationController;

  @Mock
  private ProductOptimisationService productOptimisationService;

  private ObjectMapper objectMapper;

  private MockMvc mockMvc;

  @BeforeEach
  void setUp() {
    this.mockMvc = standaloneSetup(this.productOptimisationController).build();
    objectMapper = new ObjectMapper();
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(productOptimisationService);
  }

  @Test
  void getProductCountsTest() throws Exception {
    ProductCountsWebResponse productCountsWebResponse = new ProductCountsWebResponse();
    productCountsWebResponse.setProductCount(PRODUCT_COUNTS);
    Mockito.when(productOptimisationService.getProductCounts(STORE_ID, SELLER_CODE))
        .thenReturn(productCountsWebResponse);
    this.mockMvc.perform(
            get(ProductOptimisationApiPath.BASE_PATH + ProductOptimisationApiPath.PRODUCT_COUNT).param(
                "storeId", STORE_ID).param("sellerCode", SELLER_CODE).param("requestId",
                REQUEST_ID)).andExpect(status().isOk());
    verify(productOptimisationService).getProductCounts(STORE_ID, SELLER_CODE);
  }

  @Test
  void updateProductOptimiseStatusTest() throws Exception {
    ProductOptimisationUpdateStatusRequest productOptimisationUpdateStatusRequest =
        new ProductOptimisationUpdateStatusRequest();
    productOptimisationUpdateStatusRequest.setProductSku(PRODUCT_SKU);
    productOptimisationUpdateStatusRequest.setViewed(Boolean.TRUE);
    productOptimisationUpdateStatusRequest.setFieldEdited(new ArrayList<>());
    this.mockMvc.perform(post(ProductOptimisationApiPath.BASE_PATH
            + ProductOptimisationApiPath.UPDATE_STATUS).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).queryParam("requestId", REQUEST_ID)
            .content(objectMapper.writeValueAsString(productOptimisationUpdateStatusRequest)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    verify(productOptimisationService).updateStatusForProductOptimisation(
        productOptimisationUpdateStatusRequest);
  }

  @Test
  void filterTest() throws Exception {
    ProductOptimisationListRequest productOptimisationListRequest =
        new ProductOptimisationListRequest();
    productOptimisationListRequest.setKeyword(PRODUCT_SKU);
    productOptimisationListRequest.setSellerCode(SELLER_CODE);
    ProductOptimisationListResponse response = new ProductOptimisationListResponse();
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    Page<ProductOptimisationListResponse> pageResponse =
        new PageImpl<>(List.of(response), pageable, SIZE);
    Mockito.when(productOptimisationService.fetchProductOptimisationList(STORE_ID,
        productOptimisationListRequest, PAGE, SIZE)).thenReturn(pageResponse);
    this.mockMvc.perform(
            post(ProductOptimisationApiPath.BASE_PATH + ProductOptimisationApiPath.FILTER).contentType(
                    MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
                .queryParam("storeId", STORE_ID).queryParam("channelId", CHANNEL_ID)
                .queryParam("clientId", CLIENT_ID).queryParam("requestId", REQUEST_ID)
                .queryParam("username", USER_NAME).queryParam("page", String.valueOf(PAGE))
                .queryParam("size", String.valueOf(SIZE))
                .content(objectMapper.writeValueAsString(productOptimisationListRequest)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    Mockito.verify(productOptimisationService)
        .fetchProductOptimisationList(STORE_ID, productOptimisationListRequest, PAGE, SIZE);
  }

  @Test
  void submitSuggestionFeedback() throws Exception {
    ProductOptimisationFeedbackRequest productOptimisationFeedbackRequest =
        new ProductOptimisationFeedbackRequest();
    productOptimisationFeedbackRequest.setProductSku(PRODUCT_SKU);
    this.mockMvc.perform(post(ProductOptimisationApiPath.BASE_PATH
            + ProductOptimisationApiPath.PROVIDE_FEEDBACK).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).queryParam("requestId", REQUEST_ID)
            .content(objectMapper.writeValueAsString(productOptimisationFeedbackRequest)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    Mockito.verify(productOptimisationService).submitSuggestionFeedback(productOptimisationFeedbackRequest);
  }

  @Test
  void showSuggestionFeedback() throws Exception {
    this.mockMvc.perform(get(ProductOptimisationApiPath.BASE_PATH
            + ProductOptimisationApiPath.SUGGESTION_DETAIL).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).queryParam("requestId", REQUEST_ID)
            .param("productSku", PRODUCT_SKU).param("storeId", STORE_ID))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    Mockito.verify(productOptimisationService).showSuggestionDetails(STORE_ID, PRODUCT_SKU);
  }

  @Test
  void testClearSellerCache_Success() throws Exception {
    Mockito.doNothing().when(productOptimisationService).clearSellerLevelCache(SELLER_CODE);
    this.mockMvc.perform(
        get(ProductOptimisationApiPath.BASE_PATH + ProductOptimisationApiPath.CLEAR_CACHE,
          SELLER_CODE).param("requestId", REQUEST_ID)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success").value(true))
      .andExpect(jsonPath("$.requestId").value(REQUEST_ID));
    verify(productOptimisationService).clearSellerLevelCache(SELLER_CODE);
  }
}
