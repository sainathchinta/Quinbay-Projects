package com.gdn.partners.pcu.external.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.ProductOptimisationApiPath;
import com.gdn.partners.pcu.external.service.ProductOptimisationService;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationFeedbackRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationListWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationUpdateStatusRequest;
import com.gdn.partners.pcu.external.web.model.response.ProductOptimisationListResponse;
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

import java.util.List;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

@ExtendWith(MockitoExtension.class)
class ProductOptimisationControllerTest {

  private static final String STORE_ID = "10001";
  private static final String SELLER_CODE = "sellerCode";
  private static final String REQUEST_ID = "requestId";
  private static final String PRODUCT_SKU = "productSku";
  private static final String STATUS = "status";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private MockMvc mockMvc;
  private ObjectMapper objectMapper;
  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @InjectMocks
  private ProductOptimisationController productOptimisationController;

  @Mock
  private ProductOptimisationService productOptimisationService;

  @BeforeEach
  void setUp() {
    this.mockMvc = standaloneSetup(this.productOptimisationController).build();
    objectMapper = new ObjectMapper();
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(productOptimisationService);
    Mockito.verifyNoMoreInteractions(mandatoryParameterHelper);
  }

  @Test
  void getProductCountTest() throws Exception {
    Mockito.when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(SELLER_CODE);
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    this.mockMvc.perform(
            get(ProductOptimisationApiPath.BASE_PATH + ProductOptimisationApiPath.PRODUCT_COUNT))
        .andExpect(status().isOk());
    Mockito.verify(this.productOptimisationService)
        .getProductCount(STORE_ID, REQUEST_ID, SELLER_CODE);
    Mockito.verify(mandatoryParameterHelper).getStoreId();
    Mockito.verify(mandatoryParameterHelper).getBusinessPartnerCode();
    Mockito.verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  void filter() throws Exception {
    ProductOptimisationListWebRequest productOptimisationListWebRequest =
        new ProductOptimisationListWebRequest();
    productOptimisationListWebRequest.setKeyword(PRODUCT_SKU);
    ProductOptimisationListResponse response = new ProductOptimisationListResponse();
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    Page<ProductOptimisationListResponse> pageResponse =
        new PageImpl<>(List.of(response), pageable, SIZE);
    Mockito.when(productOptimisationService.fetchProductOptimisationList(SELLER_CODE,
        productOptimisationListWebRequest, PAGE, SIZE)).thenReturn(pageResponse);
    Mockito.when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(SELLER_CODE);
    this.mockMvc.perform(
            post(ProductOptimisationApiPath.BASE_PATH + ProductOptimisationApiPath.FILTER).contentType(
                    MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
                .queryParam("page", String.valueOf(PAGE))
                .queryParam("size", String.valueOf(SIZE))
                .content(objectMapper.writeValueAsString(productOptimisationListWebRequest)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    Mockito.verify(productOptimisationService).fetchProductOptimisationList(SELLER_CODE,
        productOptimisationListWebRequest, PAGE, SIZE);
    Mockito.verify(mandatoryParameterHelper).getBusinessPartnerCode();
    Mockito.verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  void provideSuggestionFeedbackTest() throws Exception {
    ProductOptimisationFeedbackRequest productOptimisationFeedbackRequest =
        new ProductOptimisationFeedbackRequest();
    productOptimisationFeedbackRequest.setProductSku(PRODUCT_SKU);
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    this.mockMvc.perform(post(ProductOptimisationApiPath.BASE_PATH
            + ProductOptimisationApiPath.PROVIDE_FEEDBACK).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(productOptimisationFeedbackRequest)))
        .andExpect(status().isOk());
    Mockito.verify(this.productOptimisationService)
        .submitSuggestionFeedback(productOptimisationFeedbackRequest);
    Mockito.verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  void suggestionDetailsTest() throws Exception {
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    this.mockMvc.perform(get(ProductOptimisationApiPath.BASE_PATH
            + ProductOptimisationApiPath.SUGGESTION_DETAILS).queryParam(PRODUCT_SKU, PRODUCT_SKU))
        .andExpect(status().isOk());
    Mockito.verify(productOptimisationService).getSuggestionDetails(PRODUCT_SKU);
    Mockito.verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  void updateStatusTest() throws Exception {
    ProductOptimisationUpdateStatusRequest productOptimisationUpdateStatusRequest =
        new ProductOptimisationUpdateStatusRequest();
    productOptimisationUpdateStatusRequest.setProductSku(PRODUCT_SKU);
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    this.mockMvc.perform(post(ProductOptimisationApiPath.BASE_PATH
            + ProductOptimisationApiPath.UPDATE_STATUS).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(productOptimisationUpdateStatusRequest)))
        .andExpect(status().isOk());
    Mockito.verify(productOptimisationService).updateStatusForProductOptimisation(REQUEST_ID,
        productOptimisationUpdateStatusRequest);
    Mockito.verify(mandatoryParameterHelper).getRequestId();
  }
}
