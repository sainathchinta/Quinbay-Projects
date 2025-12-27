package com.gdn.partners.pcu.internal.web.controller;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ProductCenterApiPath;
import com.gdn.partners.pcu.internal.service.ProductCenterService;
import com.gdn.partners.pcu.internal.web.TestApplication;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.ProductCenterListingActionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductCenterSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SalesCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterSummaryWebResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.ArrayList;
import java.util.Arrays;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@AutoConfigureMockMvc
@ExtendWith(MockitoExtension.class)
public class ProductCenterControllerTest extends TestHelper {

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private ProductCenterService productCenterService;

  @InjectMocks
  private ProductCenterController productCenterController;

  private static final String CATEGORY_CODE = "CAT_CODE";
  private static final String KEYWORD = "KEYWORD";
  private static final String PRODUCT_SKU = "productSku";

  private ProductCenterSummaryWebRequest productCenterSummaryWebRequest;
  private ProductCenterListingActionWebRequest productCenterListingActionWebRequest;
  private ProductCenterDetailWebResponse productCenterDetailWebResponse;
  private SalesCategoryMappingWebRequest salesCategoryMappingWebRequest;

  @BeforeEach
  public void setUp() {
    mockMvc = MockMvcBuilders.standaloneSetup(productCenterController).build();

    productCenterSummaryWebRequest = new ProductCenterSummaryWebRequest(CATEGORY_CODE, KEYWORD);
    productCenterListingActionWebRequest = new ProductCenterListingActionWebRequest();
    productCenterDetailWebResponse = new ProductCenterDetailWebResponse();
    salesCategoryMappingWebRequest = new SalesCategoryMappingWebRequest(new ArrayList<>(), new ArrayList<>());
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productCenterService);
    verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  public void getProductCenterSummaryTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productCenterService
        .getProductCenterFilterSummary(Constants.STORE_ID, Constants.REQUEST_ID, productCenterSummaryWebRequest, 0,
            100)).thenReturn(
        new PageImpl<ProductCenterSummaryWebResponse>(Arrays.asList(new ProductCenterSummaryWebResponse()),
            PageRequest.of(0, 100), 1));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductCenterApiPath.BASE_PATH + ProductCenterApiPath.PRODUCT_CENTER_SUMMARY_FILTER)
            .param("storeId", Constants.STORE_ID)
            .param("page", String.valueOf(0)).param("size", String.valueOf(100))
            .content(toJson(productCenterSummaryWebRequest)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(productCenterService)
        .getProductCenterFilterSummary(Constants.STORE_ID, Constants.REQUEST_ID, productCenterSummaryWebRequest, 0,
            100);
  }

  @Test
  public void productCenterActionTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    doNothing().when(productCenterService)
        .updateProductCenterListing(Constants.STORE_ID, Constants.REQUEST_ID, productCenterListingActionWebRequest);
    MockHttpServletRequestBuilder requestBuilder =
        put(ProductCenterApiPath.BASE_PATH + ProductCenterApiPath.PRODUCT_CENTER_LISTING_ACTION)
            .content(toJson(productCenterSummaryWebRequest)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).param("storeId", Constants.STORE_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(productCenterService)
        .updateProductCenterListing(Constants.STORE_ID, Constants.REQUEST_ID, productCenterListingActionWebRequest);
  }

  @Test
  public void productCenterHistoryTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productCenterService.getProductCenterHistoryByProductSku(PRODUCT_SKU, 0, 10))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductCenterApiPath.BASE_PATH + ProductCenterApiPath.PRODUCT_CENTER_HISTORY, PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(productCenterService)
        .getProductCenterHistoryByProductSku(PRODUCT_SKU, 0, 10);
  }

  @Test
  public void downloadUnmappedSkusTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    doNothing().when(productCenterService)
        .downloadUnmappedSkus(Constants.STORE_ID, Constants.REQUEST_ID, Constants.USER_NAME, CATEGORY_CODE, "EN");
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductCenterApiPath.BASE_PATH + ProductCenterApiPath.PRODUCT_CENTER_DOWNLOAD_UNMAPPED_SKUS, CATEGORY_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .param("storeId", Constants.STORE_ID).param("language", "EN");
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(productCenterService)
        .downloadUnmappedSkus(Constants.STORE_ID, Constants.REQUEST_ID, Constants.USER_NAME, CATEGORY_CODE, "EN");
  }

  @Test
  public void productCenterDetailTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productCenterService.getProductCenterDetail(Constants.STORE_ID, Constants.REQUEST_ID, PRODUCT_SKU))
        .thenReturn(new ProductCenterDetailWebResponse());

    MockHttpServletRequestBuilder requestBuilder =
        get(ProductCenterApiPath.BASE_PATH + ProductCenterApiPath.PRODUCT_CENTER_DETAIL, PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .param("storeId", Constants.STORE_ID);

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(productCenterService).getProductCenterDetail(Constants.STORE_ID, Constants.REQUEST_ID, PRODUCT_SKU);
  }

  @Test
  public void productCenterDetailUpdateTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    doNothing().when(productCenterService).updateProductCenterDetail(Constants.STORE_ID, Constants.REQUEST_ID,
        PRODUCT_SKU, salesCategoryMappingWebRequest);

    MockHttpServletRequestBuilder requestBuilder =
        put(ProductCenterApiPath.BASE_PATH + ProductCenterApiPath.PRODUCT_CENTER_DETAIL_UPDATE, PRODUCT_SKU)
            .content(toJson(salesCategoryMappingWebRequest)).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).param("storeId", Constants.STORE_ID);

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(productCenterService).updateProductCenterDetail(Constants.STORE_ID, Constants.REQUEST_ID, PRODUCT_SKU,
        salesCategoryMappingWebRequest);
  }
}
