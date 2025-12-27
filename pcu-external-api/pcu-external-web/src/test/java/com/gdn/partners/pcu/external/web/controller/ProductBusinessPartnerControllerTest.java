package com.gdn.partners.pcu.external.web.controller;

import com.gda.mta.product.dto.ProductCopyRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ProductBusinessPartnerApiPath;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.service.PickupPointService;
import com.gdn.partners.pcu.external.service.ProductBusinessPartnerService;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.model.request.CopyProductItemWebRequest;
import com.gdn.partners.pcu.external.web.model.request.DefaultConfigurationAndPickupPointRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointSummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductBusinessPartnerAttributeWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductBusinessPartnerWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemBusinessPartnerWebRequest;
import com.gdn.partners.pcu.external.web.model.response.BusinessPartnerProfileWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CreateProductBusinessPartnerResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

/**
 * Created by govind on 24/12/2018 AD.
 */
public class ProductBusinessPartnerControllerTest extends TestHelper {

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private PickupPointService pickupPointService;

  @InjectMocks
  private ProductBusinessPartnerController productBusinessPartnerController;

  private static final String BUSINESS_PARTNER_ID = "business-partner-id";
  private static final String LINKED_BUSINESS_PARTNER_ID = "linked-business-partner-id";
  private static final String PRODUCT_ID = "product-id";
  private static final String BRAND = "brand";
  private static final String GDN_PRODUCT_SKU = "gdn-product-sku";
  private static final String GDN_PRODUCT_ITEM_SKU = "gdn-product-item-sku";
  private static final String ATTRIBUTE_ID = "attribute-id";
  private static final String ATTR_VALUE = "attr-value";
  private static final String PRODUCT_NAME = "product-name";

  private ProductBusinessPartnerWebRequest productBusinessPartnerWebRequest;

  @Captor
  private ArgumentCaptor<ProductBusinessPartnerServiceRequest> productBusinessPartnerServiceRequest;

  @Captor
  private ArgumentCaptor<ProductCopyRequest> productCopyRequestCaptor;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.productBusinessPartnerController).build();

    productBusinessPartnerWebRequest = new ProductBusinessPartnerWebRequest();
    productBusinessPartnerWebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_ID);
    productBusinessPartnerWebRequest.setBrand(BRAND);
    productBusinessPartnerWebRequest.setProductId(PRODUCT_ID);
    productBusinessPartnerWebRequest.setGdnProductSku(GDN_PRODUCT_SKU);
    productBusinessPartnerWebRequest.setProductName(PRODUCT_NAME);
    ProductItemBusinessPartnerWebRequest productItemBusinessPartnerWebRequest =
        new ProductItemBusinessPartnerWebRequest();
    productItemBusinessPartnerWebRequest.setGdnProductItemSku(GDN_PRODUCT_ITEM_SKU);
    List<ProductItemBusinessPartnerWebRequest> productItemBusinessPartnerWebRequests = new ArrayList<>();
    productItemBusinessPartnerWebRequests.add(productItemBusinessPartnerWebRequest);
    productBusinessPartnerWebRequest.setProductItemBusinessPartners(productItemBusinessPartnerWebRequests);

    ProductBusinessPartnerAttributeWebRequest productBusinessPartnerAttributeWebRequest =
        new ProductBusinessPartnerAttributeWebRequest();
    productBusinessPartnerAttributeWebRequest.setAttributeId(ATTRIBUTE_ID);
    productBusinessPartnerAttributeWebRequest.setValue(ATTR_VALUE);
    List<ProductBusinessPartnerAttributeWebRequest> productBusinessPartnerAttributeWebRequests = new ArrayList<>();
    productBusinessPartnerAttributeWebRequests.add(productBusinessPartnerAttributeWebRequest);
    productBusinessPartnerWebRequest
        .setProductBusinessPartnerAttributes(productBusinessPartnerAttributeWebRequests);

    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_ID);
  }

  @Test
  public void createProductTest() throws Exception {

    SingleBaseResponse<CreateProductBusinessPartnerResponse> response =
        new SingleBaseResponse<>(null, null, true, null, null);
    when(productBusinessPartnerService.create(any(ProductBusinessPartnerServiceRequest.class)))
        .thenReturn(response);

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductBusinessPartnerApiPath.BASE_PATH + ProductBusinessPartnerApiPath.CREATE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(productBusinessPartnerWebRequest));

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));

    verify(productBusinessPartnerService)
        .create(productBusinessPartnerServiceRequest.capture());
    ProductBusinessPartnerServiceRequest productBusinessPartnerServiceRequestModel =
        productBusinessPartnerServiceRequest.getValue();
    Assertions.assertEquals(BRAND, productBusinessPartnerServiceRequestModel.getBrand());
    Assertions.assertEquals(PRODUCT_NAME, productBusinessPartnerServiceRequestModel.getProductName());
    Assertions.assertEquals(PRODUCT_ID, productBusinessPartnerServiceRequestModel.getProductId());
    Assertions.assertEquals(BUSINESS_PARTNER_ID, productBusinessPartnerServiceRequestModel.getBusinessPartnerCode());
    Assertions.assertEquals(GDN_PRODUCT_SKU, productBusinessPartnerServiceRequestModel.getGdnProductSku());
    Assertions.assertEquals(GDN_PRODUCT_ITEM_SKU,
        productBusinessPartnerServiceRequestModel.getProductItemBusinessPartners().get(0)
            .getGdnProductItemSku());
    Assertions.assertEquals(ATTRIBUTE_ID,
        productBusinessPartnerServiceRequestModel.getProductBusinessPartnerAttributes().get(0)
            .getAttributeId());
    Assertions.assertEquals(ATTR_VALUE,
        productBusinessPartnerServiceRequestModel.getProductBusinessPartnerAttributes().get(0)
            .getValue());
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper, times(2)).getRequestId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();

  }

  @Test
  public void getBusinessPartnerProfileByIdTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(productBusinessPartnerService.getBusinessPartnerProfile(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(new BusinessPartnerProfileWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductBusinessPartnerApiPath.BASE_PATH +
            ProductBusinessPartnerApiPath.GET_BUSINESS_PARTNER_PROFILE_BY_ID)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productBusinessPartnerService).getBusinessPartnerProfile(Constants.BUSINESS_PARTNER_CODE);
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void copyProductItemsTest() throws Exception {
    when(productBusinessPartnerService
      .copy(any(ProductCopyRequest.class), eq(false)))
      .thenReturn(new GdnBaseRestResponse(true));

    Map<String, List<String>> itemSKus = new HashMap<>();
    itemSKus.put(GDN_PRODUCT_SKU, Arrays.asList(GDN_PRODUCT_ITEM_SKU));
    CopyProductItemWebRequest webRequest = CopyProductItemWebRequest.builder()
      .itemSkus(itemSKus)
      .businessPartnerCode(BUSINESS_PARTNER_ID)
      .build();

    MockHttpServletRequestBuilder requestBuilder =
      post(ProductBusinessPartnerApiPath.BASE_PATH + ProductBusinessPartnerApiPath.COPY_PRODUCT_ITEMS)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(webRequest));

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));

    verify(productBusinessPartnerService)
      .copy(productCopyRequestCaptor.capture(), eq(false));

    Assertions.assertEquals(BUSINESS_PARTNER_ID, productCopyRequestCaptor.getValue().getBusinessPartnerCode());
    Assertions.assertTrue(productCopyRequestCaptor.getValue().getGdnItemSkus().containsKey(GDN_PRODUCT_SKU));
    Assertions.assertEquals(GDN_PRODUCT_ITEM_SKU,
      productCopyRequestCaptor.getValue().getGdnItemSkus().get(GDN_PRODUCT_SKU).get(0));
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getLinkedBusinessPartnerCode();
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void retryCopyProductItemsTest() throws Exception {
    when(productBusinessPartnerService
      .copy(any(ProductCopyRequest.class), eq(true)))
      .thenReturn(new GdnBaseRestResponse(true));

    Map<String, List<String>> itemSKus = new HashMap<>();
    itemSKus.put(GDN_PRODUCT_SKU, Arrays.asList(GDN_PRODUCT_ITEM_SKU));
    CopyProductItemWebRequest webRequest = CopyProductItemWebRequest.builder()
      .itemSkus(itemSKus)
      .businessPartnerCode(BUSINESS_PARTNER_ID)
      .build();

    MockHttpServletRequestBuilder requestBuilder =
      post(ProductBusinessPartnerApiPath.BASE_PATH + ProductBusinessPartnerApiPath.RETRY_COPY_PRODUCT_ITEMS)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(webRequest));

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));

    verify(productBusinessPartnerService)
      .copy(productCopyRequestCaptor.capture(), eq(true));

    Assertions.assertEquals(BUSINESS_PARTNER_ID, productCopyRequestCaptor.getValue().getBusinessPartnerCode());
    Assertions.assertTrue(productCopyRequestCaptor.getValue().getGdnItemSkus().containsKey(GDN_PRODUCT_SKU));
    Assertions.assertEquals(GDN_PRODUCT_ITEM_SKU,
      productCopyRequestCaptor.getValue().getGdnItemSkus().get(GDN_PRODUCT_SKU).get(0));
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getLinkedBusinessPartnerCode();
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void copyAllProductItemsTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_ID);
    when(mandatoryParameterHelper.getLinkedBusinessPartnerCode()).thenReturn(LINKED_BUSINESS_PARTNER_ID);

    when(productBusinessPartnerService
      .copyAll(any(ProductCopyRequest.class)))
      .thenReturn(new GdnBaseRestResponse(true));

    CopyProductItemWebRequest webRequest = CopyProductItemWebRequest.builder()
      .build();

    MockHttpServletRequestBuilder requestBuilder =
      post(ProductBusinessPartnerApiPath.BASE_PATH + ProductBusinessPartnerApiPath.COPY_ALL_PRODUCT_ITEMS)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(webRequest));

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));

    verify(productBusinessPartnerService)
      .copyAll(productCopyRequestCaptor.capture());

    Assertions.assertEquals(BUSINESS_PARTNER_ID, productCopyRequestCaptor.getValue().getBusinessPartnerCode());
    Assertions.assertEquals(LINKED_BUSINESS_PARTNER_ID, productCopyRequestCaptor.getValue().getSourceBusinessPartnerCode());
    Assertions.assertTrue(productCopyRequestCaptor.getValue().getGdnItemSkus().isEmpty());
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getLinkedBusinessPartnerCode();
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void productsAvailableToBeCopiedTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_ID);
    when(mandatoryParameterHelper.getLinkedBusinessPartnerCode()).thenReturn(LINKED_BUSINESS_PARTNER_ID);

    when(productBusinessPartnerService
      .productsAvailableToDirectCopy(eq(BUSINESS_PARTNER_ID), eq(LINKED_BUSINESS_PARTNER_ID), eq(0), eq(10), any(
        ProductLevel3SummaryRequest.class)))
      .thenReturn(new GdnRestListResponse<>(null, null, true, null));

    MockHttpServletRequestBuilder requestBuilder =
      post(ProductBusinessPartnerApiPath.BASE_PATH + ProductBusinessPartnerApiPath.PRODUCT_ITEMS_TO_COPY)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .accept(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(new ProductLevel3SummaryRequest()));

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));


    verify(productBusinessPartnerService)
      .productsAvailableToDirectCopy(eq(BUSINESS_PARTNER_ID), eq(LINKED_BUSINESS_PARTNER_ID), eq(0), eq(10), any(
        ProductLevel3SummaryRequest.class));
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getLinkedBusinessPartnerCode();
  }

  @Test
  public void productsAvailableToBeCopiedTest_withCustomPageDetails() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_ID);
    when(mandatoryParameterHelper.getLinkedBusinessPartnerCode()).thenReturn(LINKED_BUSINESS_PARTNER_ID);

    when(productBusinessPartnerService
      .productsAvailableToDirectCopy(eq(BUSINESS_PARTNER_ID), eq(LINKED_BUSINESS_PARTNER_ID), eq(3), eq(20), any(
        ProductLevel3SummaryRequest.class)))
      .thenReturn(new GdnRestListResponse<>(null, null, true, null));

    MockHttpServletRequestBuilder requestBuilder =
      post(ProductBusinessPartnerApiPath.BASE_PATH + ProductBusinessPartnerApiPath.PRODUCT_ITEMS_TO_COPY)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .accept(MediaType.APPLICATION_JSON_VALUE)
        .param("page", "3")
        .param("size", "20")
        .content(toJson(new ProductLevel3SummaryRequest()));

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));


    verify(productBusinessPartnerService)
      .productsAvailableToDirectCopy(eq(BUSINESS_PARTNER_ID), eq(LINKED_BUSINESS_PARTNER_ID), eq(3), eq(20), any(
        ProductLevel3SummaryRequest.class));
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getLinkedBusinessPartnerCode();
  }

  @Test
  public void isProductsMappedToBusinessPartnerCodeTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(productBusinessPartnerService.isProductMappedToMerchant(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(true);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductBusinessPartnerApiPath.BASE_PATH +
            ProductBusinessPartnerApiPath.IS_PRODUCT_MAPPED_TO_MERCHANT)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productBusinessPartnerService).isProductMappedToMerchant(Constants.BUSINESS_PARTNER_CODE);
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void fetchPickupPointDetailSummaryTest() throws Exception {
    PickupPointSummaryWebRequest pickupPointSummaryWebRequest = new PickupPointSummaryWebRequest();
    pickupPointSummaryWebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_ID);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_ID);
    when(pickupPointService.getPickupPointSummaryFilter(Constants.STORE_ID, 0, 1,
      pickupPointSummaryWebRequest)).thenReturn(new PageImpl<>(new ArrayList<>()));
    MockHttpServletRequestBuilder requestBuilder = post(ProductBusinessPartnerApiPath.BASE_PATH
        + ProductBusinessPartnerApiPath.FETCH_PICKUP_POINT_DETAIL_SUMMARY).param("page", "0").param("size", "1")
        .contentType(MediaType.APPLICATION_JSON).content(toJson(pickupPointSummaryWebRequest))
        .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(pickupPointService).getPickupPointSummaryFilter(Constants.STORE_ID, 0, 1,
      pickupPointSummaryWebRequest);
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void saveDefaultPickupPointsAndConfigurationsTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    MockHttpServletRequestBuilder requestBuilder = post(ProductBusinessPartnerApiPath.BASE_PATH
        + ProductBusinessPartnerApiPath.SAVE_DEFAULT_PICKUP_POINT_AND_CONFIG, Constants.BUSINESS_PARTNER_CODE)
        .contentType(MediaType.APPLICATION_JSON).content(toJson(new DefaultConfigurationAndPickupPointRequest()))
        .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productBusinessPartnerService).updateDefaultConfigurationsAndPickupPoints(Constants.STORE_ID, Constants.BUSINESS_PARTNER_CODE, new DefaultConfigurationAndPickupPointRequest());
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getRequestId();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productBusinessPartnerService);
    verifyNoMoreInteractions(mandatoryParameterHelper);
  }

}
