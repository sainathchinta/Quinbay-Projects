package com.gdn.partners.pcu.external.web.controller;

import com.gda.mta.product.dto.response.ProductCountResponse;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.tuple.Pair;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.BulkProcessResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.client.model.OmniChannelSkuWebRequest;
import com.gdn.partners.pcu.external.client.model.ValidOmniChannelSkuWebResponse;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ProductV2ApiPath;
import com.gdn.partners.pcu.external.service.ProductV2Service;
import com.gdn.partners.pcu.external.service.ProductWrapperV2Service;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointListingL3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ListingUpdateV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.QuickEditV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.WarehouseStockDetailsRequest;
import com.gdn.partners.pcu.external.web.model.request.WholesaleStatusV2Request;
import com.gdn.partners.pcu.external.web.model.response.BulkProcessStatusListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ConsignmentDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.EditProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InventorySummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemCodeBasicDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3CountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3V2WebResponse;
import com.gdn.partners.pcu.external.web.model.response.VariantsErrorListWebResponse;
import com.gdn.partners.pcu.external.web.model.response.WarehouseStockDetailsWebResponse;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class ProductV2ControllerTest extends TestHelper {

  private static final String PRODUCT_SKU = "productSku";

  private QuickEditV2WebRequest quickEditV2WebRequest = new QuickEditV2WebRequest();
  private ListingUpdateV2WebRequest listingUpdateV2WebRequest = new ListingUpdateV2WebRequest();

  private static final Integer PAGE = 0;
  private static final Integer SIZE = 50;
  public static final String ITEM_SKU = "ITEM_SKU";
  public static final String STORE_ID = "10001";
  public static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  public static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String GDN_PRODUCT_SKU = "gdn-product-sku";
  private static final String ERROR_CODE = "error-code";
  private static final String PRE_LIVE_REVIEW_TYPE = "prelive";
  private static final String PRODUCT_CODE = "product-code";
  private static final String BRAND = "brand";
  private static final String CATEGORY_CODE = "category-code";
  private static final String DESCRIPTION = "description";
  private static final String USP = "usp";
  private ProductSummaryV2WebRequest productSummaryV2WebRequest = new ProductSummaryV2WebRequest();
  private ProductEditInfoV2WebRequest productEditInfoV2WebRequest;
  private EditProductWebResponse editProductWebResponse;
  private static final Pageable pageable = PageRequest.of(0, 100);
  private static final String REQUEST_ID = "REQ-001";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANNEL_ID = "channelId";
  private static final String USERNAME = "username";
  private static final String BULK_PROCESS_TYPE = "ProductLevel3";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String ITEM_CODE = "itemCode";
  private static final String WAREHOUSE_ITEM_SKU = "warehouse item sku";


  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private ProductV2Service productV2Service;

  @Mock
  private ProductWrapperV2Service productWrapperV2Service;

  @InjectMocks
  private ProductV2Controller productV2Controller;

  @Captor
  private ArgumentCaptor<ProductEditInfoV2WebRequest> productEditInfoWebRequestArgumentCaptor;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(productV2Controller).build();
    listingUpdateV2WebRequest.setQuickEditRequests(Collections.singletonList(quickEditV2WebRequest));
    productSummaryV2WebRequest.setMerchantCode(Constants.BUSINESS_PARTNER_CODE);

    productEditInfoV2WebRequest = new ProductEditInfoV2WebRequest();
    productEditInfoV2WebRequest.setProductCode(PRODUCT_CODE);
    productEditInfoV2WebRequest.setProductSku(BUSINESS_PARTNER_CODE);
    productEditInfoV2WebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productEditInfoV2WebRequest.setBrand(BRAND);
    productEditInfoV2WebRequest.setCategoryCode(CATEGORY_CODE);
    productEditInfoV2WebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productEditInfoV2WebRequest.setDescription(DESCRIPTION);

    editProductWebResponse =
      EditProductWebResponse.builder().productReview(true).reviewType(PRE_LIVE_REVIEW_TYPE).build();
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(mandatoryParameterHelper);
    Mockito.verifyNoMoreInteractions(productV2Service);
  }

  @Test
  public void updateItemListingTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
      put(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.LISTING_UPDATE, PRODUCT_SKU).accept(
          MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(listingUpdateV2WebRequest)).param("storeId", Constants.STORE_ID)
        .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
        .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(productWrapperV2Service).updateItemListing(PRODUCT_SKU,
      Collections.singletonList(quickEditV2WebRequest));
  }

  @Test
  public void getProductCountsTest() throws Exception {
    when(productV2Service.getL3CountsByType(Constants.BUSINESS_PARTNER_CODE, Constants.INACTIVE_STATUS))
        .thenReturn(new ProductL3CountWebResponse());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_SECONDARY_COUNTS).param("type", Constants.INACTIVE_STATUS)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productV2Service).getL3CountsByType(Constants.BUSINESS_PARTNER_CODE, Constants.INACTIVE_STATUS);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getFilterSummaryL3Test() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(productWrapperV2Service.getProductL3Listing(productSummaryV2WebRequest, PAGE,
      SIZE, true)).thenReturn(new PageImpl<>(Collections.emptyList(), PageRequest.of(PAGE, SIZE), SIZE));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.L3_LISTING).accept(
                MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(productSummaryV2WebRequest)).param("storeId", Constants.STORE_ID)
            .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
            .param("requestId", Constants.REQUEST_ID).param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE));
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productWrapperV2Service).getProductL3Listing(productSummaryV2WebRequest, PAGE, SIZE, true);
  }

  @Test
  public void getPrimaryProductCountsTest() throws Exception {
    when(productV2Service.getL3PrimaryCountsByMerchantCode(
      Constants.BUSINESS_PARTNER_CODE)).thenReturn(new ProductL3CountWebResponse());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    MockHttpServletRequestBuilder requestBuilder =
      get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRIMARY_COUNTS).accept(
        MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productV2Service).getL3PrimaryCountsByMerchantCode(Constants.BUSINESS_PARTNER_CODE);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getWholesalePromoStatusTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(
      productV2Service.getWholesaleStatusByRequest(eq(Constants.STORE_ID), eq(Constants.REQUEST_ID),
        Mockito.any(WholesaleStatusV2Request.class))).thenReturn(new ArrayList<>());
    MockHttpServletRequestBuilder requestBuilder =
      post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.WHOLESALE_STATUS).accept(
          MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(WholesaleStatusV2Request.builder().build())).param("storeId", Constants.STORE_ID)
        .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
        .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getStoreId();
    verify(productV2Service).getWholesaleStatusByRequest(eq(Constants.STORE_ID),
      eq(Constants.REQUEST_ID), Mockito.any(WholesaleStatusV2Request.class));
  }

  @Test
  public void getInventorySummaryTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternal()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    when(productV2Service.getInventorySummary(ITEM_SKU, true, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE))
        .thenReturn(new InventorySummaryWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_INVENTORY_SUMMARY, ITEM_SKU, PICKUP_POINT_CODE)
            .param("isWareHouse", String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productV2Service).getInventorySummary(ITEM_SKU, true, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE);
  }

  @Test
  public void editProductV2Info() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    productEditInfoV2WebRequest.setUniqueSellingPoint(USP);
    when(productV2Service.editProductV2Info(eq(productEditInfoV2WebRequest),
      eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean()))
      .thenReturn(editProductWebResponse);

    MockHttpServletRequestBuilder requestBuilder =
      put(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.EDIT_PRODUCT_INFO, GDN_PRODUCT_SKU)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(productEditInfoV2WebRequest)).param("storeId", Constants.STORE_ID)
        .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
        .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).isExternalOnly();
    verify(productV2Service)
      .editProductV2Info(productEditInfoWebRequestArgumentCaptor.capture(), eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean());
  }

  @Test
  public void editProductInfoWithErrorCodeTest() throws Exception {
    editProductWebResponse.setApiErrorCode(ERROR_CODE);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    when(productV2Service.editProductV2Info(eq(productEditInfoV2WebRequest),
      eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean()))
      .thenReturn(editProductWebResponse);

    MockHttpServletRequestBuilder requestBuilder =
      put(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.EDIT_PRODUCT_INFO, GDN_PRODUCT_SKU)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(productEditInfoV2WebRequest)).param("storeId", Constants.STORE_ID)
        .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
        .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).isExternalOnly();
    verify(productV2Service)
      .editProductV2Info(productEditInfoWebRequestArgumentCaptor.capture(),
        eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean());
  }

  @Test
  public void getL3ProductDetailsByProductSkuTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(productV2Service
      .fetchL3DetailsByProductSku(Constants.STORE_ID, PRODUCT_SKU, BUSINESS_PARTNER_CODE, false))
      .thenReturn(new ProductLevel3V2WebResponse());
    MockHttpServletRequestBuilder requestBuilder =
      get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.FETCH_L3_DETAILS, PRODUCT_SKU)
        .param("isNeedCorrection", "false").accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getStoreId();
    verify(productV2Service)
      .fetchL3DetailsByProductSku(Constants.STORE_ID, PRODUCT_SKU, BUSINESS_PARTNER_CODE, false);
  }

  @Test
  public void getProductItemPickupPointsByProductSku() throws Exception {
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest =
      ItemPickupPointListingL3WebRequest.builder().build();

    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(productV2Service.getItemPickupPointListingByProductSku(eq(PAGE), eq(SIZE),
      eq(PRODUCT_SKU), Mockito.any(ItemPickupPointListingL3WebRequest.class))).thenReturn(
      new PageImpl<>(new ArrayList<>(), pageable, SIZE));

    MockHttpServletRequestBuilder requestBuilder =
      post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.FETCH_L5_DETAILS, PRODUCT_SKU).accept(
        MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(itemPickupPointListingL3WebRequest)).param("storeId", Constants.STORE_ID)
        .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
        .param("requestId", Constants.REQUEST_ID).param("page", String.valueOf(PAGE))
        .param("size", String.valueOf(SIZE));
    mockMvc.perform(requestBuilder).andExpect(status().isOk());

    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productV2Service).getItemPickupPointListingByProductSku(eq(PAGE),
      eq(SIZE),
      eq(PRODUCT_SKU), Mockito.any(ItemPickupPointListingL3WebRequest.class));
  }

  @Test
  public void editProductV2InfoNullResponseTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    productEditInfoV2WebRequest.setUniqueSellingPoint(USP);
    editProductWebResponse.setApiErrorCode("UNDEFINED");
    editProductWebResponse.setVariantsErrorList(new ArrayList<>(1));
    when(productV2Service.editProductV2Info(eq(productEditInfoV2WebRequest),
      eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean()))
      .thenReturn(editProductWebResponse);

    MockHttpServletRequestBuilder requestBuilder =
      put(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.EDIT_PRODUCT_INFO, GDN_PRODUCT_SKU)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(productEditInfoV2WebRequest)).param("storeId", Constants.STORE_ID)
        .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
        .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).isExternalOnly();
    verify(productV2Service)
      .editProductV2Info(productEditInfoWebRequestArgumentCaptor.capture(), eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean());
  }

  @Test
  public void editProductV2InfoEmptyResponseTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(Constants.IS_EXTERNAL_ONLY);
    productEditInfoV2WebRequest.setUniqueSellingPoint(USP);
    List<VariantsErrorListWebResponse> list = new ArrayList<>();
    VariantsErrorListWebResponse variantsErrorListWebResponse = new VariantsErrorListWebResponse();
    variantsErrorListWebResponse.setPickupPointId("PP-Code");
    list.add(variantsErrorListWebResponse);
    editProductWebResponse.setVariantsErrorList(list);
    when(productV2Service.editProductV2Info(Mockito.any(ProductEditInfoV2WebRequest.class),
      Mockito.anyString(), Mockito.anyBoolean()))
      .thenReturn(editProductWebResponse);

    MockHttpServletRequestBuilder requestBuilder =
      put(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.EDIT_PRODUCT_INFO, GDN_PRODUCT_SKU)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .content(toJson(productEditInfoV2WebRequest)).param("storeId", Constants.STORE_ID)
        .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
        .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).isExternalOnly();
    verify(productV2Service)
      .editProductV2Info(productEditInfoWebRequestArgumentCaptor.capture(), eq(BUSINESS_PARTNER_CODE), Mockito.anyBoolean());
  }

  @Test
  public void fetchBulkProcessListingResponseTest() throws Exception {
    List<BulkProcessStatusListingWebResponse> expectedWebResponseList = new ArrayList<>();
    expectedWebResponseList.add(
      BulkProcessStatusListingWebResponse.builder().bulkProcessCode(BUSINESS_PARTNER_CODE).build());
    Page<BulkProcessStatusListingWebResponse> webResponsePage = new PageImpl<>(expectedWebResponseList);

    MockHttpServletRequestBuilder requestBuilder =
      get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.BULK_PROCESS_LISTING,
        BULK_PROCESS_TYPE).param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE)).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    Mockito.when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    Mockito.when(productV2Service.fetchBulkProcessListingResponse(Constants.STORE_ID, Constants.REQUEST_ID,
      BUSINESS_PARTNER_CODE, BULK_PROCESS_TYPE, Optional.empty(), false, PAGE, SIZE)).thenReturn(webResponsePage);

    mockMvc.perform(requestBuilder).andExpect(status().isOk());

    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    Mockito.verify(mandatoryParameterHelper).getStoreId();
    Mockito.verify(productV2Service)
      .fetchBulkProcessListingResponse(Constants.STORE_ID, Constants.REQUEST_ID,
        BUSINESS_PARTNER_CODE,
        BULK_PROCESS_TYPE, Optional.empty(), false, PAGE, SIZE);
  }
  @Test
  public void fetchConsignmentDetailsByItemSkuTest() throws Exception {
    String businessPartnerCode = Constants.BUSINESS_PARTNER_CODE;
    String requestId = Constants.REQUEST_ID;
    String storeId = Constants.STORE_ID;
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(businessPartnerCode);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(requestId);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(storeId);
    List<ConsignmentDetailWebResponse> consignmentDetails = Collections.emptyList();
    Page<ConsignmentDetailWebResponse> page = new PageImpl<>(consignmentDetails);
    Pair<List<ConsignmentDetailWebResponse>, Integer>  consignmentResponse =
      Pair.of(consignmentDetails,1);
    when(productV2Service.fetchConsignmentDetailResponse(storeId, businessPartnerCode, ITEM_SKU, PAGE,
      SIZE))
      .thenReturn(consignmentResponse);
    mockMvc.perform(get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.FETCH_CONSIGNMENT_DETAIL_BY_ITEM_SKU, ITEM_SKU)
        .param("page", String.valueOf(PAGE))
        .param("size", String.valueOf(SIZE))
        .accept(MediaType.APPLICATION_JSON_VALUE))
      .andExpect(status().isOk());
    verify(mandatoryParameterHelper, times(1)).getBusinessPartnerCode();
    verify(mandatoryParameterHelper, times(1)).getRequestId();
    verify(mandatoryParameterHelper, times(1)).getStoreId();
    verify(productV2Service, times(1)).fetchConsignmentDetailResponse(storeId,
      businessPartnerCode, ITEM_SKU, PAGE, SIZE);
  }

  @Test
  public void getProductCountForProductLimitTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(productV2Service.getProductCountForProductLimit(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(new ProductCountResponse(20L));
    mockMvc.perform(get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.PRODUCT_COUNT)).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getStoreId();
    verify(productV2Service).getProductCountForProductLimit(STORE_ID,BUSINESS_PARTNER_CODE);
  }

  @Test
  public void fetchBasicItemDetailsByItemCodesTest() throws Exception {
    Page<ItemCodeBasicDetailWebResponse> itemCodeBasicDetailWebResponses =
        new PageImpl<>(Collections.singletonList(new ItemCodeBasicDetailWebResponse()), pageable, SIZE);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(USERNAME);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(productV2Service.fetchBasicItemDetailsByItemCodes(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, StringUtils.EMPTY,
        PAGE, SIZE)).thenReturn(itemCodeBasicDetailWebResponses);
    mockMvc.perform(get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.FETCH_BASIC_ITEM_DETAILS_BY_ITEM_CODE, ITEM_SKU).param("page",
        String.valueOf(PAGE)).param("size", String.valueOf(SIZE)).accept(MediaType.APPLICATION_JSON_VALUE)).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getStoreId();
    verify(productV2Service).fetchBasicItemDetailsByItemCodes(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, StringUtils.EMPTY, PAGE, SIZE);
  }

  @Test
  public void fetchStockDetailsByWarehouseItemSkuTest() throws Exception {
    WarehouseStockDetailsRequest warehouseStockDetailsRequest = new WarehouseStockDetailsRequest();
    List<String> itemCodes = new ArrayList<String>();
    itemCodes.add(WAREHOUSE_ITEM_SKU);
    warehouseStockDetailsRequest.setItemCodes(itemCodes);
    List<WarehouseStockDetailsWebResponse> warehouseStockDetailsWebResponse =
      List.of(WarehouseStockDetailsWebResponse.builder().warehouseStockAvailable(true)
        .pendingInBounds(false).itemCode(ITEM_CODE).build());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(productV2Service.fetchWarehouseStockStatusByItemCode(itemCodes)).thenReturn(warehouseStockDetailsWebResponse);
    mockMvc.perform(post(ProductV2ApiPath.BASE_PATH
        + ProductV2ApiPath.FETCH_STOCK_DETAILS_BY_WAREHOUSE_ITEM_SKU).contentType(MediaType.APPLICATION_JSON_VALUE).content(
        toJson(warehouseStockDetailsRequest)).accept(MediaType.APPLICATION_JSON_VALUE))
      .andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(productV2Service).fetchWarehouseStockStatusByItemCode(itemCodes);
  }

  @Test
  public void getBulkProcessByProcessCodeTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(productV2Service.getBulkProcessResponse(BULK_PROCESS_CODE)).thenReturn(new BulkProcessResponse());
    mockMvc.perform(get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.BULK_PROCESS_NOTES, BULK_PROCESS_CODE)).andExpect(status().isOk());
    Mockito.verify(productV2Service).getBulkProcessResponse(BULK_PROCESS_CODE);
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void checkOmniChannelSkuExistsInSellerTest() throws Exception {
    OmniChannelSkuWebRequest omniChannelSkuWebRequest = new OmniChannelSkuWebRequest();
    omniChannelSkuWebRequest.setSellerCode(WAREHOUSE_ITEM_SKU);
    omniChannelSkuWebRequest.setOmniChannelSkus(List.of(WAREHOUSE_ITEM_SKU));
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(REQUEST_ID);
    when(productV2Service.checkOmniChannelSkuExistsInSeller(
      any(OmniChannelSkuWebRequest.class))).thenReturn(new ValidOmniChannelSkuWebResponse());
    mockMvc.perform(
      post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.CHECK_IF_SELLER_SKU_EXISTS).contentType(
          MediaType.APPLICATION_JSON_VALUE).content(toJson(omniChannelSkuWebRequest))
        .accept(MediaType.APPLICATION_JSON_VALUE)).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(productV2Service).checkOmniChannelSkuExistsInSeller(any(OmniChannelSkuWebRequest.class));
  }

  @Test
  public void checkOmniChannelSkuExistsInSeller_NullRequestBody_ShouldThrowException() {
    OmniChannelSkuWebRequest omniChannelSkuWebRequest = null;
    try {
      mockMvc.perform(
        post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.CHECK_IF_SELLER_SKU_EXISTS).contentType(
            MediaType.APPLICATION_JSON_VALUE).content(toJson(omniChannelSkuWebRequest))
          .accept(MediaType.APPLICATION_JSON_VALUE)).andExpect(status().isBadRequest());
    } catch (Exception e) {
      Throwable cause = e.getCause();
      assertNotNull(cause);
      assertTrue(cause instanceof ApplicationRuntimeException);
      assertEquals("Can not process invalid input data : Omni channel sku list cannot be empty",
        cause.getMessage());
    }
  }


  @Test
  public void checkOmniChannelSkuExistsInSellerListEmptyTest() throws Exception {
    OmniChannelSkuWebRequest omniChannelSkuWebRequest = new OmniChannelSkuWebRequest();
    omniChannelSkuWebRequest.setOmniChannelSkus(Collections.emptyList());

    try {
      mockMvc.perform(
        post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.CHECK_IF_SELLER_SKU_EXISTS).contentType(
            MediaType.APPLICATION_JSON_VALUE).content(toJson(omniChannelSkuWebRequest))
          .accept(MediaType.APPLICATION_JSON_VALUE)).andExpect(status().isBadRequest());
    } catch (Exception e) {
      Throwable cause = e.getCause();
      assertNotNull(cause);
      assertTrue(cause instanceof ApplicationRuntimeException);
      assertEquals("Can not process invalid input data : Omni channel sku list cannot be empty",
        cause.getMessage());
    }
  }

}
