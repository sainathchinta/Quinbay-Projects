package com.gdn.partners.pbp.controller.offlineitem;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.gdn.partners.pbp.dto.offlineitem.OfflineItemResponseDetailResponse;
import com.gdn.x.base.controller.GlobalControllerAdvice;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.converter.offlineitem.OfflineItemRequestConverter;
import com.gdn.mta.product.converter.offlineitem.OfflineItemResponseConverter;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.dto.common.ListRequestDTO;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.FailedOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.SuccessOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.ListRequest;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemAttributeRequest;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupBulkDownloadResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupRequest;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupV2Request;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupV2Response;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemRequest;
import com.gdn.partners.pbp.model.offlineitem.DeleteOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.OfflineItemDetail;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;
import com.gdn.partners.pbp.service.offlineitem.OfflineItemService;
import com.gdn.partners.pbp.web.model.OfflineItemControllerPath;

public class OfflineItemControllerTest {

  @InjectMocks
  private OfflineItemController offlineItemController;

  @Mock
  private OfflineItemService offlineItemService;

  @Mock
  private OfflineItemResponseConverter responseConverter;

  @Mock
  private OfflineItemRequestConverter requestConverter;

  private ListRequestDTO<OfflineItemAttributeRequest> successRequest;

  private ObjectMapper objectMapper = new ObjectMapper();
  private MockMvc mockMvc;
  private List<SuccessOfflineItemResponse> successOfflineItemResponses;
  private List<FailedOfflineItemResponse> failedOfflineItemResponses;

  private List<OfflineItemDetail> offlineItemDetails;
  private List<OfflineItemDetailResponse> offlineItemDetailResponses;

  private List<DeleteOfflineItemRequest> deleteOfflineItemRequests;
  private DeleteOfflineItemRequest deleteOfflineItemRequest;
  private DeleteOfflineItemResponse deleteOfflineItemResponse;
  private List<DeleteOfflineItem> deleteOfflineItems;
  private DeleteOfflineItem deleteOfflineItem;
  private ListRequest<DeleteOfflineItemRequest> deleteOfflineItemRequestListRequest;
  private UpsertOfflineItemRequest upsertOfflineItemRequest;
  private List<UpsertOfflineItemRequest> upsertOfflineItemRequests;
  private ListRequestDTO<UpsertOfflineItemRequest> upsertOfflineItemListRequest;
  private UpsertOfflineItem upsertOfflineItem;
  private List<UpsertOfflineItem> upsertOfflineItems;

  private UpsertOfflineItemFailedResponse upsertOfflineItemFailedResponse;
  private List<UpsertOfflineItemFailedResponse> upsertOfflineItemFailedResponses;
  private UpsertOfflineItemDetailResponse upsertOfflineItemDetailResponse;
  private List<UpsertOfflineItemDetailResponse> upsertOfflineItemDetailResponses;

  private Pair<List<UpsertOfflineItemDetailResponse>, List<UpsertOfflineItemFailedResponse>> pairOfUpsertOfflineItemErrorResponse;

  private List<DeleteOfflineItemDetailResponse> deleteOfflineItemSuccessProducts;
  private List<DeleteOfflineItemDetailResponse> deleteOfflineItemFailedProducts;
  private Pair<List<DeleteOfflineItemDetailResponse>, List<DeleteOfflineItemDetailResponse>> successAndFailedDeleteOfflineItem;

  private OfflineItemResponseDetail offlineItemResponseDetail;
  private List<OfflineItemResponseDetail> offlineItemResponseDetails;
  private OfflineItemResponseDetailResponse offlineItemResponseDetailResponse;

  private static final String MERCHANT_CODE = "merchant-code";
  private static final String MERCHANT_SKU = "merchant-sku";
  private static final List<String> MERCHANT_SKUS = Collections.singletonList(MERCHANT_SKU);
  private static final String STORE_ID = "10001";
  private static final String CHANNEL_ID = "mta-api";
  private static final String CLIENT_ID = "mta-api";
  private static final String REQUEST_ID = "a3df-21q63-e5r4t-3ad1-fga5-ds4f-3fad1";
  private static final String USERNAME = "username@mail.com";
  private static final int SAFETY_STOCK = 2;
  private static final String FILE_NAME = "file-name";
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 10;
  private static final String ORDER_BY = "orderBy";
  private static final String SORT_BY = "sortBy";
  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final List<String> PICKUP_POINT_CODES =
      Collections.singletonList(PICKUP_POINT_CODE);
  private static final String ITEM_NAME = "item-name";
  private static final String CATEGORY_CODE = "category-code";
  private static final String GDN_SKU = "gdn-sku";
  private static final List<String> GDN_SKUS = Collections.singletonList(GDN_SKU);
  private static final String BUSINESS_PARTNER_CODE = "business-partner-code";
  private static final String ITEM_SKU = "item-sku";
  private static final String ERROR_MESSAGE = "error-message";
  private static final double LIST_PRICE = 200000D;
  private static final double OFFER_PRICE = 100000D;
  private static final int STOCK = 5;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    this.mockMvc = MockMvcBuilders.standaloneSetup(this.offlineItemController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter())
            .setControllerAdvice(new GlobalControllerAdvice()).build();

    successRequest = new ListRequestDTO<>();

    OfflineItemAttributeRequest offlineItemAttributeRequest =
        new OfflineItemAttributeRequest();
    offlineItemAttributeRequest.setMerchantSku(MERCHANT_SKU);

    List<OfflineItemAttributeRequest> offlineItemAttributeRequests = new ArrayList();
    offlineItemAttributeRequests.add(offlineItemAttributeRequest);

    successRequest.setList(offlineItemAttributeRequests);

    successOfflineItemResponses = new ArrayList<>();
    SuccessOfflineItemResponse successOfflineItemResponse = new SuccessOfflineItemResponse();
    successOfflineItemResponses.add(successOfflineItemResponse);

    failedOfflineItemResponses = new ArrayList();
    FailedOfflineItemResponse failedOfflineItemResponse = new FailedOfflineItemResponse();
    failedOfflineItemResponses.add(failedOfflineItemResponse);

    this.offlineItemDetails = new ArrayList<>();
    this.offlineItemDetailResponses = new ArrayList<>();

    this.deleteOfflineItemRequests = new ArrayList<>();
    this.deleteOfflineItemRequestListRequest = new ListRequest<>();
    this.deleteOfflineItemRequest = new DeleteOfflineItemRequest();
    this.deleteOfflineItemRequest.setItemSku(ITEM_SKU);
    this.deleteOfflineItemRequest.setMerchantSku(MERCHANT_SKU);
    this.deleteOfflineItemRequest.setPickupPointCode(PICKUP_POINT_CODE);
    this.deleteOfflineItemRequests.add(this.deleteOfflineItemRequest);
    this.deleteOfflineItemRequestListRequest.setList(this.deleteOfflineItemRequests);

    this.deleteOfflineItems = new ArrayList<>();
    this.deleteOfflineItem = new DeleteOfflineItem();
    this.deleteOfflineItem.setItemSku(ITEM_SKU);
    this.deleteOfflineItem.setMerchantSku(MERCHANT_SKU);
    this.deleteOfflineItem.setPickupPointCode(PICKUP_POINT_CODE);
    this.deleteOfflineItems.add(this.deleteOfflineItem);

    this.upsertOfflineItemRequest = new UpsertOfflineItemRequest();
    this.upsertOfflineItemRequest.setFileName(FILE_NAME);
    this.upsertOfflineItemRequest.setItemSku(ITEM_SKU);
    this.upsertOfflineItemRequest.setOfferPrice(OFFER_PRICE);
    this.upsertOfflineItemRequest.setPickupPointCode(PICKUP_POINT_CODE);
    this.upsertOfflineItemRequest.setStock(STOCK);

    this.upsertOfflineItemRequests = new ArrayList<>();
    this.upsertOfflineItemRequests.add(this.upsertOfflineItemRequest);

    this.upsertOfflineItemListRequest = new ListRequestDTO<>();
    this.upsertOfflineItemListRequest.setList(this.upsertOfflineItemRequests);

    this.upsertOfflineItem = new UpsertOfflineItem();
    this.upsertOfflineItem.setFileName(FILE_NAME);
    this.upsertOfflineItem.setItemSku(ITEM_SKU);
    this.upsertOfflineItem.setListPrice(LIST_PRICE);
    this.upsertOfflineItem.setOfferPrice(OFFER_PRICE);
    this.upsertOfflineItem.setPickupPointCode(PICKUP_POINT_CODE);
    this.upsertOfflineItem.setStock(STOCK);

    this.upsertOfflineItems = new ArrayList<>();
    this.upsertOfflineItems.add(this.upsertOfflineItem);

    this.upsertOfflineItemDetailResponse = new UpsertOfflineItemDetailResponse();
    this.upsertOfflineItemDetailResponse.setItemSku(ITEM_SKU);
    this.upsertOfflineItemDetailResponse.setListPrice(LIST_PRICE);
    this.upsertOfflineItemDetailResponse.setPrice(OFFER_PRICE);
    this.upsertOfflineItemDetailResponse.setPickupPointCode(PICKUP_POINT_CODE);
    this.upsertOfflineItemDetailResponse.setStock(STOCK);

    this.upsertOfflineItemDetailResponses = new ArrayList<>();
    this.upsertOfflineItemDetailResponses.add(upsertOfflineItemDetailResponse);

    this.upsertOfflineItemFailedResponse = new UpsertOfflineItemFailedResponse();
    
    this.upsertOfflineItemFailedResponses = new ArrayList<>();

    DeleteOfflineItemDetailResponse deleteSuccessProduct = new DeleteOfflineItemDetailResponse();
    deleteSuccessProduct.setItemSku(ITEM_SKU);
    deleteSuccessProduct.setPickupPointCode(PICKUP_POINT_CODE);
    DeleteOfflineItemDetailResponse deleteFailedProduct = new DeleteOfflineItemDetailResponse();
    deleteFailedProduct.setItemSku(GDN_SKU);
    deleteFailedProduct.setPickupPointCode(PICKUP_POINT_CODE);
    deleteFailedProduct.setErrorMessage(ERROR_MESSAGE);
    this.deleteOfflineItemSuccessProducts = Collections.singletonList(deleteSuccessProduct);
    this.deleteOfflineItemFailedProducts = Collections.singletonList(deleteFailedProduct);

    this.offlineItemResponseDetail = new OfflineItemResponseDetail();
    this.offlineItemResponseDetail.setItemSku(ITEM_SKU);
    this.offlineItemResponseDetail.setMerchantSku(MERCHANT_SKU);
    this.offlineItemResponseDetails = Collections.singletonList(offlineItemResponseDetail);

    this.offlineItemResponseDetailResponse = new OfflineItemResponseDetailResponse();
    this.offlineItemResponseDetailResponse.setItemSku(ITEM_SKU);
    this.offlineItemResponseDetailResponse.setMerchantSku(MERCHANT_SKU);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.offlineItemService);
    Mockito.verifyNoMoreInteractions(this.responseConverter);
    Mockito.verifyNoMoreInteractions(this.requestConverter);
  }

  @Test
  public void updateOfflineItemPriceByItemSku_SuccessTest() throws Exception {
    Mockito.when(this.offlineItemService.updateOfflineItemPriceByItemSku(eq(MERCHANT_CODE),
        eq(ITEM_SKU), eq(LIST_PRICE), eq(OFFER_PRICE))).thenReturn(true);

    URI uri = new URIBuilder()
        .setPath(OfflineItemControllerPath.BASE_PATH
            + OfflineItemControllerPath.UPDATE_PRICE_BY_ITEM_SKU)
        .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
        .addParameter("clientId", CLIENT_ID).addParameter("merchantCode", MERCHANT_CODE)
        .addParameter("itemSku", ITEM_SKU).addParameter("listPrice", String.valueOf(LIST_PRICE))
        .addParameter("offerPrice", String.valueOf(OFFER_PRICE)).build();

    this.mockMvc
        .perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(this.offlineItemService).updateOfflineItemPriceByItemSku(eq(MERCHANT_CODE),
        eq(ITEM_SKU), eq(LIST_PRICE), eq(OFFER_PRICE));
  }

  @Test
  public void updateOfflineItemPriceByItemSku_ExceptionTest() throws Exception {
    Mockito.doThrow(new Exception()).when(this.offlineItemService).updateOfflineItemPriceByItemSku(
        eq(MERCHANT_CODE), eq(ITEM_SKU), eq(LIST_PRICE), eq(OFFER_PRICE));

    URI uri = new URIBuilder()
        .setPath(OfflineItemControllerPath.BASE_PATH
            + OfflineItemControllerPath.UPDATE_PRICE_BY_ITEM_SKU)
        .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
        .addParameter("clientId", CLIENT_ID).addParameter("merchantCode", MERCHANT_CODE)
        .addParameter("itemSku", ITEM_SKU).addParameter("listPrice", String.valueOf(LIST_PRICE))
        .addParameter("offerPrice", String.valueOf(OFFER_PRICE)).build();

    this.mockMvc
        .perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)));

    Mockito.verify(this.offlineItemService).updateOfflineItemPriceByItemSku(eq(MERCHANT_CODE),
        eq(ITEM_SKU), eq(LIST_PRICE), eq(OFFER_PRICE));
  }

  @Test
  public void filterOfflineItemProductByMerchantSkusTest() throws Exception {
    SimpleListStringRequest request = new SimpleListStringRequest();
    request.setValue(MERCHANT_SKUS);

    when(this.offlineItemService.findOfflineItemProductByMerchantSkus(BUSINESS_PARTNER_CODE, MERCHANT_SKUS))
        .thenReturn(this.offlineItemResponseDetails);
    when(this.responseConverter.convertOfflineItemResponseDetailResponses(this.offlineItemResponseDetails))
        .thenReturn(Collections.singletonList(this.offlineItemResponseDetailResponse));

    URI uri =
        new URIBuilder()
            .setPath(OfflineItemControllerPath.BASE_PATH
                + OfflineItemControllerPath.FILTER_OFFLINE_ITEM_PRODUCT_BY_MERCHANT_SKUS)
            .addParameter("storeId", STORE_ID)
            .addParameter("channelId", CHANNEL_ID)
            .addParameter("requestId", REQUEST_ID)
            .addParameter("username", USERNAME)
            .addParameter("clientId", CLIENT_ID)
            .addParameter("businessPartnerCode", BUSINESS_PARTNER_CODE)
            .build();

    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.offlineItemService).findOfflineItemProductByMerchantSkus(BUSINESS_PARTNER_CODE,
        MERCHANT_SKUS);
    verify(this.responseConverter).convertOfflineItemResponseDetailResponses(
        this.offlineItemResponseDetails);
  }

  @Test
  public void deleteOfflineItemTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(OfflineItemControllerPath.BASE_PATH
            + OfflineItemControllerPath.DELETE_OFFLINE_ITEM)
        .addParameter("storeId", STORE_ID)
        .addParameter("channelId", CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID)
        .addParameter("username", USERNAME)
        .addParameter("clientId", CLIENT_ID)
        .addParameter("merchantCode", BUSINESS_PARTNER_CODE)
        .build();
    Mockito.when(this.requestConverter.convertToDeleteOfflineItem(anyList()))
        .thenReturn(this.deleteOfflineItems);
    this.mockMvc
        .perform(post(uri).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON)
            .content(this.objectMapper.writeValueAsString(this.deleteOfflineItemRequestListRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.requestConverter).convertToDeleteOfflineItem(anyList());
    Mockito.verify(this.offlineItemService)
        .bulkDeleteOfflineItem(REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE, this.deleteOfflineItems);
  }

  @Test
  public void deleteOfflineItemTest_emptyMerchantCode() throws Exception {
    URI uri = new URIBuilder()
        .setPath(OfflineItemControllerPath.BASE_PATH
            + OfflineItemControllerPath.DELETE_OFFLINE_ITEM)
        .addParameter("storeId", STORE_ID)
        .addParameter("channelId", CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID)
        .addParameter("username", USERNAME)
        .addParameter("clientId", CLIENT_ID)
        .addParameter("merchantCode", "")
        .build();
    try {
      this.mockMvc.perform(post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
              .content(this.objectMapper.writeValueAsString(this.deleteOfflineItemRequestListRequest)))
          .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Mockito.verify(this.offlineItemService, Mockito.never())
          .bulkDeleteOfflineItem(REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE, this.deleteOfflineItems);
    }
  }

  @Test
  public void deleteOfflineItemTest_emptyRequest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(OfflineItemControllerPath.BASE_PATH
            + OfflineItemControllerPath.DELETE_OFFLINE_ITEM)
        .addParameter("storeId", STORE_ID)
        .addParameter("channelId", CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID)
        .addParameter("username", USERNAME)
        .addParameter("clientId", CLIENT_ID)
        .addParameter("merchantCode", BUSINESS_PARTNER_CODE)
        .build();
    try {
      this.mockMvc.perform(post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
              .content(this.objectMapper.writeValueAsString(new ListRequest<>())))
          .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Mockito.verify(this.offlineItemService, Mockito.never())
          .bulkDeleteOfflineItem(REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE, this.deleteOfflineItems);
    }
  }

  @Test
  public void upsertOfflineItemsTest_success() throws Exception {
    Mockito.when(this.requestConverter.convertToUpsertOfflineItems(this.upsertOfflineItemRequests))
        .thenReturn(upsertOfflineItems);

    this.pairOfUpsertOfflineItemErrorResponse =
        Pair.of(this.upsertOfflineItemDetailResponses, this.upsertOfflineItemFailedResponses);

    Mockito
        .when(this.offlineItemService.upsertOfflineItems(Mockito.eq(REQUEST_ID),
            Mockito.eq(USERNAME), Mockito.eq(MERCHANT_CODE), anyList(), eq(true)))
        .thenReturn(this.pairOfUpsertOfflineItemErrorResponse);

    URI uri = new URIBuilder()
        .setPath(OfflineItemControllerPath.BASE_PATH + OfflineItemControllerPath.UPSERT_PATH)
        .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
        .addParameter("clientId", CLIENT_ID).addParameter("merchantCode", MERCHANT_CODE)
        .addParameter("fileName", FILE_NAME).build();

    this.mockMvc
        .perform(post(uri).accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(this.upsertOfflineItemListRequest))
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(this.requestConverter)
        .convertToUpsertOfflineItems(this.upsertOfflineItemRequests);
    Mockito.verify(this.offlineItemService).upsertOfflineItems(Mockito.eq(REQUEST_ID),
        Mockito.eq(USERNAME), Mockito.eq(MERCHANT_CODE), anyList(), eq(true));
  }

  @Test
  public void upsertOfflineItemsTest_exception() throws Exception {
    this.upsertOfflineItemListRequest = new ListRequestDTO<>();

    URI uri = new URIBuilder()
        .setPath(OfflineItemControllerPath.BASE_PATH + OfflineItemControllerPath.UPSERT_PATH)
        .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
        .addParameter("clientId", CLIENT_ID).addParameter("merchantCode", MERCHANT_CODE)
        .addParameter("fileName", FILE_NAME).build();

    try {
      this.mockMvc
          .perform(post(uri).accept(MediaType.APPLICATION_JSON)
              .content(objectMapper.writeValueAsString(this.upsertOfflineItemListRequest))
              .contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {

    }
  }

  @Test
  public void bulkDeleteOfflineItemTest_success() throws Exception {
    successAndFailedDeleteOfflineItem =
        Pair.of(deleteOfflineItemSuccessProducts, deleteOfflineItemFailedProducts);

    when(requestConverter.convertToDeleteOfflineItem(anyList())).thenReturn(deleteOfflineItems);
    when(offlineItemService.bulkDeleteOfflineItem(eq(REQUEST_ID), eq(USERNAME), eq(MERCHANT_CODE), eq(deleteOfflineItems)))
        .thenReturn(successAndFailedDeleteOfflineItem);

    URI uri = new URIBuilder()
        .setPath(OfflineItemControllerPath.BASE_PATH + OfflineItemControllerPath.BULK_DELETE_OFFLINE_ITEM)
        .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
        .addParameter("clientId", CLIENT_ID).addParameter("merchantCode", MERCHANT_CODE)
        .build();
    this.mockMvc
        .perform(post(uri).accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(deleteOfflineItemRequestListRequest))
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(requestConverter).convertToDeleteOfflineItem(anyList());
    verify(offlineItemService).bulkDeleteOfflineItem(eq(REQUEST_ID), eq(USERNAME), eq(MERCHANT_CODE), eq(deleteOfflineItems));
  }

  @Test
  public void bulkDeleteOfflineItemTest_exception() throws Exception {
    deleteOfflineItemRequestListRequest = new ListRequest<>();

    URI uri = new URIBuilder()
        .setPath(OfflineItemControllerPath.BASE_PATH + OfflineItemControllerPath.BULK_DELETE_OFFLINE_ITEM)
        .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
        .addParameter("clientId", CLIENT_ID).addParameter("merchantCode", MERCHANT_CODE)
        .build();

    try {
      this.mockMvc
          .perform(post(uri).accept(MediaType.APPLICATION_JSON)
              .content(objectMapper.writeValueAsString(deleteOfflineItemRequestListRequest))
              .contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {

    }
  }
}
