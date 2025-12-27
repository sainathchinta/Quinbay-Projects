package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import com.gdn.x.product.rest.web.model.request.ReelProductListingRequest;
import com.gdn.x.product.rest.web.model.response.ReelProductDetailResponse;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.exception.SolrCustomException;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.BulkItemSummaryRequestVo;
import com.gdn.x.product.model.vo.CampaignItemSummaryRequestVO;
import com.gdn.x.product.model.vo.ItemListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;
import com.gdn.x.product.model.vo.ItemSummaryRequestVO;
import com.gdn.x.product.model.vo.ItemSummaryResponseVO;
import com.gdn.x.product.model.vo.ItemsSummaryDetailRequestVo;
import com.gdn.x.product.model.vo.ProductCenterSummaryRequest;
import com.gdn.x.product.model.vo.ProductCenterSummaryResponse;
import com.gdn.x.product.model.vo.UpdateItemSummaryRequestVo;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.QuickEditUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpdateItemSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponse;
import com.gdn.x.product.service.api.CategoryService;
import com.gdn.x.product.service.api.ItemSummaryService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.impl.BusinessPartnerPickupPointServiceImpl;
import com.gdn.x.product.service.util.ModelConverter;

/**
 * Created by sarang on 17/03/17.
 */
public class SummaryControllerTest {


  private static final boolean NEED_MASTER_DATA_DETAIL = true;

  private static final String STORE_ID = "store-id";

  private static final String CHANNEL_ID = "channel-id";

  private static final String CLIENT_ID = "client-id";

  private static final String REQUEST_ID = "request-id";

  private static final String USERNAME = "username";

  private static final String ORDER_BY = "orderBy";

  private static final String SORT_BY = "asc";

  private static final String PAGE = "0";

  private static final String SIZE = "10";

  private static final int PAGE_NUMBER = 0;

  private static final int PAGE_SIZE = 10;

  private static final String BLANK = StringUtils.EMPTY;

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "Store id must not be blank";

  private static final String ITEM_SKU_INVALID = "invalid-item-sku";

  private static final String LEVEL2_MERCHANT_CODE = "level2MerchantCode";

  private static final String PRODUCT_SKU = "productSku";

  private static final Boolean DO_ARCHIVE_TRUE = Boolean.TRUE;

  private static final Boolean DO_ARCHIVE_FALSE = Boolean.FALSE;

  private static final String CATALOG_CODE = "catalog-code";

  private static final String SALES_CATEGORY_CODE = null;

  private static final String ITEM_CODE = null;

  private static final String ITEM_CODE1 = "itemcode1";
  private static final String ITEM_CODE2 = "itemcode2";


  private static final String DELIMITER = "delimiter";

  private static final boolean BUYABLE = true;

  private static final boolean DISCOVERABLE = true;

  private static final double OFFER_PRICE = 10000D;

  private static final String MASTER_CATEGORY_CODE = "master-categorycode";

  private static final String PICKUPPOINT_CODE = "pickup-pointcode";

  private static final String ITEM_SKU = "itemsku";

  private static final String ITEM_SKU1 = "itemsku1";

  private static final String ITEM_SKU2 = "itemsku2";

  private static final String ITEM_NAME = "itemName";

  private static final String MERCHANT_CODE = "merchant-code";

  private static final String MERCHANT_SKU = "merchant-sku";

  private static final String CHANNEL = null;

  private static final boolean CNC_ACTIVATED = false;


 ObjectMapper objectMapper = new ObjectMapper();

  @InjectMocks
  SummaryController summaryController;

  @Mock
  ItemSummaryService itemSummaryService;

  @Mock
  CategoryService categoryService;

  @Mock
  ModelConverter modelConverter;

  @Mock
  private ProductService productService;

  @Mock
  private BusinessPartnerPickupPointServiceImpl businessPartnerPickupPointService;

  @Captor
  private ArgumentCaptor<ItemSummaryRequestVO> itemSummaryRequestVOArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductSummaryRequest> productSummaryRequestArgumentCaptor;

  private MockMvc mockMvc;

  private ItemSummaryRequestVO itemSummaryRequestVO;

  private BulkItemSummaryRequestVo bulkItemSummaryRequestVo;

  private ItemSummaryPageResponseVo itemSummaryPageResponseVo;

  private BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();

  private ItemSummaryResponse itemSummaryResponse;
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private String itemSummaryRequestJson;
  private String promoItemSummaryRequestJson;
  private String bulkItemSummaryRequestJson;
  private String productSummaryRequestJson;
  private String productSummaryRequestJson1;
  private CampaignItemSummaryRequest campaignItemSummaryRequest;
  private String campaignItemSummaryRequestJson;
  private PageRequest pageRequest = PageRequest.of(0, 10, Sort.by(Sort.Direction.ASC, ORDER_BY));
  private String productCenterSummaryRequest;
  private ProductSkuSummaryRequest productSkuSummaryRequest = new ProductSkuSummaryRequest();
  private ItemsSummaryDetailRequestVo itemsSummaryDetailRequestVo;
  private String itemSummaryDetailRequestJson;


  @Test
  public void getArchivedItemSummaryByFilterApplicationExceptionTest() throws Exception {
    pageRequest = PageRequest.of(0, 10);
    itemSummaryRequestVO.setPreOrderStatus(false);
    when(this.itemSummaryService.getItemSummaryByArchivedFilter(SummaryControllerTest.STORE_ID,
        SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO,
        pageRequest)).thenThrow(new ApplicationRuntimeException());
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_ARCHIVED_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.itemSummaryRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo("UNSPECIFIED")))
        .andExpect(jsonPath("$.errorMessage", equalTo("Unspecified error :")))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService).getItemSummaryByArchivedFilter(SummaryControllerTest.STORE_ID,
        SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO,
        pageRequest);
  }

  @Test
  public void getArchivedItemSummaryByFilterWithExceptionTest() throws Exception {
    pageRequest = PageRequest.of(0, 10);
    itemSummaryRequestVO.setPreOrderStatus(false);
    when(this.itemSummaryService.getItemSummaryByArchivedFilter(SummaryControllerTest.STORE_ID,
      SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO,
      pageRequest)).thenThrow(new Exception("Error"));
    this.mockMvc.perform(
        post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_ARCHIVED_FILTER).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(this.itemSummaryRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
          .param("channelId", SummaryControllerTest.CHANNEL_ID)
          .param("clientId", SummaryControllerTest.CLIENT_ID)
          .param("requestId", SummaryControllerTest.REQUEST_ID).param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
          .param("size", String.valueOf(10))).andExpect(status().isOk())
      .andExpect(jsonPath("$.errorCode", equalTo("Error")))
      .andExpect(jsonPath("$.errorMessage", equalTo("Error")))
      .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService).getItemSummaryByArchivedFilter(SummaryControllerTest.STORE_ID,
      SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO,
      pageRequest);
  }

  @Test
  public void getArchivedItemSummaryByFilterExceptionTest() throws Exception {
    pageRequest = PageRequest.of(0, 10);
    itemSummaryRequestVO.setPreOrderStatus(false);
    when(this.itemSummaryService.getItemSummaryByArchivedFilter(SummaryControllerTest.STORE_ID,
        SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO,
        pageRequest)).thenThrow(new RuntimeException());
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_ARCHIVED_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.itemSummaryRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService).getItemSummaryByArchivedFilter(SummaryControllerTest.STORE_ID,
        SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO,
        pageRequest);
  }

  @Test
  public void getArchivedItemSummaryByFilterTest() throws Exception {
    pageRequest = PageRequest.of(0, 10);
    itemSummaryRequestVO.setPreOrderStatus(false);
    when(this.itemSummaryService.getItemSummaryByArchivedFilter(SummaryControllerTest.STORE_ID,
        SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO,
        pageRequest)).thenReturn(itemSummaryPageResponseVo);
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_ARCHIVED_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.itemSummaryRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.itemSummaryService).getItemSummaryByArchivedFilter(SummaryControllerTest.STORE_ID,
        SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO,
        pageRequest);
    verify(this.modelConverter).convertToItemSummaryListResponse(SummaryControllerTest.REQUEST_ID,
        0, 10, itemSummaryPageResponseVo);

  }

  @Test
  public void getArchivedItemSummaryByFilterWithARExceptionTest() throws Exception {
    pageRequest = PageRequest.of(0, 10);

    Assertions.assertThrows(AssertionError.class, () -> this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_ARCHIVED_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", SummaryControllerTest.BLANK)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false))));

  }

  @Test
  public void getItemSummaryByArchivedFilterWithNullPointerException() throws Exception {
    pageRequest = PageRequest.of(0, 10);
    itemSummaryRequestJson = null;
    Assertions.assertThrows(NullPointerException.class, () -> this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_ARCHIVED_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.itemSummaryRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.BLANK)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false))));
  }

  @Test
  public void getItemSummaryByFilterTest() throws Exception {
    itemSummaryRequestVO.setPreOrderStatus(true);
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.itemSummaryRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
            .param("sortBy", SummaryControllerTest.SORT_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.itemSummaryService).getItemSummaryByFilter(SummaryControllerTest.STORE_ID,
        SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO, ORDER_BY, SORT_BY, pageRequest);
    verify(this.modelConverter).convertToItemSummaryListResponse(SummaryControllerTest.REQUEST_ID,
        0, 10, itemSummaryPageResponseVo);
  }

  @Test
  public void getItemSummaryByFilterTest_withExcludedItemSKUs() throws Exception {
    String payload = this.itemSummaryRequestJson.substring(0, itemSummaryRequestJson.lastIndexOf("}")) +
      ",\"excludedItemSkus\":[\"itemsku3\",\"itemsku4\"]" + "}";

    this.itemSummaryRequestVO.setExcludedItemSkus(Arrays.asList("itemsku3", "itemsku4"));

    this.mockMvc
      .perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(payload).param("storeId", SummaryControllerTest.STORE_ID)
        .param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID)
        .param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
        .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
        .param("sortBy", SummaryControllerTest.SORT_BY))
      .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
      .andExpect(jsonPath("$.errorMessage", equalTo(null)))
      .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.itemSummaryService).getItemSummaryByFilter(SummaryControllerTest.STORE_ID,
      SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO, ORDER_BY, SORT_BY, pageRequest);
    verify(this.modelConverter).convertToItemSummaryListResponse(SummaryControllerTest.REQUEST_ID,
      0, 10, itemSummaryPageResponseVo);
  }

  @Test
  public void getItemSummaryByFilterTest_withLinkedPartnerCode() throws Exception {
    String payload = this.itemSummaryRequestJson.substring(0, itemSummaryRequestJson.lastIndexOf("}")) +
      ",\"linkedPartnerCode\":\"linked-partner-code\"" + "}";

    this.itemSummaryRequestVO.setLinkedPartnerCode("linked-partner-code");

    this.mockMvc
      .perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(payload).param("storeId", SummaryControllerTest.STORE_ID)
        .param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID)
        .param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
        .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
        .param("sortBy", SummaryControllerTest.SORT_BY))
      .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
      .andExpect(jsonPath("$.errorMessage", equalTo(null)))
      .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.itemSummaryService).getItemSummaryByFilter(SummaryControllerTest.STORE_ID,
      SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO, ORDER_BY, SORT_BY, pageRequest);
    verify(this.modelConverter).convertToItemSummaryListResponse(SummaryControllerTest.REQUEST_ID,
      0, 10, itemSummaryPageResponseVo);
  }

  @Test
  public void getItemSummaryByFilteTest_withZeroPageSize() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.itemSummaryRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(0)).param("orderBy", SummaryControllerTest.ORDER_BY)
            .param("sortBy", SummaryControllerTest.SORT_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.itemSummaryService).getItemSummaryByFilter(SummaryControllerTest.STORE_ID,
        SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO, ORDER_BY, SORT_BY, pageRequest);
    verify(this.modelConverter).convertToItemSummaryListResponse(SummaryControllerTest.REQUEST_ID,
        0, 10, itemSummaryPageResponseVo);

  }

  @Test
  public void getItemSummaryByFilterWithARExceptionTest() throws Exception {

    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.itemSummaryRequestJson).param("storeId", SummaryControllerTest.BLANK)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
            .param("sortBy", SummaryControllerTest.SORT_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService).getItemSummaryByFilter(SummaryControllerTest.BLANK,
        SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO, ORDER_BY, SORT_BY, pageRequest);

  }

  @Test
  public void getPromoItemSummaryByFilterTest() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.PROMO_ITEM_SUMMARY_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.promoItemSummaryRequestJson).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)).param("orderBy", ORDER_BY)
            .param("sortBy", SORT_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryService).getPromoItemSummaryByFilter(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
        itemSummaryRequestVOArgumentCaptor.capture(), eq(ORDER_BY), eq(SORT_BY), eq(pageRequest));
    verify(this.modelConverter).convertToItemSummaryListResponse(REQUEST_ID, 0, 10, itemSummaryPageResponseVo);
    this.itemSummaryRequestVO = itemSummaryRequestVOArgumentCaptor.getValue();
   Assertions.assertEquals(2, this.itemSummaryRequestVO.getProductSkus().size());
   Assertions.assertEquals(1, this.itemSummaryRequestVO.getBoostProductSkus().size());
  }

  @Test
  public void getPromoItemSummaryByFilterPageSizeTest() throws Exception {
    GdnRestListResponse<ItemSummaryResponse> response =
        new GdnRestListResponse<>(null, null, SummaryControllerTest.REQUEST_ID);
    response.setSuccess(true);
    when(this.modelConverter
        .convertToItemSummaryListResponse(SummaryControllerTest.REQUEST_ID, 0, 100, null))
        .thenReturn(response);
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.PROMO_ITEM_SUMMARY_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.promoItemSummaryRequestJson).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("page", String.valueOf(0))
            .param("size", String.valueOf(1000))
            .param("orderBy", ORDER_BY)
            .param("sortBy", SORT_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    pageRequest = PageRequest.of(0, 100, Sort.by(Sort.Direction.ASC, ORDER_BY));
    verify(this.itemSummaryService).getPromoItemSummaryByFilter(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
        itemSummaryRequestVOArgumentCaptor.capture(), eq(ORDER_BY), eq(SORT_BY), eq(pageRequest));
    verify(this.modelConverter).convertToItemSummaryListResponse(REQUEST_ID, 0, 100, itemSummaryPageResponseVo);
    this.itemSummaryRequestVO = itemSummaryRequestVOArgumentCaptor.getValue();
   Assertions.assertEquals(2, this.itemSummaryRequestVO.getProductSkus().size());
   Assertions.assertEquals(1, this.itemSummaryRequestVO.getBoostProductSkus().size());
  }

  @Test
  public void getPromoItemSummaryByFilter_whenApplicationRuntimeExceptionTest() throws Exception {
    when(this.itemSummaryService.getPromoItemSummaryByFilter(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
        itemSummaryRequestVOArgumentCaptor.capture(), eq(ORDER_BY), eq(SORT_BY), eq(pageRequest)))
        .thenThrow(new ApplicationRuntimeException());
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.PROMO_ITEM_SUMMARY_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.promoItemSummaryRequestJson).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)).param("orderBy", ORDER_BY)
            .param("sortBy", SORT_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.itemSummaryService).getPromoItemSummaryByFilter(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
        itemSummaryRequestVOArgumentCaptor.capture(), eq(ORDER_BY), eq(SORT_BY), eq(pageRequest));
    this.itemSummaryRequestVO = itemSummaryRequestVOArgumentCaptor.getValue();
   Assertions.assertEquals(2, this.itemSummaryRequestVO.getProductSkus().size());
   Assertions.assertEquals(1, this.itemSummaryRequestVO.getBoostProductSkus().size());
  }

  @Test
  public void getPromoItemSummaryByFilte_SolrExceptionTest() throws Exception {
    when(this.itemSummaryService.getPromoItemSummaryByFilter(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
      itemSummaryRequestVOArgumentCaptor.capture(), eq(ORDER_BY), eq(SORT_BY), eq(pageRequest)))
      .thenThrow(SolrCustomException.class);
    this.mockMvc
      .perform(post(ProductApiPath.SUMMARY + ProductApiPath.PROMO_ITEM_SUMMARY_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(this.promoItemSummaryRequestJson).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("page", String.valueOf(0))
        .param("size", String.valueOf(10)).param("orderBy", ORDER_BY)
        .param("sortBy", SORT_BY))
      .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.itemSummaryService).getPromoItemSummaryByFilter(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
      itemSummaryRequestVOArgumentCaptor.capture(), eq(ORDER_BY), eq(SORT_BY), eq(pageRequest));
    this.itemSummaryRequestVO = itemSummaryRequestVOArgumentCaptor.getValue();
   Assertions.assertEquals(2, this.itemSummaryRequestVO.getProductSkus().size());
   Assertions.assertEquals(1, this.itemSummaryRequestVO.getBoostProductSkus().size());
  }

  @Test
  public void getPromoItemSummaryByFilter_whenExceptionTest() throws Exception {
    when(this.itemSummaryService.getPromoItemSummaryByFilter(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
        itemSummaryRequestVOArgumentCaptor.capture(), eq(ORDER_BY), eq(SORT_BY), eq(pageRequest)))
        .thenThrow(new RuntimeException());
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.PROMO_ITEM_SUMMARY_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.promoItemSummaryRequestJson).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)).param("orderBy", ORDER_BY)
            .param("sortBy", SORT_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.itemSummaryService).getPromoItemSummaryByFilter(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME),
        itemSummaryRequestVOArgumentCaptor.capture(), eq(ORDER_BY), eq(SORT_BY), eq(pageRequest));
    this.itemSummaryRequestVO = itemSummaryRequestVOArgumentCaptor.getValue();
   Assertions.assertEquals(2, this.itemSummaryRequestVO.getProductSkus().size());
   Assertions.assertEquals(1, this.itemSummaryRequestVO.getBoostProductSkus().size());
  }

  @Test
  public void getItemNamesByFilterTest() throws Exception {
    itemSummaryRequestVO.setPreOrderStatus(false);
    when(this.itemSummaryService.getItemNamesByKeyword(SummaryControllerTest.STORE_ID,
        SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO, null, null, pageRequest)).thenReturn(itemSummaryPageResponseVo);

    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.NAME_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.itemSummaryRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
            .param("sortBy", SummaryControllerTest.SORT_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.itemSummaryService).getItemNamesByKeyword(SummaryControllerTest.STORE_ID,
        SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO, ORDER_BY, SORT_BY, pageRequest);
    verify(this.modelConverter).convertToItemSummaryListResponse(SummaryControllerTest.REQUEST_ID,
        0, 10, itemSummaryPageResponseVo);
  }


  @Test
  public void getItemNamesByFilterWithARExceptionTest() throws Exception {
    itemSummaryRequestVO.setPreOrderStatus(false);
    when(this.itemSummaryService.getItemNamesByKeyword(SummaryControllerTest.STORE_ID,
        SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO, null, null, pageRequest)).thenReturn(itemSummaryPageResponseVo);

    this.mockMvc.perform(post(ProductApiPath.SUMMARY + ProductApiPath.NAME_FILTER).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(this.itemSummaryRequestJson)
        .param("storeId", SummaryControllerTest.BLANK).param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
        .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
        .param("sortBy", SummaryControllerTest.SORT_BY)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService).getItemNamesByKeyword(SummaryControllerTest.BLANK, SummaryControllerTest.USERNAME,
        SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO, ORDER_BY, SORT_BY, pageRequest);
  }

  @Test
  public void getItemNamesByFilterWithNullPointerExceptionTest() throws Exception {
    itemSummaryRequestJson = null;
    Assertions.assertThrows(NullPointerException.class, () -> this.mockMvc.perform(post(ProductApiPath.SUMMARY + ProductApiPath.NAME_FILTER).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(this.itemSummaryRequestJson)
        .param("storeId", SummaryControllerTest.BLANK).param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
        .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
        .param("sortBy", SummaryControllerTest.SORT_BY)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false))));
  }

  @Test
  public void getBulkItemSummaryByFilterTest() throws Exception {
    this.mockMvc
      .perform(post(ProductApiPath.SUMMARY + ProductApiPath.BULK_SUMMARY_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(this.bulkItemSummaryRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
        .param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID)
        .param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
        .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
        .param("sortBy", SummaryControllerTest.SORT_BY))
      .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
      .andExpect(jsonPath("$.errorMessage", equalTo(null)))
      .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.itemSummaryService).getBulkItemSummaryByFilter(SummaryControllerTest.STORE_ID,
      SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.bulkItemSummaryRequestVo,
      pageRequest, SORT_BY, ORDER_BY);
    verify(this.modelConverter).convertToItemSummaryListResponse(SummaryControllerTest.REQUEST_ID,
      0, 10, itemSummaryPageResponseVo);

  }

  @Test
  public void getBulkItemSummaryByFilterTest_withExcludedSKUs() throws Exception {
    String payload = this.bulkItemSummaryRequestJson.substring(0, bulkItemSummaryRequestJson.lastIndexOf("}")) +
      ",\"excludedItemSkus\":[\"itemsku3\",\"itemsku4\"]" + "}";

    this.bulkItemSummaryRequestVo.setExcludedItemSkus(Arrays.asList("itemsku3", "itemsku4"));

    this.mockMvc
      .perform(post(ProductApiPath.SUMMARY + ProductApiPath.BULK_SUMMARY_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(payload).param("storeId", SummaryControllerTest.STORE_ID)
        .param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID)
        .param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
        .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
        .param("sortBy", SummaryControllerTest.SORT_BY))
      .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
      .andExpect(jsonPath("$.errorMessage", equalTo(null)))
      .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.itemSummaryService).getBulkItemSummaryByFilter(SummaryControllerTest.STORE_ID,
      SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.bulkItemSummaryRequestVo,
      pageRequest, SORT_BY, ORDER_BY);
    verify(this.modelConverter).convertToItemSummaryListResponse(SummaryControllerTest.REQUEST_ID,
      0, 10, itemSummaryPageResponseVo);
  }

  @Test
  public void getBulkItemSummaryByFilterTest_withLinkedPartnerCode() throws Exception {
    String payload = this.bulkItemSummaryRequestJson.substring(0, bulkItemSummaryRequestJson.lastIndexOf("}")) +
      ",\"linkedPartnerCode\":\"linked-partner-code\"" + "}";

    this.bulkItemSummaryRequestVo.setLinkedPartnerCode("linked-partner-code");

    this.mockMvc
      .perform(post(ProductApiPath.SUMMARY + ProductApiPath.BULK_SUMMARY_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(payload).param("storeId", SummaryControllerTest.STORE_ID)
        .param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID)
        .param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
        .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
        .param("sortBy", SummaryControllerTest.SORT_BY))
      .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
      .andExpect(jsonPath("$.errorMessage", equalTo(null)))
      .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.itemSummaryService).getBulkItemSummaryByFilter(SummaryControllerTest.STORE_ID,
      SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.bulkItemSummaryRequestVo,
      pageRequest, SORT_BY, ORDER_BY);
    verify(this.modelConverter).convertToItemSummaryListResponse(SummaryControllerTest.REQUEST_ID,
      0, 10, itemSummaryPageResponseVo);
  }

  @Test
  public void getBulkItemSummaryByFilteTest_withZeroPageSize() throws Exception {
    this.mockMvc
      .perform(post(ProductApiPath.SUMMARY + ProductApiPath.BULK_SUMMARY_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(this.bulkItemSummaryRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
        .param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID)
        .param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
        .param("size", String.valueOf(0)).param("orderBy", SummaryControllerTest.ORDER_BY)
        .param("sortBy", SummaryControllerTest.SORT_BY))
      .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
      .andExpect(jsonPath("$.errorMessage", equalTo(null)))
      .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.itemSummaryService).getBulkItemSummaryByFilter(SummaryControllerTest.STORE_ID,
      SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.bulkItemSummaryRequestVo,
      pageRequest, SORT_BY, ORDER_BY);
    verify(this.modelConverter).convertToItemSummaryListResponse(SummaryControllerTest.REQUEST_ID,
      0, 10, itemSummaryPageResponseVo);

  }

  @Test
  public void getBulkItemSummaryByFilterWithSolrException() throws Exception {
    when(this.itemSummaryService.getBulkItemSummaryByFilter(SummaryControllerTest.STORE_ID,
      SummaryControllerTest.USERNAME, BLANK, this.bulkItemSummaryRequestVo, pageRequest, SORT_BY, ORDER_BY))
      .thenThrow(SolrCustomException.class);

    this.mockMvc
      .perform(post(ProductApiPath.SUMMARY + ProductApiPath.BULK_SUMMARY_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(this.bulkItemSummaryRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
        .param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID)
        .param("requestId", SummaryControllerTest.BLANK)
        .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
        .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
        .param("sortBy", SummaryControllerTest.SORT_BY))
      .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService).getBulkItemSummaryByFilter(SummaryControllerTest.STORE_ID,
      SummaryControllerTest.USERNAME, BLANK, this.bulkItemSummaryRequestVo, pageRequest, SORT_BY, ORDER_BY);
  }

  @Test
  public void getItemSummaryByFilterWithNullPointerException() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.itemSummaryRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.BLANK)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
            .param("sortBy", SummaryControllerTest.SORT_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService).getItemSummaryByFilter(SummaryControllerTest.STORE_ID,
        SummaryControllerTest.USERNAME, BLANK, this.itemSummaryRequestVO, ORDER_BY, SORT_BY, pageRequest);
  }

  @Test
  public void getItemSummaryByCategoryAndBrandFilterTest() throws Exception{
    Mockito.when(this.itemSummaryService
        .getCampaignItemSummaryByFilter(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any(CampaignItemSummaryRequestVO.class),
            Mockito.any(PageRequest.class))).thenReturn(itemSummaryPageResponseVo);
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_BY_CATEGORY_AND_BRAND)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.campaignItemSummaryRequestJson)
            .param("storeId", SummaryControllerTest.STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
            .param("sortBy", SummaryControllerTest.SORT_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.itemSummaryService).getCampaignItemSummaryByFilter(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.any(CampaignItemSummaryRequestVO.class),
        Mockito.any(PageRequest.class));
    verify(this.modelConverter).convertToItemSummaryListResponse(SummaryControllerTest.REQUEST_ID,
        0, 10, itemSummaryPageResponseVo);
  }

  @Test
  public void getItemSummaryByCategoryAndBrandFilterWithARExceptionTest() throws Exception{
    Mockito.when(this.itemSummaryService
        .getCampaignItemSummaryByFilter(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any(CampaignItemSummaryRequestVO.class),
            Mockito.any(PageRequest.class))).thenThrow(new ApplicationRuntimeException(ErrorCategory.VALIDATION, STORE_ID_MUST_NOT_BE_BLANK));
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_BY_CATEGORY_AND_BRAND)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.campaignItemSummaryRequestJson)
            .param("storeId", STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
            .param("sortBy", SummaryControllerTest.SORT_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.VALIDATION.getCode())))
        .andExpect(jsonPath("$.errorMessage", equalTo(ErrorCategory.VALIDATION.getMessage()+STORE_ID_MUST_NOT_BE_BLANK)))
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.itemSummaryService)
        .getCampaignItemSummaryByFilter(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any(CampaignItemSummaryRequestVO.class),
            Mockito.any(PageRequest.class));
  }

  @Test
  public void getItemSummaryByCategoryAndBrandFilterWithExceptionTest() throws Exception{
    Mockito.when(this.itemSummaryService
        .getCampaignItemSummaryByFilter(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any(CampaignItemSummaryRequestVO.class),
            Mockito.any(PageRequest.class))).thenThrow(RuntimeException.class);
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_BY_CATEGORY_AND_BRAND)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.campaignItemSummaryRequestJson)
            .param("storeId", STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
            .param("sortBy", SummaryControllerTest.SORT_BY))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.itemSummaryService)
        .getCampaignItemSummaryByFilter(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any(CampaignItemSummaryRequestVO.class),
            Mockito.any(PageRequest.class));
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.summaryController).build();

    List<String> categories = Arrays.asList("CAT-01","CAT-02");
    List<String> brands = Arrays.asList("BRANS1", "BRAND2");
    campaignItemSummaryRequest =
        new CampaignItemSummaryRequest("TEC-12121", categories, brands, null, null);
    this.itemSummaryRequestJson =
        FileUtils.readFileToString(new File("src/test/resources/itemSummaryRequest.json"));
    this.promoItemSummaryRequestJson =
        FileUtils.readFileToString(new File("src/test/resources/promoItemSummaryRequest.json"));
    this.productSummaryRequestJson =
        FileUtils.readFileToString(new File("src/test/resources/productSummaryRequest.json"));
    this.productSummaryRequestJson1 =
        FileUtils.readFileToString(new File("src/test/resources/productSummaryRequest1.json"));

    this.bulkItemSummaryRequestJson =
      FileUtils.readFileToString(new File("src/test/resources/bulkItemSummaryRequest.json"));
    campaignItemSummaryRequestJson = OBJECT_MAPPER.writeValueAsString(campaignItemSummaryRequest);

    GdnRestListResponse<ItemSummaryResponse> response =
        new GdnRestListResponse<ItemSummaryResponse>(null, null, SummaryControllerTest.REQUEST_ID);
    response.setSuccess(true);

    this.itemSummaryRequestVO = new ItemSummaryRequestVO();

    this.itemSummaryRequestVO = new ItemSummaryRequestVO(SummaryControllerTest.MERCHANT_CODE,
        SummaryControllerTest.ITEM_NAME,
        Arrays.asList(SummaryControllerTest.ITEM_SKU1, SummaryControllerTest.ITEM_SKU2),
        SummaryControllerTest.ITEM_SKU, SummaryControllerTest.MASTER_CATEGORY_CODE,
        SummaryControllerTest.OFFER_PRICE, SummaryControllerTest.PICKUPPOINT_CODE,
        SummaryControllerTest.DISCOVERABLE, SummaryControllerTest.BUYABLE,
        SummaryControllerTest.CHANNEL, SummaryControllerTest.ITEM_CODE);
    this.itemSummaryRequestVO.setMerchantSku(SummaryControllerTest.MERCHANT_SKU);
    this.itemSummaryRequestVO.setCncActivated(SummaryControllerTest.CNC_ACTIVATED);
    this.itemSummaryRequestVO.setSalesCategoryCode(SummaryControllerTest.SALES_CATEGORY_CODE);
    this.itemSummaryRequestVO.setPreOrderStatus(true);

    this.bulkItemSummaryRequestVo = new BulkItemSummaryRequestVo();

    this.bulkItemSummaryRequestVo = new BulkItemSummaryRequestVo (SummaryControllerTest.MERCHANT_CODE,
      SummaryControllerTest.ITEM_NAME,
      Arrays.asList(SummaryControllerTest.ITEM_SKU1, SummaryControllerTest.ITEM_SKU2),
      SummaryControllerTest.ITEM_SKU, SummaryControllerTest.MASTER_CATEGORY_CODE,
      SummaryControllerTest.OFFER_PRICE, SummaryControllerTest.PICKUPPOINT_CODE,
      SummaryControllerTest.DISCOVERABLE, SummaryControllerTest.BUYABLE,
      SummaryControllerTest.CHANNEL, Arrays.asList(SummaryControllerTest.ITEM_CODE1, SummaryControllerTest.ITEM_CODE2));
    this.bulkItemSummaryRequestVo.setMerchantSku(SummaryControllerTest.MERCHANT_SKU);
    this.bulkItemSummaryRequestVo.setCncActivated(SummaryControllerTest.CNC_ACTIVATED);
    this.bulkItemSummaryRequestVo.setSalesCategoryCode(SummaryControllerTest.SALES_CATEGORY_CODE);

    productCenterSummaryRequest = OBJECT_MAPPER.writeValueAsString(new ProductCenterSummaryRequest());

    when(this.modelConverter.convertToItemSummaryListResponse(SummaryControllerTest.REQUEST_ID,
        Integer.valueOf(0), Integer.valueOf(10), itemSummaryPageResponseVo)).thenReturn(response);

    doThrow(new ApplicationRuntimeException()).when(this.itemSummaryService)
        .getItemSummaryByArchivedFilter(SummaryControllerTest.BLANK, SummaryControllerTest.USERNAME,
            SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO, pageRequest);


    doThrow(new ApplicationRuntimeException()).when(this.itemSummaryService).getItemSummaryByFilter(
        SummaryControllerTest.BLANK, SummaryControllerTest.USERNAME,
        SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO, ORDER_BY, SORT_BY, pageRequest);

    doThrow(new NullPointerException()).when(this.itemSummaryService).getItemSummaryByFilter(
        SummaryControllerTest.STORE_ID, SummaryControllerTest.USERNAME, BLANK,
        this.itemSummaryRequestVO, ORDER_BY, SORT_BY, pageRequest);

    doThrow(new NullPointerException()).when(this.itemSummaryService)
        .getItemSummaryByArchivedFilter(SummaryControllerTest.STORE_ID,
            SummaryControllerTest.USERNAME, BLANK, this.itemSummaryRequestVO, pageRequest);


    when(this.itemSummaryService.getItemSummaryByFilter(SummaryControllerTest.STORE_ID,
        SummaryControllerTest.USERNAME, SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO, null, null, pageRequest)).thenReturn(itemSummaryPageResponseVo);

    doThrow(new ApplicationRuntimeException()).when(this.itemSummaryService)
        .getItemNamesByKeyword(SummaryControllerTest.BLANK, SummaryControllerTest.USERNAME,
            SummaryControllerTest.REQUEST_ID, this.itemSummaryRequestVO, ORDER_BY, SORT_BY, pageRequest);

    doThrow(new ApplicationRuntimeException()).when(this.itemSummaryService)
        .getItemsSummaryDetailByFilter(SummaryControllerTest.BLANK, SummaryControllerTest.USERNAME,
            SummaryControllerTest.REQUEST_ID, this.itemsSummaryDetailRequestVo, Integer.parseInt(PAGE),
            Integer.parseInt(SIZE));

    doThrow(new NullPointerException()).when(this.itemSummaryService)
        .getItemsSummaryDetailByFilter(SummaryControllerTest.STORE_ID, SummaryControllerTest.USERNAME, BLANK,
            this.itemsSummaryDetailRequestVo, Integer.parseInt(PAGE), Integer.parseInt(SIZE));

    itemsSummaryDetailRequestVo = new ItemsSummaryDetailRequestVo();
    itemsSummaryDetailRequestVo.setItemSku(ITEM_SKU);
    itemsSummaryDetailRequestVo.setProductSku(PRODUCT_SKU);

    this.itemSummaryDetailRequestJson =
        FileUtils.readFileToString(new File("src/test/resources/itemSummaryDetailRequest.json"));

    ReflectionTestUtils.setField(summaryController, "solrMaxPageSize", 100);
  }

  @Test
  public void getItemNameByItemSkusTest() throws Exception {
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    simpleListStringRequest.setValue(Arrays.asList(ITEM_SKU));
    Mockito.when(
        itemSummaryService.getItemNameByItemSkus(SummaryControllerTest.STORE_ID, simpleListStringRequest.getValue(),
            false)).thenReturn(new HashMap<>());
    this.mockMvc.perform(
        post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_ITEM_NAME).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(OBJECT_MAPPER.writeValueAsString(simpleListStringRequest))
            .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryService).getItemNameByItemSkus(SummaryControllerTest.STORE_ID,
        simpleListStringRequest.getValue(), false);
  }

  @Test
  public void getProductNameByProductSkusTest() throws Exception {
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    simpleListStringRequest.setValue(Arrays.asList(ITEM_SKU));
    Mockito.when(itemSummaryService
        .getProductNameByProductSkus(SummaryControllerTest.STORE_ID, simpleListStringRequest.getValue()))
        .thenReturn(new HashMap<>());
    this.mockMvc.perform(
        post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_PRODUCT_NAME).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(OBJECT_MAPPER.writeValueAsString(simpleListStringRequest))
            .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryService)
        .getProductNameByProductSkus(SummaryControllerTest.STORE_ID, simpleListStringRequest.getValue());
  }

  @Test
  public void getProductNamesByProductCodesTest() throws Exception {
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    simpleListStringRequest.setValue(Arrays.asList(ITEM_SKU));
    Mockito.when(itemSummaryService
        .getProductNamesByProductCodes(SummaryControllerTest.STORE_ID, simpleListStringRequest.getValue()))
        .thenReturn(new HashMap<>());
    this.mockMvc.perform(
        post(ProductApiPath.SUMMARY + ProductApiPath.GET_PRODUCT_NAME_BY_CODE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(OBJECT_MAPPER.writeValueAsString(simpleListStringRequest))
            .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryService)
        .getProductNamesByProductCodes(SummaryControllerTest.STORE_ID, simpleListStringRequest.getValue());
  }

  @Test
  public void updateItemSummaryTest() throws Exception {
    UpdateItemSummaryRequest updateItemSummaryRequest = new UpdateItemSummaryRequest();
    UpdateItemSummaryRequestVo updateItemSummaryRequestVo = new UpdateItemSummaryRequestVo();
    ItemSummaryResponseVO itemSummaryResponseVO = new ItemSummaryResponseVO();
    Mockito.when(modelConverter.convertToUpdateItemSummaryVo(updateItemSummaryRequest))
        .thenReturn(updateItemSummaryRequestVo);
    Mockito.when(itemSummaryService.updateItemSummary(SummaryControllerTest.STORE_ID, SummaryControllerTest.REQUEST_ID,
        SummaryControllerTest.USERNAME, SummaryControllerTest.ITEM_SKU, SummaryControllerTest.MERCHANT_CODE,
        updateItemSummaryRequestVo, null)).thenReturn(itemSummaryResponseVO);
    Mockito.when(modelConverter.convertToResponse(itemSummaryResponseVO, ItemSummaryResponse.class))
        .thenReturn(new ItemSummaryResponse());
    this.mockMvc.perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_UPDATE).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(OBJECT_MAPPER.writeValueAsString(updateItemSummaryRequest))
        .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME).param("itemSku", SummaryControllerTest.ITEM_SKU)
        .param("merchantCode", SummaryControllerTest.MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryService).updateItemSummary(SummaryControllerTest.STORE_ID, SummaryControllerTest.REQUEST_ID,
        SummaryControllerTest.USERNAME, SummaryControllerTest.ITEM_SKU, SummaryControllerTest.MERCHANT_CODE,
        updateItemSummaryRequestVo, null);
    verify(modelConverter).convertToUpdateItemSummaryVo(updateItemSummaryRequest);
    verify(modelConverter).convertToResponse(itemSummaryResponseVO, ItemSummaryResponse.class);
  }

  @Test
  public void getSingleItemSummaryByItemSkuTest() throws Exception {
    Mockito.when(itemSummaryService
        .getItemSummaryByItemSku(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            eq(null)))
        .thenReturn(new ItemSummaryPageResponseVo());
    Mockito.when(modelConverter
        .convertToItemSummarySingleResponse(Mockito.anyString(), Mockito.any(ItemSummaryPageResponseVo.class)))
        .thenReturn(new GdnRestSingleResponse(new ItemSummaryResponse(), SummaryControllerTest.REQUEST_ID));
    this.mockMvc.perform(
        get(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_SINGLE).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("itemSku", SummaryControllerTest.ITEM_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(SummaryControllerTest.REQUEST_ID)));
    verify(this.itemSummaryService)
        .getItemSummaryByItemSku(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            eq(null));
    verify(this.modelConverter)
        .convertToItemSummarySingleResponse(Mockito.anyString(), Mockito.any(ItemSummaryPageResponseVo.class));
  }

  @Test
  public void getSingleItemSummaryByItemSku_exceptionTest() throws Exception {
    Mockito.when(this.itemSummaryService
        .getItemSummaryByItemSku(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            eq(null)))
        .thenThrow(Exception.class);
    this.mockMvc.perform(
        get(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_SINGLE).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("itemSku", SummaryControllerTest.ITEM_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.itemSummaryService)
        .getItemSummaryByItemSku(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            eq(null));
  }

  @Test
  public void getSingleArchivedItemSummaryByItemSkuTest() throws Exception {
    Mockito.when(itemSummaryService
        .getItemSummaryByArchivedFilter(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any(ItemSummaryRequestVO.class), Mockito.any(PageRequest.class)))
        .thenReturn(new ItemSummaryPageResponseVo());
    Mockito.when(modelConverter
        .convertToItemSummarySingleResponse(Mockito.anyString(), Mockito.any(ItemSummaryPageResponseVo.class)))
        .thenReturn(new GdnRestSingleResponse(new ItemSummaryResponse(), SummaryControllerTest.REQUEST_ID));
    this.mockMvc.perform(
        get(ProductApiPath.SUMMARY + ProductApiPath.ARCHIVED_SUMMARY_SINGLE).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("itemSku", SummaryControllerTest.ITEM_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(SummaryControllerTest.REQUEST_ID)));
    verify(this.itemSummaryService)
        .getItemSummaryByArchivedFilter(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any(ItemSummaryRequestVO.class), Mockito.any(PageRequest.class));
    verify(this.modelConverter)
        .convertToItemSummarySingleResponse(Mockito.anyString(), Mockito.any(ItemSummaryPageResponseVo.class));
  }

  @Test
  public void getListOfProductSummaryByMasterCatalogTest() throws Exception {
    Page<ProductAndItemSolr> page = new PageImpl<>(Arrays.asList(new ProductAndItemSolr()), PageRequest.of(0, 10), 1);
    Mockito.when(categoryService
        .getProductsByMasterCatalog(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.eq(true),
            Mockito.any(PageRequest.class))).thenReturn(page);
    Mockito.when(modelConverter
        .convertToProductSummaryResponse(Mockito.any(Page.class), Mockito.anyString(), Mockito.anyInt(),
            Mockito.anyInt())).thenReturn(
        new GdnRestListResponse(Arrays.asList(new ProductSummaryResponse()), new PageMetaData(0, 1, 1),
            SummaryControllerTest.REQUEST_ID));
    this.mockMvc.perform(
        get(ProductApiPath.SUMMARY + ProductApiPath.BY_MASTER_CATALOG).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("catalogCode", SummaryControllerTest.CATALOG_CODE)
            .param("categoryCode", SummaryControllerTest.MASTER_CATEGORY_CODE).param("searchEmptySalesOnly", "true")
            .param("page", "0").param("size", "10")).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(SummaryControllerTest.REQUEST_ID)));
    verify(this.categoryService)
        .getProductsByMasterCatalog(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.eq(true),
            Mockito.any(PageRequest.class));
    verify(this.modelConverter)
        .convertToProductSummaryResponse(Mockito.any(Page.class), Mockito.anyString(), Mockito.anyInt(),
            Mockito.anyInt());
  }

  @Test
  public void getListOfProductSummaryByMasterCatalogApplicationExceptionTest() throws Exception {
    Page<ProductAndItemSolr> page =
      new PageImpl<>(Arrays.asList(new ProductAndItemSolr()), PageRequest.of(0, 10), 1);
    doThrow(SolrCustomException.class).when(categoryService)
      .getProductsByMasterCatalog(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.eq(true), Mockito.any(PageRequest.class));
    Mockito.when(
      modelConverter.convertToProductSummaryResponse(Mockito.any(Page.class), Mockito.anyString(),
        Mockito.anyInt(), Mockito.anyInt())).thenReturn(
      new GdnRestListResponse(Arrays.asList(new ProductSummaryResponse()),
        new PageMetaData(0, 1, 1), SummaryControllerTest.REQUEST_ID));
    this.mockMvc.perform(get(ProductApiPath.SUMMARY + ProductApiPath.BY_MASTER_CATALOG).contentType(
          MediaType.APPLICATION_JSON).param("storeId", SummaryControllerTest.STORE_ID)
        .param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID)
        .param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME)
        .param("catalogCode", SummaryControllerTest.CATALOG_CODE)
        .param("categoryCode", SummaryControllerTest.MASTER_CATEGORY_CODE)
        .param("searchEmptySalesOnly", "true").param("page", "0").param("size", "10"))
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.errorMessage", equalTo("Unspecified error :")))
      .andExpect(jsonPath("$.errorCode", equalTo("UNSPECIFIED")))
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.requestId", equalTo(SummaryControllerTest.REQUEST_ID)));
    verify(this.categoryService).getProductsByMasterCatalog(Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(), Mockito.eq(true), Mockito.any(PageRequest.class));
  }

  @Test
  public void getListOfProductSummaryByMasterCatalogExceptionTest() throws Exception {
    Page<ProductAndItemSolr> page =
      new PageImpl<>(Arrays.asList(new ProductAndItemSolr()), PageRequest.of(0, 10), 1);
    doThrow(new Exception("Error")).when(categoryService)
      .getProductsByMasterCatalog(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.eq(true), Mockito.any(PageRequest.class));
    Mockito.when(
      modelConverter.convertToProductSummaryResponse(Mockito.any(Page.class), Mockito.anyString(),
        Mockito.anyInt(), Mockito.anyInt())).thenReturn(
      new GdnRestListResponse(Arrays.asList(new ProductSummaryResponse()),
        new PageMetaData(0, 1, 1), SummaryControllerTest.REQUEST_ID));
    this.mockMvc.perform(get(ProductApiPath.SUMMARY + ProductApiPath.BY_MASTER_CATALOG).contentType(
          MediaType.APPLICATION_JSON).param("storeId", SummaryControllerTest.STORE_ID)
        .param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID)
        .param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME)
        .param("catalogCode", SummaryControllerTest.CATALOG_CODE)
        .param("categoryCode", SummaryControllerTest.MASTER_CATEGORY_CODE)
        .param("searchEmptySalesOnly", "true").param("page", "0").param("size", "10"))
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.errorMessage", equalTo("Error")))
      .andExpect(jsonPath("$.errorCode", equalTo("Error")))
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.requestId", equalTo(SummaryControllerTest.REQUEST_ID)));
    verify(this.categoryService).getProductsByMasterCatalog(Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(), Mockito.eq(true), Mockito.any(PageRequest.class));
  }

  @Test
  public void getListOfProductSummaryBySalesCatalogTest() throws Exception {
    Page<ProductAndItemSolr> page = new PageImpl<>(Arrays.asList(new ProductAndItemSolr()), PageRequest.of(0, 10), 1);
    Mockito.when(categoryService
        .getProductsBySalesCatalog(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any(PageRequest.class))).thenReturn(page);
    Mockito.when(modelConverter
        .convertToProductSummaryResponse(Mockito.any(Page.class), Mockito.anyString(), Mockito.anyInt(),
            Mockito.anyInt())).thenReturn(
        new GdnRestListResponse(Arrays.asList(new ProductSummaryResponse()), new PageMetaData(0, 1, 1),
            SummaryControllerTest.REQUEST_ID));
    this.mockMvc.perform(
        get(ProductApiPath.SUMMARY + ProductApiPath.BY_SALES_CATALOG).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("catalogCode", SummaryControllerTest.CATALOG_CODE)
            .param("categoryCode", SummaryControllerTest.MASTER_CATEGORY_CODE).param("searchEmptySalesOnly", "true")
            .param("page", "0").param("size", "10")).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(SummaryControllerTest.REQUEST_ID)));
    verify(this.categoryService)
        .getProductsBySalesCatalog(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any(PageRequest.class));
    verify(this.modelConverter)
        .convertToProductSummaryResponse(Mockito.any(Page.class), Mockito.anyString(), Mockito.anyInt(),
            Mockito.anyInt());
  }

  @Test
  public void getListOfProductSummaryBySalesCatalogExceptionTest() throws Exception {
    Page<ProductAndItemSolr> page = new PageImpl<>(Arrays.asList(new ProductAndItemSolr()), PageRequest.of(0, 10), 1);
    doThrow(new Exception("Error")).when(categoryService).getProductsBySalesCatalog(Mockito.anyString()
      , Mockito.anyString(), Mockito.anyString(),
      Mockito.any(PageRequest.class));
    Mockito.when(modelConverter
      .convertToProductSummaryResponse(Mockito.any(Page.class), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyInt())).thenReturn(
      new GdnRestListResponse(Arrays.asList(new ProductSummaryResponse()), new PageMetaData(0, 1, 1),
        SummaryControllerTest.REQUEST_ID));
    this.mockMvc.perform(
        get(ProductApiPath.SUMMARY + ProductApiPath.BY_SALES_CATALOG).contentType(MediaType.APPLICATION_JSON)
          .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
          .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
          .param("username", SummaryControllerTest.USERNAME).param("catalogCode", SummaryControllerTest.CATALOG_CODE)
          .param("categoryCode", SummaryControllerTest.MASTER_CATEGORY_CODE).param("searchEmptySalesOnly", "true")
          .param("page", "0").param("size", "10")).andExpect(status().isOk())
      .andExpect(jsonPath("$.errorMessage", equalTo("Error")))
      .andExpect(jsonPath("$.errorCode", equalTo(("Error"))))
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.requestId", equalTo(SummaryControllerTest.REQUEST_ID)));
    verify(this.categoryService)
      .getProductsBySalesCatalog(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(PageRequest.class));
  }

  @Test
  public void getListOfProductSummaryBySalesCatalogSolrExceptionTest() throws Exception {
    Page<ProductAndItemSolr> page = new PageImpl<>(Arrays.asList(new ProductAndItemSolr()), PageRequest.of(0, 10), 1);
    doThrow(SolrCustomException.class).when(categoryService).getProductsBySalesCatalog(Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(),
      Mockito.any(PageRequest.class));
    Mockito.when(modelConverter
      .convertToProductSummaryResponse(Mockito.any(Page.class), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyInt())).thenReturn(
      new GdnRestListResponse(Arrays.asList(new ProductSummaryResponse()), new PageMetaData(0, 1, 1),
        SummaryControllerTest.REQUEST_ID));
    this.mockMvc.perform(
        get(ProductApiPath.SUMMARY + ProductApiPath.BY_SALES_CATALOG).contentType(MediaType.APPLICATION_JSON)
          .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
          .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
          .param("username", SummaryControllerTest.USERNAME).param("catalogCode", SummaryControllerTest.CATALOG_CODE)
          .param("categoryCode", SummaryControllerTest.MASTER_CATEGORY_CODE).param("searchEmptySalesOnly", "true")
          .param("page", "0").param("size", "10")).andExpect(status().isOk())
      .andExpect(jsonPath("$.errorMessage", equalTo(ErrorCategory.UNSPECIFIED.getMessage()))).andExpect(jsonPath("$"
          + ".errorCode",
        equalTo(ErrorCategory.UNSPECIFIED.getCode())))
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.requestId", equalTo(SummaryControllerTest.REQUEST_ID)));
    verify(this.categoryService)
      .getProductsBySalesCatalog(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(PageRequest.class));
  }

  @Test
  public void getProductCenterSummaryFilterTest() throws Exception {
    Page<ProductCenterSummaryResponse> page =
        new PageImpl<>(Arrays.asList(new ProductCenterSummaryResponse()), PageRequest.of(0, 100), 1);
    Mockito.when(productService
        .getProductCenterSummary(STORE_ID, REQUEST_ID, new ProductCenterSummaryRequest(), PageRequest.of(0, 100)))
        .thenReturn(page);
    this.mockMvc.perform(
        post(ProductApiPath.SUMMARY + ProductApiPath.PRODUCT_CENTER_SUMMARY_FILTER).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.productCenterSummaryRequest)
            .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(100))).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.productService)
        .getProductCenterSummary(STORE_ID, REQUEST_ID, new ProductCenterSummaryRequest(), PageRequest.of(0, 100));
  }

  @Test
  public void getProductCenterSummaryFilterExceptionTest() throws Exception {
    Page<ProductCenterSummaryResponse> page =
        new PageImpl<>(Arrays.asList(new ProductCenterSummaryResponse()), PageRequest.of(0, 100), 1);
    Mockito.when(productService
        .getProductCenterSummary(STORE_ID, REQUEST_ID, new ProductCenterSummaryRequest(), PageRequest.of(0, 100)))
        .thenThrow(new ApplicationException(ErrorCategory.DATA_NOT_FOUND, ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK));
    this.mockMvc.perform(
        post(ProductApiPath.SUMMARY + ProductApiPath.PRODUCT_CENTER_SUMMARY_FILTER).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.productCenterSummaryRequest)
            .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(100))).andExpect(status().isOk()).andExpect(jsonPath("$.errorCode",
        equalTo(ErrorCategory.DATA_NOT_FOUND.getMessage() + ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK))).andExpect(
        jsonPath("$.errorMessage",
            equalTo(ErrorCategory.DATA_NOT_FOUND.getMessage() + ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK)))
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.productService)
        .getProductCenterSummary(STORE_ID, REQUEST_ID, new ProductCenterSummaryRequest(), PageRequest.of(0, 100));
  }

  @Test
  public void getProductSkuSummaryListTest() throws Exception {
    Pageable pageable = PageRequest.of(PAGE_NUMBER, PAGE_SIZE);
    Mockito.when(this.productService
        .getProductSkuList(STORE_ID, productSkuSummaryRequest, MERCHANT_CODE, PAGE_NUMBER,
            PAGE_SIZE)).thenReturn(new PageImpl<>(new ArrayList<>(), pageable, PAGE_SIZE));
    this.mockMvc.perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_PRODUCT_SKU_LIST)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(OBJECT_MAPPER.writeValueAsString(productSkuSummaryRequest))
        .param("storeId", SummaryControllerTest.STORE_ID)
        .param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID)
        .param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME)
        .param("businessPartnerCode", MERCHANT_CODE).param("page", PAGE).param("size", SIZE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.productService)
        .getProductSkuList(STORE_ID, productSkuSummaryRequest, MERCHANT_CODE, PAGE_NUMBER,
            PAGE_SIZE);
  }

  @Test
  public void getProductSkuSummaryList_exceptionTest() throws Exception {
    Mockito.when(this.productService
        .getProductSkuList(STORE_ID, productSkuSummaryRequest, MERCHANT_CODE, PAGE_NUMBER,
            PAGE_SIZE)).thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_PRODUCT_SKU_LIST)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(OBJECT_MAPPER.writeValueAsString(productSkuSummaryRequest))
        .param("storeId", SummaryControllerTest.STORE_ID)
        .param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID)
        .param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME)
        .param("businessPartnerCode", MERCHANT_CODE).param("page", PAGE).param("size", SIZE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.productService)
        .getProductSkuList(STORE_ID, productSkuSummaryRequest, MERCHANT_CODE, PAGE_NUMBER,
            PAGE_SIZE);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.itemSummaryService);
    verifyNoMoreInteractions(this.modelConverter);
    verifyNoMoreInteractions(this.businessPartnerPickupPointService);
  }

  @Test
  public void getItemsSummaryDetailByFilterTest() throws Exception {
    when(this.itemSummaryService
        .getItemsSummaryDetailByFilter(anyString(), anyString(), anyString(), any(ItemsSummaryDetailRequestVo.class),
            anyInt(), anyInt())).thenReturn(itemSummaryPageResponseVo);
    try {
      this.mockMvc.perform(
          post(ProductApiPath.SUMMARY + ProductApiPath.DETAIL_SUMMARY_FILTER).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON).content(this.itemSummaryDetailRequestJson)
              .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
              .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
              .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
              .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
              .param("sortBy", SummaryControllerTest.SORT_BY)).andExpect(status().isOk())
          .andExpect(status().isOk());
    } finally {
      verify(this.itemSummaryService)
          .getItemsSummaryDetailByFilter(anyString(), anyString(), anyString(), any(ItemsSummaryDetailRequestVo.class),
              anyInt(), anyInt());
      verify(modelConverter).convertToItemSummaryDetailListResponse(anyString(), anyInt(), anyInt(),
          Mockito.isNull(), Mockito.isNull());
    }
  }

  @Test
  public void getItemsSummaryDetailByFilterWithARExceptionTest() throws Exception {
    when(this.itemSummaryService
        .getItemsSummaryDetailByFilter(anyString(), anyString(), anyString(), any(ItemsSummaryDetailRequestVo.class),
            anyInt(), anyInt())).thenThrow(ApplicationRuntimeException.class);

    this.mockMvc.perform(
        post(ProductApiPath.SUMMARY + ProductApiPath.DETAIL_SUMMARY_FILTER).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.itemSummaryDetailRequestJson)
            .param("storeId", SummaryControllerTest.BLANK).param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
            .param("sortBy", SummaryControllerTest.SORT_BY)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService)
        .getItemsSummaryDetailByFilter(anyString(), anyString(), anyString(), any(ItemsSummaryDetailRequestVo.class),
            anyInt(), anyInt());
  }

  @Test
  public void getItemsSummaryDetailByFilterWithNullPointerException() throws Exception {
    when(this.itemSummaryService
        .getItemsSummaryDetailByFilter(anyString(), anyString(), anyString(), any(ItemsSummaryDetailRequestVo.class),
            anyInt(), anyInt())).thenThrow(Exception.class);

    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.DETAIL_SUMMARY_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.itemSummaryDetailRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.BLANK)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)).param("orderBy", SummaryControllerTest.ORDER_BY)
            .param("sortBy", SummaryControllerTest.SORT_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService)
        .getItemsSummaryDetailByFilter(anyString(), anyString(), anyString(), any(ItemsSummaryDetailRequestVo.class),
            anyInt(), anyInt());
  }

  @Test
  public void updateItemListingTest() throws Exception {
    QuickEditUpdateRequest quickEditUpdateRequest = new QuickEditUpdateRequest();
    ItemListingUpdateRequest itemListingUpdateRequest = new ItemListingUpdateRequest();
    itemListingUpdateRequest.setProductType(ProductType.REGULAR);
    itemListingUpdateRequest.setQuickEditUpdateRequests(Arrays.asList(quickEditUpdateRequest));
    List<ItemListingUpdateRequestVo> requestVo = new ArrayList<>();
    Mockito.when(modelConverter.convertToItemListingUpdateRequestVo(itemListingUpdateRequest)).thenReturn(requestVo);
    Mockito.doNothing().when(itemSummaryService)
        .updateItemListing(SummaryControllerTest.STORE_ID, SummaryControllerTest.REQUEST_ID,
            SummaryControllerTest.USERNAME, PRODUCT_SKU, ProductType.REGULAR, requestVo);
    this.mockMvc.perform(post(ProductApiPath.SUMMARY + ProductApiPath.LISTING_UPDATE, PRODUCT_SKU).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(OBJECT_MAPPER.writeValueAsString(itemListingUpdateRequest))
        .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME).param("productSku", SummaryControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryService).updateItemListing(SummaryControllerTest.STORE_ID, SummaryControllerTest.REQUEST_ID,
        SummaryControllerTest.USERNAME, PRODUCT_SKU, ProductType.REGULAR, requestVo);
    verify(modelConverter).convertToItemListingUpdateRequestVo(itemListingUpdateRequest);
  }

  @Test
  public void updateItemListingExceptionTest() throws Exception {
    QuickEditUpdateRequest quickEditUpdateRequest = new QuickEditUpdateRequest();
    ItemListingUpdateRequest itemListingUpdateRequest = new ItemListingUpdateRequest();
    itemListingUpdateRequest.setProductType(ProductType.REGULAR);
    itemListingUpdateRequest.setQuickEditUpdateRequests(Arrays.asList(quickEditUpdateRequest));
    List<ItemListingUpdateRequestVo> requestVo = new ArrayList<>();
    Mockito.when(modelConverter.convertToItemListingUpdateRequestVo(itemListingUpdateRequest)).thenReturn(requestVo);
    Mockito.doThrow(ApplicationRuntimeException.class).when(itemSummaryService)
        .updateItemListing(SummaryControllerTest.STORE_ID, SummaryControllerTest.REQUEST_ID,
            SummaryControllerTest.USERNAME, PRODUCT_SKU, ProductType.REGULAR, requestVo);
    this.mockMvc.perform(post(ProductApiPath.SUMMARY + ProductApiPath.LISTING_UPDATE, PRODUCT_SKU).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(OBJECT_MAPPER.writeValueAsString(itemListingUpdateRequest))
        .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
        .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.REQUEST_ID)
        .param("username", SummaryControllerTest.USERNAME).param("productSku", SummaryControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.itemSummaryService).updateItemListing(SummaryControllerTest.STORE_ID, SummaryControllerTest.REQUEST_ID,
        SummaryControllerTest.USERNAME, PRODUCT_SKU, ProductType.REGULAR, requestVo);
    verify(modelConverter).convertToItemListingUpdateRequestVo(itemListingUpdateRequest);
  }

  @Test
  public void getFilterSummaryL3() throws Exception {
    Mockito.when(productService.getProductL3SummaryResponse(eq(SummaryControllerTest.STORE_ID),
            Mockito.any(ProductSummaryRequest.class), Mockito.any(PageRequest.class), Mockito.anyBoolean()))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    this.mockMvc.perform(
            post(ProductApiPath.SUMMARY + ProductApiPath.GET_L3_PRODUCT_LIST).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(this.productSummaryRequestJson)
                .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
                .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.BLANK)
                .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
                .param("size", String.valueOf(10))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.productService).getProductL3SummaryResponse(eq(SummaryControllerTest.STORE_ID),
        Mockito.any(ProductSummaryRequest.class), Mockito.any(PageRequest.class), Mockito.anyBoolean());
  }

  @Test
  public void getFilterSummaryL3ExceptionTest() throws Exception {
    Mockito.when(productService.getProductL3SummaryResponse(eq(SummaryControllerTest.STORE_ID), Mockito.any(ProductSummaryRequest.class),
        Mockito.any(PageRequest.class), Mockito.anyBoolean())).thenThrow(ApplicationRuntimeException.class);
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.GET_L3_PRODUCT_LIST)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.productSummaryRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.BLANK)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.productService)
        .getProductL3SummaryResponse(eq(SummaryControllerTest.STORE_ID), Mockito.any(ProductSummaryRequest.class),
            Mockito.any(PageRequest.class), Mockito.anyBoolean());
  }

  @Test
  public void getFilterSummaryL3ApplicationExceptionExceptionTest() throws Exception {
    pageRequest = PageRequest.of(0, 10);
    Mockito.when(productService.getProductL3SummaryResponse(Mockito.eq(SummaryControllerTest.STORE_ID),
            productSummaryRequestArgumentCaptor.capture(), Mockito.eq(pageRequest), Mockito.eq(true)))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(
            post(ProductApiPath.SUMMARY + ProductApiPath.GET_L3_PRODUCT_LIST).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(this.productSummaryRequestJson)
                .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
                .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.BLANK)
                .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
                .param("size", String.valueOf(10))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(productService).getProductL3SummaryResponse(Mockito.eq(SummaryControllerTest.STORE_ID),
        productSummaryRequestArgumentCaptor.capture(), Mockito.eq(pageRequest), Mockito.eq(true));
  }

  @Test
  public void getFilterProductSkuListTest() throws Exception {
    Mockito.when(productService.getProductSkuListResponse(eq(SummaryControllerTest.STORE_ID),
            Mockito.any(ProductSummaryRequest.class), Mockito.any(PageRequest.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    this.mockMvc.perform(
            post(ProductApiPath.SUMMARY + ProductApiPath.GET_PRODUCT_SKU_LIST).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(this.productSummaryRequestJson)
                .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
                .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.BLANK)
                .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
                .param("size", String.valueOf(10))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.productService).getProductSkuListResponse(eq(SummaryControllerTest.STORE_ID),
        Mockito.any(ProductSummaryRequest.class), Mockito.any(PageRequest.class));
  }

  @Test
  public void getFilterProductSkuListApplicationExceptionTest() throws Exception {
    Mockito.when(productService.getProductSkuListResponse(eq(SummaryControllerTest.STORE_ID),
            Mockito.any(ProductSummaryRequest.class), Mockito.any(PageRequest.class)))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(
            post(ProductApiPath.SUMMARY + ProductApiPath.GET_PRODUCT_SKU_LIST).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(this.productSummaryRequestJson)
                .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
                .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.BLANK)
                .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
                .param("size", String.valueOf(10))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.productService).getProductSkuListResponse(eq(SummaryControllerTest.STORE_ID),
        Mockito.any(ProductSummaryRequest.class), Mockito.any(PageRequest.class));
  }

  @Test
  public void getFilterProductSkuListExceptionTest() throws Exception {
    Mockito.when(productService.getProductSkuListResponse(eq(SummaryControllerTest.STORE_ID),
        Mockito.any(ProductSummaryRequest.class), Mockito.any(PageRequest.class))).thenThrow(RuntimeException.class);
    this.mockMvc.perform(
            post(ProductApiPath.SUMMARY + ProductApiPath.GET_PRODUCT_SKU_LIST).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(this.productSummaryRequestJson)
                .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
                .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.BLANK)
                .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
                .param("size", String.valueOf(10))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.productService).getProductSkuListResponse(eq(SummaryControllerTest.STORE_ID),
        Mockito.any(ProductSummaryRequest.class), Mockito.any(PageRequest.class));
  }

  @Test
  public void getProductNamesByFilterTest() throws Exception {
    Mockito.when(productService
        .getProductNamesByKeyword(eq(SummaryControllerTest.STORE_ID), Mockito.any(ProductSummaryRequest.class),
            Mockito.any(PageRequest.class))).thenReturn(new PageImpl<>(new ArrayList<>()));
    this.mockMvc.perform(
        post(ProductApiPath.SUMMARY + ProductApiPath.PRODUCT_NAME_FILTER).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.productSummaryRequestJson1)
            .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.BLANK)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.productService)
        .getProductNamesByKeyword(eq(SummaryControllerTest.STORE_ID), Mockito.any(ProductSummaryRequest.class),
            Mockito.any(PageRequest.class));
  }

  @Test
  public void getProductNamesByFilterExceptionTest() throws Exception {
    Mockito.when(productService.getProductNamesByKeyword(eq(SummaryControllerTest.STORE_ID), Mockito.any(ProductSummaryRequest.class),
        Mockito.any(PageRequest.class))).thenThrow(ApplicationRuntimeException.class);
    this.mockMvc
        .perform(post(ProductApiPath.SUMMARY + ProductApiPath.PRODUCT_NAME_FILTER)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.productSummaryRequestJson).param("storeId", SummaryControllerTest.STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.BLANK)
            .param("username", SummaryControllerTest.USERNAME).param("page", String.valueOf(0))
            .param("size", String.valueOf(10)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.productService)
        .getProductNamesByKeyword(eq(SummaryControllerTest.STORE_ID), Mockito.any(ProductSummaryRequest.class),
            Mockito.any(PageRequest.class));
  }

  @Test
  public void getItemSummaryByItemSkusListTest() throws Exception {
    Mockito.when(itemSummaryService.getItemSummaryByItemSkusList(eq(SummaryControllerTest.STORE_ID), anyList()))
        .thenReturn(new ArrayList<>());
    SimpleListStringRequest listStringRequest = new SimpleListStringRequest();
    this.mockMvc.perform(
        post(ProductApiPath.SUMMARY + ProductApiPath.GET_ITEM_SUMMARY_BY_ITEM_LIST).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(OBJECT_MAPPER.writeValueAsString(listStringRequest))
            .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.BLANK)
            .param("username", SummaryControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryService).
        getItemSummaryByItemSkusList(eq(SummaryControllerTest.STORE_ID), Mockito.isNull());
  }

  @Test
  public void getItemSummaryByItemSkusListApplicationExceptionTest() throws Exception {
    Mockito.when(itemSummaryService.getItemSummaryByItemSkusList(eq(SummaryControllerTest.STORE_ID), Mockito.isNull()))
        .thenThrow(ApplicationRuntimeException.class);
    SimpleListStringRequest listStringRequest = new SimpleListStringRequest();

    this.mockMvc.perform(
        post(ProductApiPath.SUMMARY + ProductApiPath.GET_ITEM_SUMMARY_BY_ITEM_LIST).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(OBJECT_MAPPER.writeValueAsString(listStringRequest))
            .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
            .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.BLANK)
            .param("username", SummaryControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService).getItemSummaryByItemSkusList(eq(SummaryControllerTest.STORE_ID), Mockito.isNull());
  }

  @Test
  public void getItemSummaryByItemSkusListExceptionTest() throws Exception {
    Mockito.when(itemSummaryService.getItemSummaryByItemSkusList(eq(SummaryControllerTest.STORE_ID), Mockito.isNull()))
      .thenThrow(ApplicationRuntimeException.class);
    SimpleListStringRequest listStringRequest = new SimpleListStringRequest();
    this.mockMvc.perform(
        post(ProductApiPath.SUMMARY + ProductApiPath.GET_ITEM_SUMMARY_BY_ITEM_LIST).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON).content(OBJECT_MAPPER.writeValueAsString(listStringRequest))
          .param("storeId", SummaryControllerTest.STORE_ID).param("channelId", SummaryControllerTest.CHANNEL_ID)
          .param("clientId", SummaryControllerTest.CLIENT_ID).param("requestId", SummaryControllerTest.BLANK)
          .param("username", SummaryControllerTest.USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService).getItemSummaryByItemSkusList(eq(SummaryControllerTest.STORE_ID), Mockito.isNull());
  }

  @Test
  public void updateWholeSaleActivationFlagTest() throws Exception {
    SimpleListStringRequest listStringRequest = new SimpleListStringRequest();
    this.mockMvc.perform(post(ProductApiPath.SUMMARY + ProductApiPath.UPDATE_WHOLESALE_ACTIVATION_FLAG).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(OBJECT_MAPPER.writeValueAsString(listStringRequest)).param("storeId", SummaryControllerTest.STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID).param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.REQUEST_ID).param("username", SummaryControllerTest.USERNAME)
            .param("wholeSalePriceActivated", "true")).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryService).updateWholeSaleActivationFlag(eq(SummaryControllerTest.STORE_ID), anyList(),
        anyBoolean());
  }

  @Test
  public void updateWholeSaleActivationFlagExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(itemSummaryService)
        .updateWholeSaleActivationFlag(eq(SummaryControllerTest.STORE_ID), anyList(), anyBoolean());
    SimpleListStringRequest listStringRequest = new SimpleListStringRequest();
    this.mockMvc.perform(post(ProductApiPath.SUMMARY + ProductApiPath.UPDATE_WHOLESALE_ACTIVATION_FLAG).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(OBJECT_MAPPER.writeValueAsString(listStringRequest)).param("storeId", SummaryControllerTest.STORE_ID)
            .param("channelId", SummaryControllerTest.CHANNEL_ID).param("clientId", SummaryControllerTest.CLIENT_ID)
            .param("requestId", SummaryControllerTest.REQUEST_ID).param("username", SummaryControllerTest.USERNAME)
            .param("wholeSalePriceActivated", "true")).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.itemSummaryService).updateWholeSaleActivationFlag(eq(SummaryControllerTest.STORE_ID), anyList(),
        anyBoolean());
  }

  @Test
  public void getPickupPointDetailByCodesTest() throws Exception {
    SimpleListStringRequest listStringRequest = new SimpleListStringRequest();
    when(this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(
      STORE_ID, listStringRequest.getValue())).thenReturn(
      Collections.singletonList(businessPartnerPickupPoint));
    this.mockMvc.perform(post(ProductApiPath.SUMMARY + ProductApiPath.PICKUP_POINT_DETAIL_BY_CODES).accept(
          MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(OBJECT_MAPPER.writeValueAsString(listStringRequest)).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME))
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.modelConverter).convertToPickupPointDetailResponseList(
      Collections.singletonList(businessPartnerPickupPoint), false);
    verify(this.businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(
      STORE_ID, listStringRequest.getValue());
  }

  @Test
  public void getPickupPointDetailByCodesExceptionTest() throws Exception {
    SimpleListStringRequest listStringRequest = new SimpleListStringRequest();
    when(this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(
      STORE_ID, listStringRequest.getValue())).thenThrow(RuntimeException.class);
    this.mockMvc.perform(post(ProductApiPath.SUMMARY + ProductApiPath.PICKUP_POINT_DETAIL_BY_CODES).accept(
          MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(OBJECT_MAPPER.writeValueAsString(listStringRequest)).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME))
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(
      STORE_ID, listStringRequest.getValue());
  }

  @Test
  public void getProductsToMapToReelsTest() throws Exception {
    Page<ReelProductDetailResponse> page =
        new PageImpl<>(Collections.singletonList(new ReelProductDetailResponse()),
            PageRequest.of(0, 10), 1);
    ReelProductListingRequest reelProductListingRequest = new ReelProductListingRequest();
    reelProductListingRequest.setCategoryCodes(List.of(MASTER_CATEGORY_CODE));
    reelProductListingRequest.setKeyword(PRODUCT_SKU);
    when(this.productService.getProductDetailsByReelProductListingRequest(STORE_ID,
        reelProductListingRequest, PageRequest.of(0, 10))).thenReturn(page);
    this.mockMvc.perform(
            post(ProductApiPath.SUMMARY + ProductApiPath.PRODUCT_LIST_FOR_REELS).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(OBJECT_MAPPER.writeValueAsString(reelProductListingRequest))
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId",
                    CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", "0")
                .param("size", "10")).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.productService).getProductDetailsByReelProductListingRequest(STORE_ID,
        reelProductListingRequest, PageRequest.of(0, 10));
  }

  @Test
  public void getProductsToMapToReelsExceptionTest() throws Exception {
    ReelProductListingRequest reelProductListingRequest = new ReelProductListingRequest();
    reelProductListingRequest.setCategoryCodes(List.of(MASTER_CATEGORY_CODE));
    reelProductListingRequest.setKeyword(PRODUCT_SKU);
    doThrow(new RuntimeException()).when(this.productService)
        .getProductDetailsByReelProductListingRequest(STORE_ID, reelProductListingRequest,
            PageRequest.of(0, 10));
    this.mockMvc.perform(
            post(ProductApiPath.SUMMARY + ProductApiPath.PRODUCT_LIST_FOR_REELS).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(OBJECT_MAPPER.writeValueAsString(reelProductListingRequest))
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId",
                    CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", "0")
                .param("size", "10")).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.productService).getProductDetailsByReelProductListingRequest(STORE_ID,
        reelProductListingRequest, PageRequest.of(0, 10));
  }
}
