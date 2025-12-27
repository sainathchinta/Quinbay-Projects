package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.CoreMatchers.equalTo;
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
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.CampaignItemSummaryRequestVO;
import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;
import com.gdn.x.product.model.vo.ItemSummaryRequestVO;
import com.gdn.x.product.model.vo.ItemSummaryResponseVO;
import com.gdn.x.product.model.vo.UpdateItemSummaryRequestVo;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpdateItemSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponse;
import com.gdn.x.product.service.api.CategoryService;
import com.gdn.x.product.service.api.ItemSummaryService;
import com.gdn.x.product.service.util.ModelConverter;

public class ItemSummaryControllerTest {

  private static final String SEARCH_EMPTY_SALES_ONLY = "true";

  private static final String CATEGORY_CODE = "categoryCode";

  private static final String CATALOG_CODE = "catalogCode";

  private static final String MERCHANT_CODE = "merchant-code";

  private static final String ITEM_SKU = "item-sku";

  private static final String PAGE = "0";

  private static final String SIZE = "10";

  private static final String ORDER_BY = "orderBy";

  private static final String SORT_BY = "asc";

  private static final String USERNAME = "username";

  private static final String REQUEST_ID = "request-id";

  private static final String CLIENT_ID = "client-id";

  private static final String CHANNEL_ID = "channel-id";

  private static final String STORE_ID = "store-id";

  private static final String PATH_FILE_CAMPAIGN_ITEM_SUMMARY_REQUEST_JSON =
      "src/test/resources/campaignItemSummaryRequest.json";

  private static final String PATH_FILE_ITEM_SUMMARY_REQUEST_JSON =
      "src/test/resources/itemSummaryRequest.json";

  private static final String PATH_FILE_ITEM_SUMMARY_WITH_CATEGORY_CODES_REQUEST_JSON =
    "src/test/resources/itemSummaryRequestWithCategories.json";

  private static final String PATH_FILE_UPDATE_ITEM_SUMMARY_REQUEST_JSON =
      "src/test/resources/updateItemSummaryRequest.json";

  private static final ItemSummaryPageResponseVo ITEM_SUMMARY_RESPONSE =
      new ItemSummaryPageResponseVo();

  private static final Pageable PAGEABLE = PageRequest.of(0, 10);

  @InjectMocks
  private SummaryController itemSummaryController;

  @Mock
  private ItemSummaryService itemSummaryFilterService;

  @Mock
  private ModelConverter modelConverter;

  @Mock
  private CategoryService categoryService;

  @Mock
  private Page<ProductAndItemSolr> pageResult;
  private MockMvc mockMvc;

  private String itemSummaryRequestJson;
  private String campaignItemSummaryRequestJson;
  private ItemSummaryRequest itemSummaryRequest;
  private ItemSummaryRequestVO itemSummaryRequestVo;
  private CampaignItemSummaryRequest campaignItemSummaryRequest;
  private CampaignItemSummaryRequestVO campaignItemSummaryRequestVO;

  private String updateItemSummaryJson;
  private UpdateItemSummaryRequest updateItemSummaryRequest;
  private UpdateItemSummaryRequestVo updateItemSummaryRequestVo;

  private String skuList;
  private SimpleListStringRequest simpleListStringRequest;

  @Test
  public void getItemNameTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_ITEM_NAME)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.skuList).param("storeId", ItemSummaryControllerTest.STORE_ID)
                .param("channelId", ItemSummaryControllerTest.CHANNEL_ID)
                .param("clientId", ItemSummaryControllerTest.CLIENT_ID)
                .param("requestId", ItemSummaryControllerTest.REQUEST_ID)
                .param("username", ItemSummaryControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryFilterService).getItemNameByItemSkus(ItemSummaryControllerTest.STORE_ID,
        this.simpleListStringRequest.getValue(), false);
  }

  @Test
  public void getItemSummaryTest() throws Exception {
    itemSummaryRequestVo.setPreOrderStatus(true);
    PageRequest pageRequest = PageRequest.of(Integer.valueOf(ItemSummaryControllerTest.PAGE),
        Integer.valueOf(ItemSummaryControllerTest.SIZE),
        Sort.by(Sort.Direction.ASC, ItemSummaryControllerTest.ORDER_BY));

    when(
        this.itemSummaryFilterService.getItemSummaryByFilter(ItemSummaryControllerTest.STORE_ID,
            ItemSummaryControllerTest.USERNAME, ItemSummaryControllerTest.REQUEST_ID,
            this.itemSummaryRequestVo, ORDER_BY, SORT_BY, pageRequest))
        .thenReturn(ItemSummaryControllerTest.ITEM_SUMMARY_RESPONSE);

    this.mockMvc
        .perform(
            post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_FILTER)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.itemSummaryRequestJson)
                .param("storeId", ItemSummaryControllerTest.STORE_ID)
                .param("channelId", ItemSummaryControllerTest.CHANNEL_ID)
                .param("clientId", ItemSummaryControllerTest.CLIENT_ID)
                .param("requestId", ItemSummaryControllerTest.REQUEST_ID)
                .param("username", ItemSummaryControllerTest.USERNAME)
                .param("page", ItemSummaryControllerTest.PAGE)
                .param("size", ItemSummaryControllerTest.SIZE)
                .param("orderBy", ItemSummaryControllerTest.ORDER_BY)
                .param("sortBy", ItemSummaryControllerTest.SORT_BY)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryFilterService).getItemSummaryByFilter(
        eq(ItemSummaryControllerTest.STORE_ID),
        eq(ItemSummaryControllerTest.USERNAME),
        eq(ItemSummaryControllerTest.REQUEST_ID),
        eq(this.itemSummaryRequestVo), eq(ORDER_BY), eq(SORT_BY), Mockito.any());

    verify(this.modelConverter).convertToItemSummaryListResponse(
        ItemSummaryControllerTest.REQUEST_ID, Integer.valueOf(ItemSummaryControllerTest.PAGE),
        Integer.valueOf(ItemSummaryControllerTest.SIZE),
        ItemSummaryControllerTest.ITEM_SUMMARY_RESPONSE);
  }

  @Test
  public void getItemSummaryTest_withMultipleCategories() throws Exception {
    this.itemSummaryRequestJson =
      FileUtils.readFileToString(new File(
        ItemSummaryControllerTest.PATH_FILE_ITEM_SUMMARY_WITH_CATEGORY_CODES_REQUEST_JSON));
    itemSummaryRequestVo.setPreOrderStatus(true);
    this.itemSummaryRequestVo.setCategoryCodes(Arrays.asList(CATEGORY_CODE));

    PageRequest pageRequest = PageRequest.of(Integer.valueOf(ItemSummaryControllerTest.PAGE),
      Integer.valueOf(ItemSummaryControllerTest.SIZE),
      Sort.by(Sort.Direction.ASC, ItemSummaryControllerTest.ORDER_BY));

    when(
      this.itemSummaryFilterService.getItemSummaryByFilter(ItemSummaryControllerTest.STORE_ID,
        ItemSummaryControllerTest.USERNAME, ItemSummaryControllerTest.REQUEST_ID,
        this.itemSummaryRequestVo, ORDER_BY, SORT_BY, pageRequest))
      .thenReturn(ItemSummaryControllerTest.ITEM_SUMMARY_RESPONSE);

    this.mockMvc
      .perform(
        post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_FILTER)
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(this.itemSummaryRequestJson)
          .param("storeId", ItemSummaryControllerTest.STORE_ID)
          .param("channelId", ItemSummaryControllerTest.CHANNEL_ID)
          .param("clientId", ItemSummaryControllerTest.CLIENT_ID)
          .param("requestId", ItemSummaryControllerTest.REQUEST_ID)
          .param("username", ItemSummaryControllerTest.USERNAME)
          .param("page", ItemSummaryControllerTest.PAGE)
          .param("size", ItemSummaryControllerTest.SIZE)
          .param("orderBy", ItemSummaryControllerTest.ORDER_BY)
          .param("sortBy", ItemSummaryControllerTest.SORT_BY)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryFilterService).getItemSummaryByFilter(
      eq(ItemSummaryControllerTest.STORE_ID),
      eq(ItemSummaryControllerTest.USERNAME),
      eq(ItemSummaryControllerTest.REQUEST_ID),
      eq(this.itemSummaryRequestVo), eq(ORDER_BY), eq(SORT_BY), Mockito.any());
    verify(this.modelConverter).convertToItemSummaryListResponse(
      ItemSummaryControllerTest.REQUEST_ID, Integer.valueOf(ItemSummaryControllerTest.PAGE),
      Integer.valueOf(ItemSummaryControllerTest.SIZE),
      ItemSummaryControllerTest.ITEM_SUMMARY_RESPONSE);
  }

  @Disabled
  @Test
  public void getItemSummaryByCategoryAndBrandFilterTest() throws Exception {
    PageRequest pageRequest = PageRequest.of(Integer.valueOf(ItemSummaryControllerTest.PAGE),
        Integer.valueOf(ItemSummaryControllerTest.SIZE),
        Sort.by(Sort.Direction.ASC, ItemSummaryControllerTest.ORDER_BY));

    when(
        this.itemSummaryFilterService.getCampaignItemSummaryByFilter(ItemSummaryControllerTest.STORE_ID,
            ItemSummaryControllerTest.USERNAME, ItemSummaryControllerTest.REQUEST_ID,
            this.campaignItemSummaryRequestVO,pageRequest))
        .thenReturn(ItemSummaryControllerTest.ITEM_SUMMARY_RESPONSE);

    verify(this.itemSummaryFilterService).getCampaignItemSummaryByFilter(
        eq(ItemSummaryControllerTest.STORE_ID),
        eq(ItemSummaryControllerTest.USERNAME),
        eq(ItemSummaryControllerTest.REQUEST_ID),
        eq(this.campaignItemSummaryRequestVO),Mockito.eq(pageRequest));

    this.mockMvc
        .perform(
            post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_BY_CATEGORY_AND_BRAND)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.campaignItemSummaryRequestJson)
                .param("storeId", ItemSummaryControllerTest.STORE_ID)
                .param("channelId", ItemSummaryControllerTest.CHANNEL_ID)
                .param("clientId", ItemSummaryControllerTest.CLIENT_ID)
                .param("requestId", ItemSummaryControllerTest.REQUEST_ID)
                .param("username", ItemSummaryControllerTest.USERNAME)
                .param("page", ItemSummaryControllerTest.PAGE)
                .param("size", ItemSummaryControllerTest.SIZE)
                .param("orderBy", ItemSummaryControllerTest.ORDER_BY)
                .param("sortBy", ItemSummaryControllerTest.SORT_BY)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.modelConverter).convertToItemSummaryListResponse(
        ItemSummaryControllerTest.REQUEST_ID, Integer.valueOf(ItemSummaryControllerTest.PAGE),
        Integer.valueOf(ItemSummaryControllerTest.SIZE),
        ItemSummaryControllerTest.ITEM_SUMMARY_RESPONSE);
  }

  @Test
  public void getListOfProductByMasterCatalogTest() throws Exception {
    this.mockMvc
        .perform(
            get(ProductApiPath.SUMMARY + ProductApiPath.BY_MASTER_CATALOG)
                .param("storeId", ItemSummaryControllerTest.STORE_ID)
                .param("channelId", ItemSummaryControllerTest.CHANNEL_ID)
                .param("clientId", ItemSummaryControllerTest.CLIENT_ID)
                .param("requestId", ItemSummaryControllerTest.REQUEST_ID)
                .param("username", ItemSummaryControllerTest.USERNAME)
                .param(ItemSummaryControllerTest.CATALOG_CODE,
                    ItemSummaryControllerTest.CATALOG_CODE)
                .param(ItemSummaryControllerTest.CATEGORY_CODE,
                    ItemSummaryControllerTest.CATEGORY_CODE)
                .param("searchEmptySalesOnly", ItemSummaryControllerTest.SEARCH_EMPTY_SALES_ONLY)
                .param("page", ItemSummaryControllerTest.PAGE)
                .param("size", ItemSummaryControllerTest.SIZE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.categoryService).getProductsByMasterCatalog(ItemSummaryControllerTest.STORE_ID,
        ItemSummaryControllerTest.CATALOG_CODE, ItemSummaryControllerTest.CATEGORY_CODE,
        Boolean.valueOf(ItemSummaryControllerTest.SEARCH_EMPTY_SALES_ONLY),
        ItemSummaryControllerTest.PAGEABLE);
    verify(this.modelConverter).convertToProductSummaryResponse(this.pageResult,
        ItemSummaryControllerTest.REQUEST_ID, Integer.valueOf(ItemSummaryControllerTest.PAGE),
        Integer.valueOf(ItemSummaryControllerTest.SIZE));
  }

  @Test
  public void getListOfProductBySalesCatalogTest() throws Exception {
    this.mockMvc
        .perform(
            get(ProductApiPath.SUMMARY + ProductApiPath.BY_SALES_CATALOG)
                .param("storeId", ItemSummaryControllerTest.STORE_ID)
                .param("channelId", ItemSummaryControllerTest.CHANNEL_ID)
                .param("clientId", ItemSummaryControllerTest.CLIENT_ID)
                .param("requestId", ItemSummaryControllerTest.REQUEST_ID)
                .param("username", ItemSummaryControllerTest.USERNAME)
                .param(ItemSummaryControllerTest.CATALOG_CODE,
                    ItemSummaryControllerTest.CATALOG_CODE)
                .param(ItemSummaryControllerTest.CATEGORY_CODE,
                    ItemSummaryControllerTest.CATEGORY_CODE)
                .param("page", ItemSummaryControllerTest.PAGE)
                .param("size", ItemSummaryControllerTest.SIZE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.categoryService).getProductsBySalesCatalog(ItemSummaryControllerTest.STORE_ID,
        ItemSummaryControllerTest.CATALOG_CODE, ItemSummaryControllerTest.CATEGORY_CODE,
        ItemSummaryControllerTest.PAGEABLE);
    verify(this.modelConverter).convertToProductSummaryResponse(this.pageResult,
        ItemSummaryControllerTest.REQUEST_ID, Integer.valueOf(ItemSummaryControllerTest.PAGE),
        Integer.valueOf(ItemSummaryControllerTest.SIZE));
  }

  @Test
  public void getProductNameTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_PRODUCT_NAME)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.skuList).param("storeId", ItemSummaryControllerTest.STORE_ID)
                .param("channelId", ItemSummaryControllerTest.CHANNEL_ID)
                .param("clientId", ItemSummaryControllerTest.CLIENT_ID)
                .param("requestId", ItemSummaryControllerTest.REQUEST_ID)
                .param("username", ItemSummaryControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryFilterService).getProductNameByProductSkus(
        ItemSummaryControllerTest.STORE_ID, this.simpleListStringRequest.getValue());
  }

  @Test
  public void getSingleArchivedItemSummaryTest() throws Exception {
    ItemSummaryRequestVO requestVo = new ItemSummaryRequestVO();
    requestVo.setItemSkus(Arrays.asList(ItemSummaryControllerTest.ITEM_SKU));
    when(
        this.itemSummaryFilterService.getItemSummaryByArchivedFilter(ItemSummaryControllerTest.STORE_ID,
            ItemSummaryControllerTest.USERNAME, ItemSummaryControllerTest.REQUEST_ID, requestVo,
            PageRequest.of(0, 1))).thenReturn(
        ItemSummaryControllerTest.ITEM_SUMMARY_RESPONSE);
    when(
        this.modelConverter.convertToItemSummarySingleResponse(
            ItemSummaryControllerTest.REQUEST_ID, ItemSummaryControllerTest.ITEM_SUMMARY_RESPONSE))
        .thenReturn(
            new GdnRestSingleResponse<ItemSummaryResponse>(null,
                ItemSummaryControllerTest.REQUEST_ID));
    this.mockMvc
        .perform(
            get(ProductApiPath.SUMMARY + ProductApiPath.ARCHIVED_SUMMARY_SINGLE)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemSummaryControllerTest.STORE_ID)
                .param("channelId", ItemSummaryControllerTest.CHANNEL_ID)
                .param("clientId", ItemSummaryControllerTest.CLIENT_ID)
                .param("requestId", ItemSummaryControllerTest.REQUEST_ID)
                .param("username", ItemSummaryControllerTest.USERNAME)
                .param("itemSku", ItemSummaryControllerTest.ITEM_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryFilterService).getItemSummaryByArchivedFilter(
        ItemSummaryControllerTest.STORE_ID, ItemSummaryControllerTest.USERNAME,
        ItemSummaryControllerTest.REQUEST_ID, requestVo, PageRequest.of(0, 1));

    verify(this.modelConverter).convertToItemSummarySingleResponse(
        ItemSummaryControllerTest.REQUEST_ID, ItemSummaryControllerTest.ITEM_SUMMARY_RESPONSE);
  }


  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.itemSummaryController).build();

    ObjectMapper mapper = new ObjectMapper();

    this.itemSummaryRequestJson =
        FileUtils.readFileToString(new File(
            ItemSummaryControllerTest.PATH_FILE_ITEM_SUMMARY_REQUEST_JSON));

    this.itemSummaryRequest =
        mapper.readValue(this.itemSummaryRequestJson,
            mapper.getTypeFactory().constructType(ItemSummaryRequest.class));
    this.itemSummaryRequestVo = new ItemSummaryRequestVO();
    BeanUtils.copyProperties(this.itemSummaryRequest, this.itemSummaryRequestVo);

    this.campaignItemSummaryRequestJson =
        FileUtils.readFileToString(new File(
        ItemSummaryControllerTest.PATH_FILE_CAMPAIGN_ITEM_SUMMARY_REQUEST_JSON));

    this.campaignItemSummaryRequest =
        mapper.readValue(this.campaignItemSummaryRequestJson,
            mapper.getTypeFactory().constructType(CampaignItemSummaryRequest.class));
    this.campaignItemSummaryRequestVO = new CampaignItemSummaryRequestVO();
    BeanUtils.copyProperties(this.campaignItemSummaryRequest, this.campaignItemSummaryRequestVO);

    this.updateItemSummaryJson =
        FileUtils.readFileToString(new File(
            ItemSummaryControllerTest.PATH_FILE_UPDATE_ITEM_SUMMARY_REQUEST_JSON));

    this.updateItemSummaryRequest =
        mapper.readValue(this.updateItemSummaryJson,
            mapper.getTypeFactory().constructType(UpdateItemSummaryRequest.class));
    this.updateItemSummaryRequestVo = new UpdateItemSummaryRequestVo();
    BeanUtils.copyProperties(this.updateItemSummaryRequest, this.updateItemSummaryRequestVo);


    GdnRestListResponse<ItemSummaryResponse> response =
        new GdnRestListResponse<ItemSummaryResponse>(null, null,
            ItemSummaryControllerTest.REQUEST_ID);
    response.setSuccess(true);
    when(
        this.modelConverter.convertToItemSummaryListResponse(ItemSummaryControllerTest.REQUEST_ID,
            Integer.valueOf(ItemSummaryControllerTest.PAGE),
            Integer.valueOf(ItemSummaryControllerTest.SIZE),
            ItemSummaryControllerTest.ITEM_SUMMARY_RESPONSE)).thenReturn(response);

    when(this.modelConverter.convertToUpdateItemSummaryVo(this.updateItemSummaryRequest))
        .thenReturn(this.updateItemSummaryRequestVo);


    this.skuList =
        FileUtils.readFileToString(new File("src/test/resources/itemSkuList.json"),
            String.valueOf(StandardCharsets.UTF_8));

    this.simpleListStringRequest = mapper.readValue(this.skuList, SimpleListStringRequest.class);

    when(
        this.categoryService.getProductsByMasterCatalog(ItemSummaryControllerTest.STORE_ID,
            ItemSummaryControllerTest.CATALOG_CODE, ItemSummaryControllerTest.CATEGORY_CODE,
            Boolean.valueOf(ItemSummaryControllerTest.SEARCH_EMPTY_SALES_ONLY),
            ItemSummaryControllerTest.PAGEABLE)).thenReturn(this.pageResult);
    when(
        this.categoryService.getProductsBySalesCatalog(ItemSummaryControllerTest.STORE_ID,
            ItemSummaryControllerTest.CATALOG_CODE, ItemSummaryControllerTest.CATEGORY_CODE,
            ItemSummaryControllerTest.PAGEABLE)).thenReturn(this.pageResult);
    when(
        this.modelConverter.convertToProductSummaryResponse(this.pageResult,
            ItemSummaryControllerTest.REQUEST_ID, Integer.valueOf(ItemSummaryControllerTest.PAGE),
            Integer.valueOf(ItemSummaryControllerTest.SIZE))).thenReturn(
        new GdnRestListResponse<ProductSummaryResponse>(null, null,
            ItemSummaryControllerTest.REQUEST_ID));
  }

  @Test
  public void updateItemSummaryTest() throws Exception {
    ItemSummaryResponseVO itemSummaryResponseVO = new ItemSummaryResponseVO();
    when(
        this.itemSummaryFilterService.updateItemSummary(ItemSummaryControllerTest.STORE_ID,
            ItemSummaryControllerTest.REQUEST_ID, ItemSummaryControllerTest.USERNAME,
            ItemSummaryControllerTest.ITEM_SKU, ItemSummaryControllerTest.MERCHANT_CODE,
            this.updateItemSummaryRequestVo, false)).thenReturn(itemSummaryResponseVO);
    this.mockMvc
        .perform(
            post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_UPDATE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.updateItemSummaryJson)
                .param("storeId", ItemSummaryControllerTest.STORE_ID)
                .param("channelId", ItemSummaryControllerTest.CHANNEL_ID)
                .param("clientId", ItemSummaryControllerTest.CLIENT_ID)
                .param("requestId", ItemSummaryControllerTest.REQUEST_ID)
                .param("username", ItemSummaryControllerTest.USERNAME)
                .param("itemSku", ItemSummaryControllerTest.ITEM_SKU)
                .param("merchantCode", ItemSummaryControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.modelConverter).convertToUpdateItemSummaryVo(this.updateItemSummaryRequest);
    verify(this.itemSummaryFilterService).updateItemSummary(ItemSummaryControllerTest.STORE_ID,
        ItemSummaryControllerTest.REQUEST_ID, ItemSummaryControllerTest.USERNAME,
        ItemSummaryControllerTest.ITEM_SKU, ItemSummaryControllerTest.MERCHANT_CODE,
        this.updateItemSummaryRequestVo, false);
    verify(this.modelConverter).convertToResponse(itemSummaryResponseVO, ItemSummaryResponse.class);
  }


  @Test
  public void updateItemSummaryWithException() throws Exception {
    doThrow(Exception.class).when(this.itemSummaryFilterService).updateItemSummary(
        ItemSummaryControllerTest.STORE_ID, ItemSummaryControllerTest.REQUEST_ID,
        ItemSummaryControllerTest.USERNAME, ItemSummaryControllerTest.ITEM_SKU,
        ItemSummaryControllerTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);
    this.mockMvc
        .perform(
            post(ProductApiPath.SUMMARY + ProductApiPath.SUMMARY_UPDATE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.updateItemSummaryJson)
                .param("storeId", ItemSummaryControllerTest.STORE_ID)
                .param("channelId", ItemSummaryControllerTest.CHANNEL_ID)
                .param("clientId", ItemSummaryControllerTest.CLIENT_ID)
                .param("requestId", ItemSummaryControllerTest.REQUEST_ID)
                .param("username", ItemSummaryControllerTest.USERNAME)
                .param("itemSku", ItemSummaryControllerTest.ITEM_SKU)
                .param("merchantCode", ItemSummaryControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.modelConverter).convertToUpdateItemSummaryVo(this.updateItemSummaryRequest);
    verify(this.itemSummaryFilterService).updateItemSummary(ItemSummaryControllerTest.STORE_ID,
        ItemSummaryControllerTest.REQUEST_ID, ItemSummaryControllerTest.USERNAME,
        ItemSummaryControllerTest.ITEM_SKU, ItemSummaryControllerTest.MERCHANT_CODE,
        this.updateItemSummaryRequestVo, false);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.itemSummaryFilterService);
    verifyNoMoreInteractions(this.modelConverter);
  }

}
