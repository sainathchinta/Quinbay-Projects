package com.gdn.x.product.rest.web.controller.api;

import static com.gdn.x.product.enums.SolrFieldNames.BRAND;
import static com.gdn.x.product.enums.SolrFieldNames.ITEM_NAME;
import static com.gdn.x.product.enums.SolrFieldNames.PRODUCT_CATENTRY_ID;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Executor;

import com.gdn.x.product.rest.web.model.request.UpcStatusRequest;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.exception.SolrCustomException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.DefaultItemSkuVO;
import com.gdn.x.product.model.vo.ItemAndBundlingInfoVO;
import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.rest.web.model.AgpConstant;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.dto.ItemDTO;
import com.gdn.x.product.rest.web.model.dto.PristineDataItemDto;
import com.gdn.x.product.rest.web.model.dto.ProductAndItemPickupPointDTO;
import com.gdn.x.product.rest.web.model.enums.ApiErrorCode;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.DefaultPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemAndBundlingInfoRequest;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateItemRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductTypeEditRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.ItemAndBundlingInfoResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicL4Response;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemCodeBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel4ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemsResponse;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ItemSummaryService;
import com.gdn.x.product.service.api.ItemViewConfigService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.ProductSearchService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.executor.api.AsyncProcessor;
import com.gdn.x.product.service.util.ModelConverter;

public class ItemControllerTest {

  private static final boolean NEED_MASTER_DATA_DETAIL = true;

  private static final String STORE_ID = "store-id";

  private static final String CHANNEL_ID = "channel-id";

  private static final String CLIENT_ID = "client-id";

  private static final String REQUEST_ID = "request-id";

  private static final String USERNAME = "username";
  
  private static final String MERCHANT_CODE = "merchantCode";

  private static final String BLANK = "";

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "Store id must not be blank";

  private static final String ITEM_SKU = "item-sku";

  private static final String PROMO_BUNDLING_ID = "dummy-promo-bundling-id";

  private static final String ITEM_SKU_INVALID = "invalid-item-sku";

  private static final String LEVEL2_MERCHANT_CODE = "level2MerchantCode";

  private static final String PRODUCT_SKU = "productSku";

  private static final Boolean FETCH_BUNDLE_RECIPE = Boolean.TRUE;

  private static final String PRISTINE_ID = "pristineId";

  private static final String PRISTINE_ID2 = "pristineId2";

  private static final String ITEM_CODE = "itemCode";

  private static final Boolean DO_ARCHIVE_TRUE = Boolean.TRUE;

  private static final Boolean DO_ARCHIVE_FALSE = Boolean.FALSE;

  private static final double OFFER_PRICE = 11;

  private static final double NORMAL_PRICE = 111;

  private static final String ADD_ITEM_FAILED = "ADD_ITEM_FAILED";

  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String FETCH_VIEW_CONFIGS_BY_CHANNEL = "DEFAULT, CNC";

  @InjectMocks
  private ItemController itemController;

  @Mock
  private ItemService itemService;

  @Mock
  private ProductService productService;

  @Mock
  private MasterDataService masterDataService;

  @Mock
  private ModelConverter modelConverter;

  @Mock
  private AsyncProcessor asyncProcessor;

  @Mock
  Executor publisherTaskExecutor;

  @Mock
  private ItemViewConfigService itemViewConfigService;

  @Mock
  private ProductSearchService productSearchService;

  @Mock
  private ItemSummaryService itemSummaryService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  private MockMvc mockMvc;

  private ItemRequest itemRequest;

  private String jsonRequestItemSkus;

  private String itemRequestJson;

  private Item item;

  private String itemDTOJson;

  private ItemDTO itemDTO;

  private Item itemRequestVO;

  private List<String> pristineIds;

  private String pristineIdsJson;

  private String pristineDataItemDTOJson;
  private String pickupPointUpdateRequestJson;
  private PickupPointUpdateRequest pickupPointUpdateRequest;
  private DefaultPickupPointRequest defaultPickupPointRequest;

  private String skuListJson;

  private ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest = new ItemViewConfigAndItemSkuRequest();

  private ItemViewConfig itemViewConfig = new ItemViewConfig();
  private String simpleSetRequestJson;
  private SimpleSetStringRequest simpleSetRequest;
  private ObjectMapper mapper = new ObjectMapper();
  private ProductAndItemPickupPointDTO productAndItemPickupPointDTO = new ProductAndItemPickupPointDTO();

  @Test
  public void addItemTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.ADD).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(this.itemDTOJson)
                .param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("productSku", ItemControllerTest.PRODUCT_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.modelConverter).convertItemDTOToItem(Mockito.any(ItemDTO.class));
    verify(this.itemService).addItem(ItemControllerTest.STORE_ID, ItemControllerTest.REQUEST_ID,
        ItemControllerTest.USERNAME, ItemControllerTest.PRODUCT_SKU, this.itemRequestVO);
  }

  @Test
  public void addItemTestWithException() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.ADD).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(this.itemDTOJson)
                .param("storeId", ItemControllerTest.BLANK)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("productSku", ItemControllerTest.PRODUCT_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.ADD_ITEM.getCode())))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.modelConverter).convertItemDTOToItem(Mockito.any(ItemDTO.class));
    verify(this.itemService).addItem(ItemControllerTest.BLANK, ItemControllerTest.REQUEST_ID,
        ItemControllerTest.USERNAME, ItemControllerTest.PRODUCT_SKU, this.itemRequestVO);
  }

  @Test
  public void addItemTes_expectApplicationRuntimeException() throws Exception {
    Mockito.when(this.itemService
        .addItem(ItemControllerTest.STORE_ID, ItemControllerTest.REQUEST_ID, ItemControllerTest.USERNAME,
            ItemControllerTest.PRODUCT_SKU, this.itemRequestVO)).thenThrow(ApplicationRuntimeException.class);
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.ADD).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(this.itemDTOJson)
                .param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("productSku", ItemControllerTest.PRODUCT_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(ADD_ITEM_FAILED)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.modelConverter).convertItemDTOToItem(Mockito.any(ItemDTO.class));
    verify(this.itemService).addItem(ItemControllerTest.STORE_ID, ItemControllerTest.REQUEST_ID,
        ItemControllerTest.USERNAME, ItemControllerTest.PRODUCT_SKU, this.itemRequestVO);
  }

  @Test
  public void deleteItemTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.DELETE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("itemSku", ItemControllerTest.ITEM_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.itemService).deleteItem(ItemControllerTest.STORE_ID, ItemControllerTest.ITEM_SKU);
  }

  @Test
  public void deleteItemTestWithException() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.DELETE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("itemSku", ItemControllerTest.ITEM_SKU_INVALID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.DELETE_ITEM.getCode())))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemService).deleteItem(ItemControllerTest.STORE_ID, ItemControllerTest.ITEM_SKU_INVALID);
  }

  @Test
  public void toggleArchiveItemTrueTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.ARCHIVE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("itemSku", ItemControllerTest.ITEM_SKU)
                .param("doArchive",ItemControllerTest.DO_ARCHIVE_TRUE.toString()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.itemService).toggleArchiveItem(ItemControllerTest.STORE_ID, ItemControllerTest.ITEM_SKU,
        ItemControllerTest.USERNAME, ItemControllerTest.DO_ARCHIVE_TRUE);
  }

  @Test
  public void toggleArchiveItemFalseTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.ARCHIVE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("itemSku", ItemControllerTest.ITEM_SKU)
                .param("doArchive",ItemControllerTest.DO_ARCHIVE_FALSE.toString()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.itemService).toggleArchiveItem(ItemControllerTest.STORE_ID, ItemControllerTest.ITEM_SKU,
        ItemControllerTest.USERNAME, ItemControllerTest.DO_ARCHIVE_FALSE);
  }

  @Test
  public void toggleArchiveItem_NonNullEditResponseTest() throws Exception {
    EditItemResponse editItemResponse =
        new EditItemResponse(ApiErrorCode.TOGGLE_UNARCHIVE_FAILED_FOR_SAME_FLAG, Boolean.FALSE, Boolean.TRUE,
            new ArrayList<>(), new ArrayList<>());
    Mockito.when(this.itemService.toggleArchiveItem(STORE_ID, ITEM_SKU, USERNAME, DO_ARCHIVE_FALSE))
      .thenReturn(editItemResponse);
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.ARCHIVE).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
          .param("channelId", ItemControllerTest.CHANNEL_ID)
          .param("clientId", ItemControllerTest.CLIENT_ID)
          .param("requestId", ItemControllerTest.REQUEST_ID)
          .param("username", ItemControllerTest.USERNAME)
          .param("itemSku", ItemControllerTest.ITEM_SKU)
          .param("doArchive", ItemControllerTest.DO_ARCHIVE_FALSE.toString()))
      .andExpect(status().isOk()).andExpect(
        jsonPath("$.errorCode", equalTo(ApiErrorCode.TOGGLE_UNARCHIVE_FAILED_FOR_SAME_FLAG.getCode())))
      .andExpect(jsonPath("$.errorMessage",
        equalTo(ApiErrorCode.TOGGLE_UNARCHIVE_FAILED_FOR_SAME_FLAG.getDesc())))
      .andExpect(jsonPath("$.success", equalTo(false)));
    verify(itemService).toggleArchiveItem(STORE_ID,ITEM_SKU,USERNAME,DO_ARCHIVE_FALSE);

  }

  @Test
  public void toggleArchiveItemTrueTestWithException() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.ARCHIVE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("itemSku", ItemControllerTest.ITEM_SKU_INVALID)
                .param("doArchive",ItemControllerTest.DO_ARCHIVE_TRUE.toString()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.ARCHIVE_ITEM.getCode())))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemService).toggleArchiveItem(ItemControllerTest.STORE_ID, ItemControllerTest.ITEM_SKU_INVALID,
        ItemControllerTest.USERNAME, ItemControllerTest.DO_ARCHIVE_TRUE);
  }

  @Test
  public void toggleArchiveItemFalseTestWithException() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.ARCHIVE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("itemSku", ItemControllerTest.ITEM_SKU_INVALID)
                .param("doArchive",ItemControllerTest.DO_ARCHIVE_FALSE.toString()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.ARCHIVE_ITEM.getCode())))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemService).toggleArchiveItem(ItemControllerTest.STORE_ID, ItemControllerTest.ITEM_SKU_INVALID,
        ItemControllerTest.USERNAME, ItemControllerTest.DO_ARCHIVE_FALSE);
  }

  @Test
  public void getItemTest() throws Exception {
    this.mockMvc
        .perform(
            get(ProductApiPath.ITEM + ProductApiPath.GET).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("itemSku", ItemControllerTest.ITEM_SKU)
                .param("level2MerchantCode", ItemControllerTest.LEVEL2_MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()));

    verify(this.itemService).getItem(ItemControllerTest.STORE_ID, ItemControllerTest.REQUEST_ID,
        ItemControllerTest.USERNAME, ItemControllerTest.ITEM_SKU,
        ItemControllerTest.NEED_MASTER_DATA_DETAIL);
    verify(this.modelConverter).convertToItemResponse(this.item);
  }

  @Test
  public void isPickupPointCodeUsedTest() throws Exception {
    when(this.productSearchService.isPickupPointCodeUsed(ItemControllerTest.STORE_ID,
        ItemControllerTest.PICKUP_POINT_CODE)).thenReturn(true);
    this.mockMvc
        .perform(
            get(ProductApiPath.ITEM + ProductApiPath.IS_PICKUP_POINT_CODE_USED).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("pickupPointCode", ItemControllerTest.PICKUP_POINT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.value.result", equalTo(true)));
    verify(this.productSearchService).isPickupPointCodeUsed(ItemControllerTest.STORE_ID,
        ItemControllerTest.PICKUP_POINT_CODE);
  }

  @Test
  public void getItemTestWithException() throws Exception {
    this.mockMvc
        .perform(
            get(ProductApiPath.ITEM + ProductApiPath.GET).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.BLANK)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("itemSku", ItemControllerTest.ITEM_SKU)
                .param("level2MerchantCode", ItemControllerTest.LEVEL2_MERCHANT_CODE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_ITEM.getCode())))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", equalTo(null)));

    verify(this.itemService).getItem(ItemControllerTest.BLANK, ItemControllerTest.REQUEST_ID,
        ItemControllerTest.USERNAME, ItemControllerTest.ITEM_SKU,
        ItemControllerTest.NEED_MASTER_DATA_DETAIL);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.itemController).build();

    ObjectMapper objectMapper = new ObjectMapper();

    this.jsonRequestItemSkus =
        FileUtils.readFileToString(new File("src/test/resources/itemSkus.json"));

    this.itemRequestJson =
        FileUtils.readFileToString(new File("src/test/resources/itemRequest.json"));

    this.itemRequest =
        objectMapper.readValue(this.itemRequestJson,
            objectMapper.getTypeFactory().constructType(ItemRequest.class));

    this.pristineIds = Arrays.asList(PRISTINE_ID, PRISTINE_ID2);
    this.pristineIdsJson = objectMapper.writeValueAsString(pristineIds);

    Assertions.assertNotNull(this.itemRequest);

    List<PristineDataItemDto> pristineDataItemDtoList = new ArrayList<>();
    PristineDataItemDto pristineDataItemDto = new PristineDataItemDto();
    pristineDataItemDto.setPristineId(PRISTINE_ID);
    pristineDataItemDtoList.add(pristineDataItemDto);
    pickupPointUpdateRequest = new PickupPointUpdateRequest();
    pickupPointUpdateRequest.setProductSku(PRODUCT_SKU);
    pickupPointUpdateRequest.setPickupPointUpdateItemRequestList(Arrays.asList(new PickupPointUpdateItemRequest()));
    this.pristineDataItemDTOJson = objectMapper.writeValueAsString(pristineDataItemDtoList);
    this.pickupPointUpdateRequestJson = objectMapper.writeValueAsString(pickupPointUpdateRequest);

    this.itemDTOJson = FileUtils.readFileToString(new File("src/test/resources/itemDTO.json"));

    this.skuListJson =
        FileUtils.readFileToString(new File("src/test/resources/itemSkuArrayList.json"),
            String.valueOf(StandardCharsets.UTF_8));

    this.itemDTO =
        objectMapper.readValue(this.itemDTOJson,
            objectMapper.getTypeFactory().constructType(ItemDTO.class));
    Assertions.assertNotNull(this.itemDTO);

    this.item = new Item();

    Item itemForGetItem = new Item();
    itemForGetItem.setItemSku(ItemControllerTest.ITEM_SKU);

    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setItemSku(ItemControllerTest.ITEM_SKU);

    when(this.modelConverter.convertItemDTOToItem(this.itemDTO)).thenReturn(this.itemRequestVO);

    when(this.modelConverter.convertToItem(this.itemRequest)).thenReturn(this.item);

    doThrow(new Exception(ItemControllerTest.STORE_ID_MUST_NOT_BE_BLANK)).when(this.itemService)
        .addItem(ItemControllerTest.BLANK, ItemControllerTest.REQUEST_ID,
            ItemControllerTest.USERNAME, ItemControllerTest.PRODUCT_SKU, this.itemRequestVO);

    doThrow(new ApplicationRuntimeException()).when(this.itemService).getItem(
        ItemControllerTest.BLANK, ItemControllerTest.REQUEST_ID, ItemControllerTest.USERNAME,
        ItemControllerTest.ITEM_SKU, ItemControllerTest.NEED_MASTER_DATA_DETAIL);

    when(this.modelConverter.convertToItemResponse(this.item)).thenReturn(itemResponse);

    when(this.itemService.deleteItem(ItemControllerTest.STORE_ID, ItemControllerTest.ITEM_SKU))
        .thenReturn(true);

    when(this.itemService.toggleArchiveItem(ItemControllerTest.STORE_ID, ItemControllerTest.ITEM_SKU,
        ItemControllerTest.USERNAME, ItemControllerTest.DO_ARCHIVE_TRUE)).thenReturn(new EditItemResponse());

    when(this.itemService.toggleArchiveItem(ItemControllerTest.STORE_ID, ItemControllerTest.ITEM_SKU,
        ItemControllerTest.USERNAME, ItemControllerTest.DO_ARCHIVE_FALSE)).thenReturn(new EditItemResponse());

    doThrow(new ApplicationRuntimeException()).when(this.itemService).deleteItem(
        ItemControllerTest.STORE_ID, ItemControllerTest.ITEM_SKU_INVALID);

    doThrow(new ApplicationRuntimeException()).when(this.itemService)
        .toggleArchiveItem(ItemControllerTest.STORE_ID, ItemControllerTest.ITEM_SKU_INVALID,
            ItemControllerTest.USERNAME, ItemControllerTest.DO_ARCHIVE_TRUE);

    doThrow(new ApplicationRuntimeException()).when(this.itemService)
        .toggleArchiveItem(ItemControllerTest.STORE_ID, ItemControllerTest.ITEM_SKU_INVALID,
            ItemControllerTest.USERNAME, ItemControllerTest.DO_ARCHIVE_FALSE);


    when(this.itemService.updateItem(ItemControllerTest.STORE_ID, this.item, USERNAME, false, false, false)).thenReturn(
        item);
    doThrow(new ApplicationRuntimeException()).when(this.itemService).updateItem(
        ItemControllerTest.BLANK, this.item, USERNAME, false, false, false);

    when(
        this.itemService.getItem(ItemControllerTest.STORE_ID, ItemControllerTest.REQUEST_ID,
            ItemControllerTest.USERNAME, ItemControllerTest.ITEM_SKU,
            ItemControllerTest.NEED_MASTER_DATA_DETAIL)).thenReturn(this.item);
    itemViewConfigAndItemSkuRequest.setBuyable(false);
    itemViewConfigAndItemSkuRequest.setDiscoverable(false);
    itemViewConfigAndItemSkuRequest.setItemSku(ITEM_SKU);

    itemViewConfig.setBuyable(false);
    itemViewConfig.setDiscoverable(false);

    this.simpleSetRequestJson =
        FileUtils.readFileToString(new File("src/test/resources/itemSkuList.json"));
    this.simpleSetRequest =
        objectMapper.readValue(this.simpleSetRequestJson,
            objectMapper.getTypeFactory()
                .constructType(SimpleSetStringRequest.class));
    defaultPickupPointRequest = new DefaultPickupPointRequest();
    defaultPickupPointRequest.setRepublish(false);
    defaultPickupPointRequest.setSkus(Arrays.asList(ITEM_SKU));
    productAndItemPickupPointDTO.setProduct(new Product());
    productAndItemPickupPointDTO.getProduct().setVersion(1L);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.modelConverter);
    verifyNoMoreInteractions(this.itemViewConfigService);
    verifyNoMoreInteractions(this.productSearchService);
  }

  @Test
  public void publishAllItemsTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.PUBLISH_ALL_ITEMS)
               .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
               .param("storeId", ItemControllerTest.STORE_ID)
               .param("channelId", ItemControllerTest.CHANNEL_ID)
               .param("clientId", ItemControllerTest.CLIENT_ID)
               .param("requestId", ItemControllerTest.REQUEST_ID)
               .param("username", ItemControllerTest.USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.asyncProcessor).submitWithBackoff(eq(AgpConstant.COMMAND_PUBLISH_ALL_ITEMS),any(Runnable.class));
  }

  @Test
  public void republishItemsToAgpTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.REPUBLISH_ITEMS_TO_AGP)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.jsonRequestItemSkus)
                .param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.asyncProcessor).submitWithBackoff(eq(AgpConstant.COMMAND_REPUBLISH_ITEMS_TO_AGP),any(Runnable.class));
  }

  @Test
  public void updateItemTest() throws Exception {
    when(this.itemService.updateItem(Mockito.anyString(), Mockito.any(Item.class), Mockito.anyString(), eq(false), eq(false), eq(false)))
        .thenReturn(item);
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.UPDATE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(this.itemRequestJson)
                .param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("isOnlyExternal", String.valueOf(false))).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.modelConverter).convertToItem(Mockito.any(ItemRequest.class));
    verify(this.modelConverter).convertToItemResponse(Mockito.isNull());
    verify(this.itemService).updateItem(ItemControllerTest.STORE_ID, null, USERNAME, false, false,
      false);
  }

  @Test
  public void updateItemTestWithException() throws Exception {
    Mockito.when(this.modelConverter.convertToItem(Mockito.any(ItemRequest.class)))
      .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.UPDATE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(this.itemRequestJson)
                .param("storeId", ItemControllerTest.BLANK)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("isOnlyExternal", String.valueOf(false))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.modelConverter).convertToItem(Mockito.any(ItemRequest.class));
  }

  @Test
  public void updateItemPristineDataTest() throws Exception {

    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.UPDATE_ITEM_PRISTINE_DATA)
        .content(this.pristineDataItemDTOJson).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
        .param("channelId", ItemControllerTest.CHANNEL_ID)
        .param("clientId", ItemControllerTest.CLIENT_ID)
        .param("requestId", ItemControllerTest.REQUEST_ID)
        .param("username", ItemControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo("")))
        .andExpect(jsonPath("$.errorMessage", equalTo("")))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.modelConverter).convertToPristineDataItem(Mockito.any(PristineDataItemDto.class));
    verify(this.itemService)
        .updateItemsPristineData(Mockito.eq(ItemControllerTest.STORE_ID), Mockito.anyMap());
  }

  @Test
  public void updateItemPristineDataTestWithException() throws Exception {
    doThrow(RuntimeException.class).when(this.itemService)
        .updateItemsPristineData(Mockito.eq(ItemControllerTest.STORE_ID), Mockito.anyMap());
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.UPDATE_ITEM_PRISTINE_DATA)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(this.pristineDataItemDTOJson).param("storeId", ItemControllerTest.STORE_ID)
        .param("channelId", ItemControllerTest.CHANNEL_ID)
        .param("clientId", ItemControllerTest.CLIENT_ID)
        .param("requestId", ItemControllerTest.REQUEST_ID)
        .param("username", ItemControllerTest.USERNAME)).andExpect(status().isOk()).andExpect(
        jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.UPDATE_PRISTINE_DATA.getCode())))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.modelConverter).convertToPristineDataItem(Mockito.any(PristineDataItemDto.class));
    verify(this.itemService)
        .updateItemsPristineData(Mockito.eq(ItemControllerTest.STORE_ID), Mockito.anyMap());
  }


  @Test
  public void updateSalesCatalogForAllPristineProduct_whenSuccess() throws Exception {
    when(itemService.updateSalesCatalogForPristineProducts(anyList())).thenReturn(true);
    this.mockMvc.perform(
        put(ProductApiPath.ITEM + ProductApiPath.UPDATE_SALES_CATALOG_FOR_PRISTINE_PRODUCT)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ItemControllerTest.BLANK)
            .param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID)
            .content(pristineIdsJson))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(itemService).updateSalesCatalogForPristineProducts(pristineIds);
  }

  @Test
  public void updateSalesCatalogForAllPristineProduct_whenPristineIdsBlank() throws Exception {
    when(itemService.updateSalesCatalogForPristineProducts(Mockito.isNull())).thenReturn(true);
    this.mockMvc.perform(
        put(ProductApiPath.ITEM + ProductApiPath.UPDATE_SALES_CATALOG_FOR_PRISTINE_PRODUCT)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ItemControllerTest.BLANK)
            .param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(itemService).updateSalesCatalogForPristineProducts(Mockito.isNull());
  }

  @Test
  public void updateSalesCatalogForAllPristineProduct_whenFailed() throws Exception {
    when(itemService.updateSalesCatalogForPristineProducts(anyList())).thenReturn(false);
    this.mockMvc.perform(
        put(ProductApiPath.ITEM + ProductApiPath.UPDATE_SALES_CATALOG_FOR_PRISTINE_PRODUCT)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ItemControllerTest.BLANK)
            .param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID)
            .content(pristineIdsJson))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(itemService).updateSalesCatalogForPristineProducts(pristineIds);
  }

  @Test
  public void getMapForPristineCategoryAttribute_whenSuccess() throws Exception {
    when(itemService.getMapForPristineCategoryAttribute()).thenReturn(new HashMap<>());
    this.mockMvc.perform(
        get(ProductApiPath.ITEM + ProductApiPath.MAP_FOR_PRISTINE_CATEGORY_ATTRIBUTE)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(itemService).getMapForPristineCategoryAttribute();
  }

  @Test
  public void getMapForPristineCategoryAttribute_whenFailed() throws Exception {
    when(itemService.getMapForPristineCategoryAttribute()).thenThrow(RuntimeException.class);
    this.mockMvc.perform(
        get(ProductApiPath.ITEM + ProductApiPath.MAP_FOR_PRISTINE_CATEGORY_ATTRIBUTE)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(itemService).getMapForPristineCategoryAttribute();
  }
  
  @Test
  public void updateResignMerchantItemsByMerchantCodeTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.UPDATE_RESIGN_MERCHANT_ITEMS_BY_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("merchantCode", ItemControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));

    verify(this.itemService).updateResignMerchantItemsByMerchantCode(ItemControllerTest.STORE_ID,
        ItemControllerTest.REQUEST_ID, ItemControllerTest.USERNAME,
        ItemControllerTest.MERCHANT_CODE);
  }

  @Test
  public void updateResignMerchantItemsByMerchantCodeTestWithException() throws Exception {
    doThrow(ApplicationRuntimeException.class).when(this.itemService).updateResignMerchantItemsByMerchantCode(
        ItemControllerTest.BLANK, ItemControllerTest.REQUEST_ID, ItemControllerTest.USERNAME,
        ItemControllerTest.MERCHANT_CODE);

    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.UPDATE_RESIGN_MERCHANT_ITEMS_BY_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemControllerTest.BLANK)
                .param("channelId", ItemControllerTest.CHANNEL_ID)
                .param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("username", ItemControllerTest.USERNAME)
                .param("merchantCode", ItemControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode",
            equalTo(ProductErrorCodesEnum.UPDATE_RESIGN_MERCHANT_ITEMS_BY_MERCHANT_CODE.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));

    verify(this.itemService).updateResignMerchantItemsByMerchantCode(ItemControllerTest.BLANK,
        ItemControllerTest.REQUEST_ID, ItemControllerTest.USERNAME,
        ItemControllerTest.MERCHANT_CODE);
  }

  @Test
  public void testGetItemsAndBundlingInfoSuccess() throws Exception {
    ItemAndBundlingInfoVO itemAndBundlingInfoVO = new ItemAndBundlingInfoVO();
    ItemAndBundlingInfoResponse itemAndBundlingInfoResponse = new ItemAndBundlingInfoResponse();
    Set<String> promoBundlingIds = new HashSet<>();
    promoBundlingIds.add(PROMO_BUNDLING_ID);
    Set<String> itemSkus = new HashSet();
    itemSkus.add(ITEM_SKU);
    ItemAndBundlingInfoRequest request = new ItemAndBundlingInfoRequest();
    request.setItemSkus(itemSkus);
    request.setPromoBundlingIds(promoBundlingIds);
    ObjectMapper objectMapper = new ObjectMapper();

    String content = objectMapper.writeValueAsString(request);
    when(itemService
        .getItemsAndBundlingInfo(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID),
            eq(itemSkus), eq(promoBundlingIds),eq(USERNAME))).thenReturn(itemAndBundlingInfoVO);
    when(modelConverter.convertPromoItemVOToPromoItemResponse(itemAndBundlingInfoVO))
        .thenReturn(itemAndBundlingInfoResponse);

    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.GET_ITEMS_AND_BUNDLING_INFO).content(content)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
        .param("channelId", ItemControllerTest.CHANNEL_ID)
        .param("clientId", ItemControllerTest.CLIENT_ID)
        .param("requestId", ItemControllerTest.REQUEST_ID)
        .param("username", ItemControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.items", equalTo(itemAndBundlingInfoResponse.getItems())))
        .andExpect(jsonPath("$.value.promoBundlings",
            equalTo(itemAndBundlingInfoResponse.getPromoBundlings())))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));

    verify(itemService, times(1))
        .getItemsAndBundlingInfo(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID),
            eq(itemSkus), eq(promoBundlingIds), eq(USERNAME));
    verify(modelConverter, times(1)).convertPromoItemVOToPromoItemResponse(itemAndBundlingInfoVO);
  }

  @Test
  public void testGetItemsAndBundlingInfoThrowException() throws Exception {
    Set<String> promoBundlingIds = new HashSet<>();
    promoBundlingIds.add(PROMO_BUNDLING_ID);
    Set<String> itemSkus = new HashSet();
    itemSkus.add(ITEM_SKU);
    ItemAndBundlingInfoRequest request = new ItemAndBundlingInfoRequest();
    request.setItemSkus(itemSkus);
    request.setPromoBundlingIds(promoBundlingIds);
    ObjectMapper objectMapper = new ObjectMapper();

    String content = objectMapper.writeValueAsString(request);
    when(itemService
        .getItemsAndBundlingInfo(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID),
            eq(itemSkus), eq(promoBundlingIds), eq(USERNAME))).thenThrow(new Exception());

    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.GET_ITEMS_AND_BUNDLING_INFO).content(content)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
        .param("channelId", ItemControllerTest.CHANNEL_ID)
        .param("clientId", ItemControllerTest.CLIENT_ID)
        .param("requestId", ItemControllerTest.REQUEST_ID)
        .param("username", ItemControllerTest.USERNAME)
        .param("itemSkus", ItemControllerTest.ITEM_SKU)
        .param("promoBundlingIds", ItemControllerTest.PROMO_BUNDLING_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_ITEM.getCode())))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));

    verify(itemService, times(1))
        .getItemsAndBundlingInfo(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID),
            eq(itemSkus), eq(promoBundlingIds), eq(USERNAME));
  }

  @Test
  public void getItemSkusByItemCodeTest() throws Exception {
    ItemPriceVO itemPriceVO = new ItemPriceVO.ItemPriceBuilder()
      .setOfferPrice(OFFER_PRICE)
      .setListPrice(NORMAL_PRICE)
      .setItemSku(ITEM_SKU).build();
    ItemPriceResponse itemPriceResponse = new ItemPriceResponse.ItemPriceResponseBuilder()
        .setOfferPrice(OFFER_PRICE)
        .setListPrice(NORMAL_PRICE)
        .setItemSku(ITEM_SKU).build();
    List<ItemPriceVO> itemPriceVOS = new ArrayList<>();
    itemPriceVOS.add(itemPriceVO);
    List<ItemPriceResponse> itemPriceResponses = new ArrayList<>();
    itemPriceResponses.add(itemPriceResponse);
    when(modelConverter.convertItemPriceVoToItemPriceListResponse(Mockito.anyList())).thenReturn(itemPriceResponses);
    when(itemService.getAllItemSkuByItemCode(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(ITEM_CODE), eq(false)))
        .thenReturn(itemPriceVOS);
    this.mockMvc.perform(
        get(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_SKUS_BY_ITEM_CODE).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("username", ItemControllerTest.USERNAME).param("itemCode", ItemControllerTest.ITEM_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(itemService, times(1)).getAllItemSkuByItemCode(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(ITEM_CODE),
        eq(false));
    verify(modelConverter).convertItemPriceVoToItemPriceListResponse(Mockito.anyList());
  }

  @Test
  public void getItemSkusByItemCodeExceptionTest1() throws Exception {
    when(itemService.getAllItemSkuByItemCode(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(ITEM_CODE), eq(false)))
        .thenThrow(new ApplicationRuntimeException());
    this.mockMvc.perform(
        get(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_SKUS_BY_ITEM_CODE).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("username", ItemControllerTest.USERNAME).param("itemCode", ItemControllerTest.ITEM_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_ITEM.getCode())))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(itemService, times(1)).getAllItemSkuByItemCode(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(ITEM_CODE),
        eq(false));
  }

  @Test
  public void getItemSkusByItemCodeExceptionTest2() throws Exception {
    when(itemService.getAllItemSkuByItemCode(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(ITEM_CODE), eq(false)))
        .thenThrow(new RuntimeException());
    this.mockMvc.perform(
        get(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_SKUS_BY_ITEM_CODE).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("username", ItemControllerTest.USERNAME).param("itemCode", ItemControllerTest.ITEM_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getCode())))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(itemService, times(1)).getAllItemSkuByItemCode(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), eq(ITEM_CODE),
        eq(false));
  }

  @Test
  public void getFirstBuyableDiscoverableItemSkuTest() throws Exception {
    LinkedHashSet<String> itemSkuSet = new LinkedHashSet<>();
    itemSkuSet.add(ITEM_SKU);
    DefaultItemSkuVO response = new DefaultItemSkuVO();
    response.setDefaultItemSku(ITEM_SKU);
    ObjectMapper objectMapper = new ObjectMapper();
    when(itemService.findFirstBuyableDiscoverableItemSkuByPristineId(eq(STORE_ID), eq(itemSkuSet), eq(PRISTINE_ID)))
        .thenReturn(response);
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.GET_FIRST_BUYABLE_DISCOVERABLE_ITEM_SKU_BY_PRISTINE_ID)
        .content(objectMapper.writeValueAsString(itemSkuSet))
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
        .param("pristineId", ItemControllerTest.PRISTINE_ID)
        .param("channelId", ItemControllerTest.CHANNEL_ID)
        .param("clientId", ItemControllerTest.CLIENT_ID)
        .param("requestId", ItemControllerTest.REQUEST_ID)
        .param("username", ItemControllerTest.USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(itemService).findFirstBuyableDiscoverableItemSkuByPristineId(eq(STORE_ID), eq(itemSkuSet), eq(PRISTINE_ID));
  }

  @Test
  public void getFirstBuyableDiscoverableItemSkuTest_ApplicationRumtimeExceptionTest() throws Exception {
    LinkedHashSet<String> itemSkuSet = new LinkedHashSet<>();
    doThrow(ApplicationRuntimeException.class).when(this.itemService)
        .findFirstBuyableDiscoverableItemSkuByPristineId(eq(STORE_ID), eq(itemSkuSet), eq(PRISTINE_ID));
    ObjectMapper objectMapper = new ObjectMapper();
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.GET_FIRST_BUYABLE_DISCOVERABLE_ITEM_SKU_BY_PRISTINE_ID)
        .content(objectMapper.writeValueAsString(itemSkuSet))
        .contentType(MediaType.APPLICATION_JSON)
        .param("pristineId", ItemControllerTest.PRISTINE_ID)
        .param("storeId", ItemControllerTest.STORE_ID)
        .param("channelId", ItemControllerTest.CHANNEL_ID)
        .param("clientId", ItemControllerTest.CLIENT_ID)
        .param("requestId", ItemControllerTest.REQUEST_ID)
        .param("username", ItemControllerTest.USERNAME))
        .andExpect(jsonPath("$.errorMessage",
            equalTo(ProductErrorCodesEnum.GET_FIRST_BUYABLE_DISCOVERABLE_ITEM_SKU.getMessage())))
        .andExpect(jsonPath("$.errorCode",
            equalTo(ProductErrorCodesEnum.GET_FIRST_BUYABLE_DISCOVERABLE_ITEM_SKU.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemService).findFirstBuyableDiscoverableItemSkuByPristineId(eq(STORE_ID), eq(itemSkuSet), eq(PRISTINE_ID));
  }

  @Test
  public void getFirstBuyableDiscoverableItemSkuTest_ExceptionTest() throws Exception {
    LinkedHashSet<String> itemSkuSet = new LinkedHashSet<>();
    doThrow(ApplicationRuntimeException.class).when(this.itemService)
        .findFirstBuyableDiscoverableItemSkuByPristineId(eq(STORE_ID), eq(itemSkuSet), eq(PRISTINE_ID));
    ObjectMapper objectMapper = new ObjectMapper();
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.GET_FIRST_BUYABLE_DISCOVERABLE_ITEM_SKU_BY_PRISTINE_ID)
        .content(objectMapper.writeValueAsString(itemSkuSet))
        .contentType(MediaType.APPLICATION_JSON)
        .param("pristineId", ItemControllerTest.PRISTINE_ID)
        .param("storeId", ItemControllerTest.STORE_ID)
        .param("channelId", ItemControllerTest.CHANNEL_ID)
        .param("clientId", ItemControllerTest.CLIENT_ID)
        .param("requestId", ItemControllerTest.REQUEST_ID)
        .param("username", ItemControllerTest.USERNAME))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemService).findFirstBuyableDiscoverableItemSkuByPristineId(eq(STORE_ID), eq(itemSkuSet), eq(PRISTINE_ID));
  }

  @Test
  public void updatePromoPriceForSkuList() throws Exception {
    doNothing().when(itemService).updatePromotionPriceForItemSkuList(eq(STORE_ID), Mockito.anySet());
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.UPDATE_PROMOTION_PRICE_FOR_SKU_LIST)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(skuListJson)
        .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
        .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
        .param("username", ItemControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(itemService, times(1)).updatePromotionPriceForItemSkuList(eq(STORE_ID), Mockito.anySet());
  }

  @Test
  public void updatePromoPriceForSkuListException() throws Exception {
    doThrow(new RuntimeException()).when(itemService)
        .updatePromotionPriceForItemSkuList(eq(STORE_ID), Mockito.anySet());
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.UPDATE_PROMOTION_PRICE_FOR_SKU_LIST)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(skuListJson)
        .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
        .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
        .param("username", ItemControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(itemService, times(1)).updatePromotionPriceForItemSkuList(eq(STORE_ID), Mockito.anySet());
  }

  @Test
  public void updateItemPromotionActiveFlagTest() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    List<String> itemSkus = Collections.singletonList(ItemControllerTest.ITEM_SKU);
    when(itemService.updateItemFlashSaleActiveFlag(ItemControllerTest.STORE_ID, itemSkus, true))
        .thenReturn(true);
    this.mockMvc.perform(put(ProductApiPath.ITEM + ProductApiPath.UPDATE_ITEM_FLASH_SALE_ACTIVE_FLAG)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
        .param("channelId", ItemControllerTest.CHANNEL_ID)
        .param("clientId", ItemControllerTest.CLIENT_ID)
        .param("requestId", ItemControllerTest.REQUEST_ID).param("isFlashSaleActive", "true")
        .content(objectMapper.writeValueAsString(itemSkus))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(itemService).updateItemFlashSaleActiveFlag(ItemControllerTest.STORE_ID, itemSkus, true);
  }

  @Test
  public void updateItemViewConfigAndForceReviewTest() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    when(this.itemViewConfigService.updateItemViewConfigAndForceReview(eq(STORE_ID), Mockito.anyList(),
        eq(true), eq(true), eq(null), eq(true))).thenReturn(Collections.singletonList(item));
    when(this.modelConverter.convertToItemViewConfig(itemViewConfigAndItemSkuRequest)).thenReturn(itemViewConfig);
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.UPDATE_VIEW_CONFIG_AND_FORCE_REVIEW).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(Collections.singletonList(itemViewConfigAndItemSkuRequest)))
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("username", ItemControllerTest.USERNAME).param("forceReview", String.valueOf(true))
            .param("isArchive", String.valueOf(true)).param("scheduleRemoval", String.valueOf(true)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemViewConfigService).updateItemViewConfigAndForceReview(eq(STORE_ID), Mockito.anyList(), eq(true),
        eq(true), eq(null), eq(true));
    verify(this.productSearchService).updateForceReviewForProduct(STORE_ID, Collections.singletonList(item), true,
        true);
    verify(itemService).publishUpdateToSolrEvent(null, Collections.singletonList(item));
  }

  @Test
  public void updateItemViewConfigAndForceReview_exceptionTest() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    when(this.itemViewConfigService.updateItemViewConfigAndForceReview(eq(STORE_ID), Mockito.anyList(),
        eq(true), eq(false), eq(null),  eq(false))).thenThrow(Exception.class);
    when(this.modelConverter.convertToItemViewConfig(itemViewConfigAndItemSkuRequest)).thenReturn(itemViewConfig);
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.UPDATE_VIEW_CONFIG_AND_FORCE_REVIEW).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(Collections.singletonList(itemViewConfigAndItemSkuRequest)))
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("username", ItemControllerTest.USERNAME).param("forceReview", String.valueOf(true))
            .param("isArchive", String.valueOf(false)).param("scheduleRemoval", String.valueOf(false))).
            andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemViewConfigService).updateItemViewConfigAndForceReview(eq(STORE_ID), Mockito.anyList(), eq(true),
        eq(false), eq(null), eq(false));
  }

  @Test
  public void getItemsByItemCodeTest() throws Exception {
    Mockito.when(this.productSearchService
        .findMasterDataWithProductAndItemsInfoByItemCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_CODE, true))
        .thenReturn(new MasterDataDetailWithProductAndItemsResponseVo());
    when(this.modelConverter
        .convertToMasterDataDetailResponse(Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class)))
        .thenReturn(new MasterDataDetailWithProductAndItemsResponse());

    this.mockMvc.perform(
        get(ProductApiPath.ITEM + ProductApiPath.GET_ITEMS_BY_ITEMCODE, ITEM_CODE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME)
            .param("forceReview", String.valueOf(true))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.productSearchService)
        .findMasterDataWithProductAndItemsInfoByItemCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_CODE, true);
    verify(this.modelConverter)
        .convertToMasterDataDetailResponse(Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class));
  }

  @Test
  public void getItemsByItemCodeTest_exceptionTest() throws Exception {
    Mockito.when(this.productSearchService
        .findMasterDataWithProductAndItemsInfoByItemCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_CODE, true))
        .thenThrow(Exception.class);
    this.mockMvc.perform(
        get(ProductApiPath.ITEM + ProductApiPath.GET_ITEMS_BY_ITEMCODE, ITEM_CODE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME)
            .param("forceReview", String.valueOf(true))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.productSearchService)
        .findMasterDataWithProductAndItemsInfoByItemCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_CODE, true);
  }

  @Test
  public void getItemSkusByPristineIdTest() throws Exception {
    Mockito.when(this.productSearchService.getItemPriceByPristineId(STORE_ID, USERNAME, REQUEST_ID, PRISTINE_ID))
        .thenReturn(Arrays.asList(new ItemPriceVO()));
    Mockito.when(this.modelConverter.convertItemPriceVoToItemPriceListResponse(Arrays.asList(new ItemPriceVO())))
        .thenReturn(Arrays.asList(new ItemPriceResponse()));
    this.mockMvc.perform(
        get(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_SKUS_BY_PRISTINE_ID).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME)
            .param("pristineId", PRISTINE_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getItemPriceByPristineId(STORE_ID, USERNAME, REQUEST_ID, PRISTINE_ID);
    verify(this.modelConverter).convertItemPriceVoToItemPriceListResponse(Arrays.asList(new ItemPriceVO()));
  }

  @Test
  public void getItemSkusByPristineIdExceptionTest() throws Exception {
    Mockito.when(this.productSearchService.getItemPriceByPristineId(STORE_ID, USERNAME, REQUEST_ID, PRISTINE_ID))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(
        get(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_SKUS_BY_PRISTINE_ID).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME)
            .param("pristineId", PRISTINE_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getItemPriceByPristineId(STORE_ID, USERNAME, REQUEST_ID, PRISTINE_ID);
  }

  @Test
  public void updatePickupPointsTest() throws Exception {
    when(itemService.updatePickupPoints(STORE_ID, pickupPointUpdateRequest, false)).thenReturn(productAndItemPickupPointDTO);
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.UPDATE_PICKUP_POINT_CODES).accept(MediaType.APPLICATION_JSON)
            .content(this.pickupPointUpdateRequestJson).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME)).
        andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(itemService, times(1)).updatePickupPoints(STORE_ID, pickupPointUpdateRequest, false);
  }

  @Test
  public void updatePickupPointsExceptionTest() throws Exception {
    doThrow(new RuntimeException()).when(itemService)
        .updatePickupPoints(STORE_ID, pickupPointUpdateRequest, false);
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.UPDATE_PICKUP_POINT_CODES).content(this.pickupPointUpdateRequestJson)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("username", ItemControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(itemService, times(1)).updatePickupPoints(STORE_ID, pickupPointUpdateRequest, false);
  }

  @Test
  public void getItemPickupPointCodeByProductSkuTest() throws Exception {
    Mockito
        .when(this.itemSummaryService.getItemPickupPointsAndItemNameByProductSku(STORE_ID, PRODUCT_SKU, 0, 10, false))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(this.modelConverter.getItemPickupPointCodeResponseFromItemPickupPointVo(new ArrayList<>()))
        .thenReturn(new ArrayList<>());
    this.mockMvc.perform(get(ProductApiPath.ITEM + ProductApiPath.GET_ITEMS_PICKUP_POINT_CODE, PRODUCT_SKU)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
        .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
        .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME)
        .param("page", "0").param("size", "10").param("fbbActivated", String.valueOf(false))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemSummaryService).getItemPickupPointsAndItemNameByProductSku(STORE_ID, PRODUCT_SKU, 0, 10, false);
    verify(this.modelConverter).getItemPickupPointCodeResponseFromItemPickupPointVo(new ArrayList<>());
  }

  @Test
  public void getItemPickupPointCodeByProductSkuExceptionTest() throws Exception {
    Mockito.when(this.itemSummaryService.getItemPickupPointsAndItemNameByProductSku(STORE_ID, PRODUCT_SKU, 0, 10, false))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(get(ProductApiPath.ITEM + ProductApiPath.GET_ITEMS_PICKUP_POINT_CODE, PRODUCT_SKU)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
        .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
        .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME)
        .param("page", "0").param("size", "10").param("fbbActivated", String.valueOf(false))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemSummaryService).getItemPickupPointsAndItemNameByProductSku(STORE_ID, PRODUCT_SKU, 0, 10, false);
  }

  @Test
  public void updateContentChangeTest() throws Exception {
    doNothing().when(itemService).updateContentChange(STORE_ID, PRODUCT_SKU, true, false);
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.UPDATE_CONTENT_CHANGE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("username", ItemControllerTest.USERNAME).param("productSku", ItemControllerTest.PRODUCT_SKU)
            .param("contentChange", String.valueOf(true))).
        andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(itemService, times(1)).updateContentChange(STORE_ID, PRODUCT_SKU, true, false);
  }

  @Test
  public void updateContentChangeExceptionTest() throws Exception {
    doThrow(new RuntimeException()).when(itemService).updateContentChange(STORE_ID, PRODUCT_SKU, true, true);
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.UPDATE_CONTENT_CHANGE).content(this.pickupPointUpdateRequestJson)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("username", ItemControllerTest.USERNAME).param("productSku", ItemControllerTest.PRODUCT_SKU)
            .param("contentChange", String.valueOf(true)).param("publishItems", String.valueOf(true)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(itemService).updateContentChange(STORE_ID, PRODUCT_SKU, true, true);
  }

  @Test
  public void getListOfProductByProductSkusTest() throws Exception {
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_IMAGES_BY_ITEM_SKUS).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.simpleSetRequestJson)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("username", ItemControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryService).getItemImagesListResponse(STORE_ID, this.simpleSetRequest.getValue());
  }

  @Test
  public void getListOfProductByProductSkusExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(itemSummaryService)
        .getItemImagesListResponse(STORE_ID, this.simpleSetRequest.getValue());
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_IMAGES_BY_ITEM_SKUS).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.simpleSetRequestJson)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("username", ItemControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())))
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.itemSummaryService).getItemImagesListResponse(STORE_ID, this.simpleSetRequest.getValue());
  }

  @Test
  public void getItemDetailsByItemCodesExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.itemService)
        .getItemDetailsByItemCodes(Mockito.anyString(), Mockito.any());
    String requestBody = mapper.writeValueAsString(new SimpleSetStringRequest());
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_DETAILS_BY_ITEM_CODES).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemService).getItemDetailsByItemCodes(Mockito.anyString(), Mockito.any());
  }

  @Test
  public void getItemDetailsByItemCodesTest() throws Exception {
    String requestBody = mapper.writeValueAsString(new SimpleSetStringRequest());
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_DETAILS_BY_ITEM_CODES).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemService).getItemDetailsByItemCodes(Mockito.anyString(), Mockito.any());
  }

  @Test
  public void updateItemViewConfigWithItemStatusTest() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    ItemViewConfigBaseRequest itemViewConfigBaseRequest =
        ItemViewConfigBaseRequest.builder().isBuyable(false).isDiscoverable(false).build();
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.UPDATE_VIEW_CONFIG_WITH_ITEM_STATUS, ITEM_SKU).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(itemViewConfigBaseRequest))
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("username", ItemControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemViewConfigService).updateProductItemViewConfig(eq(STORE_ID), anyString(), eq(false),
        eq(false));
  }

  @Test
  public void updateItemViewConfigWithItemStatusExceptionTest() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    ItemViewConfigBaseRequest itemViewConfigBaseRequest =
        ItemViewConfigBaseRequest.builder().isBuyable(false).isDiscoverable(false).build();
    doThrow(new Exception()).when(this.itemViewConfigService).updateProductItemViewConfig(eq(STORE_ID), anyString(), eq(false),
        eq(false));
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.UPDATE_VIEW_CONFIG_WITH_ITEM_STATUS, ITEM_SKU).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(itemViewConfigBaseRequest))
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("username", ItemControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemViewConfigService).updateProductItemViewConfig(eq(STORE_ID), anyString(), eq(false),
        eq(false));
  }

  @Test
  public void updateProductTypeExceptionTest() throws Exception {
    ProductTypeEditRequest request =
        ProductTypeEditRequest.builder().productType(ProductType.BOPIS).contentChanged(true).productCode(PRODUCT_SKU)
            .build();
    Mockito.doThrow(Exception.class).when(this.itemService).updateProductTypeOrContentChange(request, STORE_ID);
    String requestBody = mapper.writeValueAsString(request);
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.UPDATE_PRODUCT_TYPE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemService).updateProductTypeOrContentChange(request, STORE_ID);
  }

  @Test
  public void updateProductTypeApplicationExceptionTest() throws Exception {
    ProductTypeEditRequest request =
      ProductTypeEditRequest.builder().productType(ProductType.BOPIS).contentChanged(true).productCode(PRODUCT_SKU)
        .build();
    doThrow(SolrCustomException.class).when(this.itemService).updateProductTypeOrContentChange(request,
      STORE_ID);
    String requestBody = mapper.writeValueAsString(request);
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.UPDATE_PRODUCT_TYPE).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", ItemControllerTest.STORE_ID)
          .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
          .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME))
      .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemService).updateProductTypeOrContentChange(request, STORE_ID);
  }


  @Test
  public void updateProductTypeTest() throws Exception {
    ProductTypeEditRequest request =
        ProductTypeEditRequest.builder().productType(ProductType.BOPIS).contentChanged(true).productCode(PRODUCT_SKU)
            .build();
    String requestBody = mapper.writeValueAsString(request);
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.UPDATE_PRODUCT_TYPE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemService).updateProductTypeOrContentChange(request, STORE_ID);
  }

  @Test
  public void getItemPickupPointCodeByItemSkusExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.itemPickupPointService)
        .findPickUpPointCodeByItemSkuInAndDelivery(eq(ItemControllerTest.STORE_ID), Mockito.anyList(), eq(true));
    String requestBody = mapper.writeValueAsString(new SimpleListStringRequest());
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.GET_ITEMS_PICKUP_POINT_CODE_BY_ITEM_SKU).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemPickupPointService).findPickUpPointCodeByItemSkuInAndDelivery(eq(ItemControllerTest.STORE_ID), Mockito.anyList(), eq(true));
  }

  @Test
  public void getItemPickupPointCodeByItemSkusTest() throws Exception {
    String requestBody = mapper.writeValueAsString(new SimpleListStringRequest());
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.GET_ITEMS_PICKUP_POINT_CODE_BY_ITEM_SKU).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(this.itemPickupPointService).findPickUpPointCodeByItemSkuInAndDelivery(eq(ItemControllerTest.STORE_ID), Mockito.anyList(), eq(true));
  }

  @Test
  public void republishItemPickupPointToAgp() throws Exception {
    List<ItemPickupPointRequest> itemPickupPointRequestList = new ArrayList<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequestList.add(itemPickupPointRequest);
    String requestList = mapper.writeValueAsString(itemPickupPointRequestList);
    final Runnable RUNNABLE = new Runnable() {
      @Override
      public void run() {
        itemPickupPointService.republishItemPickupPointToAgp(itemPickupPointRequestList, STORE_ID, true);
      }
    };
    Mockito.doNothing().when(publisherTaskExecutor).execute(RUNNABLE);
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.REPUBLISH_ITEM_PICKUP_POINT_TO_AGP).accept(
            MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestList)
          .param("storeId", ItemControllerTest.STORE_ID)
          .param("channelId", ItemControllerTest.CHANNEL_ID)
          .param("clientId", ItemControllerTest.CLIENT_ID)
          .param("requestId", ItemControllerTest.REQUEST_ID)
          .param("username", ItemControllerTest.USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.errorCode", equalTo(null)))
      .andExpect(jsonPath("$.errorMessage", equalTo(null)))
      .andExpect(jsonPath("$.success", equalTo(true)))
      .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    this.asyncProcessor.submitWithBackoff(AgpConstant.COMMAND_REPUBLISH_PRODUCTS_TO_AGP, RUNNABLE);
    verify(this.asyncProcessor).submitWithBackoff(eq(AgpConstant.COMMAND_REPUBLISH_ITEMS_TO_AGP),
      any(Runnable.class));
  }
  @Test
  public void republishItemPickupPointToAgp_ExceptionTest() throws Exception {
    List<ItemPickupPointRequest> itemPickupPointRequestList = new ArrayList<>();
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.asyncProcessor)
      .submitWithBackoff(eq(AgpConstant.COMMAND_REPUBLISH_ITEMS_TO_AGP), any(Runnable.class));
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.REPUBLISH_ITEM_PICKUP_POINT_TO_AGP).accept(
            MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(mapper.writeValueAsString(itemPickupPointRequestList))
          .param("storeId", ItemControllerTest.STORE_ID)
          .param("channelId", ItemControllerTest.CHANNEL_ID)
          .param("clientId", ItemControllerTest.CLIENT_ID)
          .param("requestId", ItemControllerTest.REQUEST_ID)
          .param("username", ItemControllerTest.USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));

    verify(this.asyncProcessor).submitWithBackoff(eq(AgpConstant.COMMAND_REPUBLISH_ITEMS_TO_AGP),
      any(Runnable.class));
  }
  @Test
  public void getL4ItemListByProductSkuTest() throws Exception {
    Set<String> productSku = new HashSet<>();
    productSku.add(PRODUCT_SKU);
    PageRequest pageRequest = PageRequest.of(0, 10);
    ItemLevel4ListingWebRequest request =
      ItemLevel4ListingWebRequest.builder().productSkus(productSku).build();
    String requestBody = mapper.writeValueAsString(request);
    ItemLevel4ListingResponse listingResponse =
      ItemLevel4ListingResponse.builder().itemSku(ITEM_SKU).productSku(PRODUCT_SKU).brand(BRAND)
        .cncActivated(true).createdDate(new Date()).updatedDate(new Date())
        .itemCatentryId(PRODUCT_CATENTRY_ID).build();
    List<ItemLevel4ListingResponse> responseList = Collections.singletonList(listingResponse);
    Page<ItemLevel4ListingResponse> responsePage = new PageImpl<>(responseList, pageRequest, responseList.size());
    Mockito.when(itemService.getL4ItemListByProductSku(productSku, STORE_ID, 0, 10))
      .thenReturn(responsePage);
    mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.GET_L4_ITEM_LIST_BY_PRODUCT_SKU).accept(
            MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
          .param("storeId", ItemControllerTest.STORE_ID)
          .param("channelId", ItemControllerTest.CHANNEL_ID)
          .param("clientId", ItemControllerTest.CLIENT_ID)
          .param("requestId", ItemControllerTest.REQUEST_ID)
          .param("username", ItemControllerTest.USERNAME).param("page", "0").param("size", "10"))
      .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
      .andExpect(jsonPath("$.errorCode", equalTo(null)))
      .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)))
      .andExpect(jsonPath("$.content", notNullValue()))
      .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(itemService).getL4ItemListByProductSku(request.getProductSkus(), STORE_ID, 0, 10);
    Assertions.assertFalse(responseList.isEmpty());
  }

  @Test
  public void getL5ItemListByProductSkuTest() throws Exception {
    Set<String> productSku = new HashSet<>();
    productSku.add(PRODUCT_SKU);
    ItemLevel4ListingWebRequest request = ItemLevel4ListingWebRequest.builder().productSkus(productSku).build();
    request.setPickupPointCodes(new ArrayList<>());
    request.setPromoTypes(new ArrayList<>());
    String requestBody = mapper.writeValueAsString(request);
    ItemLevel5Response listingL5Response = new ItemLevel5Response();
    listingL5Response.setProductSku(PRODUCT_SKU);
    List<ItemLevel5Response> responseList = Collections.singletonList(listingL5Response);
    Mockito.when(itemService.getL5ItemListByProductSku(STORE_ID, new ArrayList<>(request.getProductSkus()), false,
            new ArrayList<>(), new ArrayList<>(), false, false, FETCH_VIEW_CONFIGS_BY_CHANNEL, false))
        .thenReturn(responseList);
    mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.GET_L5_ITEM_LIST_BY_PRODUCT_SKU).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("username", ItemControllerTest.USERNAME)
            .param("fetchViewConfigByChannel", ItemControllerTest.FETCH_VIEW_CONFIGS_BY_CHANNEL)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE))).andExpect(jsonPath("$.content", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(itemService).getL5ItemListByProductSku(STORE_ID, new ArrayList<>(request.getProductSkus()), false,
        new ArrayList<>(), new ArrayList<>(), false, false, FETCH_VIEW_CONFIGS_BY_CHANNEL, false);
    Assertions.assertFalse(responseList.isEmpty());
  }

  @Test
  public void getL5ItemListByProductSkuExceptionTest() throws Exception {
    Set<String> productSku = new HashSet<>();
    productSku.add(PRODUCT_SKU);
    ItemLevel4ListingWebRequest request = ItemLevel4ListingWebRequest.builder().productSkus(productSku).build();
    request.setPickupPointCodes(new ArrayList<>());
    request.setPromoTypes(new ArrayList<>());
    String requestBody = mapper.writeValueAsString(request);
    ItemLevel5Response listingL5Response = new ItemLevel5Response();
    listingL5Response.setProductSku(PRODUCT_SKU);
    List<ItemLevel5Response> responseList = Collections.singletonList(listingL5Response);
    Mockito.doThrow(Exception.class).when(this.itemService)
        .getL5ItemListByProductSku(STORE_ID, new ArrayList<>(productSku), false, new ArrayList<>(), new ArrayList<>(),
            false, false, FETCH_VIEW_CONFIGS_BY_CHANNEL, false);
    mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.GET_L5_ITEM_LIST_BY_PRODUCT_SKU).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("username", ItemControllerTest.USERNAME)
            .param("fetchViewConfigByChannel", ItemControllerTest.FETCH_VIEW_CONFIGS_BY_CHANNEL)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())))
        .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(itemService).getL5ItemListByProductSku(STORE_ID, new ArrayList<>(request.getProductSkus()), false,
        new ArrayList<>(), new ArrayList<>(), false, false, FETCH_VIEW_CONFIGS_BY_CHANNEL, false);
  }

  @Test
  public void getL4ItemListByProductSkuExceptionTest() throws Exception {
    Set<String> productSku = new HashSet<>();
    productSku.add(PRODUCT_SKU);
    PageRequest pageRequest = PageRequest.of(0, 10);
    ItemLevel4ListingWebRequest request =
      ItemLevel4ListingWebRequest.builder().productSkus(productSku).build();
    String requestBody = mapper.writeValueAsString(request);
    ItemLevel4ListingResponse listingResponse = new ItemLevel4ListingResponse();
    List<ItemLevel4ListingResponse> responseList = Collections.singletonList(listingResponse);
    Page<ItemLevel4ListingResponse> companies = new PageImpl<>(responseList);
    Mockito.doThrow(Exception.class).when(this.itemService)
      .getL4ItemListByProductSku(productSku, STORE_ID, 0, 10);
    mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.GET_L4_ITEM_LIST_BY_PRODUCT_SKU).accept(
            MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
          .param("storeId", ItemControllerTest.STORE_ID)
          .param("channelId", ItemControllerTest.CHANNEL_ID)
          .param("clientId", ItemControllerTest.CLIENT_ID)
          .param("requestId", ItemControllerTest.REQUEST_ID)
          .param("username", ItemControllerTest.USERNAME).param("page", "0").param("size", "10"))
      .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
      .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())))
      .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)))
      .andExpect(jsonPath("$" + ".requestId", equalTo(ItemControllerTest.REQUEST_ID)));
    verify(itemService).getL4ItemListByProductSku(request.getProductSkus(), STORE_ID, 0, 10);
  }

  @Test
  public void getBasicItemDetailsTest() throws Exception {
    this.mockMvc.perform(
            get(ProductApiPath.ITEM + ProductApiPath.GET_BASIC_ITEM_DETAILS, ITEM_SKU).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME)
                .param("itemSku", ItemControllerTest.ITEM_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true))).andExpect(jsonPath("$.value", equalTo(null)));

    verify(this.itemPickupPointService).getBasicItemDetails(ItemControllerTest.STORE_ID, 0, 20,
        ItemControllerTest.ITEM_SKU);
  }

  @Test
  public void getBasicItemDetailsExceptionTest() throws Exception {

    Mockito.doThrow(ApplicationRuntimeException.class).when(this.itemPickupPointService)
        .getBasicItemDetails(ItemControllerTest.STORE_ID, 0, 20, ItemControllerTest.ITEM_SKU);
    this.mockMvc.perform(
            get(ProductApiPath.ITEM + ProductApiPath.GET_BASIC_ITEM_DETAILS, ITEM_SKU).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME)
                .param("itemSku", ItemControllerTest.ITEM_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())))
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", equalTo(null)));

    verify(this.itemPickupPointService).getBasicItemDetails(ItemControllerTest.STORE_ID, 0, 20,
        ItemControllerTest.ITEM_SKU);
  }

  @Test
  public void getDefaultPickupPointCodeTest() throws Exception {
    String request = mapper.writeValueAsString(defaultPickupPointRequest);
    when(itemPickupPointService.getL5BasedOnItemSkuListAndOnlineOrCncFlagAndRepublishToAgp(STORE_ID, Arrays.asList(ITEM_SKU), false))
        .thenReturn(Arrays.asList(new ItemSkuPickupPointResponse()));
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.GET_DEFAULT_PICKUP_POINT_CODE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.itemPickupPointService).getL5BasedOnItemSkuListAndOnlineOrCncFlagAndRepublishToAgp(
        ItemControllerTest.STORE_ID, Arrays.asList(ITEM_SKU), false);
  }

  @Test
  public void getDefaultPickupPointCodeExceptionTest() throws Exception {
    String request = mapper.writeValueAsString(defaultPickupPointRequest);
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.itemPickupPointService)
        .getL5BasedOnItemSkuListAndOnlineOrCncFlagAndRepublishToAgp(STORE_ID, Arrays.asList(ITEM_SKU), false);
    this.mockMvc.perform(
        post(ProductApiPath.ITEM + ProductApiPath.GET_DEFAULT_PICKUP_POINT_CODE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID).param("username", ItemControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", notNullValue())).andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemPickupPointService).getL5BasedOnItemSkuListAndOnlineOrCncFlagAndRepublishToAgp(
        ItemControllerTest.STORE_ID, Arrays.asList(ITEM_SKU), false);
  }

  @Test
  public void getItemBasicDetailsTest() throws Exception {
    when(itemSummaryService.getItemBasicDetailsByProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(
        Collections.singletonList(new ItemBasicDetailV2Response()));
    this.mockMvc.perform(get(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_BASIC_DETAILS_BY_PRODUCT_SKU).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("productSku", ItemControllerTest.PRODUCT_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryService).getItemBasicDetailsByProductSku(ItemControllerTest.STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void getItemBasicDetailsExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.itemSummaryService)
        .getItemBasicDetailsByProductSku(STORE_ID, PRODUCT_SKU);
    this.mockMvc.perform(get(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_BASIC_DETAILS_BY_PRODUCT_SKU).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("productSku", ItemControllerTest.PRODUCT_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", notNullValue())).andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService).getItemBasicDetailsByProductSku(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void getItemBasicDetailsByItemSkus1Test() throws Exception {
    when(itemSummaryService.getBulkItemDetailsByItemSkus(STORE_ID, FETCH_BUNDLE_RECIPE,
        Collections.singletonList(ITEM_SKU))).thenReturn(Collections.singletonList(new ItemBasicDetailV2Response()));
    String requestBody = mapper.writeValueAsString(new SimpleListStringRequest(Collections.singletonList(ITEM_SKU)));
    this.mockMvc.perform(
            post(ProductApiPath.ITEM + ProductApiPath.GET_BULK_ITEM_DETAIL_BY_ITEM_SKUS).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("fetchBundleRecipe", String.valueOf(ItemControllerTest.FETCH_BUNDLE_RECIPE)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryService).getBulkItemDetailsByItemSkus(STORE_ID, FETCH_BUNDLE_RECIPE,
        Collections.singletonList(ITEM_SKU));
  }

  @Test
  public void getItemBasicDetailsByItemSkusException2Test() throws Exception {
    Mockito.doThrow(Exception.class).when(this.itemSummaryService)
        .getBulkItemDetailsByItemSkus(STORE_ID, FETCH_BUNDLE_RECIPE, Collections.singletonList(ITEM_SKU));
    String requestBody = mapper.writeValueAsString(new SimpleListStringRequest(Collections.singletonList(ITEM_SKU)));
    this.mockMvc.perform(
            post(ProductApiPath.ITEM + ProductApiPath.GET_BULK_ITEM_DETAIL_BY_ITEM_SKUS).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", ItemControllerTest.STORE_ID)
                .param("channelId", ItemControllerTest.CHANNEL_ID).param("clientId", ItemControllerTest.CLIENT_ID)
                .param("requestId", ItemControllerTest.REQUEST_ID)
                .param("fetchBundleRecipe", String.valueOf(ItemControllerTest.FETCH_BUNDLE_RECIPE)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService).getBulkItemDetailsByItemSkus(STORE_ID, FETCH_BUNDLE_RECIPE,
        Collections.singletonList(ITEM_SKU));
  }

  @Test
  public void getItemBasicDetailsByItemCodesTest() throws Exception {
    List<String> itemCodes = new ArrayList<>();
    itemCodes.add(ITEM_CODE);
    String requestBody = mapper.writeValueAsString(new SimpleListStringRequest(itemCodes));
    when(itemSummaryService.getItemBasicDetailsByItemCodes(STORE_ID, itemCodes)).thenReturn(
        Collections.singletonList(new ItemBasicDetailV2Response()));
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_BASIC_DETAILS_BY_ITEM_CODES).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            ).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryService).getItemBasicDetailsByItemCodes(ItemControllerTest.STORE_ID, itemCodes);
  }

  @Test
  public void getItemBasicDetailsByItemCodesExceptionTest() throws Exception {
    List<String> itemCodes = new ArrayList<>();
    String requestBody = mapper.writeValueAsString(new SimpleListStringRequest(itemCodes));
    Mockito.doThrow(Exception.class).when(this.itemSummaryService)
        .getItemBasicDetailsByItemCodes(STORE_ID, itemCodes);
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_BASIC_DETAILS_BY_ITEM_CODES).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("productSku", ItemControllerTest.PRODUCT_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", notNullValue())).andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService).getItemBasicDetailsByItemCodes(STORE_ID, itemCodes);
  }

  @Test
  public void getItemBasicDetailsByItemSkusTest() throws Exception {
    List<String> itemSkus = new ArrayList<>();
    itemSkus.add(ITEM_SKU);
    String requestBody = mapper.writeValueAsString(new SimpleListStringRequest(itemSkus));
    when(itemSummaryService.getItemBasicDetailsByItemSkus(STORE_ID, true, false, false, itemSkus, true, true)).thenReturn(
        Collections.singletonList(new ItemBasicDetailV2Response()));
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_BASIC_DETAILS_BY_ITEM_SKUS).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("inAllProducts", "true").param("needProductData", "false").param("needCategoryData", "false"))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.itemSummaryService).getItemBasicDetailsByItemSkus(ItemControllerTest.STORE_ID, true, false, false,
        itemSkus, true, true);
  }

  @Test
  public void getItemBasicDetailsByItemSkusExceptionTest() throws Exception {
    List<String> itemCodes = new ArrayList<>();
    String requestBody = mapper.writeValueAsString(new SimpleListStringRequest(itemCodes));
    Mockito.doThrow(Exception.class).when(this.itemSummaryService)
        .getItemBasicDetailsByItemSkus(STORE_ID, true, false, false, itemCodes, true, true);
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.GET_ITEM_BASIC_DETAILS_BY_ITEM_SKUS).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID)
            .param("productSku", ItemControllerTest.PRODUCT_SKU).param("inAllProducts", "true")
            .param("needProductData", "false").param("needCategoryData", "false")).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", notNullValue())).andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemSummaryService).getItemBasicDetailsByItemSkus(STORE_ID, true, false, false, itemCodes, true, true);
  }

  @Test
  public void getSharedProductBundleRecipeDetailsTest() throws Exception {
    Set<String> itemCodes = new HashSet<>();
    itemCodes.add(ITEM_CODE);
    String requestBody = mapper.writeValueAsString(new SimpleSetStringRequest(itemCodes));
    when(itemService.getBundleRecipeForSharedItems(STORE_ID, itemCodes)).thenReturn(new ArrayList<>());
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.GET_SHARED_PRODUCT_BUNDLE_RECIPE_BY_ITEM_CODES).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)));
    verify(itemService).getBundleRecipeForSharedItems(STORE_ID, itemCodes);
  }

  @Test
  public void getSharedProductBundleRecipeDetailsExceptionTest() throws Exception {
    Set<String> itemCodes = new HashSet<>();
    itemCodes.add(ITEM_CODE);
    String requestBody = mapper.writeValueAsString(new SimpleSetStringRequest(itemCodes));
    when(itemService.getBundleRecipeForSharedItems(STORE_ID, itemCodes)).thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(post(ProductApiPath.ITEM + ProductApiPath.GET_SHARED_PRODUCT_BUNDLE_RECIPE_BY_ITEM_CODES).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", ItemControllerTest.STORE_ID).param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID).param("requestId", ItemControllerTest.REQUEST_ID))
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(itemService).getBundleRecipeForSharedItems(STORE_ID, itemCodes);
  }

  @Test
  public void fetchBasicItemDetailsByItemCodesExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED))
      .when(this.itemSummaryService)
      .fetchBasicItemDetailsByItemCodes(Mockito.eq(STORE_ID), Mockito.eq(ITEM_CODE),
        Mockito.eq(StringUtils.EMPTY), Mockito.eq(0),
        Mockito.eq(10), Mockito.eq(ITEM_NAME), Mockito.eq("ASC"));
    try {
      this.mockMvc.perform(
          get(ProductApiPath.ITEM + ProductApiPath.FETCH_BASIC_ITEM_DETAILS_BY_ITEM_CODES).accept(
              MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID)
            .param("productSku", ItemControllerTest.PRODUCT_SKU).param("itemCode", ITEM_CODE).param("searchKey",
              ITEM_NAME)
            .param("page", "0").param("size", "10").param("sortBy", ITEM_SKU).param("orderBy", "ASC"))
        .andExpect(status().isOk());
    } finally {
      verify(this.itemSummaryService).fetchBasicItemDetailsByItemCodes(STORE_ID, ITEM_CODE,
        ITEM_NAME, 0, 10, ITEM_SKU, String.valueOf(Sort.Direction.ASC));
    }
  }

  @Test
  public void fetchBasicItemDetailsByItemCodesTest() throws Exception {
    Page<ItemCodeBasicDetailResponse> responsePage = new PageImpl<>(Collections.singletonList(
      ItemCodeBasicDetailResponse.builder().itemName(ITEM_NAME).itemSku(ITEM_SKU).suspended(true)
        .build()), PageRequest.of(1, 10), 200);
    Mockito.when(this.itemSummaryService.fetchBasicItemDetailsByItemCodes(Mockito.eq(STORE_ID),
      Mockito.eq(ITEM_CODE), Mockito.eq(StringUtils.EMPTY), Mockito.eq(0), Mockito.eq(10),
      Mockito.eq(ITEM_SKU),
      Mockito.eq("ASC"))).thenReturn(responsePage);
    try {
      this.mockMvc.perform(
          get(ProductApiPath.ITEM + ProductApiPath.FETCH_BASIC_ITEM_DETAILS_BY_ITEM_CODES).accept(
              MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ItemControllerTest.STORE_ID)
            .param("channelId", ItemControllerTest.CHANNEL_ID)
            .param("clientId", ItemControllerTest.CLIENT_ID)
            .param("requestId", ItemControllerTest.REQUEST_ID)
            .param("productSku", ItemControllerTest.PRODUCT_SKU).param("itemCode", ITEM_CODE)
            .param("page", "0").param("size", "10").param("sortBy", ITEM_SKU).param("orderBy", "ASC"))
        .andExpect(status().isOk());
    } finally {
      verify(this.itemSummaryService).fetchBasicItemDetailsByItemCodes(STORE_ID, ITEM_CODE,
        StringUtils.EMPTY, 0, 10, ITEM_SKU, String.valueOf(Sort.Direction.ASC));
    }
  }

  @Test
  public void getUpcStatusTest() throws Exception {
    Mockito.when(this.itemService.fetchUpcCodeStatus(Mockito.eq(STORE_ID), Mockito.eq(null), Mockito.eq(null))).thenReturn(
        new ArrayList<>());
    String requestBody = mapper.writeValueAsString(new UpcStatusRequest());
    try {
      this.mockMvc.perform(
          post(ProductApiPath.ITEM + ProductApiPath.GET_UPC_CODE_STATUS).accept(
                  MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
          .param("storeId", ItemControllerTest.STORE_ID)
          .param("requestId", ItemControllerTest.REQUEST_ID))
          .andExpect(status().isOk());
    } finally {
      verify(this.itemService).fetchUpcCodeStatus(STORE_ID, null, null);
    }
  }

  @Test
  public void getUpcStatusExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED))
        .when(this.itemService)
        .fetchUpcCodeStatus(Mockito.eq(STORE_ID), Mockito.eq(null), Mockito.eq(null));
    String requestBody = mapper.writeValueAsString(new UpcStatusRequest());
    try {
      this.mockMvc.perform(
              post(ProductApiPath.ITEM + ProductApiPath.GET_UPC_CODE_STATUS).accept(
                      MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
                  .param("storeId", ItemControllerTest.STORE_ID)
                  .param("requestId", ItemControllerTest.REQUEST_ID))
          .andExpect(status().isOk());
    } finally {
      verify(this.itemService).fetchUpcCodeStatus(STORE_ID, null, null);
    }
  }

  @Test
  public void getItemL4LiteTest() throws Exception {
    ItemLevel4ListingWebRequest request = new ItemLevel4ListingWebRequest();
    request.setProductSkus(Collections.singleton(PRODUCT_SKU));
    
    ItemBasicL4Response itemBasicL4Response = new ItemBasicL4Response();
    Page<ItemBasicL4Response> pageResponse = new PageImpl<>(Collections.singletonList(itemBasicL4Response), PageRequest.of(0, 10), 1);

    when(itemService.getL4ItemListByProductSkuLite(anySet(), anyString(), any(), any()))
        .thenReturn(pageResponse);

    this.mockMvc.perform(
            post(ProductApiPath.ITEM + ProductApiPath.GET_BASIC_ITEMS_DETAILS_PAGINATION)
                .contentType(MediaType.APPLICATION_JSON)
                .content(mapper.writeValueAsString(request))
                .param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID)
                .param("username", USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content", notNullValue()));

    verify(itemService).getL4ItemListByProductSkuLite(eq(request.getProductSkus()), eq(STORE_ID), any(), any());
  }

  @Test
  public void getItemL4LiteTest_Exception() throws Exception {
    ItemLevel4ListingWebRequest request = new ItemLevel4ListingWebRequest();
    request.setProductSkus(Collections.singleton(PRODUCT_SKU));

    when(itemService.getL4ItemListByProductSkuLite(anySet(), anyString(), any(), any()))
        .thenThrow(new RuntimeException("Error"));

    this.mockMvc.perform(
            post(ProductApiPath.ITEM + ProductApiPath.GET_BASIC_ITEMS_DETAILS_PAGINATION)
                .contentType(MediaType.APPLICATION_JSON)
                .content(mapper.writeValueAsString(request))
                .param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID)
                .param("username", USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(itemService).getL4ItemListByProductSkuLite(eq(request.getProductSkus()), eq(STORE_ID), any(), any());
  }
}
