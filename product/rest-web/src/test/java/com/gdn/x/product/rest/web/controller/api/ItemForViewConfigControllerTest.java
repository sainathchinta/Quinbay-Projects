package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.io.File;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ItemViewConfigService;
import com.gdn.x.product.service.util.ModelConverter;

public class ItemForViewConfigControllerTest {

  private static final String STORE_ID = "store-id";

  private static final String CHANNEL_ID = "channel-id";

  private static final String CLIENT_ID = "client-id";

  private static final String REQUEST_ID = "request-id";

  private static final String USERNAME = "username";

  private static final String BLANK = "";

  private static final String ITEM_SKU = "item-sku";

  private static final String CHANNEL_NAME = ChannelName.DEFAULT.toString();

  private static final String MERCHANT_CODE = "merchantCode";

  private static final String MERCHANT_SKU = "merchantSku";

  @InjectMocks
  private ItemController itemController;

  @Mock
  private ModelConverter modelConverter;

  @Mock
  private ItemService itemService;

  @Mock
  private ItemViewConfigService itemViewConfigService;

  private MockMvc mockMvc;

  private String jsonRequestItemViewConfig;

  private ItemViewConfigRequest itemViewConfigRequest;

  private ItemViewConfig itemViewConfig;

  @Test
  public void addItemViewConfigByMerchantSkuAndMerchantCodeTest() throws Exception {
    this.mockMvc
        .perform(
            post(
                ProductApiPath.ITEM
                    + ProductApiPath.ADD_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.jsonRequestItemViewConfig)
                .param("storeId", ItemForViewConfigControllerTest.STORE_ID)
                .param("channelId", ItemForViewConfigControllerTest.CHANNEL_ID)
                .param("clientId", ItemForViewConfigControllerTest.CLIENT_ID)
                .param("requestId", ItemForViewConfigControllerTest.REQUEST_ID)
                .param("username", ItemForViewConfigControllerTest.USERNAME)
                .param("merchantSku", ItemForViewConfigControllerTest.MERCHANT_SKU)
                .param("merchantCode", ItemForViewConfigControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemForViewConfigControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToItemViewConfig(this.itemViewConfigRequest);
    verify(this.itemViewConfigService).addItemViewConfigByMerchantSkuAndMerchantCode(
        ItemForViewConfigControllerTest.STORE_ID, ItemForViewConfigControllerTest.REQUEST_ID,
        ItemForViewConfigControllerTest.USERNAME, ItemForViewConfigControllerTest.MERCHANT_SKU,
        ItemForViewConfigControllerTest.MERCHANT_CODE, this.itemViewConfig);
  }

  @Test
  public void addItemViewConfigByMerchantSkuAndMerchantCodeTestWithException() throws Exception {
    this.mockMvc
        .perform(
            post(
                ProductApiPath.ITEM
                    + ProductApiPath.ADD_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.jsonRequestItemViewConfig)
                .param("storeId", ItemForViewConfigControllerTest.BLANK)
                .param("channelId", ItemForViewConfigControllerTest.CHANNEL_ID)
                .param("clientId", ItemForViewConfigControllerTest.CLIENT_ID)
                .param("requestId", ItemForViewConfigControllerTest.REQUEST_ID)
                .param("username", ItemForViewConfigControllerTest.USERNAME)
                .param("merchantSku", ItemForViewConfigControllerTest.MERCHANT_SKU)
                .param("merchantCode", ItemForViewConfigControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(
            jsonPath(
                "$.errorCode",
                equalTo(ProductErrorCodesEnum.ADD_ITEM_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE
                    .getCode()))).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemForViewConfigControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToItemViewConfig(this.itemViewConfigRequest);
    verify(this.itemViewConfigService).addItemViewConfigByMerchantSkuAndMerchantCode(
        ItemForViewConfigControllerTest.BLANK, ItemForViewConfigControllerTest.REQUEST_ID,
        ItemForViewConfigControllerTest.USERNAME, ItemForViewConfigControllerTest.MERCHANT_SKU,
        ItemForViewConfigControllerTest.MERCHANT_CODE, this.itemViewConfig);
  }

  @Test
  public void addItemViewConfigTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.ADD_VIEW_CONFIG)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.jsonRequestItemViewConfig)
                .param("storeId", ItemForViewConfigControllerTest.STORE_ID)
                .param("channelId", ItemForViewConfigControllerTest.CHANNEL_ID)
                .param("clientId", ItemForViewConfigControllerTest.CLIENT_ID)
                .param("requestId", ItemForViewConfigControllerTest.REQUEST_ID)
                .param("username", ItemForViewConfigControllerTest.USERNAME)
                .param("itemSku", ItemForViewConfigControllerTest.ITEM_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemForViewConfigControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToItemViewConfig(this.itemViewConfigRequest);
    verify(this.itemViewConfigService).addItemViewConfig(ItemForViewConfigControllerTest.STORE_ID,
        ItemForViewConfigControllerTest.ITEM_SKU, this.itemViewConfig);
  }


  @Test
  public void addItemViewConfigTestWithException() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.ADD_VIEW_CONFIG)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.jsonRequestItemViewConfig)
                .param("storeId", ItemForViewConfigControllerTest.BLANK)
                .param("channelId", ItemForViewConfigControllerTest.CHANNEL_ID)
                .param("clientId", ItemForViewConfigControllerTest.CLIENT_ID)
                .param("requestId", ItemForViewConfigControllerTest.REQUEST_ID)
                .param("username", ItemForViewConfigControllerTest.USERNAME)
                .param("itemSku", ItemForViewConfigControllerTest.ITEM_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.ADD_ITEM_VIEW_CONFIG.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemForViewConfigControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToItemViewConfig(this.itemViewConfigRequest);

    verify(this.itemViewConfigService).addItemViewConfig(ItemForViewConfigControllerTest.BLANK,
        ItemForViewConfigControllerTest.ITEM_SKU, this.itemViewConfig);
  }

  @Test
  public void deleteItemViewConfigTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.DELETE_VIEW_CONFIG)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemForViewConfigControllerTest.STORE_ID)
                .param("channelId", ItemForViewConfigControllerTest.CHANNEL_ID)
                .param("clientId", ItemForViewConfigControllerTest.CLIENT_ID)
                .param("requestId", ItemForViewConfigControllerTest.REQUEST_ID)
                .param("username", ItemForViewConfigControllerTest.USERNAME)
                .param("itemSku", ItemForViewConfigControllerTest.ITEM_SKU)
                .param("channel", ItemForViewConfigControllerTest.CHANNEL_NAME.toString()))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemForViewConfigControllerTest.REQUEST_ID)));

    verify(this.itemViewConfigService).deleteItemViewConfig(
        ItemForViewConfigControllerTest.STORE_ID, ItemForViewConfigControllerTest.ITEM_SKU,
        ItemForViewConfigControllerTest.CHANNEL_NAME);
  }

  @Test
  public void deleteItemViewConfigTestWithException() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.DELETE_VIEW_CONFIG)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemForViewConfigControllerTest.BLANK)
                .param("channelId", ItemForViewConfigControllerTest.CHANNEL_ID)
                .param("clientId", ItemForViewConfigControllerTest.CLIENT_ID)
                .param("requestId", ItemForViewConfigControllerTest.REQUEST_ID)
                .param("username", ItemForViewConfigControllerTest.USERNAME)
                .param("itemSku", ItemForViewConfigControllerTest.ITEM_SKU)
                .param("channel", ItemForViewConfigControllerTest.CHANNEL_NAME.toString()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(
            jsonPath("$.errorCode",
                equalTo(ProductErrorCodesEnum.DELETE_ITEM_VIEW_CONFIG.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemForViewConfigControllerTest.REQUEST_ID)));

    verify(this.itemViewConfigService).deleteItemViewConfig(ItemForViewConfigControllerTest.BLANK,
        ItemForViewConfigControllerTest.ITEM_SKU, ItemForViewConfigControllerTest.CHANNEL_NAME);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.itemController).build();

    ObjectMapper mapper = new ObjectMapper();

    this.jsonRequestItemViewConfig =
        FileUtils.readFileToString(new File("src/test/resources/itemViewConfigRequest.json"));
    this.itemViewConfigRequest =
        mapper.readValue(this.jsonRequestItemViewConfig,
            mapper.getTypeFactory().constructType(ItemViewConfigRequest.class));
    assertNotNull(this.itemViewConfigRequest);

    this.itemViewConfig = new ItemViewConfig();

    when(this.modelConverter.convertToItemViewConfig(this.itemViewConfigRequest)).thenReturn(
        this.itemViewConfig);
    when(
        this.itemViewConfigService.addItemViewConfig(ItemForViewConfigControllerTest.STORE_ID,
            ItemForViewConfigControllerTest.ITEM_SKU, this.itemViewConfig)).thenReturn(true);

    doThrow(ApplicationRuntimeException.class).when(this.itemViewConfigService).addItemViewConfig(
        ItemForViewConfigControllerTest.BLANK, ItemForViewConfigControllerTest.ITEM_SKU,
        this.itemViewConfig);

    doThrow(ApplicationRuntimeException.class).when(this.itemViewConfigService).updateItemViewConfig(
        ItemForViewConfigControllerTest.BLANK, ItemForViewConfigControllerTest.ITEM_SKU,
        this.itemViewConfig);

    when(
        this.itemViewConfigService.updateItemViewConfig(ItemForViewConfigControllerTest.STORE_ID,
            ItemForViewConfigControllerTest.ITEM_SKU, this.itemViewConfig)).thenReturn(true);

    doThrow(ApplicationRuntimeException.class).when(this.itemViewConfigService).deleteItemViewConfig(
        ItemForViewConfigControllerTest.BLANK, ItemForViewConfigControllerTest.ITEM_SKU,
        ItemForViewConfigControllerTest.CHANNEL_NAME);

    when(
        this.itemViewConfigService.deleteItemViewConfig(ItemForViewConfigControllerTest.STORE_ID,
            ItemForViewConfigControllerTest.ITEM_SKU, ItemForViewConfigControllerTest.CHANNEL_NAME))
        .thenReturn(true);

    doThrow(ApplicationRuntimeException.class).when(this.itemViewConfigService)
        .updateItemViewConfigByMerchantSkuAndMerchantCode(ItemForViewConfigControllerTest.BLANK,
            ItemForViewConfigControllerTest.REQUEST_ID, ItemForViewConfigControllerTest.USERNAME,
            ItemForViewConfigControllerTest.MERCHANT_SKU,
            ItemForViewConfigControllerTest.MERCHANT_CODE, this.itemViewConfig);

    doThrow(ApplicationRuntimeException.class).when(this.itemViewConfigService)
        .addItemViewConfigByMerchantSkuAndMerchantCode(ItemForViewConfigControllerTest.BLANK,
            ItemForViewConfigControllerTest.REQUEST_ID, ItemForViewConfigControllerTest.USERNAME,
            ItemForViewConfigControllerTest.MERCHANT_SKU,
            ItemForViewConfigControllerTest.MERCHANT_CODE, this.itemViewConfig);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.modelConverter);
    verifyNoMoreInteractions(this.itemService);
  }

  @Test
  public void updateItemViewConfigByMerchantSkuAndMerchantCodeTest() throws Exception {
    this.mockMvc
        .perform(
            post(
                ProductApiPath.ITEM
                    + ProductApiPath.UPDATE_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.jsonRequestItemViewConfig)
                .param("storeId", ItemForViewConfigControllerTest.STORE_ID)
                .param("channelId", ItemForViewConfigControllerTest.CHANNEL_ID)
                .param("clientId", ItemForViewConfigControllerTest.CLIENT_ID)
                .param("requestId", ItemForViewConfigControllerTest.REQUEST_ID)
                .param("username", ItemForViewConfigControllerTest.USERNAME)
                .param("merchantSku", ItemForViewConfigControllerTest.MERCHANT_SKU)
                .param("merchantCode", ItemForViewConfigControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemForViewConfigControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToItemViewConfig(this.itemViewConfigRequest);
    verify(this.itemViewConfigService).updateItemViewConfigByMerchantSkuAndMerchantCode(
        ItemForViewConfigControllerTest.STORE_ID, ItemForViewConfigControllerTest.REQUEST_ID,
        ItemForViewConfigControllerTest.USERNAME, ItemForViewConfigControllerTest.MERCHANT_SKU,
        ItemForViewConfigControllerTest.MERCHANT_CODE, this.itemViewConfig);
  }

  @Test
  public void updateItemViewConfigByMerchantSkuAndMerchantCodeTestWithException() throws Exception {
    this.mockMvc
        .perform(
            post(
                ProductApiPath.ITEM
                    + ProductApiPath.UPDATE_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.jsonRequestItemViewConfig)
                .param("storeId", ItemForViewConfigControllerTest.BLANK)
                .param("channelId", ItemForViewConfigControllerTest.CHANNEL_ID)
                .param("clientId", ItemForViewConfigControllerTest.CLIENT_ID)
                .param("requestId", ItemForViewConfigControllerTest.REQUEST_ID)
                .param("username", ItemForViewConfigControllerTest.USERNAME)
                .param("merchantSku", ItemForViewConfigControllerTest.MERCHANT_SKU)
                .param("merchantCode", ItemForViewConfigControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(
            jsonPath(
                "$.errorCode",
                equalTo(ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE
                    .getCode()))).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemForViewConfigControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToItemViewConfig(this.itemViewConfigRequest);

    verify(this.itemViewConfigService).updateItemViewConfigByMerchantSkuAndMerchantCode(
        ItemForViewConfigControllerTest.BLANK, ItemForViewConfigControllerTest.REQUEST_ID,
        ItemForViewConfigControllerTest.USERNAME, ItemForViewConfigControllerTest.MERCHANT_SKU,
        ItemForViewConfigControllerTest.MERCHANT_CODE, this.itemViewConfig);
  }

  @Test
  public void updateItemViewConfigTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.UPDATE_VIEW_CONFIG)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.jsonRequestItemViewConfig)
                .param("storeId", ItemForViewConfigControllerTest.STORE_ID)
                .param("channelId", ItemForViewConfigControllerTest.CHANNEL_ID)
                .param("clientId", ItemForViewConfigControllerTest.CLIENT_ID)
                .param("requestId", ItemForViewConfigControllerTest.REQUEST_ID)
                .param("username", ItemForViewConfigControllerTest.USERNAME)
                .param("itemSku", ItemForViewConfigControllerTest.ITEM_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemForViewConfigControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToItemViewConfig(this.itemViewConfigRequest);
    verify(this.itemViewConfigService).updateItemViewConfig(
        ItemForViewConfigControllerTest.STORE_ID, ItemForViewConfigControllerTest.ITEM_SKU,
        this.itemViewConfig);
  }

  @Test
  public void updateItemViewConfigTestWithException() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.UPDATE_VIEW_CONFIG)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.jsonRequestItemViewConfig)
                .param("storeId", ItemForViewConfigControllerTest.BLANK)
                .param("channelId", ItemForViewConfigControllerTest.CHANNEL_ID)
                .param("clientId", ItemForViewConfigControllerTest.CLIENT_ID)
                .param("requestId", ItemForViewConfigControllerTest.REQUEST_ID)
                .param("username", ItemForViewConfigControllerTest.USERNAME)
                .param("itemSku", ItemForViewConfigControllerTest.ITEM_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(
            jsonPath("$.errorCode",
                equalTo(ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemForViewConfigControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToItemViewConfig(this.itemViewConfigRequest);

    verify(this.itemViewConfigService).updateItemViewConfig(ItemForViewConfigControllerTest.BLANK,
        ItemForViewConfigControllerTest.ITEM_SKU, this.itemViewConfig);
  }
}
