package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
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
import java.util.ArrayList;
import java.util.HashSet;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.util.ModelConverter;

public class ItemControllerPriceTest {

  private static final String STORE_ID = "store-id";

  private static final String CHANNEL_ID = "channel-id";

  private static final String CLIENT_ID = "client-id";

  private static final String REQUEST_ID = "request-id";

  private static final String USERNAME = "username";

  private static final String BLANK = "";

  private static final String ITEM_SKU = "item-sku";

  private static final String CHANNEL_WEB_REQUEST = "DESKTOP_WEB";

  private static final String MERCHANT_SKU = "merchant-sku";

  private static final String MERCHANT_CODE = "merchant-code";

  @InjectMocks
  ItemController itemController;

  @Mock
  ItemService itemService;

  @Mock
  ModelConverter modelConverter;

  private String priceRequestJson;

  private MockMvc mockMvc;

  private PriceRequest priceRequest;

  private Price price;

  private PriceDTO priceResponse;

  private Price priceAsResponse;

  private Price priceUpdated;

  private PriceDTO priceUpdatedResponse;

  private Item item;

  private HashSet<Price> prices;

  private ArrayList<Item> items;

  @Test
  public void addItemPriceByMerchantSkuAndMerchantCodeTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.ADD_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.priceRequestJson).param("storeId", ItemControllerPriceTest.STORE_ID)
                .param("channelId", ItemControllerPriceTest.CHANNEL_ID)
                .param("clientId", ItemControllerPriceTest.CLIENT_ID)
                .param("requestId", ItemControllerPriceTest.REQUEST_ID)
                .param("username", ItemControllerPriceTest.USERNAME)
                .param("merchantSku", ItemControllerPriceTest.MERCHANT_SKU)
                .param("merchantCode", ItemControllerPriceTest.MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)));

    verify(this.modelConverter).convertToItemPrice(this.priceRequest);
    verify(this.itemService).addItemPriceByMerchantSkuAndMerchantCode(
        ItemControllerPriceTest.STORE_ID, ItemControllerPriceTest.REQUEST_ID,
        ItemControllerPriceTest.USERNAME, ItemControllerPriceTest.MERCHANT_SKU,
        ItemControllerPriceTest.MERCHANT_CODE, this.price);
  }

  @Test
  public void addItemPriceByMerchantSkuAndMerchantCodeTestWithStoreIdBlank() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.ADD_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.priceRequestJson).param("storeId", ItemControllerPriceTest.BLANK)
                .param("channelId", ItemControllerPriceTest.CHANNEL_ID)
                .param("clientId", ItemControllerPriceTest.CLIENT_ID)
                .param("requestId", ItemControllerPriceTest.REQUEST_ID)
                .param("username", ItemControllerPriceTest.USERNAME)
                .param("merchantSku", ItemControllerPriceTest.MERCHANT_SKU)
                .param("merchantCode", ItemControllerPriceTest.MERCHANT_CODE))
        .andExpect(status().isOk())
        .andExpect(
            jsonPath("$.errorCode",
                equalTo(ProductErrorCodesEnum.ADD_ITEM_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE
                    .getCode()))).andExpect(jsonPath("$.errorMessage", notNullValue()));

    verify(this.modelConverter).convertToItemPrice(this.priceRequest);
    verify(this.itemService).addItemPriceByMerchantSkuAndMerchantCode(
        ItemControllerPriceTest.BLANK, ItemControllerPriceTest.REQUEST_ID,
        ItemControllerPriceTest.USERNAME, ItemControllerPriceTest.MERCHANT_SKU,
        ItemControllerPriceTest.MERCHANT_CODE, this.price);
  }

  @Test
  public void addItemPriceTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.ADD_PRICE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(this.priceRequestJson)
                .param("storeId", ItemControllerPriceTest.STORE_ID)
                .param("channelId", ItemControllerPriceTest.CHANNEL_ID)
                .param("clientId", ItemControllerPriceTest.CLIENT_ID)
                .param("requestId", ItemControllerPriceTest.REQUEST_ID)
                .param("username", ItemControllerPriceTest.USERNAME)
                .param("itemSku", ItemControllerPriceTest.ITEM_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)));

    verify(this.modelConverter).convertToItemPrice(this.priceRequest);
    verify(this.itemService).addItemPrice(ItemControllerPriceTest.STORE_ID, this.price,
        ItemControllerPriceTest.ITEM_SKU, USERNAME);
  }

  @Test
  public void addItemPriceTestWithStoreIdBlank() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.ADD_PRICE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(this.priceRequestJson)
                .param("storeId", ItemControllerPriceTest.BLANK)
                .param("channelId", ItemControllerPriceTest.CHANNEL_ID)
                .param("clientId", ItemControllerPriceTest.CLIENT_ID)
                .param("requestId", ItemControllerPriceTest.REQUEST_ID)
                .param("username", ItemControllerPriceTest.USERNAME)
                .param("itemSku", ItemControllerPriceTest.ITEM_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.ADD_ITEM_PRICE.getCode())))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.modelConverter).convertToItemPrice(this.priceRequest);
    verify(this.itemService).addItemPrice(ItemControllerPriceTest.BLANK, this.price,
        ItemControllerPriceTest.ITEM_SKU, USERNAME);
  }

  @Test
  public void deleteItemPriceByChannelTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.DELETE_PRICE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemControllerPriceTest.STORE_ID)
                .param("channelId", ItemControllerPriceTest.CHANNEL_ID)
                .param("clientId", ItemControllerPriceTest.CLIENT_ID)
                .param("requestId", ItemControllerPriceTest.REQUEST_ID)
                .param("username", ItemControllerPriceTest.USERNAME)
                .param("itemSku", ItemControllerPriceTest.ITEM_SKU)
                .param("channel", ItemControllerPriceTest.CHANNEL_WEB_REQUEST))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ItemControllerPriceTest.REQUEST_ID)));

    verify(this.itemService).deleteItemPrice(ItemControllerPriceTest.STORE_ID,
        ItemControllerPriceTest.ITEM_SKU, ItemControllerPriceTest.CHANNEL_WEB_REQUEST);
  }

  @Test
  public void deleteItemPriceByChannelTestWithStoreIdBlank() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.DELETE_PRICE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemControllerPriceTest.BLANK)
                .param("channelId", ItemControllerPriceTest.CHANNEL_ID)
                .param("clientId", ItemControllerPriceTest.CLIENT_ID)
                .param("requestId", ItemControllerPriceTest.REQUEST_ID)
                .param("username", ItemControllerPriceTest.USERNAME)
                .param("itemSku", ItemControllerPriceTest.ITEM_SKU)
                .param("channel", ItemControllerPriceTest.CHANNEL_WEB_REQUEST))
        .andExpect(status().isOk())
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.DELETE_ITEM_PRICE.getCode())))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.itemService).deleteItemPrice(ItemControllerPriceTest.BLANK,
        ItemControllerPriceTest.ITEM_SKU, ItemControllerPriceTest.CHANNEL_WEB_REQUEST);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.itemController).build();

    ObjectMapper objectMapper = new ObjectMapper();

    this.priceRequestJson =
        FileUtils.readFileToString(new File("src/test/resources/priceRequest.json"));

    this.priceRequest =
        objectMapper.readValue(this.priceRequestJson,
            objectMapper.getTypeFactory().constructType(PriceRequest.class));

    Assertions.assertNotNull(this.priceRequest);

    this.price = new Price();
    this.item = new Item();
    this.prices = new HashSet<Price>();
    this.item.setPrice(this.prices);
    this.items = new ArrayList<Item>();
    this.items.add(this.item);

    when(this.modelConverter.convertToItemPrice(this.priceRequest)).thenReturn(this.price);

    this.priceAsResponse = new Price();
    when(
        this.itemService.addItemPrice(ItemControllerPriceTest.STORE_ID, this.price,
            ItemControllerPriceTest.ITEM_SKU, USERNAME)).thenReturn(true);

    this.priceResponse = new PriceDTO();
    when(this.modelConverter.convertToItemPriceResponse(this.priceAsResponse)).thenReturn(
        this.priceResponse);

    doThrow(new ApplicationRuntimeException()).when(this.itemService).addItemPrice(
        ItemControllerPriceTest.BLANK, this.price, ItemControllerPriceTest.ITEM_SKU, USERNAME);

    this.priceUpdated = new Price();
    when(
        this.itemService.updateItemPrice(USERNAME, ItemControllerPriceTest.STORE_ID, this.price,
            ItemControllerPriceTest.ITEM_SKU, null)).thenReturn(true);

    this.priceUpdatedResponse = new PriceDTO();
    when(this.modelConverter.convertToItemPriceResponse(this.priceUpdated)).thenReturn(
        this.priceUpdatedResponse);

    doThrow(new ApplicationRuntimeException()).when(this.itemService).updateItemPrice(USERNAME,
        ItemControllerPriceTest.BLANK, this.price, ItemControllerPriceTest.ITEM_SKU, null);

    when(
        this.itemService.deleteItemPrice(ItemControllerPriceTest.STORE_ID,
            ItemControllerPriceTest.ITEM_SKU, ItemControllerPriceTest.CHANNEL_WEB_REQUEST))
        .thenReturn(true);

    doThrow(new ApplicationRuntimeException()).when(this.itemService).deleteItemPrice(
        ItemControllerPriceTest.BLANK, ItemControllerPriceTest.ITEM_SKU,
        ItemControllerPriceTest.CHANNEL_WEB_REQUEST);

    when(
        this.itemService.updateItemPriceByMerchantSkuAndMerchantCode(
            ItemControllerPriceTest.STORE_ID, ItemControllerPriceTest.REQUEST_ID,
            ItemControllerPriceTest.USERNAME, ItemControllerPriceTest.MERCHANT_SKU,
            ItemControllerPriceTest.MERCHANT_CODE, this.price)).thenReturn(true);

    doThrow(new ApplicationRuntimeException())
        .when(this.itemService)
        .updateItemPriceByMerchantSkuAndMerchantCode(ItemControllerPriceTest.BLANK,
            ItemControllerPriceTest.REQUEST_ID, ItemControllerPriceTest.USERNAME,
            ItemControllerPriceTest.MERCHANT_SKU, ItemControllerPriceTest.MERCHANT_CODE, this.price);

    doThrow(new ApplicationRuntimeException())
        .when(this.itemService)
        .addItemPriceByMerchantSkuAndMerchantCode(ItemControllerPriceTest.BLANK,
            ItemControllerPriceTest.REQUEST_ID, ItemControllerPriceTest.USERNAME,
            ItemControllerPriceTest.MERCHANT_SKU, ItemControllerPriceTest.MERCHANT_CODE, this.price);

  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.modelConverter);
  }

  @Test
  public void updateItemPriceByMerchantSkuAndMerchantCodeTest() throws Exception {
    this.mockMvc
        .perform(
            post(
                ProductApiPath.ITEM + ProductApiPath.UPDATE_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.priceRequestJson).param("storeId", ItemControllerPriceTest.STORE_ID)
                .param("channelId", ItemControllerPriceTest.CHANNEL_ID)
                .param("clientId", ItemControllerPriceTest.CLIENT_ID)
                .param("requestId", ItemControllerPriceTest.REQUEST_ID)
                .param("username", ItemControllerPriceTest.USERNAME)
                .param("merchantSku", ItemControllerPriceTest.MERCHANT_SKU)
                .param("merchantCode", ItemControllerPriceTest.MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)));

    verify(this.modelConverter).convertToItemPrice(this.priceRequest);
    verify(this.itemService).updateItemPriceByMerchantSkuAndMerchantCode(
        ItemControllerPriceTest.STORE_ID, ItemControllerPriceTest.REQUEST_ID,
        ItemControllerPriceTest.USERNAME, ItemControllerPriceTest.MERCHANT_SKU,
        ItemControllerPriceTest.MERCHANT_CODE, this.price);
  }

  @Test
  public void updateItemPriceByMerchantSkuAndMerchantCodeTestWithStoreIdBlank() throws Exception {
    this.mockMvc
        .perform(
            post(
                ProductApiPath.ITEM + ProductApiPath.UPDATE_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.priceRequestJson).param("storeId", ItemControllerPriceTest.BLANK)
                .param("channelId", ItemControllerPriceTest.CHANNEL_ID)
                .param("clientId", ItemControllerPriceTest.CLIENT_ID)
                .param("requestId", ItemControllerPriceTest.REQUEST_ID)
                .param("username", ItemControllerPriceTest.USERNAME)
                .param("merchantSku", ItemControllerPriceTest.MERCHANT_SKU)
                .param("merchantCode", ItemControllerPriceTest.MERCHANT_CODE))
        .andExpect(status().isOk())
        .andExpect(
            jsonPath("$.errorCode",
                equalTo(ProductErrorCodesEnum.UPDATE_ITEM_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE
                    .getCode()))).andExpect(jsonPath("$.errorMessage", notNullValue()));

    verify(this.modelConverter).convertToItemPrice(this.priceRequest);
    verify(this.itemService).updateItemPriceByMerchantSkuAndMerchantCode(
        ItemControllerPriceTest.BLANK, ItemControllerPriceTest.REQUEST_ID,
        ItemControllerPriceTest.USERNAME, ItemControllerPriceTest.MERCHANT_SKU,
        ItemControllerPriceTest.MERCHANT_CODE, this.price);
  }

  @Test
  public void updateItemPriceTest() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.UPDATE_PRICE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.priceRequestJson).param("storeId", ItemControllerPriceTest.STORE_ID)
                .param("channelId", ItemControllerPriceTest.CHANNEL_ID)
                .param("clientId", ItemControllerPriceTest.CLIENT_ID)
                .param("requestId", ItemControllerPriceTest.REQUEST_ID)
                .param("username", ItemControllerPriceTest.USERNAME)
                .param("itemSku", ItemControllerPriceTest.ITEM_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)));
    verify(this.modelConverter).convertToItemPrice(this.priceRequest);
    verify(this.itemService).updateItemPrice(USERNAME, ItemControllerPriceTest.STORE_ID,
        this.price, ItemControllerPriceTest.ITEM_SKU, null);
  }

  @Test
  public void updateItemPriceTestWithStoreIdBlank() throws Exception {
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM + ProductApiPath.UPDATE_PRICE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.priceRequestJson).param("storeId", ItemControllerPriceTest.BLANK)
                .param("channelId", ItemControllerPriceTest.CHANNEL_ID)
                .param("clientId", ItemControllerPriceTest.CLIENT_ID)
                .param("requestId", ItemControllerPriceTest.REQUEST_ID)
                .param("username", ItemControllerPriceTest.USERNAME)
                .param("itemSku", ItemControllerPriceTest.ITEM_SKU))
        .andExpect(status().isOk())
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.UPDATE_ITEM_PRICE.getCode())))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.modelConverter).convertToItemPrice(this.priceRequest);
    verify(this.itemService).updateItemPrice(USERNAME, ItemControllerPriceTest.BLANK, this.price,
        ItemControllerPriceTest.ITEM_SKU, null);
  }
}
