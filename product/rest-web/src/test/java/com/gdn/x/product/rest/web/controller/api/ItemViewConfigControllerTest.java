package com.gdn.x.product.rest.web.controller.api;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.Date;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.rest.web.model.ItemBuyableRequest;
import com.gdn.x.product.rest.web.model.ItemBuyableScheduleRequest;
import com.gdn.x.product.rest.web.model.ItemDiscoverableRequest;
import com.gdn.x.product.rest.web.model.ItemDiscoverableScheduleRequest;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.service.api.ItemViewConfigService;

public class ItemViewConfigControllerTest {

  private static final Date DATE = new Date();

  private static final String CHANNEL = "channel";

  private static final boolean BUYABLE = true;

  private static final boolean DISCOVERABLE = true;

  @InjectMocks
  private ItemViewConfigController controller;

  @Mock
  private ItemViewConfigService itemViewConfigService;

  @Mock
  private GdnMapper gdnMapper;

  private MockMvc mockMvc;
  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String ITEM_SKU = "itemSku";

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.controller).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.itemViewConfigService);
    verifyNoMoreInteractions(this.gdnMapper);
  }

  @Test
  public void updateBuyableDefaultTest() throws Exception {
    ItemBuyableRequest itemBuyableRequest = new ItemBuyableRequest();
    itemBuyableRequest.setBuyable(ItemViewConfigControllerTest.BUYABLE);
    itemBuyableRequest.setChannel(ItemViewConfigControllerTest.CHANNEL);
    String bodyJson = new ObjectMapper().writeValueAsString(itemBuyableRequest);
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM_VIEW_CONFIG_UPDATE_BUYABLE_DEFAULT)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemViewConfigControllerTest.STORE_ID)
                .param("channelId", ItemViewConfigControllerTest.CHANNEL_ID)
                .param("clientId", ItemViewConfigControllerTest.CLIENT_ID)
                .param("requestId", ItemViewConfigControllerTest.REQUEST_ID)
                .param("username", ItemViewConfigControllerTest.USERNAME)
                .param("itemSku", ItemViewConfigControllerTest.ITEM_SKU).content(bodyJson))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));

    verify(this.itemViewConfigService).updateDefaultBuyable(ItemViewConfigControllerTest.STORE_ID,
        itemBuyableRequest.getChannel(), ItemViewConfigControllerTest.ITEM_SKU,
        itemBuyableRequest.isBuyable());
  }

  @Test
  public void updateBuyableScheduleTest() throws Exception {
    ItemBuyableScheduleRequest itemBuyableScheduleRequest =
        new ItemBuyableScheduleRequest(ItemViewConfigControllerTest.CHANNEL,
            ItemViewConfigControllerTest.BUYABLE, ItemViewConfigControllerTest.DATE,
            ItemViewConfigControllerTest.DATE);

    ItemBuyableSchedule itemBuyableSchedule =
        new ItemBuyableSchedule(ItemViewConfigControllerTest.BUYABLE,
            ItemViewConfigControllerTest.DATE, ItemViewConfigControllerTest.DATE);

    when(this.gdnMapper.deepCopy(itemBuyableScheduleRequest, ItemBuyableSchedule.class))
        .thenReturn(itemBuyableSchedule);

    String bodyJson = new ObjectMapper().writeValueAsString(itemBuyableScheduleRequest);
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM_VIEW_CONFIG_UPDATE_BUYABLE_SCHEDULE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemViewConfigControllerTest.STORE_ID)
                .param("channelId", ItemViewConfigControllerTest.CHANNEL_ID)
                .param("clientId", ItemViewConfigControllerTest.CLIENT_ID)
                .param("requestId", ItemViewConfigControllerTest.REQUEST_ID)
                .param("username", ItemViewConfigControllerTest.USERNAME)
                .param("itemSku", ItemViewConfigControllerTest.ITEM_SKU).content(bodyJson))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    verify(this.gdnMapper).deepCopy(itemBuyableScheduleRequest, ItemBuyableSchedule.class);
    verify(this.itemViewConfigService).updateBuyableSchedule(ItemViewConfigControllerTest.STORE_ID,
        ItemViewConfigControllerTest.CHANNEL, ItemViewConfigControllerTest.ITEM_SKU,
        itemBuyableSchedule);
  }

  @Test
  public void updateDiscoverableDefaultTest() throws Exception {
    ItemDiscoverableRequest itemDiscoverableRequest =
        new ItemDiscoverableRequest(ItemViewConfigControllerTest.CHANNEL,
            ItemViewConfigControllerTest.DISCOVERABLE);

    String bodyJson = new ObjectMapper().writeValueAsString(itemDiscoverableRequest);
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM_VIEW_CONFIG_UPDATE_DISCOVERABLE_DEFAULT)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemViewConfigControllerTest.STORE_ID)
                .param("channelId", ItemViewConfigControllerTest.CHANNEL_ID)
                .param("clientId", ItemViewConfigControllerTest.CLIENT_ID)
                .param("requestId", ItemViewConfigControllerTest.REQUEST_ID)
                .param("username", ItemViewConfigControllerTest.USERNAME)
                .param("itemSku", ItemViewConfigControllerTest.ITEM_SKU).content(bodyJson))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    verify(this.itemViewConfigService).updateDefaultDiscoverable(
        ItemViewConfigControllerTest.STORE_ID, ItemViewConfigControllerTest.CHANNEL,
        ItemViewConfigControllerTest.ITEM_SKU, ItemViewConfigControllerTest.DISCOVERABLE);
  }

  @Test
  public void updateDiscoverableScheduleTest() throws Exception {
    ItemDiscoverableScheduleRequest itemDiscoverableScheduleRequest =
        new ItemDiscoverableScheduleRequest(ItemViewConfigControllerTest.CHANNEL,
            ItemViewConfigControllerTest.DISCOVERABLE, ItemViewConfigControllerTest.DATE,
            ItemViewConfigControllerTest.DATE);

    ItemDiscoverableSchedule itemDiscoverableSchedule =
        new ItemDiscoverableSchedule(ItemViewConfigControllerTest.DISCOVERABLE,
            ItemViewConfigControllerTest.DATE, ItemViewConfigControllerTest.DATE);

    when(this.gdnMapper.deepCopy(itemDiscoverableScheduleRequest, ItemDiscoverableSchedule.class))
        .thenReturn(itemDiscoverableSchedule);

    String bodyJson = new ObjectMapper().writeValueAsString(itemDiscoverableScheduleRequest);
    this.mockMvc
        .perform(
            post(ProductApiPath.ITEM_VIEW_CONFIG_UPDATE_DISCOVERABLE_SCHEDULE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ItemViewConfigControllerTest.STORE_ID)
                .param("channelId", ItemViewConfigControllerTest.CHANNEL_ID)
                .param("clientId", ItemViewConfigControllerTest.CLIENT_ID)
                .param("requestId", ItemViewConfigControllerTest.REQUEST_ID)
                .param("username", ItemViewConfigControllerTest.USERNAME)
                .param("itemSku", ItemViewConfigControllerTest.ITEM_SKU).content(bodyJson))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    verify(this.gdnMapper)
        .deepCopy(itemDiscoverableScheduleRequest, ItemDiscoverableSchedule.class);
    verify(this.itemViewConfigService).updateDiscoverableSchedule(
        ItemViewConfigControllerTest.STORE_ID, ItemViewConfigControllerTest.CHANNEL,
        ItemViewConfigControllerTest.ITEM_SKU, itemDiscoverableSchedule);
  }
}
