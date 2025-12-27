package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.test.web.servlet.MockMvc;

import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.service.api.ChannelService;

public class ChannelControllerTest {

  private static final String MERCHANT_CODE = "merchant-code";

  private static final String ITEM_SKU = "item-sku";

  private static final String PAGE = "0";

  private static final String SIZE = "10";

  private static final String USERNAME = "username";

  private static final String REQUEST_ID = "request-id";

  private static final String CLIENT_ID = "client-id";

  private static final String CHANNEL_ID = "channel-id";

  private static final String STORE_ID = "store-id";


  private static final ItemSummaryPageResponseVo ITEM_SUMMARY_RESPONSE =
      new ItemSummaryPageResponseVo();

  @InjectMocks
  private ChannelController channelController;

  @Mock
  private ChannelService channelService;

  private MockMvc mockMvc;

  @Test
  public void getListOfChannelTest() throws Exception {
    when(this.channelService.getListOfChannel()).thenReturn(Arrays.asList("web", "mobile"));
    this.mockMvc
        .perform(
            get(ProductApiPath.CHANNEL + ProductApiPath.GET)
                .param("storeId", ChannelControllerTest.STORE_ID)
                .param("channelId", ChannelControllerTest.CHANNEL_ID)
                .param("clientId", ChannelControllerTest.CLIENT_ID)
                .param("requestId", ChannelControllerTest.REQUEST_ID)
                .param("username", ChannelControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.channelService).getListOfChannel();
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.channelController).build();

  }


  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.channelService);
  }

}
