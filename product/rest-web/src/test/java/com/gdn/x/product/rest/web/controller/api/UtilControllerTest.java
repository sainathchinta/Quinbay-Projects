package com.gdn.x.product.rest.web.controller.api;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.util.LuceneOptimizeService;

public class UtilControllerTest {

  private static final String SKU_1 = "sku-1";
  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String UPDATED_BY = "updatedBy";

  @InjectMocks
  private UtilController utilController;

  @Mock
  private LuceneOptimizeService optimizeService;

  @Mock
  private ItemService itemService;

  private MockMvc mockMvc;

  @Test
  public void optimizeTest() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.UTIL + ProductApiPath.OPTIMIZE_INDEX)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", UtilControllerTest.STORE_ID)
            .param("channelId", UtilControllerTest.CHANNEL_ID)
            .param("clientId", UtilControllerTest.CLIENT_ID)
            .param("requestId", UtilControllerTest.REQUEST_ID)
            .param("username", UtilControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    verify(this.optimizeService).optimize();
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.utilController).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.optimizeService);
    verifyNoMoreInteractions(this.itemService);
  }

  @Test
  public void sendItemChangeByUpdatedBy() throws Exception {
    doNothing().when(this.itemService).sendItemChangeEventByUpdatedBy(STORE_ID, UPDATED_BY);
    this.mockMvc
        .perform(post(ProductApiPath.UTIL + ProductApiPath.SEND_ITEM_CHANGE_BY_UPDATED_BY)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", UtilControllerTest.STORE_ID)
            .param("channelId", UtilControllerTest.CHANNEL_ID)
            .param("clientId", UtilControllerTest.CLIENT_ID)
            .param("requestId", UtilControllerTest.REQUEST_ID)
            .param("username", UtilControllerTest.USERNAME)
            .param("updatedBy", UtilControllerTest.UPDATED_BY))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    verify(this.itemService).sendItemChangeEventByUpdatedBy(Mockito.eq(STORE_ID), Mockito.eq(UPDATED_BY));
  }
}
