package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.concurrent.ExecutorService;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.stubbing.Answer;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.gdn.x.product.rest.web.model.ItemPickupPointMigrationApiPath;
import com.gdn.x.product.service.api.ItemPickupPointWrapperService;

public class ItemPickupPointMigrationControllerTest {

  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String STATUS = "status";
  private MockMvc mockMvc;

  @Mock
  private ExecutorService executorService;

  @Mock
  private ItemPickupPointWrapperService itemPickupPointWrapperService;

  @InjectMocks
  private ItemPickupPointMigrationController itemPickupPointMigrationController;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    this.mockMvc = standaloneSetup(this.itemPickupPointMigrationController).build();
    implementAsDirectExecutor(executorService);
  }

  void implementAsDirectExecutor(ExecutorService executor) {
    doAnswer((Answer<Object>) invocation -> {
      ((Runnable) invocation.getArguments()[0]).run();
      return null;
    }).when(executor).execute(Mockito.any(Runnable.class));
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(itemPickupPointWrapperService);
  }

  @Test
  public void migrateItemPickupPointTest() throws Exception {
    this.mockMvc.perform(
        get(ItemPickupPointMigrationApiPath.ITEM_PICKUP_POINT_MIGRATION_BASE_PATH + ItemPickupPointMigrationApiPath.MIGRATE_ITEM_PICKUP_POINT).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
          .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
          .param("requestId", REQUEST_ID).param("username", USERNAME)
          .param("status", STATUS))
      .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
      .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)))
      .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.itemPickupPointWrapperService).migrateItemPickupPointCollection(STORE_ID,
      null, STATUS);
  }
}