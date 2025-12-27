package com.gdn.partners.product.analytics.web.controller;

import com.gdn.partners.product.analytics.model.TerminatedSellerDeletionApiPath;
import com.gdn.partners.product.analytics.service.ImageDeleteService;
import com.gdn.partners.product.analytics.service.TerminatedSellerDeletionService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

@ExtendWith(MockitoExtension.class)
class TerminatedSellerDeletionControllerTest {

  private static final String STORE_ID = "10001";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "userName";

  @InjectMocks
  private TerminatedSellerDeletionController terminatedSellerDeletionController;

  @Mock
  private TerminatedSellerDeletionService terminatedSellerDeletionService;

  @Mock
  private ImageDeleteService imageDeleteService;

  private MockMvc mockMvc;

  @BeforeEach
  public void setup() {
    this.mockMvc = standaloneSetup(this.terminatedSellerDeletionController).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(terminatedSellerDeletionService);
    Mockito.verifyNoMoreInteractions(imageDeleteService);
  }

  @Test
  void publishEventsTest() throws Exception {
    this.mockMvc.perform(get(TerminatedSellerDeletionApiPath.BASE_PATH
        + TerminatedSellerDeletionApiPath.PUBLISH_EVENTS).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME)).andExpect(status().isOk());
    verify(terminatedSellerDeletionService).publishEventsForProductDeletion(STORE_ID, false);
  }

  @Test
  void deleteImagesTest() throws Exception {
    this.mockMvc.perform(get(TerminatedSellerDeletionApiPath.BASE_PATH
      + TerminatedSellerDeletionApiPath.DELETE_IMAGES).param("storeId", STORE_ID)
      .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
      .param("username", USERNAME)).andExpect(status().isOk());
    verify(imageDeleteService).deleteImagesOfProduct(STORE_ID, null);
  }
}
