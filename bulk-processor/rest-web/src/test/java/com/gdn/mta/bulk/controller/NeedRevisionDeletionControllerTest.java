package com.gdn.mta.bulk.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
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
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.gdn.mta.bulk.service.NeedRevisionDeletionService;

public class NeedRevisionDeletionControllerTest {

  private static final String STORE_ID = "store-123";
  private static final String REQUEST_ID = "req-456";
  private static final String CHANNEL_ID = "channel-1";
  private static final String CLIENT_ID = "client-1";
  private static final String USERNAME = "test-user";
  private static final Integer FETCH_PROCESS_COUNT_FOR_DELETION = 1;

  private MockMvc mockMvc;

  @InjectMocks
  private NeedRevisionDeletionController needRevisionDeletionController;

  @Mock
  private NeedRevisionDeletionService needRevisionDeletionService;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.needRevisionDeletionController).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(needRevisionDeletionService);
  }

  @Test
  public void processNewInternalProcessRequests_success() throws Exception {
    Mockito.doNothing().when(needRevisionDeletionService).populateNRBusinessPartnerCodes(STORE_ID, REQUEST_ID);
    this.mockMvc.perform(get(NeedRevisionDeletionController.BASE_PATH
            + NeedRevisionDeletionController.FETCH_AND_POPULATE_NR_BUSINESS_PARTNER_CODES).param("storeId", STORE_ID)
            .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("username", USERNAME).accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    Mockito.verify(needRevisionDeletionService).populateNRBusinessPartnerCodes(STORE_ID, REQUEST_ID);
  }

  @Test
  public void processNewInternalProcessRequests_failure() throws Exception {
    Mockito.doThrow(new RuntimeException("Test exception")).when(needRevisionDeletionService)
        .populateNRBusinessPartnerCodes(STORE_ID, REQUEST_ID);
    this.mockMvc.perform(get(NeedRevisionDeletionController.BASE_PATH
            + NeedRevisionDeletionController.FETCH_AND_POPULATE_NR_BUSINESS_PARTNER_CODES).param("storeId", STORE_ID)
            .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("username", USERNAME).accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));

    Mockito.verify(needRevisionDeletionService).populateNRBusinessPartnerCodes(STORE_ID, REQUEST_ID);
  }

  @Test
  public void testSendNotificationForNeedRevisionDeletion() throws Exception {
    String storeId = "store123";
    String channelId = "channel123";
    String clientId = "client123";
    String requestId = "request123";
    String username = "user123";
    mockMvc.perform(get(NeedRevisionDeletionController.BASE_PATH
            + NeedRevisionDeletionController.SEND_NOTIFICATION)
            .param("storeId", storeId).param("channelId", channelId).param("clientId", clientId)
            .param("requestId", requestId).param("username", username).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
    Mockito.verify(needRevisionDeletionService).sendNotificationForNeedRevisionDeletion(storeId, requestId);
  }

  @Test
  public void fetchBusinessPartnerProductsAndSaveSuccessTest() throws Exception {
    Mockito.doNothing().when(needRevisionDeletionService)
        .fetchProductsOfABusinessPartner(STORE_ID, REQUEST_ID);
    this.mockMvc.perform(post(NeedRevisionDeletionController.BASE_PATH
            + NeedRevisionDeletionController.FETCH_BUSINESS_PARTNER_NR_PRODUCTS).param("storeId",
                STORE_ID).param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("username", USERNAME)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    Mockito.verify(needRevisionDeletionService)
        .fetchProductsOfABusinessPartner(STORE_ID, REQUEST_ID);
  }

  @Test
  public void fetchBusinessPartnerProductsAndSaveFailureTest() throws Exception {
    Mockito.doThrow(new RuntimeException()).when(needRevisionDeletionService)
        .fetchProductsOfABusinessPartner(STORE_ID, REQUEST_ID);
    this.mockMvc.perform(post(NeedRevisionDeletionController.BASE_PATH
            + NeedRevisionDeletionController.FETCH_BUSINESS_PARTNER_NR_PRODUCTS).param("storeId",
                STORE_ID).param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("username", USERNAME)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    Mockito.verify(needRevisionDeletionService)
        .fetchProductsOfABusinessPartner(STORE_ID, REQUEST_ID);
  }



  @Test
  public void processNeedRevisionDeletion_success() throws Exception {
    Mockito.doNothing().when(needRevisionDeletionService).processNeedRevisionDeletion(STORE_ID, FETCH_PROCESS_COUNT_FOR_DELETION);

    this.mockMvc.perform(get(NeedRevisionDeletionController.BASE_PATH
        + NeedRevisionDeletionController.PROCESS_NEED_REVISION_DELETION)
        .param("storeId", STORE_ID)
        .param("fetchProcessCountForDeletion", String.valueOf(FETCH_PROCESS_COUNT_FOR_DELETION))
        .param("requestId", REQUEST_ID)
        .accept(MediaType.APPLICATION_JSON))
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)))
      .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));

    Mockito.verify(needRevisionDeletionService).processNeedRevisionDeletion(STORE_ID, FETCH_PROCESS_COUNT_FOR_DELETION);
  }

  @Test
  public void processNeedRevisionDeletion_failure() throws Exception {
    Mockito.doThrow(new RuntimeException("Test exception")).when(needRevisionDeletionService)
      .processNeedRevisionDeletion(STORE_ID, FETCH_PROCESS_COUNT_FOR_DELETION);

    this.mockMvc.perform(get(NeedRevisionDeletionController.BASE_PATH
        + NeedRevisionDeletionController.PROCESS_NEED_REVISION_DELETION)
        .param("storeId", STORE_ID)
        .param("fetchProcessCountForDeletion", String.valueOf(FETCH_PROCESS_COUNT_FOR_DELETION))
        .param("requestId", REQUEST_ID)
        .accept(MediaType.APPLICATION_JSON))
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));

    Mockito.verify(needRevisionDeletionService).processNeedRevisionDeletion(STORE_ID, FETCH_PROCESS_COUNT_FOR_DELETION);
  }


}
