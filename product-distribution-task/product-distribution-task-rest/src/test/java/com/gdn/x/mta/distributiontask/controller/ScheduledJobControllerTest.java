package com.gdn.x.mta.distributiontask.controller;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

import java.util.ArrayList;
import java.util.Calendar;

import com.gdn.x.mta.distributiontask.service.api.ProductEmailService;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.enums.ProductMigrationType;
import com.gdn.x.mta.distributiontask.rest.model.ScheduledJobControllerPath;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductCodeListRequest;
import com.gdn.x.mta.distributiontask.service.api.ScheduledJobService;

public class ScheduledJobControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANNEL_ID = "channelId";
  private static final long dateInMillis = 1000;
  private static final int NUM_OF_HOURS = 1;

  private MockMvc mockMvc;
  private Calendar calendar = Calendar.getInstance();
  private ObjectMapper objectMapper = new ObjectMapper();

  @Mock
  private ScheduledJobService scheduledJobService;

  @Mock
  private ProductEmailService productEmailService;

  @InjectMocks
  private ScheduledJobController scheduledJobController;

  @Captor
  private ArgumentCaptor<ProductCodeListRequest> productCodeListRequestArgumentCaptor;

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.scheduledJobController).build();
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(scheduledJobService);
  }

  @Test
   void runPostLiveReviewConfigChangesTest() throws Exception {
    Mockito.doNothing().when(this.scheduledJobService).runPostLiveConfigChanges(
        DEFAULT_STORE_ID, REQUEST_ID, USERNAME, dateInMillis);
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.RUN_POST_LIVE_CONFIG_CHANGES)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("dateInMillis", String.valueOf(dateInMillis)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.scheduledJobService).runPostLiveConfigChanges(DEFAULT_STORE_ID, REQUEST_ID, USERNAME, dateInMillis);
  }

  @Test
   void runPostLiveReviewConfigChangesTest_noDateParameter() throws Exception {
    Mockito.doNothing().when(this.scheduledJobService).runPostLiveConfigChanges(
        Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.anyLong());
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.RUN_POST_LIVE_CONFIG_CHANGES)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.scheduledJobService).runPostLiveConfigChanges(
        Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.anyLong());
  }


  @Test
   void runPostLiveReviewConfigChangesTest_expectException() throws Exception {
    Mockito.doThrow(Exception.class).when(this.scheduledJobService).runPostLiveConfigChanges(
        Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.anyLong());
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.post(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.RUN_POST_LIVE_CONFIG_CHANGES)
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
          .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
          .param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    } catch (Exception e) {
    }
    finally {
      Mockito.verify(this.scheduledJobService).runPostLiveConfigChanges(
          Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.anyLong());
    }
  }

  @Test
   void autoApprovalOfPendingProductsTest() throws Exception {
    Mockito.doNothing().when(this.scheduledJobService).autoApprovePendingProducts(DEFAULT_STORE_ID, Constants.CREATED_DATE, Constants.ASC, Constants.PENDING);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ScheduledJobControllerPath.BASE_PATH
        + ScheduledJobControllerPath.AUTO_APPROVAL_OF_PENDING_PRODUCTS)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", DEFAULT_STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.scheduledJobService).autoApprovePendingProducts(DEFAULT_STORE_ID, Constants.CREATED_DATE, Constants.ASC, Constants.PENDING);
  }

  @Test
   void autoApprovalOfPendingProductsFailTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.scheduledJobService).autoApprovePendingProducts(DEFAULT_STORE_ID, Constants.CREATED_DATE, Constants.ASC, Constants.PENDING);
      this.mockMvc.perform(MockMvcRequestBuilders.post(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.AUTO_APPROVAL_OF_PENDING_PRODUCTS)
          .accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .param("storeId", DEFAULT_STORE_ID)
          .param("channelId", CHANNEL_ID)
          .param("clientId", CLIENT_ID)
          .param("requestId", REQUEST_ID)
          .param("username", USERNAME))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.scheduledJobService).autoApprovePendingProducts(DEFAULT_STORE_ID, Constants.CREATED_DATE, Constants.ASC, Constants.PENDING);
  }

  @Test
   void retryProductsByActionTest() throws Exception {
    Mockito.doNothing().when(this.scheduledJobService).retryProductsByAction(DEFAULT_STORE_ID, Constants.CREATED_DATE, Constants.ASC, Constants.AUTO_NEED_REVISION);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ScheduledJobControllerPath.BASE_PATH
        + ScheduledJobControllerPath.RETRY_PRODUCTS_BY_ACTION)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", DEFAULT_STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.scheduledJobService).retryProductsByAction(DEFAULT_STORE_ID, Constants.CREATED_DATE, Constants.ASC, Constants.AUTO_NEED_REVISION);
  }

  @Test
   void retryProductsByActionFailTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.scheduledJobService).retryProductsByAction(DEFAULT_STORE_ID, Constants.CREATED_DATE, Constants.ASC, Constants.AUTO_NEED_REVISION);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.RETRY_PRODUCTS_BY_ACTION)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", DEFAULT_STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.scheduledJobService).retryProductsByAction(DEFAULT_STORE_ID, Constants.CREATED_DATE, Constants.ASC, Constants.AUTO_NEED_REVISION);
  }

  @Test
   void addProductToAutoApprovalConfigChangeTest() throws Exception {
    Mockito.doNothing().when(this.scheduledJobService).addProductsForAutoApprovalOnConfigChange(DEFAULT_STORE_ID);
    this.mockMvc.perform(MockMvcRequestBuilders.get(
                ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.Add_PRODUCT_TO_AUTO_APPROVAL)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.scheduledJobService).addProductsForAutoApprovalOnConfigChange(DEFAULT_STORE_ID);
  }

  @Test
   void addProductToAutoApprovalConfigChangeExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.scheduledJobService).addProductsForAutoApprovalOnConfigChange(DEFAULT_STORE_ID);
    this.mockMvc.perform(MockMvcRequestBuilders.get(
                ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.Add_PRODUCT_TO_AUTO_APPROVAL)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.scheduledJobService).addProductsForAutoApprovalOnConfigChange(DEFAULT_STORE_ID);
  }

  @Test
   void publishCommonImageMigrationRecordsTest() throws Exception {
    ProductCodeListRequest productCodeListRequest = new ProductCodeListRequest();
    productCodeListRequest.setProductList(new ArrayList<>());
    Mockito.doNothing().when(this.scheduledJobService)
        .publishPendingProductMigrationRecords(Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductCodeListRequest.class));
    this.mockMvc.perform(MockMvcRequestBuilders.post(
                ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.PUBLISH_PENDING_COMMON_IMAGE_RECORDS,
                ProductMigrationType.COMMON_IMAGE_MIGRATION.name())
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME).content(objectMapper.writeValueAsString(productCodeListRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.scheduledJobService).publishPendingProductMigrationRecords(Mockito.eq(DEFAULT_STORE_ID),
        Mockito.eq(ProductMigrationType.COMMON_IMAGE_MIGRATION.name()), productCodeListRequestArgumentCaptor.capture());
  }

  @Test
   void publishCommonImageMigrationRecordsNoRequestBodyTest() throws Exception {
    Mockito.doNothing().when(this.scheduledJobService)
        .publishPendingProductMigrationRecords(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(ProductCodeListRequest.class));
    this.mockMvc.perform(MockMvcRequestBuilders.post(
                ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.PUBLISH_PENDING_COMMON_IMAGE_RECORDS,
                ProductMigrationType.COMMON_IMAGE_MIGRATION.name()).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
        .contentType(MediaType.APPLICATION_JSON).content("{\"productList\":[]}"))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.scheduledJobService)
        .publishPendingProductMigrationRecords(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(ProductCodeListRequest.class));
  }

  @Test
   void publishCommonImageMigrationRecordsExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.scheduledJobService)
        .publishPendingProductMigrationRecords(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(ProductCodeListRequest.class));
    this.mockMvc.perform(MockMvcRequestBuilders.post(
            ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.PUBLISH_PENDING_COMMON_IMAGE_RECORDS,
            ProductMigrationType.COMMON_IMAGE_MIGRATION.name()).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
        .contentType(MediaType.APPLICATION_JSON).content("{\"productList\":[]}"))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.scheduledJobService)
        .publishPendingProductMigrationRecords(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(ProductCodeListRequest.class));
  }

  @Test
   void syncNeedCorrectionProductsTest() throws Exception {
    Mockito.doNothing().when(this.scheduledJobService).syncNeedCorrectionProducts(Mockito.anyString(), Mockito.anyInt());
    this.mockMvc.perform(MockMvcRequestBuilders.get(
                ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.SYNC_NEED_CORRECTION_PRODUCTS)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("numOfHours", String.valueOf(NUM_OF_HOURS)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.scheduledJobService).syncNeedCorrectionProducts(Mockito.anyString(), Mockito.anyInt());
  }

  @Test
   void syncNeedCorrectionProductsExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.scheduledJobService)
        .syncNeedCorrectionProducts(Mockito.anyString(), Mockito.anyInt());
    this.mockMvc.perform(MockMvcRequestBuilders.get(
                ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.SYNC_NEED_CORRECTION_PRODUCTS)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("numOfHours", String.valueOf(NUM_OF_HOURS)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.scheduledJobService).syncNeedCorrectionProducts(Mockito.anyString(), Mockito.anyInt());
  }

  @Test
  void sendMailForEvidenceRequestedTest() throws Exception {
    Mockito.doNothing().when(this.productEmailService)
      .sendProductMailEventsToBusinessPartnersForSuspension(Mockito.anyString(),
        Mockito.anyString());
    this.mockMvc.perform(MockMvcRequestBuilders.get(ScheduledJobControllerPath.BASE_PATH
          + ScheduledJobControllerPath.SEND_EVIDENCE_REQUESTED_MAIL).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
        .param("requestId", REQUEST_ID).param("productEmailType", "productEmailType"))
      .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.productEmailService)
      .sendProductMailEventsToBusinessPartnersForSuspension(Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  void sendMailForEvidenceRequestedExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.productEmailService)
      .sendProductMailEventsToBusinessPartnersForSuspension(Mockito.anyString(), Mockito.anyString());
    this.mockMvc.perform(MockMvcRequestBuilders.get(ScheduledJobControllerPath.BASE_PATH
          + ScheduledJobControllerPath.SEND_EVIDENCE_REQUESTED_MAIL).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
        .param("requestId", REQUEST_ID).param("productEmailType", "productEmailType"))
      .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.productEmailService)
      .sendProductMailEventsToBusinessPartnersForSuspension(Mockito.anyString(),
        Mockito.anyString());
  }
}
