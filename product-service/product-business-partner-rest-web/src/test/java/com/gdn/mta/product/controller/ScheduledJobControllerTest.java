package com.gdn.mta.product.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.enums.FbbMigrationConstants;
import com.gdn.mta.product.service.ProductFbbMigrationService;
import com.gdn.mta.product.service.ScheduledJobService;
import com.gdn.mta.product.web.model.ScheduledJobControllerPath;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import org.apache.http.client.utils.URIBuilder;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.net.URI;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

public class ScheduledJobControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel-1";
  private static final String DEFAULT_CLIENT_ID = "client-1";
  private static final String DEFAULT_USERNAME = "com.gdn.mta.developer";
  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String ORDER_BY = "orderBy";
  private static final String ORDER_IN = "orderIn";
  private static final String MIGRATION_TYPE = "migration-type";

  @InjectMocks
  private ScheduledJobController controller;

  @Mock
  private ScheduledJobService scheduledJobService;

  @Mock
  private ProductFbbMigrationService productFbbMigrationService;

  @Captor
  private ArgumentCaptor<Long> longArgumentCaptor;

  private MockMvc mockMvc;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders
        .standaloneSetup(controller)
        .setMessageConverters(new ByteArrayHttpMessageConverter(),
            new StringHttpMessageConverter(), new ResourceHttpMessageConverter(),
            new FormHttpMessageConverter(), new MappingJackson2HttpMessageConverter()).build();
  }

  @Test
  public void runPostLiveReviewConfigChangesTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.RUN_POST_LIVE_CONFIG_CHANGES)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType
        (MediaType.APPLICATION_JSON)).andExpect
        (MockMvcResultMatchers.status().isOk()).andReturn();
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, -1);
    Mockito.verify(scheduledJobService).runPostLiveConfigChanges(
        Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        longArgumentCaptor.capture());
    Assertions.assertEquals(calendar.get(Calendar.DAY_OF_MONTH), new Date(longArgumentCaptor.getValue()).getDate());
  }

  @Test
  public void runPostLiveReviewConfigChangesWithDateParmeterTest() throws Exception {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, -1);
    URI uri =
        new URIBuilder()
            .setPath(
                ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.RUN_POST_LIVE_CONFIG_CHANGES)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("dateInMillis", String.valueOf(calendar.getTimeInMillis()))
            .build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType
        (MediaType.APPLICATION_JSON)).andExpect
        (MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService).runPostLiveConfigChanges(
        Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        longArgumentCaptor.capture());
  }

  @Test
  public void runPostLiveReviewConfigChangesExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(scheduledJobService).runPostLiveConfigChanges(
        Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        longArgumentCaptor.capture());
    URI uri =
        new URIBuilder()
            .setPath(
                ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.RUN_POST_LIVE_CONFIG_CHANGES)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType
        (MediaType.APPLICATION_JSON)).andExpect
        (MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService).runPostLiveConfigChanges(
        Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        longArgumentCaptor.capture());
  }

  @Test
  public void migrateProductsTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.MIGRATE_PRODUCTS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService).migrateProducts(DEFAULT_STORE_ID, null);
  }

  @Test
  public void migrateProductsWithRequestTest() throws Exception {
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest(Arrays.asList(PRODUCT_CODE));
    String requestBody = new ObjectMapper().writeValueAsString(simpleListStringRequest);
    URI uri =
        new URIBuilder().setPath(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.MIGRATE_PRODUCTS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService).migrateProducts(DEFAULT_STORE_ID, Arrays.asList(PRODUCT_CODE));
  }


  @Test
  public void migrateProductsWithEmptyRequestTest() throws Exception {
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    String requestBody = new ObjectMapper().writeValueAsString(simpleListStringRequest);
    URI uri =
        new URIBuilder().setPath(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.MIGRATE_PRODUCTS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService).migrateProducts(DEFAULT_STORE_ID, null);
  }

  @Test
  public void retryFailedMigratedProductsTest() throws Exception {
    Mockito.doNothing().when(scheduledJobService).retryFailedMigratedProducts(DEFAULT_STORE_ID, null);
    URI uri =
        new URIBuilder()
            .setPath(
                ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.RETRY_MIGRATE_PRODUCTS)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType
        (MediaType.APPLICATION_JSON)).andExpect
        (MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService).retryFailedMigratedProducts(
        DEFAULT_STORE_ID, null);
  }

  @Test
  public void retryFailedMigratedProductsWithProductCodeTest() throws Exception {
    Mockito.doNothing().when(scheduledJobService)
        .retryFailedMigratedProducts(DEFAULT_STORE_ID, Arrays.asList(PRODUCT_CODE));
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest(Arrays.asList(PRODUCT_CODE));
    String requestBody = new ObjectMapper().writeValueAsString(simpleListStringRequest);
    URI uri = new URIBuilder()
        .setPath(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.RETRY_MIGRATE_PRODUCTS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(requestBody)).andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService).retryFailedMigratedProducts(DEFAULT_STORE_ID, Arrays.asList(PRODUCT_CODE));
  }

  @Test
  public void retryFailedMigratedProductsWithEmptyListTest() throws Exception {
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    String requestBody = new ObjectMapper().writeValueAsString(simpleListStringRequest);
    URI uri = new URIBuilder()
        .setPath(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.RETRY_MIGRATE_PRODUCTS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(requestBody)).andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService).retryFailedMigratedProducts(DEFAULT_STORE_ID, null);
  }


  @Test
  public void updateMigrationStatusTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.UPDATE_MIGRATION_STATUS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService).updateMigrationStatus(DEFAULT_STORE_ID);
  }

  @Test
  public void publishImageQcBacklogProductsTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.PUBLISH_IMAGE_QC_BACKLOG_PRODUCTS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("orderProductsBy", ORDER_BY)
        .addParameter("orderProductsIn", ORDER_IN).build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService).publishImageQcBacklogProducts(DEFAULT_STORE_ID, ORDER_BY, ORDER_IN);
  }

  @Test
  public void syncReviewTypeProductsTest() throws Exception {
    URI uri = new URIBuilder()
      .setPath(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.SYNC_IN_REVIEW_PRODUCTS)
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
      .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
      MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService).syncInReviewProducts(DEFAULT_STORE_ID);
  }

  @Test
  public void syncActiveProductsTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.SYNC_ACTIVE_PRODUCTS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService).syncActiveProducts(DEFAULT_STORE_ID);
  }

  @Test
  public void syncPreLiveProductsTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.SYNC_PRE_LIVE_PRODUCTS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService).syncPreLiveProducts(DEFAULT_STORE_ID);
  }

  @Test
  public void migrateFbbProductsTest() throws Exception {
    this.mockMvc.perform(MockMvcRequestBuilders.post(
          ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.MIGRATE_FBB_PRODUCTS,
          FbbMigrationConstants.BACK_FILL_FBB_FLAG.name()).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", DEFAULT_STORE_ID).param("channelId", DEFAULT_CHANNEL_ID)
        .param("clientId", DEFAULT_CLIENT_ID).param("requestId", DEFAULT_REQUEST_ID)
        .param("username", DEFAULT_USERNAME)).andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(productFbbMigrationService).migrateFbbProducts(DEFAULT_STORE_ID,
      FbbMigrationConstants.BACK_FILL_FBB_FLAG.name());
  }

  @Test
  public void addDeleteVariantRetryPublish() throws Exception {
    URI uri = new URIBuilder().setPath(
            ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.ADD_DELETE_VARIANT_RETRY_PUBLISH)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("productCode", PRODUCT_CODE).build();
    mockMvc.perform(
            MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService)
        .addDeleteVariantRetryPublishEvents(DEFAULT_STORE_ID, PRODUCT_CODE, DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
  }

  @Test
  public void addDeleteVariantRetryPublishExceptionTest() throws Exception {
    URI uri = new URIBuilder().setPath(
            ScheduledJobControllerPath.BASE_PATH + ScheduledJobControllerPath.ADD_DELETE_VARIANT_RETRY_PUBLISH)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("productCode", PRODUCT_CODE).build();
    Mockito.doThrow(RuntimeException.class).when(scheduledJobService)
        .addDeleteVariantRetryPublishEvents(DEFAULT_STORE_ID, PRODUCT_CODE, DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
    mockMvc.perform(
            MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(scheduledJobService).addDeleteVariantRetryPublishEvents(DEFAULT_STORE_ID, PRODUCT_CODE, DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(scheduledJobService);
  }
}