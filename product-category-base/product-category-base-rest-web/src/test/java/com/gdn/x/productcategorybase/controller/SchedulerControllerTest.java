package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;

import com.gdn.x.productcategorybase.ProductMigrationStatus;
import com.gdn.x.productcategorybase.dto.request.ProductMigrationRequest;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.ProductMigrationType;
import com.gdn.x.productcategorybase.SchedulerApiPath;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.service.SchedulerService;

public class SchedulerControllerTest {
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String REQUEST_ID = "REQ-001";
  private static final String STORE_ID = "10001";
  private static final String USERNAME = "developer";
  private static final String DAYS_THRESHOLD = "1";
  private static final String PRODUCT_CODE = "productCode";


  @InjectMocks
  private SchedulerController schedulerController;

  @Mock
  private SchedulerService schedulerService;

  @Captor
  private ArgumentCaptor<ProductCodesRequest> productCodeListRequestArgumentCaptor;


  private MockMvc mockMvc;
  private ObjectMapper objectMapper = new ObjectMapper();

  @BeforeEach
  public void setUp() {
    initMocks(this);
    this.mockMvc = standaloneSetup(this.schedulerController).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(schedulerService);
  }

  @Test
  public void backfillProductExtractedAttributeTest() throws Exception {
    Mockito.doNothing().when(schedulerService)
        .processPublishingProductAttributeExtractionBackFilling(STORE_ID, USERNAME, "PENDING");
    this.mockMvc.perform(get(SchedulerApiPath.BASE_PATH + SchedulerApiPath.BACKFILL_PRODUCT_ATTRIBUTE_EXTRACTED)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME).param("status", "PENDING")).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(schedulerService).processPublishingProductAttributeExtractionBackFilling(STORE_ID, USERNAME, "PENDING");
  }

  @Test
  public void publishCommonImageMigrationRecordsTest() throws Exception {
    ProductCodesRequest productCodeListRequest = new ProductCodesRequest();
    productCodeListRequest.setProductCodes(new ArrayList<>());
    Mockito.doNothing().when(this.schedulerService)
        .publishPendingProductMigrationRecords(Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductCodesRequest.class));
    this.mockMvc.perform(MockMvcRequestBuilders.post(
                SchedulerApiPath.BASE_PATH + SchedulerApiPath.PUBLISH_PENDING_COMMON_IMAGE_RECORDS,
                ProductMigrationType.COMMON_IMAGE_MIGRATION.name())
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME).content(objectMapper.writeValueAsString(productCodeListRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.schedulerService).publishPendingProductMigrationRecords(Mockito.eq(STORE_ID),
        Mockito.eq(ProductMigrationType.COMMON_IMAGE_MIGRATION.name()), productCodeListRequestArgumentCaptor.capture());
  }

  @Test
  public void publishCommonImageMigrationRecordsNoRequestBodyTest() throws Exception {
    Mockito.doNothing().when(this.schedulerService)
        .publishPendingProductMigrationRecords(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(ProductCodesRequest.class));
    this.mockMvc.perform(MockMvcRequestBuilders.post(
                SchedulerApiPath.BASE_PATH + SchedulerApiPath.PUBLISH_PENDING_COMMON_IMAGE_RECORDS,
                ProductMigrationType.COMMON_IMAGE_MIGRATION.name()).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.schedulerService)
        .publishPendingProductMigrationRecords(Mockito.any(), Mockito.any(),
            Mockito.any());
  }

  @Test
  public void publishCommonImageMigrationRecordsExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.schedulerService)
        .publishPendingProductMigrationRecords(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(ProductCodesRequest.class));
    this.mockMvc.perform(MockMvcRequestBuilders.post(
                SchedulerApiPath.BASE_PATH + SchedulerApiPath.PUBLISH_PENDING_COMMON_IMAGE_RECORDS,
                ProductMigrationType.COMMON_IMAGE_MIGRATION.name()).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
            .contentType(MediaType.APPLICATION_JSON).content("{\"productList\":[]}"))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.schedulerService)
        .publishPendingProductMigrationRecords(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(ProductCodesRequest.class));
  }

  @Test
  public void deleteArchivedProductDataTest() throws Exception {
    Mockito.doNothing().when(schedulerService).deleteArchiveProductData(STORE_ID);
    this.mockMvc.perform(
        get(SchedulerApiPath.BASE_PATH + SchedulerApiPath.DELETE_ARCHIVED_PRODUCT_DATA).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME)).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(schedulerService).deleteArchiveProductData(STORE_ID);
  }

  @Test
  public void deleteArchivedProductDataErrorTest() throws Exception {
    Mockito.doThrow(Exception.class).when(schedulerService).deleteArchiveProductData(STORE_ID);
    this.mockMvc.perform(
        get(SchedulerApiPath.BASE_PATH + SchedulerApiPath.DELETE_ARCHIVED_PRODUCT_DATA).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME)).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(schedulerService).deleteArchiveProductData(STORE_ID);
  }

  @Test
  public void publishRejectedProductForDeletionTest() throws Exception {
    Mockito.doNothing().when(schedulerService).publishRejectedProductForDeletion(STORE_ID);
    this.mockMvc.perform(
        get(SchedulerApiPath.BASE_PATH + SchedulerApiPath.PUBLISH_REJECTED_PRODUCTS_FOR_DELETION).param("storeId",
                STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME)).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(schedulerService).publishRejectedProductForDeletion(STORE_ID);
  }

  @Test
  void activateBrandAuthorisationTest() throws Exception {
    Mockito.doNothing().when(this.schedulerService)
      .activateUpcomingBrandAuthorisation(Mockito.anyString(), Mockito.anyInt());
    this.mockMvc.perform(MockMvcRequestBuilders.post(
          SchedulerApiPath.BASE_PATH + SchedulerApiPath.ACTIVATE_BRAND_AUTHORISATION)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("requestId", REQUEST_ID)
        .param("daysThreshold", DAYS_THRESHOLD)).andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.schedulerService).activateUpcomingBrandAuthorisation(STORE_ID, 1);
  }

  @Test
  void activateBrandAuthorisationExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.schedulerService)
      .activateUpcomingBrandAuthorisation(STORE_ID, 1);
    this.mockMvc.perform(MockMvcRequestBuilders.post(
          SchedulerApiPath.BASE_PATH + SchedulerApiPath.ACTIVATE_BRAND_AUTHORISATION)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("requestId", REQUEST_ID)
        .param("daysThreshold", DAYS_THRESHOLD)).andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.schedulerService).activateUpcomingBrandAuthorisation(STORE_ID, 1);
  }

  @Test
  void updateProductMigrationStatusTest() throws Exception {
    ProductMigrationRequest productMigrationRequest = new ProductMigrationRequest();
    productMigrationRequest.setProductCode(PRODUCT_CODE);
    productMigrationRequest.setUpdatedStatus(ProductMigrationStatus.FAILED.name());
    productMigrationRequest.setMigrationType(ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name());
    Mockito.doNothing().when(this.schedulerService).updateProductMigrationStatus(
      STORE_ID, REQUEST_ID, productMigrationRequest);

    this.mockMvc.perform(MockMvcRequestBuilders.post(
          SchedulerApiPath.BASE_PATH + SchedulerApiPath.UPDATE_PRODUCT_MIGRATION_STATUS)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .content(new ObjectMapper().writeValueAsString(productMigrationRequest)))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(true)));

    Mockito.verify(this.schedulerService).updateProductMigrationStatus(
      STORE_ID, REQUEST_ID, productMigrationRequest);
  }

  @Test
  void updateProductMigrationStatusExceptionTest() throws Exception {
    ProductMigrationRequest productMigrationRequest = new ProductMigrationRequest();
    productMigrationRequest.setProductCode(PRODUCT_CODE);
    productMigrationRequest.setUpdatedStatus(ProductMigrationStatus.FAILED.name());
    productMigrationRequest.setMigrationType(ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name());
    Mockito.doThrow(RuntimeException.class).when(this.schedulerService).updateProductMigrationStatus(
      STORE_ID, REQUEST_ID, productMigrationRequest);

    this.mockMvc.perform(MockMvcRequestBuilders.post(
          SchedulerApiPath.BASE_PATH + SchedulerApiPath.UPDATE_PRODUCT_MIGRATION_STATUS)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .content(new ObjectMapper().writeValueAsString(productMigrationRequest)))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(false)));

    Mockito.verify(this.schedulerService).updateProductMigrationStatus(
      STORE_ID, REQUEST_ID, productMigrationRequest);
  }

  @Test
  void sendNearExpiryMailsTest() throws Exception {
    Mockito.doNothing().when(this.schedulerService).sendNearExpiryMails(STORE_ID);
    this.mockMvc.perform(MockMvcRequestBuilders.post(
                SchedulerApiPath.BASE_PATH + SchedulerApiPath.SEND_NEAR_EXPIRY_MAIL)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID).param("requestId", REQUEST_ID))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.schedulerService).sendNearExpiryMails(STORE_ID);
  }
}
