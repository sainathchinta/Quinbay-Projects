package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.any;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gdn.partners.pcu.internal.client.model.request.ProductAttributeExtractionsRequest;
import com.gdn.partners.pcu.internal.web.model.request.BigQueryFetchWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.DeleteProductWebRequest;
import com.gdn.x.productcategorybase.dto.request.SystemParameterRequest;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.UtilityApiPath;
import com.gdn.partners.pcu.internal.service.UtilityService;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;

@AutoConfigureMockMvc
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class UtilityControllerTest extends TestHelper {

  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String REVIEW_TYPE = "reviewType";
  private static final String STATE = "state";
  private static final String PRODUCT_ID = "productId";
  private static final String REQUEST_ID = "requestId";
  private static final String PROCESS_NAME = "processName";

  private String productWebRequestAsString;

  @InjectMocks
  private UtilityController utilityController;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private UtilityService utilityService;

  @BeforeEach
  public void setUp() throws IOException {
    mockMvc = MockMvcBuilders.standaloneSetup(utilityController).build();

    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(clientParameterHelper.getClientId()).thenReturn(Constants.CLIENT_ID);
    when(clientParameterHelper.getChannelId()).thenReturn(Constants.CHANNEL_ID);

    productWebRequestAsString = FileUtils.readFileToString(
        new File(Thread.currentThread().getContextClassLoader().getResource("VendorUpdateTestRequest.json").getFile()),
        "UTF-8");
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(utilityService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  public void publishAddEditedEventToPDT() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(UtilityApiPath.BASE_PATH + UtilityApiPath.PUBLISH_ADD_EDITED_EVENT_TO_PDT, PRODUCT_CODE)
            .param("reviewType", REVIEW_TYPE).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).publishAddEditedEventToPDT(PRODUCT_CODE, REVIEW_TYPE);
  }

  @Test
  public void publishAddRevisedEventToPDT() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(UtilityApiPath.BASE_PATH + UtilityApiPath.PUBLISH_ADD_REVISED_EVENT_TO_PDT, PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).publishAddRevisedEventToPDT(PRODUCT_CODE);
  }

  @Test
  public void retryProductImageResize() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(UtilityApiPath.BASE_PATH + UtilityApiPath.RETRY_PRODUCT_IMAGE_RESIZE, PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).retryProductImageResize(PRODUCT_CODE);
  }

  @Test
  public void retryEditedProductImageResize() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(UtilityApiPath.BASE_PATH + UtilityApiPath.RETRY_EDITED_PRODUCT_IMAGE_RESIZE, PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).retryEditedProductImageResize(PRODUCT_CODE);
  }

  @Test
  public void updatePBPProductWorkflow() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(UtilityApiPath.BASE_PATH + UtilityApiPath.UPDATE_PBP_PRODUCT_WORKFLOW, PRODUCT_CODE).param("state", STATE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).updatePBPProductWorkflow(PRODUCT_CODE, STATE);
  }

  @Test
  public void updatePBPReviewPending() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(UtilityApiPath.BASE_PATH + UtilityApiPath.UPDATE_PBP_REVIEW_PENDING, PRODUCT_CODE)
            .param("reviewPending", String.valueOf(false)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).updatePBPReviewPending(PRODUCT_CODE, false);
  }

  @Test
  public void updatePBPActivatedAndViewable() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(UtilityApiPath.BASE_PATH + UtilityApiPath.UPDATE_PBP_ACTIVATED_AND_VIEWABLE, PRODUCT_CODE)
            .param("activated", String.valueOf(true))
            .param("viewable", String.valueOf(false)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).updatePBPActivatedAndViewable(PRODUCT_CODE, true, false);
  }

  @Test
  public void checkProductAutoApprovalEligibility() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        post(UtilityApiPath.BASE_PATH + UtilityApiPath.CHECK_AUTO_APPROVAL_ELIGIBILITY, PRODUCT_CODE)
            .param("reviewPending", String.valueOf(false)).contentType(MediaType.APPLICATION_JSON)
            .content(productWebRequestAsString).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).checkProductAutoApprovalEligibility(Mockito.any(), Mockito.any());
  }

  @Test
  public void republishPCBProductPublishEvent() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(UtilityApiPath.BASE_PATH + UtilityApiPath.REPUBLISH_PCB_PRODUCT_PUBLISH_EVENT, PRODUCT_CODE)
            .param("operationType", PRODUCT_ID).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).republishPCBProductPublishEvent(PRODUCT_CODE, PRODUCT_ID);
  }

  @Test
  public void clearPCBProductCache() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(UtilityApiPath.BASE_PATH + UtilityApiPath.CLEAR_PCB_PRODUCT_CACHE, PRODUCT_CODE)
            .param("productId", PRODUCT_ID).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).clearPCBProductCache(PRODUCT_CODE, PRODUCT_ID);
  }

  @Test
  public void updatePCBProductViewable() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(UtilityApiPath.BASE_PATH + UtilityApiPath.UPDATE_PCB_PRODUCT_VIEWABLE, PRODUCT_CODE)
            .param("viewable", String.valueOf(false)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).updatePCBProductViewable(PRODUCT_CODE, false);
  }

  @Test
  public void updatePCBProductReviewPending() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(UtilityApiPath.BASE_PATH + UtilityApiPath.UPDATE_PCB_PRODUCT_REVIEW_PENDING, PRODUCT_CODE)
            .param("reviewPending", String.valueOf(false)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).updatePCBProductReviewPending(PRODUCT_CODE, false);
  }

  @Test
  public void updatePDTState() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        post(UtilityApiPath.BASE_PATH + UtilityApiPath.UPDATE_PDT_STATE_UPDATE, PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON).content(productWebRequestAsString)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).updatePDTState(Mockito.any(), Mockito.any());
  }

  @Test
  public void takeDownOrReactivateProduct() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        post(UtilityApiPath.BASE_PATH + UtilityApiPath.TAKE_DOWN_OR_REACTIVE_PRODUCT_IN_X_PRODUCT, PRODUCT_SKU)
            .param("forceReview", String.valueOf(false)).contentType(MediaType.APPLICATION_JSON)
            .content(String.valueOf(new ArrayList())).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).takeDownOrReactivateProduct(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any());
  }

  @Test
  public void abortPendingBulkProcessByIdTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
      put(UtilityApiPath.BASE_PATH + UtilityApiPath.ABORT_PENDING_BULK_PROCESS_DATA_BY_ID).param(
          "id", PRODUCT_ID).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(utilityService).abortPendingBulkProcessById(PRODUCT_ID);
  }

  @Test
  public void abortPendingDownloadTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
            put(UtilityApiPath.BASE_PATH + UtilityApiPath.ABORT_PENDING_DOWNLOADS_BY_ENTITY).param("entityType", PRODUCT_ID).param("status", PRODUCT_CODE).contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
            .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(utilityService).abortPendingDownloadsByEntity(PRODUCT_ID, PRODUCT_CODE);
  }


  @Test
  public void deleteProductCollectionTest() throws Exception {
    DeleteProductWebRequest deleteProductWebRequest = new DeleteProductWebRequest();
    deleteProductWebRequest.setProductCode(PRODUCT_CODE);
    Mockito.doNothing().when(utilityService).deleteProductCollection(Mockito.any(),Mockito.any(),
      Mockito.any(),Mockito.any(),Mockito.any(),Mockito.anyBoolean(),Mockito.any(
        DeleteProductWebRequest.class));
    MockHttpServletRequestBuilder requestBuilder =
      post(UtilityApiPath.BASE_PATH + UtilityApiPath.DELETE_PRODUCT_BY_PRODUCT_CODE).param(
          "needEmailNotification", String.valueOf(false)).contentType(MediaType.APPLICATION_JSON).content(
          toJson(deleteProductWebRequest))
        .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper, times(2)).getRequestId();
    Mockito.verify(clientParameterHelper, times(2)).getUsername();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getChannelId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getClientId();
    Mockito.verify(utilityService).deleteProductCollection(Mockito.any(),Mockito.any(),
      Mockito.any(),Mockito.any(),Mockito.any(),Mockito.anyBoolean(),Mockito.any(
        DeleteProductWebRequest.class));
  }

  @Test
  public void reindexActiveProductByProductCodeTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        post(UtilityApiPath.BASE_PATH + UtilityApiPath.REINDEX_ACTIVE_PRODUCT_BY_PRODUCT_CODE,
            PRODUCT_CODE).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(utilityService).reindexActiveProductByProductCode(PRODUCT_CODE);
  }

  @Test
  public void fetchBQDataForMasterSkuTest() throws Exception {
    BigQueryFetchWebRequest bigQueryFetchWebRequest = new BigQueryFetchWebRequest();
    bigQueryFetchWebRequest.setAnchorDetailsDeltaFetch(true);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder = put(UtilityApiPath.BASE_PATH
        + UtilityApiPath.FETCH_DATA_FROM_BIG_QUERY_FOR_MASTER_SKU_REVIEW).param("processName",
            PROCESS_NAME).content(toJson(bigQueryFetchWebRequest))
        .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper, times(2)).getRequestId();
    Mockito.verify(utilityService)
        .fetchDataFromBigQueryForMasterSkuReview(eq(PROCESS_NAME), ArgumentMatchers.any());
  }

  @Test
  public void migrateProductAndL5DetailByProductSkuTest() throws Exception {
    ProductAndL5MigrationRequest migrationRequest = new ProductAndL5MigrationRequest();
    migrationRequest.setProductSku(PRODUCT_SKU);
    migrationRequest.setDimensionsMissing(true);
    migrationRequest.setIncludeMfd(true);
    migrationRequest.setBuyable(false);
    migrationRequest.setDiscoverable(false);
    migrationRequest.setL5Updated(true);
    when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(clientParameterHelper.getClientId()).thenReturn(Constants.CLIENT_ID);
    when(clientParameterHelper.getChannelId()).thenReturn(Constants.CHANNEL_ID);
    Mockito.doNothing().when(utilityService).migrateProductAndL5DetailsByProductSku(
      eq(Constants.STORE_ID), eq(REQUEST_ID), eq(Constants.CLIENT_ID), eq(Constants.CHANNEL_ID),
      ArgumentMatchers.any(ProductAndL5MigrationRequest.class), eq(Constants.USER_NAME));

    MockHttpServletRequestBuilder requestBuilder =
      post(UtilityApiPath.BASE_PATH + UtilityApiPath.MIGRATE_PRODUCT_AND_L5_DETAIL_BY_PRODUCT_SKU)
      .param("storeId", Constants.STORE_ID)
      .param("username", Constants.USER_NAME)
      .content(toJson(migrationRequest))
      .contentType(MediaType.APPLICATION_JSON)
      .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder)
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)))
      .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));

    Mockito.verify(clientParameterHelper, times(1)).getRequestId();
    Mockito.verify(clientParameterHelper, times(1)).getClientId();
    Mockito.verify(clientParameterHelper, times(1)).getChannelId();
    Mockito.verify(utilityService).migrateProductAndL5DetailsByProductSku(eq(Constants.STORE_ID),
      eq(REQUEST_ID),
      eq(Constants.CLIENT_ID), eq(Constants.CHANNEL_ID),
      ArgumentMatchers.any(ProductAndL5MigrationRequest.class),
      eq(Constants.USER_NAME));
  }


  @Test
  public void systemParameterUpdateInPCBTest() throws Exception {
    SystemParameterRequest systemParameterRequest = new SystemParameterRequest();
    when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(clientParameterHelper.getClientId()).thenReturn(Constants.CLIENT_ID);
    when(clientParameterHelper.getChannelId()).thenReturn(Constants.CHANNEL_ID);
    Mockito.doNothing().when(utilityService)
      .updateSystemParameterInPCB(eq(Constants.STORE_ID), eq(REQUEST_ID), eq(Constants.CHANNEL_ID),
        eq(Constants.CLIENT_ID), ArgumentMatchers.any(SystemParameterRequest.class));

    MockHttpServletRequestBuilder requestBuilder =
      put(UtilityApiPath.BASE_PATH + UtilityApiPath.UPDATE_SYSTEM_PARAMETER_IN_PCB).param(
          "storeId", Constants.STORE_ID).param("username", Constants.USER_NAME)
        .content(toJson(systemParameterRequest)).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)))
      .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));

    Mockito.verify(clientParameterHelper, times(1)).getRequestId();
    Mockito.verify(clientParameterHelper, times(1)).getClientId();
    Mockito.verify(clientParameterHelper, times(1)).getChannelId();
    Mockito.verify(utilityService)
      .updateSystemParameterInPCB(eq(Constants.STORE_ID), eq(REQUEST_ID), eq(Constants.CHANNEL_ID),
        eq(Constants.CLIENT_ID), ArgumentMatchers.any(SystemParameterRequest.class));
  }

  @Test
  void generateProductScoreByProductSkuOrProductCodeTest() throws Exception {
    String storeId = "10001";
    String channelId = "ProductChannel";
    String clientId = "ProductClient";
    String requestId = "request-40de6d5c-6dd7-45cb-a141-e10570774b06";
    String username = "user1";
    boolean updateCategory = false;
    String productSku = "sku1";
    String productCode = "code1";

    MockHttpServletRequestBuilder requestBuilder =
      get(UtilityApiPath.BASE_PATH + UtilityApiPath.GENERATE_PRODUCT_SCORE).param("storeId",
          storeId).param("channelId", channelId).param("clientId", clientId)
        .param("requestId", requestId).param("username", username)
        .param("updateCategory", String.valueOf(updateCategory)).param("productSku", productSku)
        .param("productCode", productCode).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON);

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)))
      .andExpect(jsonPath("$.requestId", equalTo(requestId)));

    Mockito.verify(utilityService)
      .generateProductScoreByProductSkuOrProductCode(storeId, channelId, clientId, requestId,
        username, updateCategory, productSku, productCode);
  }

  @Test
  public void publishProductAttributeExtractionsTest() throws Exception {
    ProductAttributeExtractionsRequest productAttributeExtractionsRequest = new ProductAttributeExtractionsRequest();
    productAttributeExtractionsRequest.setProductSkuList(List.of(PRODUCT_SKU));
    Mockito.doNothing().when(utilityService)
        .publishProductAttributeExtractions(eq(Constants.STORE_ID),
            ArgumentMatchers.any(ProductAttributeExtractionsRequest.class));

    MockHttpServletRequestBuilder requestBuilder =
        post(UtilityApiPath.BASE_PATH + UtilityApiPath.PUBLISH_PRODUCT_ATTRIBUTE_EXTRACTIONS)
            .param("storeId", Constants.STORE_ID)
            .param("channelId", Constants.CHANNEL_ID)
            .param("clientId", Constants.CLIENT_ID)
            .param("username", Constants.USER_NAME)
            .param("requestId", REQUEST_ID)
            .content(toJson(productAttributeExtractionsRequest))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));

    Mockito.verify(utilityService).publishProductAttributeExtractions(eq(Constants.STORE_ID),
        ArgumentMatchers.any(ProductAttributeExtractionsRequest.class));
  }
}