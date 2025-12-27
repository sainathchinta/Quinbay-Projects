package com.gdn.mta.product.controller;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.LogAuditTrailUpdatedOfflineProductResponse;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductBulkRequest;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductRequest;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductResponse;
import com.gda.mta.product.dto.response.SimpleBooleanResponse;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.entity.LogAuditTrailUpdatedOfflineProduct;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.service.UpdatedProductHistoryService;
import com.gdn.mta.product.service.LogAuditTrailUpdatedOfflineProductService;
import com.gdn.mta.product.web.model.UpdatedProductHistoryControllerPath;
import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.net.URI;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;

public class UpdatedProductHistoryControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel-1";
  private static final String DEFAULT_CLIENT_ID = "client-1";
  private static final String DEFAULT_USERNAME = "com.gdn.mta.developer";
  private static final Integer DEFAULT_PAGE = 0;
  private static final Integer DEFAULT_SIZE = 10;
  private static final VerificationMode AT_LEAST_ONE = Mockito.times(1);
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_SKU = UUID.randomUUID().toString();
  private static final String DEFAULT_PICKUP_POINT_CODE = "PP-123456";
  private static final String DAYS = "6";
  private static final String BATCH_SIZE = "100";

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;

  @InjectMocks
  UpdatedProductHistoryController updatedProductHistoryController;

  @Mock
  UpdatedProductHistoryService updatedProductHistoryService;

  @Mock
  private LogAuditTrailUpdatedOfflineProductService auditTrailOfflineProductService;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.setMockMvc(MockMvcBuilders
        .standaloneSetup(this.getUpdatedProductHistoryController())
        .setMessageConverters(new ByteArrayHttpMessageConverter(),
            new StringHttpMessageConverter(), new ResourceHttpMessageConverter(),
            new FormHttpMessageConverter(), new MappingJackson2HttpMessageConverter()).build());
    this.setObjectMapper(new ObjectMapper(new JsonFactory()));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void saveLogAuditTrailUpdatedProductSuccess() throws Exception {
    LogAuditTrailUpdatedProductBulkRequest request = new LogAuditTrailUpdatedProductBulkRequest();
    LogAuditTrailUpdatedProductRequest auditTrailUpdatedProductRequest =
        new LogAuditTrailUpdatedProductRequest();
    request.getLogAuditTrailUpdatedProductList().add
        (auditTrailUpdatedProductRequest);
    Mockito.doNothing().when(updatedProductHistoryService)
        .updateProductHistoryDeltailList(Mockito.anyList());
    URI uri =
        new URIBuilder()
            .setPath(UpdatedProductHistoryControllerPath.BASE_PATH + UpdatedProductHistoryControllerPath.SAVE)
            .addParameter("storeId", UpdatedProductHistoryControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", UpdatedProductHistoryControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", UpdatedProductHistoryControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", UpdatedProductHistoryControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", UpdatedProductHistoryControllerTest.DEFAULT_CLIENT_ID).build();
    this.getMockMvc()
        .perform(
            MockMvcRequestBuilders.post(uri).content(objectMapper.writeValueAsString(request))
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
  }

  @Test
  public void getProductUpdateLogs() throws Exception {
    Pageable pageable =
        PageRequest.of(UpdatedProductHistoryControllerTest.DEFAULT_PAGE,
            UpdatedProductHistoryControllerTest.DEFAULT_SIZE);
    Date accessTime = new Date();
    DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
    List<UpdatedProductHistory> updatedProductHistoryList =
        new ArrayList<UpdatedProductHistory>();
    UpdatedProductHistory updatedProductHistory1 = new UpdatedProductHistory();
    updatedProductHistory1.setAccessTime(accessTime);
    updatedProductHistoryList.add(updatedProductHistory1);
    Page<UpdatedProductHistory> logAuditTrailUpdatedProductPage =
        new PageImpl<UpdatedProductHistory>(updatedProductHistoryList);
    Mockito.when(updatedProductHistoryService.getAuditLogsForProduct(pageable, DEFAULT_SKU)).thenReturn(
        logAuditTrailUpdatedProductPage);
    LogAuditTrailUpdatedProductResponse logAuditTrailUpdatedProductResponse =
        new LogAuditTrailUpdatedProductResponse();
    logAuditTrailUpdatedProductResponse.setCreatedDateLog(dateFormat.format(accessTime));
    List<LogAuditTrailUpdatedProductResponse> logAuditTrailUpdatedProductResponseList =
        new ArrayList<LogAuditTrailUpdatedProductResponse>();
    logAuditTrailUpdatedProductResponseList.add(logAuditTrailUpdatedProductResponse);
    GdnRestListResponse<LogAuditTrailUpdatedProductResponse> response =
        new GdnRestListResponse<LogAuditTrailUpdatedProductResponse>(null, null, true,
            logAuditTrailUpdatedProductResponseList, new PageMetaData(pageable.getPageSize(),
                pageable.getPageNumber(), logAuditTrailUpdatedProductPage.getTotalElements()),
            DEFAULT_REQUEST_ID);
    URI uri =
        new URIBuilder()
            .setPath(
                UpdatedProductHistoryControllerPath.BASE_PATH + UpdatedProductHistoryControllerPath.GET_PRD_UPD_AUDITLOGS)
            .addParameter("storeId", UpdatedProductHistoryControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", UpdatedProductHistoryControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", UpdatedProductHistoryControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", UpdatedProductHistoryControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", UpdatedProductHistoryControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("page", String.valueOf(UpdatedProductHistoryControllerTest.DEFAULT_PAGE))
            .addParameter("size", String.valueOf(UpdatedProductHistoryControllerTest.DEFAULT_SIZE))
            .addParameter("gdnSku", UpdatedProductHistoryControllerTest.DEFAULT_SKU).build();
    MvcResult result =
        this.getMockMvc()
            .perform(
                MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    assertEquals(this.getObjectMapper().writeValueAsString(response), result.getResponse()
        .getContentAsString());
    verify(this.getUpdatedProductHistoryService(), UpdatedProductHistoryControllerTest.AT_LEAST_ONE)
        .getAuditLogsForProduct(pageable, DEFAULT_SKU);
  }

  @Test
  public void checkProductPriceChangeTest() throws Exception {
    Mockito.when(updatedProductHistoryService.isPriceChangedForSku(Mockito.eq(DEFAULT_SKU), Mockito.any(Date.class)))
        .thenReturn(new SimpleBooleanResponse(true));
    URI uri =
        new URIBuilder()
            .setPath(UpdatedProductHistoryControllerPath.BASE_PATH +
                UpdatedProductHistoryControllerPath.CHECK_PRODUCT_PRICE_CHANGE.replaceAll("\\{sku}", DEFAULT_SKU))
            .addParameter("storeId", UpdatedProductHistoryControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", UpdatedProductHistoryControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", UpdatedProductHistoryControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", UpdatedProductHistoryControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", UpdatedProductHistoryControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("timeStampFromInMillis", "10000")
            .build();
    this.getMockMvc().perform(MockMvcRequestBuilders.get(uri)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
  }

  @Test
  public void checkProductPriceChangeAndPPcodeTest() throws Exception {
    Mockito.when(
        updatedProductHistoryService.isPriceChangedForSkuAndPPCode(Mockito.eq(DEFAULT_SKU), Mockito.any(Date.class),
            Mockito.eq(DEFAULT_SKU))).thenReturn(new SimpleBooleanResponse(true));
    URI uri =
        new URIBuilder()
            .setPath(UpdatedProductHistoryControllerPath.BASE_PATH +
                UpdatedProductHistoryControllerPath.CHECK_PRODUCT_PRICE_CHANGE_FOR_L5.replaceAll("\\{sku}", DEFAULT_SKU)
                    .replace("\\{pickupPointCode}", DEFAULT_SKU))
            .addParameter("storeId", UpdatedProductHistoryControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", UpdatedProductHistoryControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", UpdatedProductHistoryControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", UpdatedProductHistoryControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", UpdatedProductHistoryControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("timeStampFromInMillis", "10000")
            .build();
    this.getMockMvc().perform(MockMvcRequestBuilders.get(uri)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
  }

  @Test
  public void getOfflineProductUpdateLogs() throws Exception {
    Pageable pageable =
        PageRequest.of(UpdatedProductHistoryControllerTest.DEFAULT_PAGE,
            UpdatedProductHistoryControllerTest.DEFAULT_SIZE);
    Date accessTime = new Date();
    DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");

    List<UpdatedProductHistory> logAuditTrailUpdatedProductList =
        new ArrayList<>();
    UpdatedProductHistory logAuditTrailUpdatedProduct1 = new UpdatedProductHistory();
    logAuditTrailUpdatedProduct1.setAccessTime(accessTime);
    logAuditTrailUpdatedProductList.add(logAuditTrailUpdatedProduct1);
    Page<UpdatedProductHistory> logAuditTrailUpdatedProductPage = new PageImpl<>(logAuditTrailUpdatedProductList);

    LogAuditTrailUpdatedOfflineProductResponse logAuditTrailUpdatedProductResponse =
        new LogAuditTrailUpdatedOfflineProductResponse();
    logAuditTrailUpdatedProductResponse.setCreatedDateLog(dateFormat.format(accessTime));
    List<LogAuditTrailUpdatedOfflineProductResponse> logAuditTrailUpdatedProductResponseList =
        new ArrayList<>();
    logAuditTrailUpdatedProductResponseList.add(logAuditTrailUpdatedProductResponse);

    GdnRestListResponse<LogAuditTrailUpdatedOfflineProductResponse> response =
        new GdnRestListResponse<>(null, null, true,
            logAuditTrailUpdatedProductResponseList, new PageMetaData(pageable.getPageSize(),
            pageable.getPageNumber(), logAuditTrailUpdatedProductPage.getTotalElements()),
            DEFAULT_REQUEST_ID);

    Mockito.when(updatedProductHistoryService
        .getOfflineProductHistoryByItemSkuAndPickupPointCode(DEFAULT_STORE_ID, DEFAULT_SKU,
            DEFAULT_PICKUP_POINT_CODE, pageable))
        .thenReturn(logAuditTrailUpdatedProductPage);

    URI uri =
        new URIBuilder()
            .setPath(
                UpdatedProductHistoryControllerPath.BASE_PATH + UpdatedProductHistoryControllerPath.GET_OFFLINE_PRD_UPD_AUDIT_LOGS)
            .addParameter("storeId", UpdatedProductHistoryControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", UpdatedProductHistoryControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("requestId", UpdatedProductHistoryControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", UpdatedProductHistoryControllerTest.DEFAULT_USERNAME)
            .addParameter("clientId", UpdatedProductHistoryControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("page", String.valueOf(UpdatedProductHistoryControllerTest.DEFAULT_PAGE))
            .addParameter("size", String.valueOf(UpdatedProductHistoryControllerTest.DEFAULT_SIZE))
            .addParameter("itemSku", UpdatedProductHistoryControllerTest.DEFAULT_SKU)
            .addParameter("pickupPointCode", UpdatedProductHistoryControllerTest.DEFAULT_PICKUP_POINT_CODE)
            .build();
    MvcResult result =
        this.getMockMvc()
            .perform(
                MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    assertEquals(this.getObjectMapper().writeValueAsString(response), result.getResponse()
        .getContentAsString());
    verify(this.updatedProductHistoryService, UpdatedProductHistoryControllerTest.AT_LEAST_ONE)
        .getOfflineProductHistoryByItemSkuAndPickupPointCode(DEFAULT_STORE_ID, DEFAULT_SKU,
            DEFAULT_PICKUP_POINT_CODE, pageable);
  }

  @Test
  public void deleteFromDbTest() throws Exception {
    Mockito.doNothing().when(updatedProductHistoryService).deleteFromDb(DEFAULT_STORE_ID);
    URI uri = new URIBuilder().setPath(UpdatedProductHistoryControllerPath.BASE_PATH + UpdatedProductHistoryControllerPath.DELETE_FROM_DB)
        .addParameter("storeId", UpdatedProductHistoryControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", UpdatedProductHistoryControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", UpdatedProductHistoryControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", UpdatedProductHistoryControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", UpdatedProductHistoryControllerTest.DEFAULT_CLIENT_ID).build();
    this.getMockMvc().perform(
        MockMvcRequestBuilders.delete(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
  }

  @Test
  public void deleteFromSolrTest() throws Exception {
    Mockito.doNothing().when(updatedProductHistoryService).deleteFromSolr(DEFAULT_STORE_ID);
    URI uri = new URIBuilder().setPath(UpdatedProductHistoryControllerPath.BASE_PATH + UpdatedProductHistoryControllerPath.DELETE_FROM_SOLR)
        .addParameter("storeId", UpdatedProductHistoryControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", UpdatedProductHistoryControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", UpdatedProductHistoryControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", UpdatedProductHistoryControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", UpdatedProductHistoryControllerTest.DEFAULT_CLIENT_ID).build();
    this.getMockMvc().perform(
        MockMvcRequestBuilders.delete(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
  }

  public MockMvc getMockMvc() {
    return this.mockMvc;
  }

  public ObjectMapper getObjectMapper() {
    return objectMapper;
  }

  public void setObjectMapper(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  public UpdatedProductHistoryController getUpdatedProductHistoryController() {
    return updatedProductHistoryController;
  }

  public void setUpdatedProductHistoryController(UpdatedProductHistoryController updatedProductHistoryController) {
    this.updatedProductHistoryController = updatedProductHistoryController;
  }

  public UpdatedProductHistoryService getUpdatedProductHistoryService() {
    return updatedProductHistoryService;
  }

  public void setUpdatedProductHistoryService(UpdatedProductHistoryService updatedProductHistoryService) {
    this.updatedProductHistoryService = updatedProductHistoryService;
  }

  public void setMockMvc(MockMvc mockMvc) {
    this.mockMvc = mockMvc;
  }
}
