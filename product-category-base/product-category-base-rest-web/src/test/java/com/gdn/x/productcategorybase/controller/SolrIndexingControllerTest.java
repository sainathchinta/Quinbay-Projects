package com.gdn.x.productcategorybase.controller;

import java.util.Calendar;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.service.solr.SolrIndexingService;
import com.gdn.x.productcategorybase.solr.service.ProductService;
import com.gdn.x.productcategorybase.util.CalendarFactory;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

/**
 * Created by keshashah on 18/10/16.
 */
public class SolrIndexingControllerTest {

  @Mock
  private CalendarFactory calendarFactory;

  @Mock
  private ProductService productService;

  @InjectMocks
  private SolrIndexingController solrIndexingController;

  @Mock
  private SolrIndexingService solrIndexingService;

  private MockMvc mockMvc;

  private static final String BASE_PATH = "/api-solr/";
  private static final String REINDEXING = "product/reindex";
  private static final String XSCHEDULER = "XSCHEDULER";
  private static final String STORE_ID = "10001";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String CATEGORY_CODE = "IP-100001";

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    this.mockMvc = standaloneSetup(this.solrIndexingController).build();
    when(calendarFactory.getInstance()).thenReturn(Calendar.getInstance());
    Mockito.when(productService.fullIndex()).thenReturn(Boolean.TRUE);
    when(productService.deltaIndex()).thenReturn(true);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(productService, calendarFactory, solrIndexingService);
  }

  @Disabled
  @Test
  public void reindex_default_FirstRun_FullIndex() throws Exception {
    this.mockMvc
        .perform(get(BASE_PATH + REINDEXING)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("username", XSCHEDULER))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).fullIndex();
    verify(calendarFactory).getInstance();
  }

  @Test
  @Disabled
  public void reindex_default_AfterFirstRun_DeltaIndex() throws Exception {
    // 1st request to set flag full index run to true
    this.mockMvc
        .perform(get(BASE_PATH + REINDEXING)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("username", XSCHEDULER))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).fullIndex();
    this.mockMvc
        .perform(get(BASE_PATH + REINDEXING)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("username", XSCHEDULER))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).deltaIndex();
    verify(calendarFactory, times(2)).getInstance();
  }

  @Test
  public void reIndexBrandCollectionTest() throws Exception {
    doNothing().when(this.solrIndexingService).fullReindexBrandCollection(STORE_ID);
    this.mockMvc.perform(
        post(BASE_PATH + SolrIndexingController.REINDEX_BRAND_COLLECTION).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("username", XSCHEDULER).param("storeId", STORE_ID)
            .param("reIndexType", SolrIndexingController.FULL_REINDEX)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.solrIndexingService).fullReindexBrandCollection(STORE_ID);
  }

  @Test
  public void reIndexBrandCollectionByBrandRequestCodeTest() throws Exception {
    this.mockMvc.perform(get(BASE_PATH + SolrIndexingController.REINDEX_BRAND_COLLECTION_BY_BRAND_REQUEST_CODE).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("username", XSCHEDULER)
            .param("storeId", STORE_ID).param(CHANNEL_ID, Constants.CHANNEL_ID).param(USERNAME, Constants.CHANNEL_ID)
            .param(REQUEST_ID, Constants.CHANNEL_ID).param(CLIENT_ID, Constants.CHANNEL_ID)
            .param("brandRequestCode", REQUEST_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.solrIndexingService).reindexBrandCollectionByBrandRequestCode(STORE_ID, REQUEST_ID);
  }

  @Test
  public void reIndexPcbCollectionByProductCodeTest() throws Exception {
    this.mockMvc.perform(get(BASE_PATH + SolrIndexingController.REINDEX_PCB_COLLECTION_BY_PRODUCT_CODE).accept(
            MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("username", XSCHEDULER)
        .param("storeId", STORE_ID).param(CHANNEL_ID, Constants.CHANNEL_ID).param(USERNAME, Constants.CHANNEL_ID)
        .param(REQUEST_ID, Constants.CHANNEL_ID).param(CLIENT_ID, Constants.CHANNEL_ID)
        .param("productCode", CHANNEL_ID)).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.solrIndexingService).deltaReindexPCBCollectionByProductCode(STORE_ID, CHANNEL_ID);
  }

  @Test
  public void partialReindexIndexBrandCollectionTest() throws Exception {
    doNothing().when(this.solrIndexingService).partialReindexBrandCollection(STORE_ID);
    this.mockMvc.perform(
        post(BASE_PATH + SolrIndexingController.REINDEX_BRAND_COLLECTION).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("username", XSCHEDULER).param("storeId", STORE_ID)
            .param("reIndexType", SolrIndexingController.PARTIAL_REINDEX)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.solrIndexingService).partialReindexBrandCollection(STORE_ID);
  }

  @Test
  public void invalidReindexTypeTest() throws Exception {
    this.mockMvc.perform(
        post(BASE_PATH + SolrIndexingController.REINDEX_BRAND_COLLECTION).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("username", XSCHEDULER).param("storeId", STORE_ID)
            .param("reIndexType", StringUtils.EMPTY)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  // To run this test, need to change calender time to 12:00AM, run it manually for testing
  @Test
  public void reindex_skipFullIndex_true_RunDeltaIndexEvenWhenFullIndexShouldRun() throws Exception {
    // 1st request to set flag full index run to true
    Calendar calendar = Calendar.getInstance();
    calendar.set(Calendar.HOUR_OF_DAY, 0);
    calendar.set(Calendar.MINUTE, 0);
    Mockito.when(calendarFactory.getInstance()).thenReturn(calendar);
    this.mockMvc
        .perform(get(BASE_PATH + REINDEXING)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("username", XSCHEDULER))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).fullIndex();
    verify(calendarFactory).getInstance();

    this.mockMvc
        .perform(get(BASE_PATH + REINDEXING)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("username", XSCHEDULER)
            .param("skipFullIndex", "true"))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).deltaIndex();
    verify(calendarFactory, times(2)).getInstance();

  }

  @Disabled
  @Test
  public void reindex_runFullIndex_true_RunFullIndexEvenAfterItsDoneOnce() throws Exception {
    // 1st request to set flag full index run to true
    this.mockMvc
        .perform(get(BASE_PATH + REINDEXING)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("username", XSCHEDULER))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    this.mockMvc
        .perform(get(BASE_PATH + REINDEXING)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("username", XSCHEDULER)
            .param("runFullIndex", "true"))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(productService
            , times(2)).fullIndex();
    verify(calendarFactory, times(2)).getInstance();
  }

  @Test
  public void fullReIndexPcbCollectionTest() throws Exception {
    this.mockMvc.perform(
        post(BASE_PATH + SolrIndexingController.REINDEX_PCB_COLLECTION).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("username", XSCHEDULER).param("storeId", STORE_ID)
            .param("reIndexType", SolrIndexingController.FULL_REINDEX)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.solrIndexingService).fullReindexPCBCollection(STORE_ID);
  }

  @Test
  public void partialReindexIndexPcbCollectionTest() throws Exception {
    this.mockMvc.perform(
        post(BASE_PATH + SolrIndexingController.REINDEX_PCB_COLLECTION).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("username", XSCHEDULER).param("storeId", STORE_ID)
            .param("reIndexType", SolrIndexingController.PARTIAL_REINDEX)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.solrIndexingService).deltaReindexPCBCollection(STORE_ID);
  }

  @Test
  public void categoryBasedReindexPcbCollectionTest() throws Exception {
    this.mockMvc
      .perform(get(BASE_PATH + SolrIndexingController.CATEGORY_BASED_REINDEX, CATEGORY_CODE)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID))
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.solrIndexingService).categoryBasedReindexPCBCollection(STORE_ID, CATEGORY_CODE);
  }
}
