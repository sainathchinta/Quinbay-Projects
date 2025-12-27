package com.gdn.mta.product.controller;

import java.net.URI;
import java.util.Arrays;
import java.util.UUID;

import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.web.model.ProductControllerErrorMessage;
import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
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
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.enums.IndexTypes;
import com.gdn.mta.product.service.solr.SolrActiveProductCollectionService;
import com.gdn.mta.product.service.solr.SolrIndexingService;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.mta.product.web.model.SolrReindexControllerPath;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;

public class SolrReindexControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel-1";
  private static final String DEFAULT_CLIENT_ID = "client-1";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String DEFAULT_PRODUCT_CODE = "product-code";
  private static final String DEFAULT_PRODUCT_SKU = "product-sku";
  private static final String DEFAULT_SOURCE_COLLECTION = "sourceCollection";
  private static final String DEFAULT_DESTINATION_COLLECTION = "destinationCollection";
  private static final String START_TIME = "01-01-2021";
  private static final String END_TIME = "31-01-2021";



  private String requestId;
  private MockMvc mockMvc;

  @Mock
  private SolrActiveProductCollectionService solrActiveProductCollectionService;

  @Mock
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @InjectMocks
  private SolrReindexController solrReindexController;

  @Mock
  private SolrIndexingService solrIndexingService;

  @Mock
  private ProductService productService;

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    this.mockMvc =
        MockMvcBuilders
            .standaloneSetup(this.solrReindexController)
            .setMessageConverters(new ByteArrayHttpMessageConverter(),
                new StringHttpMessageConverter(), new ResourceHttpMessageConverter(),
                new FormHttpMessageConverter(), new MappingJackson2HttpMessageConverter()).build();
    this.requestId = UUID.randomUUID().toString();
  }

  @Test
  public void testDeleteFromSolrProductCollection() throws Exception {
    URI uri = new URIBuilder()
        .setPath(
            SolrReindexControllerPath.BASE_PATH + SolrReindexControllerPath.PRD_COLLECTION_DELETE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", this.requestId).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("productCode", DEFAULT_PRODUCT_CODE).build();
    this.mockMvc
        .perform(MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
  }

  @Test
  public void testSyncInactiveProductCollection() throws Exception {
    URI uri = new URIBuilder().setPath(SolrReindexControllerPath.BASE_PATH
        + SolrReindexControllerPath.PRD_COLLECTION_SYNC_INACTIVE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", this.requestId).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc
        .perform(MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
  }

  @Test
  public void copySameCollectionTest() throws Exception {
    Mockito.doNothing().when(solrIndexingService).updateAll(DEFAULT_SOURCE_COLLECTION ,DEFAULT_DESTINATION_COLLECTION);
    URI uri = new URIBuilder()
        .setPath(SolrReindexControllerPath.BASE_PATH + SolrReindexControllerPath.MOVE_EXISTING_SOLR_COLLECTION_TO_CLOUD)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", this.requestId).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("sourceCollection", DEFAULT_SOURCE_COLLECTION)
        .addParameter("destinationCollection", DEFAULT_DESTINATION_COLLECTION).build();
    this.mockMvc.perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(solrIndexingService).updateAll(DEFAULT_SOURCE_COLLECTION, DEFAULT_DESTINATION_COLLECTION);
  }

  @Test
  public void copySameCollectionExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(solrIndexingService).updateAll(DEFAULT_SOURCE_COLLECTION ,DEFAULT_DESTINATION_COLLECTION);
    URI uri = new URIBuilder()
        .setPath(SolrReindexControllerPath.BASE_PATH + SolrReindexControllerPath.MOVE_EXISTING_SOLR_COLLECTION_TO_CLOUD)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", this.requestId).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("sourceCollection", DEFAULT_SOURCE_COLLECTION)
        .addParameter("destinationCollection", DEFAULT_DESTINATION_COLLECTION).build();
    MvcResult result = this.mockMvc.perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(solrIndexingService).updateAll(DEFAULT_SOURCE_COLLECTION, DEFAULT_DESTINATION_COLLECTION);
  }

  @Test
  public void fullReindexReviewProductsTest() throws Exception {
    Mockito.doNothing().when(solrReviewProductCollectionService).fullReindexCollection(DEFAULT_STORE_ID, false);
    URI uri = new URIBuilder()
        .setPath(SolrReindexControllerPath.BASE_PATH + SolrReindexControllerPath.REINDEX_IN_REVIEW_PRODUCTS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", this.requestId).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("indexType", IndexTypes.FULL_REINDEX.getIndexType())
        .addParameter("isScreeningReindex", String.valueOf(Boolean.FALSE)).build();
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(solrReviewProductCollectionService).fullReindexCollection(DEFAULT_STORE_ID, false);
  }

  @Test
  public void partialReindexReviewProductsTest() throws Exception {
    Mockito.doNothing().when(solrReviewProductCollectionService).deltaReindexCollection(DEFAULT_STORE_ID);
    URI uri = new URIBuilder()
        .setPath(SolrReindexControllerPath.BASE_PATH + SolrReindexControllerPath.REINDEX_IN_REVIEW_PRODUCTS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", this.requestId).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("indexType", IndexTypes.DELTA_REINDEX.getIndexType())
        .addParameter("isScreeningReindex", String.valueOf(Boolean.FALSE)).build();
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(solrReviewProductCollectionService).deltaReindexCollection(DEFAULT_STORE_ID);
  }

  @Test
  public void reindexReviewProductByProductCodeTest() throws Exception {
    Mockito.doNothing().when(solrReviewProductCollectionService)
        .deltaReindexProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    URI uri = new URIBuilder().setPath(
        SolrReindexControllerPath.BASE_PATH + SolrReindexControllerPath.REINDEX_IN_REVIEW_PRODUCT_BY_PRODUCT_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("requestId", this.requestId).addParameter("productCode", DEFAULT_PRODUCT_CODE).build();
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(solrReviewProductCollectionService).deltaReindexProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
  }

  @Test
  public void reindexReviewProductByProductCodeEmptyTest() throws Exception {
    Mockito.doNothing().when(solrReviewProductCollectionService).deltaReindexProductCode(DEFAULT_STORE_ID, "");
    URI uri = new URIBuilder().setPath(
        SolrReindexControllerPath.BASE_PATH + SolrReindexControllerPath.REINDEX_IN_REVIEW_PRODUCT_BY_PRODUCT_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("requestId", this.requestId).addParameter("productCode", "").build();
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
      Mockito.verify(solrReviewProductCollectionService).deltaReindexProductCode(DEFAULT_STORE_ID, "");
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK));
      Mockito.verify(this.solrReviewProductCollectionService, Mockito.times(0))
          .deltaReindexProductCode(DEFAULT_STORE_ID, "");

    }
  }

  @Test
  public void reindexActiveProductByProductCodeTest() throws Exception {
    Mockito.doNothing().when(productService)
        .reindexActiveProductCollectionByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    URI uri = new URIBuilder().setPath(
            SolrReindexControllerPath.BASE_PATH + SolrReindexControllerPath.REINDEX_ACTIVE_PRODUCT_BY_PRODUCT_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("requestId", this.requestId).addParameter("productCode", DEFAULT_PRODUCT_CODE).build();
    this.mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productService)
        .reindexActiveProductCollectionByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
  }

  @Test
  public void reindexActiveProductByProductCodeEmptyTest() throws Exception {
    Mockito.doNothing().when(productService)
        .reindexActiveProductCollectionByStoreIdAndProductCode(DEFAULT_STORE_ID, "");
    URI uri = new URIBuilder().setPath(
            SolrReindexControllerPath.BASE_PATH + SolrReindexControllerPath.REINDEX_ACTIVE_PRODUCT_BY_PRODUCT_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("requestId", this.requestId).addParameter("productCode", "").build();
    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
      Mockito.verify(productService).reindexActiveProductCollectionByStoreIdAndProductCode(DEFAULT_STORE_ID, "");
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void reindexVariantHistoryTest() throws Exception {
    String request =
        new ObjectMapper().writeValueAsString(new SimpleListStringRequest(Arrays.asList(DEFAULT_PRODUCT_SKU)));
    Mockito.doNothing().when(solrIndexingService)
        .reindexProductHistoryByProductSkus(DEFAULT_STORE_ID, Arrays.asList(DEFAULT_PRODUCT_SKU));
    URI uri = new URIBuilder()
        .setPath(SolrReindexControllerPath.BASE_PATH + SolrReindexControllerPath.REINDEX_VARIANT_HISTORY)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", this.requestId).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request)).andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(solrIndexingService)
        .reindexProductHistoryByProductSkus(DEFAULT_STORE_ID, Arrays.asList(DEFAULT_PRODUCT_SKU));
  }

  @Test
  public void deltaReindexVariantHistoryTest() throws Exception {
    Mockito.doNothing().when(solrIndexingService).deltaReindexHistoryCollection(DEFAULT_STORE_ID, START_TIME, END_TIME);
    URI uri = new URIBuilder()
        .setPath(SolrReindexControllerPath.BASE_PATH + SolrReindexControllerPath.DELTA_REINDEX_VARIANT_HISTORY)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", this.requestId).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("startTime", START_TIME)
        .addParameter("endTime", END_TIME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(solrIndexingService).deltaReindexHistoryCollection(DEFAULT_STORE_ID, START_TIME, END_TIME);
  }

  @Test
  public void deltaReindexForPrdProductCollection() throws Exception {
    Mockito.doNothing().when(solrIndexingService).deltaReindexPrdProductCollection(DEFAULT_STORE_ID, START_TIME, END_TIME);
    URI uri = new URIBuilder()
        .setPath(SolrReindexControllerPath.BASE_PATH + SolrReindexControllerPath.PRD_COLLECTION_DELTA_REINDEX)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", this.requestId).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("indexFrom", START_TIME)
        .addParameter("indexTill", END_TIME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(solrIndexingService).deltaReindexPrdProductCollection(DEFAULT_STORE_ID, START_TIME, END_TIME);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productService);
  }
}
