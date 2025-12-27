package com.gdn.x.product.rest.web.controller.api;

import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutorService;

import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.service.api.ReindexService;

/**
 * This unit test class template is the work of maria.o.a.sunaryo and felix.w.wijaya as
 * participation in Blibli Hackathon #1. Generated on 23 Jun 2016 14:03:35
 */
public class ReindexControllerTest {

  private static final String SKU_1 = "sku-1";
  private static final List<String> SKU_LIST = Arrays.asList(ReindexControllerTest.SKU_1);
  @InjectMocks
  private ReindexController sut;
  @Mock
  private ExecutorService executorService;
  @Mock
  private ReindexService reindexService;
  private MockMvc mockMvc;
  private ObjectMapper mapper = new ObjectMapper();
  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String PAGE = "0";
  private static final String SIZE = "10";
  private static final String INDEX_FROM = "26/12/2012";
  private static final String INDEX_TILL = "26/12/2012";

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel-1";
  private static final String DEFAULT_CLIENT_ID = "client-1";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String DEFAULT_PRODUCT_CODE = "product-code";
  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String DEFAULT_SOURCE_COLLECTION = "sourceCollection";
  private static final String DEFAULT_DESTINATION_COLLECTION = "destinationCollection";
  private static final String TIMESTAMP_FROM = "100000000";
  private List<String> categoryCodes = new ArrayList<>();

  @Test
  public void reindexSolrAndClearCacheByProductSkusTest() throws Exception {
    SimpleListStringRequest bodyObject = new SimpleListStringRequest(ReindexControllerTest.SKU_LIST);
    String bodyJson = new ObjectMapper().writeValueAsString(bodyObject);
    this.mockMvc.perform(post(ProductApiPath.REINDEX + ProductApiPath.REINDEX_AND_CLEARCACHE_BY_PRODUCT_SKUS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ReindexControllerTest.STORE_ID).param("channelId", ReindexControllerTest.CHANNEL_ID)
        .param("clientId", ReindexControllerTest.CLIENT_ID).param("requestId", ReindexControllerTest.REQUEST_ID)
        .param("username", ReindexControllerTest.USERNAME).content(bodyJson)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true));
    verify(this.reindexService)
        .reindexSolrAndClearCacheByProductSkus(ReindexControllerTest.REQUEST_ID, ReindexControllerTest.USERNAME,
            ReindexControllerTest.STORE_ID, ReindexControllerTest.SKU_LIST);
  }

  @Test
  public void reindexSolrFullTest() throws Exception {
    this.mockMvc.perform(post(ProductApiPath.REINDEX + ProductApiPath.REINDEX_FULL).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ReindexControllerTest.STORE_ID)
        .param("channelId", ReindexControllerTest.CHANNEL_ID).param("clientId", ReindexControllerTest.CLIENT_ID)
        .param("requestId", ReindexControllerTest.REQUEST_ID).param("username", ReindexControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    verify(this.reindexService)
        .reindexFull(ReindexControllerTest.REQUEST_ID, ReindexControllerTest.USERNAME, ReindexControllerTest.STORE_ID);
  }

  @Test
  public void deltaReindexTest() throws Exception {
    this.mockMvc.perform(post(ProductApiPath.REINDEX + ProductApiPath.DELTA_REINDEX).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ReindexControllerTest.STORE_ID)
        .param("channelId", ReindexControllerTest.CHANNEL_ID).param("clientId", ReindexControllerTest.CLIENT_ID)
        .param("requestId", ReindexControllerTest.REQUEST_ID).param("username", ReindexControllerTest.USERNAME)
        .param("indexFrom", INDEX_FROM).param("indexTill", INDEX_TILL)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true));
    verify(this.reindexService)
        .deltaReindex(ReindexControllerTest.STORE_ID, ReindexControllerTest.REQUEST_ID, ReindexControllerTest.USERNAME,
            INDEX_FROM, INDEX_TILL);
  }

  @Test
  public void deltaReindexItemSkusTest() throws Exception {
    this.mockMvc.perform(
        post(ProductApiPath.REINDEX + ProductApiPath.DELTA_REINDEX_ITEM_SKUS).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", ReindexControllerTest.STORE_ID)
            .param("channelId", ReindexControllerTest.CHANNEL_ID).param("clientId", ReindexControllerTest.CLIENT_ID)
            .param("requestId", ReindexControllerTest.REQUEST_ID).param("username", ReindexControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    verify(this.reindexService).deltaReindexItemSkus(ReindexControllerTest.STORE_ID);
  }

  @Test
  public void deltaReindexItemSkusExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.reindexService).deltaReindexItemSkus(STORE_ID);
    this.mockMvc.perform(
        post(ProductApiPath.REINDEX + ProductApiPath.DELTA_REINDEX_ITEM_SKUS).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", ReindexControllerTest.STORE_ID)
            .param("channelId", ReindexControllerTest.CHANNEL_ID).param("clientId", ReindexControllerTest.CLIENT_ID)
            .param("requestId", ReindexControllerTest.REQUEST_ID).param("username", ReindexControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(false));
    verify(this.reindexService).deltaReindexItemSkus(ReindexControllerTest.STORE_ID);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.sut).build();
    implementAsDirectExecutor(executorService);
  }

  protected void implementAsDirectExecutor(ExecutorService executor) {
    doAnswer((Answer<Object>) invocation -> {
      ((Runnable) invocation.getArguments()[0]).run();
      return null;
    }).when(executor).execute(Mockito.any(Runnable.class));
  }

  @Test
  public void copySameCollectionTest() throws Exception {
    Mockito.doNothing().when(reindexService).updateAll(DEFAULT_SOURCE_COLLECTION, DEFAULT_DESTINATION_COLLECTION);
    URI uri = new URIBuilder().setPath(ProductApiPath.REINDEX + ProductApiPath.MOVE_EXISTING_SOLR_COLLECTION_TO_CLOUD)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("sourceCollection", DEFAULT_SOURCE_COLLECTION)
        .addParameter("destinationCollection", DEFAULT_DESTINATION_COLLECTION).build();
    this.mockMvc.perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(reindexService).updateAll(DEFAULT_SOURCE_COLLECTION, DEFAULT_DESTINATION_COLLECTION);
  }

  @Test
  public void copySameCollectionExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(reindexService)
        .updateAll(DEFAULT_SOURCE_COLLECTION, DEFAULT_DESTINATION_COLLECTION);
    URI uri = new URIBuilder().setPath(ProductApiPath.REINDEX + ProductApiPath.MOVE_EXISTING_SOLR_COLLECTION_TO_CLOUD)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("sourceCollection", DEFAULT_SOURCE_COLLECTION)
        .addParameter("destinationCollection", DEFAULT_DESTINATION_COLLECTION).build();
    MvcResult result = this.mockMvc.perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(reindexService).updateAll(DEFAULT_SOURCE_COLLECTION, DEFAULT_DESTINATION_COLLECTION);
  }

  @Test
  public void copyToL3CollectionTest() throws Exception {
    String jsonRequest = mapper.writeValueAsString(categoryCodes);
    Mockito.doNothing().when(reindexService)
        .copyProductsToL3Collection(DEFAULT_SOURCE_COLLECTION, DEFAULT_DESTINATION_COLLECTION, categoryCodes,
            SolrConstants.ASC, DEFAULT_STORE_ID);
    URI uri = new URIBuilder().setPath(ProductApiPath.REINDEX + ProductApiPath.REINDEX_PRODUCTS_TO_L3_COLLECTION)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("sourceCollection", DEFAULT_SOURCE_COLLECTION)
        .addParameter("destinationCollection", DEFAULT_DESTINATION_COLLECTION)
        .addParameter("sortOrder", SolrConstants.ASC)
        .build();
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(jsonRequest).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(reindexService).copyProductsToL3Collection(DEFAULT_SOURCE_COLLECTION, DEFAULT_DESTINATION_COLLECTION, categoryCodes,
        SolrConstants.ASC, DEFAULT_STORE_ID);
  }

  @Test
  public void copyToL3CollectionExceptionTest() throws Exception {
    String jsonRequest = mapper.writeValueAsString(categoryCodes);
    Mockito.doThrow(RuntimeException.class).when(reindexService)
        .copyProductsToL3Collection(DEFAULT_SOURCE_COLLECTION, DEFAULT_DESTINATION_COLLECTION, categoryCodes,
            SolrConstants.ASC, DEFAULT_STORE_ID);
    URI uri = new URIBuilder().setPath(ProductApiPath.REINDEX + ProductApiPath.REINDEX_PRODUCTS_TO_L3_COLLECTION)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("sourceCollection", DEFAULT_SOURCE_COLLECTION)
        .addParameter("destinationCollection", DEFAULT_DESTINATION_COLLECTION)
        .addParameter("sortOrder", SolrConstants.ASC).build();
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(jsonRequest).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(reindexService)
        .copyProductsToL3Collection(DEFAULT_SOURCE_COLLECTION, DEFAULT_DESTINATION_COLLECTION, categoryCodes,
            SolrConstants.ASC, DEFAULT_STORE_ID);
  }

  @Test
  public void reindexDeferredItemsTest() throws Exception {
    URI uri = new URIBuilder().setPath(ProductApiPath.REINDEX + ProductApiPath.REINDEX_DEFERRED_ITEMS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result = this.mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(reindexService, timeout(100)).reindexDeferredItemsFirstPage(DEFAULT_STORE_ID);
  }

  @Test
  public void reindexDeferredItemsExceptionTest() throws Exception {
    URI uri = new URIBuilder().setPath(ProductApiPath.REINDEX + ProductApiPath.REINDEX_DEFERRED_ITEMS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    Mockito.doThrow(Exception.class).when(reindexService).reindexDeferredItemsFirstPage(DEFAULT_STORE_ID);
    MvcResult result = this.mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success").value(false)).andReturn();
    Mockito.verify(reindexService).reindexDeferredItemsFirstPage(DEFAULT_STORE_ID);
  }

  @Test
  public void reindexDeferredItemsByTypeTest() throws Exception {
    Mockito.doNothing().when(reindexService)
        .reindexDeferredItemsByReindexType(DEFAULT_STORE_ID, ReindexType.ITEM_REINDEX.getDescription(), "updatedDate",
            ProductReindexStatus.REINDEX_PENDING.name());
    this.mockMvc.perform(
            MockMvcRequestBuilders.post(ProductApiPath.REINDEX + ProductApiPath.REINDEX_DEFERRED_ITEMS_BY_TYPE,
                    ReindexType.ITEM_REINDEX.getDescription()).param("storeId", DEFAULT_STORE_ID)
                .param("channelId", DEFAULT_CHANNEL_ID).param("clientId", DEFAULT_REQUEST_ID)
                .param("requestId", DEFAULT_REQUEST_ID).param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true)).andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(reindexService)
        .reindexDeferredItemsByReindexType(DEFAULT_STORE_ID, ReindexType.ITEM_REINDEX.getDescription(), "updatedDate",
            ProductReindexStatus.REINDEX_PENDING.name());
  }

  @Test
  public void reindexDeferredItemsByTypeExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(reindexService)
        .reindexDeferredItemsByReindexType(DEFAULT_STORE_ID, ReindexType.ITEM_REINDEX.getDescription(), "updatedDate",
            ProductReindexStatus.REINDEX_PENDING.name());
    this.mockMvc.perform(
            MockMvcRequestBuilders.post(ProductApiPath.REINDEX + ProductApiPath.REINDEX_DEFERRED_ITEMS_BY_TYPE,
                    ReindexType.ITEM_REINDEX.getDescription()).param("storeId", DEFAULT_STORE_ID)
                .param("channelId", DEFAULT_CHANNEL_ID).param("clientId", DEFAULT_REQUEST_ID)
                .param("requestId", DEFAULT_REQUEST_ID).param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false)).andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(reindexService)
        .reindexDeferredItemsByReindexType(DEFAULT_STORE_ID, ReindexType.ITEM_REINDEX.getDescription(), "updatedDate",
            ProductReindexStatus.REINDEX_PENDING.name());
  }

  @Test
  public void deltaReindexToNewCollectionTest() throws Exception {
    this.mockMvc.perform(post(ProductApiPath.REINDEX + ProductApiPath.DELTA_REINDEX_L3_COLLECTION).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ReindexControllerTest.STORE_ID)
        .param("channelId", ReindexControllerTest.CHANNEL_ID).param("clientId", ReindexControllerTest.CLIENT_ID)
        .param("requestId", ReindexControllerTest.REQUEST_ID).param("username", ReindexControllerTest.USERNAME)
        .param("indexFrom", INDEX_FROM).param("indexTill", INDEX_TILL)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true));
    verify(this.reindexService)
        .deltaReindexToL3Collection(ReindexControllerTest.STORE_ID, INDEX_FROM, INDEX_TILL);
  }

  @Test
  public void deltaReindexItemSkuByPristineData() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    this.mockMvc.perform(post(ProductApiPath.REINDEX + ProductApiPath.DELTA_REINDEX_ITEM_SKUS_BY_PRISTINE_DATA)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ReindexControllerTest.STORE_ID).param("channelId", ReindexControllerTest.CHANNEL_ID)
        .param("clientId", ReindexControllerTest.CLIENT_ID).param("requestId", ReindexControllerTest.REQUEST_ID)
        .param("username", ReindexControllerTest.USERNAME).param("page", PAGE).param("size", SIZE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    verify(this.reindexService).deltaReindexItemSkuByPristine(ReindexControllerTest.STORE_ID, pageable);
  }

  @Test
  public void deltaReindexItemSkuByPristineDataException() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    doThrow(Exception.class).when(reindexService)
        .deltaReindexItemSkuByPristine(ReindexControllerTest.STORE_ID, pageable);
    this.mockMvc.perform(post(ProductApiPath.REINDEX + ProductApiPath.DELTA_REINDEX_ITEM_SKUS_BY_PRISTINE_DATA)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ReindexControllerTest.STORE_ID).param("channelId", ReindexControllerTest.CHANNEL_ID)
        .param("clientId", ReindexControllerTest.CLIENT_ID).param("requestId", ReindexControllerTest.REQUEST_ID)
        .param("username", ReindexControllerTest.USERNAME).param("page", PAGE).param("size", SIZE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(false));
    verify(this.reindexService).deltaReindexItemSkuByPristine(ReindexControllerTest.STORE_ID, pageable);
  }

  @Test
  public void populateNewFieldsL3SolrTest() throws Exception {
    this.mockMvc.perform(get(ProductApiPath.REINDEX + ProductApiPath.POPULATE_NEW_FIELDS_L3_SOLR)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ReindexControllerTest.STORE_ID)
        .param("channelId", ReindexControllerTest.CHANNEL_ID)
        .param("clientId", ReindexControllerTest.CLIENT_ID)
        .param("requestId", ReindexControllerTest.REQUEST_ID)
        .param("username", ReindexControllerTest.USERNAME).param("maxReindexCount", SIZE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
  }

  @AfterEach
  public void tearDown() {
  }
}
