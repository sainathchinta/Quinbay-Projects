package com.gdn.x.mta.distributiontask.controller;

import com.gdn.x.mta.distributiontask.rest.model.SolrIndexControllerPath;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.web.servlet.MockMvc;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

public class SolrIndexControllerTest {

  private static final String STORE_ID = "storeId";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANNEL_ID = "channelId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String PRODUCT_CODE = "productcode";

  private MockMvc mockMvc;

  @Mock
  private SolrVendorCollectionService solrVendorCollectionService;

  @InjectMocks
  private SolrIndexController solrIndexController;

  @BeforeEach
  public void init() {
    initMocks(this);
    this.mockMvc = standaloneSetup(this.solrIndexController).build();
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(solrVendorCollectionService);
  }

  @Test
   void fullReindexTest() throws Exception {
    Mockito.doNothing().when(this.solrVendorCollectionService).fullReindexPDTProductSolr(STORE_ID);
    this.mockMvc.perform(get(
        SolrIndexControllerPath.BASE_PATH + SolrIndexControllerPath.FULL_REINDEX_PDT_SOLR)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.solrVendorCollectionService).fullReindexPDTProductSolr(STORE_ID);
  }

  @Test
   void deltaReindexTest() throws Exception {
    Mockito.doNothing().when(this.solrVendorCollectionService).deltaReindexPDTProductSolr(STORE_ID, PRODUCT_CODE);
    this.mockMvc.perform(get(
        SolrIndexControllerPath.BASE_PATH + SolrIndexControllerPath.DELTA_REINDEX_PDT_SOLR)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("productCode", PRODUCT_CODE)
        .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.solrVendorCollectionService).deltaReindexPDTProductSolr(STORE_ID, PRODUCT_CODE);
  }
}
