package com.gdn.partners.pbp.controller.mv;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
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
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.IndexingRequest;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.partners.pbp.service.mv.indexing.IndexingService;
import com.gdn.partners.pbp.web.model.ProductAggregatorIndexingControllerPath;
import com.gdn.x.base.controller.GlobalControllerAdvice;

public class ProductAggregatorIndexingControllerTest {

  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String BUSINESS_PARTNER_CODE = "1234";

  @InjectMocks
  private ProductAggregatorIndexingController controller;

  @Mock
  private IndexingService partialByBusinessPartnerCodeService;

  @Mock
  private IndexingService partialByItemSkusService;

  @Mock
  private IndexingService fullIndexingService;

  private MockMvc mockMvc;

  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.controller)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter())
            .setControllerAdvice(new GlobalControllerAdvice()).build();
    objectMapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.partialByBusinessPartnerCodeService);
    Mockito.verifyNoMoreInteractions(this.partialByItemSkusService);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void partialIndexingByBusinessPartnerCodeTest() throws Exception {
    Mockito.doNothing().when(this.partialByBusinessPartnerCodeService).doIndexing(
        Mockito.any(MandatoryRequestParam.class), Mockito.any(Map.class),
        Mockito.any(Boolean.class));
    IndexingRequest partialIndexingRequest = new IndexingRequest();
    URI uri = new URIBuilder()
        .setPath(ProductAggregatorIndexingControllerPath.BASE_PATH
            + ProductAggregatorIndexingControllerPath.PARTIAL_INDEXING_BUSINESS_PARTNER_CODES_PATH)
        .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
        .addParameter("clientId", CLIENT_ID)
        .addParameter("isForce", "false")
        .build();
    this.mockMvc
        .perform(
            MockMvcRequestBuilders.post(uri).contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(partialIndexingRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.partialByBusinessPartnerCodeService).doIndexing(
        Mockito.any(MandatoryRequestParam.class), Mockito.any(), Mockito.any(Boolean.class));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void partialIndexingByItemSkusTest() throws Exception {
    Mockito.doNothing().when(this.partialByItemSkusService).doIndexing(
        Mockito.any(MandatoryRequestParam.class), Mockito.any(Map.class),
        Mockito.any(Boolean.class));
    Map<String, Object> request = new HashMap<>();
    request.put(BUSINESS_PARTNER_CODE, new ArrayList<>(Arrays.asList("SKU1", "SKU2")));
    IndexingRequest partialIndexingRequest = new IndexingRequest();
    partialIndexingRequest.setRequest(request);
    URI uri = new URIBuilder()
        .setPath(ProductAggregatorIndexingControllerPath.BASE_PATH
            + ProductAggregatorIndexingControllerPath.PARTIAL_INDEXING_ITEM_SKUS_PATH)
        .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
        .addParameter("clientId", CLIENT_ID).addParameter("isForce", "false").build();
    this.mockMvc
        .perform(MockMvcRequestBuilders.post(uri).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(partialIndexingRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.partialByItemSkusService).doIndexing(
        Mockito.any(MandatoryRequestParam.class), Mockito.any(Map.class),
        Mockito.any(Boolean.class));
  }

  @Test
  public void fullIndexingTest() throws Exception {
    Map<String, Object> request = new HashMap<>();
    URI uri = new URIBuilder()
        .setPath(ProductAggregatorIndexingControllerPath.BASE_PATH
            + ProductAggregatorIndexingControllerPath.FULL_INDEXING_PATH)
        .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
        .addParameter("clientId", CLIENT_ID).addParameter("isForce", "false").build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.fullIndexingService).doIndexing(
        Mockito.any(MandatoryRequestParam.class), Mockito.any(), Mockito.any(Boolean.class));
  }

}
