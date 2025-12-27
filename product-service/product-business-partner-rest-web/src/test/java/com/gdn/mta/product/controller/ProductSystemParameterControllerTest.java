package com.gdn.mta.product.controller;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;

import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.mta.product.util.BeanUtils;
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

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductSystemParameterRequest;
import com.gda.mta.product.dto.ProductSystemParameterResponse;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.web.model.ProductSystemParameterControllerPath;

public class ProductSystemParameterControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel-1";
  private static final String DEFAULT_CLIENT_ID = "client-1";
  private static final String DEFAULT_USERNAME = "com.gdn.mta.developer";
  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String DESCRIPTION = "desc";
  private static final String VARIABLE = "variable";
  private static final String VALUE = "value";
  private static final String ROOT = "/";
  private ProductSystemParameterRequest productSystemParameterRequest;
  private ProductSystemParameter productSystemParameter;
  private ProductSystemParameterResponse productSystemParameterResponse;

  private String requestId;
  private MockMvc mockMvc;
  private ObjectMapper objectMapper;

  @InjectMocks
  private ProductSystemParameterController productSystemParameterController;

  @Mock
  private ProductSystemParameterService productSystemParameterService;


  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    this.setMockMvc(MockMvcBuilders.standaloneSetup(this.productSystemParameterController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build());
    this.setObjectMapper(new ObjectMapper(new JsonFactory()));
    this.requestId = UUID.randomUUID().toString();
    productSystemParameterRequest = new ProductSystemParameterRequest();
    productSystemParameterRequest.setDescription(DESCRIPTION);
    productSystemParameterRequest.setValue(VALUE);
    productSystemParameterRequest.setVariable(VARIABLE);

    productSystemParameter = new ProductSystemParameter();
    BeanUtils.copyProperties(productSystemParameterRequest, productSystemParameter);

    productSystemParameterResponse = new ProductSystemParameterResponse();
    BeanUtils.copyProperties(productSystemParameterRequest, productSystemParameterResponse);
  }

  public void setMockMvc(MockMvc mockMvc) {
    this.mockMvc = mockMvc;
  }

  public MockMvc getMockMvc() {
    return this.mockMvc;
  }


  public void setObjectMapper(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  public ObjectMapper getObjectMapper() {
    return objectMapper;
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productSystemParameterService);
  }

  @Test
  public void insertSystemParameterTest() throws Exception {
    URI uri = new URIBuilder().setPath(
        ProductSystemParameterControllerPath.BASE_PATH + ProductSystemParameterControllerPath.SYSTEM_PARAMETER_INSERT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(productSystemParameterRequest))
        .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productSystemParameterService).insert(productSystemParameter);
  }

  @Test
  public void updateSystemParameterTest() throws Exception {
    URI uri = new URIBuilder().setPath(
        ProductSystemParameterControllerPath.BASE_PATH + ProductSystemParameterControllerPath.SYSTEM_PARAMETER_UPDATE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(productSystemParameterRequest))
        .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productSystemParameterService).update(productSystemParameter);
  }

  @Test
  public void deleteSystemParameterTest() throws Exception {
    URI uri = new URIBuilder().setPath(
        ProductSystemParameterControllerPath.BASE_PATH + ROOT + VARIABLE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.delete(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productSystemParameterService).delete(DEFAULT_STORE_ID, VARIABLE);
  }

  @Test
  public void findSystemParameterTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE))
        .thenReturn(productSystemParameter);
    URI uri = new URIBuilder().setPath(
        ProductSystemParameterControllerPath.BASE_PATH + ProductSystemParameterControllerPath.SYSTEM_PARAMETER_FIND)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("variable", VARIABLE).build();
    MvcResult response = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE);
    assertTrue(response.getResponse().getContentAsString()
        .contains(getObjectMapper().writeValueAsString(productSystemParameterResponse)));
  }

  @Test
  public void findSystemParameterTestShowOnUITrue() throws Exception {
    productSystemParameter.setShowOnUI(true);
    productSystemParameterResponse.setShowOnUI(true);
    List<ProductSystemParameter> productSystemParameterList = new ArrayList<>();
    productSystemParameterList.add(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndShowOnUITrue(DEFAULT_STORE_ID))
        .thenReturn(productSystemParameterList);
    URI uri = new URIBuilder().setPath(
            ProductSystemParameterControllerPath.BASE_PATH + ProductSystemParameterControllerPath.SYSTEM_PARAMETER_FETCH_WITH_SHOW_ON_UI)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult response = getMockMvc().perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productSystemParameterService).findByStoreIdAndShowOnUITrue(DEFAULT_STORE_ID);
  }

  @Test
  public void findSystemParameterSwitchTest() throws Exception {
    Mockito.when(productSystemParameterService.findSwitchValues(DEFAULT_STORE_ID))
        .thenReturn(new HashMap<>());
    URI uri = new URIBuilder().setPath(
        ProductSystemParameterControllerPath.BASE_PATH + ProductSystemParameterControllerPath.SYSTEM_PARAMETER_SWITCH_FETCH)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult response = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(productSystemParameterService).findSwitchValuesWithCanaryAndNonCanary(DEFAULT_STORE_ID);
  }
}