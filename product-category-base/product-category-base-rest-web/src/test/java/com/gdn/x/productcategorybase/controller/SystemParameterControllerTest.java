package com.gdn.x.productcategorybase.controller;


import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URI;
import java.util.UUID;

import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.common.util.BeanUtils;
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
import com.gdn.x.productcategorybase.SystemParameterControllerPath;
import com.gdn.x.productcategorybase.dto.request.SystemParameterRequest;
import com.gdn.x.productcategorybase.dto.response.SystemParameterResponse;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.service.SystemParameterService;

public class SystemParameterControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel-1";
  private static final String DEFAULT_CLIENT_ID = "client-1";
  private static final String DEFAULT_USERNAME = "com.gdn.mta.developer";
  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String DESCRIPTION = "desc";
  private static final String VARIABLE = "variable";
  private static final String VALUE = "value";
  private static final String ROOT = "/";
  private SystemParameterRequest systemParameterRequest;
  private SystemParameter systemParameter;
  private SystemParameterResponse systemParameterResponse;

  private String requestId;
  private MockMvc mockMvc;
  private ObjectMapper objectMapper;

  @InjectMocks
  private SystemParameterController systemParameterController;

  @Mock
  private SystemParameterService systemParameterService;


  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    this.setMockMvc(MockMvcBuilders.standaloneSetup(this.systemParameterController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build());
    this.setObjectMapper(new ObjectMapper(new JsonFactory()));
    this.requestId = UUID.randomUUID().toString();
    systemParameterRequest = new SystemParameterRequest();
    systemParameterRequest.setDescription(DESCRIPTION);
    systemParameterRequest.setValue(VALUE);
    systemParameterRequest.setVariable(VARIABLE);

    systemParameter = new SystemParameter();
    BeanUtils.copyProperties(systemParameterRequest, systemParameter);

    systemParameterResponse = new SystemParameterResponse();
    BeanUtils.copyProperties(systemParameterRequest, systemParameterResponse);
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
    Mockito.verifyNoMoreInteractions(systemParameterService);
  }

  @Test
  public void insertSystemParameterTest() throws Exception {
    Mockito.doNothing().when(this.systemParameterService).insert(systemParameter);
    URI uri = new URIBuilder()
        .setPath(SystemParameterControllerPath.BASE_PATH + SystemParameterControllerPath.SYSTEM_PARAMETER_INSERT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(systemParameterRequest)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.systemParameterService).insert(Mockito.any(SystemParameter.class));
  }

  @Test
  public void updateSystemParameterTest() throws Exception {
    Mockito.doNothing().when(this.systemParameterService).update(systemParameter);
    URI uri = new URIBuilder()
        .setPath(SystemParameterControllerPath.BASE_PATH + SystemParameterControllerPath.SYSTEM_PARAMETER_UPDATE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(systemParameterRequest)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(systemParameterService).update(Mockito.any(SystemParameter.class));
  }

  @Test
  public void deleteSystemParameterTest() throws Exception {
    Mockito.doNothing().when(this.systemParameterService).delete(DEFAULT_STORE_ID, VARIABLE);
    URI uri = new URIBuilder().setPath(SystemParameterControllerPath.BASE_PATH + ROOT + VARIABLE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.delete(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(systemParameterService).delete(DEFAULT_STORE_ID, VARIABLE);
  }

  @Test
  public void findSystemParameterTest() throws Exception {
    Mockito.when(systemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE))
        .thenReturn(systemParameter);
    URI uri = new URIBuilder()
        .setPath(SystemParameterControllerPath.BASE_PATH + SystemParameterControllerPath.SYSTEM_PARAMETER_FIND)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("variable", VARIABLE).build();
    MvcResult response = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(systemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE);
    assertTrue(response.getResponse().getContentAsString()
        .contains(getObjectMapper().writeValueAsString(systemParameterResponse)));
  }
}
