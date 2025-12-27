package com.gdn.mta.product.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

import java.net.URI;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;

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

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AutoQcConfigRequest;
import com.gda.mta.product.dto.response.AutoApprovalRuleDetailsDto;
import com.gdn.mta.product.entity.AutoApprovalRules;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.service.AutoApprovalService;
import com.gdn.mta.product.service.AutoQcConfigService;
import com.gdn.mta.product.service.ProductImagePredictionService;
import com.gdn.mta.product.service.exception.ApiInvalidImageQcConfigException;
import com.gdn.mta.product.web.model.AutoQcConfigControllerPath;
import com.gdn.partners.pbp.commons.constants.Constants;

public class AutoQcConfigControllerTest {
  private static final Integer SEQUENCE_NUMBER = 0;
  private static final String RULE_NAME = "rule_name";
  private static final String RULE_CONFIG =
      "[{\"keyName\":\"is_edited_by_internal_user\",\"value\":\"@gdn-commerce.com\",\"valueType\":\"string\",\"operator\":\"contains\"}]";
  private static final String IMAGE_QC_CONFIG =
      "[{\"keyName\":\"blur\",\"value\":\"101\",\"valueType\":\"int\",\"operator\":\"<=\"},{\"keyName\":\"watermark\",\"value\":\"101\",\"valueType\":\"int\",\"operator\":\"<=\"},{\"keyName\":\"text\",\"value\":\"0\",\"valueType\":\"int\",\"operator\":\"==\"},{\"keyName\":\"Adult\",\"value\":\"50\",\"valueType\":\"int\",\"operator\":\"<=\"}]";

  private static final String DEFAULT_CREATED_BY = "test_user_create";
  private static final String DEFAULT_UPDATED_BY = "test_user_update";
  private static final Long DEFAULT_VERSION = 53535L;

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel-1";
  private static final String DEFAULT_CLIENT_ID = "client-1";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "developer";
  private static final String DEFAULT_RULE_NAME = "OFFICIAL_SELLERS";
  private static final String SUCCESS_FALSE = "\"success\":false";

  @Mock
  private AutoQcConfigService autoQcConfigService;

  @Mock
  private ProductImagePredictionService productImagePredictionService;
  @InjectMocks
  private AutoQcConfigController autoQcConfigController;
  private MockMvc mockMvc;
  private ObjectMapper objectMapper;
  private AutoQcConfigRequest autoQcConfigRequest;

  @Mock
  private AutoApprovalService autoApprovalService;

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

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.setMockMvc(MockMvcBuilders.standaloneSetup(this.autoQcConfigController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build());
    this.setObjectMapper(new ObjectMapper(new JsonFactory()));
    autoQcConfigRequest = new AutoQcConfigRequest(new ArrayList<AutoApprovalRuleDetailsDto>(), Boolean.TRUE);
    Mockito.doNothing().when(autoQcConfigService).update(Mockito.anyString(), Mockito.anyString(), Mockito.any(), Mockito.anyBoolean());
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(autoQcConfigService);
  }

  @Test
  public void updateTest() throws Exception {
    URI uri = new URIBuilder().setPath(
        AutoQcConfigControllerPath.BASE_PATH + AutoQcConfigControllerPath.UPDATE_AUTO_QC_RULE
            .replaceAll("\\{ruleName\\}", DEFAULT_RULE_NAME)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(autoQcConfigRequest)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(autoQcConfigService).update(DEFAULT_RULE_NAME, DEFAULT_STORE_ID, autoQcConfigRequest, false);
  }

  @Test
  public void updateInvalidImageConfigTest() throws Exception {
    ApiInvalidImageQcConfigException apiInvalidImageQcConfigException =
        new ApiInvalidImageQcConfigException(Constants.IMAGE_QC_CONFIG_VALUE_GT_CONFIDENCE_THRESHOLD,
            ApiErrorCode.IMAGE_QC_CONFIG_VALUE_GT_CONFIDENCE_THRESHOLD);
    Mockito.doThrow(apiInvalidImageQcConfigException).when(autoQcConfigService)
        .update(Mockito.anyString(), Mockito.anyString(), Mockito.any(), Mockito.anyBoolean());
    URI uri = new URIBuilder().setPath(
        AutoQcConfigControllerPath.BASE_PATH + AutoQcConfigControllerPath.UPDATE_AUTO_QC_RULE
            .replaceAll("\\{ruleName\\}", DEFAULT_RULE_NAME)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(autoQcConfigRequest)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect((jsonPath("$.success", equalTo(false))));
    Mockito.verify(autoQcConfigService).update(DEFAULT_RULE_NAME, DEFAULT_STORE_ID, autoQcConfigRequest, false);
  }

  @Test
  public void updateTestForException() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(autoQcConfigService)
        .update(DEFAULT_RULE_NAME, DEFAULT_STORE_ID, autoQcConfigRequest, false);
    URI uri = new URIBuilder().setPath(
        AutoQcConfigControllerPath.BASE_PATH + AutoQcConfigControllerPath.UPDATE_AUTO_QC_RULE
            .replaceAll("\\{ruleName\\}", DEFAULT_RULE_NAME)).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID).build();
    getMockMvc().perform(MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(autoQcConfigRequest)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(autoQcConfigService).update(DEFAULT_RULE_NAME, DEFAULT_STORE_ID, autoQcConfigRequest, false);
  }

  private List<AutoApprovalRules> generateAutoApprovalRules() {
    AutoApprovalRules autoApprovalRules = new AutoApprovalRules();
    autoApprovalRules.setStoreId(DEFAULT_STORE_ID);
    autoApprovalRules.setCreatedBy(DEFAULT_CREATED_BY);
    autoApprovalRules.setUpdatedDate(new Date());
    autoApprovalRules.setUpdatedBy(DEFAULT_UPDATED_BY);
    autoApprovalRules.setVersion(DEFAULT_VERSION);
    autoApprovalRules.setCreatedDate(new Date());
    autoApprovalRules.setSequenceNumber(SEQUENCE_NUMBER);
    autoApprovalRules.setRuleName(RULE_NAME);
    autoApprovalRules.setRuleConfig(RULE_CONFIG);
    autoApprovalRules.setImageQcConfig(IMAGE_QC_CONFIG);
    autoApprovalRules.setNeedRevisionConfig(IMAGE_QC_CONFIG);
    autoApprovalRules.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    List<AutoApprovalRules> autoApprovalRulesList = new ArrayList<>();
    autoApprovalRulesList.add(autoApprovalRules);
    return autoApprovalRulesList;
  }

  @Test
  public void getAllAutoQcConfigRulesTest() throws Exception {
    List<AutoApprovalRules> autoApprovalRulesList = generateAutoApprovalRules();
    Mockito.when(autoApprovalService.findAllAutoQcConfigRules()).thenReturn(autoApprovalRulesList);
    URI uri =
        new URIBuilder().setPath(AutoQcConfigControllerPath.BASE_PATH + AutoQcConfigControllerPath.GET_AUTO_QC_RULES)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).build();
    MvcResult response = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Assertions.assertTrue(response.getResponse().getContentAsString().contains(RULE_CONFIG));
    Assertions.assertTrue(response.getResponse().getContentAsString().contains(IMAGE_QC_CONFIG));
  }

  @Test
  public void getAllAutoQcConfigRulesTestException() throws Exception {
    Mockito.doThrow(new IndexOutOfBoundsException()).when(autoApprovalService).findAllAutoQcConfigRules();
    URI uri =
        new URIBuilder().setPath(AutoQcConfigControllerPath.BASE_PATH + AutoQcConfigControllerPath.GET_AUTO_QC_RULES)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).build();
    MvcResult response = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Assertions.assertTrue(response.getResponse().getContentAsString().contains(SUCCESS_FALSE));
  }

  @Test
  public void getAllAutoQcConfigRulesTestExceptionAutoApprovalRulesNull() throws Exception {
    Mockito.when(autoApprovalService.findAllAutoQcConfigRules()).thenReturn(null);
    URI uri =
        new URIBuilder().setPath(AutoQcConfigControllerPath.BASE_PATH + AutoQcConfigControllerPath.GET_AUTO_QC_RULES)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).build();
    MvcResult response = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Assertions.assertTrue(response.getResponse().getContentAsString().contains(SUCCESS_FALSE));
  }
}
