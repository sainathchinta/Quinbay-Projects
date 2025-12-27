package com.gdn.partners.pcu.master.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.net.URI;
import java.util.ArrayList;
import java.util.UUID;

import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.model.AutoQcConfigApiPath;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.service.AutoQcConfigService;
import com.gdn.partners.pcu.master.web.helper.TestHelper;
import com.gdn.partners.pcu.master.web.model.request.AutoQcConfigUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.response.AutoApprovalRulesListWebResponse;

@ExtendWith(MockitoExtension.class)
public class AutoQcConfigControllerTest extends TestHelper {
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_RULE_NAME = "OFFICIAL_SELLERS";
  private static final String SUCCESS_TRUE = "\"success\":true";
  private AutoApprovalRulesListWebResponse autoApprovalRulesListWebResponse;

  @InjectMocks
  private AutoQcConfigController autoQcConfigController;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private AutoQcConfigService autoQcConfigService;

  private AutoQcConfigUpdateWebRequest autoQcConfigUpdateWebRequest;

  private ObjectMapper objectMapper;

  private MockMvc mockMvc;

  void setMockMvc(MockMvc mockMvc) {
    this.mockMvc = mockMvc;
  }

  public MockMvc getMockMvc() {
    return this.mockMvc;
  }

  void setObjectMapper(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  public ObjectMapper getObjectMapper() {
    return objectMapper;
  }

  @BeforeEach
  void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.setMockMvc(MockMvcBuilders.standaloneSetup(this.autoQcConfigController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build());
    this.setObjectMapper(new ObjectMapper(new JsonFactory()));
    autoQcConfigUpdateWebRequest = new AutoQcConfigUpdateWebRequest(new ArrayList<>(), Boolean.TRUE);
    mockMvc = MockMvcBuilders.standaloneSetup(autoQcConfigController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build();
    autoApprovalRulesListWebResponse = new AutoApprovalRulesListWebResponse();
    autoApprovalRulesListWebResponse.setAutoApprovalRulesWebResponseList(new ArrayList<>());
  }

  @Test
  void updateTest() throws Exception {
    Mockito.when(autoQcConfigService.updateAutoQcConfigRule(DEFAULT_RULE_NAME, false, autoQcConfigUpdateWebRequest))
        .thenReturn(new GdnBaseRestResponse(null, null, true, DEFAULT_REQUEST_ID));

    URI uri = new URIBuilder().setPath(
        AutoQcConfigApiPath.BASE_PATH + AutoQcConfigApiPath.UPDATE_AUTO_APPROVAL_RULES_BY_RULE_NAME
            .replaceAll("\\{ruleName\\}", DEFAULT_RULE_NAME)).build();

    MvcResult response = getMockMvc().perform(MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(autoQcConfigUpdateWebRequest))
        .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk()).andReturn();

    Mockito.verify(autoQcConfigService).updateAutoQcConfigRule(DEFAULT_RULE_NAME, false, autoQcConfigUpdateWebRequest);
    Assertions.assertTrue(response.getResponse().getContentAsString().contains(SUCCESS_TRUE));
  }

  @Test
  void getAutoApprovalRulesTest() throws Exception {
    Mockito.when(this.autoQcConfigService.getAutoApprovalRules()).thenReturn(this.autoApprovalRulesListWebResponse);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(AutoQcConfigApiPath.BASE_PATH + AutoQcConfigApiPath.GET_AUTO_APPROVAL_RULES)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(autoQcConfigService).getAutoApprovalRules();
  }
}
