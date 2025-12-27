package com.gdn.mta.bulk.controller;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.dto.SystemParameterConfigRequest;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

public class SystemParameterConfigControllerTest {

  private static final String STORE_ID = "store-id";
  private static final String CLIENT_ID = "client-id";
  private static final String REQUEST_ID = "request-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String USERNAME = "username";
  private static final String VALUE = "value";
  private static final String VALUE_UPDATED = "value1";
  private static final String VARIABLE = "variable";
  private static final String DESCRIPTION = "description";

  @InjectMocks
  private SystemParameterConfigController controller;

  @Mock
  private SystemParameterConfigService service;

  private MockMvc mockMvc;
  private SystemParameterConfigRequest systemParameterConfigRequest;
  private SystemParameterConfig systemParameterConfig;
  private String systemParameterConfigString;
  private String systemParameterConfigStringUpdated;
  private SystemParameterConfigRequest systemParameterConfigRequestUpdated;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    ObjectMapper mapper = new ObjectMapper();
    this.mockMvc = standaloneSetup(this.controller).build();
    this.systemParameterConfigRequest = new SystemParameterConfigRequest(VARIABLE, VALUE, DESCRIPTION);
    this.systemParameterConfig = new SystemParameterConfig(VARIABLE, VALUE, DESCRIPTION);
    systemParameterConfigString = mapper.writeValueAsString(systemParameterConfigRequest);
    systemParameterConfigRequestUpdated = new SystemParameterConfigRequest(VARIABLE, VALUE_UPDATED, DESCRIPTION);
    systemParameterConfigStringUpdated = mapper.writeValueAsString(systemParameterConfigRequestUpdated);
    when(this.service.findValueByStoreIdAndVariable(STORE_ID, VARIABLE)).thenReturn(this.systemParameterConfig);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.service);
  }

  @Test
  public void deleteSystemParameterConfig() throws Exception {
    this.mockMvc.perform(delete(SystemParameterConfigController.BASE_PATH +
        SystemParameterConfigController.SYSTEM_PARAMETER_DELETE).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME)
        .param("variable", VARIABLE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.service).delete(STORE_ID, USERNAME, VARIABLE);
  }

  @Test
  public void deleteSystemParameterConfigFailed() throws Exception {
    doThrow(RuntimeException.class).when(service).delete(STORE_ID, USERNAME, VARIABLE);
    this.mockMvc.perform(delete(SystemParameterConfigController.BASE_PATH +
        SystemParameterConfigController.SYSTEM_PARAMETER_DELETE).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME)
        .param("variable", VARIABLE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.service).delete(STORE_ID, USERNAME, VARIABLE);
  }


  @Test
  public void findOne() throws Exception {
    this.mockMvc.perform(get(SystemParameterConfigController.BASE_PATH +
        SystemParameterConfigController.SYSTEM_PARAMETER_FIND_ONE).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME)
        .param("variable", VARIABLE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)))
        .andExpect(jsonPath("$.value.variable", equalTo(VARIABLE)))
        .andExpect(jsonPath("$.value.value", equalTo(VALUE)))
        .andExpect(jsonPath("$.value.description", equalTo(DESCRIPTION)));
    verify(this.service).findValueByStoreIdAndVariable(STORE_ID, VARIABLE);
  }

  @Test
  public void findOneFailed() throws Exception {
    doThrow(RuntimeException.class).when(service).findValueByStoreIdAndVariable(STORE_ID, VARIABLE);
    this.mockMvc.perform(get(SystemParameterConfigController.BASE_PATH +
        SystemParameterConfigController.SYSTEM_PARAMETER_FIND_ONE).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME)
        .param("variable", VARIABLE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)))
        .andExpect(jsonPath("$.value.variable", equalTo(null)))
        .andExpect(jsonPath("$.value.value", equalTo(null)))
        .andExpect(jsonPath("$.value.description", equalTo(null)));
    verify(this.service).findValueByStoreIdAndVariable(STORE_ID, VARIABLE);
  }

  @Test
  public void insertSystemParameterConfig() throws Exception {
    this.mockMvc.perform(post(SystemParameterConfigController.BASE_PATH +
        SystemParameterConfigController.SYSTEM_PARAMETER_ADD).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(systemParameterConfigString)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("username", USERNAME)
        .param("clientId", CLIENT_ID)).andExpect(status().isOk());
    verify(this.service).insert(STORE_ID, USERNAME, this.systemParameterConfigRequest);
  }

  @Test
  public void insertSystemParameterConfigFailed() throws Exception {
    doThrow(Exception.class).when(service).insert(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(SystemParameterConfigRequest.class));
    this.mockMvc.perform(post(SystemParameterConfigController.BASE_PATH +
        SystemParameterConfigController.SYSTEM_PARAMETER_ADD).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(systemParameterConfigString)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("username", USERNAME)
        .param("clientId", CLIENT_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.service).insert(STORE_ID, USERNAME, this.systemParameterConfigRequest);
  }

  @Test
  public void updateSystemParameterConfig() throws Exception {
    this.mockMvc.perform(put(SystemParameterConfigController.BASE_PATH +
        SystemParameterConfigController.SYSTEM_PARAMETER_UPDATE).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(systemParameterConfigStringUpdated)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("username", USERNAME)
        .param("clientId", CLIENT_ID)).andExpect(status().isOk());
    verify(this.service).update(STORE_ID, USERNAME, this.systemParameterConfigRequestUpdated);
  }

  @Test
  public void updateSystemParameterConfigFailed() throws Exception {
    doThrow(RuntimeException.class).when(service).update(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(SystemParameterConfigRequest.class));
    this.mockMvc.perform(put(SystemParameterConfigController.BASE_PATH +
        SystemParameterConfigController.SYSTEM_PARAMETER_UPDATE).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(systemParameterConfigStringUpdated)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("username", USERNAME)
        .param("clientId", CLIENT_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.service).update(STORE_ID, USERNAME, this.systemParameterConfigRequestUpdated);
  }
}
