package com.gdn.partners.pcu.internal.web.controller;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.client.model.request.SystemParameterRequest;
import com.gdn.partners.pcu.internal.client.model.response.SystemParameterResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.SystemParameterApiPath;
import com.gdn.partners.pcu.internal.service.SystemParameterService;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.gdn.partners.pcu.internal.client.constants.ClientParameterConstants.CHANNEL_ID;
import static com.gdn.partners.pcu.internal.client.constants.ClientParameterConstants.CLIENT_ID;
import static com.gdn.partners.pcu.internal.client.constants.ClientParameterConstants.REQUEST_ID;
import static com.gdn.partners.pcu.internal.client.constants.ClientParameterConstants.STORE_ID;
import static com.gdn.partners.pcu.internal.client.constants.ClientParameterConstants.USERNAME;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@AutoConfigureMockMvc
public class SystemParameterControllerTest extends TestHelper {


  @InjectMocks
  private SystemParameterController systemParameterController;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ClientParameterHelper clientParameterHelper;


  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(this.systemParameterController).build();
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(systemParameterService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  public void updateSystemParameter() throws Exception {
    List<SystemParameterRequest> systemParameterRequests = new ArrayList<>();
    SystemParameterRequest productSystemParameterRequest = new SystemParameterRequest();
    systemParameterRequests.add(productSystemParameterRequest);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(clientParameterHelper.getChannelId()).thenReturn(Constants.CHANNEL_ID);
    when(clientParameterHelper.getClientId()).thenReturn(Constants.CLIENT_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    MockHttpServletRequestBuilder requestBuilder = put(SystemParameterApiPath.BASE_PATH
        + SystemParameterApiPath.UPDATE_SYSTEM_PARAMETER).content(
            toJson(Collections.singletonList(productSystemParameterRequest)))
        .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
        .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(systemParameterService)
        .updateSystemParameter(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            systemParameterRequests);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getClientId();
    verify(clientParameterHelper).getChannelId();
  }

  @Test
  public void fetchSystemParameter() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(clientParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(SystemParameterApiPath.BASE_PATH
            + SystemParameterApiPath.GET_ALL_SYSTEM_PARAMETER_WITH_SHOW_ON_UI)
            .param("storeId", STORE_ID)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    Mockito.verify(systemParameterService)
        .fetchSystemParameterShowOnUI(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getClientId();
    verify(clientParameterHelper).getChannelId();
  }

  @Test
  public void getSystemParameterSwitchesTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(systemParameterService.getSystemParameterSwitches(STORE_ID, REQUEST_ID)).thenReturn(
      new SystemParameterResponse());

    MockHttpServletRequestBuilder requestBuilder =
      get(SystemParameterApiPath.BASE_PATH + SystemParameterApiPath.FETCH_SYSTEM_PARAMETER).accept(
        MediaType.APPLICATION_JSON_VALUE);

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success",
      is(true)));

    verify(clientParameterHelper, times(2)).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(systemParameterService).getSystemParameterSwitches(STORE_ID, REQUEST_ID);
  }
}
