package com.gdn.mta.product.controller;

import com.gdn.mta.product.service.SequenceServiceImpl;
import com.gdn.mta.product.web.model.SequenceControllerPath;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.Matchers.is;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class SequenceControllerTest {

  private static final String KEY = "key";
  private static final String PRODUCT_CODE_KEY = "MTA";
  private static final Long COUNTER = Long.valueOf(10);
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channelId";
  private static final String DEFAULT_USERNAME = "username";
  private static final String DEFAULT_CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";

  private MockMvc mockMvc;

  @Mock
  private SequenceServiceImpl sequenceServiceImpl;

  @InjectMocks
  private SequenceController sequenceController;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(sequenceController).build();
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(sequenceServiceImpl);
  }

  @Test
  public void findCounterByKeyTest() throws Exception {
    Mockito.when(sequenceServiceImpl.findCounterByKey(KEY)).thenReturn(COUNTER);
    MockHttpServletRequestBuilder requestBuilder =
        get(SequenceControllerPath.BASE_PATH + SequenceControllerPath.GET_SEQUENCE_BY_KEY)
            .param("storeId", DEFAULT_STORE_ID).param("channelId", DEFAULT_CHANNEL_ID)
            .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)
            .param("clientId", DEFAULT_CLIENT_ID).param("key", KEY);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(this.sequenceServiceImpl).findCounterByKey(KEY);
  }

  @Test
  public void findCounterByKey_exceptionTest() throws Exception {
    Mockito.when(sequenceServiceImpl.findCounterByKey(KEY)).thenThrow(RuntimeException.class);
    MockHttpServletRequestBuilder requestBuilder =
        get(SequenceControllerPath.BASE_PATH + SequenceControllerPath.GET_SEQUENCE_BY_KEY)
            .param("storeId", DEFAULT_STORE_ID).param("channelId", DEFAULT_CHANNEL_ID)
            .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)
            .param("clientId", DEFAULT_CLIENT_ID).param("key", KEY);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(false)));
    Mockito.verify(this.sequenceServiceImpl).findCounterByKey(KEY);
  }

  @Test
  public void findCounterByKey_productCodeKeyTest() throws Exception {
    Mockito.when(sequenceServiceImpl.findCounterByKey(KEY)).thenThrow(RuntimeException.class);
    MockHttpServletRequestBuilder requestBuilder =
        get(SequenceControllerPath.BASE_PATH + SequenceControllerPath.GET_SEQUENCE_BY_KEY)
            .param("storeId", DEFAULT_STORE_ID).param("channelId", DEFAULT_CHANNEL_ID)
            .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)
            .param("clientId", DEFAULT_CLIENT_ID).param("key", PRODUCT_CODE_KEY);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(false)));
  }
}