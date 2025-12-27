package com.gdn.x.mta.distributiontask.controller;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.x.mta.distributiontask.service.api.SlaCheckerService;

public class SlaControllerTest {

  private static final String STORE_ID = "10001";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "user";

  @InjectMocks
  private SlaController slaController;

  private MockMvc mockMvc;
  
  @Mock
  private SlaCheckerService slaServiceChecker;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.slaController).build();
  }

  @AfterEach
  public void tearDown() throws Exception {}

  @Test
   void testExecuteSlaCheckerJob() throws Exception {
    Mockito.doNothing().when(this.slaServiceChecker).execute();
    this.mockMvc
        .perform(MockMvcRequestBuilders.get(SlaController.BASE_PATH + SlaController.EXECUTE_JOB)
            .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.slaServiceChecker, Mockito.times(1)).execute();
  }

}
