package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.net.URI;
import java.util.ArrayList;
import java.util.UUID;

import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.web.servlet.MockMvc;

import com.gdn.x.productcategorybase.service.LookupService;

public class LookupControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_USERNAME = "com.gdn.mta";
  private static final String DEFAULT_CLIENT_ID = "CLIENT";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String LOOKUP_GROUP = "LOOKUP_GROUP";

  private MockMvc mockMvc;

  @Mock
  private LookupService lookupService;

  @InjectMocks
  private LookupController lookupController;

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.lookupController).build();
  }

  @Test
  public void getLookupByLookupGroupTest() throws Exception {
    Mockito.when(this.lookupService.getLookupByLookupGroup(LOOKUP_GROUP)).thenReturn(new ArrayList<>());

    URI uri = new URIBuilder().setPath(LookupController.BASE_PATH  + LookupController.LOOKUP_GROUP)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("lookupGroup", LOOKUP_GROUP).build();
    this.mockMvc.perform(get(uri)).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.lookupService).getLookupByLookupGroup(LOOKUP_GROUP);
  }

  @Test
  public void getLookupByLookupGroupExceptionTest() throws Exception {
    when(this.lookupService.getLookupByLookupGroup(LOOKUP_GROUP)).thenThrow(RuntimeException.class);

    URI uri = new URIBuilder().setPath(LookupController.BASE_PATH  + LookupController.LOOKUP_GROUP)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("lookupGroup", LOOKUP_GROUP).build();
    this.mockMvc.perform(get(uri)).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.lookupService).getLookupByLookupGroup(LOOKUP_GROUP);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(lookupService);
  }
}
