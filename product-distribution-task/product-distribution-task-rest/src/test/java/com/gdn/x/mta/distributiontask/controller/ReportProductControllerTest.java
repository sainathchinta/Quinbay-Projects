package com.gdn.x.mta.distributiontask.controller;

import org.hamcrest.Matchers;
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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.rest.model.ReportProductControllerPath;
import com.gdn.x.mta.distributiontask.rest.model.request.ReportProductRequest;
import com.gdn.x.mta.distributiontask.service.api.ReportProductService;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

public class ReportProductControllerTest {

  @Mock
  private ReportProductService reportProductService;

  @InjectMocks
  private ReportProductController reportProductController;

  private MockMvc mockMvc;

  private static final String STORE_ID = "store-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String CLIENT_ID = "client-id";
  private static final String USER_NAME = "user-name";
  private static final String REQUEST_ID = "request-id";
  private static final ObjectMapper objectMapper = new ObjectMapper();

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(reportProductController).build();
  }

  @Test
   void filterProductTest() throws Exception {
    ReportProductRequest reportProductRequest = ReportProductRequest.builder().build();
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(ReportProductControllerPath.BASE_PATH + ReportProductControllerPath.CREATE_REPORT_PRODUCT)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USER_NAME)
        .content(objectMapper.writeValueAsString(reportProductRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(reportProductService).addReportProduct(reportProductRequest);
  }

  @Test
   void filterProductApplicationExceptionTest() throws Exception {
    ReportProductRequest reportProductRequest = ReportProductRequest.builder().build();
    Mockito.doThrow(new ApplicationRuntimeException(ErrorCategory.VALIDATION))
        .when(reportProductService).addReportProduct(Mockito.any(ReportProductRequest.class));
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(ReportProductControllerPath.BASE_PATH + ReportProductControllerPath.CREATE_REPORT_PRODUCT)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USER_NAME)
        .content(objectMapper.writeValueAsString(reportProductRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(reportProductService).addReportProduct(reportProductRequest);
  }

  @Test
   void filterProductExceptionTest() throws Exception {
    ReportProductRequest reportProductRequest = ReportProductRequest.builder().build();
    Mockito.doThrow(new RuntimeException())
        .when(reportProductService).addReportProduct(Mockito.any(ReportProductRequest.class));
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(ReportProductControllerPath.BASE_PATH + ReportProductControllerPath.CREATE_REPORT_PRODUCT)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USER_NAME)
        .content(objectMapper.writeValueAsString(reportProductRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(reportProductService).addReportProduct(reportProductRequest);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(reportProductService);
  }

}
