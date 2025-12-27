package com.gdn.x.mta.distributiontask.controller;

import java.util.ArrayList;
import java.util.List;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.x.mta.distributiontask.model.TaskHistory;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.TaskHistoryService;

public class TaskHistoryControllerTest {
  private static final String STORE_ID = "STORE_ID";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String PRODUCT_NAME = "PRODUCT_CODE";
  private static final String CATEGORY_CODE = "CATEGORY_CODES";
  private static final String USERNAME = "USERNAME";
  private static final String CATEGORY_NAME = "CATEGORY_NAME";
  private static final String REASON = "REASON";
  private static final String TASK_CODE = "TASK_CODE";
  private static final String REQUEST_ID = "REQUEST_ID";
  private static final String CLIENT_ID = "CLIENT_ID";
  private static final String CHANNEL_ID = "CHANNEL_ID";
  private static final String VENDOR_CODE = "VENDOR_CODE";
  private static final String VENDOR_NAME = "VENDOR_NAME";
  private static final String REASON_WITH_ENDLINE = "Reason \\n endline";
  private static final String REASON_WITHOUT_ENDLINE = "Reason  endline";


  @InjectMocks
  private TaskHistoryController instance;

  @Mock
  private TaskHistoryService taskHistoryService;

  private MockMvc mockMvc;

  private List<TaskHistory> taskHistories;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    this.mockMvc = MockMvcBuilders.standaloneSetup(instance).build();
    this.taskHistories = new ArrayList<TaskHistory>();
    Vendor vendor = new Vendor.Builder().vendorCode(VENDOR_CODE).name(VENDOR_NAME).build();
    TaskHistory taskHistory = new TaskHistory(PRODUCT_CODE, PRODUCT_NAME, CATEGORY_CODE,
        CATEGORY_NAME, vendor, "", WorkflowState.IN_REVIEW, STORE_ID, USERNAME, TASK_CODE);
    this.taskHistories.add(taskHistory);
  }

  @Test
   void testTaskHistorySummaryByProductCode() throws Exception {
    this.taskHistories.get(0).setReason(REASON_WITH_ENDLINE);
    Mockito
        .when(this.taskHistoryService.getHistoryFromProductCode(Mockito.anyString(),
            Mockito.anyString(), (Pageable) Mockito.any()))
        .thenReturn(new PageImpl<TaskHistory>(new ArrayList<TaskHistory>(this.taskHistories)));
    this.mockMvc
        .perform(
            MockMvcRequestBuilders.get(TaskHistoryController.TASK_HISTORY_SUMMARY_BY_PRODUCT_CODE)
                .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("productCode", PRODUCT_CODE).param("escapeString", String.valueOf(true)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.content.[0].reason", Matchers.equalTo(REASON_WITHOUT_ENDLINE)));
    Mockito.verify(this.taskHistoryService, Mockito.times(1)).getHistoryFromProductCode(
        Mockito.anyString(), Mockito.anyString(), (Pageable) Mockito.any());
  }
  
  @Test
   void testTaskHistorySummaryByProductCodeNullHistory() throws Exception {
    Mockito
        .when(this.taskHistoryService.getHistoryFromProductCode(Mockito.anyString(),
            Mockito.anyString(), (Pageable) Mockito.any()))
        .thenReturn(null);
    this.mockMvc
        .perform(
            MockMvcRequestBuilders.get(TaskHistoryController.TASK_HISTORY_SUMMARY_BY_PRODUCT_CODE)
                .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("productCode", PRODUCT_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.taskHistoryService, Mockito.times(1)).getHistoryFromProductCode(
        Mockito.anyString(), Mockito.anyString(), (Pageable) Mockito.any());
  }
  
  @SuppressWarnings("unchecked")
  @Test
   void testTaskHistorySummaryByProductCodeException() throws Exception {
    Mockito
        .when(this.taskHistoryService.getHistoryFromProductCode(Mockito.anyString(),
            Mockito.anyString(), (Pageable) Mockito.any()))
        .thenThrow(Exception.class);
    this.mockMvc
        .perform(
            MockMvcRequestBuilders.get(TaskHistoryController.TASK_HISTORY_SUMMARY_BY_PRODUCT_CODE)
                .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("productCode", PRODUCT_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.taskHistoryService, Mockito.times(1)).getHistoryFromProductCode(
        Mockito.anyString(), Mockito.anyString(), (Pageable) Mockito.any());
  }
  
  @Test
   void testTaskHistorySummaryByTaskCode() throws Exception {
    Mockito
        .when(this.taskHistoryService.getHistoryFromTaskCode(Mockito.anyString(),
            Mockito.anyString(), (Pageable) Mockito.any()))
        .thenReturn(new PageImpl<TaskHistory>(new ArrayList<TaskHistory>(this.taskHistories)));
    this.mockMvc
        .perform(
            MockMvcRequestBuilders.get(TaskHistoryController.TASK_HISTORY_SUMMARY_BY_TASK_CODE)
                .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("taskCode", TASK_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.taskHistoryService, Mockito.times(1)).getHistoryFromTaskCode(
        Mockito.anyString(), Mockito.anyString(), (Pageable) Mockito.any());
  }
  
  @Test
   void testTaskHistorySummaryByTaskCodeNullHistory() throws Exception {
    Mockito
        .when(this.taskHistoryService.getHistoryFromTaskCode(Mockito.anyString(),
            Mockito.anyString(), (Pageable) Mockito.any()))
        .thenReturn(null);
    this.mockMvc
        .perform(
            MockMvcRequestBuilders.get(TaskHistoryController.TASK_HISTORY_SUMMARY_BY_TASK_CODE)
                .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("taskCode", TASK_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.taskHistoryService, Mockito.times(1)).getHistoryFromTaskCode(
        Mockito.anyString(), Mockito.anyString(), (Pageable) Mockito.any());
  }
  
  @SuppressWarnings("unchecked")
  @Test
   void testTaskHistorySummaryByTaskCodeException() throws Exception {
    Mockito
        .when(this.taskHistoryService.getHistoryFromTaskCode(Mockito.anyString(),
            Mockito.anyString(), (Pageable) Mockito.any()))
        .thenThrow(Exception.class);
    this.mockMvc
        .perform(
            MockMvcRequestBuilders.get(TaskHistoryController.TASK_HISTORY_SUMMARY_BY_TASK_CODE)
                .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("taskCode", TASK_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.taskHistoryService, Mockito.times(1)).getHistoryFromTaskCode(
        Mockito.anyString(), Mockito.anyString(), (Pageable) Mockito.any());
  }

}
