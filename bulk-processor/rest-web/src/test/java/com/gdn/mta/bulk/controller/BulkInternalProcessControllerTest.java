package com.gdn.mta.bulk.controller;


import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.HashMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.dto.BulkInternalProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkProcessUpdateRequest;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.InternalProcessPendingFilesResponse;
import com.gdn.mta.bulk.service.InternalProcessService;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;

public class BulkInternalProcessControllerTest {

  private static final String STORE_ID = "store-id";
  private static final String CLIENT_ID = "client-id";
  private static final String REQUEST_ID = "request-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String USERNAME = "username";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String INTERNAL_PROCESS_REQUEST_CODE = "internalProcessRequestCode";

  private MockMvc mockMvc;
  private ObjectMapper objectMapper = new ObjectMapper();
  private BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest = new BulkInternalProcessSummaryRequest();
  private BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest = new BulkInternalProcessUploadRequest();

  @InjectMocks
  private BulkInternalProcessController bulkInternalProcessController;

  @Mock
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Mock
  private InternalProcessService internalProcessService;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.bulkInternalProcessController).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(internalProcessServiceWrapper);
  }

  @Test
  public void processNewStoreCopyRequestsTest() throws Exception {
    this.mockMvc.perform(get(BulkInternalProcessController.BASE_PATH +
            BulkInternalProcessController.PROCESS_NEW_FILE_REQUESTS).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID)
            .param("requestId", REQUEST_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("username", USERNAME)
            .param("processType", BulkInternalProcessType.STORE_COPY.name()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.internalProcessServiceWrapper).processNewInternalProcessRequest(STORE_ID, USERNAME, BulkInternalProcessType.STORE_COPY.name());
  }

  @Test
  public void processNewStoreCopyRequestsExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(internalProcessServiceWrapper).processNewInternalProcessRequest(STORE_ID, USERNAME, BulkInternalProcessType.STORE_COPY.name());
    this.mockMvc.perform(get(BulkInternalProcessController.BASE_PATH +
            BulkInternalProcessController.PROCESS_NEW_FILE_REQUESTS).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID)
            .param("requestId", REQUEST_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("username", USERNAME)
            .param("processType", BulkInternalProcessType.STORE_COPY.name()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.internalProcessServiceWrapper).processNewInternalProcessRequest(STORE_ID, USERNAME,
        BulkInternalProcessType.STORE_COPY.name());
  }

  @Test
  public void processStoreCopyDataProductCreationTest() throws Exception {
    this.mockMvc.perform(get(BulkInternalProcessController.BASE_PATH +
            BulkInternalProcessController.PROCESS_INTERNAL_REQUEST_DATA).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID)
            .param("requestId", REQUEST_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("username", USERNAME)
            .param("processType", BulkInternalProcessType.STORE_COPY.name()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.internalProcessServiceWrapper).processInternalProcessDataRequest(STORE_ID, REQUEST_ID, USERNAME, BulkInternalProcessType.STORE_COPY.name());
  }

  @Test
  public void deleteInternalRequestTest() throws Exception {
    this.mockMvc.perform(
        get(BulkInternalProcessController.BASE_PATH + BulkInternalProcessController.DELETE_INTERNAL_REQUEST).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("username", USERNAME).param("processType", BulkInternalProcessType.STORE_COPY.name()))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.internalProcessServiceWrapper).deleteOldBulkInternalProcessRequest(STORE_ID, BulkInternalProcessType.STORE_COPY.name());
  }

  @Test
  public void bulkInternalProcessSummaryTest() throws Exception {
    Mockito.when(this.internalProcessServiceWrapper
        .bulkInternalProcessSummary(eq(STORE_ID), Mockito.any(BulkInternalProcessSummaryRequest.class), eq(PAGE), eq(SIZE)))
        .thenReturn(new PageImpl(new ArrayList()));
    this.mockMvc.perform(post(BulkInternalProcessController.BASE_PATH + BulkInternalProcessController.SUMMARY)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(bulkInternalProcessSummaryRequest)).param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("username", USERNAME).param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.internalProcessServiceWrapper).bulkInternalProcessSummary(STORE_ID, bulkInternalProcessSummaryRequest, PAGE, SIZE);
  }

  @Test
  public void bulkInternalProcessSummaryTestException() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(internalProcessServiceWrapper)
        .bulkInternalProcessSummary(STORE_ID, bulkInternalProcessSummaryRequest, PAGE, SIZE);
    this.mockMvc.perform(post(BulkInternalProcessController.BASE_PATH + BulkInternalProcessController.SUMMARY)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(bulkInternalProcessSummaryRequest)).param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("username", USERNAME).param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.internalProcessServiceWrapper).bulkInternalProcessSummary(STORE_ID, bulkInternalProcessSummaryRequest, PAGE, SIZE);
  }

  @Test
  public void bulkInternalProcessUploadTest() throws Exception {
    Mockito.doNothing().when(this.internalProcessServiceWrapper)
        .uploadBulkInternalProcess(eq(STORE_ID), Mockito.any(BulkInternalProcessUploadRequest.class));
    this.mockMvc.perform(post(BulkInternalProcessController.BASE_PATH + BulkInternalProcessController.UPLOAD)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(bulkInternalProcessUploadRequest)).param("storeId", STORE_ID)
            .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("username", USERNAME).param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.internalProcessServiceWrapper).uploadBulkInternalProcess(STORE_ID, bulkInternalProcessUploadRequest);
  }

  @Test
  public void bulkInternalProcessUploadExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.internalProcessServiceWrapper)
        .uploadBulkInternalProcess(eq(STORE_ID), Mockito.any(BulkInternalProcessUploadRequest.class));
    this.mockMvc.perform(post(BulkInternalProcessController.BASE_PATH + BulkInternalProcessController.UPLOAD)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(bulkInternalProcessUploadRequest)).param("storeId", STORE_ID)
            .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("username", USERNAME).param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.internalProcessServiceWrapper).uploadBulkInternalProcess(STORE_ID, bulkInternalProcessUploadRequest);
  }

  @Test
  public void abortPendingBulkInternalProcessTest() throws Exception {
    this.mockMvc.perform(get(BulkInternalProcessController.BASE_PATH + BulkInternalProcessController.ABORT_PENDING_TASK)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("username", USERNAME).param("processType", BulkInternalProcessType.SALES_CATEGORY_UPDATE.name()))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.internalProcessServiceWrapper)
        .abortPendingBulkInternalProcessBefore(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
  }

  @Test
  public void abortPendingBulkInternalProcessExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(internalProcessServiceWrapper)
        .abortPendingBulkInternalProcessBefore(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    this.mockMvc.perform(get(BulkInternalProcessController.BASE_PATH + BulkInternalProcessController.ABORT_PENDING_TASK)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("username", USERNAME).param("processType", BulkInternalProcessType.SALES_CATEGORY_UPDATE.name()))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.internalProcessServiceWrapper)
        .abortPendingBulkInternalProcessBefore(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
  }

  @Test
  public void failPendingBulkInternalProcessTest() throws Exception {
    this.mockMvc.perform(get(BulkInternalProcessController.BASE_PATH + BulkInternalProcessController.FAIL_PENDING_TASK)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("username", USERNAME).param("processType", BulkInternalProcessType.SALES_CATEGORY_UPDATE.name()))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.internalProcessServiceWrapper)
        .failPendingBulkInternalProcessDataBefore(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
  }

  @Test
  public void failPendingBulkInternalProcessExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(internalProcessServiceWrapper)
        .failPendingBulkInternalProcessDataBefore(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    this.mockMvc.perform(get(BulkInternalProcessController.BASE_PATH + BulkInternalProcessController.FAIL_PENDING_TASK)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("username", USERNAME).param("processType", BulkInternalProcessType.SALES_CATEGORY_UPDATE.name()))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.internalProcessServiceWrapper)
        .failPendingBulkInternalProcessDataBefore(STORE_ID, BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
  }

  @Test
  public void bulkInternalProcessStatusUpdateTest() throws Exception {
    this.mockMvc.perform(get(BulkInternalProcessController.BASE_PATH +
            BulkInternalProcessController.PROCESS_STATUS_UPDATE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID)
            .param("requestId", REQUEST_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("username", USERNAME)
            .param("processType", BulkInternalProcessType.STORE_COPY.name()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.internalProcessServiceWrapper).processStatusUpdate(STORE_ID, REQUEST_ID, USERNAME,
        BulkInternalProcessType.STORE_COPY.name());
  }

  @Test
  public void bulkInternalProcessCancelReqestTest() throws Exception {
    this.mockMvc.perform(post(BulkInternalProcessController.BASE_PATH + BulkInternalProcessController.CANCEL_REQUEST)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("username", USERNAME).param("internalProcessRequestCode", INTERNAL_PROCESS_REQUEST_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.internalProcessServiceWrapper)
        .bulkInternalProcessCancelRequest(STORE_ID, USERNAME, INTERNAL_PROCESS_REQUEST_CODE);
  }

  @Test
  public void bulkInternalProcessCancelReqestExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.internalProcessServiceWrapper)
        .bulkInternalProcessCancelRequest(Mockito.any(), Mockito.any(), Mockito.any());
    this.mockMvc.perform(post(BulkInternalProcessController.BASE_PATH + BulkInternalProcessController.CANCEL_REQUEST)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("username", USERNAME).param("internalProcessRequestCode", INTERNAL_PROCESS_REQUEST_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.internalProcessServiceWrapper)
        .bulkInternalProcessCancelRequest(STORE_ID, USERNAME, INTERNAL_PROCESS_REQUEST_CODE);
  }

  @Test
  public void checkPendingFilesTest() throws Exception {
    Mockito
        .when(internalProcessService.checkPendingFiles(Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new InternalProcessPendingFilesResponse());
    this.mockMvc.perform(post(BulkInternalProcessController.BASE_PATH + BulkInternalProcessController.PENDING_FILES)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("username", USERNAME).param("processType", BulkInternalProcessType.STORE_COPY.name()))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.internalProcessService)
        .checkPendingFiles(STORE_ID, USERNAME, BulkInternalProcessType.STORE_COPY.name());
  }

  @Test
  public void bulkRebateTest() throws Exception {
    BulkUpdateProcessDTO bulkUpdateProcessDTO = new BulkUpdateProcessDTO();
    bulkUpdateProcessDTO.setBusinessPartnerCode("businessPartnerCode");
    BulkProcessUpdateRequest bulkProcessUpdateRequest = getBulkProcessUpdateRequest();
    BeanUtils.copyProperties(bulkProcessUpdateRequest, bulkUpdateProcessDTO);
    Mockito.doNothing().when(internalProcessServiceWrapper).preProcessBulkPriceRecommendationFile(bulkUpdateProcessDTO);
    this.mockMvc.perform(
        post(BulkInternalProcessController.BASE_PATH + BulkInternalProcessController.BULK_PRICE_RECOMMENDATION_UPLOAD)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(bulkProcessUpdateRequest)).param("storeId", STORE_ID)
            .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
          .param("processType", BulkInternalProcessType.BULK_PRICE_REBATE.name())
            .param("username", USERNAME)).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    Mockito.verify(internalProcessServiceWrapper).preProcessBulkPriceRecommendationFile(bulkUpdateProcessDTO);
  }

  @Test
  public void bulkRebateErrorTest() throws Exception {
    BulkUpdateProcessDTO bulkUpdateProcessDTO = new BulkUpdateProcessDTO();
    bulkUpdateProcessDTO.setBusinessPartnerCode("businessPartnerCode");
    BulkProcessUpdateRequest bulkProcessUpdateRequest = getBulkProcessUpdateRequest();
    BeanUtils.copyProperties(bulkProcessUpdateRequest, bulkUpdateProcessDTO);
    Mockito.doThrow(new Exception()).when(internalProcessServiceWrapper)
        .preProcessBulkPriceRecommendationFile(bulkUpdateProcessDTO);
    try {
      this.mockMvc.perform(
          post(BulkInternalProcessController.BASE_PATH + BulkInternalProcessController.BULK_PRICE_RECOMMENDATION_UPLOAD)
              .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(objectMapper.writeValueAsString(bulkProcessUpdateRequest)).param("storeId", STORE_ID)
              .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("processType", BulkInternalProcessType.BULK_PRICE_REBATE.name())
              .param("username", USERNAME)).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
          .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    } finally {
      Mockito.verify(internalProcessServiceWrapper).preProcessBulkPriceRecommendationFile(bulkUpdateProcessDTO);
    }
  }



  private BulkProcessUpdateRequest getBulkProcessUpdateRequest() {
    BulkProcessUpdateRequest bulkProcessUpdateRequest = new BulkProcessUpdateRequest();
    bulkProcessUpdateRequest.setBulkProcessType(BulkInternalProcessType.BULK_PRICE_REBATE.name());
    bulkProcessUpdateRequest.setUpdatedBy("");
    bulkProcessUpdateRequest.setBusinessPartnerCode("");
    bulkProcessUpdateRequest.setFileContent(new byte[]{1});
    bulkProcessUpdateRequest.setClientHost("");
    bulkProcessUpdateRequest.setPrivilegedMap(new HashMap<>());
    bulkProcessUpdateRequest.setFileName("");
    return bulkProcessUpdateRequest;
  }
}