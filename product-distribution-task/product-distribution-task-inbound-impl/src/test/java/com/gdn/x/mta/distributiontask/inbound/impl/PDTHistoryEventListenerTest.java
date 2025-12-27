package com.gdn.x.mta.distributiontask.inbound.impl;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.dao.api.TaskHistoryRepository;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTHistoryEventModel;
import com.gdn.x.mta.distributiontask.model.TaskHistory;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class PDTHistoryEventListenerTest {

  @InjectMocks
  PDTHistoryEventListener pdtHistoryEventListener;

  @Mock
  ObjectMapper objectMapper;

  @Mock
  TaskHistoryRepository taskHistoryRepository;

  private static final String JSON = "{}";
  private static final String PRODUCT_CODE = "productCode";
  private static final String TASK_CODE = "taskCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String PRODUCT_NAME = "productName";
  private static final String REASON = "reason";
  private static final String STORE_ID = "10001";
  private static final String EXECUTOR = "userName";
  private static final Vendor vendor = new Vendor();

  @Captor
  private ArgumentCaptor<TaskHistory> taskHistoryArgumentCaptor;

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, taskHistoryRepository);
  }

  @Test
   void pdtHistoryEventListenerTest() throws JsonProcessingException {
    PDTHistoryEventModel pdtHistoryEventModel = new PDTHistoryEventModel();
    pdtHistoryEventModel.setProductCode(PRODUCT_CODE);
    pdtHistoryEventModel.setProductName(PRODUCT_NAME);
    pdtHistoryEventModel.setCategoryCode(CATEGORY_CODE);
    pdtHistoryEventModel.setCategoryName(CATEGORY_NAME);
    pdtHistoryEventModel.setVendor(vendor);
    pdtHistoryEventModel.setReason(REASON);
    pdtHistoryEventModel.setState(WorkflowState.PASSED);
    pdtHistoryEventModel.setStoreId(STORE_ID);
    pdtHistoryEventModel.setExecutor(EXECUTOR);
    pdtHistoryEventModel.setTaskCode(TASK_CODE);
    Mockito.when(objectMapper.readValue(JSON, PDTHistoryEventModel.class)).thenReturn(pdtHistoryEventModel);
    pdtHistoryEventListener.pdtHistoryEventListener(JSON);
    Mockito.verify(objectMapper).readValue(JSON, PDTHistoryEventModel.class);
    Mockito.verify(taskHistoryRepository).saveAndFlush(taskHistoryArgumentCaptor.capture());
  }

  @Test
   void pdtHistoryEventListenerExceptionTest() throws JsonProcessingException {
    PDTHistoryEventModel pdtHistoryEventModel = new PDTHistoryEventModel();
    pdtHistoryEventModel.setProductCode(PRODUCT_CODE);
    pdtHistoryEventModel.setProductName(PRODUCT_NAME);
    pdtHistoryEventModel.setCategoryCode(CATEGORY_CODE);
    pdtHistoryEventModel.setCategoryName(CATEGORY_NAME);
    pdtHistoryEventModel.setVendor(vendor);
    pdtHistoryEventModel.setReason(REASON);
    pdtHistoryEventModel.setState(WorkflowState.PASSED);
    pdtHistoryEventModel.setStoreId(STORE_ID);
    pdtHistoryEventModel.setExecutor(EXECUTOR);
    pdtHistoryEventModel.setTaskCode(TASK_CODE);
    Mockito.when(objectMapper.readValue(JSON, PDTHistoryEventModel.class)).thenThrow(new ApplicationRuntimeException());
    pdtHistoryEventListener.pdtHistoryEventListener(JSON);
    Mockito.verify(objectMapper).readValue(JSON, PDTHistoryEventModel.class);
  }
}
