package com.gdn.partners.pbp.workflow.product;

import java.util.HashMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.workflow.product.ApproveQCWorkflowWorkerBean;

public class ApproveQCWorkflowWorkerTest {

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @InjectMocks
  private ApproveQCWorkflowWorkerBean ApproveQCWorkflowWorkerBean;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    Mockito.doNothing().when(this.productLevel1HistoryService)
        .create(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productLevel1HistoryService);
  }

  @Test
  public void processTest() throws Exception {
    this.ApproveQCWorkflowWorkerBean.process(new HashMap<String, Object>());
    Mockito.verify(this.productLevel1HistoryService).create((String) Mockito.any(), Mockito.any(), Mockito.any());
  }

}
