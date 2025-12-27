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
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;

public class ApproveContentWorkflowWorkerTest {

  @Mock
  private ProductLevel3WipService productLevel3WipService;

  @Mock
  private ProductLevel1WipService productLevel1WipService;

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @InjectMocks
  private ApproveContentWorkflowWorkerBean approveContentWorkflowWorkerBean;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    Mockito.doNothing().when(this.productLevel3WipService).update(Mockito.any());
    Mockito.doNothing().when(this.productLevel1WipService).approveContent(Mockito.any());
    Mockito.doNothing().when(this.productLevel1HistoryService).create((String) Mockito.any(), Mockito.any(), Mockito.any());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productLevel3WipService);
    Mockito.verifyNoMoreInteractions(this.productLevel1WipService);
    Mockito.verifyNoMoreInteractions(this.productLevel1HistoryService);
  }

  @Test
  public void processTest() throws Exception {
    this.approveContentWorkflowWorkerBean.process(new HashMap<String, Object>());
    Mockito.verify(this.productLevel3WipService).update(Mockito.any());
    Mockito.verify(this.productLevel1WipService).approveContent(Mockito.any());
    Mockito.verify(this.productLevel1HistoryService).create((String) Mockito.any(), Mockito.any(), Mockito.any());
  }

}
