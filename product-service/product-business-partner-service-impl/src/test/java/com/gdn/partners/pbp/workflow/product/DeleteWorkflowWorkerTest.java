package com.gdn.partners.pbp.workflow.product;

import java.util.HashMap;
import java.util.Map;


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
import com.gdn.mta.product.service.ProductMailEventService;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEventsEnum;

public class DeleteWorkflowWorkerTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String PROCESS_CODE = "processCode";

  @Mock
  private ProductLevel3WipService productLevel3WipService;

  @Mock
  private ProductLevel1WipService productLevel1WipService;

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @InjectMocks
  private DeleteWorkflowWorkerBean deleteWorkflowWorkerBean;

  @SuppressWarnings("unchecked")
  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    Mockito.doNothing().when(this.productLevel3WipService).delete(Mockito.anyString());
    Mockito.doNothing().when(this.productLevel1WipService).delete(Mockito.anyString(), Mockito.anyString());
    Mockito.doNothing().when(this.productLevel1HistoryService)
        .create(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productLevel3WipService);
    Mockito.verifyNoMoreInteractions(this.productLevel1WipService);
    Mockito.verifyNoMoreInteractions(this.productLevel1HistoryService);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void processTest() throws Exception {
    Map<String, Object> datas = new HashMap<>();
    datas.put("needEmailNotification", "true");
    datas.put(PRODUCT_CODE, PRODUCT_CODE);
    datas.put(PROCESS_CODE, PROCESS_CODE);
    this.deleteWorkflowWorkerBean.process(datas);
    Mockito.verify(this.productLevel3WipService).delete(Mockito.eq(PRODUCT_CODE));
    Mockito.verify(this.productLevel1WipService).delete(Mockito.eq(PRODUCT_CODE), Mockito.anyString());
    Mockito.verify(this.productLevel1HistoryService)
        .create(Mockito.eq(PRODUCT_CODE), Mockito.eq(PROCESS_CODE), Mockito.anyString());
  }

  @Test
  public void processTest_NoEmailNotify() throws Exception {
    Map<String, Object> datas = new HashMap<>();
    datas.put("needEmailNotification", "false");
    datas.put(PRODUCT_CODE, PRODUCT_CODE);
    datas.put(PROCESS_CODE, PROCESS_CODE);
    this.deleteWorkflowWorkerBean.process(datas);
    Mockito.verify(this.productLevel3WipService).delete(Mockito.eq(PRODUCT_CODE));
    Mockito.verify(this.productLevel1WipService).delete(Mockito.eq(PRODUCT_CODE), Mockito.anyString());
    Mockito.verify(this.productLevel1HistoryService)
        .create(Mockito.eq(PRODUCT_CODE), Mockito.eq(PROCESS_CODE), Mockito.anyString());
  }

}
