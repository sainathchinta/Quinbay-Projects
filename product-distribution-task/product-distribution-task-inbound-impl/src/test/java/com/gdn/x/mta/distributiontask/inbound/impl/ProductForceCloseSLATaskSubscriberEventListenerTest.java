package com.gdn.x.mta.distributiontask.inbound.impl;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductForcedRollbackSLAExceedDomainEventModel;
import com.gdn.x.mta.distributiontask.service.api.SlaCheckerService;

public class ProductForceCloseSLATaskSubscriberEventListenerTest {

  private static final String PRODUCT_ID = "prd_id";
  private static final String PRODUCT_CODE = "prd_code";
  private static final String TASK_ID = "task_id";
  private static final String TASK_CODE = "task_code";

  @InjectMocks
  private ProductForceCloseSLATaskSubscriberEventListener instance;

  @Mock
  private SlaCheckerService slaCheckerService;

  @Mock
  private ProductForcedRollbackSLAExceedDomainEventModel productForcedRollbackSLAExceedDomainEventModel;

  @Mock
  private ObjectMapper objectMapper;

  private ObjectMapper mapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    mapper = new ObjectMapper();
    this.productForcedRollbackSLAExceedDomainEventModel =
        new ProductForcedRollbackSLAExceedDomainEventModel(TASK_CODE, TASK_ID, PRODUCT_CODE,
            PRODUCT_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
   void testOnDomainEventConsumed() throws Exception {
    String message = mapper.writeValueAsString(productForcedRollbackSLAExceedDomainEventModel);
    Mockito.when(objectMapper.readValue(message, ProductForcedRollbackSLAExceedDomainEventModel.class)).thenReturn(productForcedRollbackSLAExceedDomainEventModel);
    this.instance.onDomainEventConsumed(message);
    Mockito.verify(this.slaCheckerService, Mockito.times(1)).turnTaskToDistributionList(TASK_ID);
    Mockito.verify(objectMapper).readValue(message, ProductForcedRollbackSLAExceedDomainEventModel.class);
  }
  
  @Test
   void testOnDomainEventConsumedException() throws Exception {
    Mockito.doThrow(Exception.class).when(this.slaCheckerService).turnTaskToDistributionList(TASK_ID);
    String message = mapper.writeValueAsString(productForcedRollbackSLAExceedDomainEventModel);
    Mockito.when(objectMapper.readValue(message, ProductForcedRollbackSLAExceedDomainEventModel.class))
        .thenReturn(productForcedRollbackSLAExceedDomainEventModel);
      this.instance.onDomainEventConsumed(message);
      Mockito.verify(this.slaCheckerService, Mockito.times(1)).turnTaskToDistributionList(TASK_ID);
      Mockito.verify(objectMapper).readValue(message, ProductForcedRollbackSLAExceedDomainEventModel.class);
    }
}
