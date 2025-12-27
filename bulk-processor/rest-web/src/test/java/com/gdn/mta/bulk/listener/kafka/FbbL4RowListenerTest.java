package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.FbbL5CreateDTO;
import com.gdn.mta.bulk.entity.FbbL5ItemEventModel;
import com.gdn.mta.bulk.models.InternalProcessDataDomainEventModel;
import com.gdn.mta.bulk.service.FbbConsignmentService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.doThrow;

public class FbbL4RowListenerTest {

  private static final String MESSAGE = "MESSAGE";
  private static final String ID = "ID";
  private static final String STORE_ID = "STORE_ID";
  private static final String PROCESS_TYPE = "PROCESS_TYPE";

  private InternalProcessDataDomainEventModel internalProcessDataDomainEventModel;

  @InjectMocks
  private FbbL4RowListener fbbL4RowListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private FbbConsignmentService fbbConsignmentService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;


  @BeforeEach
  public void setUp(){
    MockitoAnnotations.initMocks(this);
    internalProcessDataDomainEventModel = new InternalProcessDataDomainEventModel();
    internalProcessDataDomainEventModel.setInternalProcessRequestId(ID);
    internalProcessDataDomainEventModel.setProcessType(PROCESS_TYPE);
    internalProcessDataDomainEventModel.setStoreId(STORE_ID);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(fbbConsignmentService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(MESSAGE, InternalProcessDataDomainEventModel.class))
      .thenReturn(internalProcessDataDomainEventModel);
    fbbL4RowListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, InternalProcessDataDomainEventModel.class);
    Mockito.verify(fbbConsignmentService)
      .processFbbL4RowEvent(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(kafkaTopicProperties).getFbbL5UpdateRows();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(MESSAGE, InternalProcessDataDomainEventModel.class))
      .thenReturn(internalProcessDataDomainEventModel);
    doThrow(ApplicationRuntimeException.class).when(fbbConsignmentService)
      .processFbbL4RowEvent(Mockito.anyString(), Mockito.anyString());
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> fbbL4RowListener.onDomainEventConsumed(MESSAGE));
    } finally {
      Mockito.verify(objectMapper).readValue(MESSAGE, InternalProcessDataDomainEventModel.class);
      Mockito.verify(fbbConsignmentService)
        .processFbbL4RowEvent(Mockito.anyString(),Mockito.anyString());
      Mockito.verify(kafkaTopicProperties).getFbbL5UpdateRows();
    }
  }
}
