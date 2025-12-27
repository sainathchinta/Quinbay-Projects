package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalProcessDataDomainEventModel;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;

public class StoreCopyCreateProductListenerTest {

  private static final String PARENT_PRODUCT = "parentProduct";
  private static final String INTERNAL_PROCESS_REQUEST_ID = "internalProcessRequestId";
  private ObjectMapper mapper = new ObjectMapper();

  @InjectMocks
  private StoreCopyCreateProductListener listener;

  @Mock
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private InternalProcessDataDomainEventModel storeCopyProductCreationDetails;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    storeCopyProductCreationDetails = InternalProcessDataDomainEventModel.builder().parentCode(PARENT_PRODUCT)
        .internalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(internalProcessServiceWrapper);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(storeCopyProductCreationDetails),
        InternalProcessDataDomainEventModel.class)).thenReturn(storeCopyProductCreationDetails);
    Mockito.doNothing().when(internalProcessServiceWrapper).processEvent(storeCopyProductCreationDetails);
    this.listener.onDomainEventConsumed(mapper.writeValueAsString(storeCopyProductCreationDetails));
    Mockito.verify(internalProcessServiceWrapper).processEvent(storeCopyProductCreationDetails);
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(storeCopyProductCreationDetails),
        InternalProcessDataDomainEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getStoreCopyProductCreationDetails();
  }

  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(storeCopyProductCreationDetails),
        InternalProcessDataDomainEventModel.class)).thenReturn(storeCopyProductCreationDetails);
    Mockito.doThrow(Exception.class).when(internalProcessServiceWrapper).processEvent(storeCopyProductCreationDetails);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> this.listener.onDomainEventConsumed(
          mapper.writeValueAsString(storeCopyProductCreationDetails)));
    } finally {
      Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(storeCopyProductCreationDetails),
          InternalProcessDataDomainEventModel.class);
      Mockito.verify(internalProcessServiceWrapper).processEvent(storeCopyProductCreationDetails);
      Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getStoreCopyProductCreationDetails();
    }
  }
}
