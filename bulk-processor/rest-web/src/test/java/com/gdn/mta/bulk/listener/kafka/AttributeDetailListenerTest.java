package com.gdn.mta.bulk.listener.kafka;


import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.entity.KafkaEventLog;
import com.gdn.mta.bulk.repository.KafkaEventLogRepository;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;

public class AttributeDetailListenerTest {

  private static final String ATTRIBUTE_ID = "ATTRIBUTE_ID";
  private static final String ATTRIBUTE_CODE = "ATTRIBUTE_CODE";
  private static final String ATTRIBUTE_TYPE = "DEFINING_ATTRIBUTE";
  private static final String DESCRIPTION = "DESCRIPTION";

  private AttributeDomainEventModel attributeDomainEventModel;

  @InjectMocks
  private AttributeDetailListener attributeDetailListener;

  @Mock
  private KafkaEventLogRepository kafkaEventLogRepository;

  @Mock
  private ObjectMapper objectMapper;

  @Captor
  private ArgumentCaptor<KafkaEventLog> kafkaEventLogArgumentCaptor;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    attributeDomainEventModel = new AttributeDomainEventModel();
    attributeDomainEventModel.setId(ATTRIBUTE_ID);
    attributeDomainEventModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeDomainEventModel.setAttributeType(ATTRIBUTE_TYPE);
    attributeDomainEventModel.setDescription(DESCRIPTION.getBytes());
    attributeDomainEventModel.setSkuValue(true);
    attributeDomainEventModel.setNewAttribute(false);
    attributeDomainEventModel.setValueUpdate(true);

    when(kafkaEventLogRepository.save(kafkaEventLogArgumentCaptor.capture())).thenReturn(new KafkaEventLog());
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(kafkaEventLogRepository);
    verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    when(objectMapper.readValue(ATTRIBUTE_ID, AttributeDomainEventModel.class)).thenReturn(attributeDomainEventModel);
    attributeDetailListener.onDomainEventConsumed(ATTRIBUTE_ID);
    Assertions.assertEquals(new ObjectMapper().writeValueAsString(attributeDomainEventModel),
        kafkaEventLogArgumentCaptor.getValue().getEventMessage());
    Assertions.assertEquals(DomainEventName.MASTER_ATTRIBUTE_INFO_EVENT, kafkaEventLogArgumentCaptor.getValue().getTopicName());
    verify(kafkaEventLogRepository).save(kafkaEventLogArgumentCaptor.getValue());
    verify(objectMapper).readValue(ATTRIBUTE_ID, AttributeDomainEventModel.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    when(objectMapper.readValue(ATTRIBUTE_ID, AttributeDomainEventModel.class)).thenReturn(attributeDomainEventModel);
    doThrow(RuntimeException.class).when(kafkaEventLogRepository).save(kafkaEventLogArgumentCaptor.capture());
    attributeDetailListener.onDomainEventConsumed(ATTRIBUTE_ID);
    verify(kafkaEventLogRepository).save(kafkaEventLogArgumentCaptor.getValue());
    verify(objectMapper).readValue(ATTRIBUTE_ID, AttributeDomainEventModel.class);
  }

}