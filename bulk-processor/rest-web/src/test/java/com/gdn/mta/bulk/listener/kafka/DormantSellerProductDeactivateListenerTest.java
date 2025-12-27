package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.DormantSellerProductDeactivation;
import com.gdn.mta.bulk.service.DormantSellerService;
import com.gdn.partners.bulk.util.Constant;


public class DormantSellerProductDeactivateListenerTest {

  @InjectMocks
  DormantSellerProductDeactivateListener dormantSellerProductDeactivateListener;

  @Mock
  DormantSellerService dormantSellerService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void initialize() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(dormantSellerService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void testOnDomainEventConsumedTest() throws Exception {
    DormantSellerProductDeactivation dormantSellerProductDeactivation = new DormantSellerProductDeactivation();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, DormantSellerProductDeactivation.class))
        .thenReturn(dormantSellerProductDeactivation);
    dormantSellerProductDeactivateListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(dormantSellerService).processSellerDeactivate(Mockito.any(), Mockito.any());
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, DormantSellerProductDeactivation.class);
    Mockito.verify(kafkaTopicProperties).getDormantSellerProductDeactivateEvent();
  }

  @Test
  public void testOnDomainEventConsumedExceptionTest() throws Exception {
    DormantSellerProductDeactivation dormantSellerProductDeactivation = new DormantSellerProductDeactivation();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, DormantSellerProductDeactivation.class))
        .thenReturn(dormantSellerProductDeactivation);
    Mockito.doThrow(new ApplicationException()).when(dormantSellerService)
        .processSellerDeactivate(Mockito.anyString(), Mockito.anyString());
    dormantSellerProductDeactivateListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(dormantSellerService).processSellerDeactivate(Mockito.any(), Mockito.any());
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, DormantSellerProductDeactivation.class);
    Mockito.verify(kafkaTopicProperties).getDormantSellerProductDeactivateEvent();
  }
}