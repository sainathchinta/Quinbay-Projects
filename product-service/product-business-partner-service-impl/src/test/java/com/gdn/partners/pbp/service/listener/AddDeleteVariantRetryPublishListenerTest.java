package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.AddDeleteVariantRetryPublishEventModel;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.mta.product.service.domainevent.publisher.AddDeleteVariantRetryPublishService;


public class AddDeleteVariantRetryPublishListenerTest {

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private AddDeleteVariantRetryPublishService addDeleteVariantRetryPublishService;

  @InjectMocks
  private AddDeleteVariantRetryPublishListener listener;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties,objectMapper,addDeleteVariantRetryPublishService);
  }

  @Test
  public void testOnDomainEventConsumed() throws Exception {
    ObjectMapper objectMapper1 = new ObjectMapper();
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel = new AddDeleteVariantRetryPublishEventModel();
    String message = objectMapper1.writeValueAsString(addDeleteVariantRetryPublishEventModel);
    Mockito.when(objectMapper.readValue(message, AddDeleteVariantRetryPublishEventModel.class))
        .thenReturn(addDeleteVariantRetryPublishEventModel);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("testTopic");
    listener.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, AddDeleteVariantRetryPublishEventModel.class);
    verify(addDeleteVariantRetryPublishService).processAddDeleteVariantRetryEvents(addDeleteVariantRetryPublishEventModel);
    Mockito.verify(kafkaTopicProperties).getAddDeleteVariantRetryPublishEvent();
  }

}