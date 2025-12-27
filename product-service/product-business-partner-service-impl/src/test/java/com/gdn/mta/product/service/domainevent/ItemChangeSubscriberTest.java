package com.gdn.mta.product.service.domainevent;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pbp.service.eventstore.EventStoreKafkaMessageProcessor;
import com.gdn.x.product.domain.event.model.ItemChange;

public class ItemChangeSubscriberTest {

  @Mock
  private EventStoreKafkaMessageProcessor<ItemChange> eventStoreKafkaMessageProcessor;

  @InjectMocks
  private ItemChangeSubscriber subscriber;

  @Mock
  private ObjectMapper objectMapper;

  private ItemChange eventModel;
  private ObjectMapper mapper;

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    eventModel = new ItemChange();
    mapper = new ObjectMapper();
  }

  @Test
  public void onDomainEventConsumed_Test() throws Exception {
    String message = mapper.writeValueAsString(this.eventModel);
    Mockito.when(objectMapper.readValue(message, ItemChange.class))
        .thenReturn(this.eventModel);
    this.subscriber.onDomainEventConsumed(message);
//    Mockito.verify(eventStoreKafkaMessageProcessor).process(eventModel,
//        ProductDomainEventName.ITEM_CHANGE_EVENT_NAME);
  }
}
