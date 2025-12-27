package com.gdn.mta.product.service.domainevent;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pbp.service.eventstore.EventStoreKafkaMessageProcessor;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.newrelic.api.agent.Trace;

@Service
public class ItemChangeSubscriber {

  @Autowired
  private EventStoreKafkaMessageProcessor<ItemChange> eventStoreKafkaMessageProcessor;

  @Autowired
  private ObjectMapper objectMapper;

  private static final Logger LOGGER = LoggerFactory
      .getLogger(ItemChangeSubscriber.class);

  @Trace(dispatcher=true)
  @KafkaListener(topics = ProductDomainEventName.ITEM_CHANGE_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    ItemChange eventModel = objectMapper.readValue(message, ItemChange.class);
    eventStoreKafkaMessageProcessor.process(eventModel,
        ProductDomainEventName.ITEM_CHANGE_EVENT_NAME);
  }
}
