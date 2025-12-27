package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.aggregate.platform.module.product.listener.constants.Enabler;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;

@Component
@ConditionalOnProperty(name = Enabler.FLOW_SIVA_ITEM_COMBINED_UPSERT_ENABLED)
public class SivaItemCollectionUpdateQueueListener {

  private static final Logger log = LoggerFactory.getLogger(SivaItemCollectionUpdateQueueListener.class);
  @Autowired
  private ListenerService listenerService;

  @KafkaListener(topics = Topics.NEW_SIVA_ITEM_COLLECTION_UPDATE_EVENT, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_ITEM)
  public void processNewItemDataUpdateEvent(ConsumerRecord<String, String> record) throws JsonProcessingException {
    log.info("Consumed {} event, message {} ", Topics.NEW_SIVA_ITEM_COLLECTION_UPDATE_EVENT, record);
    listenerService.processSivaItemUpdateQueueEvent(record);
  }
}
