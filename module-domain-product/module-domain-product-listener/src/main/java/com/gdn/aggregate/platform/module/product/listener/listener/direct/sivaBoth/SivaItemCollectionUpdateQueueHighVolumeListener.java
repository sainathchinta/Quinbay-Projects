package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.aggregate.platform.module.product.listener.constants.Enabler;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;

import java.util.List;

@Component
@ConditionalOnProperty(name = Enabler.FLOW_SIVA_ITEM_COMBINED_UPSERT_ENABLED_HIGH_VOLUME)
public class SivaItemCollectionUpdateQueueHighVolumeListener {

  @Autowired
  private ListenerService listenerService;

  @KafkaListener(topics = Topics.NEW_SIVA_ITEM_COLLECTION_UPDATE_EVENT_HIGH_VOLUME, 
                 groupId = Topics.GROUP_ID_SIVA_BOTH_BY_ITEM_HIGH_VOLUME, 
                 containerFactory = "batchDeduplicationContainerFactory")
  public void processHighVolumeItemDataUpdateEvent(List<ConsumerRecord<String, String>> records) throws JsonProcessingException {
    listenerService.processSivaItemUpdateQueueEventBatch(records);
  }
}
