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

import java.util.List;

@Component
@ConditionalOnProperty(name = Enabler.FLOW_SIVA_ITEM_BATCH_DEDUPLICATION_ENABLED, havingValue = "true")
public class SivaItemCollectionBatchDeduplicationListener {

  @Autowired
  private ListenerService listenerService;

  @KafkaListener(topics = Topics.NEW_SIVA_ITEM_COLLECTION_UPDATE_EVENT, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_ITEM,
    containerFactory = "batchDeduplicationContainerFactory")
  public void processNewItemDataUpdateEventBatch(List<ConsumerRecord<String, String>> records)
    throws JsonProcessingException {
    listenerService.processSivaItemUpdateQueueEventBatch(records);
  }
} 