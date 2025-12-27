package com.gdn.x.product.service.event.listener;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ItemSolrReindexEvent;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.service.api.ReindexService;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.product.l4.solr.reindex.listener.enabled",
                       havingValue = "true")
public class ItemL4SolrReindexListener {

  @Autowired
  private ReindexService reindexService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = ProductDomainEventName.L4_SOLR_REINDEX_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Received event to reindex itemSkus : {} ", message);
    try {
      ItemSolrReindexEvent itemSolrReindexEvent =
          this.objectMapper.readValue(message, ItemSolrReindexEvent.class);
      if (CollectionUtils.isNotEmpty(itemSolrReindexEvent.getItemSkus())) {
        if (ReindexType.ITEM_REINDEX.name().equalsIgnoreCase(itemSolrReindexEvent.getStatus())) {
          reindexService.reindexItems(itemSolrReindexEvent.getItemSkus());
        } else if (ReindexType.OFFLINE_ITEM_REINDEX.name().equalsIgnoreCase(itemSolrReindexEvent.getStatus())) {
          reindexService.reindexOfflineItems(itemSolrReindexEvent.getItemSkus());
        } else if (ReindexType.DELETE_FROM_SOLR.name().equalsIgnoreCase(itemSolrReindexEvent.getStatus())) {
          reindexService.deleteProductAndItemsFromSolr(itemSolrReindexEvent.getItemSkus());
        }
      }
    } catch (Exception e) {
      log.error("Error on process of ItemL4SolrReindexListener, with payload : {}, error - ",
          message, e);
    }
  }
}
