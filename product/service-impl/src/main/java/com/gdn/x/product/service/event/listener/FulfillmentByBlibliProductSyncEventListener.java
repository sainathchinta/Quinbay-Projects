package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductItemSyncEvent;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

/**
 * Fulfillment By Blibli (FBB) sync event to update linked partners for copied SKU
 *
 * @author anand
 * @since nov, 2019
 */
@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.fbb.item.sync.event.listener.enabled", havingValue = "true")
public class FulfillmentByBlibliProductSyncEventListener {

  @Autowired
  private ProductAndItemSolrIndexerService indexerService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = ProductDomainEventName.FBB_ITEM_SYNC_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("product sync event with payload : {}", message);
    try {
      ProductItemSyncEvent productItemSyncEvent = this.objectMapper.readValue(message,
        ProductItemSyncEvent.class);
      indexerService.updateItemSyncStatusForFulfillmentByBlibli(productItemSyncEvent.getStoreId(), productItemSyncEvent.getItemSku(),
        productItemSyncEvent.getLinkedPartnerCode());
    } catch(Exception ex) {
      log.error("Error while processing product sync event {} from PBP ", message, ex);
    }
  }

}
