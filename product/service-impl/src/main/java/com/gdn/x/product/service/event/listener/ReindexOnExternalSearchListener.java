package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ExternalSearchReindexToSolrEventModel;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@ConditionalOnProperty(value = "external.search.reindex.listener.enabled", havingValue = "true")
public class ReindexOnExternalSearchListener {

  @Autowired
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = ProductDomainEventName.TRIGGER_REINDEX_WITH_EXTERNAL_SEARCH, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Triggering reindex on external event : {} , message : {}",
        ProductDomainEventName.TRIGGER_REINDEX_WITH_EXTERNAL_SEARCH, message);
    try {
      ExternalSearchReindexToSolrEventModel externalSearchReindexToSolrEventModel =
          this.objectMapper.readValue(message, ExternalSearchReindexToSolrEventModel.class);
      productAndItemSolrIndexerService.reindexOnExternalSearch(externalSearchReindexToSolrEventModel);
      log.info("updated in solr by event : {} for productSku : {} ",
          ProductDomainEventName.TRIGGER_REINDEX_WITH_EXTERNAL_SEARCH,
          externalSearchReindexToSolrEventModel.getProductSku());
    } catch (Exception e) {
      log.error("Exception caught while updating to solr by event : {} , payload : {} ",
          ProductDomainEventName.TRIGGER_REINDEX_WITH_EXTERNAL_SEARCH, message, e);
    }
  }
}
