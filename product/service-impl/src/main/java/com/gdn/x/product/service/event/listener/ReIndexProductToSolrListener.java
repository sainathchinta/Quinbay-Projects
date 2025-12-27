package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.DeltaReindexToSolrEventModel;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.google.common.collect.ImmutableSet;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "reindex.product.to.solr.enabled", havingValue = "true")
public class ReIndexProductToSolrListener {

  @Autowired
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${l3.solr.delta.reindexing.atomic.update.enabled}")
  private boolean l3DeltaReindexingAtomicUpdateEnabled;

  @KafkaListener(topics = ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Listened to Add to solr event : {} , message : {}", ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR, message);
    try {
      DeltaReindexToSolrEventModel request =
          this.objectMapper.readValue(message, DeltaReindexToSolrEventModel.class);
      if (request.isRejected()) {
        productAndItemSolrIndexerService.deleteProductsFromSolrAfterPostLiveRejection(ImmutableSet.of(request.getProductSku()));
      } else {
        productAndItemSolrIndexerService.applyProduct(objectConverterService.convertToProduct(request.getProduct()), l3DeltaReindexingAtomicUpdateEnabled);
      }
      log.info("updated in solr by event : {} for productSku : {} ", ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR, request.getProductSku());
    } catch (Exception e) {
      log.error("Exception caught while updating to solr by event : {} , payload : {} ", ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR, message, e);
    }
  }
}
