package com.gdn.x.product.service.event.listener;

import java.util.HashMap;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductL3SolrReindexEvent;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.service.api.ReindexService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.product.product.l3.solr.reindex.listener.enabled",
                       havingValue = "true")
public class ProductL3SolrReindexListener {

  @Autowired
  private ReindexService reindexService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = ProductDomainEventName.PRODUCT_L3_SOLR_REINDEX_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Received event to reindex productSku : {} ", message);
    try {
      ProductL3SolrReindexEvent productL3SolrReindexEvent = this.objectMapper.readValue(message,
        ProductL3SolrReindexEvent.class);
      if (CollectionUtils.isNotEmpty(productL3SolrReindexEvent.getProductSkuList())) {
        if (ProductReindexStatus.REINDEX_PENDING_FLAGS.name().equals(productL3SolrReindexEvent.getStatus())) {
          reindexService.reindexNewFlagValuesToL3Collection(productL3SolrReindexEvent.getProductSkuList());
        } else if (ProductReindexStatus.FULL_REINDEX_PENDING_PRODUCTS.name().equals(productL3SolrReindexEvent.getStatus())) {
          processFullReindexByProductSkus(productL3SolrReindexEvent);
        } else if(ProductReindexStatus.REINDEX_PENDING_L3.name().equals(productL3SolrReindexEvent.getStatus())){
          reindexService.reindexPendingL3Collection(productL3SolrReindexEvent.getProductSkuList());
        } else if (ProductReindexStatus.REINDEX_PENDING_L3_SOLR_DB.name()
          .equals(productL3SolrReindexEvent.getStatus())) {
          reindexService.reindexPendingL3SolrAndDatabase(
            productL3SolrReindexEvent.getProductSkuList(), new HashMap<>());
        }
        else {
          reindexService.reindexNewFieldsToL3Collection(productL3SolrReindexEvent.getProductSkuList());
        }
      }
    } catch (Exception e) {
      log.error("Error on process of ProductL3SolrReindexListener, with payload : {}, error - ",
        message, e);
    }
  }

  private void processFullReindexByProductSkus(ProductL3SolrReindexEvent message) {
    try {
      reindexService.reindexSolrAndClearCacheByProductSkus(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          Constants.DEFAULT_STORE_ID, message.getProductSkuList());
      reindexService
          .updateStatusInL3ReindexCollection(message.getProductSkuList(), ProductReindexStatus.REINDEX_SUCCESS);
    } catch (Exception e) {
      reindexService.updateStatusInL3ReindexCollection(message.getProductSkuList(),
          ProductReindexStatus.FULL_REINDEX_PENDING_PRODUCTS_FAILED);
    }
  }
}
