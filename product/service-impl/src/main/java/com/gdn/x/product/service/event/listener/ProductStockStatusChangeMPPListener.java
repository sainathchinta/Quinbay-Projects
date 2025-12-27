package com.gdn.x.product.service.event.listener;


import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.inventory.domain.event.config.DomainEventName;
import com.gdn.x.inventory.domain.event.model.StockStatusLevel3Event;
import com.gdn.x.product.service.api.ProductL3SolrService;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.inventory.stock.status.l3.level.multiple.pickuppoint"
    + ".event.listener.enabled", havingValue = "true")
public class ProductStockStatusChangeMPPListener {

  @Autowired
  private ProductL3SolrService productL3SolrService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.STOCK_STATUS_LEVEL3_MULTIPLE_PICKUP_POINT_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Consume product stock change event with message : {}", message);
    try {
      StockStatusLevel3Event stockStatusLevel3Event = this.objectMapper.readValue(message,
        StockStatusLevel3Event.class);
      if (validateEvent(stockStatusLevel3Event)) {
        productL3SolrService
          .updateStockStatusInL3Solr(stockStatusLevel3Event.getWebProductSku(),
            stockStatusLevel3Event.getStatus(), stockStatusLevel3Event.getWebMerchantCode());
      }
    } catch (Exception e) {
      log.error(
        "Error processing event ProductStockStatusChangeListener with payload :{} , error " + "- ",
        message, e);
    }
  }

  private boolean validateEvent(StockStatusLevel3Event stockStatusLevel3Event) {
    if (StringUtils.isNotBlank(stockStatusLevel3Event.getWebProductSku()) && StringUtils
        .isNotBlank(stockStatusLevel3Event.getStatus())) {
      return true;
    }
    return false;
  }
}
