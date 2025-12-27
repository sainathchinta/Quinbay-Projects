package com.gdn.x.productcategorybase.domainevent;

import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.inventory.dto.WarehouseMasterSKUEvent;
import com.gdn.inventory.enums.KafkaEventNames;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;

/**
 * Created by parvej on 13/04/20.
 */
@Service
public class SaveDGLevelAndDimensionsListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(SaveDGLevelAndDimensionsListener.class);

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = KafkaEventNames.WAREHOUSE_MASTER_SKU_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    LOGGER.info("Message received, from topic {}, message : {}", KafkaEventNames.WAREHOUSE_MASTER_SKU_EVENT, message);
    try {
      WarehouseMasterSKUEvent warehouseMasterSKUEvent = objectMapper.readValue(message, WarehouseMasterSKUEvent.class);
      if (Objects.nonNull(warehouseMasterSKUEvent)) {
        this.productServiceWrapper.updateProductDimensions(warehouseMasterSKUEvent);
      }
    } catch (Exception ex) {
      LOGGER.error("error while listening '{}', error is : ", KafkaEventNames.WAREHOUSE_MASTER_SKU_EVENT, ex);
    }
  }
}
