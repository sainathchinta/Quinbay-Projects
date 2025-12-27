package com.gdn.x.product.service.event.listener;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.warehouse.itemmaster.streamingmodel.WarehouseMasterSKUAllEvent;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.properties.KafkaTopicProperties;


import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "wms.master.data.item.event.listener.enabled", havingValue = "true")
public class DimensionUpdateListener {

  @Autowired
  private ItemService itemService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @KafkaListener(topics = "#{kafkaTopicProperties.getWmsMasterDataItemEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    try {

      log.info("Consume event : {},  for dimension update, message : {} ",
          kafkaTopicProperties.getWmsMasterDataItemEvent(), message);
      WarehouseMasterSKUAllEvent warehouseMasterSKUEvent =
          this.objectMapper.readValue(message, WarehouseMasterSKUAllEvent.class);
      itemService.updateItemDimensionsAndUpcCode(warehouseMasterSKUEvent.getItemCode(), warehouseMasterSKUEvent.getLength(),
          warehouseMasterSKUEvent.getWidth(), warehouseMasterSKUEvent.getWeight(), warehouseMasterSKUEvent.getHeight(), warehouseMasterSKUEvent.getUpcCodes());
    } catch (Exception e) {
      log.error("Exception caught while updating dimensions in x-product : message : {} ", message);
    }
  }
}
