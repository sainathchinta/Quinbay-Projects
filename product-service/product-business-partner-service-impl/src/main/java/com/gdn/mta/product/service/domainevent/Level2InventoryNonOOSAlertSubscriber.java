package com.gdn.mta.product.service.domainevent;

import java.util.Arrays;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.service.ProductStockAlertService;
import com.gdn.partners.pbp.service.eventstore.EventStoreKafkaMessageProcessor;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorService;
import com.gdn.x.inventory.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.Level2InventoryNonOosEvent;
import com.newrelic.api.agent.Trace;

@Service
public class Level2InventoryNonOOSAlertSubscriber {

  private static final Logger LOGGER =
      LoggerFactory.getLogger(Level2InventoryNonOOSAlertSubscriber.class);
  
  @Autowired
  private ProductStockAlertService productStockAlertService;

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private ProductLevel3AggregatorService productLevel3AggregatorService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private EventStoreKafkaMessageProcessor<Level2InventoryNonOosEvent> eventStoreKafkaMessageProcessor;

  private String itemName;

  @Trace(dispatcher=true)
  @KafkaListener(topics = DomainEventName.STOCK_NON_OOS_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    Level2InventoryNonOosEvent messageEvent = objectMapper.readValue(message, Level2InventoryNonOosEvent.class);
    if (messageEvent.isCncActivated()) {
      LOGGER.info("Level2InventoryNonOosEvent skip processing offline item non OOS event : {}", messageEvent);
      return;
    }
    try {
      if (this.validateMessageEvent(messageEvent)) {
//          this.productStockAlertService.updateNonOOS(messageEvent, itemName, 0);
//          this.productLevel3AggregatorService.updateNonOOS(messageEvent, 0);
          this.eventStoreKafkaMessageProcessor.process(messageEvent,
              DomainEventName.STOCK_NON_OOS_EVENT_NAME);
      }
    } catch (Exception e) {
      LOGGER.error("Level2InventoryNonOosEvent stockEventConsumed ERROR with topic STOCK_NON_OOS_EVENT_NAME : {}, Exception : {}", messageEvent, e);
    }
  }

  private boolean validateMessageEvent(Level2InventoryNonOosEvent messageEvent)
      throws Exception {
    LOGGER.info("Received message stockEventConsumed from topic : {} with message : {}", DomainEventName.STOCK_NON_OOS_EVENT_NAME, messageEvent);
    if (StringUtils.isNotEmpty(messageEvent.getLevel2Id())) {
      Map<String, String> itemNames = this.productLevel3Repository.getItemNameByItemSku(Arrays.asList(messageEvent.getLevel2Id()));
      itemName = itemNames != null ? itemNames.get(messageEvent.getLevel2Id()) : null;
      LOGGER.info("validateMessageEvent stockEventConsumed {} : {}", messageEvent.getLevel2Id(), itemName);
      if (StringUtils.isNotEmpty(itemName)) {
        MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, messageEvent.getStoreId());
        MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Level2InventoryNonOOSAlertSubscriber.class.getName());
        MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Level2InventoryNonOOSAlertSubscriber.class.getName());
        MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
        MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DomainEventName.STOCK_NON_OOS_EVENT_NAME);
        MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, DomainEventName.STOCK_NON_OOS_EVENT_NAME);
        return true;
      } else {
        LOGGER.error("Level2InventoryNonOosEvent stockEventConsumed ERROR : gdnSku not found with message : {}", messageEvent);
      }
    } else {
      LOGGER.error("Level2InventoryNonOosEvent stockEventConsumed ERROR : gdnSku is empty with message : {}" , messageEvent);
    }
    return false;
  }

}
