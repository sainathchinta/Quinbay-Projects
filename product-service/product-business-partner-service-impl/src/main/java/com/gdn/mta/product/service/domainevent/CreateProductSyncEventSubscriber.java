package com.gdn.mta.product.service.domainevent;

import java.util.UUID;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.CreateProductSyncEvent;
import com.gdn.mta.product.service.CreateProductSyncService;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

/**
 * @author anand
 * @since Sep 2019
 */
@Slf4j
@Service
public class CreateProductSyncEventSubscriber {

  @Autowired
  private CreateProductSyncService createProductSyncService;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.CREATE_PRODUCT_SYNC_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    CreateProductSyncEvent event = objectMapper.readValue(message, CreateProductSyncEvent.class);
    try {
      this.initMandatoryRequestParams(event.getStoreId(), event.getUsername());
      createProductSyncService
        .createSyncProduct(event.getStoreId(), event.getUsername(), event.getBusinessPartnerCode(),
          event.getPickupPointCode(), event.getSourceItemSkus());
    } catch(Exception e) {
      log.error("failed to create copy product for {}", event, e);
    } finally {
      MDC.clear();
    }
  }

  private void initMandatoryRequestParams(String storeId, String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, "web");
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, "product-business-partner");
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
  }

}