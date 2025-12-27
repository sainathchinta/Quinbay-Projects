package com.gdn.x.product.service.event.listener;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.businesspartner.domain.event.config.PickupPointDomainEventName;
import com.gdn.x.product.domain.event.model.PickupPointVOEventModel;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.ObjectConverterService;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.businesspartner.pickuppoint.update.all.listener"
    + ".enabled", havingValue = "true")
public class PickupPointUpdateAllEventListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @KafkaListener(topics = PickupPointDomainEventName.UPDATE_ALL, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Received event with payload for #PickupPointUpdateAllEventListener : {}", message);
    try {
      PickupPointVOEventModel pickupPointChange = this.objectMapper.readValue(message, PickupPointVOEventModel.class);
      BusinessPartnerPickupPoint businessPartnerPickupPoint = businessPartnerPickupPointService
          .getBusinessPartnerPickupPoint(pickupPointChange.getStoreId(), pickupPointChange.getBusinessPartnerCode(),
              pickupPointChange.getCode());
      businessPartnerPickupPointService.saveBusinessPartnerPickupPoint(objectConverterService
          .convertToBusinessPartnerPickupPointFromPickupPointChange(pickupPointChange,
              pickupPointChange.getBusinessPartnerCode(),
              Optional.ofNullable(businessPartnerPickupPoint).orElse(new BusinessPartnerPickupPoint())));
    } catch (Exception ex) {
      log.error("Error while listening PickupPointUpdateAllEventListener from X-BP, payload:{}, error - ", message, ex);
    }
  }
}
