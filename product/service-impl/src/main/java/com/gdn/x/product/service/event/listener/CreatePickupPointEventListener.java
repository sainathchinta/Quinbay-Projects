package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.businesspartner.domain.event.config.PickupPointDomainEventName;
import com.gdn.x.product.domain.event.model.BusinessPartnerAddPickupPointEventModel;
import com.gdn.x.product.domain.event.model.PickupPointVOEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.PickupPoint;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PickupPointService;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.businesspartner.pickuppoint.add.listener.enabled",
                       havingValue = "true")
public class CreatePickupPointEventListener {

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @KafkaListener(topics = PickupPointDomainEventName.ADD, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Received event with payload for #CreatePickupPointEventListener : {}", message);
    try {
      BusinessPartnerAddPickupPointEventModel businessPartnerAddPickupPoint =
        this.objectMapper.readValue(message, BusinessPartnerAddPickupPointEventModel.class);
      for (PickupPointVOEventModel pickupPointVO : businessPartnerAddPickupPoint.getAddPickupPoints()) {
        PickupPoint pickupPoint = new PickupPoint();
        pickupPoint.setPickupPointCode(pickupPointVO.getCode());
        pickupPoint.setCncActivated(pickupPointVO.isCncActivated());
        pickupPointService.upsertPickupPoint(Constants.DEFAULT_STORE_ID, pickupPoint,
          Constants.DEFAULT_USERNAME);
        businessPartnerPickupPointService.saveBusinessPartnerPickupPoint(objectConverterService
            .convertToBusinessPartnerPickupPoint(pickupPointVO,
                businessPartnerAddPickupPoint.getBusinessPartnerCode()));
      }
    } catch (Exception ex) {
      log.error("Error while listening CreatePickupPoint from X-BP, payload:{}, error - ",
        message, ex);
    }
  }
}
