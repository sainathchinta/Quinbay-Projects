package com.gdn.partners.product.analytics.service.impl.domainEvent;

import java.io.IOException;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.service.SellerDataWrapperService;
import com.gdn.partners.product.analytics.web.model.config.DomainEventName;
import lombok.extern.slf4j.Slf4j;
import model.OxfordMerchantEvent;

@Service
@Slf4j
public class OxfordMerchantEventListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private SellerDataWrapperService sellerDataWrapperService;

  @KafkaListener(topics = DomainEventName.OXFORD_UPDATE_MERCHANT_EVENT, autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws IOException {
    log.info("Consume event {} with message {} ", DomainEventName.OXFORD_UPDATE_MERCHANT_EVENT, message);
    OxfordMerchantEvent oxfordMerchantEvent = objectMapper.readValue(message, OxfordMerchantEvent.class);
    if (Objects.nonNull(oxfordMerchantEvent)) {
      sellerDataWrapperService
          .updateOfficialStoreFlagForASeller(oxfordMerchantEvent.getCode(), oxfordMerchantEvent.isOfficialStore());
    }
    log.info("Event {} processed successfully {} ", DomainEventName.OXFORD_UPDATE_MERCHANT_EVENT, message);
  }
}
