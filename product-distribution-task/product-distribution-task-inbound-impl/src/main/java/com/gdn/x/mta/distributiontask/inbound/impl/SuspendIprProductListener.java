package com.gdn.x.mta.distributiontask.inbound.impl;

import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.api.IprWrapperService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
public class SuspendIprProductListener {

  private final KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;
  private final IprWrapperService iprWrapperService;

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer.getSuspendIprProductEvent()}",
    autoStartup = "#{kafkaTopicPropertiesConsumer.isAutoStartup()}")
  public void onDomainEventConsumed(String productSku) throws Exception {
    log.info("Consumed event : {}, message : {} ",
      kafkaTopicPropertiesConsumer.getSuspendIprProductEvent(), productSku);
    try {
      iprWrapperService.suspendEvidenceRequestedProduct(productSku);
    } catch (Exception ex) {
      log.error("Error while processing event : {} , message : {} ",
        kafkaTopicPropertiesConsumer.getSuspendIprProductEvent(), productSku, ex);
    }
  }
}
