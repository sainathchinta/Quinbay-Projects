package com.gdn.x.mta.distributiontask.inbound.impl;

import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.api.SystemParameterConfigService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
public class SystemParameterCacheEvictEventListener {

  private final KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;
  private final SystemParameterConfigService systemParameterConfigService;

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer"
      + ".getSystemParameterCaffeineCacheEvictEvent()}", groupId = "#{ T(java.util.UUID)"
      + ".randomUUID().toString() }", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consume event {} with message {} ",
        kafkaTopicPropertiesConsumer.getSystemParameterCaffeineCacheEvictEvent(), message);
    try {
      String storeId = message;
      systemParameterConfigService.evictSystemParametersCache(storeId);
    } catch (Exception e) {
      log.error("Error while consuming the event : {} and the payload : {} ",
          kafkaTopicPropertiesConsumer.getSystemParameterCaffeineCacheEvictEvent(), message, e);
    }
  }
}

