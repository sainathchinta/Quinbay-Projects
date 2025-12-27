package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.entity.KafkaEventLog;
import com.gdn.mta.bulk.repository.KafkaEventLogRepository;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class AttributeDetailListener {

  @Autowired
  private KafkaEventLogRepository kafkaLogRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.MASTER_ATTRIBUTE_INFO_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : Attribute Detail Domain Event consumed {} ", message);
    AttributeDomainEventModel attributeDomainEventModel =
        objectMapper.readValue(message, AttributeDomainEventModel.class);
    try {
      KafkaEventLog kafkaEventLog = getKafkaEventLog(attributeDomainEventModel);
      kafkaLogRepository.save(kafkaEventLog);
    }
    catch (Exception e) {
      log.error("onDomainEventConsumed : Error occured on Attribute Detail Domain Event consumed {} ", message, e);
    }
  }

  private KafkaEventLog getKafkaEventLog(AttributeDomainEventModel message) throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    KafkaEventLog kafkaEventLog =
        new KafkaEventLog(objectMapper.writeValueAsString(message), DomainEventName.MASTER_ATTRIBUTE_INFO_EVENT);
    kafkaEventLog.setStoreId(Constant.STORE_ID);
    return kafkaEventLog;
  }

}