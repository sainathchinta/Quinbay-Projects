package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.service.QRCodeGenerateService;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class GenerateQRCodeRowListener {

  @Autowired
  private ObjectMapper objectMapper;
  @Autowired
  private QRCodeGenerateService qrCodeGenerateService;
  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getGenerateQrCodeRow()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Received event for topic : {} with message : {}",
        kafkaTopicProperties.getGenerateQrCodeRow(), message);
    BulkUpdateEventModel eventModel = objectMapper.readValue(message, BulkUpdateEventModel.class);
    try {
      qrCodeGenerateService.generateQRCode(eventModel);
    } catch (Exception e) {
      log.error("Error while consuming event : {}, message : {} ",
          kafkaTopicProperties.getGenerateQrCodeRow(), message, e);
    }
  }
}
