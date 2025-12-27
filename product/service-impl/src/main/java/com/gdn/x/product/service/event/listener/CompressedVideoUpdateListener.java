package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.CompressedVideoUpdateEventModel;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.properties.KafkaTopicProperties;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
@ConditionalOnProperty(value = "com.gdn.video.processor.compressed.video.update.listener.enabled"
    , havingValue = "true")
public class CompressedVideoUpdateListener {
  private final KafkaTopicProperties kafkaTopicProperties;
  private final ObjectMapper objectMapper;
  private final ProductService productServiceWrapper;

  @KafkaListener(topics = "#{kafkaTopicProperties.getCompressedVideoUpdateEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws JsonProcessingException {
    log.info("Received Video update Message, from topic {}, message : {} ",
      kafkaTopicProperties.getCompressedVideoUpdateEvent(), message);
    try {
      CompressedVideoUpdateEventModel compressedVideoUpdateEventModel =
        objectMapper.readValue(message, CompressedVideoUpdateEventModel.class);
      productServiceWrapper.updateFinalVideoData(compressedVideoUpdateEventModel);
    } catch (Exception e) {
      log.error(
        "Error processing video compression update for message: {}. Error type: {}, Error message: {}",
        message, e.getClass().getName(), e.getMessage(), e);
    }
  }
}
