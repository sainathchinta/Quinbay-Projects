package com.gdn.x.productcategorybase.domainevent;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.domain.event.model.CompressedVideoUpdateEventModel;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@Slf4j
public class CompressedVideoUpdateListener {
  private final KafkaTopicProperties kafkaTopicProperties;
  private final ObjectMapper objectMapper;
  private final ProductServiceWrapper productServiceWrapper;

  @Autowired
  public CompressedVideoUpdateListener(KafkaTopicProperties kafkaTopicProperties,
    ObjectMapper objectMapper, ProductServiceWrapper productServiceWrapper) {
    this.kafkaTopicProperties = kafkaTopicProperties;
    this.objectMapper = objectMapper;
    this.productServiceWrapper = productServiceWrapper;
  }

  @KafkaListener(topics = "#{kafkaTopicProperties.getCompressedVideoUpdateEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws JsonProcessingException {
    log.info("Received Video update Message, from topic {}, message : {} ",
      kafkaTopicProperties.getCompressedVideoUpdateEvent(), message);
    CompressedVideoUpdateEventModel compressedVideoUpdateEventModel =
      objectMapper.readValue(message, CompressedVideoUpdateEventModel.class);
    Optional.ofNullable(compressedVideoUpdateEventModel).ifPresentOrElse(event -> {
      try {
        productServiceWrapper.processCompressedUpdatedVideo(event);
      } catch (Exception e) {
        log.error("Error While processing video compression final url update for message : {} ",
          event, e);
      }
    }, () -> {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ErrorMessage.INVALID_EVENT_PAYLOAD_RECEIVED.getMessage());
    });
  }
}

