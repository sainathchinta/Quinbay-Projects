package com.gdn.partners.pbp.service.listener;

import java.util.UUID;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.modal.ImageResizeEvent;
import com.gdn.mta.product.service.ImageProcessorService;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;


@Service
@Slf4j
public class ImageResizeKafkaSubscriberBean {

  public static final String DEFAULT_USERNAME = "SYSTEM";

  @Autowired
  private ImageProcessorService imageProcessorService;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${resize.image.channel.id.default}")
  private String channelId;

  @Value("${resize.image.authenticator.id.default}")
  private String authenticatorKey;

  @Value("${resize.image.client.id.default}")
  private String clientId;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getImageResizeEventNoPriority()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    ImageResizeEvent imageResizeEvent = objectMapper.readValue(message, ImageResizeEvent.class);
    log.info("Message received, from topic {}, message :{}", kafkaTopicProperties.getImageResizeEventNoPriority(),
        imageResizeEvent);
    try {
      setMandatoryParameters(imageResizeEvent.getStoreId());
      imageProcessorService.resizeImage(imageResizeEvent.getStoreId(), imageResizeEvent.getProductCode(), 0);
    } catch (Exception e) {
      log.error("Exception caught while processing image resize event , product code:{} ",
          imageResizeEvent.getProductCode(), e);
    }
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getImageResizeEventPriority1()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedForPrioritySeller1(String message) throws Exception {
    ImageResizeEvent imageResizeEvent = objectMapper.readValue(message, ImageResizeEvent.class);
    log.info("Message received, from topic {}, message :{}", kafkaTopicProperties.getImageResizeEventPriority1(), imageResizeEvent);
    try {
      setMandatoryParameters(imageResizeEvent.getStoreId());
      imageProcessorService.resizeImage(imageResizeEvent.getStoreId(), imageResizeEvent.getProductCode(), 1);
    } catch (Exception e) {
      log.error("Exception caught while processing image resize event , product code:{} ",
          imageResizeEvent.getProductCode(), e);
    }
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getImageResizeEventPriority2()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedForPrioritySeller2(String message) throws Exception {
    ImageResizeEvent imageResizeEvent = objectMapper.readValue(message, ImageResizeEvent.class);
    log.info("Message received, from topic {}, message :{}", kafkaTopicProperties.getImageResizeEventPriority2(), imageResizeEvent);
    try {
      setMandatoryParameters(imageResizeEvent.getStoreId());
      imageProcessorService.resizeImage(imageResizeEvent.getStoreId(), imageResizeEvent.getProductCode(), 2);
    } catch (Exception e) {
      log.error("Exception caught while processing image resize event , product code:{} ",
          imageResizeEvent.getProductCode(), e);
    }
  }

  private void setMandatoryParameters(String storeId) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, this.authenticatorKey);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, this.clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, this.channelId);
  }
}
