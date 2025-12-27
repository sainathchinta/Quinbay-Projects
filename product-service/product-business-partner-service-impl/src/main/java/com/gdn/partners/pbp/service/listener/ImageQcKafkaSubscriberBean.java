package com.gdn.partners.pbp.service.listener;

import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ImageQcResponseDomainEvent;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ImageQcKafkaSubscriberBean {

  public static final String DEFAULT_USERNAME = "SYSTEM";

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ProductService productService;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.IMAGE_QC_PREDICTION_RESPONSE, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    ImageQcResponseDomainEvent imageQcResponseDomainEvent =
        objectMapper.readValue(message, ImageQcResponseDomainEvent.class);
    try {
      if (StringUtils.isBlank(imageQcResponseDomainEvent.getProductCode())) {
        log.error("Product code is empty for " + DomainEventName.IMAGE_QC_PREDICTION_RESPONSE
            + " , request : {} ", imageQcResponseDomainEvent);
        return;
      }
      if (imageQcResponseDomainEvent.getProductCode().contains(Constants.BACKLOG)) {
        productService.publishImageQcBacklogRequestEvent(imageQcResponseDomainEvent);
        return;
      }
    log.info("com.gdn.image.qc.prediction.response : {}", imageQcResponseDomainEvent);
      setMandatoryParameters();
      productServiceWrapper.processImageQcResponse(MDC.get(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER),
          imageQcResponseDomainEvent);
      log.info("Image qc processing is done : {}", imageQcResponseDomainEvent.getProductCode());
    } catch (Exception e) {
      // Can change image resized to true and add product to screening
      log.error("Exception caught while processing event com.gdn.image.qc.prediction.response for productCode : {} ",
          imageQcResponseDomainEvent.getProductCode(), e);
    }
  }

  private void setMandatoryParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, Constants.DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
  }
}