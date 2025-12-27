package com.gdn.partners.pbp.service.listener;

import java.util.UUID;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.domain.event.config.DomainEventName;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class EditedImageResizeStatusKafkaSubscriberBean {

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.GRAPHIC_EDITED_RESIZE_IMAGE_STATUS_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    BulkImageProcessResponse bulkImageProcessResponse = objectMapper.readValue(message, BulkImageProcessResponse.class);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, bulkImageProcessResponse.getUsername());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, bulkImageProcessResponse.getStoreId());
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    log.info("Message received, from topic {}, message :{}", DomainEventName.GRAPHIC_EDITED_RESIZE_IMAGE_STATUS_EVENT,
        bulkImageProcessResponse);
    try {
      productServiceWrapper.updateImagePathsForEditedResizeImages(bulkImageProcessResponse, false);
    } catch (Exception e) {
      log.error("Updating image location paths and flags failed, productCode :{}, Error - ",
          bulkImageProcessResponse.getGroupCode(), e);
    }
  }

}