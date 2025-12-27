package com.gdn.partners.pbp.service.listener;


import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.domain.event.config.DomainEventName;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ImageResizeStatusKafkaSubscriberBean {

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getImageResizeStatusEventNoPriority()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    BulkImageProcessResponse bulkImageProcessResponse = objectMapper.readValue(message, BulkImageProcessResponse.class);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, bulkImageProcessResponse.getUsername());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, bulkImageProcessResponse.getStoreId());
    log.info("Message received, from topic {}, message :{}", kafkaTopicProperties.getImageResizeStatusEventNoPriority(),
        bulkImageProcessResponse);
    try {
      if (checkImageResizeStatus(bulkImageProcessResponse)) {
        productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      } else {
        productServiceWrapper.updateImagePathsAndFlagOnResizingImageFailure(bulkImageProcessResponse);
      }
    } catch (Exception e) {
      log.error("Updating image location paths and flags failed, productCode :{} ",
          bulkImageProcessResponse.getGroupCode(), e);
    }
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getImageResizeStatusEventPriority1()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedStatusEventForPriority1(String message) throws Exception {
    BulkImageProcessResponse bulkImageProcessResponse = objectMapper.readValue(message, BulkImageProcessResponse.class);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, bulkImageProcessResponse.getUsername());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, bulkImageProcessResponse.getStoreId());
    log.info("Message received, from topic {}, message :{}", kafkaTopicProperties.getImageResizeStatusEventPriority1(),
            bulkImageProcessResponse);
    try {
      if (checkImageResizeStatus(bulkImageProcessResponse)) {
        productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      } else {
        productServiceWrapper.updateImagePathsAndFlagOnResizingImageFailure(bulkImageProcessResponse);
      }
    } catch (Exception e) {
      log.error("Updating image location paths and flags failed, productCode :{} ",
              bulkImageProcessResponse.getGroupCode(), e);
    }
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getImageResizeStatusEventPriority2()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedStatusEventForPriority2(String message) throws Exception {
    BulkImageProcessResponse bulkImageProcessResponse = objectMapper.readValue(message, BulkImageProcessResponse.class);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, bulkImageProcessResponse.getUsername());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, bulkImageProcessResponse.getStoreId());
    log.info("Message received, from topic {}, message :{}", kafkaTopicProperties.getImageResizeStatusEventPriority2(),
            bulkImageProcessResponse);
    try {
      if (checkImageResizeStatus(bulkImageProcessResponse)) {
        productServiceWrapper.updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
      } else {
        productServiceWrapper.updateImagePathsAndFlagOnResizingImageFailure(bulkImageProcessResponse);
      }
    } catch (Exception e) {
      log.error("Updating image location paths and flags failed, productCode :{} ",
              bulkImageProcessResponse.getGroupCode(), e);
    }
  }



  private static boolean checkImageResizeStatus(BulkImageProcessResponse bulkImageProcessResponse) {
    return CollectionUtils.isNotEmpty(
        bulkImageProcessResponse.getImageResponses().stream().filter(imageResponse -> !imageResponse.isSuccess())
            .collect(Collectors.toList())) ? false : true;
  }
}


