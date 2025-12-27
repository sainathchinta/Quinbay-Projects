package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkCreateProductEventModel;
import com.gdn.mta.bulk.service.BulkGenericProcessorService;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class BulkGenericCreateConvertedProductListener {
  @Autowired
  private BulkGenericProcessorService bulkGenericProcessorService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkGenericCreateProductForConvertedUpload()"
    + "}", autoStartup = "#{kafkaTopicProperties" + ".isAutoStartup()}")
  public void onDomainEventConsumedForPriority1(String message) throws ApplicationException {
    String topic = kafkaTopicProperties.getBulkGenericCreateProductForConvertedUpload();
    log.info("Received message from topic {} : {} ", topic, message);
    try {
      BulkCreateProductEventModel bulkCreateProductEventModel =
        objectMapper.readValue(message, BulkCreateProductEventModel.class);
      setMandatoryParameters(bulkCreateProductEventModel.getStoreId());
      bulkGenericProcessorService.processBulkGenericEvent(bulkCreateProductEventModel);
      log.info("Successfully processed bulk generic event for storeId {} ",
        bulkCreateProductEventModel.getStoreId());
    } catch (Exception e) {
      log.error("Error processing message from topic {} : {} ", topic, message, e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
        "Error while processing bulk generic event from topic: " + topic + ", message: " + message,
        e);
    } finally {
      MDC.clear();
    }
  }

  private void setMandatoryParameters(String storeId) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
      StringUtils.isNotBlank(storeId) ? storeId : Constant.STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, Constant.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constant.USER_NAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constant.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constant.CHANNEL_ID);
  }
}
