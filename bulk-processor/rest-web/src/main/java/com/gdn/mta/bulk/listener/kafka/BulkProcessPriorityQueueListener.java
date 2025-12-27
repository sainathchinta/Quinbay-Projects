package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkProcessQueue;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.ProcessorService;
import com.gdn.mta.bulk.service.TrackerService;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BulkProcessPriorityQueueListener {

  private static final String PRODUCT_LEVEL_3 = "ProductLevel3";

  @Autowired
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkCreatePriorityEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkCreatePriorityEvent(), message);
    BulkProcessQueue bulkProcessQueue = objectMapper.readValue(message, BulkProcessQueue.class);
    try {
      setMandatoryParameters(bulkProcessQueue.getStoreId());
      ProcessorService processorService = (ProcessorService) autowireCapableBeanFactory.getBean(
          bulkProcessQueue.getBulkProcessType() + Constant.PROCESSOR_SERVICE);
      processorService.process(bulkProcessQueue);
    } catch (Exception e) {
      log.error("Error while consuming event : {}, message : {} ", kafkaTopicProperties.getBulkCreatePriorityEvent(),
          message, e);
      bulkProcessService.abortBulkProcess(bulkProcessQueue.getStoreId(), bulkProcessQueue.getBulkProcessCode());
    }
  }

  private void setMandatoryParameters(String storeId) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, Constant.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constant.USER_NAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constant.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constant.CHANNEL_ID);
  }
}
