package com.gdn.mta.bulk.listener.kafka;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.BulkUpdateService;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BulkUpdateOff2OnListener {
  private static final String SERVICE_BEAN_NAME = "BulkUpdateService";
  private static final String PRODUCT_LEVEL_3 = "ProductLevel3";

  @Autowired
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{ kafkaTopicProperties.getBulkUpdateOff2On()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : Bulk update off2on Domain Event consumed {} , with event : {} ", message,
        kafkaTopicProperties.getBulkUpdateOff2On());
    BulkUpdateQueue bulkUpdateQueue = objectMapper.readValue(message, BulkUpdateQueue.class);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(bulkUpdateQueue.getBulkProcessCode()),
        "Bulk Process code is empty " + bulkUpdateQueue);
    BulkUpdateService bulkUpdateService =
        (BulkUpdateService) autowireCapableBeanFactory.getBean(PRODUCT_LEVEL_3 + SERVICE_BEAN_NAME);
    try {
      bulkUpdateService.processBulkUpdateOff2On(bulkUpdateQueue);
    } catch (Exception ex) {
      bulkProcessService.abortBulkProcess(bulkUpdateQueue.getStoreId(),
          bulkUpdateQueue.getBulkProcessCode());
      log.error("Exception occurred while bulk update off2on for bulkProcessCode : {} ,{} ", bulkUpdateQueue.getBulkProcessCode(),
          ex);
    }
  }
}
