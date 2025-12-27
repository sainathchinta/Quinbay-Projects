package com.gdn.mta.bulk.listener;

import com.gdn.mta.bulk.entity.BulkProcess;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BulkWorkOrderCreationListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getWorkOrderEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Bulk Work Order Domain Event consumed {} , message {} ", kafkaTopicProperties.getWorkOrderEvent(), message);
    try {
      BulkUpdateQueue bulkUpdateQueue = objectMapper.readValue(message, BulkUpdateQueue.class);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(bulkUpdateQueue.getBulkProcessCode()),
          "Bulk Process code is empty " + bulkUpdateQueue);
      BulkProcess bulkProcess = bulkProcessService.validateAndUpdateWorkOrder(bulkUpdateQueue);
      bulkProcessService.processWorkOrder(bulkUpdateQueue, bulkProcess);
    } catch (Exception e) {
      log.error("Error while processing Work Order Creation event :  {} - Error {} ", kafkaTopicProperties.getWorkOrderEvent(),
          e);
    }
  }
}
