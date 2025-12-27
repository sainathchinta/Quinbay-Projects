package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalProcessDataDomainEventModel;
import com.gdn.mta.bulk.service.FbbConsignmentService;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class FbbL4RowListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private FbbConsignmentService fbbConsignmentService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getFbbL5UpdateRows()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Received event for topic : {} with message : {}", kafkaTopicProperties.getFbbL5UpdateRows(),
      message);
    InternalProcessDataDomainEventModel eventModel =
      objectMapper.readValue(message, InternalProcessDataDomainEventModel.class);
    try {
      fbbConsignmentService
        .processFbbL4RowEvent(eventModel.getStoreId(), eventModel.getInternalProcessRequestId());
    } catch (Exception e) {
      log.error("Error while consuming fbb l4 item : {}, error - ", message, e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
        "Error while listening and processing fbb l5 creation : " + message + ", error is : " + e
          .getMessage(), e);
    }
  }
}
