package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.FbbL5CreateDTO;
import com.gdn.mta.bulk.entity.FbbL5ItemEventModel;
import com.gdn.mta.bulk.service.FbbConsignmentService;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class FbbCreateL5Listener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private FbbConsignmentService fbbConsignmentService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getFbbCreateL5()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Received event for topic : {} with message : {}",
      kafkaTopicProperties.getFbbCreateL5(), message);
    try {
      FbbL5ItemEventModel fbbL5ItemEventModel =
        objectMapper.readValue(message, FbbL5ItemEventModel.class);
      FbbL5CreateDTO fbbL5CreateDTO = new FbbL5CreateDTO();
      BeanUtils.copyProperties(fbbL5ItemEventModel,fbbL5CreateDTO);
      fbbConsignmentService.processL5CreationEvent(fbbL5ItemEventModel.getInternalProcessCode(),fbbL5CreateDTO);
    } catch (Exception e) {
      log.error("Error while consuming fbb l4 item : {}, error - ", message, e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
        "Error while listening and processing fbb l5 creation : " + message
          + ", error is : " + e.getMessage(), e);
    }

  }
}
