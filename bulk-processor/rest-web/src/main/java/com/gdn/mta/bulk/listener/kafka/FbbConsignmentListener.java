package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.FbbConsignmentEventModel;
import com.gdn.mta.bulk.service.FbbConsignmentService;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class FbbConsignmentListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private FbbConsignmentService fbbConsignmentService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getFbbCreateConsignment()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Received event for topic : {} with message : {}",
      kafkaTopicProperties.getFbbCreateConsignment(), message);
    try {
      TypeReference<FbbConsignmentEventModel> typ = new TypeReference<FbbConsignmentEventModel>() {
      };
      FbbConsignmentEventModel fbbConsignmentEventModel = objectMapper.readValue(message, typ);

      if (!validateEventModel(fbbConsignmentEventModel)) {
        throw new ApplicationException(ErrorCategory.VALIDATION, "Invalid event payload");
      }
      fbbConsignmentService.preProcessFbbConsignmentCreation(fbbConsignmentEventModel);
    } catch (Exception e) {
      log.error("Error while fbb consignment creation payload : {}, error - ", message, e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
        "Error while listening and processing fbb consignment creation payload : " + message
          + ", error is : " + e.getMessage(), e);
    }

  }

  private boolean validateEventModel(FbbConsignmentEventModel eventModel) {
    return StringUtils.isNotEmpty(eventModel.getConsignmentId()) && StringUtils
      .isNotEmpty(eventModel.getBusinessPartnerCode()) && CollectionUtils
      .isNotEmpty(eventModel.getFbbItems());
  }
}
