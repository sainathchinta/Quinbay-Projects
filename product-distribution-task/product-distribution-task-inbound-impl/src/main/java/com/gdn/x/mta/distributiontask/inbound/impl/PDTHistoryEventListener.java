package com.gdn.x.mta.distributiontask.inbound.impl;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.dao.api.TaskHistoryRepository;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTHistoryEventModel;
import com.gdn.x.mta.distributiontask.model.TaskHistory;
import com.gdn.x.productcategorybase.MandatoryParameterConstants;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class PDTHistoryEventListener {

  @Autowired
  ObjectMapper objectMapper;

  @Autowired
  private TaskHistoryRepository taskHistoryRepository;

  @KafkaListener(topics = DomainEventName.PDT_PRODUCT_HISTORY_EVENT, autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void pdtHistoryEventListener(String message) throws JsonProcessingException {
    log.info("Event consumed for topic : {} , message : {} ", DomainEventName.PDT_PRODUCT_HISTORY_EVENT, message);
    try {
      PDTHistoryEventModel pdtHistoryEventModel = objectMapper.readValue(message, PDTHistoryEventModel.class);
      MDC.put(MandatoryParameterConstants.USERNAME, pdtHistoryEventModel.getExecutor());
      TaskHistory taskHistory =
          new TaskHistory(pdtHistoryEventModel.getProductCode(), pdtHistoryEventModel.getProductName(),
              pdtHistoryEventModel.getCategoryCode(), pdtHistoryEventModel.getCategoryName(),
              pdtHistoryEventModel.getVendor(), pdtHistoryEventModel.getReason(), pdtHistoryEventModel.getState(),
              pdtHistoryEventModel.getStoreId(), pdtHistoryEventModel.getExecutor(),
              pdtHistoryEventModel.getTaskCode());
      taskHistoryRepository.saveAndFlush(taskHistory);
    } catch (Exception e) {
      log.error("Exception caught while processing event. topic : {} , message : {} , error - ",
          DomainEventName.PDT_PRODUCT_HISTORY_EVENT, message, e);
    }
  }

}
