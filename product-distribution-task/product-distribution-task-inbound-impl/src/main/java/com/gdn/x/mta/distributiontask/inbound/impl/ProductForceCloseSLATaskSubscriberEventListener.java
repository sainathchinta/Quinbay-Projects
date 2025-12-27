package com.gdn.x.mta.distributiontask.inbound.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductForcedRollbackSLAExceedDomainEventModel;
import com.gdn.x.mta.distributiontask.service.api.SlaCheckerService;

@Service
@Slf4j
public class ProductForceCloseSLATaskSubscriberEventListener{

  @Autowired
  private SlaCheckerService slaCheckerService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics =
                     DomainEventName.PRODUCT_FORCE_CLOSE_TASK_EVENT_AND_ROLLBACK_PRODUCT_NAME,
                 autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    ProductForcedRollbackSLAExceedDomainEventModel productDomainEventModel =
        objectMapper.readValue(message, ProductForcedRollbackSLAExceedDomainEventModel.class);
    log.info("RECEIVED MESSAGE FROM [{}] PRODUCT CODE={}, TASK CODE={}",
        DomainEventName.PRODUCT_FORCE_CLOSE_TASK_EVENT_AND_ROLLBACK_PRODUCT_NAME,
        productDomainEventModel.getProductCode(), productDomainEventModel.getTaskCode());
    try {
      this.slaCheckerService.turnTaskToDistributionList(productDomainEventModel.getTaskId());
    } catch (Exception e) {
      log.error("Error when consume SLA Exceeded Product Code {}, Error {}",
          productDomainEventModel.getProductCode(), e.getMessage(), e);
    }

  }
}
