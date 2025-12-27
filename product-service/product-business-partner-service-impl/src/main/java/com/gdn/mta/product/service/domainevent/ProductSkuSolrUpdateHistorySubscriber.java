package com.gdn.mta.product.service.domainevent;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.response.UpdatedProductHistoryRequest;
import com.gdn.mta.product.service.UpdatedProductHistoryService;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@ConditionalOnProperty(value = "product.update.history.solr.listener.enabled", havingValue = "true")
public class ProductSkuSolrUpdateHistorySubscriber {

  @Autowired
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${history.solr.update.new.event}")
  private boolean historySolrUpdateNewEvent;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getProductSkuSolrUpdateEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    UpdatedProductHistoryRequest auditTrailRequest =
        objectMapper.readValue(message, UpdatedProductHistoryRequest.class);
    try {
      updatedProductHistoryService.saveUpdatedProductHistoryToSolr(auditTrailRequest.getUpdatedProductHistories());
    } catch (Exception exp) {
      log.error("Exception caught while processing event {}  ", kafkaTopicProperties.getProductSkuSolrUpdateEvent(),
          exp);
    }
  }
}
