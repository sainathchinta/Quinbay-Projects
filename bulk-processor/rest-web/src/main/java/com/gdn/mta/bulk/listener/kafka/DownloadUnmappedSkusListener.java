package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.DownloadUnmappedSkuDomainEventModel;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.newrelic.api.agent.Trace;

@Service
public class DownloadUnmappedSkusListener {

  private static final Logger LOG = LoggerFactory.getLogger(BulkConfigurationUpdateListener.class);

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getDownloadUnmappedSkus()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOG.info("Consume event {} with message {} ", kafkaTopicProperties.getDownloadUnmappedSkus(), message);
    DownloadUnmappedSkuDomainEventModel downloadUnmappedSkuDomainEventModel =
        objectMapper.readValue(message, DownloadUnmappedSkuDomainEventModel.class);
    LoggerAttributeModel loggerModel = null;
    loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed", message.toString());
    LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
    try {
      bulkProcessService.downloadUnmappedProductSkus(downloadUnmappedSkuDomainEventModel.getStoreId(),
          downloadUnmappedSkuDomainEventModel.getUsername(), downloadUnmappedSkuDomainEventModel.getEmailTo(),
          downloadUnmappedSkuDomainEventModel.getRequestId(), downloadUnmappedSkuDomainEventModel.getParentCategoryCode(),
          downloadUnmappedSkuDomainEventModel.getLanguage());
    } catch (Exception e) {
      LOG.error("Error when consume event {} with message :{} ", kafkaTopicProperties.getDownloadUnmappedSkus(), message, e);
    }
  }

}
