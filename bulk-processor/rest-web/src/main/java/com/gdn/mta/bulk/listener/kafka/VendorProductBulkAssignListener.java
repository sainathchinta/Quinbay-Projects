package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.mta.bulk.service.VendorProductBulkAssignService;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class VendorProductBulkAssignListener {

  @Autowired
  private VendorProductBulkAssignService vendorProductBulkAssignService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkAssignVendorProduct()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkAssignVendorProduct(), message);
    BulkVendorProductAssignRequest bulkUpdateQueue = null;
    String userName;
    try {
      bulkUpdateQueue = objectMapper.readValue(message, BulkVendorProductAssignRequest.class);
      userName = bulkUpdateQueue.getUpdatedBy();
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, userName);
      log.info(
          "Bulk assign of products using excel by username = {} requestId = {} storeId = {} filePath = {} bulkUpdateQueue = {}",
          userName, bulkUpdateQueue.getRequestId(), bulkUpdateQueue.getStoreId(), bulkUpdateQueue.getFilePath(),
          bulkUpdateQueue);
      internalProcessServiceWrapper.uploadVendorBulkAssignmentProcess(bulkUpdateQueue.getStoreId(), bulkUpdateQueue);
    } catch (Exception e) {
      log.error("Error while assigning products using excel bulkUpdateQueue = {} ", bulkUpdateQueue, e);
    }
  }
}
