package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.download.VendorAutoAssignmentRequest;
import com.gdn.mta.bulk.service.VendorProductBulkAssignService;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class VendorBulkAutoAssignmentEventListener {
  @Autowired
  private VendorProductBulkAssignService vendorProductBulkAssignService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getVendorAutoAssignmentEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : {} message : {} ", kafkaTopicProperties.getVendorAutoAssignmentEvent(), message);
    try {
      VendorAutoAssignmentRequest vendorAutoAssignmentRequest =
          objectMapper.readValue(message, VendorAutoAssignmentRequest.class);
      MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, vendorAutoAssignmentRequest.getStoreId());
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, vendorAutoAssignmentRequest.getVendorEmail());
      vendorProductBulkAssignService.processVendorAutoAssignment(vendorAutoAssignmentRequest);
      log.info(
          "Bulk auto assign of products event consumed storeId = {} vendorEmail = {} vendorAutoAssignmentRequest = {}",
          vendorAutoAssignmentRequest.getStoreId(), vendorAutoAssignmentRequest.getVendorEmail(),
          vendorAutoAssignmentRequest);
    } catch (Exception e) {
      log.info("error caught on onDomainEventConsumed event : {} ", kafkaTopicProperties.getVendorAutoAssignmentEvent(),
          e);
    }
  }
}
