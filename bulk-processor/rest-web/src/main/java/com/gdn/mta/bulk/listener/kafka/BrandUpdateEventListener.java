package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.service.BrandUpdateService;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BrandUpdateEventListener {

  @Autowired
  private BrandUpdateService brandUpdateService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBrandUpdateEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : {} message : {} ", kafkaTopicProperties.getBrandUpdateEvent(), message);
    try {
      InternalBulkUploadDataDomainEventModel internalProcessDataDomainEventModel =
          objectMapper.readValue(message, InternalBulkUploadDataDomainEventModel.class);
      setMandatoryParameters(internalProcessDataDomainEventModel.getStoreId(),
          internalProcessDataDomainEventModel.getUpdatedBy());
      brandUpdateService.processBrandUpdateEvent(internalProcessDataDomainEventModel.getStoreId(),
          internalProcessDataDomainEventModel.getProcessType(),
          internalProcessDataDomainEventModel.getInternalProcessDataRequestId());
    } catch (Exception e) {
      log.info("error caught on onDomainEventConsumed. event : {} message : {} ",
          kafkaTopicProperties.getBrandUpdateEvent(), message, e);
    }
  }

  private void setMandatoryParameters(String storeId, String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, Constant.REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constant.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constant.CHANNEL_ID);
  }

}
