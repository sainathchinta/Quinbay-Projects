package com.gdn.x.productcategorybase.domainevent;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.service.RestrictedKeywordHistoryService;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class RestrictedKeywordHistoryListener {

  @Autowired
  private RestrictedKeywordHistoryService restrictedKeywordHistoryService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = "#{kafkaTopicProperties.getRestrictedKeywordHistoryEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Message received, from topic {}, message : {}", DomainEventName.RESTRICTED_KEYWORD_HISTORY_EVENT,
        message);
    try {
      RestrictedKeywordHistoryEventModel restrictedKeywordHistoryEventModel =
          objectMapper.readValue(message, RestrictedKeywordHistoryEventModel.class);
      setMandatoryParameters(restrictedKeywordHistoryEventModel.getStoreId(), restrictedKeywordHistoryEventModel.getUserName());
      restrictedKeywordHistoryService.saveRestrictedKeywordHistory(restrictedKeywordHistoryEventModel);
    } catch (Exception e) {
      log.error("Error while listening {}, error - ", DomainEventName.BRAND_AUTH_HISTORY_EVENT, e);
    }
  }

  private void setMandatoryParameters(String storeId, String userName) {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, userName);
  }
}
