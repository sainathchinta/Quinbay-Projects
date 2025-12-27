package com.gdn.mta.product.service.domainevent;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.response.InternalProductHistoryEventModel;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.product.service.ProductServiceBean;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductHistorySubscriber {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductServiceBean productServiceBean;


  @KafkaListener(topics = DomainEventName.PRODUCT_INTERNAL_HISTORY_SAVE, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {

    InternalProductHistoryEventModel internalProductHistoryEventModel =
        objectMapper.readValue(message, InternalProductHistoryEventModel.class);
    log.info("Consume event {} with message : {}", DomainEventName.PRODUCT_INTERNAL_HISTORY_SAVE,
        internalProductHistoryEventModel.toString());
    try {
      setMandatoryParameters(internalProductHistoryEventModel.getStoreId(),
          internalProductHistoryEventModel.getUsername());
      this.productServiceBean.saveProductHistory(internalProductHistoryEventModel.getStoreId(),
          internalProductHistoryEventModel.getProductCode(), internalProductHistoryEventModel.getUsername(),
          internalProductHistoryEventModel.getActivity(), internalProductHistoryEventModel.getNotes());
    } catch (Exception exp) {
      log.error("Exception caught while processing event {} for product : {} ",
          DomainEventName.PRODUCT_INTERNAL_HISTORY_SAVE, internalProductHistoryEventModel.getProductCode(), exp);
    }
  }

  private void setMandatoryParameters(String storeId, String userName) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, userName);
  }
}
