package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.model.AddCustomerProductToIPREventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.service.api.IprWrapperService;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Slf4j
@Service
public class AddProductToIPREventListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private IprWrapperService iprWrapperService;

  @Autowired
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer.getAddCustomerProductToIprEvent()}",
                 autoStartup = "#{kafkaTopicPropertiesConsumer.isAutoStartup()}")
  public void addProductToIPREventListener(String message) throws JsonProcessingException {
    log.info("Event consumed {} for add product to IPR, message : {} ",
        kafkaTopicPropertiesConsumer.getAddCustomerProductToIprEvent(), message);
    try {
      AddCustomerProductToIPREventModel addCustomerProductToIPREventModel =
          objectMapper.readValue(message, AddCustomerProductToIPREventModel.class);
      setMandatoryParameters(addCustomerProductToIPREventModel.getUpdatedBy());
      iprWrapperService.addProductToIPR(addCustomerProductToIPREventModel.getProductSku(),
          addCustomerProductToIPREventModel.getStoreId(),
          addCustomerProductToIPREventModel.getSource(), null, null);
    } catch (Exception e) {
      log.error(
          "Exception caught while processing event add product to IPR, message : {} , error - ",
          message, e);
    }
  }

  private void setMandatoryParameters(String updatedBy) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, Constants.DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, updatedBy);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        Constants.DEFAULT_CHANNEL_ID);
  }
}
