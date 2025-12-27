package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.model.AddDSModelProductToIPREventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.service.api.IprWrapperService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class AddDSModelProductToIPREventListener {

  private final ObjectMapper objectMapper;
  private final IprWrapperService iprWrapperService;
  private final KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer.getAddDSModelProductToIprEvent}",
                 autoStartup = "#{kafkaTopicPropertiesConsumer.isAutoStartup()}")
  public void addDSModelProductToIPREventListener(String message) {
    log.info("Event consumed {} for DS Model Add product to IPR, message : {} ",
        kafkaTopicPropertiesConsumer.getAddDSModelProductToIprEvent(), message);
    try {
      AddDSModelProductToIPREventModel addDSModelProductToIPREventModel =
          objectMapper.readValue(message, AddDSModelProductToIPREventModel.class);
      iprWrapperService.addDSModelProductToIPR(addDSModelProductToIPREventModel.getProductSku(),
          Constants.DEFAULT_STORE_ID, addDSModelProductToIPREventModel.getSource());
    } catch (Exception e) {
      log.error(
          "Exception caught while processing event add product to IPR, message : {} , error - ",
          message, e);
    }
  }
}
