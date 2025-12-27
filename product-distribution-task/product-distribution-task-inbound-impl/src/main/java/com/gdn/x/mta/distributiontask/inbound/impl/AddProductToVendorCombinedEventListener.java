package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.AddProductToVendorCombinedEventModel;
import com.gdn.mta.product.enums.PrioritySeller;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.service.api.AddProductToVendorService;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.UUID;

@Service
@Slf4j
public class AddProductToVendorCombinedEventListener {
  private static final String DEFAULT_USERNAME = "PDT";
  private static final String STORE_ID = "10001";

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private AddProductToVendorService addProductToVendorService;

  @Autowired
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer.getVendorCombinedEventNoPriority()}",
                 autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedPriority0(String message) throws Exception {
    try {
      setMandatoryParameters();
      AddProductToVendorCombinedEventModel addProductToVendorCombinedEventModel =
          objectMapper.readValue(message, AddProductToVendorCombinedEventModel.class);
      log.info("Received message for event {} - message: {}",
          kafkaTopicPropertiesConsumer.getVendorCombinedEventNoPriority(), addProductToVendorCombinedEventModel);
      if (Objects.nonNull(addProductToVendorCombinedEventModel.getScreeningProductApprovalEvent())) {
        addProductToVendorService.processScreeningApprovalEvent(
            addProductToVendorCombinedEventModel.getScreeningProductApprovalEvent(),
            PrioritySeller.NO_PRIORITY.getPrioritySeller());
      } else if (Objects.nonNull(addProductToVendorCombinedEventModel.getAddEditedProductToPDTEvent())) {
        addProductToVendorService.processAddEditedProductEvent(
            addProductToVendorCombinedEventModel.getAddEditedProductToPDTEvent());
      } else if (Objects.nonNull(addProductToVendorCombinedEventModel.getAddRevisedProductToPDTEvent())) {
        addProductToVendorService.processAddRevisedProductEvent(
            addProductToVendorCombinedEventModel.getAddRevisedProductToPDTEvent());
      } else if (Objects.nonNull(addProductToVendorCombinedEventModel.getPdtDimensionRefreshEventModel())) {
        log.info("Process dimensions update for event {}", addProductToVendorCombinedEventModel);
        addProductToVendorService.processDimensionsUpdateEvent(
            addProductToVendorCombinedEventModel.getPdtDimensionRefreshEventModel());
      }
      else if(Objects.nonNull(addProductToVendorCombinedEventModel.getAutoApprovalTypeRequestModel())) {
        log.info("Process Auto Approval check {} ", addProductToVendorCombinedEventModel);
        addProductToVendorService
            .processAutoApprovalCheckEvent(addProductToVendorCombinedEventModel.getAutoApprovalTypeRequestModel());
      }
    } catch (Exception e) {
      log.error("Error when consume event : {}, message : {} ", message, e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer.getVendorCombinedEventPriority1()}")
  public void onDomainEventConsumedPriority1(String message) throws Exception {
    try {
      setMandatoryParameters();
      AddProductToVendorCombinedEventModel addProductToVendorCombinedEventModel =
          objectMapper.readValue(message, AddProductToVendorCombinedEventModel.class);
      log.info("Received message for event {} - message: {}",
          kafkaTopicPropertiesConsumer.getVendorCombinedEventPriority1(), addProductToVendorCombinedEventModel);
      if (Objects.nonNull(addProductToVendorCombinedEventModel.getScreeningProductApprovalEvent())) {
        addProductToVendorService.processScreeningApprovalEvent(
            addProductToVendorCombinedEventModel.getScreeningProductApprovalEvent(),
            PrioritySeller.PRIORITY_1.getPrioritySeller());
      }
    } catch (Exception e) {
      log.error("Error when consume event : {}, message : {}",
          kafkaTopicPropertiesConsumer.getVendorCombinedEventPriority1(), message, e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer.getVendorCombinedEventPriority2()}")
  public void onDomainEventConsumedPriority2(String message) throws Exception {
    try {
      setMandatoryParameters();
      AddProductToVendorCombinedEventModel addProductToVendorCombinedEventModel =
          objectMapper.readValue(message, AddProductToVendorCombinedEventModel.class);
      log.info("Received message for event {} - message: {}",
          kafkaTopicPropertiesConsumer.getVendorCombinedEventPriority2(), addProductToVendorCombinedEventModel);
      if (Objects.nonNull(addProductToVendorCombinedEventModel.getScreeningProductApprovalEvent())) {
        addProductToVendorService.processScreeningApprovalEvent(
            addProductToVendorCombinedEventModel.getScreeningProductApprovalEvent(),
            PrioritySeller.PRIORITY_2.getPrioritySeller());
      }
    } catch (Exception e) {
      log.error("Error when consume event : {}, message : {} ",
          kafkaTopicPropertiesConsumer.getVendorCombinedEventPriority2(), message, e);
    }
  }

  private void setMandatoryParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
  }
}
