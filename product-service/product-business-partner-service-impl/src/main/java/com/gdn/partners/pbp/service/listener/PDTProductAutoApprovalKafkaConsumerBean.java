package com.gdn.partners.pbp.service.listener;

import java.util.Objects;
import java.util.UUID;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTAutoApprovalEventModel;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class PDTProductAutoApprovalKafkaConsumerBean {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CLIENT_ID = "10001";
  private static final String DEFAULT_USER = "System";
  private static final String CHANNEL_ID = "WEB";
  private static final String AUTHENTICATION_ID = "com.gdn.mta.orchestration";

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  private void setMandatoryParameters() {
    String username = DEFAULT_USER;
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, username + UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, AUTHENTICATION_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
  }


  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    PDTAutoApprovalEventModel pdtAutoApprovalEventModel =
        objectMapper.readValue(message, PDTAutoApprovalEventModel.class);
    try {
      if (Objects.nonNull(pdtAutoApprovalEventModel)) {
        log.info("Request to Auto approve product : message : {} ", pdtAutoApprovalEventModel);
        setMandatoryParameters();
        productServiceWrapper.autoApproveProduct(DEFAULT_STORE_ID, pdtAutoApprovalEventModel.getProductCode());
      }
    } catch (Exception e) {
      log.error("Error while Auto Approving product. Payload received : {}, error - ",
          pdtAutoApprovalEventModel, e);
    }
  }
}
