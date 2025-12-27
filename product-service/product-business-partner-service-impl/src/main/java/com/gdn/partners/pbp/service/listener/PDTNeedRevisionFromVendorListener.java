package com.gdn.partners.pbp.service.listener;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.NeedCorrectionService;
import com.gdn.mta.product.service.ProductService;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTNeedRevisionEventModel;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class PDTNeedRevisionFromVendorListener {

  private static final String DEFAULT_CLIENT_ID = "pbp";
  private static final String CHANNEL_ID = "pbp";
  private static final String AUTHENTICATION_ID = "com.gdn.mta.orchestration";

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private NeedCorrectionService needCorrectionService;

  @Autowired
  private ProductService productService;

  private void setMandatoryParameters(PDTNeedRevisionEventModel pdtNeedRevisionEventModel) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, pdtNeedRevisionEventModel.getStoreId());
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, AUTHENTICATION_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.PRODUCT_REVISED_TASK_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    PDTNeedRevisionEventModel pdtNeedRevisionEventModel = this.objectMapper.readValue(message,
        PDTNeedRevisionEventModel.class);
    try {
      GdnPreconditions.checkArgument(Objects.nonNull(pdtNeedRevisionEventModel), ErrorMessages.REVISION_EVENT_NULL);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(pdtNeedRevisionEventModel.getProductCode()),
          ErrorMessages.PRODUCT_CODE_BLANK);
      log.info("Received need revision event for : {}", pdtNeedRevisionEventModel.getProductCode());
      setMandatoryParameters(pdtNeedRevisionEventModel);
      this.needCorrectionService.sendForNeedCorrectionByProductCode(pdtNeedRevisionEventModel.getProductCode(), false,
          null, true);
      productService.publishProductStatusEventByProductCode(pdtNeedRevisionEventModel.getProductCode(),
          ProductStatus.NEED_CORRECTION, StringUtils.EMPTY);
    } catch (Exception e) {
      log.error("Error on marking product for need revision. Payload received : {}, error - ",
          pdtNeedRevisionEventModel, e);
    }
  }
}
