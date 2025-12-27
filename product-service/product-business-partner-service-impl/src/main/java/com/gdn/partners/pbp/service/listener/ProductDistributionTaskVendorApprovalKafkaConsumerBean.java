package com.gdn.partners.pbp.service.listener;

import java.util.Objects;
import java.util.UUID;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.service.ItemService;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.mta.product.service.partners.pbp.distributiontask.ProductDistributionTaskQCService;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductVendorApprovedEventModel;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;


@Slf4j
@Service
public class ProductDistributionTaskVendorApprovalKafkaConsumerBean {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CLIENT_ID = "10001";
  private static final String DEFAULT_USER = "System";
  private static final String CHANNEL_ID = "WEB";
  private static final String AUTHENTICATION_ID = "com.gdn.mta.orchestration";

  @Autowired
  private ProductDistributionTaskQCService productDistributionTaskQCService;

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  private String productCode;

  /**
   * set mandatory parameters in MDC
   *
   */
  private void setMandatoryParameters(PDTProductVendorApprovedEventModel pdtProductVendorApprovedEventModel) {
    String username = pdtProductVendorApprovedEventModel.getUpdatedBy() != null ?
        pdtProductVendorApprovedEventModel.getUpdatedBy() :
        DEFAULT_USER;
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, username + UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, AUTHENTICATION_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getVendorApprovalEventNoPriority()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    PDTProductVendorApprovedEventModel pdtProductVendorApprovedEventModel =
        objectMapper.readValue(message, PDTProductVendorApprovedEventModel.class);
    try {
      vendorTaskApprovedEvent(pdtProductVendorApprovedEventModel, 0);
    } catch (Exception e) {
      log.error("Error when consume event : {} Product Code : {} {} ",
          kafkaTopicProperties.getVendorApprovalEventNoPriority(), productCode, e.getMessage(), e);
    }
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getVendorApprovalEventPriority1()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedPriority1(String message) throws Exception {
    PDTProductVendorApprovedEventModel pdtProductVendorApprovedEventModel =
        objectMapper.readValue(message, PDTProductVendorApprovedEventModel.class);
    try {
      vendorTaskApprovedEvent(pdtProductVendorApprovedEventModel, 1);
    } catch (Exception e) {
      log.error("Error when consume event : {} Product Code : {} {} ",
          kafkaTopicProperties.getVendorApprovalEventPriority1(), productCode, e.getMessage(), e);
    }
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getVendorApprovalEventPriority2()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedPriority2(String message) throws Exception {
    PDTProductVendorApprovedEventModel pdtProductVendorApprovedEventModel =
        objectMapper.readValue(message, PDTProductVendorApprovedEventModel.class);
    try {
      vendorTaskApprovedEvent(pdtProductVendorApprovedEventModel, 2);
    } catch (Exception e) {
      log.error("Error when consume event : {} Product Code : {} {}",
          kafkaTopicProperties.getVendorApprovalEventPriority2(), productCode, e.getMessage(), e);
    }
  }

  private void vendorTaskApprovedEvent(PDTProductVendorApprovedEventModel pdtProductVendorApprovedEventModel, int prioritySeller)
      throws Exception {
    if (Objects.nonNull(pdtProductVendorApprovedEventModel)) {
      log.info("Request to approve product after QC : message : {} ", pdtProductVendorApprovedEventModel);
      setMandatoryParameters(pdtProductVendorApprovedEventModel);
      productCode = pdtProductVendorApprovedEventModel.getProductCode();
      PDTProductDomainEventModel pdtDomainModelResponseByCode = productDistributionTaskRepository
          .getPDTDomainModelResponseByCode(GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), pdtProductVendorApprovedEventModel.getProductCode());
      log.info(
          "Product-workflow-tracker : approve product after qc for productCode : {}, reviewPending :{}, postLive : {}, marginExceeded : {}",
          productCode, pdtDomainModelResponseByCode.isReviewPending(), pdtDomainModelResponseByCode.isPostLive(),
          pdtDomainModelResponseByCode.isMarginExceeded());
      this.productDistributionTaskQCService
          .processProductDistributionTaskQCKafkaConsumer(pdtDomainModelResponseByCode, prioritySeller);
      if ((pdtProductVendorApprovedEventModel.isPostLive()
          && pdtProductVendorApprovedEventModel.isReviewPending()) || (
          !pdtProductVendorApprovedEventModel.isPostLive()
              && !pdtProductVendorApprovedEventModel.isReviewPending())) {
        itemService.publishItemStatusEvent(pdtProductVendorApprovedEventModel.getProductCode(), ProductStatus.ACTIVE);
      }
    }
  }

}
