package com.gdn.partners.pbp.service.listener;

import java.util.Objects;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.partners.pbp.distributiontask.ProductDistributionTaskQCService;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTRevisedProductVendorApprovedEventModel;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class PDTRevisedProductVendorApprovalKafkaConsumerBean {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CLIENT_ID = "10001";
  private static final String DEFAULT_USER = "System";
  private static final String CHANNEL_ID = "WEB";
  private static final String AUTHENTICATION_ID = "com.gdn.mta.orchestration";

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductDistributionTaskQCService productDistributionTaskQCService;

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  private ProductService productService;

  /**
   * set mandatory parameters in MDC
   *
   */
  private void setMandatoryParameters(
      PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel) {
    String username = StringUtils.isNotBlank(pdtRevisedProductVendorApprovedEventModel.getUpdatedBy()) ?
        pdtRevisedProductVendorApprovedEventModel.getUpdatedBy() :
        DEFAULT_USER;
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, username + UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, AUTHENTICATION_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
  }


  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.REVISED_PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel =
        objectMapper.readValue(message, PDTRevisedProductVendorApprovedEventModel.class);
    String productCode = null;
    try {
      if (Objects.nonNull(pdtRevisedProductVendorApprovedEventModel)) {
        log.info("Request to approve revised product after QC : message : {} ",
            pdtRevisedProductVendorApprovedEventModel);
        setMandatoryParameters(pdtRevisedProductVendorApprovedEventModel);
        productCode = pdtRevisedProductVendorApprovedEventModel.getProductCode();
        PDTProductDomainEventModel pdtDomainModelResponseByCode = productDistributionTaskRepository
            .getPDTDomainModelResponseByCode(GdnMandatoryRequestParameterUtil.getRequestId(),
                GdnMandatoryRequestParameterUtil.getUsername(),
                pdtRevisedProductVendorApprovedEventModel.getProductCode());
        log.info("Product-workflow-tracker : approve revised product after qc for productCode : {}, reviewPending :{}, "
                + "postLive : {}, marginExceeded : {}", productCode, pdtDomainModelResponseByCode.isReviewPending(),
            pdtDomainModelResponseByCode.isPostLive(), pdtDomainModelResponseByCode.isMarginExceeded());
        this.productDistributionTaskQCService.processVendorApprovalEventForRevisedProducts(pdtDomainModelResponseByCode,
            pdtRevisedProductVendorApprovedEventModel);
        if ((pdtRevisedProductVendorApprovedEventModel.isPostLive()
            && pdtRevisedProductVendorApprovedEventModel.isReviewPending()) ||
            (!pdtRevisedProductVendorApprovedEventModel.isPostLive()
                && !pdtRevisedProductVendorApprovedEventModel.isReviewPending())) {
          productService.publishProductStatusEventByProductCode(pdtRevisedProductVendorApprovedEventModel.getProductCode(),
              ProductStatus.ACTIVE, StringUtils.EMPTY);
        }
      }
    } catch (Exception e) {
      log.error("methodName=onDomainEventConsumed action=receive-kafka-from-pdt service=MTA status=ERROR "
          + "ref=product-update productCode={} ", productCode, e);
    }
  }
}
