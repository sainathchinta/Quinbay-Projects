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
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.partners.pbp.distributiontask.ProductDistributionTaskQCService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTEditedProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class PDTEditedProductVendorApprovalKafkaConsumerBean {

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
  private void setMandatoryParameters(PDTEditedProductVendorApprovedEventModel pdtProductVendorApprovedEventModel) {
    String username = StringUtils.isNotBlank(pdtProductVendorApprovedEventModel.getUpdatedBy()) ?
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
  @KafkaListener(topics = DomainEventName.EDITED_PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {

    PDTEditedProductVendorApprovedEventModel pdtEditedProductVendorApprovedEventModel =
        objectMapper.readValue(message, PDTEditedProductVendorApprovedEventModel.class);
    String productCode = null;
    try {
      if (Objects.nonNull(pdtEditedProductVendorApprovedEventModel)) {
        log.info("Request to approve edited product after QC : message : {} ",
            pdtEditedProductVendorApprovedEventModel);
        setMandatoryParameters(pdtEditedProductVendorApprovedEventModel);
        productCode = pdtEditedProductVendorApprovedEventModel.getProductCode();
        PDTProductDomainEventModel pdtDomainModelResponseByCode = productDistributionTaskRepository
            .getPDTDomainModelResponseByCode(GdnMandatoryRequestParameterUtil.getRequestId(),
                GdnMandatoryRequestParameterUtil.getUsername(),
                pdtEditedProductVendorApprovedEventModel.getProductCode());
        log.info("Product-workflow-tracker : approve edited product after qc for productCode : {}, reviewPending :{}, "
                + "postLive : {}, marginExceeded : {}", productCode, pdtDomainModelResponseByCode.isReviewPending(),
            pdtDomainModelResponseByCode.isPostLive(), pdtDomainModelResponseByCode.isMarginExceeded());
        this.productDistributionTaskQCService.processVendorApprovalEventForEditedProducts(pdtDomainModelResponseByCode,
            pdtEditedProductVendorApprovedEventModel.getApprovalType());
      }
    } catch (Exception e) {
      productService.setReviewPendingFlagToTrue(Constants.DEFAULT_STORE_ID, productCode);
      log.error("methodName=onDomainEventConsumed action=receive-kafka-from-pdt service=MTA status=ERROR "
          + "ref=product-update productCode={} ", productCode, e);
    }
  }
}
