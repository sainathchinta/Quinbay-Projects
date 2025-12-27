package com.gdn.partners.pbp.service.listener;

import java.io.IOException;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.partners.pbp.distributiontask.ProductDistributionTaskQCService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.newrelic.api.agent.Trace;

/**
 * Created by akshay.bhatt on 25/04/18
 */
@Service
public class ProductDistributionTaskQCKafkaConsumerBean {

  private static final Logger LOGGER = LoggerFactory.getLogger(PDTProductDomainEventModel.class);
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CLIENT_ID = "10001";
  private static final String DEFAULT_USER = "System";
  private static final String CHANNEL_ID = "WEB";
  private static final String AUTHENTICATION_ID = "com.gdn.mta.orchestration";

  @Autowired
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Autowired
  private ProductDistributionTaskQCService productDistributionTaskQCService;

  @Autowired
  private ObjectMapper objectMapper;

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductStatusPublisherService productStatusPublisherService;

  @Value("${approveQc.max.retry.count}")
  private Integer maxRetryCount;

  private String productCode;

  /**
   * set mandatory parameters in MDC
   *
   */
  private void setMandatoryParameters(PDTProductDomainEventModel pdtProductDomainEventModel) {
    String username =
        pdtProductDomainEventModel.getUpdatedBy() != null ? pdtProductDomainEventModel.getUpdatedBy() : DEFAULT_USER;
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, username + UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, AUTHENTICATION_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.PRODUCT_QC_APPROVED_TASK_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    PDTProductDomainEventModel pdtProductDomainEventModel =
        objectMapper.readValue(message, PDTProductDomainEventModel.class);
    try {
      if (pdtProductDomainEventModel != null) {
        LOGGER.info("Request to approve product after QC : message : {} ", pdtProductDomainEventModel);
        setMandatoryParameters(pdtProductDomainEventModel);
        productCode = pdtProductDomainEventModel.getProductCode();
        LOGGER.info(
            "Product-workflow-tracker : approve product after qc for productCode : {}, reviewPending :{}, postLive : {}, marginExceeded : {}",
            productCode, pdtProductDomainEventModel.isReviewPending(), pdtProductDomainEventModel.isPostLive(),
            pdtProductDomainEventModel.isMarginExceeded());
        this.productDistributionTaskQCService.processProductDistributionTaskQCKafkaConsumer(pdtProductDomainEventModel, 0);
      }
    } catch (Exception e) {
      LOGGER.error("methodName=onDomainEventConsumed action=receive-kafka-from-pdt service=MTA status=ERROR "
          + "ref=product-creation productCode={} ", productCode, e);
    }
  }
}
