package com.gdn.partners.pbp.service.listener;

import java.util.ArrayList;
import java.util.UUID;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.micro.graphics.domain.event.config.DomainEventName;
import com.gdn.micro.graphics.domain.event.model.ScaleEditedImagesResponse;
import com.gdn.micro.graphics.domain.event.model.ScaleImageResponse;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.util.MandatoryParameterUtil;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ScaleRevisedImageKafkaSubscriberBean {

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductWfService productWfService;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${approve.image.channel.id.default}")
  private String channelId;

  @Value("${approve.image.authenticator.id.default}")
  private String authenticatorId;

  private String productCode;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.GRAPHIC_REVISED_IMAGE_SCALE_STATUS_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    ScaleEditedImagesResponse scaleEditedImagesResponse =
        objectMapper.readValue(message, ScaleEditedImagesResponse.class);
    log.info("[ScaleRevisedImageKafkaSubscriber] Retrieved message from Topic: {}, Message: {}",
        DomainEventName.GRAPHIC_REVISED_IMAGE_SCALE_STATUS_EVENT, scaleEditedImagesResponse);
    final String requestId = Constants.XGP_USER + UUID.randomUUID().toString();
    String storeId = scaleEditedImagesResponse.getStoreId();
    try {
      if (CollectionUtils.isNotEmpty(scaleEditedImagesResponse.getImageResponses())) {
        productCode = scaleEditedImagesResponse.getProductCode();
        String clientId = null;
        boolean success = Boolean.TRUE;
        for (ScaleImageResponse imageResponse : scaleEditedImagesResponse.getImageResponses()) {
          if (!imageResponse.isSuccess()) {
            success = Boolean.FALSE;
            log.error("ImageProcessing not successful from XGP, product-code : {}", productCode);
            break;
          }
          ConverterUtil.trimLocationPaths(imageResponse);
          clientId = imageResponse.getClientId();
        }
        log.info("subscribe image from kafka subscribe-image for : {}", productCode);
        MandatoryParameterUtil.mandatoryParameterSetter(MandatoryRequestParam
            .generateMandatoryRequestParam(scaleEditedImagesResponse.getStoreId(), this.channelId, clientId, requestId,
                scaleEditedImagesResponse.getUsername(), this.authenticatorId));
        if (success) {
          this.productWfService
              .approveImageForRevisedProduct(storeId, productCode, scaleEditedImagesResponse.getImageResponses(), false,
                  new ArrayList<>());
        } else {
          log.warn("image result is not success for : {}", productCode);
          this.productService.saveProductHistory(storeId, productCode, Constants.DEFAULT_USERNAME,
              SaveHistoryConstants.PROCESS_IMAGE_FAILED, null);
        }
      }
    } catch (Exception e) {
      log.error("Error while listening to event : {} for product : {} {}",
          DomainEventName.GRAPHIC_REVISED_IMAGE_SCALE_STATUS_EVENT, productCode, e.getMessage(), e);
      if (StringUtils.isNotEmpty(productCode)) {
        productService.setReviewPendingFlagToTrue(storeId, this.productCode);
      }
    }
  }

}
