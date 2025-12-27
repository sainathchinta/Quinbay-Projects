package com.gdn.partners.pbp.service.listener;

import java.util.UUID;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.EditedImageResizeEvent;
import com.gdn.mta.product.service.ImageProcessorService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ReviseImageResizeKafkaSubscriberBean {

  @Autowired
  private ImageProcessorService imageProcessorService;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${resize.image.channel.id.default}")
  private String channelId;

  @Value("${resize.image.authenticator.id.default}")
  private String authenticatorKey;

  @Value("${resize.image.client.id.default}")
  private String clientId;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.REVISE_IMAGE_RESIZE_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    EditedImageResizeEvent editedImageResizeEvent = objectMapper.readValue(message, EditedImageResizeEvent.class);
    try {
      setMandatoryParameters(editedImageResizeEvent.getStoreId());
      BulkResizeImageRequest bulkResizeImageRequest = new BulkResizeImageRequest();
      bulkResizeImageRequest.setGroupCode(editedImageResizeEvent.getProductCode());
      bulkResizeImageRequest.setImageRequests(editedImageResizeEvent.getImageRequests());
      imageProcessorService.resizeRevisedImage(bulkResizeImageRequest);
    } catch (Exception e) {
      log.error("Exception caught while processing edited image resize event , product code:{}",
          editedImageResizeEvent.getProductCode(), e);
    }
  }

  private void setMandatoryParameters(String storeId) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, this.authenticatorKey);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, this.clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, this.channelId);
  }
}
