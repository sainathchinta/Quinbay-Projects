package com.gdn.x.mta.distributiontask.inbound.impl;

import java.util.Objects;
import java.util.UUID;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.PublishAndSavedProductAndHistoryModel;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class PBPAutoApprovalCheckEventListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductServiceRepository productServiceRepository;

  @Autowired
  private ProductService productService;

  @KafkaListener(topics = DomainEventName.PRODUCT_AUTO_APPROVAL_CHECK, autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Consume event {} with message {} ", DomainEventName.PRODUCT_AUTO_APPROVAL_CHECK, message);
    try {
      AutoApprovalTypeRequest autoApprovalTypeRequest = objectMapper.readValue(message, AutoApprovalTypeRequest.class);
      setMandatoryParameters(autoApprovalTypeRequest.getStoreId());
      GdnRestSingleResponse<AutoApprovalTypeResponse> autoApprovalTypeResponse = productServiceRepository
          .getAutoApprovalType(autoApprovalTypeRequest.getStoreId(), autoApprovalTypeRequest.getProductCode(), false,
              new AutoApprovalTypeRequest(autoApprovalTypeRequest.getCategoryCode(), autoApprovalTypeRequest.isEdited(),
                  autoApprovalTypeRequest.getReviewType(), autoApprovalTypeRequest.isRevised()));
      if (!autoApprovalTypeResponse.isSuccess() || Objects.isNull(autoApprovalTypeResponse.getValue())) {
        log.error("Failed to get response from PBP while auto approving product : {} ",
            autoApprovalTypeRequest.getProductCode(), autoApprovalTypeResponse.getErrorMessage());
      } else if (Constants.CONTENT_AND_IMAGE.equalsIgnoreCase(
          autoApprovalTypeResponse.getValue().getAutoApprovalType())) {
        PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
            productService.autoApproveProduct(autoApprovalTypeRequest.getProductCode());
        if (Objects.nonNull(publishAndSavedProductAndHistoryModel.getInternalHistoryEventModel())) {
          productService.publishInternalHistoryEventForProduct(
              publishAndSavedProductAndHistoryModel.getInternalHistoryEventModel());
        }
      }
    } catch (Exception e) {
      log.error("Error while consuming auto approval criteria check event message : {} ", message, e);
    }
  }

  private void setMandatoryParameters(String storeId) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
  }
}
