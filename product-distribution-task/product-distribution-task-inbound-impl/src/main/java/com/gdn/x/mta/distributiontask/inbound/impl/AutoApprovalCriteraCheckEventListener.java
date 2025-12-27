package com.gdn.x.mta.distributiontask.inbound.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Objects;
import java.util.UUID;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductAutoApprovalEventModel;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import com.gdn.x.mta.distributiontask.service.api.ProductAutoApprovalService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class AutoApprovalCriteraCheckEventListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductWrapperService productWrapperService;

  @Autowired
  private ProductServiceRepository productServiceRepository;

  @Autowired
  private ProductAutoApprovalService productAutoApprovalService;

  @KafkaListener(topics = DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME,
                 autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    try {
      ProductAutoApprovalEventModel productAutoApprovalEvent =
          objectMapper.readValue(message, ProductAutoApprovalEventModel.class);
      setMandatoryParameters(productAutoApprovalEvent.getStoreId());
      log.info("Consume auto approval criteria check event message : {}", productAutoApprovalEvent);
      String productState = productServiceRepository.getProductStatus(productAutoApprovalEvent.getStoreId(),
          productAutoApprovalEvent.getProductCode());
      if(!Constants.ACTIVE.equals(productState) && !productAutoApprovalEvent.isOnlyCategoryUpdate()){
        log.info("Product is not active for productCode : {} ", productAutoApprovalEvent.getProductCode());
        productAutoApprovalService.addProductsToAutoApprovalTable(productAutoApprovalEvent.getStoreId(),
            new ArrayList<>(Arrays.asList(productAutoApprovalEvent.getProductCode())),
          Collections.emptyMap());
      } else {
        AutoApprovalTypeRequest autoApprovalTypeRequest =
            new AutoApprovalTypeRequest(productAutoApprovalEvent.getCategoryCode(),
                productAutoApprovalEvent.getEdited(), productAutoApprovalEvent.getReviewType(),
                productAutoApprovalEvent.isRevised());
        autoApprovalTypeRequest.setDestinationCategoryCode(productAutoApprovalEvent.getDestinationCategoryCode());
        GdnRestSingleResponse<AutoApprovalTypeResponse> autoApprovalTypeResponse =
            productServiceRepository.getAutoApprovalType(productAutoApprovalEvent.getStoreId(),
                productAutoApprovalEvent.getProductCode(), productAutoApprovalEvent.isOnlyCategoryUpdate(),
                autoApprovalTypeRequest);
        if (!autoApprovalTypeResponse.isSuccess() || Objects.isNull(autoApprovalTypeResponse.getValue())) {
          log.error("Failed to get response from PBP while auto approving product : {} ",
              productAutoApprovalEvent.getProductCode(), autoApprovalTypeResponse.getErrorMessage());
          productAutoApprovalService.updateProductAutoApprovalDetailsByProductCode(productAutoApprovalEvent.getStoreId(),
              productAutoApprovalEvent.getProductCode(), AutoApprovalStatus.FAILED, true);
        } else {
          boolean isEligibleForAutoApproval = Constants.CONTENT_AND_IMAGE.equalsIgnoreCase(autoApprovalTypeResponse.getValue().getAutoApprovalType());
          productWrapperService.autoApproveOfPendingProductsAfterEligibilityCheck(productAutoApprovalEvent.getStoreId(),
              productAutoApprovalEvent.getProductCode(), isEligibleForAutoApproval,
              autoApprovalTypeResponse.getValue());
        }
      }
    } catch (Exception e) {
      log.error("Error while consuming auto approval criteria check event message : {} ",
          message, e);
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
