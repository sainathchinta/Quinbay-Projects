package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.PickupPointDeleteProcessDTO;
import com.gdn.mta.bulk.service.PickupPointDeleteService;
import com.gdn.x.businesspartner.domain.event.model.PickupPointChange;
import com.gdn.x.product.enums.Constants;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import static com.google.common.base.Preconditions.checkArgument;

import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
@Slf4j
public class PickupPointDeleteListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${seller.action.types.skip.pp.deletion}")
  private String sellerActionTypesToSkipPPDeletion;

  @Value("${change.fields.for.pp.deletion}")
  private String changeFieldsForPPDeletion;

  @Autowired
  private PickupPointDeleteService pickupPointDeleteService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getUpdateFields()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Received event for topic : {} with message : {}", kafkaTopicProperties.getUpdateFields(),
      message);
    try {
      PickupPointChange pickupPointChangeModel =
        this.objectMapper.readValue(message, PickupPointChange.class);

      if (isEligibleForPickupPointDeletion(pickupPointChangeModel)) {
        String storeId = pickupPointChangeModel.getStoreId();
        String businessPartnerCode = pickupPointChangeModel.getBusinessPartnerCode();
        String pickupPointCode = pickupPointChangeModel.getCode();
        String createdBy = pickupPointChangeModel.getCreatedBy();
        String updatedBy = pickupPointChangeModel.getUpdatedBy();

        checkArgument(StringUtils.isNotBlank(businessPartnerCode),
          BulkProcessValidationErrorMessages.BUSINESS_PARTNER_CODE_CANNOT_BE_BLANK);
        checkArgument(StringUtils.isNotBlank(pickupPointCode),
          BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK_EN);
        checkArgument(StringUtils.isNotBlank(storeId),
          BulkProcessValidationErrorMessages.STORE_ID_CANNOT_BE_BLANK);

        PickupPointDeleteProcessDTO pickupPointDeleteProcessDTO =
          PickupPointDeleteProcessDTO.builder().storeId(storeId)
            .businessPartnerCode(businessPartnerCode).pickupPointCode(pickupPointCode)
            .updatedBy(updatedBy).createdBy(createdBy).build();
        pickupPointDeleteService.processDeletePickupPointEvent(pickupPointDeleteProcessDTO);
      }
    } catch (Exception e) {
      log.error("Error while process bulk delete instant pickup event with payload : {}, error - ",
        message, e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
        "Error while listening and processing "
          + "BULK_DELETE_INSTANT_PICKUP_ITEM_EVENT bulkUpdateEventModel : " + message
          + ", error is : " + e.getMessage(), e);
    }
  }

  private boolean isEligibleForPickupPointDeletion(PickupPointChange pickupPointChangeModel) {
    Set<String> sellerActionTypesToSkipPPUpdate = getSellerActionTypesToSkipPPDeletion();
    Set<String> changeFieldsToProcessPPDeletion = getChangeFieldsToProcessPPDeletion();
    // Check if actionTypes has common elements with SELLER_TERMINATION,SELLER_RESIGN
    if (CollectionUtils.isNotEmpty(sellerActionTypesToSkipPPUpdate) && CollectionUtils.isNotEmpty(
      pickupPointChangeModel.getActionTypes())) {
      if (!Collections.disjoint(sellerActionTypesToSkipPPUpdate, pickupPointChangeModel.getActionTypes())) {
        // Return false immediately if SELLER_TERMINATION,SELLER_RESIGN are found
        return false;
      }
    }
    return !Collections.disjoint(pickupPointChangeModel.getChangedFields(), changeFieldsToProcessPPDeletion)
      && pickupPointChangeModel.isWaitingDeletion();
  }


  public Set<String> getSellerActionTypesToSkipPPDeletion() {
    return Stream.of(StringUtils.split(sellerActionTypesToSkipPPDeletion, Constants.COMMA))
      .collect(Collectors.toSet());
  }

  public Set<String> getChangeFieldsToProcessPPDeletion() {
    return Stream.of(StringUtils.split(changeFieldsForPPDeletion, Constants.COMMA))
      .collect(Collectors.toSet());
  }

}
