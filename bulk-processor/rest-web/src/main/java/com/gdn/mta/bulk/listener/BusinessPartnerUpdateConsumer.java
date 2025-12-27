package com.gdn.mta.bulk.listener;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.mta.bulk.SellerProcessType;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.DormantSellerEvent;
import com.gdn.mta.bulk.service.DormantSellerService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.domain.event.config.BusinessPartnerDomainEventName;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

/**
 * Consumes Event for Business Partner Update
 * If business partner is change to INACTIVE, it will update products related to business partner
 * to un-buyable and stock will be reset to zero.
 *
 * @author anand
 * @since 2.4.0
 */
@Service
@Slf4j
public class BusinessPartnerUpdateConsumer {

  private final String MERCHANT_STATUS = "MERCHANT_STATUS";

  @Autowired
  private DormantSellerService dormantSellerService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${inactivated.merchant.status.list}")
  private String inactiveMerchantStatusList;

  @Value("${terminated.merchant.status.list}")
  private String terminatedMerchantStatusList;

  @Value("${dormant.seller.threshold}")
  private long dormantSellerThreshold;

  @Value("${dormant.seller.status.to.make.mfd.true}")
  private String dormantSellerStatuses;

  @Value("${check.merchant.status.changed}")
  private boolean checkMerchantStatusChanged;

  @Value("${seller.penalty.enabled.phase2}")
  private boolean sellerPenaltyEnabledPhase2;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBusinessPartnerUpdateEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consume event {} with message {} ", kafkaTopicProperties.getBusinessPartnerUpdateEvent(), message);
    try {
      BusinessPartnerChange businessPartnerChange = objectMapper.readValue(message, BusinessPartnerChange.class);
      String sellerProcessType = getSellerProcessType(businessPartnerChange);
      if (sellerPenaltyEnabledPhase2 && businessPartnerChange.isSuspensionFlag()) {
        log.info("onDomainEventConsumed: skipped event because of suspension flag");
        return;
      }
      if (StringUtils.isNotBlank(sellerProcessType) && isEligibleToProcess(businessPartnerChange, sellerProcessType)
          && isMerchantStatusChanged(businessPartnerChange)) {
        log.info("onDomainEventConsumed : Business partner termination product deactivation event consumed {} ",
            businessPartnerChange);
        dormantSellerService.processSellerDeactivate(businessPartnerChange.getBusinessPartnerCode(), sellerProcessType);
      }
    } catch (Exception e) {
      log.error("Error while consuming the event : {} and the payload : {} ", kafkaTopicProperties.getBusinessPartnerUpdateEvent(),
          message, e);
    }
  }

  private boolean isMerchantStatusChanged(BusinessPartnerChange businessPartnerChange) {
    if (checkMerchantStatusChanged) {
      return Objects.nonNull(businessPartnerChange.getCompany()) && CollectionUtils.isNotEmpty(
          businessPartnerChange.getCompany().getChangedFields()) && businessPartnerChange.getCompany()
          .getChangedFields().contains(MERCHANT_STATUS);
    }
    return true;
  }

  private boolean isEligibleToProcess(BusinessPartnerChange businessPartnerChange, String sellerProcessType) {
    List<String> statusToMarkMfdTrue = Arrays.asList(dormantSellerStatuses.split(Constant.COMMA));
    List<DormantSellerEvent> inProgressEvents = new ArrayList<>();
    List<DormantSellerEvent> dormantSellerEvents = dormantSellerService
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(Constant.STORE_ID,
            businessPartnerChange.getBusinessPartnerCode(), sellerProcessType);
    if (CollectionUtils.isNotEmpty(dormantSellerEvents)) {
      setStatusFailForInProgressEvents(inProgressEvents, dormantSellerEvents);
      getDormantSellerEventsToSetMfdTrue(statusToMarkMfdTrue, inProgressEvents, dormantSellerEvents);
      if (CollectionUtils.isNotEmpty(inProgressEvents)) {
        setMfdTrueForInProgressDormantSellerEvents(inProgressEvents);
      }
    }
    return !businessPartnerChange.isActivated() && CollectionUtils.isEmpty(dormantSellerService
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(Constant.STORE_ID,
            businessPartnerChange.getBusinessPartnerCode(), sellerProcessType));
  }

  private void getDormantSellerEventsToSetMfdTrue(List<String> statusToMarkMfdTrue, List<DormantSellerEvent> inProgressEvents,
      List<DormantSellerEvent> dormantSellerEvents) {
    inProgressEvents.addAll(dormantSellerEvents.stream()
        .filter(dormantSellerEvent -> statusToMarkMfdTrue.contains(dormantSellerEvent.getStatus()))
        .collect(Collectors.toList()));
  }

  private void setStatusFailForInProgressEvents(List<DormantSellerEvent> inProgressEvents, List<DormantSellerEvent> dormantSellerEvents) {
    inProgressEvents.addAll(dormantSellerEvents.stream()
        .filter(dormantSellerEvent -> dormantSellerEvent.getStatus().equals(Constant.DORMANT_SELLER_IN_PROGRESS))
        .filter(dormantSellerEvent -> (hoursDifferenceGreaterThanThreshold(dormantSellerEvent.getUpdatedDate())))
        .collect(Collectors.toList()));
    inProgressEvents.stream().map(dormantSellerEvent -> {
      dormantSellerEvent.setStatus(Constant.FAILED);
      return dormantSellerEvent;
    }).collect(Collectors.toList());
  }

  private boolean hoursDifferenceGreaterThanThreshold(Date createdDate) {
    Instant createdDateInstant = createdDate.toInstant();
    Instant currentDateInstant = new Date().toInstant();
    long hoursDifference = Duration.between(createdDateInstant, currentDateInstant).toHours();
    return hoursDifference > dormantSellerThreshold;
  }

  private void setMfdTrueForInProgressDormantSellerEvents(List<DormantSellerEvent> dormantSellerEvents) {
    for (DormantSellerEvent dormantSellerEvent : dormantSellerEvents) {
      dormantSellerEvent.setUpdatedDate(new Date());
      dormantSellerEvent.setMarkForDelete(true);
      dormantSellerEvent.setUpdatedBy(Constant.SYSTEM_ABORTED);
    }
    dormantSellerService.saveDormantSellerEvents(dormantSellerEvents);
  }

  private String getSellerProcessType(BusinessPartnerChange businessPartnerChange) {
    if (Arrays.asList(inactiveMerchantStatusList.split(Constant.COMMA))
        .contains((businessPartnerChange.getMerchantStatus()))) {
      return SellerProcessType.SUSPEND.name();
    } else if (Arrays.asList(terminatedMerchantStatusList.split(Constant.COMMA))
        .contains(businessPartnerChange.getMerchantStatus())) {
      return SellerProcessType.TERMINATED.name();
    } else {
      return StringUtils.EMPTY;
    }
  }
}