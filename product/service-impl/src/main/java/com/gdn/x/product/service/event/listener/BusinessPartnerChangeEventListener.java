package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.businesspartner.domain.event.config.BusinessPartnerDomainEventName;
import com.gdn.x.businesspartner.domain.event.enums.CompanyChangeFields;
import com.gdn.x.product.model.entity.KafkaEventLogger;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import com.gdn.x.product.service.api.ItemPickupPointWrapperService;
import com.gdn.x.product.service.api.OfflineItemService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.gdn.x.product.service.api.KafkaEventLoggerService;

import java.util.Objects;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.businesspartner.profile.update.fields.listener.enabled"
    , havingValue = "true")
public class BusinessPartnerChangeEventListener {

  @Autowired
  private OfflineItemService offlineItemService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ItemPickupPointWrapperService itemPickupPointWrapperService;

  @Autowired
  private KafkaEventLoggerService kafkaEventLoggerService;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @KafkaListener(topics = BusinessPartnerDomainEventName.UPDATE_FIELDS, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("BusinessPartnerChangeEventListener consume event with message : {}", message);
    String kafkaEventLoggerId = StringUtils.EMPTY;
    try {
      BusinessPartnerChange businessPartnerChange = this.objectMapper.readValue(message,
        BusinessPartnerChange.class);
      String storeId = businessPartnerChange.getStoreId();
      String topicName = BusinessPartnerDomainEventName.UPDATE_FIELDS;
      String businessPartnerCode = businessPartnerChange.getBusinessPartnerCode();
      long timestamp = businessPartnerChange.getTimestamp();
      if (checkForExistingEventEntry(storeId, timestamp, businessPartnerCode,topicName)) {
        return;
      }
      KafkaEventLogger kafkaEventLogger = this.kafkaEventLoggerService.insertKafkaEventLogger(
          storeId, timestamp, businessPartnerCode, BusinessPartnerDomainEventName.UPDATE_FIELDS, BusinessPartnerDomainEventName.UPDATE_FIELDS, message);
      kafkaEventLoggerId = kafkaEventLogger.getId();
      if (businessPartnerChange.getCompany() != null &&
          businessPartnerChange.getCompany().getChangedFields().contains(CompanyChangeFields.CNC_ACTIVATED) &&
          !businessPartnerChange.getCompany().isCncActivated()) {
        offlineItemService.deleteOfflineItemByMerchantCode(businessPartnerChange.getStoreId(),
            businessPartnerChange.getBusinessPartnerCode(), businessPartnerChange.getUpdatedBy());

        if (!cncForWarehouseFeatureSwitch) {
          itemPickupPointWrapperService.updateMarkForDeleteByMerchantCode(
              businessPartnerChange.getStoreId(), businessPartnerChange.getBusinessPartnerCode());
        }
      }
      kafkaEventLoggerService.updateKafkaEventToFinished(kafkaEventLoggerId);
    } catch (Exception ex) {
      log.error("Error while processing BusinessPartner Update from X-BP, "
        + "businessPartnerChange: {}, error - ", message, ex);
      kafkaEventLoggerService.updateKafkaEventToFinished(kafkaEventLoggerId);

    }
  }

  private boolean checkForExistingEventEntry(String storeId, long timestamp, String businessPartnerCode,String topicName) {
    KafkaEventLogger kafkaEventLogger = this.kafkaEventLoggerService.findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(
        storeId, timestamp, businessPartnerCode, topicName);
    if (Objects.nonNull(kafkaEventLogger)) {
      return true;
    }
    return false;
  }
}
