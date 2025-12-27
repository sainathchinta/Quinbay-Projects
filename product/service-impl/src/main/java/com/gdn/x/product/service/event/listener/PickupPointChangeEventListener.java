package com.gdn.x.product.service.event.listener;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.x.product.domain.event.enums.PickupPointChangeFields;
import com.gdn.x.product.domain.event.model.PickupPointChange;
import com.gdn.x.product.service.api.ChannelService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.businesspartner.domain.event.config.PickupPointDomainEventName;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.KafkaEventLogger;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PickupPoint;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.KafkaEventLoggerService;
import com.gdn.x.product.service.api.OfflineItemService;
import com.gdn.x.product.service.api.PickupPointService;
import com.gdn.x.product.service.util.CommonUtil;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.businesspartner.pickuppoint.update.fields.listener"
    + ".enabled", havingValue = "true")
public class PickupPointChangeEventListener {

  @Autowired
  private OfflineItemService offlineItemService;

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private KafkaEventLoggerService kafkaEventLoggerService;

  @Autowired
  private ChannelService channelService;

  @Value("${delete.pickup.point.switch}")
  private boolean deletePickupPointSwitch;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${seller.action.types.skip.pp.update}")
  private String sellerActionTypesToSkipPPUpdate;

  @KafkaListener(topics = PickupPointDomainEventName.UPDATE_FIELDS, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("PickupPointUpdateEventListener consume event with pickupPointChange : {}", message);
    String kafkaEventLoggerId = StringUtils.EMPTY;
    try {
      PickupPointChange pickupPointChange = this.objectMapper.readValue(message,
        PickupPointChange.class);
      String storeId = pickupPointChange.getStoreId();
      String merchantCode = pickupPointChange.getBusinessPartnerCode();
      String pickupPointCode = pickupPointChange.getCode();
      long timestamp = pickupPointChange.getTimestamp();
      if(checkForExistingEventEntryAndSellerStatus(storeId, timestamp, pickupPointChange)) {
        return;
      }
      KafkaEventLogger kafkaEventLogger =
        this.kafkaEventLoggerService.insertKafkaEventLogger(storeId, timestamp, pickupPointCode,
          merchantCode, PickupPointDomainEventName.UPDATE_FIELDS, message);
      kafkaEventLoggerId = kafkaEventLogger.getId();
      if (pickupPointChange.getChangedFields().contains(PickupPointChangeFields.CNC_ACTIVATED)
          && !pickupPointChange.isCncActivated()) {
        List<ItemPickupPoint> itemPickupPointList;
        if (cncForWarehouseFeatureSwitch) {
          itemPickupPointList =
              itemPickupPointService.findByStoreIdAndPickupPointCodeAndConfigFlagValuesAndMarkForDeleteAndChannel(
                  storeId, pickupPointCode, true, false, channelService.getCncChannel());
        } else {
          itemPickupPointList =
              itemPickupPointService.findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(
                  storeId, pickupPointCode, true, false);
        }
        List<OfflineItem> offlineItems = CommonUtil.getOfflineItemsByItemPickupPoint(itemPickupPointList, false, null);
        offlineItemService.updateItemPickupPointForCNCDeactivation(storeId, itemPickupPointList, merchantCode,
            pickupPointCode);
        offlineItemService.validateAndUpdateProductItemCncActivatedFalse(storeId, offlineItems,
            pickupPointChange.getUpdatedBy(), Constants.PICKUP_POINT_CHANGE);
        offlineItemService.validateAndUpdateProductCncActivatedFalse(storeId, itemPickupPointList,
            pickupPointChange.getUpdatedBy(), Constants.PICKUP_POINT_CHANGE);
      }
      if (!deletePickupPointSwitch) {
      if (pickupPointChange.getChangedFields().contains(PickupPointChangeFields.EXTERNAL_PICKUP_POINT_CODE) || (
          pickupPointChange.getChangedFields().contains(PickupPointChangeFields.ARCHIVED)
              && pickupPointChange.isArchived())) {
        List<ItemPickupPoint> itemPickupPointList =
            itemPickupPointService.findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(storeId, pickupPointCode);
        offlineItemService.deleteByPickupPointCode_ItemPickupPoint(storeId, pickupPointCode);
        List<OfflineItem> offlineItems = CommonUtil.getOfflineItemsByItemPickupPoint(itemPickupPointList, false, null);
        offlineItemService.validateAndUpdateProductItemCncActivatedFalse(storeId, offlineItems,
            pickupPointChange.getUpdatedBy(), Constants.PICKUP_POINT_CHANGE);
        offlineItemService.validateAndUpdateProductCncActivatedFalse(storeId, itemPickupPointList,
            pickupPointChange.getUpdatedBy(), Constants.PICKUP_POINT_CHANGE);
      }
      }
      if (cncForWarehouseFeatureSwitch && pickupPointChange.getChangedFields()
          .contains(PickupPointChangeFields.DELIVERY_FLAG) && !pickupPointChange.getFlags()
          .isDeliveryFlag()) {
        List<ItemPickupPoint> itemPickupPointList =
            itemPickupPointService.findByStoreIdAndPickupPointCodeAndConfigFlagValuesAndMarkForDeleteAndChannel(
                storeId, pickupPointCode, true, false, channelService.getDefaultChannel());
        offlineItemService.updateItemPickupPointForDeliveryDeactivation(storeId,
            itemPickupPointList, merchantCode, pickupPointCode);
      }
      PickupPoint pickupPoint = new PickupPoint();
      pickupPoint.setPickupPointCode(pickupPointCode);
      pickupPoint.setCncActivated(pickupPointChange.isCncActivated());
      pickupPointService.upsertPickupPoint(storeId, pickupPoint, Constants.DEFAULT_USERNAME);
      kafkaEventLoggerService.updateKafkaEventToFinished(kafkaEventLoggerId);
    } catch (Exception ex) {
      log.error("Error while listening PickupPointChange from X-BP, payload:{}, error - ",
        message, ex);
      kafkaEventLoggerService.updateKafkaEventToFinished(kafkaEventLoggerId);
    }
  }

  private boolean checkForExistingEventEntryAndSellerStatus(String storeId, long timestamp,
    PickupPointChange pickupPointChange) {
    Set<String> sellerActionTypesToSkipPPUpdate = getSellerActionTypesToSkipPPUpdate();
    if (CollectionUtils.isNotEmpty(sellerActionTypesToSkipPPUpdate)
      && CollectionUtils.isNotEmpty(pickupPointChange.getActionTypes())
      && !Collections.disjoint(sellerActionTypesToSkipPPUpdate, pickupPointChange.getActionTypes())) {
      return true;
    }
    KafkaEventLogger kafkaEventLogger =
      this.kafkaEventLoggerService.findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(storeId,
        timestamp, pickupPointChange.getCode(), pickupPointChange.getBusinessPartnerCode());
    if (Objects.nonNull(kafkaEventLogger)) {
      return true;
    }
    return false;
  }

  public Set<String> getSellerActionTypesToSkipPPUpdate() {
    return Stream.of(StringUtils.split(
      sellerActionTypesToSkipPPUpdate, Constants.COMMA)).collect(Collectors.toSet());
  }
}
