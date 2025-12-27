package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.ext.catalog.config.ExtCatalogDomainEventName;
import com.gdn.ext.catalog.model.entity.BlibliApprovedProductChange;
import com.gdn.x.product.domain.event.model.PristineDataItemEventModel;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.service.api.CacheEvictItemService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.PristineCacheableService;
import com.gdn.x.product.service.api.SystemParameterService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.beanutils.PropertyUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

/**
 * Created by govind on 12/09/2017 AD.
 */
@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.ext.catalog.approved.product.change.listener.enabled", havingValue = "true")
public class PristineEventListener {

  @Value("${storeid.default.value}")
  private String storeId;

  @Value("${product.visibility.switch.enabled}")
  private boolean isPristineSettingEnabled;

  @Autowired
  private ItemService itemService;

  @Autowired
  private PristineCacheableService pristineCacheableService;

  @Autowired
  private CacheEvictItemService cacheEvictItemService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = ExtCatalogDomainEventName.APPROVED_PRODUCT_CHANGE_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Listened to event : {} - {}", ExtCatalogDomainEventName.APPROVED_PRODUCT_CHANGE_EVENT,
      message);
    try {
      BlibliApprovedProductChange blibliApprovedProductChange =
        this.objectMapper.readValue(message, BlibliApprovedProductChange.class);
      if ((!Boolean.valueOf(systemParameterService.findValueByStoreIdAndVariable(storeId,
          SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED).getValue()))
          && this.isPristineSettingEnabled && StringUtils.isNotBlank(blibliApprovedProductChange.getProductId())) {
        PristineDataItemEventModel pristineDataItemEventModel =
            blibliApprovedProductChange.getPristineDataItemEventModel();
        if (pristineDataItemEventModel != null && StringUtils
            .isNotBlank(pristineDataItemEventModel.getPcbProductItemId())) {
          log.info("Event listening PristineData from Ext-catalog, pristineId {},  itemCode: {}",
            pristineDataItemEventModel.getPristineId(),
            pristineDataItemEventModel.getPcbProductItemId());
          PristineDataItem pristineItem = new PristineDataItem();
          PropertyUtils.copyProperties(pristineItem, pristineDataItemEventModel);
          itemService.updateItemsPristineDataByItemCode(pristineItem);
          pristineCacheableService.evictPristineItemAndSiblingsCacheAndRebuild(storeId, pristineItem);
          cacheEvictItemService.evictFindItemSkusByPristineId(storeId, pristineItem.getPristineId());
        }
      }
    } catch (Exception ex) {
      log.error("Error while Event listening PristineData from Ext-catalog , payload: {} ,",
          message, ex);
    }
  }
}
