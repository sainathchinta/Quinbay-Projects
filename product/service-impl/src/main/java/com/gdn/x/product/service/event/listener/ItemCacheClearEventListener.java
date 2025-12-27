package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ItemCacheClearModel;
import com.gdn.x.product.service.api.CacheEvictItemService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@ConditionalOnProperty(value = "com.gdn.x.product.item.cache.clear.event.listener.enabled",
                       havingValue = "true")
public class ItemCacheClearEventListener {

  @Autowired
  private ObjectMapper objectMapper;
  @Autowired
  private CacheEvictItemService cacheEvictItemService;

  @KafkaListener(topics = ProductDomainEventName.ITEM_CACHE_CLEAR_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Received event to clear cache : {} ", message);
    try {
      ItemCacheClearModel itemCacheClearModel =
        this.objectMapper.readValue(message, ItemCacheClearModel.class);
      String storeId = itemCacheClearModel.getStoreId();
      String productSku = itemCacheClearModel.getProductSku();
      String itemSku = itemCacheClearModel.getItemSku();
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productSku),
        ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(itemSku),
        ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
        ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
      this.cacheEvictItemService.evictFindItemByItemSku(storeId, itemSku);
      this.cacheEvictItemService.evictFindItemByProductSku(storeId, productSku);
    } catch (Exception e) {
      log.error("Error cache clear for : {}, error - ", message, e);
    }
  }
}
