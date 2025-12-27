package com.gdn.x.product.service.config;

import com.gdn.x.product.dao.api.ProductRetryEventPublishRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.interceptor.CacheErrorHandler;

import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.RetryPublishStatus;
import com.gdn.x.product.model.entity.ProductRetryEventPublish;
import com.gdn.x.product.service.api.SkuValidator;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class CacheErrorHandlerAsMiss implements CacheErrorHandler {

  public CacheErrorHandlerAsMiss() {
  }

  @Autowired
  private ProductRetryEventPublishRepository productRetryEventPublishRepository;

  @Autowired
  private SkuValidator skuValidator;

  public void handleCacheClearError(RuntimeException exception, Cache cache) {
    log.error("Exception caught while clearing cache ", exception);
  }

  public void handleCacheEvictError(RuntimeException exception, Cache cache, Object key) {
    log.error("Exception caught while evicting cache key : {} ", key, exception);
    try {
      String identifier = key.toString().split(Constants.DEFAULT_STORE_ID)[1].substring(1);
      if (skuValidator.isItemSku(identifier)) {
        ProductRetryEventPublish productRetryEventPublish =
            ProductRetryEventPublish.builder().clearCache(Boolean.TRUE).retryCount(0).identifier(identifier)
                .retryPublishStatus(RetryPublishStatus.PENDING).topicName(ProductDomainEventName.ITEM_CHANGE_EVENT_NAME)
                .build();
        log.info("Saving for retry cache clear and publishing : {}", productRetryEventPublish);
        productRetryEventPublishRepository.save(productRetryEventPublish);
      }
    } catch (Exception e) {
      log.error("Exception while inserting to productRetryEventPublish : {} ", key, exception);
    }
  }

  public void handleCacheGetError(RuntimeException exception, Cache cache, Object key) {
    log.error("Exception caught while getting cache key : {} ", key, exception);
  }

  public void handleCachePutError(RuntimeException exception, Cache cache, Object key, Object value) {
    log.error("Exception caught while putting cache key : {} ", key, exception);
  }
}
