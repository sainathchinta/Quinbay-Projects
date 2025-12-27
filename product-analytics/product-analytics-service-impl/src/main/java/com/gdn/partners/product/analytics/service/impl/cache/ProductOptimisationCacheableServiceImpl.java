package com.gdn.partners.product.analytics.service.impl.cache;

import com.gdn.partners.product.analytics.model.CacheNames;
import com.gdn.partners.product.analytics.model.RedisConstants;
import com.gdn.partners.product.analytics.repository.ProductOptimisationRepository;
import com.gdn.partners.product.analytics.service.cache.ProductOptimisationCacheableService;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ProductOptimisationCacheableServiceImpl implements
    ProductOptimisationCacheableService {

  private final ProductOptimisationRepository productOptimisationRepository;

  @Override
  @Cacheable(cacheManager = RedisConstants.DEFAULT_REDIS, value = {
      CacheNames.PRODUCT_COUNTS}, key = "#sellerCode", unless = "#result == null")
  public long findProductCountCacheablesBySellerCode(String sellerCode) {
    return productOptimisationRepository.countBySellerCodeAndMarkForDeleteFalse(sellerCode);
  }

  @Override
  @CacheEvict(cacheManager = RedisConstants.DEFAULT_REDIS,
              value = CacheNames.PRODUCT_COUNTS, key = "#sellerCode")
  public void evictCacheBySellerCode(String sellerCode) {
  }
}
