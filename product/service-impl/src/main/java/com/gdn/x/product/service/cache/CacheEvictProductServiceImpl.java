package com.gdn.x.product.service.cache;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.service.api.CacheEvictProductService;
import com.gdn.x.product.service.util.CommonUtil;

@Service
public class CacheEvictProductServiceImpl implements CacheEvictProductService {

  @Autowired
  @Qualifier("redisTemplateProductCombination")
  private RedisTemplate<String, Object> productRedisTemplate;

  @Value("${redis.product.enabled}")
  private boolean redisProductEnabled;


  @Override
//  @CacheEvict(cacheManager = Constants.PRODUCT_CACHE_MANAGER,
//              value = CacheNames.FIND_PRODUCT_BY_PRODUCT_CODE, key = "#storeId + '-' + #productCode")
  public void evictFindProductByProductCode(String storeId, String productCode) {
    if(redisProductEnabled) {
       productRedisTemplate.delete(CommonUtil.getProductRedisKey(productCode, storeId));
    }
  }

  @Override
  @CacheEvict(cacheManager = Constants.PRODUCT_CACHE_MANAGER, value = CacheNames.FIND_PRODUCT_BY_PRODUCT_CODE, key = "#storeId + '-' + #productCode")
  public void evictFindProductByProductCodeUsingCacheEvict(String storeId, String productCode) {

  }

  @Override
  @CacheEvict(cacheManager = Constants.PRODUCT_SKU_CACHE_MANAGER, value = {CacheNames.FIND_PRODUCT_BY_PRODUCT_SKU,
      CacheNames.FIND_PRODUCT_BY_PRODUCT_SKU_ALL}, key = "#storeId + '-' + #productSku")
  public void evictFindProductByProductSku(String storeId, String productSku) {

  }
}
