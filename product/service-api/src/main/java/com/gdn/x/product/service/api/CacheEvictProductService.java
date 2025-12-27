package com.gdn.x.product.service.api;

public interface CacheEvictProductService {

  void evictFindProductByProductCode(String storeId, String productCode);

  void evictFindProductByProductSku(String storeId, String productSku);

  void evictFindProductByProductCodeUsingCacheEvict(String storeId, String productCode);
}
