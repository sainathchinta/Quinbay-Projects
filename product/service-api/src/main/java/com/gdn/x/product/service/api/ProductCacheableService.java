package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.model.entity.Product;

public interface ProductCacheableService {

  List<Product> findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId,
      String productCode);

  List<Product> findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
      String storeId, String productCode);

  Product findProductByStoreIdAndProductSku(String storeId, String productSku);

  Product findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku);

  Product findProductByStoreIdAndProductSkuAndIncludeMarkForDelete(String storeId, String productSku,
      boolean includeMfd);

  Map<String, Product> findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(String storeId,
      Set<String> productSkus);

  List<String> getCategoryCodesCachedByAttributeCode(String storeId, String attributeCode);

}
