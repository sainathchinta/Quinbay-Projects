package com.gdn.x.product.service.cache;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.util.CommonUtil;
import static java.util.function.Function.identity;

@Service
public class ProductCacheableServiceImpl implements ProductCacheableService {

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Autowired
  private ApplicationContext applicationContext;

  @Autowired
  @Qualifier("redisTemplateProductCombination")
  private RedisTemplate<String, Object> productRedisTemplate;

  @Value("${redis.product.enabled}")
  private boolean redisProductEnabled;

  @Value("${redis.cache.product.expiration}")
  private String redisProductExpiration;

  @Value("${redis.cache.product.sync.expiration}")
  private String redisProductSyncExpiration;

  @Override
  public List<Product> findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId,
      String productCode) {
    if (redisProductEnabled) {
      List<Product> products = (List<Product>) productRedisTemplate.boundValueOps(CommonUtil.getProductRedisKey(productCode, storeId)).get();
      if (CollectionUtils.isNotEmpty(products)) {
        return products;
      }
    }
    Set<String> productCodes = new HashSet<>();
    productCodes.add(productCode);
    List<Product> products = productRepository.findByStoreIdAndProductCodeInAndMarkForDeleteFalse(storeId, productCodes);
    for (Product product : products) {
      if (product.isSynchronized()) {
        product.setMasterDataProduct(null);
      }
    }
    if (redisProductEnabled) {
      productRedisTemplate.boundValueOps(CommonUtil.getProductRedisKey(productCode, storeId)).set(products);
      productRedisTemplate
          .expire(CommonUtil.getProductRedisKey(productCode, storeId), Long.valueOf(redisProductExpiration),
              TimeUnit.SECONDS);
    }
    return products;
  }

  @Override
  public List<Product> findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
      String storeId, String productCode) {
    List<Product> products =
        (List<Product>) productRedisTemplate.boundValueOps(CommonUtil.getProductRedisKey(productCode, storeId)).get();
    return products.stream().filter(Product::isSynchronized).collect(Collectors.toList());
  }

  @Override
  @Cacheable(cacheManager = Constants.PRODUCT_SKU_CACHE_MANAGER, value = {
      CacheNames.FIND_PRODUCT_BY_PRODUCT_SKU_ALL}, key = "#storeId + '-' + #productSku", unless = "#result == null")
  public Product findProductByStoreIdAndProductSku(String storeId, String productSku) {
    return productRepository.findProductByStoreIdAndProductSku(storeId, productSku, false);
  }

  private ProductCacheableService getProductCacheableServiceBean() {
    return applicationContext.getBean(ProductCacheableService.class);
  }

  @Override
  public Product findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId,
      String productSku) {
    Product product = getProductCacheableServiceBean().findProductByStoreIdAndProductSku(storeId, productSku);
    if (Objects.isNull(product) || product.isMarkForDelete()) {
      return null;
    }
    return product;
  }

  @Override
  public Product findProductByStoreIdAndProductSkuAndIncludeMarkForDelete(String storeId, String productSku,
      boolean includeMarkForDelete) {
    Product product = getProductCacheableServiceBean().findProductByStoreIdAndProductSku(storeId, productSku);
    if (Objects.isNull(product) || (!includeMarkForDelete && product.isMarkForDelete())) {
      return null;
    }
    return product;
  }
  
  @Override
  public Map<String, Product> findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(
      String storeId, Set<String> productSkus) {
    List<Product> products = productRepository
        .findProductByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId, productSkus);
    return products.stream().collect(
        Collectors.toMap(Product::getProductSku, identity(), (oldValue, newValue)->newValue));
  }

  @Override
  @Cacheable(cacheManager = Constants.CATEGORY_CACHE_MANAGER, value = {
      CacheNames.FIND_CATEGORY_CODES_BY_ATTRIBUTE_CODE}, key = "#storeId + '-' + #attributeCode",
             unless = "#result == null")
  public List<String> getCategoryCodesCachedByAttributeCode(String storeId, String attributeCode) {
    return productCategoryBaseOutbound.getCategoryCodesByAttributeCode(storeId, attributeCode);
  }

}
