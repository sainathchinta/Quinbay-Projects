package com.gdn.x.productcategorybase.service.impl;

import java.util.List;

import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHierarchyCacheEvictEventModel;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.context.ApplicationContext;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.Constants;

@Service
public class ApplicationCacheServiceBean {
  private static final Logger LOGGER = LoggerFactory.getLogger(ApplicationCacheServiceBean.class);
  private static final String UNDERSCORE_DELIMITER = "_";
  private static final String HYPHEN = "-";

  @Autowired
  private ApplicationContext applicationContext;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${caffeine.cache.enabled}")
  private boolean caffeineCacheEnabled;

  private ApplicationCacheServiceBean getApplicationCacheServiceBean() {
    return applicationContext.getBean(ApplicationCacheServiceBean.class);
  }

  @CacheEvict(value = CacheNames.CATEGORIES_BY_CATALOGTYPE, key = "#storeId + '_' + #catalogType" + ".toString()")
  public void evictCategoriesByCatalogTypeCache(String storeId, CatalogType catalogType) {
    LOGGER.debug("Evicting categories cache for storeId: {}, catalogType: {}", storeId, catalogType);
  }

  @CacheEvict(value = CacheNames.CHILD_CATEGORY_CACHE, key = "#storeId +'_'+ #parentCategoryId")
  public void evictChildCategoryCacheByParentCategoryId(String storeId, String parentCategoryId) {
    LOGGER.info("Evicting child category cache for storeId: {}, parentCategoryId: {}", storeId, parentCategoryId);
  }

  /**
   * Never use keys, it makes blocking all process
   */

  @Async
  @Deprecated
  public void evictAllCategoriesByCatalogTypeCache() {
    getApplicationCacheServiceBean().evictCategoriesByCatalogType();
    getApplicationCacheServiceBean().evictActiveCategoryTree();
  }

  @CacheEvict(value = CacheNames.ACTIVE_CATEGORY_TREE, key = "#storeId + '_' + #catalogType" + ".toString()")
  public void evictActiveCategoriesByCatalogIdCache(String storeId, CatalogType catalogType) {
    LOGGER.debug("Evicting active categories cache for storeId: {}, catalogType: {}", storeId, catalogType);
  }

  @Async
  public void clearCategoryDetails(String storeId, List<String> categoryIds) {
    categoryIds.forEach(
      categoryId -> getApplicationCacheServiceBean().evictCategoryDetailCache(storeId, categoryId));
  }

  public void evictCategoryHierarchyCacheByStoreIdAndCategoryCode(String storeId,
      String categoryCode) {
    LOGGER.debug("Evicting category hierarchy cache for storeId: {}, categoryCode: {}", storeId,
        categoryCode);
    if (caffeineCacheEnabled) {
      CategoryHierarchyCacheEvictEventModel categoryHierarchyCacheEvictEventModel =
          CategoryHierarchyCacheEvictEventModel.builder().storeId(storeId)
              .categoryCode(categoryCode).build();
      kafkaPublisher.send(kafkaTopicProperties.getCategoryHierarchyCaffeineCacheEvictEvent(),
          categoryCode, categoryHierarchyCacheEvictEventModel);
    }
    getApplicationCacheServiceBean().evictCategoryHierarchyCacheByStoreIdAndCategoryCodeFromRedis(
        storeId, categoryCode);
  }

  @CacheEvict(value = CacheNames.CATEGORY_HIERARCHY_CACHE, key = "#storeId +'_'+ #categoryCode")
  public void evictCategoryHierarchyCacheByStoreIdAndCategoryCodeFromRedis(String storeId,
      String categoryCode) {
    LOGGER.debug("Evicting category hierarchy cache for storeId: {}, categoryCode: {}", storeId,
        categoryCode);
  }

  @CacheEvict(cacheManager = Constants.CAFFEINE_CACHE_MANAGER, value =
      CacheNames.CATEGORY_HIERARCHY_CACHE, key = "#storeId +'_'+ #categoryCode")
  public void evictCategoryHierarchyByByStoreIdAndCategoryCodeFromCaffeine(String storeId,
      String categoryCode) {
    LOGGER.debug("Evicting category hierarchy caffeine cache for storeId: {}, categoryCode: {}",
        storeId, categoryCode);
  }

  @CacheEvict(value = CacheNames.PRODUCT_CACHE, key = "#storeId +'_'+ #productCode")
  public void evictProductCacheByStoreIdAndProductCode(String storeId, String productCode) {
    LOGGER.debug("Evicting product cache for storeId: {}, productCode: {}", storeId, productCode);
  }

  @CacheEvict(value = CacheNames.PRODUCT_CATEGORIES_CACHE, key = "#storeId +'_'+ #productId")
  public void evictProductCategoriesCacheByStoreIdAndProductId(String storeId, String productId) {
    LOGGER.debug("Evicting productCategory cache for storeId: {}, productId: {}", storeId, productId);
  }

  @CacheEvict(value = CacheNames.PRODUCT_ATTRIBUTES_CACHE, key = "#storeId +'_'+ #productId")
  public void evictProductAttributesCacheByStoreIdAndProductId(String storeId, String productId) {
    LOGGER.debug("Evicting productAttributes cache for storeId: {}, productId: {}", storeId, productId);
  }

  @CacheEvict(value = CacheNames.PRODUCT_ITEM_IMAGES_CACHE, key = "#storeId +'_'+ #productId")
  public void evictProductItemImagesCacheByStoreIdAndProductId(String storeId, String productId) {
    LOGGER.debug("Evicting productItemImages cache for storeId: {}, productItemId: {}", storeId, productId);
  }

  @CacheEvict(value = CacheNames.PRODUCT_IMAGES_CACHE, key = "#storeId +'_'+ #productId")
  public void evictProductImagesCacheByStoreIdAndProductId(String storeId, String productId) {
    LOGGER.debug("Evicting productImages cache for storeId: {}, productId: {}", storeId, productId);
  }

  @CacheEvict(value = CacheNames.PRODUCT_ITEMS_CACHE, key = "#storeId +'_'+ #productId")
  public void evictProductItemsCacheByStoreIdAndProductId(String storeId, String productId) {
    LOGGER.debug("Evicting productItems cache for storeId: {}, productId: {}", storeId, productId);
  }

  @CacheEvict(value = CacheNames.PRODUCT_ITEM_ATTRIBUTE_VALUES_CACHE, key = "#storeId +'_'+ #productId")
  public void evictProductItemAttributeValuesCacheByStoreIdAndProductId(String storeId, String productId) {
    LOGGER.debug("Evicting productItemAttributeValues cache for storeId: {}, productId: {}", storeId, productId);
  }

  @CacheEvict(value = CacheNames.CATEGORY_CACHE, key = "#storeId +'_'+ #categoryId")
  public void evictCategoryCacheByStoreIdAndCategoryId(String storeId, String categoryId) {
    LOGGER.debug("Evicting category cache for storeId: {}, categoryId: {}", storeId, categoryId);
  }

  @CacheEvict(value = CacheNames.ATTRIBUTE_CACHE, key = "#storeId +'_'+ #attributeId")
  public void evictAttributeCacheByStoreIdAndAttributeId(String storeId, String attributeId) {
    LOGGER.debug("Evicting attribute cache for storeId: {}, attributeId: {}", storeId, attributeId);
  }

  @CacheEvict(value = CacheNames.ALLOWED_ATTRIBUTE_VALUES_CACHE, key = "#storeId +'_'+ #attributeId")
  public void evictAllowedAttributeValuesCacheByStoreIdAndAttributeId(String storeId, String attributeId) {
    LOGGER.debug("Evicting allowedAttributeValues cache for storeId: {}, attributeId: {}", storeId, attributeId);
  }

  @CacheEvict(value = CacheNames.PREDEFINED_ALLOWED_ATTRIBUTE_VALUES_CACHE, key = "#storeId +'_'+ #attributeId")
  public void evictPredefinedAllowedAttributeValuesCacheByStoreIdAndAttributeId(String storeId, String attributeId) {
    LOGGER.debug("Evicting predefinedAllowedAttributeValues cache for storeId: {}, attributeId: {}", storeId, attributeId);
  }

  /**
   * Evict the redis cache of category restricted keyword mappings
   *
   * @param storeId
   * @param categoryCode
   */
  @CacheEvict(cacheManager = Constants.RESTRICTED_KEYWORD_CACHE_MANAGER, value =
      CacheNames.RESTRICTED_KEYWORDS_CACHE, key = "#storeId +'_'+ #categoryCode")
  public void evictCategoryRestrictedKeywordCache(String storeId, String categoryCode) {
  }

  /**
   * Evict brand authorization cache
   *
   * @param brandCode
   */
  @CacheEvict(value = CacheNames.BRAND_AUTHORISATION, key = "#brandCode")
  public void evictBrandAuthorizationCache(String brandCode) {
  }


  @CacheEvict(value = CacheNames.CATEGORIES_BY_CATALOGTYPE, allEntries = true)
  public void evictCategoriesByCatalogType() {
    LOGGER.debug("Evicting Categories by Catalog type");
  }

  @CacheEvict(value = CacheNames.ACTIVE_CATEGORY_TREE, allEntries = true)
  public void evictActiveCategoryTree() {
    LOGGER.debug("Evicting Active Categories");
  }

  @CacheEvict(value = CacheNames.CATEGORY_DETAIL, key = "#storeId + '_' + #categoryId")
  public void evictCategoryDetailCache(String storeId, String categoryId) {
    LOGGER.debug("Evicting category detail cache for storeId: {}, categoryId: {}", storeId,
      categoryId);
  }

  /**
   * Clear Brand cache whenever brand gets approved or rejected
   *
   * @param brandCode
   * @param brandName
   * @throws Exception
   */
  @Async
  public void deleteAllBrandCache(String storeId, String brandCode, String brandName) {
    getApplicationCacheServiceBean().evictBrandCache(storeId, brandCode);
    getApplicationCacheServiceBean().evictBrandCache(storeId, formatBrandName(brandName, true));
    getApplicationCacheServiceBean().evictBrandCache(storeId, formatBrandName(brandName, false));
  }

  private String formatBrandName(String brandName, boolean isActive) {
    return brandName.replace(StringUtils.SPACE, HYPHEN).toLowerCase() +
      (isActive ? UNDERSCORE_DELIMITER.concat(String.valueOf(true)) :
        UNDERSCORE_DELIMITER.concat(String.valueOf(false)));
  }

  /**
   * Evict brand authorization cache
   *
   * @param storeId
   */
  @CacheEvict(value = CacheNames.PROTECTED_BRANDS, key = "#storeId")
  public void evictProtectedBrandCache(String storeId) {
  }

  @CacheEvict(value = CacheNames.ACTIVE_CHILD_CATEGORY_CACHE, key = "#storeId +'_'+ #parentCategoryId")
  public void evictActiveChildCategoryCacheByParentCategoryId(String storeId,
    String parentCategoryId) {
    LOGGER.info(
      "Evicting child category cache for active categories, storeId: {}, " + "parentCategoryId: {}",
      storeId, parentCategoryId);
  }

  @CacheEvict(value = CacheNames.BRAND, key = "#storeId + '_' + #key")
  public void evictBrandCache(String storeId, String key) {
    LOGGER.debug("Evicting Brand Cache by : {}", key);
  }
}
