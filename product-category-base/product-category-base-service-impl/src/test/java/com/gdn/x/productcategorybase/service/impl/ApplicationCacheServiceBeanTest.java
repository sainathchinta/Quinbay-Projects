package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.context.ApplicationContext;
import org.springframework.data.redis.core.RedisTemplate;

import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.dto.CustomCategoryDto;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;
import org.springframework.test.util.ReflectionTestUtils;

public class ApplicationCacheServiceBeanTest {

  private static final String ID = "id";
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "product-code";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_ATTRIBUTE_ID = "productAttributeId";
  private static final String CATALOGTOCATEGORIESKEY = "com.gdn.x:productcategorybase:catalogtype"
      + "-to-categories:*";
  private static final String ACTIVE_CATEGORY_TREE_KEYS = "com.gdn.x:productcategorybase:activeCategoryTree:*";
  private static final String REDISVALUE = "categoryValue";
  private static final String CATEGORY_CODE = "category-code";
  private static final String CATEGORY_ID = "categoryId";
  private static final String ATTRIBUTE_ID = "attributeId";
  private static final String PRODUCT_ITEM_ID = "item_id";
  private static final String SKU_CODE = "sku_code";
  private static final String BRAND_CODE = "brand_code";
  private static final String BRAND_NAME = "brand_name";

  private Set<String> finalParentCategoryKeys;
  private List<String> categoryCodes;

  @Mock
  private RedisTemplate<String, Product> productCache;

  @Mock
  private RedisTemplate<String, List<Category>> categoryHierarchyCache;

  @Mock
  private RedisTemplate<String, List<CustomCategoryDto>> categoryFromCatalogTypeCache;

  @Mock
  private Set<String> categories;

  @Mock
  private Set<String> categoryKeys;

  @InjectMocks
  private ApplicationCacheServiceBean service;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private KafkaPublisher kafkaPublisher;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;
  
  private Product product;
  private ProductItem productItem;

  @BeforeEach
  public void initialize() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.categoryKeys = new HashSet<>();
    this.categoryKeys.add(REDISVALUE);
    this.categories = new HashSet<String>();
    
    this.categoryCodes = new ArrayList<>();
    this.categoryCodes.add(CATEGORY_CODE);

    this.finalParentCategoryKeys = new HashSet<String>();
    this.finalParentCategoryKeys.add(REDISVALUE);
    
    this.product = new Product();
    this.product.setId(ApplicationCacheServiceBeanTest.ID);
    this.product.setProductCode(ApplicationCacheServiceBeanTest.PRODUCT_CODE);

    this.productItem = new ProductItem();
    this.productItem.setId(PRODUCT_ITEM_ID);
    this.productItem.setSkuCode(SKU_CODE);
    this.productItem.setProduct(this.product);

    this.product.setProductItems(Arrays.asList(productItem));

    Mockito.when(this.applicationContext.getBean(ApplicationCacheServiceBean.class)).thenReturn(service);
    Mockito.when(this.categoryHierarchyCache.keys(Mockito.anyString())).thenReturn(this.categories);
  }

  @AfterEach
  public void postTest() throws Exception {
    verifyNoMoreInteractions(this.productCache);
    verifyNoMoreInteractions(this.categoryHierarchyCache);
    verifyNoMoreInteractions(this.categoryFromCatalogTypeCache);
    verifyNoMoreInteractions(this.kafkaPublisher);
    verifyNoMoreInteractions(this.kafkaTopicProperties);
  }

  @Test
  public void testEvictCategoryDetail_keyDoestExist_dontDoAnything() {
    this.service.evictCategoryDetailCache("10001", "123");
  }


  @Test
  public void testEvictCategoryDetail_keyExist_remove() {
    this.service.evictCategoryDetailCache("10001", "123");
  }

  @Test
  public void testEvictCategoryDetails_keyDoestExist_dontDoAnything() {
    List<String> categoryIds = new ArrayList<>();
    categoryIds.add("category_id_1");
    this.service.clearCategoryDetails("10001", categoryIds);
  }


  @Test
  public void testEvictCategoryDetails_keyExist_remove() {
    List<String> categoryIds = new ArrayList<>();
    categoryIds.add("category_id_1");
    this.service.clearCategoryDetails("10001", categoryIds);
  }

  @Test
  public void evictProductCacheByStoreIdAndProductCode() {
    service.evictProductCacheByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void evictProductCategoriesCacheByStoreIdAndProductId() {
    service.evictProductCategoriesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void evictProductAttributesCacheByStoreIdAndProductId() {
    service.evictProductAttributesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void evictProductItemImagesCacheByStoreIdAndProductItemId() {
    service.evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ITEM_ID);
  }

  @Test
  public void evictProductImagesCacheByStoreIdAndProductId() {
    service.evictProductImagesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void evictProductItemsCacheByStoreIdAndProductId() {
    service.evictProductItemsCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void evictProductItemAttributeValuesCacheByStoreIdAndProductItemId() {
    service.evictProductItemAttributeValuesCacheByStoreIdAndProductId(STORE_ID, PRODUCT_ITEM_ID);
  }

  @Test
  public void evictCategoryCacheByStoreIdAndCategoryId() {
    service.evictCategoryCacheByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void evictAttributeCacheByStoreIdAndAttributeId() {
    service.evictAttributeCacheByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
  }

  @Test
  public void evictAllowedAttributeValuesCacheByStoreIdAndAttributeId() {
    service.evictAllowedAttributeValuesCacheByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
  }

  @Test
  public void evictPredefinedAllowedAttributeValuesCacheByStoreIdAndAttributeId() {
    service.evictPredefinedAllowedAttributeValuesCacheByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
  }

  @Test
  public void evictCategoryRestrictedKeywordCache() {
    service.evictCategoryRestrictedKeywordCache(STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void deleteAllBrandCacheTest() throws Exception {
    this.service.deleteAllBrandCache(STORE_ID, BRAND_CODE, BRAND_NAME);
  }

  @Test
  public void deleteAllBrandCacheFalseTest() throws Exception {
    this.service.deleteAllBrandCache(STORE_ID, BRAND_CODE, BRAND_NAME);
  }

  @Test
  public void evictAllCategoriesByCatalogTypeCacheTest() {
    when(categoryFromCatalogTypeCache.keys(CacheNames.CATEGORIES_BY_CATALOGTYPE + ":*")).thenReturn(categoryKeys);
    when(categoryFromCatalogTypeCache.keys(CacheNames.ACTIVE_CATEGORY_TREE + ":*")).thenReturn(categoryKeys);
    service.evictAllCategoriesByCatalogTypeCache();
  }

  @Test
  public void evictChildCategoryCacheByParentCategoryIdTest() {
    service.evictChildCategoryCacheByParentCategoryId(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void evictCategoriesByCatalogTypeCacheTest() {
    service.evictCategoriesByCatalogTypeCache(STORE_ID, CatalogType.MASTER_CATALOG);
  }

  @Test
  public void evictActiveCategoriesByCatalogIdCacheTest() {
    service.evictActiveCategoriesByCatalogIdCache(STORE_ID, CatalogType.MASTER_CATALOG);
  }


  @Test
  public void evictCategoryHierarchyCacheByStoreIdAndCategoryCodeTest() {
    service.evictCategoryHierarchyCacheByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void evictCategoryHierarchyCacheByStoreIdAndCategoryCodeCaffeineTest() {
    service.evictCategoryHierarchyByByStoreIdAndCategoryCodeFromCaffeine(STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void evictCategoryHierarchyCacheByStoreIdAndCategoryCodeCaffeineOnTest() {
    ReflectionTestUtils.setField(service, "caffeineCacheEnabled", true);
    service.evictCategoryHierarchyCacheByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(kafkaPublisher).send(Mockito.any(), Mockito.eq(CATEGORY_CODE), Mockito.any());
    verify(kafkaTopicProperties).getCategoryHierarchyCaffeineCacheEvictEvent();
  }

  @Test
  public void evictBrandAuthorizationCacheTest() {
    service.evictBrandAuthorizationCache(BRAND_CODE);
  }

  @Test
  public void evictProtectedBrandListCacheTest() {
    service.evictProtectedBrandCache(STORE_ID);
  }

  @Test
  public void evictActiveChildCategoriesCacheTest(){
      service.evictActiveChildCategoryCacheByParentCategoryId(STORE_ID, CATEGORY_ID);
    }

}