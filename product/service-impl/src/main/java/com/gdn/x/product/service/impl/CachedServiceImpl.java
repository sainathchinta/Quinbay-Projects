package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Objects;
import java.util.stream.Collectors;

import com.gdn.x.product.constants.CommonConstants;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.service.api.CachedService;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

@Service
public class CachedServiceImpl implements CachedService {

  private static final Logger LOGGER = LoggerFactory.getLogger(CachedServiceImpl.class);
  private static final String LIST_OF_CATEGORIES_FROM_PRODUCT_CATEGORY_BASE_RETURNED_NULL =
      "listOfCategories from product category base returned null";
  private static final String REQUEST_ID_MUST_NOT_BE_BLANK = "requestId must not be blank";
  private static final String CATEGORY_CODE_MUST_NOT_BE_BLANK = "categoryCode must not be blank";
  private static final String PRODUCT_CODE_MUST_NOT_BE_BLANK = "productCode must not be blank";

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Autowired
  @Qualifier("redisTemplateCategoryCombination")
  private RedisTemplate categoryRedisTemplate;

  @Override
  @Cacheable(cacheManager = Constants.CATEGORY_CACHE_MANAGER, value = {
      CacheNames.PARENT_CATEGORIES}, key = "#categoryCode", unless = "#result == null")
  public List<CategoryResponse> getParentCategoriesFromMasterData(String requestId,
      String username, String categoryCode) {
    checkArgument(StringUtils.isNotBlank(requestId), CachedServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(categoryCode),
        CachedServiceImpl.CATEGORY_CODE_MUST_NOT_BE_BLANK);
    List<CategoryResponse> listOfCategories = null;
    try {
      listOfCategories =
          this.productCategoryBaseOutbound.getCategoryHierarchy(requestId, username, categoryCode);
    } catch (Exception e) {
      CachedServiceImpl.LOGGER.error("categoryCode: {}, with error: {}", categoryCode,
          e.getMessage(), e);
    }
    checkState(listOfCategories != null && !listOfCategories.isEmpty(),
        CachedServiceImpl.LIST_OF_CATEGORIES_FROM_PRODUCT_CATEGORY_BASE_RETURNED_NULL);
    return listOfCategories;
  }

  @Override
  public Map<String, List<CategoryResponse>> getParentCategoriesFromDbAndCache(String requestId, String username,
      Set<String> categoryCodes) {
    checkArgument(StringUtils.isNotBlank(requestId), CachedServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(categoryCodes),
            CachedServiceImpl.CATEGORY_CODE_MUST_NOT_BE_BLANK);
    categoryCodes.removeIf(String::isBlank);
    checkArgument(CollectionUtils.isNotEmpty(categoryCodes),
            CachedServiceImpl.CATEGORY_CODE_MUST_NOT_BE_BLANK);
    Map<String, List<CategoryResponse>> categoryResponseMap = new HashMap<>();

    //generate cache keys and get the value from cache using multiget And create a response map by using the category code of first object as key
    Set<String> cacheKeys = categoryCodes.stream()
        .map(categoryCode -> CacheNames.PARENT_CATEGORIES + CommonConstants.CACHE_SEPARATOR + categoryCode)
        .collect(Collectors.toSet());
    for (Object cacheObject : getCategoryDataFromCache(cacheKeys)) {
      if (Objects.nonNull(cacheObject) && cacheObject instanceof List<?> && CollectionUtils.isNotEmpty(
          (List<?>) cacheObject)) {
        List<?> cacheObjectList = (List<?>) cacheObject;
        if (cacheObjectList.stream().allMatch(element -> element instanceof CategoryResponse)) {
          List<CategoryResponse> categoryResponseList = (List<CategoryResponse>) cacheObjectList;
          categoryResponseMap.put(categoryResponseList.stream().findFirst().get().getCategoryCode(),
              categoryResponseList);
        }
      }
    }

    //get the category codes for which data is not present from cache and get those from PCB.
    Map<String, List<CategoryResponse>> categoryHierarchyNotFoundInCache = new HashMap<>();
    Set<String> categoryCodesNotFoundInCache = categoryCodes.stream().filter(categoryCode ->
       !categoryResponseMap.keySet().contains(categoryCode)).collect(Collectors.toSet());
    for (String categoryCodeNotFoundInCache : categoryCodesNotFoundInCache) {
      try {
        List<CategoryResponse> categoryResponseList =
                this.productCategoryBaseOutbound.getCategoryHierarchy(requestId, username, categoryCodeNotFoundInCache);
        categoryResponseMap.put(categoryResponseList.stream().findFirst().get().getCategoryCode(), categoryResponseList);
        categoryHierarchyNotFoundInCache.put(CacheNames.PARENT_CATEGORIES + Constants.COLON +
        categoryResponseList.stream().findFirst().get().getCategoryCode(), categoryResponseList);
      } catch (Exception e) {
        CachedServiceImpl.LOGGER.error("categoryCode: {}, with error: {}", categoryCodeNotFoundInCache,
                e.getMessage(), e);
      }
    }

    //multi set the object which was not found from cache and was fetched from PCB
    if (MapUtils.isNotEmpty(categoryHierarchyNotFoundInCache)) {
      setCategoryDataFromCache(categoryHierarchyNotFoundInCache);
    }

    checkState(categoryResponseMap != null && !categoryResponseMap.isEmpty(),
        CachedServiceImpl.LIST_OF_CATEGORIES_FROM_PRODUCT_CATEGORY_BASE_RETURNED_NULL);

    return categoryResponseMap;
  }


  private List<Object> getCategoryDataFromCache(Set<String> cacheKeys) {
    try {
      return categoryRedisTemplate.opsForValue().multiGet(cacheKeys);
    } catch (Exception e) {
      LOGGER.error("Error while fetching category data from cache. cacheKeys : {} ", cacheKeys, e);
      return new ArrayList<>();
    }
  }

  private void setCategoryDataFromCache(Map<String, List<CategoryResponse>> categoryHierarchyNotFoundInCache) {
    try {
      categoryRedisTemplate.opsForValue().multiSet(categoryHierarchyNotFoundInCache);
    } catch (Exception e) {
      LOGGER.error("Error while setting category data from cache. categoryHierarchyNotFoundInCache : {} ",
          categoryHierarchyNotFoundInCache, e);
    }
  }

  @Override
  @Cacheable(cacheManager = Constants.CATEGORY_CACHE_MANAGER, value = {
      CacheNames.GET_MASTER_PARENT_CATEGORY_BY_PRODUCT_CODE}, key = "#productCode", unless = "#result == null")
  public List<CategoryResponse> getMasterParentCategoryResponseByProductCode(String requestId,
      String username, String productCode) {
    checkArgument(StringUtils.isNotBlank(productCode),
        CachedServiceImpl.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    return productCategoryBaseOutbound
        .getMasterParentCategoryResponseByProductCode(requestId, username, productCode);
  }
}
