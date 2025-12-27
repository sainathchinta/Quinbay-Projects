package com.gdn.x.productcategorybase.service.impl;

import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.repository.CategoryRepository;
import com.gdn.x.productcategorybase.repository.brand.BrandRepository;
import com.gdn.x.productcategorybase.service.brand.BrandWipService;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Slf4j
@Service
public class CacheServiceHelperBean {

  @Autowired
  private CategoryRepository categoryRepository;

  @Autowired
  private BrandRepository brandRepository;

  @Autowired
  private BrandWipService brandWipService;

  @Cacheable(value = CacheNames.CHILD_CATEGORY_CACHE, key = "#storeId + '_' + #parentCategoryId", unless = "#result == null")
  public List<String> findChildCategoriesIncludingInactive(String storeId, String categoryCode,
    String parentCategoryId) throws Exception {
    return findChildCategories(storeId, categoryCode, parentCategoryId, false);
  }

  @Cacheable(value = CacheNames.ACTIVE_CHILD_CATEGORY_CACHE, key = "#storeId + '_' + "
    + "#parentCategoryId", unless = "#result == null")
  public List<String> findChildCategoriesActiveOnly(String storeId, String categoryCode,
    String parentCategoryId) throws Exception {
    return findChildCategories(storeId, categoryCode, parentCategoryId, true);
  }

  @Cacheable(value = CacheNames.BRAND, key = "#storeId + '_' + #filteredBrandNameForCache + '_' + #markForDelete",  unless = "#result == null")
  public BrandResponse findByBrandName(String storeId, String brandName, boolean markForDelete, boolean activeBrandsOnly,
    String filteredBrandNameForCache) throws Exception {
    if (activeBrandsOnly) {
      Brand brand = this.brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(storeId, brandName, markForDelete);
      return Objects.nonNull(brand) ? ConverterUtil.generateBrandResponse(brand) : null;
    }
    Brand brand = this.brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(storeId, brandName, markForDelete);
    return Objects.nonNull(brand) ?
      ConverterUtil.generateBrandResponse(brand) :
      this.brandWipService.getBrandByNameFromBrandWip(storeId, brandName);
  }


  @Cacheable(value = CacheNames.BRAND, key = "#storeId + '_' + #brandCode")
  public BrandResponse findByBrandCode(String storeId, String brandCode) throws Exception {
    Brand brand = this.brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(storeId, brandCode);
    return ConverterUtil.generateBrandResponse(brand);
  }

  @CacheEvict(value = CacheNames.BRAND, key = "#storeId + '_' + #brandCode")
  public void evictBrandCacheByCode(String storeId, String brandCode) {
    log.debug("Evicting cache by brand code for : {} ", brandCode);
  }

  @CacheEvict(value = CacheNames.BRAND, key = "#storeId + '_' + #brandName + '_true'")
  public void evictBrandCacheByNameAndMarkForDelete(String storeId, String brandName) {
    log.debug("Evicting cache by brand name and MFD True for : {} ", brandName);
  }

  @CacheEvict(value = CacheNames.BRAND, key = "#storeId + '_' + #brandName + '_false'")
  public void evictBrandCacheByNameAndActiveBrandsOnly(String storeId, String brandName) {
    log.debug("Evicting cache by brand name and MFD False for : {} ", brandName);

  }

  private List<String> findChildCategories(String storeId, String categoryCode,
    String parentCategoryId, boolean filterOutInactiveCn) throws Exception {
    List<String> childCategories = new ArrayList<>();
    List<Object[]> results = retrieveCategoryResults(storeId, parentCategoryId, filterOutInactiveCn);

    if (results.isEmpty()) {
      childCategories.add(categoryCode);
    } else {
      for (Object[] record : results) {
        childCategories.addAll(findChildCategories(storeId, (String) record[1], (String) record[0], filterOutInactiveCn));
      }
    }

    return childCategories;
  }

  private List<Object[]> retrieveCategoryResults(String storeId, String parentCategoryId,
    boolean filterOutInactiveCn) {
    if (filterOutInactiveCn) {
      log.info("Fetching Only active CN for parentCategoryId : {}", parentCategoryId);
      return categoryRepository.findByStoreIdAndParentCategoryIdAndIsActivatedTrue(storeId,
        parentCategoryId);
    }
    log.info("Fetching All CN for parentCategoryId : {}", parentCategoryId);
    return categoryRepository.findByStoreIdAndParentCategoryId(storeId, parentCategoryId);
  }

}
