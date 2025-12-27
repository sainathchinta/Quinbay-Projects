package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.util.Assert;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.repository.CategoryRepository;
import com.gdn.x.productcategorybase.repository.brand.BrandRepository;
import com.gdn.x.productcategorybase.service.brand.BrandWipService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class CacheServiceHelperBeanTest{

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String CATEGORY_CODE_1 = "AK-1234";
  private static final String CATEGORY_CODE_2 = "AK-5678";
  private static final String PARENT_CATEGORY_ID_1 = "parent-category-id-1";
  private static final String PARENT_CATEGORY_ID_2 = "parent-category-id-2";
  private static final String BRAND_NAME = "brandName";
  private static final String BRAND_CODE = "brandCode";

  @InjectMocks
  CacheServiceHelperBean cacheServiceHelperBean;

  @Mock
  private CategoryRepository categoryRepository;

  @Mock
  private BrandRepository brandRepository;

  @Mock
  private BrandWipService brandWipService;

  private List<Object[]> categoryResponse;
  @Captor
  ArgumentCaptor<String> messageCaptor;
  private String filteredBrandNameForCache;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    filteredBrandNameForCache = BRAND_NAME.replace(Constants.SPACE, Constants.HYPHEN).toLowerCase();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(categoryRepository, brandRepository, brandWipService);
  }

  private List<Object[]> getCategoriesWithCategoryCode(String parentCategoryId, String categoryCode) {
    List<Object[]> categories = new ArrayList<>();
    Object[] object = new Object[]{parentCategoryId, categoryCode};
    categories.add(object);
    return categories;
  }

  @Test
  public void testFindChildCategoriesIncludingInactive() throws Exception {
    List<Object[]> emptyResponse = new ArrayList<>();
    Mockito.when(categoryRepository.findByStoreIdAndParentCategoryIdAndIsActivatedTrue(DEFAULT_STORE_ID,
      PARENT_CATEGORY_ID_1)).thenReturn(emptyResponse);
    Mockito.when(categoryRepository.findByStoreIdAndParentCategoryId(DEFAULT_STORE_ID,
      PARENT_CATEGORY_ID_1)).thenReturn(emptyResponse);
    cacheServiceHelperBean.findChildCategoriesIncludingInactive(DEFAULT_STORE_ID, CATEGORY_CODE_1,
      PARENT_CATEGORY_ID_1);
    Mockito.verify(categoryRepository).findByStoreIdAndParentCategoryId(DEFAULT_STORE_ID, PARENT_CATEGORY_ID_1);
    Mockito.verify(categoryRepository).findByStoreIdAndParentCategoryId(DEFAULT_STORE_ID, PARENT_CATEGORY_ID_1);
  }

  @Test
  public void testFindChildCategoriesIncludingInactiveForEmptyFetchCase() throws Exception {
    List<Object[]> initialResponse = getCategoriesWithCategoryCode(PARENT_CATEGORY_ID_2, CATEGORY_CODE_2);
    Mockito.when(categoryRepository.findByStoreIdAndParentCategoryId(DEFAULT_STORE_ID,
      PARENT_CATEGORY_ID_1))
      .thenReturn(initialResponse);
    List<Object[]> emptyResponse = new ArrayList<>();
    Mockito.when(categoryRepository.findByStoreIdAndParentCategoryId(DEFAULT_STORE_ID,
      PARENT_CATEGORY_ID_2))
      .thenReturn(emptyResponse);
    cacheServiceHelperBean.findChildCategoriesIncludingInactive(DEFAULT_STORE_ID, CATEGORY_CODE_1,
      PARENT_CATEGORY_ID_1);
    Mockito.verify(categoryRepository).findByStoreIdAndParentCategoryId(DEFAULT_STORE_ID, PARENT_CATEGORY_ID_1);
    Mockito.verify(categoryRepository).findByStoreIdAndParentCategoryId(DEFAULT_STORE_ID, PARENT_CATEGORY_ID_2);
  }

  @Test
  public void testFindChildCategoriesActiveOnly() throws Exception {
    List<Object[]> emptyResponse = new ArrayList<>();
    Mockito.when(categoryRepository.findByStoreIdAndParentCategoryIdAndIsActivatedTrue(DEFAULT_STORE_ID,
      PARENT_CATEGORY_ID_1)).thenReturn(emptyResponse);
    Mockito.when(categoryRepository.findByStoreIdAndParentCategoryIdAndIsActivatedTrue(DEFAULT_STORE_ID,
      PARENT_CATEGORY_ID_1)).thenReturn(emptyResponse);
    cacheServiceHelperBean.findChildCategoriesActiveOnly(DEFAULT_STORE_ID, CATEGORY_CODE_1,
      PARENT_CATEGORY_ID_1);
    Mockito.verify(categoryRepository).findByStoreIdAndParentCategoryIdAndIsActivatedTrue(DEFAULT_STORE_ID, PARENT_CATEGORY_ID_1);
    Mockito.verify(categoryRepository).findByStoreIdAndParentCategoryIdAndIsActivatedTrue(DEFAULT_STORE_ID, PARENT_CATEGORY_ID_1);

  }

  private BrandResponse createBrandResponse() {
    // Create and return a mock BrandResponse
    BrandResponse response = new BrandResponse();
    response.setBrandName(BRAND_NAME);
    return response;
  }

  private Brand createBrand() {
    // Create and return a mock Brand
    Brand brand = new Brand();
    brand.setBrandName(BRAND_NAME);
    return brand;
  }

  @Test
  public void testFindByBrandNameActiveBrandsOnlyAndBrandFound() throws Exception {
    Brand brand = createBrand();
    BrandResponse brandResponse = createBrandResponse();
    Mockito.when(brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME, false))
      .thenReturn(brand);
    BrandResponse result = cacheServiceHelperBean.findByBrandName(DEFAULT_STORE_ID, BRAND_NAME,
      false,
      true, filteredBrandNameForCache);
    Mockito.verify(brandRepository).findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME, false);
    Assertions.assertEquals(brandResponse.getBrandName(), result.getBrandName());
  }

  @Test
  public void testFindByBrandNameActiveBrandsOnlyAndBrandNotFound() throws Exception {
    Mockito.when(brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME, true))
      .thenReturn(null);

    BrandResponse result = cacheServiceHelperBean.findByBrandName(DEFAULT_STORE_ID, BRAND_NAME, true,
      true, filteredBrandNameForCache);

    Mockito.verify(brandRepository).findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME, true);
    Assertions.assertEquals(null, result);
  }

  @Test
  public void testFindByBrandNameInactiveBrandsAndBrandFoundInRepository() throws Exception {
    Brand brand = createBrand();
    BrandResponse brandResponse = createBrandResponse();

    Mockito.when(brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME, false))
      .thenReturn(brand);
    BrandResponse result = cacheServiceHelperBean.findByBrandName(DEFAULT_STORE_ID, BRAND_NAME,
      false
      , false, filteredBrandNameForCache);
    Mockito.verify(brandRepository).findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME, false);
    Assertions.assertEquals(brandResponse.getBrandName(), result.getBrandName());
  }

  @Test
  public void testFindByBrandNameInactiveBrandsAndBrandNotFoundInRepositoryButFoundInWip() throws Exception {
    Brand brand = createBrand();
    BrandResponse brandResponse = createBrandResponse();
    Mockito.when(brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME, false))
      .thenReturn(null);
    Mockito.when(brandWipService.getBrandByNameFromBrandWip(DEFAULT_STORE_ID, BRAND_NAME)).thenReturn(brandResponse);
    BrandResponse result = cacheServiceHelperBean.findByBrandName(DEFAULT_STORE_ID, BRAND_NAME,
      false, false, filteredBrandNameForCache);

    Mockito.verify(brandRepository).findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME, false);
    Mockito.verify(brandWipService).getBrandByNameFromBrandWip(DEFAULT_STORE_ID, BRAND_NAME);
    Assertions.assertEquals(brandResponse, result);
  }

  @Test
  public void testFindByBrandNameInactiveBrandsAndBrandNotFoundInBothRepositoryAndWip() throws Exception {
    Mockito.when(brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME, true))
      .thenReturn(null);
    Mockito.when(brandWipService.getBrandByNameFromBrandWip(DEFAULT_STORE_ID, BRAND_NAME)).thenReturn(null);
    BrandResponse result = cacheServiceHelperBean.findByBrandName(DEFAULT_STORE_ID, BRAND_NAME, true, false,
      filteredBrandNameForCache);

    Mockito.verify(brandRepository).findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME, true);
    Mockito.verify(brandWipService).getBrandByNameFromBrandWip(DEFAULT_STORE_ID, BRAND_NAME);
    Assertions.assertNull(result);
  }

  @Test
  public void testFindByBrandNameWithNullBrandResponse() throws Exception {
    Brand brand = createBrand();
    BrandResponse brandResponse = createBrandResponse();

    Mockito.when(brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME, true))
      .thenReturn(brand);
    BrandResponse result = cacheServiceHelperBean.findByBrandName(DEFAULT_STORE_ID, BRAND_NAME, true,
      false, filteredBrandNameForCache);
    Mockito.verify(brandRepository).findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME, true);
    Assertions.assertEquals(brandResponse.getBrandName(), result.getBrandName());
  }
  @Test
  public void testFindByBrandCode() throws Exception {
    Brand brand = createBrand();
    Mockito.when(brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
      BRAND_CODE)).thenReturn(brand);
    cacheServiceHelperBean.findByBrandCode(DEFAULT_STORE_ID, BRAND_CODE);
    Mockito.verify(brandRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, BRAND_CODE);
  }

  @Test
  public void testEvictBrandCacheByCode() {
    cacheServiceHelperBean.evictBrandCacheByCode(DEFAULT_STORE_ID, BRAND_CODE);
    Mockito.verifyNoInteractions(brandRepository);
  }

  @Test
  public void testEvictBrandCacheByNameAndMarkForDelete() {
    cacheServiceHelperBean.evictBrandCacheByNameAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME);
    Mockito.verifyNoInteractions(brandRepository);
  }

  @Test
  public void testEvictBrandCacheByNameAndActiveBrandsOnly() {
    cacheServiceHelperBean.evictBrandCacheByNameAndActiveBrandsOnly(DEFAULT_STORE_ID, BRAND_NAME);
    Mockito.verifyNoInteractions(brandRepository);

  }
}