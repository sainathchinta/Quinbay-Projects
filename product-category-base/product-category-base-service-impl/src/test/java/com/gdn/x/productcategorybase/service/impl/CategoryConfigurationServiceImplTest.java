package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryConfiguration;
import com.gdn.x.productcategorybase.repository.CategoryConfigurationRepository;

public class CategoryConfigurationServiceImplTest {

  private static final String CATEGORY_CODE = "categoryCode";
  private static final String DEFAULT_REVIEW_CONFIG_FLAG = "Pre-live";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String CATEGORY_ID = "categoryId";
  private static final Long CATEGORY_CONFIGURATION_COUNT = Long.valueOf(10);
  private static final Date DATE = new Date();
  private static final Pageable PAGEABLE = PageRequest.of(0, 10);
  private static final String SEARCH_KEY = "CAT";
  private static final String SORT_ORDER = "desc";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final Long TOTAL_RECORDS = Long.valueOf(1);

  private CategoryConfiguration categoryConfiguration;
  private Category category;
  private List<Category> categoryList;
  private List<String> categoryIdList;
  private ConfigurationFilterRequest configurationFilterRequest;
  private Pageable pageable;
  private List<CategoryConfiguration> categoryConfigurationList = new ArrayList<>();

  @InjectMocks
  private CategoryConfigurationServiceImpl categoryConfigurationService;

  @Mock
  private CategoryConfigurationRepository categoryConfigurationRepository;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    categoryConfiguration = new CategoryConfiguration();
    category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    category.setId(CATEGORY_ID);
    categoryList = new ArrayList<>();
    categoryList.add(category);
    categoryConfiguration.setCategory(category);
    categoryConfiguration.setReviewConfig(DEFAULT_REVIEW_CONFIG_FLAG);

    categoryIdList = new ArrayList<>();
    categoryIdList.add(CATEGORY_ID);

    configurationFilterRequest =
        new ConfigurationFilterRequest(Constants.PRE_LIVE_STATUS, CATEGORY_CODE, SEARCH_KEY, SORT_ORDER);

    pageable = PageRequest.of(PAGE, SIZE);
    categoryConfigurationList.add(categoryConfiguration);
  }

  @Test
  public void getCategoryConfigurationsByCategoryListTest() throws Exception {
    Mockito.when(this.categoryConfigurationRepository
        .findByStoreIdAndCategoryIn(DEFAULT_STORE_ID, Collections.singletonList(category)))
        .thenReturn(new ArrayList<>());
    this.categoryConfigurationService
        .getCategoryConfigurationsByCategoryList(DEFAULT_STORE_ID, Collections.singletonList(category));
    Mockito.verify(this.categoryConfigurationRepository)
        .findByStoreIdAndCategoryIn(DEFAULT_STORE_ID, Collections.singletonList(category));
  }

  @Test
  public void getCategoryConfigurationByCategoryTest() throws Exception {
    Mockito.when(this.categoryConfigurationRepository.findByStoreIdAndCategory(DEFAULT_STORE_ID, category))
        .thenReturn(categoryConfiguration);
    this.categoryConfigurationService.getCategoryConfigurationByCategory(DEFAULT_STORE_ID, category);
    Mockito.verify(this.categoryConfigurationRepository).findByStoreIdAndCategory(DEFAULT_STORE_ID, category);
  }

  @Test
  public void getCategoryConfigurationByCategoryByMarkForDeleteFalseTest() throws Exception {
    Mockito.when(this.categoryConfigurationRepository
        .findByStoreIdAndCategoryAndMarkForDeleteFalse(DEFAULT_STORE_ID, category))
        .thenReturn(categoryConfiguration);
    this.categoryConfigurationService
        .getCategoryConfigurationByCategoryAndMarkForDeleteFalse(DEFAULT_STORE_ID, category);
    Mockito.verify(this.categoryConfigurationRepository)
        .findByStoreIdAndCategoryAndMarkForDeleteFalse(DEFAULT_STORE_ID, category);
  }

  @Test
  public void saveCategoryEntitiesTest() throws Exception {
    this.categoryConfigurationService.saveCategoryConfigurations(Collections.singletonList(categoryConfiguration));
    Mockito.verify(this.categoryConfigurationRepository).saveAll(Collections.singletonList(categoryConfiguration));
  }

  @Test
  public void saveCategoryEntityTest() throws Exception {
    this.categoryConfigurationService.saveCategoryConfiguration(categoryConfiguration);
    Mockito.verify(this.categoryConfigurationRepository).save(categoryConfiguration);
  }

  @Test
  public void getCategoryConfigurationCountTest() throws Exception {
    Mockito.when(this.categoryConfigurationRepository.countByStoreIdAndMarkForDeleteFalse(DEFAULT_STORE_ID))
        .thenReturn(CATEGORY_CONFIGURATION_COUNT);
    Long result = this.categoryConfigurationService.getCategoryConfigurationCount(DEFAULT_STORE_ID);
    Mockito.verify(this.categoryConfigurationRepository).countByStoreIdAndMarkForDeleteFalse(DEFAULT_STORE_ID);
    Assertions.assertEquals(CATEGORY_CONFIGURATION_COUNT, result);
  }

  @Test
  public void getMerchantConfigurationByUpdatedDateGreaterThanTest() {
    List<CategoryConfiguration> categoryConfigurationList = Collections.singletonList(categoryConfiguration);
    Mockito.when(categoryConfigurationRepository.findByStoreIdAndUpdatedDateGreaterThan(
        DEFAULT_STORE_ID, DATE, PAGEABLE))
        .thenReturn(new PageImpl<>(categoryConfigurationList));
    Page<CategoryConfiguration> categoryConfigurations =
        categoryConfigurationService.getCategoryConfigurationByUpdatedDateGreaterThan(DEFAULT_STORE_ID, DATE, PAGEABLE);
    Mockito.verify(categoryConfigurationRepository).findByStoreIdAndUpdatedDateGreaterThan(DEFAULT_STORE_ID, DATE, PAGEABLE);
    Assertions.assertEquals(categoryConfigurationList, categoryConfigurations.getContent());
  }

  @Test
  public void getMerchantConfigurationByUpdatedDateGreaterThanEmptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> categoryConfigurationService.getCategoryConfigurationByUpdatedDateGreaterThan(null, DATE, PAGEABLE));
  }

  @Test
  public void getMerchantConfigurationByUpdatedDateGreaterThanEmptyDateTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> categoryConfigurationService.getCategoryConfigurationByUpdatedDateGreaterThan(DEFAULT_STORE_ID, null, PAGEABLE));
  }

  @Test
  public void getCategoryConfigurationListTest() throws Exception {
    Mockito.when(this.categoryConfigurationRepository
        .findByReviewConfigAndCategoryAndMarkForDeleteFalseOrderByCreatedDate(DEFAULT_STORE_ID,
            Constants.PRE_LIVE_STATUS, CATEGORY_CODE, SEARCH_KEY, SORT_ORDER, pageable))
        .thenReturn(new PageImpl<>(categoryConfigurationList, pageable, TOTAL_RECORDS));
    Page<CategoryConfiguration> categoryConfigurationPage = this.categoryConfigurationService
        .getCategoryConfigurationList(DEFAULT_STORE_ID, configurationFilterRequest, pageable);
    Mockito.verify(this.categoryConfigurationRepository)
        .findByReviewConfigAndCategoryAndMarkForDeleteFalseOrderByCreatedDate(DEFAULT_STORE_ID,
            Constants.PRE_LIVE_STATUS, CATEGORY_CODE, SEARCH_KEY, SORT_ORDER, pageable);
    Assertions.assertEquals(CATEGORY_CODE, categoryConfigurationPage.getContent().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(CATEGORY_ID, categoryConfigurationPage.getContent().get(0).getCategory().getId());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS, categoryConfigurationPage.getContent().get(0).getReviewConfig());
    Assertions.assertEquals(TOTAL_RECORDS, (Long)categoryConfigurationPage.getTotalElements());
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.categoryConfigurationRepository);
  }
}
