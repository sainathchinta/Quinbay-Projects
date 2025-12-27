package com.gdn.x.productcategorybase.service.impl;

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;

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

import com.gdn.x.productcategorybase.entity.CategoryConfigurationHistory;
import com.gdn.x.productcategorybase.repository.CategoryConfigurationHistoryRepository;

public class CategoryConfigurationHistoryServiceImplTest {

  private static final String CATEGORY_CODE = "categoryCode";
  private static final String POST_LIVE_STATUS = "Post-live";
  private static final String PRE_LIVE_STATUS = "Pre-live";
  private static final String ACTIVITY = "Registered";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String STORE_ID = "10001";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final Long TOTAL_RECORDS = Long.valueOf(1);

  private Pageable pageable;

  private CategoryConfigurationHistory categoryConfigurationHistory;
  private Page<CategoryConfigurationHistory> configurationHistoryPage;

  @InjectMocks
  private CategoryConfigurationHistoryServiceImpl categoryConfigurationHistoryService;

  @Mock
  private CategoryConfigurationHistoryRepository categoryConfigurationHistoryRepository;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);

    categoryConfigurationHistory = new CategoryConfigurationHistory();
    categoryConfigurationHistory.setCategoryCode(CATEGORY_CODE);
    categoryConfigurationHistory.setOldValue(PRE_LIVE_STATUS);
    categoryConfigurationHistory.setNewValue(POST_LIVE_STATUS);
    categoryConfigurationHistory.setActivity(ACTIVITY);

    pageable = PageRequest.of(PAGE, SIZE);
    categoryConfigurationHistory.setOldValue(POST_LIVE_STATUS);
    categoryConfigurationHistory.setNewValue(PRE_LIVE_STATUS);
    categoryConfigurationHistory.setActivity(ACTIVITY);

    configurationHistoryPage = new PageImpl<>(Arrays.asList(categoryConfigurationHistory), pageable,
        Arrays.asList(categoryConfigurationHistory).size());
  }

  @Test
  public void saveCategoryHistoryEntitiesTest() throws Exception {
    this.categoryConfigurationHistoryService.saveCategoryHistoryConfigurations(Arrays.asList(categoryConfigurationHistory));
    Mockito.verify(this.categoryConfigurationHistoryRepository)
        .saveAll(Collections.singletonList(categoryConfigurationHistory));
  }

  @Test
  public void saveCategoryHistoryEntityTest() throws Exception {
    this.categoryConfigurationHistoryService.saveCategoryHistoryConfiguration(categoryConfigurationHistory);
    Mockito.verify(this.categoryConfigurationHistoryRepository).save(categoryConfigurationHistory);
  }

  @Test
  public void getCategoryConfigurationByCreatedDateTest() {
    Date date = new Date();
    Mockito.when(this.categoryConfigurationHistoryRepository
        .findByStoreIdAndCreatedDateGreaterThanOrderByCreatedDateDesc(DEFAULT_STORE_ID, date, pageable))
        .thenReturn(configurationHistoryPage);
    this.categoryConfigurationHistoryService.getCategoryConfigurationByCreatedDate(DEFAULT_STORE_ID, date, pageable);
    Mockito.verify(this.categoryConfigurationHistoryRepository)
        .findByStoreIdAndCreatedDateGreaterThanOrderByCreatedDateDesc(DEFAULT_STORE_ID, date, pageable);
  }

  @Test
  public void getCategoryHistoryTest() {
    Mockito.when(this.categoryConfigurationHistoryRepository
        .findByStoreIdAndCategoryCodeAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, CATEGORY_CODE, pageable))
        .thenReturn(new PageImpl<>(Arrays.asList(categoryConfigurationHistory), pageable, TOTAL_RECORDS));
    Page<CategoryConfigurationHistory> categoryConfigurationHistoryPage =
        this.categoryConfigurationHistoryService.getCategoryConfigurationHistory(STORE_ID, CATEGORY_CODE, PAGE, SIZE);
    Mockito.verify(this.categoryConfigurationHistoryRepository)
        .findByStoreIdAndCategoryCodeAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, CATEGORY_CODE, pageable);
    Assertions.assertEquals(CATEGORY_CODE, categoryConfigurationHistoryPage.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(POST_LIVE_STATUS, categoryConfigurationHistoryPage.getContent().get(0).getOldValue());
    Assertions.assertEquals(PRE_LIVE_STATUS, categoryConfigurationHistoryPage.getContent().get(0).getNewValue());
    Assertions.assertEquals(ACTIVITY, categoryConfigurationHistoryPage.getContent().get(0).getActivity());
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.categoryConfigurationHistoryRepository);
  }
}
