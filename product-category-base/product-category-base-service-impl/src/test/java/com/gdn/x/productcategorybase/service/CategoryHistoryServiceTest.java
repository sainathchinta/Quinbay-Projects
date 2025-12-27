package com.gdn.x.productcategorybase.service;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.gdn.x.productcategorybase.entity.CategoryHistory;
import com.gdn.x.productcategorybase.repository.CategoryHistoryRepository;

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

import java.util.Collections;

public class CategoryHistoryServiceTest {

  private static final String STORE_ID = "10001";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String ACTIVITY = "Activity";

  private CategoryHistory categoryHistory;

  @InjectMocks
  private CategoryHistoryServiceBean categoryHistoryService;

  @Mock
  private CategoryHistoryRepository categoryHistoryRepository;

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
    categoryHistory = new CategoryHistory();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.categoryHistoryRepository);
  }

  @Test
  public void categoryHistorySaveTest() {
    CategoryHistoryEventModel categoryHistoryEventModel = new CategoryHistoryEventModel();
    categoryHistoryEventModel.setActivity(ACTIVITY);
    categoryHistory.setActivity(ACTIVITY);
    Mockito.when(categoryHistoryService.saveCategoryHistory(categoryHistoryEventModel))
      .thenReturn(categoryHistory);
    CategoryHistory savedHistory = categoryHistoryService.saveCategoryHistory(
      CategoryHistoryEventModel.builder().activity(ACTIVITY).build());
    Mockito.verify(categoryHistoryRepository).save(categoryHistory);
    Assertions.assertEquals(savedHistory.getActivity(), ACTIVITY);
  }

  @Test
  public void categoryHistoryFetchTest() {
    Mockito.when(
        categoryHistoryRepository.findByStoreIdAndCategoryCodeOrderByCreatedDateDesc(STORE_ID,
            CATEGORY_CODE, PageRequest.of(0, 20))).thenReturn(categoryHistory(categoryHistory));
    categoryHistoryService.fetchCategoryHistory(STORE_ID, CATEGORY_CODE, 0, 20);
    Mockito.verify(categoryHistoryRepository)
        .findByStoreIdAndCategoryCodeOrderByCreatedDateDesc(STORE_ID, CATEGORY_CODE, PageRequest.of(0, 20));
  }

  @Test
  public void categoryHistoryFetchEmptyCategoryCodeTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> categoryHistoryService.fetchCategoryHistory(STORE_ID, null, 0, 20));
  }

  private Page<CategoryHistory> categoryHistory(
      CategoryHistory categoryHistory) {
    return new PageImpl<>(Collections.singletonList(categoryHistory),
        PageRequest.of(0, 20), 20);
  }
}
