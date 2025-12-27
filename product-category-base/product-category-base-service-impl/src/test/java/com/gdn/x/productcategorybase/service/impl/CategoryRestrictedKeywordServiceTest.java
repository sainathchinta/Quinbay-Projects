package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.collections.CollectionUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryRestrictedKeyword;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;
import com.gdn.x.productcategorybase.repository.CategoryRestrictedKeywordRepository;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.RestrictedKeywordService;
import com.google.common.collect.ImmutableMap;

public class CategoryRestrictedKeywordServiceTest {

  private static final String STORE_ID = "storeId";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_CODE1 = "categoryCode1";
  private static final String KEYWORD = "keyword";
  private static final String KEYWORD_ID = "keywordId";
  private static final String KEYWORD1 = "keyword1";
  private static final String KEYWORD_ID1 = "keywordId1";
  private static final String NEW_KEYWORD_ID1 = "newKeywordId1";
  private static final String KEYWORD2 = "keyword2";
  private static final String KEYWORD_ID2 = "keywordId2";
  private static final String ID = "id";
  private static final String ID1 = "id1";
  private static final int ACTION = 1;
  private static final String TYPE = "type";
  private static final String MESSAGE = "message";
  private static final String DESTINATION_CATEGORY = "destination_category";

  private static final PageRequest DEFAULT_PAGE_REQUEST = PageRequest.of(0, 10);

  @InjectMocks
  private CategoryRestrictedKeywordServiceImpl categoryRestrictedKeywordService;

  @Mock
  private CategoryRestrictedKeywordRepository categoryRestrictedKeywordRepository;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Mock
  private CategoryService categoryService;

  @Mock
  private RestrictedKeywordService restrictedKeywordService;

  private CategoryRestrictedKeyword categoryRestrictedKeyword;
  private CategoryRestrictedKeyword categoryRestrictedKeyword1;
  private CategoryRestrictedKeyword categoryRestrictedKeyword2;
  private CategoryRestrictedKeywordResponse categoryRestrictedKeywordResponse;
  private RestrictedKeyword restrictedKeyword;
  private RestrictedKeyword restrictedKeyword1;
  private RestrictedKeyword restrictedKeyword2;
  private List<CategoryRestrictedKeyword> categoryRestrictedKeywords;
  private Category category;
  private Category category1;
  private CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO;

  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);

    restrictedKeyword = new RestrictedKeyword();
    restrictedKeyword.setKeyword(KEYWORD);
    restrictedKeyword.setId(KEYWORD_ID);

    restrictedKeyword1 = new RestrictedKeyword();
    restrictedKeyword1.setKeyword(KEYWORD1);
    restrictedKeyword1.setId(KEYWORD_ID1);

    restrictedKeyword2 = new RestrictedKeyword();
    restrictedKeyword2.setKeyword(KEYWORD2);
    restrictedKeyword2.setId(KEYWORD_ID2);

    categoryRestrictedKeywordResponse = new CategoryRestrictedKeywordResponse();
    categoryRestrictedKeyword = new CategoryRestrictedKeyword();
    categoryRestrictedKeyword.setMarkForDelete(Boolean.FALSE);
    categoryRestrictedKeyword.setCategoryCode(CATEGORY_CODE);
    categoryRestrictedKeyword.setRestrictedKeyword(restrictedKeyword);
    categoryRestrictedKeyword.setRestrictedKeywordId(KEYWORD_ID);
    categoryRestrictedKeyword.setAction(ACTION);
    categoryRestrictedKeyword.setType(TYPE);
    categoryRestrictedKeyword.setMessage(MESSAGE);
    categoryRestrictedKeyword.setDestinationCategory(DESTINATION_CATEGORY);

    categoryRestrictedKeyword1 = new CategoryRestrictedKeyword();
    categoryRestrictedKeyword1.setMarkForDelete(Boolean.FALSE);
    categoryRestrictedKeyword1.setCategoryCode(CATEGORY_CODE);
    categoryRestrictedKeyword1.setRestrictedKeyword(restrictedKeyword1);
    categoryRestrictedKeyword1.setRestrictedKeywordId(KEYWORD_ID1);
    categoryRestrictedKeyword1.setAction(ACTION);
    categoryRestrictedKeyword1.setType(TYPE);
    categoryRestrictedKeyword1.setMessage(MESSAGE);
    categoryRestrictedKeyword1.setDestinationCategory(DESTINATION_CATEGORY);

    categoryRestrictedKeyword2 = new CategoryRestrictedKeyword();
    categoryRestrictedKeyword2.setMarkForDelete(Boolean.FALSE);
    categoryRestrictedKeyword2.setCategoryCode(CATEGORY_CODE);
    categoryRestrictedKeyword2.setRestrictedKeyword(restrictedKeyword2);
    categoryRestrictedKeyword2.setRestrictedKeywordId(KEYWORD_ID2);
    categoryRestrictedKeyword2.setAction(ACTION);
    categoryRestrictedKeyword2.setType(TYPE);
    categoryRestrictedKeyword2.setMessage(MESSAGE);
    categoryRestrictedKeyword2.setDestinationCategory(DESTINATION_CATEGORY);

    categoryRestrictedKeywords = new ArrayList<>();
    categoryRestrictedKeywords.add(categoryRestrictedKeyword);
    categoryRestrictedKeywords.add(categoryRestrictedKeyword1);
    categoryRestrictedKeywords.add(categoryRestrictedKeyword2);

    category = new Category();
    category.setId(ID);
    category.setCategoryCode(CATEGORY_CODE);

    category1 = new Category();
    category1.setId(ID1);
    category1.setCategoryCode(CATEGORY_CODE1);

    categoryKeywordsUpdateDTO = new CategoryKeywordsUpdateDTO();
    categoryKeywordsUpdateDTO.setKeyword(KEYWORD_ID1);

    Mockito.when(this.restrictedKeywordService.findByKeywordId(eq(STORE_ID), Mockito.any()))
        .thenReturn(Arrays.asList(restrictedKeyword, restrictedKeyword1, restrictedKeyword2));
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.categoryRestrictedKeywordRepository);
    verifyNoMoreInteractions(this.applicationCacheServiceBean);
    verifyNoMoreInteractions(this.categoryService);
    verifyNoMoreInteractions(this.restrictedKeywordService);
  }

  @Test
  public void findByStoreIdAndCategoryCodeTest() {
    Mockito.when(
        this.categoryRestrictedKeywordRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalseOrderByUpdatedDateDescIdDesc(
            STORE_ID, CATEGORY_CODE, DEFAULT_PAGE_REQUEST)).thenReturn(
        new PageImpl<>(categoryRestrictedKeywords, DEFAULT_PAGE_REQUEST, categoryRestrictedKeywords.size()));
    Mockito.when(this.categoryRestrictedKeywordRepository.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryRestrictedKeywords);
    this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE, DEFAULT_PAGE_REQUEST);
    Mockito.verify(categoryRestrictedKeywordRepository).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRestrictedKeywordRepository)
        .findByStoreIdAndCategoryCodeAndMarkForDeleteFalseOrderByUpdatedDateDescIdDesc(STORE_ID, CATEGORY_CODE,
            DEFAULT_PAGE_REQUEST);
    Mockito.verify(restrictedKeywordService, times(2)).findByKeywordId(eq(STORE_ID), Mockito.any());
  }

  @Test
  public void findByStoreIdAndCategoryCode_emptyTest() {
    List<CategoryRestrictedKeyword> result =
        this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRestrictedKeywordRepository).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    Assertions.assertTrue(CollectionUtils.isEmpty(result));
  }

  @Test
  public void updateCategoryRestrictedKeywordAndEvictCacheTest() {
    doNothing().when(applicationCacheServiceBean).evictCategoryRestrictedKeywordCache(STORE_ID, CATEGORY_CODE);
    this.categoryRestrictedKeywordService.updateCategoryRestrictedKeywordAndEvictCache(STORE_ID, CATEGORY_CODE,
        Collections.singletonList(categoryRestrictedKeyword));
    Mockito.verify(categoryRestrictedKeywordRepository).saveAll(Mockito.anyList());
    Mockito.verify(applicationCacheServiceBean).evictCategoryRestrictedKeywordCache(STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void addAndDeleteCategoryRestrictedKeywordsMappingsTest() throws Exception {
    Mockito.when(this.categoryRestrictedKeywordRepository.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.singletonList(categoryRestrictedKeyword));
    doNothing().when(applicationCacheServiceBean).evictCategoryRestrictedKeywordCache(STORE_ID, CATEGORY_CODE);
    this.categoryRestrictedKeywordService
        .addAndDeleteCategoryRestrictedKeywordsMappings(STORE_ID, Arrays.asList(restrictedKeyword, restrictedKeyword1),
            category, Collections.singletonList(categoryKeywordsUpdateDTO), new HashMap<>());
    Mockito.verify(this.categoryRestrictedKeywordRepository).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRestrictedKeywordRepository).saveAll(Mockito.anyList());
    Mockito.verify(applicationCacheServiceBean).evictCategoryRestrictedKeywordCache(STORE_ID, CATEGORY_CODE);
    Mockito.verify(restrictedKeywordService).findByKeywordId(eq(STORE_ID), Mockito.any());
  }

  @Test
  public void addAndDeleteCategoryRestrictedKeywordsMappingsExclusionListTest() throws Exception {
    String categoryId = UUID.randomUUID().toString();
    category.setId(categoryId);
    categoryKeywordsUpdateDTO.setKeyword(KEYWORD1);
    categoryKeywordsUpdateDTO.getExclusionList().add(category.getCategoryCode());
    Mockito.when(this.categoryRestrictedKeywordRepository.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.singletonList(categoryRestrictedKeyword));
    doNothing().when(applicationCacheServiceBean).evictCategoryRestrictedKeywordCache(STORE_ID, CATEGORY_CODE);
    this.categoryRestrictedKeywordService.addAndDeleteCategoryRestrictedKeywordsMappings(STORE_ID,
        Arrays.asList(restrictedKeyword, restrictedKeyword1), category,
        Collections.singletonList(categoryKeywordsUpdateDTO),
        ImmutableMap.of(categoryKeywordsUpdateDTO.getKeyword(), categoryKeywordsUpdateDTO));
    Mockito.verify(this.categoryRestrictedKeywordRepository).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRestrictedKeywordRepository).saveAll(Mockito.anyList());
    Mockito.verify(applicationCacheServiceBean).evictCategoryRestrictedKeywordCache(STORE_ID, CATEGORY_CODE);
    Mockito.verify(restrictedKeywordService).findByKeywordId(eq(STORE_ID), Mockito.any());
  }

  @Test
  public void addAndDeleteCategoryRestrictedKeywordsMappingsExclusionListChildCategoriesTest() throws Exception {
    String categoryId = UUID.randomUUID().toString();
    category.setParentCategoryId(categoryId);
    categoryKeywordsUpdateDTO.setKeyword(KEYWORD1);
    categoryKeywordsUpdateDTO.getExclusionList().add(categoryId);
    Mockito.when(this.categoryRestrictedKeywordRepository.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.singletonList(categoryRestrictedKeyword));
    doNothing().when(applicationCacheServiceBean).evictCategoryRestrictedKeywordCache(STORE_ID, CATEGORY_CODE);
    this.categoryRestrictedKeywordService.addAndDeleteCategoryRestrictedKeywordsMappings(STORE_ID,
        Arrays.asList(restrictedKeyword, restrictedKeyword1), category,
        Collections.singletonList(categoryKeywordsUpdateDTO),
        ImmutableMap.of(categoryKeywordsUpdateDTO.getKeyword(), categoryKeywordsUpdateDTO));
    Mockito.verify(this.categoryRestrictedKeywordRepository).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRestrictedKeywordRepository).saveAll(Mockito.anyList());
    Mockito.verify(applicationCacheServiceBean).evictCategoryRestrictedKeywordCache(STORE_ID, CATEGORY_CODE);
    Mockito.verify(restrictedKeywordService).findByKeywordId(eq(STORE_ID), Mockito.any());
  }

  @Test
  public void addAndDeleteCategoryRestrictedKeywordsMappingsNoChangeTest() throws Exception {
    Mockito.when(this.categoryRestrictedKeywordRepository.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.singletonList(categoryRestrictedKeyword));
    doNothing().when(applicationCacheServiceBean).evictCategoryRestrictedKeywordCache(STORE_ID, CATEGORY_CODE);
    RestrictedKeyword restrictedKeyword1 = new RestrictedKeyword();
    restrictedKeyword1.setKeyword(KEYWORD);
    restrictedKeyword1.setId(NEW_KEYWORD_ID1);
    Map<String, CategoryKeywordsUpdateDTO> addedRestrictedKeywordAndRequestMap = new HashMap<>();
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO = new CategoryKeywordsUpdateDTO();
    categoryKeywordsUpdateDTO.setKeyword(restrictedKeyword.getKeyword());
    categoryKeywordsUpdateDTO.setKeywordId(restrictedKeyword.getId());
    categoryKeywordsUpdateDTO.setAction(ACTION);
    categoryKeywordsUpdateDTO.setType(TYPE);
    categoryKeywordsUpdateDTO.setMessage(MESSAGE);
    categoryKeywordsUpdateDTO.setDestinationCategory(DESTINATION_CATEGORY);
    addedRestrictedKeywordAndRequestMap.put(restrictedKeyword.getKeyword(), categoryKeywordsUpdateDTO);
    this.categoryRestrictedKeywordService
        .addAndDeleteCategoryRestrictedKeywordsMappings(STORE_ID, Arrays.asList(restrictedKeyword, restrictedKeyword1),
            category, Collections.singletonList(categoryKeywordsUpdateDTO), addedRestrictedKeywordAndRequestMap);
    Mockito.verify(this.categoryRestrictedKeywordRepository).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRestrictedKeywordRepository).saveAll(Mockito.anyList());
    Mockito.verify(applicationCacheServiceBean).evictCategoryRestrictedKeywordCache(STORE_ID, CATEGORY_CODE);
    Mockito.verify(restrictedKeywordService).findByKeywordId(eq(STORE_ID), Mockito.any());
  }

  @Test
  public void addAndDeleteCategoryRestrictedKeywordsMappingsAsyncTest() throws Exception {
    categoryRestrictedKeyword.setMarkForDelete(Boolean.TRUE);
    Mockito.when(this.categoryService.findAllChildForC1CategoryCodesTree(STORE_ID, CATEGORY_CODE))
        .thenReturn(Arrays.asList(category, category1));
    Mockito.when(this.categoryRestrictedKeywordRepository.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE1))
        .thenReturn(Arrays.asList(categoryRestrictedKeyword, categoryRestrictedKeyword2));
    doNothing().when(applicationCacheServiceBean).evictCategoryRestrictedKeywordCache(STORE_ID, CATEGORY_CODE1);
    this.categoryRestrictedKeywordService.addAndDeleteCategoryRestrictedKeywordsForChildCategories(STORE_ID,
        Arrays.asList(restrictedKeyword, restrictedKeyword1, restrictedKeyword2), category,
        Collections.singletonList(categoryKeywordsUpdateDTO), new HashMap<>());
    Mockito.verify(this.categoryService).findAllChildForC1CategoryCodesTree(STORE_ID, CATEGORY_CODE);
    Mockito.verify(this.categoryRestrictedKeywordRepository).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE1);
    Mockito.verify(categoryRestrictedKeywordRepository).saveAll(Mockito.anyList());
    Mockito.verify(applicationCacheServiceBean).evictCategoryRestrictedKeywordCache(STORE_ID, CATEGORY_CODE1);
    Mockito.verify(restrictedKeywordService).findByKeywordId(eq(STORE_ID), Mockito.any());
  }
  @Test
  public void getCategoryRestrictedKeywordByIdTest() {
    Mockito.when(
        this.categoryRestrictedKeywordRepository.findByStoreIdAndId(
            STORE_ID, ID)).thenReturn(categoryRestrictedKeyword);
    this.categoryRestrictedKeywordService.getCategoryRestrictedKeywordById(STORE_ID, ID);
    Mockito.verify(categoryRestrictedKeywordRepository).findByStoreIdAndId(Mockito.anyString(), Mockito.anyString());
    }

  @Test
  public void getCategoryRestrictedKeywordByIdEmptyTest() {
    Mockito.when(this.categoryRestrictedKeywordRepository.findByStoreIdAndId(STORE_ID, ID)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.categoryRestrictedKeywordService.getCategoryRestrictedKeywordById(STORE_ID, ID));
    } finally {
      Mockito.verify(this.categoryRestrictedKeywordRepository).findByStoreIdAndId(STORE_ID, ID);
    }
  }

  @Test
  public void getCategoryRestrictedKeywordByCategoryCodeAndKeywordIdsEmptyTest() {
    List<CategoryRestrictedKeywordResponse> response =
        categoryRestrictedKeywordService.getCategoryRestrictedKeywordByCategoryCodeAndKeywordIds(STORE_ID,
            CATEGORY_CODE, Collections.singletonList(KEYWORD));
    Mockito.verify(categoryRestrictedKeywordRepository).findByStoreIdAndCategoryCodeAndRestrictedKeywordIdIn(STORE_ID, CATEGORY_CODE,
        Collections.singletonList(KEYWORD));
    Assertions.assertTrue(response.isEmpty());
  }

  @Test
  public void getCategoryRestrictedKeywordByCategoryCodeAndKeywordIdsTest() {
    Mockito.when(categoryRestrictedKeywordRepository.findByStoreIdAndCategoryCodeAndRestrictedKeywordIdIn(STORE_ID, CATEGORY_CODE,
        Collections.singletonList(KEYWORD))).thenReturn(Collections.singletonList(categoryRestrictedKeyword));
    List<CategoryRestrictedKeywordResponse> response =
        categoryRestrictedKeywordService.getCategoryRestrictedKeywordByCategoryCodeAndKeywordIds(STORE_ID,
            CATEGORY_CODE, Collections.singletonList(KEYWORD));
    Mockito.verify(categoryRestrictedKeywordRepository).findByStoreIdAndCategoryCodeAndRestrictedKeywordIdIn(STORE_ID, CATEGORY_CODE,
        Collections.singletonList(KEYWORD));
    Mockito.verify(restrictedKeywordService)
        .findByKeywordId(STORE_ID, Collections.singletonList(categoryRestrictedKeyword.getRestrictedKeywordId()));
    Assertions.assertFalse(response.isEmpty());
  }
}
