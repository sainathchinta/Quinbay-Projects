package com.gdn.x.productcategorybase.helper;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.BoundValueOperations;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.service.CategoryService;

;

/**
 * Created by Kesha on 19/07/16.
 */
public class CategoryServiceHelperTest {

  @Mock
  private CategoryService mockCategoryService;

  @Mock
  private Page<Category> categoryPage;

  @Mock
  private BoundValueOperations operations;

  ObjectMapper mapper = new ObjectMapper();

  public static final String STORE_ID = "10001";
  private static final String NAME = "name";
  private static final String STATE = "state";
  private final Pageable pageable = PageRequest.of(0, 10);
  private Category categoryResponse = new Category();
  private List<Category> categoryList = new ArrayList<>();
  private static final String DOCUMENT_FILTER_TYPE = "ALL";

  @InjectMocks
  private CategoryServiceHelper categoryServiceHelper;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    Catalog catalog =
        new Catalog("catalog1", "catalog-code1",
            CatalogType.MASTER_CATALOG, STORE_ID);
    catalog.setId("cat1");
    categoryResponse.setName("cat1");
    categoryResponse.setDescription("mock descr".getBytes());
    categoryResponse.setActivated(true);
    categoryResponse.setId("123");
    categoryResponse.setCatalog(catalog);
    categoryList.add(categoryResponse);
    when(this.categoryPage.getContent()).thenReturn(categoryList);
    when(this.categoryPage.getTotalElements()).thenReturn(10l);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(mockCategoryService);
    verifyNoMoreInteractions(operations);
  }

  @Test
  public void getCategoryList_CategoriesFound_returnNonEmptyJson() throws Exception {
    when(mockCategoryService.findByStoreId(anyString(), any(Pageable.class))).thenReturn(this
        .categoryPage);
    String responseStr = categoryServiceHelper.getCategoryList(STORE_ID, 0, 10, new AtomicLong());
    List<CategoryResponse> responseList = mapper.readValue(responseStr, new
        TypeReference<List<CategoryResponse>>() {
        });
    assertNotNull(responseList);
    assertEquals(responseList.size(), categoryPage.getContent().size());
    assertEquals(responseList.get(0).getName(), categoryPage.getContent().get(0).getName());
    verify(mockCategoryService).findByStoreId(anyString(), any(Pageable.class));
  }

  @Test
  public void getCategoryList_NoCategoriesFound_returnNonEmptyJson() throws Exception {
    when(mockCategoryService.findByStoreId(anyString(), any(Pageable.class))).thenReturn(this
        .categoryPage);
    when(categoryPage.getContent()).thenReturn(new ArrayList<Category>());
    String responseStr = categoryServiceHelper.getCategoryList(STORE_ID, 0, 10, new AtomicLong());
    List<CategoryResponse> responseList = mapper.readValue(responseStr, new
        TypeReference<List<CategoryResponse>>() {
        });
    assertNotNull(responseList);
    assertTrue(StringUtils.isNotBlank(responseStr));
    assertTrue(CollectionUtils.isEmpty(responseList));
    verify(mockCategoryService).findByStoreId(anyString(), any(Pageable.class));
  }

  @Test
  public void findByNameWithChildCountTest() {
    when(this.mockCategoryService.findByName(STORE_ID, NAME, this.pageable, STATE, DOCUMENT_FILTER_TYPE))
        .thenReturn(this.categoryPage);
    when(mockCategoryService.findActiveChildCountForParent(STORE_ID, categoryResponse)).thenReturn((long) 10);
    categoryServiceHelper
        .findByNameWithChildCount(STORE_ID, STORE_ID, NAME, this.pageable, STATE, DOCUMENT_FILTER_TYPE);
    verify(this.mockCategoryService).findByName(STORE_ID, NAME, this.pageable, STATE, DOCUMENT_FILTER_TYPE);
    verify(this.mockCategoryService).findActiveChildCountForParent(STORE_ID, categoryResponse);
  }
}
