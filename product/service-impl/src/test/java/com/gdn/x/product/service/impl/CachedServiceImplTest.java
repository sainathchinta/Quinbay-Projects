package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import io.lettuce.core.RedisException;

public class CachedServiceImplTest {

  private static final String REQUEST_ID = "request-id";

  private static final String USERNAME = "username";

  private static final String CATEGORY_CODE = "category-code";

  private static final String PRODUCT_CODE = "product-code";


  private static final String CATEGORY_CODE_NOT_FOUND = "category-code-not-found";

  @InjectMocks
  CachedServiceImpl cachedServiceImpl;

  @Mock
  ProductCategoryBaseOutbound productCategoryBaseClient;

  @Mock
  private RedisTemplate categoryRedisTemplate;

  @Mock
  private ValueOperations<String, List<CategoryResponse>> valueOperations;

  @Mock
  private ValueOperations<String, List<Item>> listValueOperations;

  private CategoryResponse categoryResponseC3;

  private List<CategoryResponse> listOfCategoryResponse;

  private CategoryResponse categoryResponseC2;

  private CategoryResponse categoryResponseC1;

  @Test
  public void getParentCategoriesFromMasterDataTest() throws Exception {
    List<CategoryResponse> listOfCategories =
        this.cachedServiceImpl.getParentCategoriesFromMasterData(CachedServiceImplTest.REQUEST_ID,
            CachedServiceImplTest.USERNAME, CachedServiceImplTest.CATEGORY_CODE);

    verify(this.productCategoryBaseClient).getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
        CachedServiceImplTest.USERNAME, CachedServiceImplTest.CATEGORY_CODE);

    assertNotNull(listOfCategories);
    assertEquals(listOfCategories.size(), 3);
    for (int i = 0; i < listOfCategories.size(); i++) {
      assertEquals(listOfCategories.get(i), this.listOfCategoryResponse.get(i));
    }
  }

  @Test
  public void getParentCategoriesFromDbAndCacheTest() {
    Set<String> categoryCodes = new HashSet<>();
    categoryCodes.add(CATEGORY_CODE);
    Mockito.when(categoryRedisTemplate.opsForValue())
            .thenReturn(Mockito.mock(ValueOperations.class));
    Map<String, List<CategoryResponse>> listOfCategories =
            this.cachedServiceImpl.getParentCategoriesFromDbAndCache(CachedServiceImplTest.REQUEST_ID,
                    CachedServiceImplTest.USERNAME, categoryCodes);
    Mockito.verify(this.categoryRedisTemplate, times(2)).opsForValue();
    verify(this.productCategoryBaseClient).getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
            CachedServiceImplTest.USERNAME, CachedServiceImplTest.CATEGORY_CODE);
  }

  @Test
  public void getParentCategoriesFromDbAndCacheNullCategoryCodeTest() {
    Set<String> categoryCodes = new HashSet<>();
    categoryCodes.add(CATEGORY_CODE);
    try {
      Mockito.when(categoryRedisTemplate.opsForValue()).thenReturn(Mockito.mock(ValueOperations.class));
      Mockito.when(productCategoryBaseClient.getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
          CachedServiceImplTest.USERNAME, CATEGORY_CODE)).thenThrow(ApplicationRuntimeException.class);
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          this.cachedServiceImpl.getParentCategoriesFromDbAndCache(CachedServiceImplTest.REQUEST_ID,
              CachedServiceImplTest.USERNAME, categoryCodes));
    } finally {
      Mockito.verify(this.categoryRedisTemplate, times(1)).opsForValue();
      Mockito.verify(this.productCategoryBaseClient)
          .getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID, CachedServiceImplTest.USERNAME, CATEGORY_CODE);
    }
  }

  @Test
  public void getParentCategoriesFromDbAndCacheWithCacheKeysTest() {
    Set<String> categoryCodes = new HashSet<>();
    categoryCodes.add(CATEGORY_CODE);
    List<List<CategoryResponse>> categoryResponseList = new ArrayList<>();
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    categoryResponses.add(categoryResponse);
    categoryResponseList.add(categoryResponses);
    Mockito.when(categoryRedisTemplate.opsForValue()).thenReturn(valueOperations);
    Mockito.when(valueOperations.multiGet(Mockito.any()))
            .thenReturn((categoryResponseList));
    Map<String, List<CategoryResponse>> listOfCategories =
            this.cachedServiceImpl.getParentCategoriesFromDbAndCache(CachedServiceImplTest.REQUEST_ID,
                    CachedServiceImplTest.USERNAME, categoryCodes);
    Mockito.verify(this.categoryRedisTemplate).opsForValue();
  }

  @Test
  public void getParentCategoriesFromDbAndCacheWithWrongInstanceObjectTest() {
    Set<String> categoryCodes = new HashSet<>();
    categoryCodes.add(CATEGORY_CODE);
    List<List<CategoryResponse>> categoryResponseList = new ArrayList<>();
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    categoryResponses.add(categoryResponse);
    categoryResponseList.add(categoryResponses);
    Mockito.when(categoryRedisTemplate.opsForValue()).thenReturn(valueOperations);
    Map<String, List<CategoryResponse>> listOfCategories =
        this.cachedServiceImpl.getParentCategoriesFromDbAndCache(CachedServiceImplTest.REQUEST_ID,
            CachedServiceImplTest.USERNAME, categoryCodes);
    Mockito.verify(this.categoryRedisTemplate, times(2)).opsForValue();
    verify(this.productCategoryBaseClient).getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
        CachedServiceImplTest.USERNAME, CATEGORY_CODE);
  }

  @Test
  public void getParentCategoriesFromDbAndCacheWithCacheKeysOfDifferentObjectTest() {
    Set<String> categoryCodes = new HashSet<>();
    categoryCodes.add(CATEGORY_CODE);
    List<List<CategoryResponse>> categoryResponseList = new ArrayList<>();
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponseList.add(categoryResponses);
    Mockito.when(categoryRedisTemplate.opsForValue()).thenReturn(valueOperations);
    Mockito.when(valueOperations.multiGet(Mockito.any()))
        .thenReturn(categoryResponseList);
    Map<String, List<CategoryResponse>> listOfCategories =
        this.cachedServiceImpl.getParentCategoriesFromDbAndCache(CachedServiceImplTest.REQUEST_ID,
            CachedServiceImplTest.USERNAME, categoryCodes);
    Mockito.verify(this.categoryRedisTemplate, times(2)).opsForValue();
    verify(this.productCategoryBaseClient).getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
        CachedServiceImplTest.USERNAME, CATEGORY_CODE);
  }

  @Test
  public void getParentCategoriesFromDbAndCacheWithCacheKeysOfDifferentObjectEmptyTest() {
    Set<String> categoryCodes = new HashSet<>();
    categoryCodes.add(CATEGORY_CODE);
    List<List<CategoryResponse>> categoryResponseList = new ArrayList<>();
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponseList.add(categoryResponses);
    Mockito.when(categoryRedisTemplate.opsForValue()).thenReturn(valueOperations);
    Map<String, List<CategoryResponse>> listOfCategories =
        this.cachedServiceImpl.getParentCategoriesFromDbAndCache(CachedServiceImplTest.REQUEST_ID,
            CachedServiceImplTest.USERNAME, categoryCodes);
    Mockito.verify(this.categoryRedisTemplate, times(2)).opsForValue();
    verify(this.productCategoryBaseClient).getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
        CachedServiceImplTest.USERNAME, CATEGORY_CODE);
  }

  @Test
  public void getParentCategoriesFromDbAndCacheWithCacheKeysOfDifferentObjectEmptyErrorTest() {
    Set<String> categoryCodes = new HashSet<>();
    categoryCodes.add(CATEGORY_CODE);
    List<List<CategoryResponse>> categoryResponseList = new ArrayList<>();
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponseList.add(categoryResponses);
    Mockito.when(categoryRedisTemplate.opsForValue()).thenThrow(RedisException.class);
    Map<String, List<CategoryResponse>> listOfCategories =
        this.cachedServiceImpl.getParentCategoriesFromDbAndCache(CachedServiceImplTest.REQUEST_ID,
            CachedServiceImplTest.USERNAME, categoryCodes);
    Mockito.verify(this.categoryRedisTemplate, times(2)).opsForValue();
    verify(this.productCategoryBaseClient).getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
        CachedServiceImplTest.USERNAME, CATEGORY_CODE);
  }

  @Test
  public void getParentCategoriesFromDbAndCacheWithEmptyCacheObjectTest() {
    Set<String> categoryCodes = new HashSet<>();
    categoryCodes.add(CATEGORY_CODE);
    List<List<CategoryResponse>> categoryResponseList = new ArrayList<>();
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    categoryResponses.add(categoryResponse);
    categoryResponseList.add(categoryResponses);
    Mockito.when(categoryRedisTemplate.opsForValue()).thenReturn(valueOperations);
    Mockito.when(valueOperations.multiGet(Collections.EMPTY_LIST))
            .thenReturn(new ArrayList<>());
    Map<String, List<CategoryResponse>> listOfCategories =
            this.cachedServiceImpl.getParentCategoriesFromDbAndCache(CachedServiceImplTest.REQUEST_ID,
                    CachedServiceImplTest.USERNAME, categoryCodes);
    Mockito.verify(this.categoryRedisTemplate, times(2)).opsForValue();
    verify(this.productCategoryBaseClient).getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
            CachedServiceImplTest.USERNAME, CATEGORY_CODE);
  }


  @Test
  public void getParentCategoriesFromDbAndCacheWithEmptyCacheObjectNullTest() {
    Set<String> categoryCodes = new HashSet<>();
    categoryCodes.add(CATEGORY_CODE);
    List<List<CategoryResponse>> categoryResponseList = new ArrayList<>();
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    categoryResponseList.add(categoryResponses);
    categoryResponseList.add(null);
    Mockito.when(categoryRedisTemplate.opsForValue()).thenReturn(valueOperations);
    Mockito.when(valueOperations.multiGet(Mockito.any()))
        .thenReturn(categoryResponseList);
    Map<String, List<CategoryResponse>> listOfCategories =
        this.cachedServiceImpl.getParentCategoriesFromDbAndCache(CachedServiceImplTest.REQUEST_ID,
            CachedServiceImplTest.USERNAME, categoryCodes);
    Mockito.verify(this.categoryRedisTemplate, times(2)).opsForValue();
    verify(this.productCategoryBaseClient).getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
        CachedServiceImplTest.USERNAME, CATEGORY_CODE);
  }

  @Test
  public void getParentCategoriesFromMasterDataTestWithCategoryCodeNotFound() throws Exception {
    try {
      this.cachedServiceImpl.getParentCategoriesFromMasterData(CachedServiceImplTest.REQUEST_ID,
          CachedServiceImplTest.USERNAME, CachedServiceImplTest.CATEGORY_CODE_NOT_FOUND);
    } catch (Exception e) {
      verify(this.productCategoryBaseClient).getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
          CachedServiceImplTest.USERNAME, CachedServiceImplTest.CATEGORY_CODE_NOT_FOUND);
      assertTrue(e instanceof Exception);
    }
  }

  @Test
  public void getParentCategoriesFromMasterDataTestWithEmptyResultReturnFromPCB() throws Exception {
    when(this.productCategoryBaseClient.getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
        CachedServiceImplTest.USERNAME, CachedServiceImplTest.CATEGORY_CODE))
            .thenReturn(new ArrayList<CategoryResponse>());
    try {
      this.cachedServiceImpl.getParentCategoriesFromMasterData(CachedServiceImplTest.REQUEST_ID,
          CachedServiceImplTest.USERNAME, CachedServiceImplTest.CATEGORY_CODE);
    } catch (Exception e) {
      verify(this.productCategoryBaseClient).getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
          CachedServiceImplTest.USERNAME, CachedServiceImplTest.CATEGORY_CODE);
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void getParentCategoriesFromMasterDataTestWithNullCategoryCode() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.cachedServiceImpl.getParentCategoriesFromMasterData(CachedServiceImplTest.REQUEST_ID,
        CachedServiceImplTest.USERNAME, null));
  }

  @Test
  public void getParentCategoriesFromMasterDataTestWithNullRequestId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->this.cachedServiceImpl.getParentCategoriesFromMasterData(null, CachedServiceImplTest.USERNAME,
        CachedServiceImplTest.CATEGORY_CODE));
  }

  @Test
  public void getParentCategoriesFromMasterDataTestWithNullResultReturnFromPCB() throws Exception {
    when(this.productCategoryBaseClient.getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
        CachedServiceImplTest.USERNAME, CachedServiceImplTest.CATEGORY_CODE)).thenReturn(null);
    try {
      this.cachedServiceImpl.getParentCategoriesFromMasterData(CachedServiceImplTest.REQUEST_ID,
          CachedServiceImplTest.USERNAME, CachedServiceImplTest.CATEGORY_CODE);
    } catch (Exception e) {
      verify(this.productCategoryBaseClient).getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
          CachedServiceImplTest.USERNAME, CachedServiceImplTest.CATEGORY_CODE);
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void getMasterParentCategoryResponseByProductCodeTest() {

    Mockito.when(productCategoryBaseClient
        .getMasterParentCategoryResponseByProductCode(CachedServiceImplTest.REQUEST_ID,
            CachedServiceImplTest.USERNAME, CachedServiceImplTest.PRODUCT_CODE))
        .thenReturn(this.listOfCategoryResponse);
    List<CategoryResponse> listOfCategories = this.cachedServiceImpl
        .getMasterParentCategoryResponseByProductCode(CachedServiceImplTest.REQUEST_ID,
            CachedServiceImplTest.USERNAME, CachedServiceImplTest.PRODUCT_CODE);
    Mockito.verify(productCategoryBaseClient)
        .getMasterParentCategoryResponseByProductCode(CachedServiceImplTest.REQUEST_ID,
            CachedServiceImplTest.USERNAME, CachedServiceImplTest.PRODUCT_CODE);
    assertNotNull(listOfCategories);
    assertEquals(listOfCategories.size(), 3);
    for (int i = 0; i < listOfCategories.size(); i++) {
      assertEquals(listOfCategories.get(i), this.listOfCategoryResponse.get(i));
    }
  }

  @Test
  public void getMasterParentCategoryResponseByProductCode_whenProductCodeNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->this.cachedServiceImpl
        .getMasterParentCategoryResponseByProductCode(CachedServiceImplTest.REQUEST_ID,
            CachedServiceImplTest.USERNAME, null));
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);


    this.categoryResponseC3 = new CategoryResponse();
    this.categoryResponseC2 = new CategoryResponse();
    this.categoryResponseC1 = new CategoryResponse();

    this.listOfCategoryResponse = new ArrayList<CategoryResponse>();
    this.listOfCategoryResponse.add(this.categoryResponseC3);
    this.listOfCategoryResponse.add(this.categoryResponseC2);
    this.listOfCategoryResponse.add(this.categoryResponseC1);

    when(this.productCategoryBaseClient.getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID,
        CachedServiceImplTest.USERNAME, CachedServiceImplTest.CATEGORY_CODE))
            .thenReturn(this.listOfCategoryResponse);


    doThrow(new ApplicationRuntimeException()).when(this.productCategoryBaseClient)
        .getCategoryHierarchy(CachedServiceImplTest.REQUEST_ID, CachedServiceImplTest.USERNAME,
            CachedServiceImplTest.CATEGORY_CODE_NOT_FOUND);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.productCategoryBaseClient);
  }
}
