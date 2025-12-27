package com.gdn.partners.pcu.internal.service.impl;


import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.redis.core.BoundValueOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.ext.catalog.rest.web.model.response.PristineCategoryMapResponse;
import com.gdn.partners.pcu.internal.client.feign.ExtCatalogFeign;
import com.gdn.partners.pcu.internal.properties.ExtCatalogProperties;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ProductSuggestionServiceImplTest {

  private static final String REQUEST_ID = "requestId";
  private static final String CATEGORY = "HANDPHONE";
  private static final String REDIS_CATEGORY_KEY = "SUPPORTED_CATEGORIES_BY_PRISTINE";
  private static final String CATEGORY_ID = "categoryId";
  private static final String BLIBLI_CATEGORY = "test";
  private static final String HANDPHONE_CATEGORY = "HANDPHONE";
  private static final String PRODUCT_NAME = "productName";
  private static final String PRODUCT_CODE = "productName";
  private static final int PAGE = 0;
  private static final int SIZE = 25;

  private Map<String, Set<String>> pristineSupportedCategoryMap;
  private Set<String> categorySet;
  private ProductCodeResponse productCodeResponse;

  @InjectMocks
  private ProductSuggestionServiceImpl extCatalogService;

  @Mock
  private ExtCatalogFeign extCatalogFeign;

  @Mock
  private ExtCatalogProperties extCatalogProperties;

  @Mock
  private RedisTemplate<String, Map<String, Set<String>>> redisTemplate;

  @Mock
  BoundValueOperations<String, Map<String, Set<String>>> operations;

  @BeforeEach
  public void setUp() throws Exception {
    pristineSupportedCategoryMap = new HashMap<>();
    categorySet = new HashSet<>();
    categorySet.add(CATEGORY_ID);
    pristineSupportedCategoryMap.put(CATEGORY, categorySet);
    when(extCatalogProperties.getPristineCategories()).thenReturn(CATEGORY);

    pristineSupportedCategoryMap = new HashMap<>();
    categorySet = new HashSet<>();
    categorySet.add(BLIBLI_CATEGORY);
    pristineSupportedCategoryMap.put(HANDPHONE_CATEGORY, categorySet);

    Map<String, Set<String>> expectedResponse = new HashMap<>();
    expectedResponse.put(HANDPHONE_CATEGORY, categorySet);

    productCodeResponse = new ProductCodeResponse();
    productCodeResponse.setProductName(PRODUCT_NAME);
    productCodeResponse.setProductCode(PRODUCT_CODE);
  }

  @Test
  public void getSupportedBlibliCategoriesByPristineFromRedisTest() {
    ReflectionTestUtils.invokeMethod(extCatalogService, "init");
    when(redisTemplate.boundValueOps(REDIS_CATEGORY_KEY)).thenReturn(operations);
    when(operations.get()).thenReturn(null);
    when(extCatalogFeign.getSupportedBlibliCategoriesByPristine()).thenReturn(
        new GdnRestSingleResponse<>(new PristineCategoryMapResponse(pristineSupportedCategoryMap), REQUEST_ID));
    when(extCatalogProperties.getPristineTimeoutPeriod()).thenReturn(5);
    Map<String, Set<String>> response = extCatalogService.getSupportedBlibliCategoriesByPristine();
    verify(extCatalogProperties).getPristineCategories();
    verify(redisTemplate, times(2)).boundValueOps(REDIS_CATEGORY_KEY);
    verify(operations).get();
    verify(operations).set(pristineSupportedCategoryMap);
    verify(extCatalogFeign).getSupportedBlibliCategoriesByPristine();
    verify(extCatalogProperties).getPristineTimeoutPeriod();
    verify(redisTemplate).expire(REDIS_CATEGORY_KEY, 5, TimeUnit.HOURS);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(pristineSupportedCategoryMap, response);
  }

  @Test
  public void getSupportedBlibliCategoriesByPristineTest() {
    ReflectionTestUtils.invokeMethod(extCatalogService, "init");
    when(redisTemplate.boundValueOps(REDIS_CATEGORY_KEY)).thenReturn(operations);
    when(operations.get()).thenReturn(pristineSupportedCategoryMap);
    Map<String, Set<String>> response = extCatalogService.getSupportedBlibliCategoriesByPristine();
    verify(extCatalogProperties).getPristineCategories();
    verify(redisTemplate).boundValueOps(REDIS_CATEGORY_KEY);
    verify(operations).get();
    Assertions.assertNotNull(response);
    Assertions.assertEquals(pristineSupportedCategoryMap, response);
  }

  @Test
  public void getSupportedBlibliCategoriesByPristineClientExceptionTest() {
    ReflectionTestUtils.invokeMethod(extCatalogService, "init");
    when(redisTemplate.boundValueOps(REDIS_CATEGORY_KEY)).thenReturn(operations);
    when(operations.get()).thenReturn(pristineSupportedCategoryMap);
    try {
      extCatalogService.getSupportedBlibliCategoriesByPristine();
    } catch (ClientException e) {
    } finally {
      verify(extCatalogProperties).getPristineCategories();
      verify(redisTemplate).boundValueOps(REDIS_CATEGORY_KEY);
      verify(operations).get();
    }
  }

  @Test
  public void getPCBProductCodesTest() {
    when(extCatalogFeign.getPCBProductCodes(PRODUCT_CODE, CATEGORY, PAGE, SIZE)).thenReturn(new GdnRestListResponse<>(
        Arrays.asList(productCodeResponse), new PageMetaData(), REQUEST_ID));
    List<ProductCodeResponse> response =
        extCatalogService.getPCBProductCodes(PRODUCT_CODE, CATEGORY, PageRequest.of(PAGE, SIZE));
    verify(extCatalogFeign).getPCBProductCodes(PRODUCT_CODE, CATEGORY, PAGE, SIZE);
    Assertions.assertEquals(PRODUCT_CODE, response.get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, response.get(0).getProductName());
  }

  @Test
  public void getPCBProductCodesClientExceptionTest() {
    when(extCatalogFeign.getPCBProductCodes(PRODUCT_CODE, CATEGORY, PAGE, SIZE)).thenReturn(null);
    try {
      extCatalogService.getPCBProductCodes(PRODUCT_CODE, CATEGORY, PageRequest.of(PAGE, SIZE));
    } catch (ClientException e) {
    } finally {
      verify(extCatalogFeign).getPCBProductCodes(PRODUCT_CODE, CATEGORY, PAGE, SIZE);
    }
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(extCatalogFeign);
    Mockito.verifyNoMoreInteractions(extCatalogProperties);
    Mockito.verifyNoMoreInteractions(redisTemplate);
    Mockito.verifyNoMoreInteractions(operations);
  }
}