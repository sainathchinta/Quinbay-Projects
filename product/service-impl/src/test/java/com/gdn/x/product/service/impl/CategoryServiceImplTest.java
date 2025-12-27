package com.gdn.x.product.service.impl;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.rest.web.model.response.IgnoreAttributeSet;
import com.gdn.x.product.service.api.ProductScoreRuleService;
import com.gdn.x.product.service.api.SystemParameterService;

public class CategoryServiceImplTest {

  private static final String PRODUCT_SKU = "product-sku";

  private static final int SEQUENCE = 1;

  private static final String STORE_ID = "store-id";

  private static final String CATALOG_CODE = "catalog-code";

  private static final String CATEGORY_CODE = "category-code";

  private static final boolean SEARCH_EMPTY_SALES_ONLY = true;

  private static final Pageable GENERATE_PAGEABLE = PageRequest.of(0, 10);

  @InjectMocks
  private CategoryServiceImpl categoryServiceImpl;

  @Mock
  private ProductAndItemSolrRepository solrRepository;

  @Mock
  private ProductScoreRuleService productScoreRuleService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ObjectMapper objectMapper;

  private SystemParameter systemParameter1;
  private SystemParameter systemParameter2;
  private IgnoreAttributeSet ignoreAttributeSet;

  @Test
  public void getProductsByMasterCatalogSuccess() {
    this.categoryServiceImpl.getProductsByMasterCatalog(CategoryServiceImplTest.STORE_ID,
        CategoryServiceImplTest.CATALOG_CODE, CategoryServiceImplTest.CATEGORY_CODE,
        CategoryServiceImplTest.SEARCH_EMPTY_SALES_ONLY, CategoryServiceImplTest.GENERATE_PAGEABLE);
    verify(this.solrRepository).getProductsByMasterCatalog(CategoryServiceImplTest.STORE_ID,
        CategoryServiceImplTest.CATALOG_CODE, CategoryServiceImplTest.CATEGORY_CODE,
        CategoryServiceImplTest.SEARCH_EMPTY_SALES_ONLY, CategoryServiceImplTest.GENERATE_PAGEABLE);
  }

  @Test
  public void getProductsByMasterCatalogWithNullCatalogCode() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.categoryServiceImpl.getProductsByMasterCatalog(CategoryServiceImplTest.STORE_ID, null,
        CategoryServiceImplTest.CATEGORY_CODE, CategoryServiceImplTest.SEARCH_EMPTY_SALES_ONLY,
        CategoryServiceImplTest.GENERATE_PAGEABLE));
  }

  @Test
  public void getProductsByMasterCatalogWithNullCategoryCode() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->this.categoryServiceImpl.getProductsByMasterCatalog(CategoryServiceImplTest.STORE_ID,
        CategoryServiceImplTest.CATALOG_CODE, null,
        CategoryServiceImplTest.SEARCH_EMPTY_SALES_ONLY, CategoryServiceImplTest.GENERATE_PAGEABLE));
  }

  @Test
  public void getProductsByMasterCatalogWithNullStoreId() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->this.categoryServiceImpl.getProductsByMasterCatalog(null, CategoryServiceImplTest.CATALOG_CODE,
        CategoryServiceImplTest.CATEGORY_CODE, CategoryServiceImplTest.SEARCH_EMPTY_SALES_ONLY,
        CategoryServiceImplTest.GENERATE_PAGEABLE));
  }

  @Test
  public void getProductsBySalesCatalogSuccess() throws Exception {
    this.categoryServiceImpl.getProductsBySalesCatalog(CategoryServiceImplTest.STORE_ID,
        CategoryServiceImplTest.CATALOG_CODE, CategoryServiceImplTest.CATEGORY_CODE,
        CategoryServiceImplTest.GENERATE_PAGEABLE);
    verify(this.solrRepository).getProductsBySalesCatalog(CategoryServiceImplTest.STORE_ID,
        CategoryServiceImplTest.CATALOG_CODE, CategoryServiceImplTest.CATEGORY_CODE,
        CategoryServiceImplTest.GENERATE_PAGEABLE);
  }

  @Test
  public void getProductsBySalesCatalogWithNullCatalogCode() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.categoryServiceImpl.getProductsBySalesCatalog(CategoryServiceImplTest.STORE_ID, null,
            CategoryServiceImplTest.CATEGORY_CODE, CategoryServiceImplTest.GENERATE_PAGEABLE));
  }

  @Test
  public void getProductsBySalesCatalogWithNullCategoryCode() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.categoryServiceImpl.getProductsBySalesCatalog(CategoryServiceImplTest.STORE_ID,
            CategoryServiceImplTest.CATALOG_CODE, null, CategoryServiceImplTest.GENERATE_PAGEABLE));
  }

  @Test
  public void getProductsBySalesCatalogWithNullStoreId() throws Exception{
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.categoryServiceImpl.getProductsBySalesCatalog(null, CategoryServiceImplTest.CATALOG_CODE,
        CategoryServiceImplTest.CATEGORY_CODE, CategoryServiceImplTest.GENERATE_PAGEABLE));
  }

  @Test
  public void getProductScoreRuleForCatgeoryTest() throws Exception {
    Mockito.when(productScoreRuleService.getProductScoreRulesForCategory(Constants.DEFAULT_STORE_ID, CATEGORY_CODE))
        .thenReturn(new HashMap<>());
    Mockito.when(objectMapper.readValue("[\" \"]", List.class)).thenReturn(Arrays.asList(" "));
    Mockito.when(objectMapper.readValue(objectMapper.writeValueAsString(ignoreAttributeSet), new TypeReference<List<IgnoreAttributeSet>>() {
    })).thenReturn(Collections.singletonList(ignoreAttributeSet));
    Mockito.when(systemParameterService
        .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.IGNORE_SYMBOLS_VARIABLE_NAME))
        .thenReturn(systemParameter1);
    Mockito.when(systemParameterService
        .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.IGNORE_ATTRIBUTE_VARIABLE_NAME))
        .thenReturn(systemParameter2);
    categoryServiceImpl.getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productScoreRuleService).getProductScoreRulesForCategory(Constants.DEFAULT_STORE_ID, CATEGORY_CODE);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.IGNORE_SYMBOLS_VARIABLE_NAME);
    Mockito.verify(objectMapper).readValue("[\" \"]", List.class);
  }

  @Test
  public void getProductScoreRuleForCatgeorEmptyTest() throws Exception {
    Mockito.when(productScoreRuleService.getProductScoreRulesGlobal(Constants.DEFAULT_STORE_ID))
        .thenReturn(new HashMap<>());
    Mockito.when(objectMapper.readValue("[\" \"]", List.class)).thenReturn(Arrays.asList(" "));
    Mockito.when(objectMapper.readValue(objectMapper.writeValueAsString(ignoreAttributeSet), new TypeReference<List<IgnoreAttributeSet>>() {
    })).thenReturn(Collections.singletonList(ignoreAttributeSet));
    Mockito.when(systemParameterService
        .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.IGNORE_SYMBOLS_VARIABLE_NAME))
        .thenReturn(systemParameter1);
    Mockito.when(systemParameterService
        .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.IGNORE_ATTRIBUTE_VARIABLE_NAME))
        .thenReturn(systemParameter2);
    categoryServiceImpl.getProductScoreRuleForCategory("");
    Mockito.verify(productScoreRuleService).getProductScoreRulesGlobal(Constants.DEFAULT_STORE_ID);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.IGNORE_SYMBOLS_VARIABLE_NAME);
    Mockito.verify(objectMapper).readValue("[\" \"]", List.class);
  }

  @BeforeEach
  public void init() throws Exception{
    openMocks(this);
    systemParameter1 = new SystemParameter();
    systemParameter1.setValue("[\" \"]");
    ignoreAttributeSet = new IgnoreAttributeSet("Warranty", "No Warranty", Arrays.asList("Warranty Detail"));
    systemParameter2 = new SystemParameter();
    systemParameter2.setValue(objectMapper.writeValueAsString(ignoreAttributeSet));
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.solrRepository);
  }

}
