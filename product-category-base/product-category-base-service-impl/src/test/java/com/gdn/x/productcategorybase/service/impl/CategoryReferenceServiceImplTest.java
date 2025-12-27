package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.Assert;

import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.dto.CategoryCodeAndUmkmFlagDTO;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.repository.CategoryReferenceRepository;

public class CategoryReferenceServiceImplTest {

  private static final String OLD_CATEGORY_ID = "oldCategoryId";
  private static final String NEW_CATEGORY_ID = "newCategoryId";
  private static final String OLD_SALES_CATEGORY = "sales_1";
  private static final String NEW_SALES_CATEGORY = "sales_2";
  private static final String SALES_CATEGORY_CODE_1 = "category_1";
  private static final String SALES_CATEGORY_CODE_2 = "category_2";
  private static final String SALES_CATEGORY_CODE_3 = "category_3";
  private static final String STORE_ID = "storeId";
  private static final String CATEGORY_ID = "categoryId";
  private static final String B2B_SALES_CATALOG_CODE = "12052";
  private static final String SALES_CATALOG_CODE = "12051";
  private List<CategoryCodeAndUmkmFlagDTO> salesCategoryAndUmkmList;

  @Mock
  private CategoryReferenceRepository categoryReferenceRepository;

  @InjectMocks
  private CategoryReferenceServiceImpl categoryReferenceService;


  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);
    salesCategoryAndUmkmList = new ArrayList<>();
    Catalog catalog = new Catalog();
    catalog.setCatalogCode(B2B_SALES_CATALOG_CODE);
    Catalog catalog1 = new Catalog();
    catalog1.setCatalogCode(SALES_CATALOG_CODE);
    CategoryCodeAndUmkmFlagDTO categoryCodeAndUmkmFlagDTO1 =
        new CategoryCodeAndUmkmFlagDTO(SALES_CATEGORY_CODE_1, true, catalog);
    CategoryCodeAndUmkmFlagDTO categoryCodeAndUmkmFlagDTO2 =
        new CategoryCodeAndUmkmFlagDTO(SALES_CATEGORY_CODE_2, false, new Catalog());
    CategoryCodeAndUmkmFlagDTO categoryCodeAndUmkmFlagDTO3 =
        new CategoryCodeAndUmkmFlagDTO(SALES_CATEGORY_CODE_3, false, catalog1);
    salesCategoryAndUmkmList.add(categoryCodeAndUmkmFlagDTO1);
    salesCategoryAndUmkmList.add(categoryCodeAndUmkmFlagDTO2);
    salesCategoryAndUmkmList.add(categoryCodeAndUmkmFlagDTO3);
    ReflectionTestUtils.setField(categoryReferenceService, "b2bSalesCatalogCode", "12052");
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(categoryReferenceRepository);
  }

  @Test
  public void getSalesCategoryReferenceByMasterCategoryTest() {
    Mockito.when(
            categoryReferenceRepository.findAllSalesCategoryCodesByMasterCategoryId(OLD_CATEGORY_ID))
        .thenReturn(salesCategoryAndUmkmList);
    Mockito.when(
        categoryReferenceRepository.findSalesCategoryCodesByMarkForDeleteFalseAndMasterCategoryId(NEW_CATEGORY_ID))
        .thenReturn(salesCategoryAndUmkmList);
    ProductSalesCategoryMapping salesCategoryReferenceByMasterCategory =
        categoryReferenceService.getSalesCategoryReferenceByMasterCategory(OLD_CATEGORY_ID, NEW_CATEGORY_ID, false);
    Mockito.verify(categoryReferenceRepository).findAllSalesCategoryCodesByMasterCategoryId(OLD_CATEGORY_ID);
    Mockito.verify(categoryReferenceRepository).findSalesCategoryCodesByMarkForDeleteFalseAndMasterCategoryId(NEW_CATEGORY_ID);
    Assertions.assertEquals(SALES_CATEGORY_CODE_2, salesCategoryReferenceByMasterCategory.getNewSalesCategoryCodes().get(0));
    Assertions.assertEquals(SALES_CATEGORY_CODE_1,
        salesCategoryReferenceByMasterCategory.getNewUmkmSalesCategoryCodes().get(0));
    Assertions.assertEquals(1, salesCategoryReferenceByMasterCategory.getNewB2bSalesCategoryCodes().size());
    Assertions.assertEquals(1, salesCategoryReferenceByMasterCategory.getOldB2bSalesCategoryCodes().size());
    Assertions.assertEquals(1, salesCategoryReferenceByMasterCategory.getOldB2bSalesCategoryCodes().size());
  }

  @Test
  public void getSalesCategoryReferenceByMasterCategoryIgnoreHalalTrueTest() {
    Mockito.when(
        categoryReferenceRepository.findAllSalesCategoryCodesByMasterCategoryIdAndHalalCategory(OLD_CATEGORY_ID, false))
        .thenReturn(salesCategoryAndUmkmList);
    Mockito.when(
        categoryReferenceRepository.findSalesCategoryCodesByMarkForDeleteFalseAndMasterCategoryIdAndHalalCategory(NEW_CATEGORY_ID, false))
        .thenReturn(salesCategoryAndUmkmList);
    ProductSalesCategoryMapping salesCategoryReferenceByMasterCategory =
        categoryReferenceService.getSalesCategoryReferenceByMasterCategory(OLD_CATEGORY_ID, NEW_CATEGORY_ID, true);
    Mockito.verify(categoryReferenceRepository).findAllSalesCategoryCodesByMasterCategoryIdAndHalalCategory(OLD_CATEGORY_ID, false);
    Mockito.verify(categoryReferenceRepository).findSalesCategoryCodesByMarkForDeleteFalseAndMasterCategoryIdAndHalalCategory(NEW_CATEGORY_ID, false);
    Assertions.assertEquals(SALES_CATEGORY_CODE_2, salesCategoryReferenceByMasterCategory.getNewSalesCategoryCodes().get(0));
    Assertions.assertEquals(SALES_CATEGORY_CODE_1,
        salesCategoryReferenceByMasterCategory.getNewUmkmSalesCategoryCodes().get(0));
    Assertions.assertEquals(1, salesCategoryReferenceByMasterCategory.getNewB2bSalesCategoryCodes().size());
    Assertions.assertEquals(1, salesCategoryReferenceByMasterCategory.getOldB2bSalesCategoryCodes().size());
    Assertions.assertEquals(1, salesCategoryReferenceByMasterCategory.getOldB2bSalesCategoryCodes().size());
  }

  @Test
  public void getSalesCategoryReferenceByMasterCategoryEmptyOldCategoryId() {
    ProductSalesCategoryMapping salesCategoryReferenceByMasterCategory =
        categoryReferenceService.getSalesCategoryReferenceByMasterCategory(null, NEW_CATEGORY_ID, false);
    Assertions.assertNull(salesCategoryReferenceByMasterCategory);
  }

  @Test
  public void getSalesCategoryReferenceByMasterCategoryEmptyNewCategoryId() {
    ProductSalesCategoryMapping salesCategoryReferenceByMasterCategory =
        categoryReferenceService.getSalesCategoryReferenceByMasterCategory(OLD_CATEGORY_ID, StringUtils.EMPTY, false);
    Assertions.assertNull(salesCategoryReferenceByMasterCategory);
  }

  @Test
  public void getSalesCategoryIdByMasterCategoryIdId() {
    Mockito.when(categoryReferenceRepository.findIdsByStoreIdAndMasterCategoryReferenceId(STORE_ID,
        CATEGORY_ID)).thenReturn(new ArrayList<>());
    categoryReferenceService.getSalesCategoryIdByMasterCategoryId(STORE_ID, CATEGORY_ID);
    Mockito.verify(categoryReferenceRepository).findIdsByStoreIdAndMasterCategoryReferenceId(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void getMasterCategoryIdBySalesCategoryId() {
    Mockito.when(categoryReferenceRepository.findIdsByStoreIdAndSalesCategoryReferenceId(STORE_ID,
        CATEGORY_ID)).thenReturn(new ArrayList<>());
    categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, CATEGORY_ID);
    Mockito.verify(categoryReferenceRepository).findIdsByStoreIdAndSalesCategoryReferenceId(STORE_ID, CATEGORY_ID);
  }
}