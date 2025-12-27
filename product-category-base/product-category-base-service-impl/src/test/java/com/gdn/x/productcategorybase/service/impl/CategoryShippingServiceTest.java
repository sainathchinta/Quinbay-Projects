package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.common.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryShipping;
import com.gdn.x.productcategorybase.repository.CategoryShippingRepository;
import com.gdn.x.productcategorybase.service.CategoryService;

;

public class CategoryShippingServiceTest {
  private static final String BLANK = "";

  private static final String STORE_ID = "STORE_ID";

  private static final int PAGE_SIZE = 10;
  private static final int PAGE_NUMBER = 0;

  private static final String CATEGORY_CODE_A = "Category A";
  private static final String CATEGORY_CODE_B = "Category B";
  private static final String SHIPPING_CODE_1 = "Snail mail";
  private static final String SHIPPING_CODE_2 = "Private jet";

  private static final double LENGTH = 10;
  private static final double WIDTH = 10;
  private static final double HEIGHT = 10;
  private static final double WEIGHT = 10;

  private static final String ID_A1 = "ID-A1";
  private static final String ID_A2 = "ID-A2";
  private static final String ID_B1 = "ID-B1";
  private static final String ID_B2 = "ID-B2";

  private static final Pageable DEFAULT_PAGE_REQUEST = PageRequest.of(0, 10);

  private static final String ERROR_MESSAGE_DATA_NON_EXISTING = "Can not perform delete on non-existing data : ";
  private static final String ERROR_MESSAGE_DATA_ACCESS = "use update for existing entity with id : ";
  private static final String ERROR_MESSAGE_DATA_NOT_FOUND = "can not update un existence data with id : ";
  private static final String ERROR_MESSAGE_CATEGORY_NOT_FOUND =
      "Can not find data :Category with category code Category A is not found";
  private static final String ERROR_MESSAGE_NOT_A_MASTER_CATALOG =
      "Can not process invalid input data :Category with category code Category A is not master catalog";
  private static final String CATEGORY_CODE_EMPTY = "Can not process invalid input data : categoryCode should not be empty";
  private static final String STORE_ID_EMPTY = "Can not process invalid input data : storeId should not be empty";

  @InjectMocks
  CategoryShippingServiceBean service;

  @Mock
  private CategoryService categoryService;

  @Mock
  CategoryShippingRepository repository;

  private String uuid;
  private CategoryShipping catShipCodeA1;
  private CategoryShipping catShipCodeA2;
  private CategoryShipping catShipCodeB1;
  private CategoryShipping catShipCodeB2;
  private CategoryShipping savedCatShipCodeB2;
  private Category category;
  private List<Category> categoryList;
  private List<CategoryShipping> catShipCodeList;
  private List<CategoryShipping> catShipCodeListA;
  private List<CategoryShipping> catShipCodeList1;
  private List<CategoryShipping> catShipCodeList2;
  private List<CategoryShipping> catShipCodeListTest;
  private Pageable pageable;
  private Page<CategoryShipping> catShipCodePage;
  private Page<CategoryShipping> catShipCodePageA;
  private Page<CategoryShipping> catShipCodePage1;

  @BeforeEach
  public void setUp() {
    this.pageable =
        PageRequest.of(CategoryShippingServiceTest.PAGE_NUMBER, CategoryShippingServiceTest.PAGE_SIZE);
    MockitoAnnotations.initMocks(this);

    this.catShipCodeA1 = new CategoryShipping(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.CATEGORY_CODE_A, CategoryShippingServiceTest.SHIPPING_CODE_1);
    this.catShipCodeA2 = new CategoryShipping(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.CATEGORY_CODE_A, CategoryShippingServiceTest.SHIPPING_CODE_2);
    this.catShipCodeB1 = new CategoryShipping(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.CATEGORY_CODE_B, CategoryShippingServiceTest.SHIPPING_CODE_1);
    this.catShipCodeB2 = new CategoryShipping(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.CATEGORY_CODE_B, CategoryShippingServiceTest.SHIPPING_CODE_2);

    this.catShipCodeA1.setId(CategoryShippingServiceTest.ID_A1);
    this.catShipCodeA2.setId(CategoryShippingServiceTest.ID_A2);
    this.catShipCodeB1.setId(CategoryShippingServiceTest.ID_B1);
    this.catShipCodeB2.setId(CategoryShippingServiceTest.ID_B2);

    this.savedCatShipCodeB2 = new CategoryShipping();
    BeanUtils.copyProperties(this.catShipCodeB2, this.savedCatShipCodeB2);
    this.uuid = GdnUUIDHelper.generateUUID();
    this.savedCatShipCodeB2.setId(this.uuid);

    this.catShipCodeList = new ArrayList<CategoryShipping>();
    this.catShipCodeList.add(this.catShipCodeA1);
    this.catShipCodeList.add(this.catShipCodeA2);
    this.catShipCodeList.add(this.catShipCodeB1);
    this.catShipCodeList.add(this.catShipCodeB2);

    this.catShipCodeListA = new ArrayList<CategoryShipping>();
    this.catShipCodeListA.add(this.catShipCodeA1);
    this.catShipCodeListA.add(this.catShipCodeA2);

    this.catShipCodeList1 = new ArrayList<CategoryShipping>();
    this.catShipCodeList1.add(this.catShipCodeA1);
    this.catShipCodeList1.add(this.catShipCodeB1);

    this.catShipCodeList2 = new ArrayList<CategoryShipping>();
    this.catShipCodeList2.add(this.catShipCodeA2);
    this.catShipCodeList2.add(this.catShipCodeB2);

    this.catShipCodePage = new PageImpl<CategoryShipping>(this.catShipCodeList,
        CategoryShippingServiceTest.DEFAULT_PAGE_REQUEST, this.catShipCodeList.size());

    this.catShipCodePageA = new PageImpl<CategoryShipping>(this.catShipCodeListA,
        CategoryShippingServiceTest.DEFAULT_PAGE_REQUEST, this.catShipCodeListA.size());

    this.catShipCodePage1 = new PageImpl<CategoryShipping>(this.catShipCodeList1,
        CategoryShippingServiceTest.DEFAULT_PAGE_REQUEST, this.catShipCodeList1.size());

    when(this.repository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.CATEGORY_CODE_A)).thenReturn(this.catShipCodeListA);
    when(this.repository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.CATEGORY_CODE_A, this.pageable)).thenReturn(this.catShipCodePageA);
    when(this.repository.findById(CategoryShippingServiceTest.ID_A1)).thenReturn(Optional.of(this.catShipCodeA1));
    when(this.repository.findById(CategoryShippingServiceTest.ID_A2)).thenReturn(Optional.of(this.catShipCodeA2));
    when(this.repository.findById(CategoryShippingServiceTest.ID_B1)).thenReturn(Optional.of(this.catShipCodeB1));
    when(this.repository.findById(CategoryShippingServiceTest.ID_B2)).thenReturn(Optional.of(this.catShipCodeB1));
    when(this.repository.findByStoreIdAndShippingCodeAndMarkForDeleteFalse(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.SHIPPING_CODE_1)).thenReturn(this.catShipCodeList1);
    when(this.repository.findByStoreIdAndShippingCodeAndMarkForDeleteFalse(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.SHIPPING_CODE_1, this.pageable)).thenReturn(this.catShipCodePage1);
    when(this.repository.findByStoreIdAndMarkForDeleteFalse(CategoryShippingServiceTest.STORE_ID))
        .thenReturn(this.catShipCodeList);
    when(this.repository.findByStoreIdAndMarkForDeleteFalse(CategoryShippingServiceTest.STORE_ID, this.pageable))
        .thenReturn(this.catShipCodePage);
    when(this.repository.findByStoreIdAndId(CategoryShippingServiceTest.STORE_ID, CategoryShippingServiceTest.ID_A1))
        .thenReturn(this.catShipCodeA1);
    when(this.repository.findByStoreIdAndId(CategoryShippingServiceTest.STORE_ID, CategoryShippingServiceTest.ID_A2))
        .thenReturn(this.catShipCodeA2);
    when(this.repository.findByStoreIdAndId(CategoryShippingServiceTest.STORE_ID, CategoryShippingServiceTest.ID_B1))
        .thenReturn(this.catShipCodeB1);
    when(this.repository.findByStoreIdAndId(CategoryShippingServiceTest.STORE_ID, CategoryShippingServiceTest.ID_B2))
        .thenReturn(this.catShipCodeB2);
    when(this.repository.saveAndFlush(this.catShipCodeB2)).thenReturn(this.savedCatShipCodeB2);
    category = new Category();
    category.setCategoryCode(CATEGORY_CODE_A);
    category.setCatalog(new Catalog());
    category.setLogisticAdjustment(100);
    category.getCatalog().setCatalogType(CatalogType.MASTER_CATALOG);
    categoryList = new ArrayList<>();
    categoryList.add(category);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.repository);
    Mockito.verifyNoMoreInteractions(this.categoryService);
  }

  @Test
  public void testFindByCategoryName() throws Exception {
    List<CategoryShipping> result = this.service.findByCategoryCode(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.CATEGORY_CODE_A);
    assertEquals(result, (this.catShipCodeListA));
    verify(this.repository).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.CATEGORY_CODE_A);
  }

  @Test
  public void testFindByCategoryNamePageable() throws Exception {
    Page<CategoryShipping> result = this.service.findByCategoryCode(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.CATEGORY_CODE_A, this.pageable);
    assertEquals(result, (this.catShipCodePageA));
    verify(this.repository).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.CATEGORY_CODE_A, this.pageable);
  }

  @Test
  public void testFindById() throws Exception {
    CategoryShipping result = this.service.findById(CategoryShippingServiceTest.ID_B1);
    assertEquals(result, (this.catShipCodeB1));
    verify(this.repository).findById(CategoryShippingServiceTest.ID_B1);
  }

  @Test
  public void testFindByShippingCode() throws Exception {
    List<CategoryShipping> result = this.service.findByShippingCode(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.SHIPPING_CODE_1);
    assertEquals(result, (this.catShipCodeList1));
    verify(this.repository).findByStoreIdAndShippingCodeAndMarkForDeleteFalse(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.SHIPPING_CODE_1);
  }

  @Test
  public void testFindByShippingCodePageable() throws Exception {
    Page<CategoryShipping> result = this.service.findByShippingCode(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.SHIPPING_CODE_1, this.pageable);
    assertEquals(result, (this.catShipCodePage1));
    verify(this.repository).findByStoreIdAndShippingCodeAndMarkForDeleteFalse(CategoryShippingServiceTest.STORE_ID,
        CategoryShippingServiceTest.SHIPPING_CODE_1, this.pageable);
  }

  @Test
  public void testFindByStoreId() throws Exception {
    List<CategoryShipping> result = this.service.findByStoreId(CategoryShippingServiceTest.STORE_ID);
    assertEquals(result, (this.catShipCodeList));
    verify(this.repository).findByStoreIdAndMarkForDeleteFalse(CategoryShippingServiceTest.STORE_ID);
  }

  @Test
  public void testFindByStoreIdAndId() throws Exception {
    CategoryShipping result =
        this.service.findByStoreIdAndId(CategoryShippingServiceTest.STORE_ID, CategoryShippingServiceTest.ID_A2);
    assertEquals(result, (this.catShipCodeA2));
    verify(this.repository).findByStoreIdAndId(CategoryShippingServiceTest.STORE_ID, CategoryShippingServiceTest.ID_A2);
  }

  @Test
  public void testFindByStoreIdPageable() throws Exception {
    Page<CategoryShipping> result = this.service.findByStoreId(CategoryShippingServiceTest.STORE_ID, this.pageable);
    assertEquals(result, (this.catShipCodePage));
    verify(this.repository).findByStoreIdAndMarkForDeleteFalse(CategoryShippingServiceTest.STORE_ID, this.pageable);
  }

  @Test
  public void testMarkForDeleteCategoryShippingCode() throws Exception {
    this.service.markForDeleteCategoryShipping(CategoryShippingServiceTest.STORE_ID, CategoryShippingServiceTest.ID_A1);
    assertTrue(this.catShipCodeA1.isMarkForDelete());
    verify(this.repository).findByStoreIdAndId(CategoryShippingServiceTest.STORE_ID, CategoryShippingServiceTest.ID_A1);
    verify(this.repository).findById(CategoryShippingServiceTest.ID_A1);
    verify(this.repository).saveAndFlush(this.catShipCodeA1);
  }

  @Test
  public void testMarkForDeleteCategoryShippingCodeList() throws Exception {
    this.catShipCodeListTest = new ArrayList<CategoryShipping>();
    this.catShipCodeListTest.add(this.catShipCodeA1);
    this.catShipCodeListTest.add(this.catShipCodeB2);

    this.service.markForDeleteCategoryShipping(CategoryShippingServiceTest.STORE_ID, this.catShipCodeListTest);
    assertTrue(this.catShipCodeA1.isMarkForDelete());
    assertTrue(this.catShipCodeB2.isMarkForDelete());
    verify(this.repository).findByStoreIdAndId(CategoryShippingServiceTest.STORE_ID, CategoryShippingServiceTest.ID_A1);
    verify(this.repository).findById(CategoryShippingServiceTest.ID_A1);
    verify(this.repository).saveAndFlush(this.catShipCodeA1);
    verify(this.repository).findByStoreIdAndId(CategoryShippingServiceTest.STORE_ID, CategoryShippingServiceTest.ID_B2);
    verify(this.repository).findById(CategoryShippingServiceTest.ID_B2);
    verify(this.repository).saveAndFlush(this.catShipCodeB2);
  }

  @Test
  public void testMarkForDeleteCategoryShippingCodeWithEmptyId() throws Exception {
    try {
      this.service.markForDeleteCategoryShipping(CategoryShippingServiceTest.STORE_ID,
          CategoryShippingServiceTest.BLANK);
    } catch (Exception e) {
      assertTrue((e instanceof ApplicationException)
          && e.getMessage().contains(CategoryShippingServiceTest.ERROR_MESSAGE_DATA_NON_EXISTING));
      verify(this.repository).findByStoreIdAndId(CategoryShippingServiceTest.STORE_ID,
          CategoryShippingServiceTest.BLANK);
    }
  }

  @Test
  public void testSave() throws Exception {
    this.catShipCodeB2.setId(null);
    String result = this.service.save(this.catShipCodeB2);
    assertEquals(result, (this.uuid));
    verify(this.repository).saveAndFlush(this.catShipCodeB2);
  }

  @Test
  public void testSaveWithExistingId() throws Exception {
    try {
      this.service.save(this.catShipCodeB2);
    } catch (Exception e) {
      assertTrue((e instanceof ApplicationRuntimeException)
          && e.getMessage().contains(CategoryShippingServiceTest.ERROR_MESSAGE_DATA_ACCESS));
    }
    verify(this.repository).findById(CategoryShippingServiceTest.ID_B2);
  }

  @Test
  public void testUpdate() throws Exception {
    this.service.update(this.catShipCodeB2);
    verify(this.repository).findById(CategoryShippingServiceTest.ID_B2);
    verify(this.repository).saveAndFlush(this.catShipCodeB2);
  }

  @Test
  public void testUpdateDataNotFound() throws Exception {
    when(repository.findById("x")).thenReturn(Optional.ofNullable(null));
    this.catShipCodeB2.setId("x");
    try {
      this.service.update(this.catShipCodeB2);
    } catch (Exception e) {
      assertTrue((e instanceof ApplicationRuntimeException)
          && e.getMessage().contains(CategoryShippingServiceTest.ERROR_MESSAGE_DATA_NOT_FOUND));
    }
    verify(this.repository).findById("x");

  }

  @Test
  public void testUpdateWithNullId() throws Exception {
    this.catShipCodeB2.setId(null);
    try {
      this.service.update(this.catShipCodeB2);
    } catch (Exception e) {
      assertTrue((e instanceof ApplicationRuntimeException)
          && e.getMessage().contains(CategoryShippingServiceTest.ERROR_MESSAGE_DATA_NOT_FOUND));
    }
  }

  @Test
  public void testDelete() throws Exception {
    this.service.delete(ID_A1);
  }

  @Test
  public void generateShippingWeightTest() throws Exception {
  when(categoryService.findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE_A)).thenReturn(categoryList);
    Double response = service.generateShippingWeight(STORE_ID, CATEGORY_CODE_A, LENGTH, HEIGHT, WEIGHT, WIDTH);
    verify(categoryService).findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE_A);
    assertEquals(10, response, 0);
  }

  @Test
  public void generateShippingWeightRoundOffToThreeDigitsTest() throws Exception {
    when(categoryService.findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE_A)).thenReturn(categoryList);
    Double response = service.generateShippingWeight(STORE_ID, CATEGORY_CODE_A, 1, 1, 0.001, 1);
    verify(categoryService).findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE_A);
    assertEquals(0.001, response, 0);
  }

  @Test
  public void generateShippingWeightNullResponseTest() throws Exception {
    try {
      when(categoryService.findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE_A)).thenReturn(null);
      service.generateShippingWeight(STORE_ID, CATEGORY_CODE_A, LENGTH, HEIGHT, WEIGHT, WIDTH);
    } catch (ApplicationException e) {
      assertEquals(ERROR_MESSAGE_CATEGORY_NOT_FOUND, e.getMessage());
    } finally {
      verify(categoryService).findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE_A);
    }
  }

  @Test
  public void generateShippingWeightNonMasterCatalogTypeTest() throws Exception {
    try {
      categoryList.get(0).getCatalog().setCatalogType(CatalogType.SALES_CATALOG);
      when(categoryService.findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE_A)).thenReturn(categoryList);
      service.generateShippingWeight(STORE_ID, CATEGORY_CODE_A, LENGTH, HEIGHT, WEIGHT, WIDTH);
    } catch (ApplicationException e) {
      assertEquals(ERROR_MESSAGE_NOT_A_MASTER_CATALOG, e.getMessage());
    } finally {
      verify(categoryService).findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE_A);
    }
  }

  @Test
  public void generateShippingWeightEmptyCategoryCodeTest() throws Exception {
    try {
      when(categoryService.findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE_A)).thenReturn(categoryList);
      service.generateShippingWeight(STORE_ID, StringUtils.EMPTY, LENGTH, HEIGHT, WEIGHT, WIDTH);
    } catch (ApplicationRuntimeException e) {
      assertEquals(CATEGORY_CODE_EMPTY, e.getMessage());
    }
  }

  @Test
  public void generateShippingWeightEmptyStoreIdTest() throws Exception {
    try {
      when(categoryService.findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE_A)).thenReturn(categoryList);
      service.generateShippingWeight(StringUtils.EMPTY, CATEGORY_CODE_A, LENGTH, HEIGHT, WEIGHT, WIDTH);
    } catch (ApplicationRuntimeException e) {
      assertEquals(STORE_ID_EMPTY, e.getMessage());
    }
  }
}
