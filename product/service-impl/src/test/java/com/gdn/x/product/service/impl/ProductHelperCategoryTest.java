package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.outbound.api.feign.PCBFeign;
import com.gdn.x.product.service.api.CachedService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;


public class ProductHelperCategoryTest {
  private static final String MASTER_CATEGORY_CODE = "category-code";

  private static final String MASTER_CATGROUP_ID = "catgroup-id";

  private static final String MASTER_CATALOG_CODE = "catalog-code";

  private static final String SALES1_CATEGORY1_CODE = "sales-category1-code";

  private static final String SALES1_CATEGORY1_CATGROUP_ID = "sales-category1-catgroup-id";

  private static final String SALES1_CATALOG_CODE = "12051";

  private static final String SALES2_CATEGORY1_CODE = "sales2-category1-code";

  private static final String SALES2_CATEGORY1_CATGROUP_ID = "sales2-category1-catgroup-id";

  private static final String SALES2_CATEGORY2_CODE = "sales2-category2-code";

  private static final String SALES2_CATEGORY2_CATGROUP_ID = "sales2-category2-catgroup-id";

  private static final String SALES2_CATALOG_CODE = "12051";

  private static final long PAGE = 0;

  private static final long SIZE = 1;

  private static final String REQUEST_ID = "request-id";

  private static final String ITEM_CODE = "item-code";

  private static final String USERNAME = "username";
  private static final String CATEGORY_CODE = "category-code";

  @InjectMocks
  private ProductHelperServiceImpl productHelperServiceImpl;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private CachedService cachedService;

  @Mock
  private ProductService productService;

  private Category masterCategory;

  private MasterCatalog masterCatalog;

  private SalesCatalog sales1Catalog;

  private Category sales1Category1;

  private ArrayList<Category> sales1ListOfCategories;

  private Category sales2Category1;

  private SalesCatalog sales2Catalog;

  private Category sales2Category2;

  private ArrayList<Category> sales2ListOfCategories;

  private ArrayList<SalesCatalog> salesCatalogs;

  private Product product;

  private CategoryResponse masterCategoryResponseC3;

  private CategoryResponse masterCategoryResponseC2;

  private CategoryResponse masterCategoryResponseC1;

  private ArrayList<CategoryResponse> masterParentCategoriesResponse;

  private CategoryResponse sales1Category1ResponseC3;

  private CategoryResponse sales1Category1ResponseC2;

  private CategoryResponse sales1Category1ResponseC1;

  private ArrayList<CategoryResponse> sales1ParentCategories1Response;

  private CategoryResponse sales2Category1ResponseC2;

  private CategoryResponse sales2Category1ResponseC1;

  private ArrayList<CategoryResponse> sales2ParentCategories1Response;

  private CategoryResponse sales2Category2ResponseC3;

  private CategoryResponse sales2Category2ResponseC2;

  private CategoryResponse sales2Category2ResponseC1;

  private ArrayList<CategoryResponse> sales2ParentCategories2Response;

  private GdnRestListRequest listRequest;

  private Item item;

  private ArrayList<String> categoryCodes;

  @Test
  public void constructListOfCategoriesListOfProductTest() throws Exception {
    List<List<CategoryResponse>> result =
        this.productHelperServiceImpl.constructListOfCategoriesListOfProduct(
            ProductHelperCategoryTest.REQUEST_ID, ProductHelperCategoryTest.USERNAME,
            this.categoryCodes);

    verify(this.cachedService).getParentCategoriesFromMasterData(
        ProductHelperCategoryTest.REQUEST_ID, ProductHelperCategoryTest.USERNAME,
        this.product.getMasterCatalog().getCategory().getCategoryCode());

    for (SalesCatalog salesCatalog : this.product.getSalesCatalogs()) {
      for (Category category : salesCatalog.getListOfCategories()) {
        verify(this.cachedService).getParentCategoriesFromMasterData(
            ProductHelperCategoryTest.REQUEST_ID, ProductHelperCategoryTest.USERNAME,
            category.getCategoryCode());
      }
    }

    assertNotNull(result);
    assertEquals(result.size(), 4);

    List<List<CategoryResponse>> listOfListCategoriesResponse =
        new ArrayList<List<CategoryResponse>>();
    listOfListCategoriesResponse.add(this.masterParentCategoriesResponse);
    listOfListCategoriesResponse.add(this.sales1ParentCategories1Response);
    listOfListCategoriesResponse.add(this.sales2ParentCategories1Response);
    listOfListCategoriesResponse.add(this.sales2ParentCategories2Response);

    for (int i = 0; i < result.size(); i++) {
      assertNotNull(result.get(i));
      assertEquals(result.get(i).size(), listOfListCategoriesResponse.get(i).size());
      for (int j = 0; j < result.get(i).size(); j++) {
        assertEquals(result.get(i).get(j), listOfListCategoriesResponse.get(i).get(j));
      }
    }
  }

  @Test
  public void constructListOfCategoriesListOfProductWithCacheAndDbTest() throws Exception {
    ReflectionTestUtils.setField(this.productHelperServiceImpl, "categoryHierarchyCacheSwitchEnabled", true);
    Set<String> categoryCodes = this.categoryCodes.stream().collect(Collectors.toSet());
    Map<String, List<CategoryResponse>> categoryHierarchyMap = new HashMap<>();
    categoryHierarchyMap.put(MASTER_CATEGORY_CODE, new ArrayList<>());
    Mockito.when(this.cachedService.getParentCategoriesFromDbAndCache(REQUEST_ID, USERNAME, categoryCodes))
        .thenReturn(categoryHierarchyMap);
    List<List<CategoryResponse>> result =
        this.productHelperServiceImpl.constructListOfCategoriesListOfProduct(ProductHelperCategoryTest.REQUEST_ID,
            ProductHelperCategoryTest.USERNAME, this.categoryCodes);

    verify(this.cachedService).getParentCategoriesFromDbAndCache(ProductHelperCategoryTest.REQUEST_ID,
        ProductHelperCategoryTest.USERNAME, categoryCodes);
  }

  @Test
  public void constructListOfCategoriesListOfProductTestWithNullProduct() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl.constructListOfCategoriesListOfProduct(
        ProductHelperCategoryTest.REQUEST_ID, ProductHelperCategoryTest.USERNAME, null));
  }

  @Test
  public void modifyItemNamesTest() {
    String oldProductName = "product name";
    String oldItemName = "product name attribute";
    String newProductName = "product change";
    Item item = new Item();
    item.setMasterDataItem(new MasterDataItem());
    item.getMasterDataItem().setGeneratedItemName(oldItemName);
    List<Item> modifyItemNames =
        this.productHelperServiceImpl.modifyItemNames(Arrays.asList(item), oldProductName,
            newProductName);
    assertEquals(modifyItemNames.get(0).getMasterDataItem().getGeneratedItemName(),
        "product change attribute");
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.categoryCodes = new ArrayList<String>();
    this.categoryCodes.add(ProductHelperCategoryTest.MASTER_CATEGORY_CODE);
    this.categoryCodes.add(ProductHelperCategoryTest.SALES1_CATEGORY1_CODE);
    this.categoryCodes.add(ProductHelperCategoryTest.SALES2_CATEGORY1_CODE);
    this.categoryCodes.add(ProductHelperCategoryTest.SALES2_CATEGORY2_CODE);

    this.masterCategory = new Category();
    this.masterCategory.setCategoryCode(ProductHelperCategoryTest.MASTER_CATEGORY_CODE);
    this.masterCategory.setCatgroupId(ProductHelperCategoryTest.MASTER_CATGROUP_ID);

    this.masterCatalog = new MasterCatalog();
    this.masterCatalog.setCatalogCode(ProductHelperCategoryTest.MASTER_CATALOG_CODE);
    this.masterCatalog.setCategory(this.masterCategory);

    this.sales1Category1 = new Category();
    this.sales1Category1.setCategoryCode(ProductHelperCategoryTest.SALES1_CATEGORY1_CODE);
    this.sales1Category1.setCatgroupId(ProductHelperCategoryTest.SALES1_CATEGORY1_CATGROUP_ID);

    this.sales1ListOfCategories = new ArrayList<Category>();
    this.sales1ListOfCategories.add(this.sales1Category1);

    this.sales1Catalog = new SalesCatalog();
    this.sales1Catalog.setCatalogCode(ProductHelperCategoryTest.SALES1_CATALOG_CODE);
    this.sales1Catalog.setListOfCategories(this.sales1ListOfCategories);

    this.sales2Category1 = new Category();
    this.sales2Category1.setCategoryCode(ProductHelperCategoryTest.SALES2_CATEGORY1_CODE);
    this.sales2Category1.setCatgroupId(ProductHelperCategoryTest.SALES2_CATEGORY1_CATGROUP_ID);

    this.sales2Category2 = new Category();
    this.sales2Category2.setCategoryCode(ProductHelperCategoryTest.SALES2_CATEGORY2_CODE);
    this.sales2Category2.setCatgroupId(ProductHelperCategoryTest.SALES2_CATEGORY2_CATGROUP_ID);

    this.sales2ListOfCategories = new ArrayList<Category>();
    this.sales2ListOfCategories.add(this.sales2Category1);
    this.sales2ListOfCategories.add(this.sales2Category2);

    this.sales2Catalog = new SalesCatalog();
    this.sales2Catalog.setCatalogCode(ProductHelperCategoryTest.SALES2_CATALOG_CODE);
    this.sales2Catalog.setListOfCategories(this.sales2ListOfCategories);

    this.salesCatalogs = new ArrayList<SalesCatalog>();
    this.salesCatalogs.add(this.sales1Catalog);
    this.salesCatalogs.add(this.sales2Catalog);

    this.product = new Product();
    this.product.setMasterCatalog(this.masterCatalog);
    this.product.setSalesCatalogs(this.salesCatalogs);

    this.masterCategoryResponseC3 = new CategoryResponse();
    this.masterCategoryResponseC2 = new CategoryResponse();
    this.masterCategoryResponseC1 = new CategoryResponse();

    this.masterParentCategoriesResponse = new ArrayList<CategoryResponse>();
    this.masterParentCategoriesResponse.add(this.masterCategoryResponseC3);
    this.masterParentCategoriesResponse.add(this.masterCategoryResponseC2);
    this.masterParentCategoriesResponse.add(this.masterCategoryResponseC1);

    this.listRequest =
        new GdnRestListRequest(ProductHelperCategoryTest.PAGE, ProductHelperCategoryTest.SIZE);
    this.listRequest.setRequestId(ProductHelperCategoryTest.REQUEST_ID);

    when(
        this.cachedService.getParentCategoriesFromMasterData(ProductHelperCategoryTest.REQUEST_ID,
            ProductHelperCategoryTest.USERNAME, this.product.getMasterCatalog().getCategory()
                .getCategoryCode())).thenReturn(this.masterParentCategoriesResponse);

    this.sales1Category1ResponseC3 = new CategoryResponse();
    this.sales1Category1ResponseC2 = new CategoryResponse();
    this.sales1Category1ResponseC1 = new CategoryResponse();

    this.sales1ParentCategories1Response = new ArrayList<CategoryResponse>();
    this.sales1ParentCategories1Response.add(this.sales1Category1ResponseC3);
    this.sales1ParentCategories1Response.add(this.sales1Category1ResponseC2);
    this.sales1ParentCategories1Response.add(this.sales1Category1ResponseC1);

    when(
        this.cachedService.getParentCategoriesFromMasterData(ProductHelperCategoryTest.REQUEST_ID,
            ProductHelperCategoryTest.USERNAME, this.sales1Category1.getCategoryCode()))
        .thenReturn(this.sales1ParentCategories1Response);

    this.sales2Category1ResponseC2 = new CategoryResponse();
    this.sales2Category1ResponseC1 = new CategoryResponse();

    this.sales2ParentCategories1Response = new ArrayList<CategoryResponse>();
    this.sales2ParentCategories1Response.add(this.sales2Category1ResponseC2);
    this.sales2ParentCategories1Response.add(this.sales2Category1ResponseC1);

    when(
        this.cachedService.getParentCategoriesFromMasterData(ProductHelperCategoryTest.REQUEST_ID,
            ProductHelperCategoryTest.USERNAME, this.sales2Category1.getCategoryCode()))
        .thenReturn(this.sales2ParentCategories1Response);

    this.sales2Category2ResponseC3 = new CategoryResponse();
    this.sales2Category2ResponseC2 = new CategoryResponse();
    this.sales2Category2ResponseC1 = new CategoryResponse();

    this.sales2ParentCategories2Response = new ArrayList<CategoryResponse>();
    this.sales2ParentCategories2Response.add(this.sales2Category2ResponseC3);
    this.sales2ParentCategories2Response.add(this.sales2Category2ResponseC2);
    this.sales2ParentCategories2Response.add(this.sales2Category2ResponseC1);

    when(
        this.cachedService.getParentCategoriesFromMasterData(ProductHelperCategoryTest.REQUEST_ID,
            ProductHelperCategoryTest.USERNAME, this.sales2Category2.getCategoryCode()))
        .thenReturn(this.sales2ParentCategories2Response);

    this.item = new Item();
    this.item.setItemCode(ProductHelperCategoryTest.ITEM_CODE);
  }

  @Test
  public void constructMapOfCategoriesAndCategoryCodeTest() {
    Mockito.when(this.cachedService.getParentCategoriesFromMasterData(REQUEST_ID, USERNAME, MASTER_CATEGORY_CODE))
        .thenReturn(Arrays.asList(masterCategoryResponseC1));
    Map<String, List<CategoryResponse>> response =
        productHelperServiceImpl.constructMapOfCategoriesAndCategoryCode(REQUEST_ID, USERNAME, Arrays.asList(MASTER_CATEGORY_CODE));
    Mockito.verify(this.cachedService).getParentCategoriesFromMasterData(REQUEST_ID, USERNAME, MASTER_CATEGORY_CODE);
    assertNotNull(response.get(MASTER_CATEGORY_CODE));
  }

  @Test
  public void constructMapOfCategoriesAndCategoryCodeWithDbAndCacheTest() {
    ReflectionTestUtils.setField(this.productHelperServiceImpl, "categoryHierarchyCacheSwitchEnabled", true);
    Set<String> categoryCodes = new HashSet<>();
    categoryCodes.add(MASTER_CATEGORY_CODE);
    Map<String, List<CategoryResponse>> response =
            productHelperServiceImpl.constructMapOfCategoriesAndCategoryCode(REQUEST_ID, USERNAME, Arrays.asList(MASTER_CATEGORY_CODE));
    verify(this.cachedService).getParentCategoriesFromDbAndCache(
            ProductHelperCategoryTest.REQUEST_ID, ProductHelperCategoryTest.USERNAME,
            categoryCodes);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.cachedService);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.objectConverterService);
    verifyNoMoreInteractions(this.pcbFeign);
  }

}
