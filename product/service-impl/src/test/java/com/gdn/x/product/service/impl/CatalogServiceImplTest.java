package com.gdn.x.product.service.impl;

import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

public class CatalogServiceImplTest {

  private static final String CATGROUP_ID_SALES = "catgroup-id-sales";

  private static final String SALES_CATALOG = "12051";

  private static final String CATEGORY_CODE_SALES = "category-code-sales";

  private static final String CATGROUP_ID_MASTER = "catgroup-id-master";

  private static final String CATEGORY_CODE_MASTER = "category-code-master";

  private static final String MASTER_CATALOG = "master-catalog";

  private static final String CATALOG_CODES = "catalogCodes";

  private static final List<List<CategoryResponse>> LIST_OF_CATEGORY_RESPONSE = Arrays
      .asList(Arrays.asList(new CategoryResponse()));

  private static final String USERNAME = "username";

  private static final String REQUEST_ID = "requestId";

  @InjectMocks
  private CatalogServiceImpl catalogServiceImpl;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private ObjectConverterService objectConverterService;

  private List<String> catalogCodes;
  private Product product = new Product();

  @Test
  public void getItemCatalogsWithCategoryHierarchyTest() throws Exception {
    when(
        this.productHelperService.constructListOfCategoriesListOfProduct(
            CatalogServiceImplTest.REQUEST_ID, CatalogServiceImplTest.USERNAME, this.catalogCodes))
        .thenReturn(CatalogServiceImplTest.LIST_OF_CATEGORY_RESPONSE);
    this.catalogServiceImpl.getItemCatalogsWithCategoryHierarchy(CatalogServiceImplTest.USERNAME,
        CatalogServiceImplTest.REQUEST_ID, this.catalogCodes);
    verify(this.productHelperService).constructListOfCategoriesListOfProduct(
        CatalogServiceImplTest.REQUEST_ID, CatalogServiceImplTest.USERNAME, this.catalogCodes);
    verify(this.objectConverterService).convertToListOfItemCatalog(
        CatalogServiceImplTest.LIST_OF_CATEGORY_RESPONSE);
  }

  @Test
  public void getItemCatalogsWithCategoryHierarchyV2Test() throws Exception {
    when(
        this.productHelperService.constructListOfCategoriesListOfProduct(
            CatalogServiceImplTest.REQUEST_ID, CatalogServiceImplTest.USERNAME, this.catalogCodes))
        .thenReturn(CatalogServiceImplTest.LIST_OF_CATEGORY_RESPONSE);
    this.catalogServiceImpl.getItemCatalogsWithCategoryHierarchyV2(CatalogServiceImplTest.USERNAME,
        CatalogServiceImplTest.REQUEST_ID, this.catalogCodes);
    verify(this.productHelperService).constructListOfCategoriesListOfProduct(
        CatalogServiceImplTest.REQUEST_ID, CatalogServiceImplTest.USERNAME, this.catalogCodes);
    verify(this.objectConverterService).convertToListOfItemCatalogV2(
        CatalogServiceImplTest.LIST_OF_CATEGORY_RESPONSE);
  }

  @Test
  public void getItemCatalogsWithCategoryHierarchyTestWithMasterCatalog() throws Exception {
    Product product = new Product();
    product.setMasterCatalog(new MasterCatalog(CatalogServiceImplTest.MASTER_CATALOG, new Category(
        CatalogServiceImplTest.CATEGORY_CODE_MASTER, CatalogServiceImplTest.CATGROUP_ID_MASTER)));
    product.setSalesCatalogs(Arrays.asList(new SalesCatalog(CatalogServiceImplTest.SALES_CATALOG,
        Arrays.asList(new Category(CatalogServiceImplTest.CATEGORY_CODE_SALES,
            CatalogServiceImplTest.CATGROUP_ID_SALES)))));

    this.catalogCodes.clear();
    this.catalogCodes.add(CatalogServiceImplTest.CATEGORY_CODE_MASTER);
    this.catalogCodes.add(CatalogServiceImplTest.CATEGORY_CODE_SALES);
    when(
        this.productHelperService.constructListOfCategoriesListOfProduct(
            CatalogServiceImplTest.REQUEST_ID, CatalogServiceImplTest.USERNAME, this.catalogCodes))
        .thenReturn(CatalogServiceImplTest.LIST_OF_CATEGORY_RESPONSE);

    this.catalogServiceImpl.getItemCatalogsWithCategoryHierarchy(CatalogServiceImplTest.USERNAME,
        CatalogServiceImplTest.REQUEST_ID, product);

    verify(this.productHelperService).constructListOfCategoriesListOfProduct(CatalogServiceImplTest.REQUEST_ID,
        CatalogServiceImplTest.USERNAME,
        Arrays.asList(CatalogServiceImplTest.CATEGORY_CODE_MASTER, CatalogServiceImplTest.CATEGORY_CODE_SALES));
    verify(this.objectConverterService).convertToListOfItemCatalog(CatalogServiceImplTest.LIST_OF_CATEGORY_RESPONSE);
  }


  @Test
  public void getItemCatalogsWithCategoryHierarchyTestWithMasterCatalogTest2() throws Exception {
    Product product = new Product();
    product.setMasterCatalog(new MasterCatalog(CatalogServiceImplTest.MASTER_CATALOG, null));
    product.setSalesCatalogs(Arrays.asList(new SalesCatalog(CatalogServiceImplTest.SALES_CATALOG, Arrays.asList(
        new Category(CatalogServiceImplTest.CATEGORY_CODE_SALES, CatalogServiceImplTest.CATGROUP_ID_SALES)))));

    this.catalogCodes.clear();
    this.catalogCodes.add(CatalogServiceImplTest.CATEGORY_CODE_MASTER);
    this.catalogCodes.add(CatalogServiceImplTest.CATEGORY_CODE_SALES);
    when(this.productHelperService.constructListOfCategoriesListOfProduct(CatalogServiceImplTest.REQUEST_ID,
        CatalogServiceImplTest.USERNAME, this.catalogCodes)).thenReturn(
        CatalogServiceImplTest.LIST_OF_CATEGORY_RESPONSE);

    this.catalogServiceImpl.getItemCatalogsWithCategoryHierarchy(CatalogServiceImplTest.USERNAME,
        CatalogServiceImplTest.REQUEST_ID, product);

    verify(this.productHelperService).constructListOfCategoriesListOfProduct(CatalogServiceImplTest.REQUEST_ID,
        CatalogServiceImplTest.USERNAME, Arrays.asList(CatalogServiceImplTest.CATEGORY_CODE_SALES));
    verify(this.objectConverterService).convertToListOfItemCatalog(new ArrayList<>());
  }

  @Test
  public void getItemCatalogsWithCategoryHierarchyTestWithMasterCatalogTest() throws Exception {
    Product product = new Product();
    product.setSalesCatalogs(Arrays.asList(new SalesCatalog(CatalogServiceImplTest.SALES_CATALOG, Arrays.asList(
        new Category(CatalogServiceImplTest.CATEGORY_CODE_SALES, CatalogServiceImplTest.CATGROUP_ID_SALES)))));

    this.catalogCodes.clear();
    this.catalogCodes.add(CatalogServiceImplTest.CATEGORY_CODE_MASTER);
    this.catalogCodes.add(CatalogServiceImplTest.CATEGORY_CODE_SALES);
    when(this.productHelperService.constructListOfCategoriesListOfProduct(CatalogServiceImplTest.REQUEST_ID,
        CatalogServiceImplTest.USERNAME, this.catalogCodes)).thenReturn(
        CatalogServiceImplTest.LIST_OF_CATEGORY_RESPONSE);

    this.catalogServiceImpl.getItemCatalogsWithCategoryHierarchy(CatalogServiceImplTest.USERNAME,
        CatalogServiceImplTest.REQUEST_ID, product);

    verify(this.productHelperService).constructListOfCategoriesListOfProduct(CatalogServiceImplTest.REQUEST_ID,
        CatalogServiceImplTest.USERNAME, List.of(CatalogServiceImplTest.CATEGORY_CODE_SALES));
    verify(this.objectConverterService).convertToListOfItemCatalog(new ArrayList<>());
  }


  @Test
  public void getItemCatalogsWithCategoryHierarchyTestWithNoMasterCatalog() throws Exception {
    Product product = new Product();
    product.setMasterDataProduct(new MasterDataProduct());
    product.getMasterDataProduct()
        .setMasterCatalog(
            new MasterCatalog(CatalogServiceImplTest.MASTER_CATALOG, new Category(
                CatalogServiceImplTest.CATEGORY_CODE_MASTER,
                CatalogServiceImplTest.CATGROUP_ID_MASTER)));
    product.setSalesCatalogs(Arrays.asList(new SalesCatalog(CatalogServiceImplTest.SALES_CATALOG,
        Arrays.asList(new Category(CatalogServiceImplTest.CATEGORY_CODE_SALES,
            CatalogServiceImplTest.CATGROUP_ID_SALES)))));

    this.catalogCodes.clear();
    this.catalogCodes.add(CatalogServiceImplTest.CATEGORY_CODE_MASTER);
    this.catalogCodes.add(CatalogServiceImplTest.CATEGORY_CODE_SALES);
    when(
        this.productHelperService.constructListOfCategoriesListOfProduct(
            CatalogServiceImplTest.REQUEST_ID, CatalogServiceImplTest.USERNAME, this.catalogCodes))
        .thenReturn(CatalogServiceImplTest.LIST_OF_CATEGORY_RESPONSE);

    this.catalogServiceImpl.getItemCatalogsWithCategoryHierarchy(CatalogServiceImplTest.USERNAME,
        CatalogServiceImplTest.REQUEST_ID, product);

    verify(this.productHelperService).constructListOfCategoriesListOfProduct(CatalogServiceImplTest.REQUEST_ID,
        CatalogServiceImplTest.USERNAME, this.catalogCodes);
    verify(this.objectConverterService).convertToListOfItemCatalog(CatalogServiceImplTest.LIST_OF_CATEGORY_RESPONSE);
  }

  @Test
  public void getItemCatalogsWithCategoryHierarchyExistsInPCBTest() throws Exception {
    when(this.productHelperService
        .getCategoryResponseListByCategoryCodesForProducts(CatalogServiceImplTest.REQUEST_ID,
            CatalogServiceImplTest.USERNAME, this.catalogCodes))
        .thenReturn(CatalogServiceImplTest.LIST_OF_CATEGORY_RESPONSE);
    this.catalogServiceImpl
        .getItemCatalogsWithCategoryHierarchyExistsInPCB(CatalogServiceImplTest.USERNAME,
            CatalogServiceImplTest.REQUEST_ID, this.catalogCodes);
    verify(this.productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(CatalogServiceImplTest.REQUEST_ID,
            CatalogServiceImplTest.USERNAME, this.catalogCodes);
    verify(this.objectConverterService)
        .convertToListOfItemCatalog(CatalogServiceImplTest.LIST_OF_CATEGORY_RESPONSE);
  }

  @Test
  public void getCategoryCodeToItemCatalogsMapTest() {
    Map<String, List<CategoryResponse>> categoryCodeAndHierarchyMap =
        Collections.singletonMap(CATALOG_CODES, LIST_OF_CATEGORY_RESPONSE.get(0));
    Mockito.when(this.productHelperService.constructMapOfCategoriesAndCategoryCode(REQUEST_ID, USERNAME, catalogCodes))
        .thenReturn(categoryCodeAndHierarchyMap);
    Mockito.when(this.objectConverterService.convertToListOfItemCatalog(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Map<String, List<ItemCatalogVO>> response =
        catalogServiceImpl.getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, catalogCodes);
    Mockito.verify(this.productHelperService)
        .constructMapOfCategoriesAndCategoryCode(REQUEST_ID, USERNAME, catalogCodes);
    Mockito.verify(this.objectConverterService).convertToListOfItemCatalog(Mockito.anyList());
  }

  @Test
  public void getItemCatalogsByProductTest() throws Exception {
    product.setSynchronized(true);
    product.setCategoryCode(CATEGORY_CODE_MASTER);
    Map<String, List<CategoryResponse>> categoryCodeAndHierarchyMap =
        Collections.singletonMap(CATALOG_CODES, LIST_OF_CATEGORY_RESPONSE.get(0));
    Mockito.when(this.productHelperService.constructListOfCategoriesListOfProduct(eq(REQUEST_ID), eq(USERNAME), anyList()))
        .thenReturn(LIST_OF_CATEGORY_RESPONSE);
    Mockito.when(this.objectConverterService.convertToListOfItemCatalog(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    List<ItemCatalogVO> response = catalogServiceImpl.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    Mockito.verify(this.productHelperService)
        .constructListOfCategoriesListOfProduct(eq(REQUEST_ID), eq(USERNAME), anyList());
    Mockito.verify(this.objectConverterService).convertToListOfItemCatalog(Mockito.anyList());
  }

  @Test
  public void getItemCatalogsByProductCategoryCodeNullTest() throws Exception {
    product.setSynchronized(true);
    product.setCategoryCode(null);
    Map<String, List<CategoryResponse>> categoryCodeAndHierarchyMap =
        Collections.singletonMap(CATALOG_CODES, LIST_OF_CATEGORY_RESPONSE.get(0));
    Mockito.when(this.productHelperService.constructListOfCategoriesListOfProduct(eq(REQUEST_ID), eq(USERNAME), anyList()))
        .thenReturn(LIST_OF_CATEGORY_RESPONSE);
    Mockito.when(this.objectConverterService.convertToListOfItemCatalog(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    List<ItemCatalogVO> response = catalogServiceImpl.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    Mockito.verify(this.productHelperService)
        .constructListOfCategoriesListOfProduct(eq(REQUEST_ID), eq(USERNAME), anyList());
    Mockito.verify(this.objectConverterService).convertToListOfItemCatalog(Mockito.anyList());
  }

  @Test
  public void getItemCatalogsByProductUnSyncMasterDataNullTest() throws Exception {
    product.setCategoryCode(null);
    product.setMasterDataProduct(null);
    Map<String, List<CategoryResponse>> categoryCodeAndHierarchyMap =
        Collections.singletonMap(CATALOG_CODES, LIST_OF_CATEGORY_RESPONSE.get(0));
    Mockito.when(this.productHelperService.constructListOfCategoriesListOfProduct(eq(REQUEST_ID), eq(USERNAME), anyList()))
        .thenReturn(LIST_OF_CATEGORY_RESPONSE);
    Mockito.when(this.objectConverterService.convertToListOfItemCatalog(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    List<ItemCatalogVO> response = catalogServiceImpl.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    Mockito.verify(this.productHelperService)
        .constructListOfCategoriesListOfProduct(eq(REQUEST_ID), eq(USERNAME), anyList());
    Mockito.verify(this.objectConverterService).convertToListOfItemCatalog(Mockito.anyList());
  }

  @Test
  public void getItemCatalogsByProductUnSyncMasterCatalogNullTest() throws Exception {
    product.setCategoryCode(null);
    product.setMasterDataProduct(new MasterDataProduct());
    product.getMasterDataProduct().setMasterCatalog(null);
    Map<String, List<CategoryResponse>> categoryCodeAndHierarchyMap =
        Collections.singletonMap(CATALOG_CODES, LIST_OF_CATEGORY_RESPONSE.get(0));
    Mockito.when(this.productHelperService.constructListOfCategoriesListOfProduct(eq(REQUEST_ID), eq(USERNAME), anyList()))
        .thenReturn(LIST_OF_CATEGORY_RESPONSE);
    Mockito.when(this.objectConverterService.convertToListOfItemCatalog(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    List<ItemCatalogVO> response = catalogServiceImpl.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    Mockito.verify(this.productHelperService)
        .constructListOfCategoriesListOfProduct(eq(REQUEST_ID), eq(USERNAME), anyList());
    Mockito.verify(this.objectConverterService).convertToListOfItemCatalog(Mockito.anyList());
  }

  @Test
  public void getItemCatalogsByProductUnSyncCategoryNullTest() throws Exception {
    product.setCategoryCode(null);
    product.setMasterDataProduct(new MasterDataProduct());
    product.getMasterDataProduct().setMasterCatalog(new MasterCatalog());
    product.getMasterDataProduct().getMasterCatalog().setCategory(null);
    Map<String, List<CategoryResponse>> categoryCodeAndHierarchyMap =
        Collections.singletonMap(CATALOG_CODES, LIST_OF_CATEGORY_RESPONSE.get(0));
    Mockito.when(this.productHelperService.constructListOfCategoriesListOfProduct(eq(REQUEST_ID), eq(USERNAME), anyList()))
        .thenReturn(LIST_OF_CATEGORY_RESPONSE);
    Mockito.when(this.objectConverterService.convertToListOfItemCatalog(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    List<ItemCatalogVO> response = catalogServiceImpl.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    Mockito.verify(this.productHelperService)
        .constructListOfCategoriesListOfProduct(eq(REQUEST_ID), eq(USERNAME), anyList());
    Mockito.verify(this.objectConverterService).convertToListOfItemCatalog(Mockito.anyList());
  }

  @Test
  public void getItemCatalogsByProductUnSyncCategoryCodeNullTest() throws Exception {
    product.setCategoryCode(null);
    product.setMasterDataProduct(new MasterDataProduct());
    product.getMasterDataProduct().setMasterCatalog(new MasterCatalog());
    product.getMasterDataProduct().getMasterCatalog().setCategory(new Category());
    product.getMasterDataProduct().getMasterCatalog().getCategory().setCategoryCode(null);
    Map<String, List<CategoryResponse>> categoryCodeAndHierarchyMap =
        Collections.singletonMap(CATALOG_CODES, LIST_OF_CATEGORY_RESPONSE.get(0));
    Mockito.when(this.productHelperService.constructListOfCategoriesListOfProduct(eq(REQUEST_ID), eq(USERNAME), anyList()))
        .thenReturn(LIST_OF_CATEGORY_RESPONSE);
    Mockito.when(this.objectConverterService.convertToListOfItemCatalog(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    List<ItemCatalogVO> response = catalogServiceImpl.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    Mockito.verify(this.productHelperService)
        .constructListOfCategoriesListOfProduct(eq(REQUEST_ID), eq(USERNAME), anyList());
    Mockito.verify(this.objectConverterService).convertToListOfItemCatalog(Mockito.anyList());
  }

  @Test
  public void getItemCatalogsByProductUnSyncTest() throws Exception {
    product.setCategoryCode(null);
    product.setMasterDataProduct(new MasterDataProduct());
    product.getMasterDataProduct().setMasterCatalog(new MasterCatalog());
    product.getMasterDataProduct().getMasterCatalog().setCategory(new Category());
    product.getMasterDataProduct().getMasterCatalog().getCategory().setCategoryCode(CATEGORY_CODE_MASTER);
    Map<String, List<CategoryResponse>> categoryCodeAndHierarchyMap =
        Collections.singletonMap(CATALOG_CODES, LIST_OF_CATEGORY_RESPONSE.get(0));
    Mockito.when(this.productHelperService.constructListOfCategoriesListOfProduct(eq(REQUEST_ID), eq(USERNAME), anyList()))
        .thenReturn(LIST_OF_CATEGORY_RESPONSE);
    Mockito.when(this.objectConverterService.convertToListOfItemCatalog(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    List<ItemCatalogVO> response = catalogServiceImpl.getItemCatalogsByProduct(USERNAME, REQUEST_ID, product);
    Mockito.verify(this.productHelperService)
        .constructListOfCategoriesListOfProduct(eq(REQUEST_ID), eq(USERNAME), anyList());
    Mockito.verify(this.objectConverterService).convertToListOfItemCatalog(Mockito.anyList());
  }


  @BeforeEach
  public void init() {
    openMocks(this);
    this.catalogCodes = new ArrayList<String>();
    this.catalogCodes.add(CatalogServiceImplTest.CATALOG_CODES);
    product.setCategoryCode(CATEGORY_CODE_MASTER);
    product.setSalesCatalogs(Arrays.asList(new SalesCatalog(SALES_CATALOG,
        Arrays.asList(new Category(CATEGORY_CODE_SALES, CATEGORY_CODE_SALES)))));
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.productHelperService);
    verifyNoMoreInteractions(this.objectConverterService);
  }

}
