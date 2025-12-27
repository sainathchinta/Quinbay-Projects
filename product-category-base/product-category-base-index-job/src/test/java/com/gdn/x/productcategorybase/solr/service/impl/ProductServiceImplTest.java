package com.gdn.x.productcategorybase.solr.service.impl;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.util.CollectionUtils;

import com.gdn.x.productcategorybase.solr.dao.ProductDao;
import com.gdn.x.productcategorybase.solr.dao.ProductDeltaDao;
import com.gdn.x.productcategorybase.solr.dao.SolrProductDao;
import com.gdn.x.productcategorybase.solr.model.ActionType;
import com.gdn.x.productcategorybase.solr.model.DeltaProduct;
import com.gdn.x.productcategorybase.solr.model.ProductModel;
import com.gdn.x.productcategorybase.solr.model.ReIndexType;


/**
 * Created by Kesha on 02/05/16.
 */
public class ProductServiceImplTest {
  private ProductServiceImpl productService;
  @Mock
  private ProductDeltaDao mockProductDeltaDao;
  @Mock
  private ProductDao mockProductDao;
  @Mock
  private SolrProductDao mockSolrProductDao;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productService = new ProductServiceImpl();
    productService.setProductDao(mockProductDao);
    productService.setProductDeltaDao(mockProductDeltaDao);
    productService.setSolrProductDao(mockSolrProductDao);

  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(mockProductDao);
    verifyNoMoreInteractions(mockProductDeltaDao);
    verifyNoMoreInteractions(mockSolrProductDao);
  }


  @Test
  public void getProductItemIds_ProductsExist_returnNonEmptyList() throws Exception {
    List<ProductModel> productList = new ArrayList<>();
    productList.add(new ProductModel("id1", "name1", "code1", new Date()));
    productList.add(new ProductModel("id2", "name2", "code2", new Date()));
    when(mockProductDao.getAllProducts()).thenReturn(productList);
    List<ProductModel> returnList = productService.getAllProducts();
    assertNotNull(returnList);
    assertFalse(CollectionUtils.isEmpty(returnList));
    verify(mockProductDao).getAllProducts();
  }

  @Test
  public void deltaUpdate_noProducts_noDaoCall() throws Exception {
    when(mockProductDao.getProductAttributes(anySet())).thenReturn(new HashMap());
    when(mockProductDao.getProductCategories(anySet())).thenReturn(new HashMap());
    productService.deltaUpdate(new ArrayList<DeltaProduct>(), new HashMap<String, String>());
  }

  @Test
  public void deltaUpdate_ProductsExists_update() throws Exception {
    when(mockProductDao.getProductAttributes(anySet())).thenReturn(new HashMap());
    when(mockProductDao.getProductCategories(anySet())).thenReturn(new HashMap());
    when(mockProductDao.getProductToUPCCodesMap(anySet())).thenReturn(new HashMap());
    List<DeltaProduct> productList = getDeltaProducts();
    productService.deltaUpdate(productList, new HashMap<String, String>());
    verify(mockProductDao).getProductAttributes(anySet());
    verify(mockProductDao).getProductCategories(anySet());
    verify(mockProductDao).getProductToUPCCodesMap(anySet());
    verify(mockSolrProductDao).deleteDocuments(anyList());
    verify(mockSolrProductDao).commit();
  }

  @Test
  public void deleteProcessedProducts_test() throws Exception {
    productService.deleteProcessedProducts();
    verify(mockProductDeltaDao).deleteProcessedElements();

  }

  @Test
  public void getCategoryToParentMap_childParentMapping() throws Exception {
    Map<String, String> childToParentCategory = new HashMap<>();
    childToParentCategory.put("id1", "id2");
    childToParentCategory.put("id2", "id3");
    childToParentCategory.put("id4", "id3");
    when(mockProductDao.getAllCategoryMapping()).thenReturn(childToParentCategory);
    Map<String, String> catToFinalParentMap = productService.getCategoryToParentMap();
    assertNotNull(catToFinalParentMap);
    assertTrue(catToFinalParentMap.size() != 0);
    assertTrue(catToFinalParentMap.get("id1").equals("id3"));
    assertTrue(catToFinalParentMap.get("id4").equals("id3"));
    assertTrue(catToFinalParentMap.get("id2").equals("id3"));
    verify(mockProductDao).getAllCategoryMapping();
  }

  @Test
  public void updateAllDetlaProductsToProcess() throws Exception {
    productService.updateAllDetlaProductsToProcess();
    verify(mockProductDeltaDao).updateAllElementsToProcessing();
  }

  @Test
  public void getDataAndPostDocumentsToSolr_productsExist_NoDocumentsCreated() throws
      Exception {
    List<ProductModel> productList = new ArrayList<>();
    productList.add(new ProductModel("id1", "name1", "code1", new Date()));
    productList.add(new ProductModel("id2", "name2", "code2", new Date()));
    when(mockProductDao.getProductCategories(anySet())).thenReturn(new HashMap());
    when(mockProductDao.getProductAttributes(anySet())).thenReturn(new HashMap());
    when(mockProductDao.getProductToUPCCodesMap(anySet())).thenReturn(new HashMap());
    productService.getDataAndPostDocumentsToSolr(productList,
        new HashMap<String, String>(), ReIndexType.DELTA);
    verify(mockProductDao).getProductAttributes(anySet());
    verify(mockProductDao).getProductCategories(anySet());
    verify(mockProductDao).getProductToUPCCodesMap(anySet());
  }

  @Test
  public void getDataAndPostDocumentsToSolr_productsExist_DocumentsCreated() throws
      Exception {
    List<ProductModel> productList = new ArrayList<>();
    productList.add(new ProductModel("id1", "name1", "code1", new Date()));
    productList.add(new ProductModel("id2", "name2", "code2", new Date()));
    Map<String, String> mockProductCategoriesMap = new HashMap<>();
    mockProductCategoriesMap.put("id1", "cat1");
    mockProductCategoriesMap.put("id2", "cat2");
    Map<String, String> mockCategoriesToFinal = new HashMap<>();
    mockCategoriesToFinal.put("cat1", "parent1");
    mockCategoriesToFinal.put("cat2", "parent1");
    when(mockProductDao.getProductCategories(anySet())).thenReturn(mockProductCategoriesMap);
    when(mockProductDao.getProductAttributes(anySet())).thenReturn(new HashMap());
    when(mockProductDao.getProductToUPCCodesMap(anySet())).thenReturn(new HashMap());
    productService.getDataAndPostDocumentsToSolr(productList,
        new HashMap<String, String>(), ReIndexType.DELTA);
    verify(mockProductDao).getProductAttributes(anySet());
    verify(mockProductDao).getProductCategories(anySet());
    verify(mockProductDao).getProductToUPCCodesMap(anySet());
    verify(mockSolrProductDao).post(anyList(), any(ReIndexType.class));
    verify(mockSolrProductDao).commit();
  }

  @Test
  public void fetchUpdatedProducts_noProductsExist_emptyList() throws Exception {
    when(mockProductDeltaDao.fetchUpdatedProducts()).thenReturn(new ArrayList<DeltaProduct>());
    Set<DeltaProduct> productList = productService.fetchUpdatedProducts();
    assertNotNull(productList);
    assertTrue(CollectionUtils.isEmpty(productList));
    verify(mockProductDeltaDao).fetchUpdatedProducts();
  }

  @Test
  public void fetchUpdatedProducts_ProductsExist_updateStateToProcessingAndReturn() throws
      Exception {
    List<DeltaProduct> productList = getDeltaProducts();
    when(mockProductDeltaDao.fetchUpdatedProducts()).thenReturn(productList);
    Set<DeltaProduct> returnProductList = productService.fetchUpdatedProducts();
    assertNotNull(productList);
    assertFalse(CollectionUtils.isEmpty(returnProductList));
    verify(mockProductDeltaDao).fetchUpdatedProducts();
    verify(mockProductDeltaDao).updateElementStateToProcessing(anyList());
  }

  private List<DeltaProduct> getDeltaProducts() {
    List<DeltaProduct> productList = new ArrayList<>();
    productList.add(new DeltaProduct("id1", "name1", "code1", ActionType.DELETE, new Date()));
    productList.add(new DeltaProduct("id2", "name2", "code2", ActionType.UPDATE, new Date()));
    return productList;
  }

  @Test
  public void fullIndexTest(){
    when(mockSolrProductDao.fullIndex()).thenReturn(Boolean.TRUE);
    productService.fullIndex();
    verify(mockSolrProductDao).fullIndex();
  }

  @Test
  public void deltaIndexTest(){
    when(mockSolrProductDao.deltaIndex()).thenReturn(Boolean.TRUE);
    productService.deltaIndex();
    verify(mockSolrProductDao).deltaIndex();
  }
}
