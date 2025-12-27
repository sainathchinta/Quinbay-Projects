package com.gdn.x.product.service.impl;

import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryReferenceResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;

public class ObjectConverterCatalogTest {
  private static final String MASTER_REFERENCE_ID = "sales-reference-id";
  private static final String MASTER_REFERENCE_ID_2 = "sales-reference-id_2";
  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";
  private static final String CATEGORY_ID_MASTER_C1 = "category-id-master-c1";
  private static final String CATEGORY_CODE_MASTER_C1 = "category-code-master-c1";
  private static final String CATEGORY_ID_MASTER_C2 = "category-id-master-c2";
  private static final String CATEGORY_CODE_MASTER_C2 = "category-code-master-c2";
  private static final String CATEGORY_ID_MASTER_C3 = "category-id-master-c3";
  private static final String CATEGORY_CODE_MASTER_C3 = "category-code-master-c3";
  private static final String CATEGORY_CODE_SALES_A1_C1 = "category-code-sales-a1-c1";
  private static final String CATEGORY_ID_SALES_A1_C1 = "category-id-sales-a1-c1";
  private static final String CATEGORY_CODE_SALES_A1_C2 = "category-code-sales-a1-c2";
  private static final String CATEGORY_ID_SALES_A1_C2 = "category-id-sales-a1-c2";
  private static final String CATEGORY_CODE_SALES_A2_C1 = "category-code-sales-a2-c1";
  private static final String CATEGORY_ID_SALES_A2_C1 = "category-id-sales-a2-c1";
  private static final String CATEGORY_CODE_SALES_A2_C2 = "category-code-sales-a2-c2";
  private static final String CATEGORY_ID_SALES_A2_C2 = "category-id-sales-a2-c2";
  private static final String CATEGORY_CODE_SALES_B1_C1 = "category-code-sales-b1-c1";
  private static final String CATEGORY_ID_SALES_B1_C1 = "category-id-sales-b1-c1";
  private static final String CATEGORY_CODE_SALES_B1_C2 = "category-code-sales-b1-c2";
  private static final String CATEGORY_ID_SALES_B1_C2 = "category-id-sales-b1-c2";
  private static final String CATEGORY_CODE_SALES_B2_C1 = "category-code-sales-b2-c1";
  private static final String CATEGORY_ID_SALES_B2_C1 = "category-id-sales-b2-c1";
  private static final String CATEGORY_CODE_SALES_B2_C2 = "category-code-sales-b2-c2";
  private static final String CATEGORY_ID_SALES_B2_C2 = "category-id-sales-b2-c2";
  private static final String CATALOG_CODE_MASTER = "10001";
  private static final String CATALOG_CODE_SALES_A = "catalog-code-sales-a";
  private static final String CATALOG_CODE_SALES_B = "catalog-code-sales-b";
  private static final String STORE_ID = "10001";
  private static final String NOT_FOUND = "not-found";

  private CatalogResponse masterCatalog;
  private CategoryResponse categoryMasterC1;
  private ProductCategoryResponse productCategoryMasterC1;
  private CategoryResponse categoryMasterC2;
  private ProductCategoryResponse productCategoryMasterC2;
  private CategoryResponse categoryMasterC3;
  private ProductCategoryResponse productCategoryMasterC3;
  private CatalogResponse salesCatalogA;
  private CategoryResponse categorySalesA1C1;
  private ProductCategoryResponse productCategorySalesA1C1;
  private CategoryResponse categorySalesA1C2;
  private ProductCategoryResponse productCategorySalesA1C2;
  private CategoryResponse categorySalesA2C1;
  private ProductCategoryResponse productCategorySalesA2C1;
  private CategoryResponse categorySalesA2C2;
  private ProductCategoryResponse productCategorySalesA2C2;
  private CatalogResponse salesCatalogB;
  private CategoryResponse categorySalesB1C1;
  private ProductCategoryResponse productCategorySalesB1C1;
  private CategoryResponse categorySalesB1C2;
  private ProductCategoryResponse productCategorySalesB1C2;
  private CategoryResponse categorySalesB2C1;
  private ProductCategoryResponse productCategorySalesB2C1;
  private CategoryResponse categorySalesB2C2;
  private ProductCategoryResponse productCategorySalesB2C2;

  private CategoryDetailResponse categoryDetailResponse;
  private CategoryDetailResponse newCategoryDetailResponse;
  private CategoryDetailResponse umkmCategoryDetailResponse2;
  private List<CategoryReferenceResponse> categoryReferenceResponses;
  private List<CategoryReferenceResponse> categoryReferenceResponseList;

  @InjectMocks
  private ObjectConverterServiceImpl objectConverterServiceImpl;
  private ArrayList<ProductCategoryResponse> productCategoryResponses;
  private ArrayList<ProductCategoryResponse> emptyProductCategoryResponses;

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseClient;

  @Captor
  private ArgumentCaptor<String> stringArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    // master catalog
    this.masterCatalog = new CatalogResponse();
    this.masterCatalog.setCatalogCode(ObjectConverterCatalogTest.CATALOG_CODE_MASTER);
    this.categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse masterResponse =
        new CatalogResponse(ObjectConverterCatalogTest.CATALOG_CODE_MASTER,
            ObjectConverterCatalogTest.CATALOG_CODE_MASTER,
            ObjectConverterCatalogTest.CATALOG_CODE_MASTER);
    this.categoryDetailResponse.setId(ObjectConverterCatalogTest.MASTER_REFERENCE_ID);
    this.categoryDetailResponse.setCatalog(this.masterCatalog);
    this.categoryDetailResponse.setCategoryCode(ObjectConverterCatalogTest.CATEGORY_CODE_MASTER_C1);

    this.categoryReferenceResponses = new ArrayList<CategoryReferenceResponse>();
    CategoryReferenceResponse salesResponse1 = new CategoryReferenceResponse();
    salesResponse1.setSalesCategoryReference(this.categoryDetailResponse);
    this.categoryReferenceResponses.add(salesResponse1);

    this.categoryMasterC1 = new CategoryResponse();
    this.categoryMasterC1.setCategoryCode(ObjectConverterCatalogTest.CATEGORY_CODE_MASTER_C1);
    this.categoryMasterC1.setId(ObjectConverterCatalogTest.CATEGORY_ID_MASTER_C1);
    this.categoryMasterC1.setParentCategoryId(ObjectConverterCatalogTest.CATEGORY_ID_MASTER_C2);
    this.categoryMasterC1.setCatalog(this.masterCatalog);
    this.productCategoryMasterC1 =
        new ProductCategoryResponse(this.categoryMasterC1, ObjectConverterCatalogTest.STORE_ID);

    this.categoryMasterC2 = new CategoryResponse();
    this.categoryMasterC2.setCategoryCode(ObjectConverterCatalogTest.CATEGORY_CODE_MASTER_C2);
    this.categoryMasterC2.setId(ObjectConverterCatalogTest.CATEGORY_ID_MASTER_C2);
    this.categoryMasterC2.setCatalog(this.masterCatalog);
    this.productCategoryMasterC2 =
        new ProductCategoryResponse(this.categoryMasterC2, ObjectConverterCatalogTest.STORE_ID);

    this.categoryMasterC3 = new CategoryResponse();
    this.categoryMasterC3.setCategoryCode(ObjectConverterCatalogTest.CATEGORY_CODE_MASTER_C3);
    this.categoryMasterC3.setId(ObjectConverterCatalogTest.CATEGORY_ID_MASTER_C3);
    this.categoryMasterC3.setParentCategoryId(ObjectConverterCatalogTest.CATEGORY_ID_MASTER_C1);
    this.categoryMasterC3.setCatalog(this.masterCatalog);
    this.productCategoryMasterC3 =
        new ProductCategoryResponse(this.categoryMasterC3, ObjectConverterCatalogTest.STORE_ID);

    // sales catalog A category 1
    this.salesCatalogA = new CatalogResponse();
    this.salesCatalogA.setCatalogCode(ObjectConverterCatalogTest.CATALOG_CODE_SALES_A);

    this.categorySalesA1C1 = new CategoryResponse();
    this.categorySalesA1C1.setCategoryCode(ObjectConverterCatalogTest.CATEGORY_CODE_SALES_A1_C1);
    this.categorySalesA1C1.setId(ObjectConverterCatalogTest.CATEGORY_ID_SALES_A1_C1);
    this.categorySalesA1C1.setCatalog(this.salesCatalogA);
    this.productCategorySalesA1C1 =
        new ProductCategoryResponse(this.categorySalesA1C1, ObjectConverterCatalogTest.STORE_ID);

    this.categorySalesA1C2 = new CategoryResponse();
    this.categorySalesA1C2.setCategoryCode(ObjectConverterCatalogTest.CATEGORY_CODE_SALES_A1_C2);
    this.categorySalesA1C2.setId(ObjectConverterCatalogTest.CATEGORY_ID_SALES_A1_C2);
    this.categorySalesA1C2.setParentCategoryId(ObjectConverterCatalogTest.CATEGORY_ID_SALES_A2_C2);
    this.categorySalesA1C2.setCatalog(this.salesCatalogA);
    this.productCategorySalesA1C2 =
        new ProductCategoryResponse(this.categorySalesA1C2, ObjectConverterCatalogTest.STORE_ID);

    // sales catalog A category 2
    this.categorySalesA2C1 = new CategoryResponse();
    this.categorySalesA2C1.setCategoryCode(ObjectConverterCatalogTest.CATEGORY_CODE_SALES_A2_C1);
    this.categorySalesA2C1.setId(ObjectConverterCatalogTest.CATEGORY_ID_SALES_A2_C1);
    this.categorySalesA2C1.setParentCategoryId(ObjectConverterCatalogTest.CATEGORY_ID_SALES_A1_C1);
    this.categorySalesA2C1.setCatalog(this.salesCatalogA);
    this.productCategorySalesA2C1 =
        new ProductCategoryResponse(this.categorySalesA2C1, ObjectConverterCatalogTest.STORE_ID);

    this.categorySalesA2C2 = new CategoryResponse();
    this.categorySalesA2C2.setCategoryCode(ObjectConverterCatalogTest.CATEGORY_CODE_SALES_A2_C2);
    this.categorySalesA2C2.setId(ObjectConverterCatalogTest.CATEGORY_ID_SALES_A2_C2);
    this.categorySalesA2C2.setParentCategoryId(ObjectConverterCatalogTest.CATEGORY_ID_SALES_A1_C1);
    this.categorySalesA2C2.setCatalog(this.salesCatalogA);
    this.productCategorySalesA2C2 =
        new ProductCategoryResponse(this.categorySalesA2C2, ObjectConverterCatalogTest.STORE_ID);

    // sales catalog B category 1
    this.salesCatalogB = new CatalogResponse();
    this.salesCatalogB.setCatalogCode(ObjectConverterCatalogTest.CATALOG_CODE_SALES_B);

    this.categorySalesB1C1 = new CategoryResponse();
    this.categorySalesB1C1.setCategoryCode(ObjectConverterCatalogTest.CATEGORY_CODE_SALES_B1_C1);
    this.categorySalesB1C1.setId(ObjectConverterCatalogTest.CATEGORY_ID_SALES_B1_C1);
    this.categorySalesB1C1.setParentCategoryId(ObjectConverterCatalogTest.CATEGORY_ID_SALES_B2_C2);
    this.categorySalesB1C1.setCatalog(this.salesCatalogB);
    this.productCategorySalesB1C1 =
        new ProductCategoryResponse(this.categorySalesB1C1, ObjectConverterCatalogTest.STORE_ID);

    this.categorySalesB1C2 = new CategoryResponse();
    this.categorySalesB1C2.setCategoryCode(ObjectConverterCatalogTest.CATEGORY_CODE_SALES_B1_C2);
    this.categorySalesB1C2.setId(ObjectConverterCatalogTest.CATEGORY_ID_SALES_B1_C2);
    this.categorySalesB1C2.setParentCategoryId(ObjectConverterCatalogTest.CATEGORY_ID_SALES_B2_C1);
    this.categorySalesB1C2.setCatalog(this.salesCatalogB);
    this.productCategorySalesB1C2 =
        new ProductCategoryResponse(this.categorySalesB1C2, ObjectConverterCatalogTest.STORE_ID);

    // sales catalog B category 2
    this.categorySalesB2C1 = new CategoryResponse();
    this.categorySalesB2C1.setCategoryCode(ObjectConverterCatalogTest.CATEGORY_CODE_SALES_B2_C1);
    this.categorySalesB2C1.setId(ObjectConverterCatalogTest.CATEGORY_ID_SALES_B2_C1);
    this.categorySalesB2C1.setParentCategoryId(ObjectConverterCatalogTest.CATEGORY_ID_SALES_B2_C2);
    this.categorySalesB2C1.setCatalog(this.salesCatalogB);
    this.productCategorySalesB2C1 =
        new ProductCategoryResponse(this.categorySalesB2C1, ObjectConverterCatalogTest.STORE_ID);

    this.categorySalesB2C2 = new CategoryResponse();
    this.categorySalesB2C2.setCategoryCode(ObjectConverterCatalogTest.CATEGORY_CODE_SALES_B2_C2);
    this.categorySalesB2C2.setId(ObjectConverterCatalogTest.CATEGORY_ID_SALES_B2_C2);
    this.categorySalesB2C2.setCatalog(this.salesCatalogB);
    this.productCategorySalesB2C2 =
        new ProductCategoryResponse(this.categorySalesB2C2, ObjectConverterCatalogTest.STORE_ID);

    this.productCategoryResponses = new ArrayList<ProductCategoryResponse>();
    this.productCategoryResponses.add(this.productCategorySalesB2C2);
    this.productCategoryResponses.add(this.productCategoryMasterC2);
    this.productCategoryResponses.add(this.productCategorySalesA2C2);
    this.productCategoryResponses.add(this.productCategorySalesB1C2);
    this.productCategoryResponses.add(this.productCategorySalesA1C2);
    this.productCategoryResponses.add(this.productCategorySalesA2C1);
    this.productCategoryResponses.add(this.productCategoryMasterC3);
    this.productCategoryResponses.add(this.productCategorySalesB1C1);
    this.productCategoryResponses.add(this.productCategoryMasterC1);
    this.productCategoryResponses.add(this.productCategorySalesB2C1);
    this.productCategoryResponses.add(this.productCategorySalesA1C1);

    this.emptyProductCategoryResponses = new ArrayList<ProductCategoryResponse>();

    umkmCategoryDetailResponse2 = new CategoryDetailResponse();
    umkmCategoryDetailResponse2.setCategoryCode(MASTER_REFERENCE_ID_2);
    umkmCategoryDetailResponse2.setUmkm(true);
    umkmCategoryDetailResponse2.setId(MASTER_REFERENCE_ID_2);
    umkmCategoryDetailResponse2
        .setCatalog(new CatalogResponse(MASTER_REFERENCE_ID_2, CATALOG_CODE_MASTER, MASTER_REFERENCE_ID_2));

    newCategoryDetailResponse = new CategoryDetailResponse();
    newCategoryDetailResponse.setCategoryCode(MASTER_REFERENCE_ID);
    newCategoryDetailResponse.setUmkm(false);
    newCategoryDetailResponse.setId(MASTER_REFERENCE_ID);
    newCategoryDetailResponse
        .setCatalog(new CatalogResponse(MASTER_REFERENCE_ID, CATALOG_CODE_MASTER, MASTER_REFERENCE_ID));

    categoryReferenceResponseList = new ArrayList<>();
    CategoryReferenceResponse salesResponse = new CategoryReferenceResponse();
    salesResponse.setSalesCategoryReference(this.umkmCategoryDetailResponse2);
    categoryReferenceResponseList.add(salesResponse);
    CategoryReferenceResponse salesResponse2 = new CategoryReferenceResponse();
    salesResponse2.setSalesCategoryReference(this.newCategoryDetailResponse);
    categoryReferenceResponseList.add(salesResponse2);
  }

  @AfterEach
  public void tearDown() throws Exception {

  }

  @Test
  public void testConvertToMasterCatalog() {
    MasterCatalog result =
        this.objectConverterServiceImpl.convertToMasterCatalog(this.productCategoryResponses);

    assertEquals(result.getCatalogCode(), ObjectConverterCatalogTest.CATALOG_CODE_MASTER);
    assertEquals(result.getCategory().getCategoryCode(), this.categoryMasterC3.getId());
    assertEquals(result.getCategory().getCatgroupId(), this.categoryMasterC3.getId());
  }

  @Test
  public void testConvertToMasterCatalogFromDirectParentCategory() {
    MasterCatalog result =
        this.objectConverterServiceImpl
            .convertToMasterCatalogFromDirectParentCategory(this.categoryDetailResponse);
    assertNotNull(result);
    assertEquals(ObjectConverterCatalogTest.CATALOG_CODE_MASTER, result.getCatalogCode());
    assertEquals(ObjectConverterCatalogTest.CATEGORY_CODE_MASTER_C1, result.getCategory()
        .getCategoryCode());
    assertEquals(ObjectConverterCatalogTest.CATEGORY_CODE_MASTER_C1, result.getCategory()
        .getCatgroupId());
  }


  @Test
  public void testConvertToMasterCatalogFromDirectParentCategoryWithNullProductCategoryResponse() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.objectConverterServiceImpl.convertToMasterCatalogFromDirectParentCategory(null));
  }

  @Test
  public void testConvertToMasterCatalogWithEmptyProductCategoryResponses() {
    this.objectConverterServiceImpl
        .convertToMasterCatalog(new ArrayList<ProductCategoryResponse>());
  }

  @Test
  public void testConvertToMasterCatalogWithNullProductCategoryResponses() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.objectConverterServiceImpl.convertToMasterCatalog(null));
  }

  @Test
  public void testConvertToSalesCatalogs() {
    this.objectConverterServiceImpl.convertToSalesCatalogs(this.productCategoryResponses);
  }

  @Test
  public void testConvertToSalesCatalogsFromDirectParentCategory() {
    when(
        this.productCategoryBaseClient.getCategoryDetail(ObjectConverterCatalogTest.REQUEST_ID,
            ObjectConverterCatalogTest.USERNAME, ObjectConverterCatalogTest.MASTER_REFERENCE_ID))
        .thenReturn(this.categoryDetailResponse);
    List<SalesCatalog> result =
        this.objectConverterServiceImpl.convertToSalesCatalogsFromDirectParentCategory(
            ObjectConverterCatalogTest.REQUEST_ID, ObjectConverterCatalogTest.USERNAME,
            this.categoryReferenceResponses, new ArrayList<>(), false);
    verify(this.productCategoryBaseClient).getCategoryDetail(ObjectConverterCatalogTest.REQUEST_ID,
        ObjectConverterCatalogTest.USERNAME, ObjectConverterCatalogTest.MASTER_REFERENCE_ID);
    Assertions.assertEquals(1, result.get(0).getListOfCategories().size());
    Assertions.assertEquals(CATEGORY_CODE_MASTER_C1, result.get(0).getListOfCategories().get(0).getCategoryCode());
  }

  @Test
  public void testConvertToSalesCatalogsFromDirectParentCategoryUmkmMerchant() {
    CategoryReferenceResponse salesResponse = new CategoryReferenceResponse();
    salesResponse.setB2bSalesCategoryReference(this.categoryDetailResponse);
    when(
        this.productCategoryBaseClient.getCategoryDetail(ObjectConverterCatalogTest.REQUEST_ID,
            ObjectConverterCatalogTest.USERNAME, ObjectConverterCatalogTest.MASTER_REFERENCE_ID))
        .thenReturn(this.categoryDetailResponse);

    List<SalesCatalog> result =
        this.objectConverterServiceImpl.convertToSalesCatalogsFromDirectParentCategory(
            ObjectConverterCatalogTest.REQUEST_ID, ObjectConverterCatalogTest.USERNAME,
             new ArrayList<>(), Arrays.asList(salesResponse), true);
    verify(this.productCategoryBaseClient).getCategoryDetail(ObjectConverterCatalogTest.REQUEST_ID,
        ObjectConverterCatalogTest.USERNAME, ObjectConverterCatalogTest.MASTER_REFERENCE_ID);
    Assertions.assertEquals(1, result.get(0).getListOfCategories().size());
    Assertions.assertEquals(CATEGORY_CODE_MASTER_C1, result.get(0).getListOfCategories().get(0).getCategoryCode());
  }

  @Test
  public void testConvertToSalesCatalogsFromDirectParentCategoryUmkmMerchantWithUmkmCategory() {
    when(this.productCategoryBaseClient
        .getCategoryDetail(ObjectConverterCatalogTest.REQUEST_ID, ObjectConverterCatalogTest.USERNAME,
            ObjectConverterCatalogTest.MASTER_REFERENCE_ID)).thenReturn(this.newCategoryDetailResponse);
    when(this.productCategoryBaseClient
        .getCategoryDetail(ObjectConverterCatalogTest.REQUEST_ID, ObjectConverterCatalogTest.USERNAME,
            ObjectConverterCatalogTest.MASTER_REFERENCE_ID_2)).thenReturn(this.umkmCategoryDetailResponse2);
    List<SalesCatalog> result = this.objectConverterServiceImpl
        .convertToSalesCatalogsFromDirectParentCategory(ObjectConverterCatalogTest.REQUEST_ID,
            ObjectConverterCatalogTest.USERNAME, this.categoryReferenceResponseList, new ArrayList<>(), true);
    verify(this.productCategoryBaseClient, times(2))
        .getCategoryDetail(eq(ObjectConverterCatalogTest.REQUEST_ID), eq(ObjectConverterCatalogTest.USERNAME),
            stringArgumentCaptor.capture());
    Assertions.assertEquals(2, result.get(0).getListOfCategories().size());
    Assertions.assertEquals(MASTER_REFERENCE_ID_2, result.get(0).getListOfCategories().get(0).getCategoryCode());
    Assertions.assertEquals(MASTER_REFERENCE_ID, result.get(0).getListOfCategories().get(1).getCategoryCode());
    Assertions.assertEquals(MASTER_REFERENCE_ID_2, stringArgumentCaptor.getAllValues().get(0));
    Assertions.assertEquals(MASTER_REFERENCE_ID, stringArgumentCaptor.getAllValues().get(1));
  }

  @Test
  public void testConvertToSalesCatalogsFromDirectParentCategoryNonUmkmMerchantWithUmkmCategory() {
    when(this.productCategoryBaseClient
        .getCategoryDetail(ObjectConverterCatalogTest.REQUEST_ID, ObjectConverterCatalogTest.USERNAME,
            ObjectConverterCatalogTest.MASTER_REFERENCE_ID)).thenReturn(this.newCategoryDetailResponse);
    List<SalesCatalog> result = this.objectConverterServiceImpl
        .convertToSalesCatalogsFromDirectParentCategory(ObjectConverterCatalogTest.REQUEST_ID,
            ObjectConverterCatalogTest.USERNAME, this.categoryReferenceResponseList, new ArrayList<>(), false);
    verify(this.productCategoryBaseClient, times(2))
        .getCategoryDetail(eq(ObjectConverterCatalogTest.REQUEST_ID), eq(ObjectConverterCatalogTest.USERNAME),
            stringArgumentCaptor.capture());
    Assertions.assertEquals(1, result.get(0).getListOfCategories().size());
    Assertions.assertEquals(MASTER_REFERENCE_ID, result.get(0).getListOfCategories().get(0).getCategoryCode());
    Assertions.assertEquals(MASTER_REFERENCE_ID_2, stringArgumentCaptor.getAllValues().get(0));
    Assertions.assertEquals(MASTER_REFERENCE_ID, stringArgumentCaptor.getAllValues().get(1));
  }

  @Test
  public void testConvertToSalesCatalogsWithEmptyProductCategoryResponses() {
    this.objectConverterServiceImpl.convertToSalesCatalogs(this.emptyProductCategoryResponses);
  }

  @Test
  public void testConvertToSalesCatalogsWithNullProductCategoryResponses() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.objectConverterServiceImpl.convertToSalesCatalogs(null));
  }
}
