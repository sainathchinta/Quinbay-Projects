package com.gdn.partners.pcu.master.service.impl;


import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.master.client.feign.PCBFeign;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.request.CategoryHierarchyServiceRequest;
import com.gdn.partners.pcu.master.model.request.GetSubCategoriesServiceRequest;
import com.gdn.partners.pcu.master.service.impl.exception.ClientException;
import com.gdn.partners.pcu.master.web.model.response.CatalogDetailResponse;
import com.gdn.partners.pcu.master.web.model.response.CategorySearchWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryWebResponse;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import org.mockito.MockitoAnnotations;

public class CatalogServiceImplTest {

  private static final String CATALOG_ID = "catalog_id";
  private static final String PARENT_ID = "parent_id";
  private static final Integer PAGE = 0;
  private static final Integer SIZE = Integer.MAX_VALUE;
  private static final Integer TOTAL_RECORDS = Integer.MAX_VALUE;
  private static final String REQUEST_ID = "request_id";
  private static final String NAME = "name";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String CATEGORY_CODE = "CC-";
  private static final String CATEGORY_CODE_1 = "CC_1";
  private static final String CATEGORY_ID = "categoryId";
  private static final String CATEGORY_ID2 = "categoryId2";
  private static final String FILTER_TYPE_ALL = "ALL";
  private static final String FILTER_TYPE_ACTIVE = "ACTIVE";
  private static final String FILTER_TYPE_INACTIVE = "INACTIVE";
  private static final String DOCUMENT_FILTER_TYPE = "ALL";
  private static final String DOCUMENT_FILTER_TYPE_HAVE_DOCUMENT = "Have document";
  private static final String DOCUMENTS = "Doctor's prescription, Passport, Driving License";
  private List<CategoryResponse> categoryResponseList;
  private CategoryResponse categoryResponse;
  private CatalogResponse catalogResponse;
  private GdnRestListResponse<CategoryDTO> categoryDTOGdnRestListResponse;
  private List<CategoryDTO> categoryDTOS;
  private CategoryDTO categoryDTO;
  private PageMetaData pageMetaData;
  private GdnRestListResponse<CatalogResponse> catalogResponseGdnRestListResponse;
  private List<CatalogResponse> catalogResponses;

  @InjectMocks
  private CatalogServiceImpl catalogService;

  @Mock
  private PCBFeign pcbFeign;

  @BeforeEach
  void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    categoryDTOS = new ArrayList<>();
    categoryDTO = new CategoryDTO();
    categoryDTO.setParentCategoryId(PARENT_ID);
    categoryDTO.setSequence(PAGE);
    categoryDTO.setName(NAME);
    catalogResponse = new CatalogResponse();
    catalogResponse.setId(CATALOG_ID);
    categoryDTO.setCatalog(catalogResponse);
    categoryDTO.setGenericTemplateEligible(true);
    categoryDTO.setDocumentType(DOCUMENTS);
    categoryDTOS.add(categoryDTO);
    pageMetaData = new PageMetaData(PAGE.longValue(), SIZE.longValue(), TOTAL_RECORDS.longValue());
    categoryDTOGdnRestListResponse = new GdnRestListResponse<>(categoryDTOS, pageMetaData, REQUEST_ID);
    catalogResponses = new ArrayList<>();
    catalogResponses.add(catalogResponse);
    catalogResponseGdnRestListResponse = new GdnRestListResponse<>(catalogResponses, pageMetaData, REQUEST_ID);
    categoryResponse = new CategoryResponse();
    categoryResponse.setId(CATEGORY_ID);
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    categoryResponse.setParentCategoryId(CATEGORY_ID);
    categoryResponse.setChildCount(14);
    catalogResponse = new CatalogResponse();
    catalogResponse.setId(CATALOG_ID);
    categoryResponse.setCatalog(catalogResponse);
    categoryResponseList = new ArrayList<>();
    categoryResponseList.add(categoryResponse);
  }

  @AfterEach
  void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pcbFeign);
  }

  @Test
  void findCategorySummaryByCatalogIdAndParentIdWithChildrenCountForMasterCatalogTest() throws Exception {
    GetSubCategoriesServiceRequest request =
        new GetSubCategoriesServiceRequest(CatalogType.MASTER_CATALOG.name(), CATALOG_ID, PARENT_ID, Boolean.TRUE,
            Constants.ALL, Constants.ALL, true, false);
    Mockito.when(
        this.pcbFeign.getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, PARENT_ID, PAGE, SIZE, FILTER_TYPE_ALL,
            DOCUMENT_FILTER_TYPE, true, false)).thenReturn(categoryDTOGdnRestListResponse);
    List<CategoryWebResponse> categoryWebResponses =
        catalogService.getSubCategoriesByCatalogIdAndParentCategoryId(request);
    Mockito.verify(this.pcbFeign)
        .getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, PARENT_ID, PAGE, SIZE, FILTER_TYPE_ALL,
            DOCUMENT_FILTER_TYPE, true, false);
    Assertions.assertTrue(categoryWebResponses.get(0).getParentCategoryId().equals(PARENT_ID));
    Assertions.assertNotNull(categoryDTOS);
    Assertions.assertTrue(categoryWebResponses.get(0).isGenericTemplateEligible());
    Assertions.assertTrue(DOCUMENTS.equals(categoryWebResponses.get(0).getDocumentType()));
  }

  @Test
  void findCategorySummaryByCatalogIdAndParentIdWithChildrenCountForSalesCatalogTest() {
    GetSubCategoriesServiceRequest request =
        new GetSubCategoriesServiceRequest(CatalogType.SALES_CATALOG.name(), CATALOG_ID, PARENT_ID, Boolean.TRUE,
            Constants.ACTIVE, DOCUMENT_FILTER_TYPE, true, false);
    Mockito.when(
        this.pcbFeign.getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, PARENT_ID, PAGE, SIZE, FILTER_TYPE_ACTIVE,
            DOCUMENT_FILTER_TYPE, true, false)).thenReturn(categoryDTOGdnRestListResponse);
    List<CategoryWebResponse> categoryWebResponses =
        catalogService.getSubCategoriesByCatalogIdAndParentCategoryId(request);
    Mockito.verify(this.pcbFeign)
        .getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, PARENT_ID, PAGE, SIZE, FILTER_TYPE_ACTIVE,
            DOCUMENT_FILTER_TYPE, true, false);
    Assertions.assertNotNull(categoryWebResponses);
  }

  @Test
  void findCategorySummaryByCatalogIdAndParentIdForSalesCatalogAndNullSequenceTest() {
    GetSubCategoriesServiceRequest request =
        new GetSubCategoriesServiceRequest(CatalogType.SALES_CATALOG.name(), CATALOG_ID, PARENT_ID, Boolean.TRUE,
            Constants.ACTIVE, DOCUMENT_FILTER_TYPE, true, false);
    categoryDTOGdnRestListResponse.getContent().get(0).setSequence(null);
    Mockito.when(
        this.pcbFeign.getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, PARENT_ID, PAGE, SIZE, FILTER_TYPE_ACTIVE,
            DOCUMENT_FILTER_TYPE, true, false)).thenReturn(categoryDTOGdnRestListResponse);
    List<CategoryWebResponse> categoryWebResponses =
        catalogService.getSubCategoriesByCatalogIdAndParentCategoryId(request);
    Mockito.verify(this.pcbFeign)
        .getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, PARENT_ID, PAGE, SIZE, FILTER_TYPE_ACTIVE,
            DOCUMENT_FILTER_TYPE, true, false);
    Assertions.assertNotNull(categoryWebResponses);
  }

  @Test
  void findCategorySummaryByCatalogIdAndParentIdForSalesCatalogAndB2bSalesCategoryNullSequenceTest() {
    GetSubCategoriesServiceRequest request =
        new GetSubCategoriesServiceRequest(CatalogType.B2B_SALES_CATALOG.name(), CATALOG_ID, PARENT_ID, Boolean.TRUE,
            Constants.ACTIVE, DOCUMENT_FILTER_TYPE, true, false);
    categoryDTOGdnRestListResponse.getContent().get(0).setSequence(null);
    Mockito.when(
        this.pcbFeign.getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, PARENT_ID, PAGE, SIZE, FILTER_TYPE_ACTIVE,
            DOCUMENT_FILTER_TYPE, true, false)).thenReturn(categoryDTOGdnRestListResponse);
    List<CategoryWebResponse> categoryWebResponses =
        catalogService.getSubCategoriesByCatalogIdAndParentCategoryId(request);
    Mockito.verify(this.pcbFeign)
        .getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, PARENT_ID, PAGE, SIZE, FILTER_TYPE_ACTIVE,
            DOCUMENT_FILTER_TYPE, true, false);
    Assertions.assertNotNull(categoryWebResponses);
  }


  @Test
  void findCategorySummaryByCatalogIdAndParentIdWithHideInactiveTrueTest() throws Exception {
    GetSubCategoriesServiceRequest request =
        new GetSubCategoriesServiceRequest(CatalogType.MASTER_CATALOG.name(), CATALOG_ID, PARENT_ID, Boolean.FALSE,
            Constants.IN_ACTIVE, DOCUMENT_FILTER_TYPE, true, false);
    Mockito.when(this.pcbFeign.getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, PARENT_ID, PAGE, SIZE,
        FILTER_TYPE_INACTIVE, DOCUMENT_FILTER_TYPE, true, false)).thenReturn(categoryDTOGdnRestListResponse);
    List<CategoryWebResponse> categoryWebResponses =
        catalogService.getSubCategoriesByCatalogIdAndParentCategoryId(request);
    Mockito.verify(this.pcbFeign)
        .getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, PARENT_ID, PAGE, SIZE, FILTER_TYPE_INACTIVE,
            DOCUMENT_FILTER_TYPE, true, false);
    Assertions.assertTrue(categoryWebResponses.size() == 1);
    Assertions.assertNotNull(categoryWebResponses);
  }

  @Test
  void findCategorySummaryByCatalogIdAndParentIdWithNullResponse() throws Exception {
    Exception exception = new Exception();
    GetSubCategoriesServiceRequest request =
        new GetSubCategoriesServiceRequest(CatalogType.MASTER_CATALOG.name(), CATALOG_ID, PARENT_ID, Boolean.FALSE,
            Constants.ACTIVE, DOCUMENT_FILTER_TYPE, true, false);
    Mockito.when(
        this.pcbFeign.getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, PARENT_ID, PAGE, SIZE, FILTER_TYPE_ACTIVE,
            DOCUMENT_FILTER_TYPE, true, false)).thenReturn(null);
    List<CategoryWebResponse> categoryWebResponses = null;
    try {
      categoryWebResponses = catalogService.getSubCategoriesByCatalogIdAndParentCategoryId(request);
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertEquals(ClientException.class, exception.getClass());
      Assertions.assertNull(categoryWebResponses);
      Mockito.verify(this.pcbFeign)
          .getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, PARENT_ID, PAGE, SIZE, FILTER_TYPE_ACTIVE,
              DOCUMENT_FILTER_TYPE, true, false);
    }
  }

  @Test
  void findCatalogSummaryTestWithValidMasterCatalogType() throws Exception {
    Mockito.when(this.pcbFeign
        .getCatalogSummaryByCatalogType(CatalogType.MASTER_CATALOG.toString(), PAGE, SIZE))
        .thenReturn(catalogResponseGdnRestListResponse);
    List<CatalogDetailResponse> catalogResponseList =
        catalogService.getCatalogSummaryByCatalogType(CatalogType.MASTER_CATALOG.toString());
    Mockito.verify(this.pcbFeign).getCatalogSummaryByCatalogType(CatalogType.MASTER_CATALOG.toString(), PAGE, SIZE);
    Assertions.assertTrue(catalogResponseList.get(0).getId().equals(CATALOG_ID));
    Assertions.assertNotNull(categoryDTOS);
  }

  @Test
  void findCatalogSummaryTestWithValidSalesCatalogType() throws Exception {
    Mockito.when(this.pcbFeign
        .getCatalogSummaryByCatalogType(CatalogType.SALES_CATALOG.toString(), PAGE, SIZE))
        .thenReturn(catalogResponseGdnRestListResponse);
    List<CatalogDetailResponse> catalogResponseList =
        catalogService.getCatalogSummaryByCatalogType(CatalogType.SALES_CATALOG.toString());
    Mockito.verify(this.pcbFeign).getCatalogSummaryByCatalogType(CatalogType.SALES_CATALOG.toString(), PAGE, SIZE);
    Assertions.assertTrue(catalogResponseList.get(0).getId().equals(CATALOG_ID));
    Assertions.assertNotNull(categoryDTOS);
  }

  @Test
  void findCatalogSummaryWithNullResponseTest() {
    Exception exception = new Exception();
    Mockito.when(this.pcbFeign
        .getCatalogSummaryByCatalogType(CatalogType.SALES_CATALOG.toString(), PAGE, SIZE))
        .thenReturn(null);
    List<CatalogDetailResponse> catalogResponseList = null;
    try {
      catalogResponseList = catalogService.getCatalogSummaryByCatalogType(CatalogType.SALES_CATALOG.toString());
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertEquals(ClientException.class, exception.getClass());
      Assertions.assertNull(catalogResponseList);
      Mockito.verify(this.pcbFeign).getCatalogSummaryByCatalogType(CatalogType.SALES_CATALOG.toString(), PAGE, SIZE);
    }
  }

  @Test
  void findCatalogSummaryTestWithInvalidCatalogType() throws Exception {
    try{
    catalogService.getCatalogSummaryByCatalogType(CATALOG_ID);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  void getListOfCategoryHierarchyByCategoryNameTest() {
    Mockito.when(
        pcbFeign.findCategorySummaryByName(CATEGORY_NAME, PAGE, Integer.MAX_VALUE, FILTER_TYPE_ALL, FILTER_TYPE_ALL))
        .thenReturn(new GdnRestListResponse<>(categoryResponseList, new PageMetaData(1, 1, 1), REQUEST_ID));
    CategoryResponse categoryDetailResponse = getCategoryResponse();
    Mockito.when(pcbFeign.getCategoryDetail(CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(new CategoryDetailResponse(categoryDetailResponse), REQUEST_ID));
    List<List<CategorySearchWebResponse>> listOfCategoryHierarchyByCategoryName = catalogService
        .getListOfCategoryHierarchyByCategoryName(
            new CategoryHierarchyServiceRequest(CATEGORY_NAME, CATALOG_ID, FILTER_TYPE_ALL, FILTER_TYPE_ALL));
    Mockito.verify(pcbFeign)
        .findCategorySummaryByName(CATEGORY_NAME, PAGE, Integer.MAX_VALUE, FILTER_TYPE_ALL, FILTER_TYPE_ALL);
    Mockito.verify(pcbFeign).getCategoryDetail(CATEGORY_ID);
    Assertions.assertEquals(CATEGORY_CODE, listOfCategoryHierarchyByCategoryName.get(0).get(0).getCategoryCode());

  }

  private CategoryResponse getCategoryResponse() {
    CategoryResponse categoryDetailResponse = new CategoryResponse();
    categoryDetailResponse.setCategoryCode(CATEGORY_CODE);
    categoryDetailResponse.setId(CATEGORY_ID);
    return categoryDetailResponse;
  }

  @Test
  void getListOfCategoryHierarchyByCategoryName_withFilterTypeActiveTest() {
    categoryResponse.setActivated(Boolean.TRUE);
    Mockito.when(pcbFeign.findCategorySummaryByName(CATEGORY_NAME, PAGE, Integer.MAX_VALUE, FILTER_TYPE_ACTIVE, FILTER_TYPE_ALL))
        .thenReturn(new GdnRestListResponse<>(categoryResponseList, new PageMetaData(1, 1, 1), REQUEST_ID));
    CategoryResponse categoryDetailResponse = getCategoryResponse();
    Mockito.when(pcbFeign.getCategoryDetail(CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(new CategoryDetailResponse(categoryDetailResponse), REQUEST_ID));
    List<List<CategorySearchWebResponse>> listOfCategoryHierarchyByCategoryName =
        catalogService.getListOfCategoryHierarchyByCategoryName(
        new CategoryHierarchyServiceRequest(CATEGORY_NAME, CATALOG_ID, Constants.ACTIVE, Constants.ALL));
    Mockito.verify(pcbFeign).findCategorySummaryByName(CATEGORY_NAME, PAGE, Integer.MAX_VALUE, FILTER_TYPE_ACTIVE, FILTER_TYPE_ALL);
    Mockito.verify(pcbFeign).getCategoryDetail(CATEGORY_ID);
    Assertions.assertEquals(CATEGORY_CODE, listOfCategoryHierarchyByCategoryName.get(0).get(0).getCategoryCode());
  }

  @Test
  void getListOfCategoryHierarchyByCategoryName_withDocumentFilterTypeTest() {
    categoryResponse.setActivated(Boolean.TRUE);
    Mockito.when(pcbFeign.findCategorySummaryByName(CATEGORY_NAME, PAGE, Integer.MAX_VALUE, FILTER_TYPE_ALL,
        DOCUMENT_FILTER_TYPE_HAVE_DOCUMENT))
        .thenReturn(new GdnRestListResponse<>(categoryResponseList, new PageMetaData(1, 1, 1), REQUEST_ID));
    CategoryResponse categoryDetailResponse = getCategoryResponse();
    categoryDetailResponse.setDocumentType(DOCUMENTS);
    Mockito.when(pcbFeign.getCategoryDetail(CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(new CategoryDetailResponse(categoryDetailResponse), REQUEST_ID));
    List<List<CategorySearchWebResponse>> listOfCategoryHierarchyByCategoryName = catalogService
        .getListOfCategoryHierarchyByCategoryName(
            new CategoryHierarchyServiceRequest(CATEGORY_NAME, CATALOG_ID, Constants.ALL, Constants.HAVE_DOCUMENT));
    Mockito.verify(pcbFeign).findCategorySummaryByName(CATEGORY_NAME, PAGE, Integer.MAX_VALUE, FILTER_TYPE_ALL,
        DOCUMENT_FILTER_TYPE_HAVE_DOCUMENT);
    Mockito.verify(pcbFeign).getCategoryDetail(CATEGORY_ID);
    Assertions.assertEquals(CATEGORY_CODE, listOfCategoryHierarchyByCategoryName.get(0).get(0).getCategoryCode());
  }

  @Test
  void getListOfCategoryHierarchyByCategoryName_withFilterTypeInActiveTest() {
    categoryResponse.setActivated(Boolean.FALSE);
    Mockito.when(pcbFeign.findCategorySummaryByName(CATEGORY_NAME, PAGE, Integer.MAX_VALUE, FILTER_TYPE_INACTIVE, FILTER_TYPE_ALL))
        .thenReturn(new GdnRestListResponse<>(categoryResponseList, new PageMetaData(1, 1, 1), REQUEST_ID));
    CategoryResponse categoryDetailResponse = getCategoryResponse();
    Mockito.when(pcbFeign.getCategoryDetail(CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(new CategoryDetailResponse(categoryDetailResponse), REQUEST_ID));
    List<List<CategorySearchWebResponse>> listOfCategoryHierarchyByCategoryName = catalogService.getListOfCategoryHierarchyByCategoryName(
        new CategoryHierarchyServiceRequest(CATEGORY_NAME, CATALOG_ID, Constants.IN_ACTIVE, Constants.ALL));
    Mockito.verify(pcbFeign).findCategorySummaryByName(CATEGORY_NAME, PAGE, Integer.MAX_VALUE, FILTER_TYPE_INACTIVE, FILTER_TYPE_ALL);
    Mockito.verify(pcbFeign).getCategoryDetail(CATEGORY_ID);
    Assertions.assertEquals(CATEGORY_CODE, listOfCategoryHierarchyByCategoryName.get(0).get(0).getCategoryCode());
  }

  @Test
  void getListOfCategoryHierarchyByCategoryName_ForC2CategoryTest() {
    categoryResponse.setParentCategoryId(CATEGORY_ID);
    categoryResponse.setActivated(Boolean.FALSE);
    categoryResponseList.add(categoryResponse);
    CategoryResponse response = new CategoryResponse();
    response.setId(CATEGORY_ID);
    response.setCategoryCode(CATEGORY_CODE_1);
    Mockito.when(pcbFeign.findCategorySummaryByName(CATEGORY_NAME, PAGE, Integer.MAX_VALUE, FILTER_TYPE_INACTIVE, FILTER_TYPE_ALL))
        .thenReturn(
            new GdnRestListResponse<CategoryResponse>(categoryResponseList, new PageMetaData(1, 1, 1), REQUEST_ID));
    Mockito.when(pcbFeign.getCategoryDetail(CATEGORY_ID)).thenReturn(
        new GdnRestSingleResponse<CategoryDetailResponse>(new CategoryDetailResponse(categoryResponse), REQUEST_ID),
        new GdnRestSingleResponse<CategoryDetailResponse>(new CategoryDetailResponse(response), REQUEST_ID));
    List<List<CategorySearchWebResponse>> listOfCategoryHierarchyByCategoryName = catalogService.getListOfCategoryHierarchyByCategoryName(
        new CategoryHierarchyServiceRequest(CATEGORY_NAME, CATALOG_ID, Constants.IN_ACTIVE, Constants.ALL));
    Mockito.verify(pcbFeign).findCategorySummaryByName(CATEGORY_NAME, PAGE, Integer.MAX_VALUE, FILTER_TYPE_INACTIVE, FILTER_TYPE_ALL);
    Mockito.verify(pcbFeign, Mockito.times(3)).getCategoryDetail(CATEGORY_ID);
    Assertions.assertEquals(CATEGORY_CODE_1, listOfCategoryHierarchyByCategoryName.get(1).get(0).getCategoryCode());
  }

  @Test
  void getListOfCategoryHierarchyByCategoryName_ForChildCountZeroTest() {
    categoryResponse.setParentCategoryId(CATEGORY_ID);
    categoryResponse.setActivated(Boolean.FALSE);
    categoryResponse.setChildCount(0);
    categoryResponse.setId(CATEGORY_ID);
    categoryResponseList.add(categoryResponse);
    CategoryResponse response = new CategoryResponse();
    response.setId(CATEGORY_ID);
    response.setCategoryCode(CATEGORY_CODE_1);
    response.setChildCount(0);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse(response);
    categoryDetailResponse.setId(CATEGORY_ID);
    Mockito.when(pcbFeign
        .findCategorySummaryByName(CATEGORY_NAME, PAGE, Integer.MAX_VALUE, FILTER_TYPE_INACTIVE, FILTER_TYPE_ALL))
        .thenReturn(new GdnRestListResponse<>(categoryResponseList, new PageMetaData(1, 1, 1), REQUEST_ID));
    Mockito.when(pcbFeign.getCategoryDetail(CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, REQUEST_ID));
    List<List<CategorySearchWebResponse>> listOfCategoryHierarchyByCategoryName = catalogService
        .getListOfCategoryHierarchyByCategoryName(
            new CategoryHierarchyServiceRequest(CATEGORY_NAME, CATALOG_ID, Constants.IN_ACTIVE, Constants.ALL));
    Mockito.verify(pcbFeign)
        .findCategorySummaryByName(CATEGORY_NAME, PAGE, Integer.MAX_VALUE, FILTER_TYPE_INACTIVE, FILTER_TYPE_ALL);
    Mockito.verify(pcbFeign, Mockito.times(2)).getCategoryDetail(CATEGORY_ID);
    Assertions.assertEquals(CATEGORY_CODE_1, listOfCategoryHierarchyByCategoryName.get(1).get(0).getCategoryCode());
  }
}