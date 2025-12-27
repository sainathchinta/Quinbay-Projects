package com.gdn.partners.pcu.master.client.fallback;


import com.gdn.partners.pcu.master.client.model.AttributeResponse;
import com.gdn.partners.pcu.master.client.model.CategoryDetailAndShippingResponse;
import com.gdn.partners.pcu.master.client.model.DimensionRequest;
import com.gdn.partners.pcu.master.client.model.MasterAttributeRequest;
import com.gdn.partners.pcu.master.client.model.ModifyDimensionMappingRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartFilterRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartFilterResponse;
import com.gdn.partners.pcu.master.client.model.SizeChartRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartResponse;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsSearchRequest;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.margin.webmodel.GdnRestSimpleResponse;
import com.gdn.partners.pcu.master.client.feign.PCBFeign;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.x.product.rest.web.model.SystemParameterResponse;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.OscInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryDetailRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.CategoryMappingsUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRestrictedKeywordsRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeAddRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeFilterRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.OriginalSalesCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleMappingRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.OscSummaryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;


public class PCBFeignFallbackTest {

  private PCBFeign pcbFeign = new PCBFeignFallback();

  private static final String ATTRIBUTE_ID = "attribute_id";
  private static final String ATTRIBUTE_CODE = "attribute_code";
  private static final String CATALOG_ID = "catalog_id";
  private static final String PARENT_ID = "parent_id";
  private static final String CATEGORY_NAME = "category_name";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_ID = "category_id";
  private static final String DEFAULT_STATE = "ALL";
  private static final String VALUE = "VALUE";
  private static final String VARIABLE = "documentType";
  private static final String OSC_CODE = "oscCode";
  private static final String KEYWORD = "keyword";
  private static final Boolean ACTIVATED = true;
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 10;
  private static final String DOCUMENT_FILTER_TYPE = "ALL";
  private static final String ID = "id";
  private static final String SIZE_CHART_CODE = "SC-0001";
  private static final String SIZE_CHART_NAME = "sizeChartName";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String DIMENSION_NAME = "DIMENSION_NAME";

  private PCBFeignFallback feignFallback = new PCBFeignFallback();

  @Test
  void getAttributeDetailTest() {
    GdnRestSingleResponse<AttributeResponse> response =
      pcbFeign.getAttributeDetail(ATTRIBUTE_ID, Boolean.FALSE);

    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  void getAttributeValuesTest() {
    GdnRestListResponse<AttributeValueResponse> response =
      pcbFeign.getAttributeValues(ATTRIBUTE_CODE, PAGE, SIZE, false, false);

    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  void saveMasterAttributeTest() {
    MasterAttributeRequest masterAttributeRequest = new MasterAttributeRequest();
    masterAttributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    GdnBaseRestResponse response = pcbFeign.saveMasterAttribute(masterAttributeRequest);

    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  void getAttributesByAttributeFilterTest() {
    GdnRestListResponse<MasterAttributeResponse> response =
      feignFallback.getAttributesByAttributeFilter(new MasterAttributeFilterRequest(), 0, 0);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertNull(response.getContent());
  }

  @Test
  void getAttributeInfoTest() {
    GdnRestSingleResponse<MasterAttributeResponse> response =
      pcbFeign.getAttributeInfo(ATTRIBUTE_CODE);

    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  void updateMasterAttributeTest() {
    MasterAttributeRequest masterAttributeRequest = new MasterAttributeRequest();
    masterAttributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    GdnBaseRestResponse response = pcbFeign.updateMasterAttribute(masterAttributeRequest);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  void updateMasterAttributeValuesTest() {
    GdnBaseRestResponse response =
      pcbFeign.updateMasterAttributeValues(ATTRIBUTE_CODE, new MasterAttributeUpdateRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  void addMasterAttributeValueTest() {
    GdnBaseRestResponse response =
      pcbFeign.addMasterAttributeValue(ATTRIBUTE_CODE, new MasterAttributeAddRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  void getChildFromParentByCatalogIdWithChildCountTest() {
    GdnRestListResponse<CategoryDTO> response =
      pcbFeign.getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, PARENT_ID, PAGE, SIZE,
        DEFAULT_STATE, DOCUMENT_FILTER_TYPE, false, false);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertNull(response.getContent());
  }

  @Test
  void getCatalogSummaryByCatalogTypeTest() {
    GdnRestListResponse<CatalogResponse> response =
      pcbFeign.getCatalogSummaryByCatalogType(CATALOG_ID, PAGE, SIZE);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertNull(response.getContent());
  }

  @Test
  void findCategorySummaryByNameTest() {
    GdnRestListResponse<CategoryResponse> response =
      feignFallback.findCategorySummaryByName(CATEGORY_NAME, 0, 0, DEFAULT_STATE, DEFAULT_STATE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertNull(response.getContent());
  }

  @Test
  void getCategoryDetailTest() {
    GdnRestSingleResponse<CategoryDetailResponse> response =
      feignFallback.getCategoryDetail(CATEGORY_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  void getCategoryShippingByCategoryCodeTest() {
    GdnRestListResponse<CategoryShippingResponse> response =
      feignFallback.getCategoryShippingByCategoryCode(CATEGORY_NAME);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertNull(response.getContent());
  }

  @Test
  void activateCategoryTest() {
    GdnBaseRestResponse response =
      pcbFeign.updateCategoryInfo(new CategoryInfoUpdateRequest(), Boolean.TRUE);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void getCategoryInfoWithShippingDetailTest() {
    GdnRestSingleResponse<CategoryDetailAndShippingResponse> response =
        feignFallback.getCategoryInfoByCategoryId(true, CATEGORY_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  void updateCategoryMappingsTest() {
    GdnBaseRestResponse response =
      pcbFeign.updateCategoryMappings(new CategoryMappingsUpdateRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void getHierarchyByCategoryCodesTest() {
    GdnBaseRestResponse response =
      feignFallback.getHierarchyByCategoryCodes(new CategoryCodeRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  void createCategoryTest() {
    GdnBaseRestResponse response = feignFallback.createCategory(new CategoryDetailRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  void getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageableTest() {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response =
      feignFallback.getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(ATTRIBUTE_ID,
        VALUE, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  void getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValueTest() {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response =
      feignFallback.getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValue(ATTRIBUTE_ID,
        VALUE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  void getAttributeValuesByAttributeCodesTest() {
    GdnRestListResponse<AttributeResponse> response =
      feignFallback.getAttributeValuesByAttributeCodes(new AttributeCodesRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  void findCategorySummaryByParentIdTest() {
    GdnRestListResponse<CategoryResponse> response =
      pcbFeign.findCategorySummaryByParentId(SIZE, PARENT_ID);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void getAttributeByAttributeCodeTest() {
    GdnRestSingleResponse<AttributeResponse> response =
      pcbFeign.getAttributeDetailAndValuesByAttributeCode(ATTRIBUTE_CODE);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void getCategoryRestrictedKeywordsTest() {
    GdnRestListResponse<RestrictedKeywordsResponse> response =
      feignFallback.getCategoryRestrictedKeywords(0, 0, new CategoryRestrictedKeywordsRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  void getRestrictedKeywordsTest() {
    GdnRestListResponse<RestrictedKeywordsResponse> response =
      feignFallback.getRestrictedKeywords(new RestrictedKeywordsSearchRequest(), 0, 0);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  void updateCategoriesWithRestrictedKeywordsTest() {
    GdnBaseRestResponse response = pcbFeign.updateCategoriesWithRestrictedKeywords(CATEGORY_CODE,
      new CategoryKeywordUpdateRequestList());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void getUiValidationRestrictedKeywordsTest() {
    GdnRestListResponse response = pcbFeign.getUiValidationRestrictedKeywords();
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void getRestrictedKeywordForListingTest() {
    GdnRestListResponse response = pcbFeign.getRestrictedKeywordForListing(KEYWORD, PAGE, SIZE);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void updateRestrictedKeywordTest() {
    GdnBaseRestResponse response = pcbFeign.updateRestrictedKeyword(null);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void getRestrictedKeywordHistoryTest() {
    GdnRestListResponse response = pcbFeign.getRestrictedKeywordHistory(KEYWORD, 0, 0);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void getCategoryWholesaleConfigTest() {
    GdnBaseRestResponse response = pcbFeign.getCategoryWholesaleConfig(CATEGORY_ID, CATEGORY_CODE);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void updateCategoriesWithWholesaleConfigTest() {
    GdnBaseRestResponse response =
      pcbFeign.updateCategoriesWithWholesaleConfig(CATEGORY_ID, new WholesaleMappingRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void getCategoryTreeTest() {
    GdnRestListResponse<CategoryTreeResponse> response = feignFallback.getCategoryTree(true);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  void getDocumentListTest() {
    GdnRestSingleResponse<SystemParameterResponse> response =
      feignFallback.getDocumentList(VARIABLE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  void validateCategory() {
    GdnBaseRestResponse response = feignFallback.validateCategory(VARIABLE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  void createOriginalSalesCategoryTest() {
    GdnRestSimpleResponse<String> response =
      pcbFeign.createOriginalSalesCategory(new OriginalSalesCategoryRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void filterOscSummaryResponse() {
    GdnRestListResponse<OscSummaryResponse> response =
      feignFallback.filterOscSummaryResponse(OSC_CODE, KEYWORD, ACTIVATED);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  void updateOriginalSalesCategoryTest() {
    GdnBaseRestResponse response = pcbFeign.updateOriginalSalesCategory(new OscInfoUpdateDTO());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void getOSCByIdTest() {
    GdnBaseRestResponse response = pcbFeign.getOSCById(ID);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void filterCategoryHierarchyByCategoryCodeTest() {
    GdnBaseRestResponse response = pcbFeign.filterCategoryHierarchyByCategoryCode(CATEGORY_CODE);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void fetchDimensionDetailTest() {
    GdnBaseRestResponse response = pcbFeign.fetchDimensionDetail(ID);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void fetchDimensionListingTest() {
    GdnBaseRestResponse response = pcbFeign.fetchDimensionListing(PAGE, SIZE, null);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void createDimensionTest() {
    DimensionRequest request = new DimensionRequest();
    GdnBaseRestResponse response = pcbFeign.createDimension(request);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void fetchDimensionMappingTest() {
    GdnBaseRestResponse response = pcbFeign.fetchDimensionMapping(ATTRIBUTE_CODE, PAGE, SIZE);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void modifyDimensionMappingTest() {
    GdnBaseRestResponse response =
        pcbFeign.modifyDimensionMapping(ATTRIBUTE_CODE, new ModifyDimensionMappingRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void createSizeChartTest() {
    GdnBaseRestResponse response =
        pcbFeign.upsertSizeChart(new SizeChartRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void fetchSizeChartTest() {
    GdnBaseRestResponse response = pcbFeign.fetchSizeChartDetails(SIZE_CHART_CODE, false);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void editDimensionTest() {
    DimensionRequest request = new DimensionRequest();
    GdnBaseRestResponse response = pcbFeign.editDimension(request);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void deleteSizeChart() {
    GdnBaseRestResponse response =
        pcbFeign.updateSizeChartStatus(SIZE_CHART_CODE, true, true, null);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void findByNameTest() {
    GdnBaseRestResponse response = pcbFeign.findByName(DIMENSION_NAME);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void filterSizeChartTest() {
    SizeChartFilterRequest request = new SizeChartFilterRequest();
    GdnRestListResponse<SizeChartFilterResponse> response = pcbFeign.filter(PAGE, SIZE, request);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void validateCategoryCodeTest() {
    GdnRestSingleResponse<SimpleBooleanResponse> response =
        pcbFeign.validateCategoryCode(SIZE_CHART_NAME, BUSINESS_PARTNER_CODE);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  void findBySizeChartNameAndBusinessPartnerCodeTest() {
    GdnRestSingleResponse<SizeChartResponse> response =
        pcbFeign.findBySizeChartNameAndBusinessPartnerCode(SIZE_CHART_NAME, BUSINESS_PARTNER_CODE);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }
}
