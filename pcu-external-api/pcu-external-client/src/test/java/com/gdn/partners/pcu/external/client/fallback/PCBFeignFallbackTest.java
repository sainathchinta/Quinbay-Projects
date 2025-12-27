package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.x.productcategorybase.dto.BrandInReviewResponse;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandPredefinedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandSummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.UPCCodeSearchRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailMapResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryShippingWeightResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class PCBFeignFallbackTest {

  private PCBFeignFallback feignFallback = new PCBFeignFallback();
  private static final String PRODUCT_ID = "product_id";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final double LENGTH = 10;
  private static final double WIDTH = 10;
  private static final double HEIGHT = 10;
  private static final double WEIGHT = 10;
  private static final String KEYWORD = "apple";
  private static final String UPCCODE = "1234567";
  private static final String BP_CODE = "BP_CODE";
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 10;
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "MTA-0001";
  private static final String SIZE_CHART_CODE = "sizeChartCode";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "BliBli";
  private static final String DEFAULT_BRAND_NAME = "Blibli.com";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_STATUS = "brandStatus";
  private static final String DEFAULT_PRODUCT_ITEM_NAME = "Product Item Name";
  private static final String DEFAULT_CATEGORY_ID = "CATEGORY_ID";
  private static final String STORE_ID = "store-id";
  private static final String REQUEST_ID = "request-id";
  private CreateBrandWipRequest createBrandWipRequest = new CreateBrandWipRequest();
  private BrandSummaryRequest brandSummaryRequest = new BrandSummaryRequest();
  private CategoryCodeRequest categoryCodeRequest = new CategoryCodeRequest();

  @Test
  public void getProductDetailsByIdTest() {
    GdnRestSingleResponse<ProductDetailResponse> response = feignFallback.getProductDetailsById(PRODUCT_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void filterCategoryHierarchyByCategoryCodeTest(){
    GdnRestListResponse<CategoryResponse> response =
        feignFallback.filterCategoryHierarchyByCategoryCode(CATEGORY_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void generateShippingWeightTest() {
    GdnRestSingleResponse<CategoryShippingWeightResponse> response =
        feignFallback.generateShippingWeight(CATEGORY_CODE, LENGTH, WIDTH, HEIGHT, WEIGHT);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getBrandSuggestionsTest() {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response =
        feignFallback.getBrandSuggestions(KEYWORD, BP_CODE, Boolean.TRUE, Boolean.FALSE, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void filterSummaryTest() {
    GdnRestListResponse<BrandResponse> response = feignFallback.filterSummary(PAGE, SIZE, brandSummaryRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void createTest() {
    GdnRestSingleResponse<CreateBrandWipResponse> response =
        feignFallback.create(createBrandWipRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void filterByBrandNameTest() {
    GdnRestSingleResponse<BrandResponse> response =
        feignFallback.filterByBrandName(DEFAULT_BRAND_NAME, true);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getBrandDetailTest() {
    GdnRestSingleResponse<BrandWipResponse> response = feignFallback.getBrandDetail(BRAND_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void findByBrandNameAndBusinessPartnerCodeTest() {
    GdnRestSingleResponse<BrandWipResponse> response = feignFallback.findByBrandNameAndBusinessPartnerCode(DEFAULT_BRAND_NAME, DEFAULT_BUSINESS_PARTNER_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getProductItemSuggestionsByItemNameAndCategoryIdTest() {
    GdnRestListResponse<ProductItemDetailResponse> response = feignFallback
        .getProductItemSuggestionsByItemNameAndCategoryId(DEFAULT_PRODUCT_ITEM_NAME, DEFAULT_CATEGORY_ID, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getAllChildCategoryCodesByC1CategoryCodeTest() {
    GdnRestSingleResponse<CategoryCodeResponse> response = feignFallback
        .getAllChildCategoryCodesByC1CategoryCode(categoryCodeRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getCategoryDetailTest() {
    GdnRestSingleResponse<CategoryDetailResponse> response = feignFallback.getCategoryDetail(StringUtils.EMPTY);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getAttributeDetailTest() {
    GdnRestSingleResponse<AttributeResponse> response = feignFallback.getAttributeDetail(StringUtils.EMPTY, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getCategoriesByCategoryCodesTest() {
    GdnRestListResponse<CategoryDTO> response =
        feignFallback.getCategoriesByCategoryCodes(PAGE, SIZE, Boolean.TRUE.toString(), StringUtils.EMPTY);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getProductItemsByUPCCodeAndCategoryIdsTest() {
    GdnRestListResponse<ProductItemDetailResponse> response =
        feignFallback.getProductItemsByUPCCodeAndCategory(PAGE, SIZE, new UPCCodeSearchRequest(), Boolean.TRUE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getCategoryHierarchyByUPCCodeWithProductCountTest() {
    GdnRestListResponse<CategoryHierarchyResponse> response =
        feignFallback.getCategoryHierarchyByUPCCodeWithProductCount(UPCCODE, Boolean.TRUE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getBrandPredefinedValueDetailTest() {
    GdnRestSingleResponse<BrandPredefinedAttributeValueResponse> response =
        feignFallback.getBrandPredefinedValueDetail(BRAND_CODE, BRAND_STATUS);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getCategoryWholesaleConfigurationTest() {
    GdnRestSingleResponse<WholesaleMappingResponse> response =
        feignFallback.getCategoryWholesaleConfiguration(null, CATEGORY_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getDefaultBrandsTest() {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response = feignFallback.getDefaultBrands();
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getCategoryNamesTest() {
    GdnRestSingleResponse<CategoryNamesResponse> response =
        feignFallback.getCategoryNames(new CategoryMultipleIdRequest(), PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void validateAuthorisedBrandTest() {
    GdnRestSingleResponse<SimpleBooleanResponse> response =
        feignFallback.validateAuthorisedBrand(BRAND_CODE, BRAND_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getAllInReviewBrandsTest() {
    GdnRestListResponse<BrandInReviewResponse> response =
      feignFallback.getAllInReviewBrands(STORE_ID, REQUEST_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getBasicSizeChartDetailsTest() {
    GdnRestSingleResponse<BasicSizeChartDetailMapResponse> response =
        feignFallback.getBasicSizeChartDetails(Collections.singletonList(SIZE_CHART_CODE));
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getDistributionInfoTest() {
    GdnRestListResponse<DistributionInfoPerSkuResponse> response =
        feignFallback.getDistributionInfo(PRODUCT_ID, Boolean.TRUE, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void reindexBrandCollectionTest() {
    GdnBaseRestResponse response = feignFallback.reindexBrandCollection(BRAND_CODE);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }
}
