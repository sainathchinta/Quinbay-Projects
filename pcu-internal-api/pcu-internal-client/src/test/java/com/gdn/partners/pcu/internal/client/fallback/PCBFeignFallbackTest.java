package com.gdn.partners.pcu.internal.client.fallback;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.gdn.partners.pcu.internal.client.model.request.BrandAuthDeleteRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthUpdateRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthorisationWipActionRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthorisationWipListRequest;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthWipDetailResponse;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthCreateWipRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthFilterRequest;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthCreateWipResponse;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthFilterResponse;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthorisationWipListResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.request.SystemParameterRequest;
import com.gdn.x.productcategorybase.dto.response.BrandAuthorisationDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.Test;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.MerchantSearchResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectionInfoResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistorySummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandResponse;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryParentResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeNodeResponse;
import com.gdn.x.productcategorybase.dto.response.ConfigurationCountResponse;
import com.gdn.x.productcategorybase.dto.response.LookupResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;

public class PCBFeignFallbackTest {

  private PCBFeignFallback feignFallback = new PCBFeignFallback();
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_ID = "categoryId";
  private static final String BRAND_CODE = "brandCode";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_CODE = "productCode";
  private static final String BRAND_DELETE_REASON = "brandDeleteReason";
  private static final String BRAND_REQUEST_CODE = "brandRequestCode";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String DEFAULT_SELLER_CODE = "sellerCode";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "username";
  private static final String DEFAULT_REQUEST_ID = "request-id";
  private static final String STATUS = "ACTIVE";

  private List<BrandWipResponse> brandWipResponseList = new ArrayList<>();
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private GdnRestListResponse<BrandWipResponse> brandWipResponseGdnRestListResponse = new GdnRestListResponse<>();
  private BrandWipSummaryRequest brandWipSummaryRequest = new BrandWipSummaryRequest();
  private BrandAuthorisationWipActionRequest brandAuthorisationWipActionRequest =
      new BrandAuthorisationWipActionRequest();
  private BrandAuthCreateWipRequest brandAuthCreateWipRequest =
      new BrandAuthCreateWipRequest();
  private BrandAuthUpdateRequest brandAuthUpdateRequest =
      new BrandAuthUpdateRequest();
  private BrandWipHistorySummaryRequest brandWipHistorySummaryRequest = new BrandWipHistorySummaryRequest();
  private CategoryConfigurationRequest categoryConfigurationRequest = new CategoryConfigurationRequest();
  private MerchantConfigurationRequest merchantConfigurationRequest = new MerchantConfigurationRequest();
  private List<ConfigurationStatusRequest> configurationStatusRequestList = new ArrayList<>();
  private List<BrandAuthDeleteRequest> brandAuthDeleteRequests = new ArrayList<>();

  @Test
  public void filterCategoryHierarchyByCategoryCodeTest(){
    GdnRestListResponse<CategoryResponse> response =
        feignFallback.filterCategoryHierarchyByCategoryCode(CATEGORY_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getCategoriesAndFinalCategoryMappingTest(){
    GdnRestListResponse<CategoryParentResponse> response = feignFallback.getCategoriesAndFinalCategoryMapping();
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getFinalParentCategoryTest(){
    GdnRestSingleResponse<SingleObjectResponse> response = feignFallback.getFinalParentCategory(CATEGORY_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getCategoryDetailTest() {
    GdnRestSingleResponse<CategoryDetailResponse> response = feignFallback.getCategoryDetail(CATEGORY_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void deleteBrandTest() {
    GdnBaseRestResponse response = feignFallback.deleteBrand(BRAND_CODE, BRAND_DELETE_REASON);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getBrandWipDetailTest() {
    GdnRestSingleResponse<BrandWipResponse> response = feignFallback.getBrandWipDetail(BRAND_REQUEST_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getBrandWipListTest() {
    GdnRestListResponse<BrandWipResponse> response = feignFallback.getBrandWipList(brandWipSummaryRequest,PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getBrandWipHistoryTest() {
    GdnRestListResponse<BrandWipHistoryResponse> response =
        feignFallback.getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void approveBrandTest() {
    GdnRestSingleResponse<CreateBrandResponse> response =
        feignFallback.approveBrand(BRAND_REQUEST_CODE, new BrandApproveRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getBrandRejectionReasonByBrandRequestCodeTest() {
    GdnRestSingleResponse<BrandRejectionInfoResponse> response =
        feignFallback.getBrandRejectionReasonByBrandRequestCode(BRAND_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void rejectBrandTest() {
    GdnRestSingleResponse<BrandWipResponse> response =
        feignFallback.rejectBrand(BRAND_REQUEST_CODE, new BrandRejectRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateTest() {
    GdnBaseRestResponse response =
        feignFallback.update(new BrandApproveRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void filterByBrandCodeTest() {
    GdnRestSingleResponse<BrandResponse> response =
        feignFallback.filterByBrandCode(new String());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void filterByBrandRequestCodeTest() {
    GdnRestSingleResponse<BrandResponse> response =
        feignFallback.filterByBrandCode(new String());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getBrandDetailTest() {
    GdnRestSingleResponse<BrandResponse> response =
        feignFallback.getBrandDetail(BRAND_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateBrandTest() {
    GdnBaseRestResponse response = feignFallback.updateBrand(new UpdateBrandRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void filterProductDetailByProductCodeTest() {
    GdnRestSingleResponse<ProductDetailResponse> response =
        feignFallback.filterProductDetailByProductCode(new String());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }


  @Test
  public void filterProductDetailByProductCodeWithOriginalImagesTest() {
    GdnRestSingleResponse<ProductDetailResponse> response =
        feignFallback.filterProductDetailByProductCodeWithOriginalImages(new String(), new String(), true);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getAttributeByAttributeCodeTest() {
    GdnRestSingleResponse<MasterAttributeResponse> response = feignFallback.getAttributeByAttributeCode(new String());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void fetchMerchantSearchResultTest() {
    GdnRestListResponse<MerchantSearchResponse> responseGdnRestListResponse =
        feignFallback.fetchMerchantSearchResult(new ArrayList<>());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), responseGdnRestListResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, responseGdnRestListResponse.getErrorMessage());
    assertFalse(responseGdnRestListResponse.isSuccess());
  }

  @Test
  public void getCategoryTreeWithReviewConfigTest() {
    GdnRestListResponse<CategoryTreeNodeResponse> responseGdnRestListResponse = feignFallback.getCategoryTreeWithReviewConfig();
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), responseGdnRestListResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, responseGdnRestListResponse.getErrorMessage());
    assertFalse(responseGdnRestListResponse.isSuccess());
  }

  @Test
  public void addCategoryConfigurationStatusTest() {
    GdnBaseRestResponse gdnBaseRestResponse = feignFallback.addCategoryConfigurationStatus(new ArrayList<>());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), gdnBaseRestResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, gdnBaseRestResponse.getErrorMessage());
    assertFalse(gdnBaseRestResponse.isSuccess());
  }

  @Test
  public void updateCategoryConfigurationStatusTest() {
    GdnBaseRestResponse gdnBaseRestResponse = feignFallback.updateCategoryConfigurationStatus(categoryConfigurationRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), gdnBaseRestResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, gdnBaseRestResponse.getErrorMessage());
    assertFalse(gdnBaseRestResponse.isSuccess());
  }

  @Test
  public void deleteCategoryConfigurationStatusTest() {
    GdnBaseRestResponse gdnBaseRestResponse = feignFallback.deleteCategoryConfigurationStatus(new String());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), gdnBaseRestResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, gdnBaseRestResponse.getErrorMessage());
    assertFalse(gdnBaseRestResponse.isSuccess());
  }

  @Test
  public void addMerchantConfigurationStatusTest() {
    GdnBaseRestResponse gdnBaseRestResponse = feignFallback.addMerchantConfigurationStatus(new ArrayList<>());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), gdnBaseRestResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, gdnBaseRestResponse.getErrorMessage());
    assertFalse(gdnBaseRestResponse.isSuccess());
  }

  @Test
  public void updateMerchantConfigurationStatusTest() {
    GdnBaseRestResponse gdnBaseRestResponse = feignFallback.updateMerchantConfigurationStatus(merchantConfigurationRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), gdnBaseRestResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, gdnBaseRestResponse.getErrorMessage());
    assertFalse(gdnBaseRestResponse.isSuccess());
  }

  @Test
  public void deleteMerchantConfigurationStatusTest() {
    GdnBaseRestResponse gdnBaseRestResponse = feignFallback.deleteMerchantConfigurationStatus(new String());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), gdnBaseRestResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, gdnBaseRestResponse.getErrorMessage());
    assertFalse(gdnBaseRestResponse.isSuccess());
  }

  @Test
  public void getConfigurationsStatusByMerchantAndCategoryCodeTest() {
    GdnRestListResponse<ConfigurationStatusResponse> response =
        feignFallback.getConfigurationsStatusByMerchantAndCategoryCode(configurationStatusRequestList);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void fetchConfigurationCountsTest(){
    GdnRestSingleResponse<ConfigurationCountResponse> response = feignFallback.fetchConfigurationCounts();
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getCategoryConfigurationListTest() {
    GdnRestListResponse<CategoryConfigurationFilterResponse> responseGdnRestListResponse =
        feignFallback.getCategoryConfigurationList(new ConfigurationFilterRequest(), PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), responseGdnRestListResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, responseGdnRestListResponse.getErrorMessage());
    assertFalse(responseGdnRestListResponse.isSuccess());
  }

  @Test
  public void getMerchantConfigurationListTest() {
    GdnRestListResponse<MerchantConfigurationFilterResponse> responseGdnRestListResponse =
        feignFallback.getMerchantConfigurationList(new ConfigurationFilterRequest(), PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), responseGdnRestListResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, responseGdnRestListResponse.getErrorMessage());
    assertFalse(responseGdnRestListResponse.isSuccess());
  }

  @Test
  public void getMerchantConfigurationsHistoryTest() {
    GdnRestListResponse<MerchantConfigurationHistoryResponse> response =
        feignFallback.getMerchantConfigurationHistory(MERCHANT_CODE, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getCategoryConfigurationHistoryListTest() {
    GdnRestListResponse<CategoryConfigurationHistoryResponse> responseGdnRestListResponse =
        feignFallback.getCategoryConfigurationHistory(StringUtils.EMPTY, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), responseGdnRestListResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, responseGdnRestListResponse.getErrorMessage());
    assertFalse(responseGdnRestListResponse.isSuccess());
  }

  @Test
  public void getLookupByLookupGroupTest() {
    GdnRestListResponse<LookupResponse> responseGdnRestListResponse =
        feignFallback.getLookupByLookupGroup(StringUtils.EMPTY);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), responseGdnRestListResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, responseGdnRestListResponse.getErrorMessage());
    assertFalse(responseGdnRestListResponse.isSuccess());
  }

  @Test
  public void getWholesaleConfigToCategoryTest(){
    GdnRestSingleResponse<WholesaleMappingResponse> response = feignFallback.getWholesaleConfigToCategory(CATEGORY_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getCategoryNamesTest() {
    GdnRestSingleResponse<CategoryNamesResponse> response =
        feignFallback.getCategoryNames(new CategoryMultipleIdRequest(), 0, 1);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getBrandAuthorisationDetailByCodeTest() {
    GdnRestSingleResponse<BrandAuthorisationDetailResponse> response =
      feignFallback.getBrandAuthorisationDetailByCode(DEFAULT_STORE_ID,DEFAULT_SELLER_CODE,BRAND_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void deleteBrandAuthTest() {
    GdnBaseRestResponse response =
      feignFallback.delete(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequests);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getBrandAuthTest() {
    BrandAuthFilterRequest request = new BrandAuthFilterRequest();
    GdnRestListResponse<BrandAuthFilterResponse> response = feignFallback.getAuthorisations(request, 0, 10);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getBrandAuthHistoryTest() {
    BrandAuthHistoryRequest brandAuthHistoryRequest = new BrandAuthHistoryRequest();
    GdnBaseRestResponse response =
      feignFallback.getBrandAuthHistory(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID,DEFAULT_USERNAME,
        brandAuthHistoryRequest,0,10);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void createBrandAuthorisationTest() {
    BrandAuthCreateRequest brandAuthCreateRequest = new BrandAuthCreateRequest();
    GdnBaseRestResponse response =
      feignFallback.createBrandAuthorisation(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID,DEFAULT_USERNAME,
        brandAuthCreateRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateBrandAuthorisationTest() {
    com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest
        brandAuthUpdateRequest = new com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest();
    GdnBaseRestResponse response =
        feignFallback.updateBrandAuthorisation(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID,DEFAULT_USERNAME,
            brandAuthUpdateRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void republishProductFromPCB() {
    GdnBaseRestResponse response =
        feignFallback.republishProductFromPCB(Collections.singletonList(BRAND_CODE), BRAND_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void clearPCBCache() {
    GdnBaseRestResponse response =
        feignFallback.clearPCBCache(PRODUCT_ID, PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateViewable() {
    GdnBaseRestResponse response =
        feignFallback.updateViewable(PRODUCT_ID, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateReviewPending() {
    GdnBaseRestResponse response =
        feignFallback.updateReviewPending(PRODUCT_ID, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getAttributeValuesByProductCodeAndAttributeCode() {
    GdnRestSingleResponse<SingleObjectResponse> response =
        feignFallback.getAttributeValuesByProductCodeAndAttributeCode("", "");
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void fetchCategoryHistory() {
    GdnRestListResponse<CategoryHistoryResponse> responseGdnRestListResponse =
        feignFallback.fetchCategoryHistory(StringUtils.EMPTY, 0, 0);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), responseGdnRestListResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, responseGdnRestListResponse.getErrorMessage());
    assertFalse(responseGdnRestListResponse.isSuccess());
  }

  @Test
  void fetchBrandAuthWipDetailsTest() {
    GdnRestSingleResponse<BrandAuthWipDetailResponse> responseGdnRestListResponse =
      feignFallback.fetchBrandAuthWipDetails(DEFAULT_STORE_ID, STATUS, DEFAULT_USERNAME);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
      responseGdnRestListResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, responseGdnRestListResponse.getErrorMessage());
    assertFalse(responseGdnRestListResponse.isSuccess());
  }

  @Test
  void validateBrandAuthRequestTest() {
    GdnRestSingleResponse<SimpleBooleanResponse> response =
      feignFallback.validateBrandAuthRequest(DEFAULT_STORE_ID, true, BRAND_CODE, DEFAULT_USERNAME);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }
  @Test
  public void actionBrandAuthorisationWipTest() {
    GdnBaseRestResponse response =
        feignFallback.brandAuthorisationWipAction(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            brandAuthorisationWipActionRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void createBrandAuthWipTest() {
    GdnRestSingleResponse<BrandAuthCreateWipResponse> response =
        feignFallback.createBrandAuthWip(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID,
            brandAuthCreateWipRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateBrandAuthWipTest() {
    GdnBaseRestResponse response =
        feignFallback.brandAuthorisationWipUpdate(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            brandAuthUpdateRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void filterSummaryTest() {
    GdnRestListResponse<BrandAuthorisationWipListResponse> responseGdnRestListResponse =
        feignFallback.filterSummary(PAGE, SIZE, new BrandAuthorisationWipListRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        responseGdnRestListResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, responseGdnRestListResponse.getErrorMessage());
    assertFalse(responseGdnRestListResponse.isSuccess());
  }

  @Test
  public void creationEligibilityTest() {
    GdnRestSingleResponse<SimpleBooleanResponse> response =
        feignFallback.creationEligibility(MERCHANT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void systemParamUpdateTest() {
    SystemParameterRequest systemParameterRequest = new SystemParameterRequest();
    GdnBaseRestResponse updateSystemParameter =
      feignFallback.updateSystemParameter(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_REQUEST_ID,
        DEFAULT_REQUEST_ID, systemParameterRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), updateSystemParameter.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, updateSystemParameter.getErrorMessage());
    assertFalse(updateSystemParameter.isSuccess());
  }
}
