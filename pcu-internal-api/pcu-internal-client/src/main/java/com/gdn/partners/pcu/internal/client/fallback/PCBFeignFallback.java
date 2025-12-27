package com.gdn.partners.pcu.internal.client.fallback;

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
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryResponse;
import com.gdn.x.productcategorybase.dto.request.SystemParameterRequest;
import com.gdn.x.productcategorybase.dto.response.BrandAuthorisationDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
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
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandlogoPath;
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
import org.springframework.web.bind.annotation.RequestParam;

/**
 * @author Pradeep Reddy
 */
@Component
public class PCBFeignFallback implements PCBFeign {

  @Override
  public GdnRestListResponse<CategoryResponse> filterCategoryHierarchyByCategoryCode(String categoryCode){
    return new GdnRestListResponse<CategoryResponse>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<CategoryParentResponse> getCategoriesAndFinalCategoryMapping() {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<SingleObjectResponse> getFinalParentCategory(String categoryId) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetail(String categoryId) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse deleteBrand(String brandCode, String brandDeletedReason) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestSingleResponse<BrandResponse> getBrandDetail(String brandCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<BrandWipResponse> getBrandWipDetail(String brandRequestCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<BrandWipHistoryResponse> getBrandWipHistory(
      BrandWipHistorySummaryRequest brandWipHistorySummaryRequest, int page, int size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<BrandWipResponse> getBrandWipList(BrandWipSummaryRequest brandWipSummaryRequest, int page,
      int size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<BrandRejectionInfoResponse> getBrandRejectionReasonByBrandRequestCode(
      String brandRequestCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<CreateBrandResponse> approveBrand(String brandRequestCode, BrandApproveRequest request) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<BrandWipResponse> rejectBrand(String brandRequestCode, BrandRejectRequest request) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<BrandResponse> filterByBrandCode(String brandCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<BrandWipResponse> filterByBrandRequestCode(String brandRequestCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<BrandWipResponse> getBrandWipByBrandCode(String brandCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<UpdateBrandlogoPath> updateBrand(UpdateBrandRequest request) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse update(BrandApproveRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestSingleResponse<ProductDetailResponse> filterProductDetailByProductCode(String productCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
       ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<MasterAttributeResponse> getAttributeByAttributeCode(String attributeCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<MerchantSearchResponse> fetchMerchantSearchResult(
      List<MerchantConfigurationRequest> merchantConfigurationRequestList) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnBaseRestResponse addCategoryConfigurationStatus(List<CategoryConfigurationRequest> requests) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnBaseRestResponse updateCategoryConfigurationStatus(CategoryConfigurationRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnBaseRestResponse deleteCategoryConfigurationStatus(String categoryCode) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnBaseRestResponse addMerchantConfigurationStatus(List<MerchantConfigurationRequest> requests) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnBaseRestResponse updateMerchantConfigurationStatus(MerchantConfigurationRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnBaseRestResponse deleteMerchantConfigurationStatus(String merchantCode) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }
  public GdnRestListResponse<CategoryTreeNodeResponse> getCategoryTreeWithReviewConfig() {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<ConfigurationStatusResponse> getConfigurationsStatusByMerchantAndCategoryCode(
      List<ConfigurationStatusRequest> configurationStatusRequestList) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  public GdnRestSingleResponse<ConfigurationCountResponse> fetchConfigurationCounts() {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<CategoryConfigurationFilterResponse> getCategoryConfigurationList(
      ConfigurationFilterRequest categoryConfigurationFilterRequest, int page, int size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<MerchantConfigurationFilterResponse> getMerchantConfigurationList(
      ConfigurationFilterRequest merchantConfigurationFilterRequest, int page, int size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<MerchantConfigurationHistoryResponse> getMerchantConfigurationHistory(String merchantCode,
      int page, int size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<CategoryConfigurationHistoryResponse> getCategoryConfigurationHistory(String categoryCode,
      int page, int size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<LookupResponse> getLookupByLookupGroup(String lookupGroup) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<WholesaleMappingResponse> getWholesaleConfigToCategory(String categoryId) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<CategoryNamesResponse> getCategoryNames(CategoryMultipleIdRequest categoryCodes,
      int page, int size) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<BrandAuthorisationDetailResponse> getBrandAuthorisationDetailByCode(
    String storeId, String sellerCode, String brandCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<BrandAuthCreateResponse> createBrandAuthorisation(String storeId,
    String requestId, String username, BrandAuthCreateRequest request) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse updateBrandAuthorisation(String storeId, String requestId, String username,
      com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestListResponse<BrandAuthHistoryResponse> getBrandAuthHistory(String storeId,
    String requestId, String username, BrandAuthHistoryRequest brandAuthHistoryRequest, int page,
    int size) {
    return new GdnRestListResponse<BrandAuthHistoryResponse>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }


  @Override
  public GdnBaseRestResponse delete(String storeId, String username,
    List<BrandAuthDeleteRequest> brandAuthDeleteRequestList) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductDetailResponse> filterProductDetailByProductCodeWithOriginalImages(
      String productCode, String categoryCode, boolean originalImages) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<BrandAuthFilterResponse> getAuthorisations(BrandAuthFilterRequest brandAuthFilterRequest,
      int page, int size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnBaseRestResponse republishProductFromPCB(List<String> productCodes, String operationType) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse clearPCBCache(String productId, String productCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse updateViewable(String productCode, boolean viewable) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse updateReviewPending(String productCode, boolean reviewPending) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<SingleObjectResponse> getAttributeValuesByProductCodeAndAttributeCode(String productCode,
      String attributeCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<CategoryHistoryResponse> fetchCategoryHistory(String categoryCode,
      int page, int size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<BrandAuthWipDetailResponse> fetchBrandAuthWipDetails(
    String storeId, String status, String id) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<SimpleBooleanResponse> validateBrandAuthRequest(String storeId,
    boolean edited, String brandCode, String sellerCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse brandAuthorisationWipAction(String storeId, String username,
      BrandAuthorisationWipActionRequest brandAuthorisationWipActionRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestSingleResponse<BrandAuthCreateWipResponse> createBrandAuthWip(String storeId,
      String username, String requestId, BrandAuthCreateWipRequest brandAuthCreateWipRequest) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse brandAuthorisationWipUpdate(String storeId, String username,
      BrandAuthUpdateRequest brandAuthUpdateRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestListResponse<BrandAuthorisationWipListResponse> filterSummary(int page, int size,
      BrandAuthorisationWipListRequest brandAuthorisationWipListRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);

  }

  @Override
  public GdnRestSingleResponse<SimpleBooleanResponse> creationEligibility(
      @RequestParam String sellerCode) {
    return new GdnRestSingleResponse<SimpleBooleanResponse>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse updateSystemParameter(String storeId, String requestId,
    String channelId, String clientId,
    @RequestParam SystemParameterRequest systemParameterRequest) {
    return new GdnRestSingleResponse<SimpleBooleanResponse>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }
}

