package com.gdn.partners.pcu.master.client.fallback;

import com.gdn.partners.pcu.master.client.model.AttributeResponse;
import com.gdn.partners.pcu.master.client.model.CategoryDetailAndShippingResponse;
import com.gdn.partners.pcu.master.client.model.DimensionFilterRequest;
import com.gdn.partners.pcu.master.client.model.DimensionRequest;
import com.gdn.partners.pcu.master.client.model.DimensionMappingResponse;
import com.gdn.partners.pcu.master.client.model.DimensionResponse;
import com.gdn.partners.pcu.master.client.model.ModifyDimensionMappingRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartFilterRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartFilterResponse;
import com.gdn.partners.pcu.master.client.model.SizeChartRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartResponse;
import com.gdn.partners.pcu.master.client.model.MasterAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsSearchRequest;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
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
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleMappingRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.CreateCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.OriginalSalesCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.OscSummaryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsListingResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import com.gdn.x.productcategorybase.dto.response.UiValidationRestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;

/**
 * @author Pradeep Reddy
 */
@Component
public class PCBFeignFallback implements PCBFeign {

  private static PageMetaData pageMetaData = new PageMetaData(0, 0, 0);

  @Override
  public GdnRestSingleResponse<AttributeResponse> getAttributeDetail(
      @PathVariable("id") String id,
      @RequestParam(required = false) boolean allowedAttributeValuesTrim) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<AttributeValueResponse> getAttributeValues(
      @PathVariable("attributeCode") String attributeCode, @RequestParam("page") Integer page,
      @RequestParam("size") Integer size, @RequestParam("getAllValues") Boolean getAllValues,
      @RequestParam("concatenateValueWithValueType") boolean concatenateValueWithValueType) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<MasterAttributeResponse> getAttributesByAttributeFilter(
      @RequestBody MasterAttributeFilterRequest masterAttributeFilterRequest,
      @RequestParam("page") Integer page, @RequestParam("size") Integer size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnBaseRestResponse saveMasterAttribute(@RequestBody MasterAttributeRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), Boolean.FALSE, null);
  }

  @Override
  public GdnRestSingleResponse<MasterAttributeResponse> getAttributeInfo(
      @PathVariable("attributeCode") String attributeCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse updateMasterAttribute(MasterAttributeRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), Boolean.FALSE, null);
  }

  @Override
  public GdnRestListResponse<CatalogResponse> getCatalogSummaryByCatalogType(String catalogType, Integer page,
      Integer size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<CategoryHierarchyResponse> getHierarchyByCategoryCodes(
      CategoryCodeRequest categoryCodeRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<CategoryDTO> getChildFromParentByCatalogIdWithChildCount(String catalogId,
      String categoryId, Integer page, Integer size, String filterType, String documentFilterType,
      boolean ignoreB2bExclusive, boolean filterHalalCategory) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnBaseRestResponse updateMasterAttributeValues(String attributeCode,
      MasterAttributeUpdateRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), Boolean.FALSE, null);
  }

  @Override
  public GdnRestSingleResponse<AttributeValueResponse> addMasterAttributeValue(String attributeCode,
      MasterAttributeAddRequest request) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetail(String categoryId) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<CategoryShippingResponse> getCategoryShippingByCategoryCode(String categoryCode) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestSingleResponse<CategoryDetailAndShippingResponse> getCategoryInfoByCategoryId(
      boolean fetchHideForSellerAttributes, String categoryId) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), Boolean.FALSE, null, null);
  }

  @Override
  public GdnBaseRestResponse validateCategory(String categoryId) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), Boolean.FALSE, null, null);
  }

  @Override
  public GdnRestListResponse<CategoryResponse> findCategorySummaryByName(String categoryName, Integer page,
      Integer size, String state, String documentFilterType) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnBaseRestResponse updateCategoryInfo(CategoryInfoUpdateRequest request, boolean categoryStatus) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), Boolean.FALSE, null);
  }

  @Override
  public GdnRestSingleResponse<CreateCategoryResponse> createCategory(CategoryDetailRequest categoryDetailRequest){
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), Boolean.FALSE, null, null);
  }

  @Override
  public GdnBaseRestResponse updateCategoryMappings(CategoryMappingsUpdateRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), Boolean.FALSE, null);
  }

  @Override
  public GdnRestListResponse<AttributeResponse> getAttributeValuesByAttributeCodes(
      AttributeCodesRequest attributeCodeRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(
      String AttributeId, String value, int page, int size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValue(
      String AttributeId, String value) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<CategoryResponse> findCategorySummaryByParentId(Integer size, String parentId) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<AttributeResponse> getAttributeDetailAndValuesByAttributeCode(String attributeCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse updateCategoriesWithRestrictedKeywords(String categoryCode,
      CategoryKeywordUpdateRequestList request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        Boolean.FALSE, null);
  }

  @Override
  public GdnRestListResponse<RestrictedKeywordsResponse> getCategoryRestrictedKeywords(Integer page, Integer size,
      CategoryRestrictedKeywordsRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<RestrictedKeywordHistoryResponse> getRestrictedKeywordHistory(String keywordId,
      Integer page, Integer size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<UiValidationRestrictedKeywordsResponse> getUiValidationRestrictedKeywords() {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<RestrictedKeywordsListingResponse> getRestrictedKeywordForListing(String keyword,
      Integer page, Integer size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnBaseRestResponse updateRestrictedKeyword(RestrictedKeywordsUpdateRequest restrictedKeywordsUpdateRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        Boolean.FALSE, null);
  }

  @Override
  public GdnRestListResponse<RestrictedKeywordsResponse> getRestrictedKeywords(
    RestrictedKeywordsSearchRequest keywordsSearchRequest,
    Integer page, Integer size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestSingleResponse<WholesaleMappingResponse> getCategoryWholesaleConfig(String categoryId,
      String categoryCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse updateCategoriesWithWholesaleConfig(String categoryId, WholesaleMappingRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        Boolean.FALSE, null);
  }

  @Override
  public GdnRestListResponse<CategoryTreeResponse> getCategoryTree(boolean genericTemplateEligible) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestSingleResponse<SystemParameterResponse> getDocumentList(String variable) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSimpleResponse<String> createOriginalSalesCategory(OriginalSalesCategoryRequest request) {
    return new GdnRestSimpleResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<OscSummaryResponse> filterOscSummaryResponse(String oscCode, String keyword,
      Boolean activated) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnBaseRestResponse updateOriginalSalesCategory(OscInfoUpdateDTO request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        Boolean.FALSE, null);
  }

  @Override
  public GdnRestSingleResponse<OriginalSalesCategoryResponse> getOSCById(String id) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<CategoryResponse> filterCategoryHierarchyByCategoryCode(String categoryCode) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestSingleResponse<DimensionResponse> fetchDimensionDetail(String dimensionCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<DimensionResponse> fetchDimensionListing(int page, int size,
    DimensionFilterRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnBaseRestResponse createDimension(DimensionRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestListResponse<DimensionMappingResponse> fetchDimensionMapping(String attributeCode,
      int page, int size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnBaseRestResponse modifyDimensionMapping(String attributeCode,
      ModifyDimensionMappingRequest modifyDimensionMappingRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse upsertSizeChart(SizeChartRequest sizeChartRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestSingleResponse<SizeChartResponse> fetchSizeChartDetails(String sizeChartCode,
    boolean preview) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public  GdnBaseRestResponse editDimension(DimensionRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage() , false, null);
  }

  @Override
  public GdnBaseRestResponse updateSizeChartStatus(String sizeChartCode, boolean waitingDeletion,
      boolean markForDelete,
      String businessPartnerCode) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage() , false, null);
  }

  @Override
  public GdnRestSingleResponse<DimensionResponse> findByName(@RequestParam String dimensionName) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<SizeChartFilterResponse> filter(int page, int size,
      SizeChartFilterRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestSingleResponse<SizeChartResponse> findBySizeChartNameAndBusinessPartnerCode(
      String sizeChartName, String businessPartnerCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<SimpleBooleanResponse> validateCategoryCode(String categoryCode, String sizeChartCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }
}

