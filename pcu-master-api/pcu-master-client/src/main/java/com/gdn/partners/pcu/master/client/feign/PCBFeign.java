package com.gdn.partners.pcu.master.client.feign;

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
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsSearchRequest;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.margin.webmodel.GdnRestSimpleResponse;
import com.gdn.partners.pcu.master.client.factory.PCBFeignFallbackFactory;
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
import com.gdn.partners.pcu.master.client.model.MasterAttributeRequest;

/**
 * @author Pradeep Reddy
 */
@FeignClient(name = "pcbFeign", url = "${service.pcb.endpoint}", fallbackFactory = PCBFeignFallbackFactory.class)
public interface PCBFeign {

  @RequestMapping(value = "/api/attribute/{id}", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<AttributeResponse> getAttributeDetail(
      @PathVariable("id") String id,
      @RequestParam("allowedAttributeValuesTrim") boolean allowedAttributeValuesTrim);

  @RequestMapping(value = "/api/master-attribute", method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse saveMasterAttribute(@RequestBody MasterAttributeRequest request);

  @RequestMapping(value = "/api/master-attribute/info/{attributeCode}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<MasterAttributeResponse> getAttributeInfo(@PathVariable("attributeCode") String attributeCode);

  @RequestMapping(value = "/api/master-attribute/{attributeCode}/values", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<AttributeValueResponse> getAttributeValues(@PathVariable("attributeCode") String attributeCode,
      @RequestParam("page") Integer page, @RequestParam("size") Integer size,
      @RequestParam("getAllValues") Boolean getAllValues,
      @RequestParam("concatenateValueWithValueType") boolean concatenateValueWithValueType);

  @RequestMapping(value = "/api/master-attribute/filter/list", method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<MasterAttributeResponse> getAttributesByAttributeFilter(
      @RequestBody MasterAttributeFilterRequest masterAttributeFilterRequest,
      @RequestParam("page") Integer page, @RequestParam("size") Integer size);

  @RequestMapping(value = "/api/master-attribute/update-master-attribute", method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateMasterAttribute(@RequestBody MasterAttributeRequest request);

  @RequestMapping(value = "/api/master-attribute/{attributeCode}/values", method = RequestMethod.PUT,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateMasterAttributeValues(
      @PathVariable("attributeCode") String attributeCode, @RequestBody MasterAttributeUpdateRequest request);

  @RequestMapping(value = "/api/master-attribute/{attributeCode}/values", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<AttributeValueResponse> addMasterAttributeValue(
      @PathVariable("attributeCode") String attributeCode, @RequestBody MasterAttributeAddRequest request);

  @RequestMapping(value = "/api/category/{id}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetail(@PathVariable("id") String categoryId);

  @RequestMapping(value = "/api/categoryShipping/filter/categorycode",
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<CategoryShippingResponse> getCategoryShippingByCategoryCode(
      @RequestParam("categoryName") String categoryCode);

  @RequestMapping(value = "/api/category/filter/childParent/catalog/pageablecount", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<CategoryDTO> getChildFromParentByCatalogIdWithChildCount(
      @RequestParam("catalogId") String catalogId, @RequestParam("categoryId") String categoryId,
      @RequestParam("page") Integer page, @RequestParam("size") Integer size,
      @RequestParam("filterType") String filterType, @RequestParam("documentFilterType") String documentFilterType,
      @RequestParam("ignoreB2bExclusive") boolean ignoreB2bExclusive,
      @RequestParam("filterHalalCategory") boolean filterHalalCategory);

  @RequestMapping(value = "/api/category/hierarchy/filter/category-codes", method = RequestMethod.POST, produces = MediaType
      .APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<CategoryHierarchyResponse> getHierarchyByCategoryCodes(
      @RequestBody CategoryCodeRequest categoryCodeRequest);

  @RequestMapping(value = "/api/catalog/filter/type", method = RequestMethod.GET, produces = MediaType
      .APPLICATION_JSON_VALUE)
  GdnRestListResponse<CatalogResponse> getCatalogSummaryByCatalogType(@RequestParam("catalogType") String catalogType,
      @RequestParam("page") Integer page, @RequestParam("size") Integer size);

  @RequestMapping(value = "/api/category/filter/name/pageable", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<CategoryResponse> findCategorySummaryByName(@RequestParam("name") String categoryName,
      @RequestParam("page") Integer page, @RequestParam("size") Integer size, @RequestParam("state") String state,
      @RequestParam("documentFilterType") String documentFilterType);

  @RequestMapping(value = "/api/category/updateInfo", method = RequestMethod.PUT, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  GdnBaseRestResponse updateCategoryInfo(@RequestBody CategoryInfoUpdateRequest request,
      @RequestParam("statusChangeFlag") boolean statusChangeFlag);

  @RequestMapping(value = "/api/category/categoryId/{categoryId}", method = RequestMethod.GET, produces = MediaType
      .APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CategoryDetailAndShippingResponse> getCategoryInfoByCategoryId(
      @RequestParam boolean fetchHideForSellerAttributes,
      @PathVariable("categoryId") String categoryId);

  @RequestMapping(value = "/api/category/{categoryId}/validate-category", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse validateCategory(@PathVariable("categoryId") String categoryId);

  @RequestMapping(value = "/api/category/updateMappings", method = RequestMethod.PUT,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateCategoryMappings(@RequestBody CategoryMappingsUpdateRequest request);

  @RequestMapping(value = "/api/category/createCategory", method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CreateCategoryResponse> createCategory(@RequestBody CategoryDetailRequest categoryDetailRequest);

  @RequestMapping(value = "/api/attribute/filter/attributecodes", method = RequestMethod.POST, produces = MediaType
      .APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<AttributeResponse> getAttributeValuesByAttributeCodes(@RequestBody
      AttributeCodesRequest attributeCodeRequest);

  @RequestMapping(value = "api/predefinedAllowedAttributeValue/filter"
      + "/getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable", method = RequestMethod.GET, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(
      @RequestParam("attributeId") String AttributeId, @RequestParam("value") String value, @RequestParam("page")
      int page, @RequestParam("size") int size);

  @RequestMapping(value = "api/predefinedAllowedAttributeValue/filter"
      + "/getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValue", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValue(
      @RequestParam("attributeId") String AttributeId, @RequestParam("value") String value);

  @RequestMapping(value = "/api/category/filter/childParent/pageable/{id}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<CategoryResponse> findCategorySummaryByParentId(@RequestParam("size") Integer size,
      @PathVariable("id") String id);

  @RequestMapping(value = "/api/attribute/attributeCode/{attributeCode}/detail", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<AttributeResponse> getAttributeDetailAndValuesByAttributeCode(
      @PathVariable("attributeCode") String attributeCode);

  @RequestMapping(value = "/api/category/{categoryCode}/restricted-keywords",
      method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateCategoriesWithRestrictedKeywords(@PathVariable("categoryCode") String categoryCode,
      @RequestBody CategoryKeywordUpdateRequestList request);

  @RequestMapping(value = "/api/category/get-restricted-keywords", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<RestrictedKeywordsResponse> getCategoryRestrictedKeywords(@RequestParam("page") Integer page,
      @RequestParam("size") Integer size, @RequestBody CategoryRestrictedKeywordsRequest request);

  @RequestMapping(value = "/api/restricted-keywords/{keywordId}/history", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<RestrictedKeywordHistoryResponse> getRestrictedKeywordHistory(
      @PathVariable("keywordId") String keywordId, @RequestParam("page") Integer page,
      @RequestParam("size") Integer size);

  @RequestMapping(value = "/api/restricted-keywords/search", method =
    RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<RestrictedKeywordsResponse> getRestrictedKeywords(
    @RequestBody RestrictedKeywordsSearchRequest keywordsSearchRequest, @RequestParam("page") Integer page,
    @RequestParam("size") Integer size);

  @RequestMapping(value = "/api/restricted-keywords/ui-validation-list", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<UiValidationRestrictedKeywordsResponse> getUiValidationRestrictedKeywords();

  @RequestMapping(value = "/api/restricted-keywords/listing", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<RestrictedKeywordsListingResponse> getRestrictedKeywordForListing(
      @RequestParam("keyword") String keyword, @RequestParam("page") Integer page, @RequestParam("size") Integer size);

  @RequestMapping(value = "/api/restricted-keywords/update-restricted-keywords", method = RequestMethod.PUT, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateRestrictedKeyword(
      @RequestBody RestrictedKeywordsUpdateRequest restrictedKeywordsUpdateRequest);

  @RequestMapping(value = "/api/category/get-wholesale-config", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<WholesaleMappingResponse> getCategoryWholesaleConfig(@RequestParam("categoryId") String categoryId,
      @RequestParam("categoryCode") String categoryCode);

  @RequestMapping(value = "/api/category/{categoryId}/wholesale-config",
      method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateCategoriesWithWholesaleConfig(@PathVariable("categoryId") String categoryId,
      @RequestBody WholesaleMappingRequest request);

  @RequestMapping(value = "/api/category/getGenericTemplateCategories", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<CategoryTreeResponse> getCategoryTree(
      @RequestParam("genericTemplateEligible") boolean genericTemplateEligible);

  @RequestMapping(value = "/api/systemParameter/find", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<SystemParameterResponse> getDocumentList(@RequestParam("variable") String variable);

  @RequestMapping(value = "/api/category/create-original-sales-category", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<String> createOriginalSalesCategory(@RequestBody OriginalSalesCategoryRequest request);

  @RequestMapping(value = "/api/category/fetch-osc-list", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<OscSummaryResponse> filterOscSummaryResponse(@RequestParam("oscCode") String oscCode,
      @RequestParam("keyword") String keyword, @RequestParam("activated") Boolean activated);

  @RequestMapping(value = "/api/category/update-osc-list", method = RequestMethod.PUT,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateOriginalSalesCategory(@RequestBody OscInfoUpdateDTO request);

  @RequestMapping(value = "/api/category/{id}/original-sales-category", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<OriginalSalesCategoryResponse> getOSCById(@PathVariable("id") String id);

  @RequestMapping(value = "/api/category/hierarchy/filter/category-code/{id}", method = RequestMethod.GET)
  GdnRestListResponse<CategoryResponse> filterCategoryHierarchyByCategoryCode(
      @PathVariable("id") String categoryCode);

  @GetMapping(value = "/api/dimensions/{dimensionCode}/detail")
  GdnRestSingleResponse<DimensionResponse> fetchDimensionDetail(
    @PathVariable("dimensionCode") String dimensionCode);

  @PostMapping(value = "/api/dimensions/filter")
  GdnRestListResponse<DimensionResponse> fetchDimensionListing(
    @RequestParam(value = "page", defaultValue = "0") int page,
    @RequestParam(value = "size", defaultValue = "10") int size,
    @RequestBody DimensionFilterRequest request);

  @PostMapping(value = "/api/dimensions/save")
  GdnBaseRestResponse createDimension(@RequestBody DimensionRequest request);

  @GetMapping(value = "/api/dimensions/{attributeCode}/dimension-mapping")
  GdnRestListResponse<DimensionMappingResponse> fetchDimensionMapping(
      @PathVariable("attributeCode") String attributeCode,
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "50") int size);

  @PostMapping(value = "/api/dimensions/{attributeCode}/update-dimension-mapping")
  GdnBaseRestResponse modifyDimensionMapping(@PathVariable("attributeCode") String attributeCode,
      @RequestBody ModifyDimensionMappingRequest modifyDimensionMappingRequest);

  @PostMapping("/api/size-chart/upsert")
  GdnBaseRestResponse upsertSizeChart(@RequestBody SizeChartRequest sizeChartRequest);

  @GetMapping("/api/size-chart/{sizeChartCode}/detail")
  GdnRestSingleResponse<SizeChartResponse> fetchSizeChartDetails(
    @PathVariable("sizeChartCode") String sizeChartCode,
    @RequestParam(required = false) boolean preview);

  @PostMapping(value = "/api/dimensions/edit")
  GdnBaseRestResponse editDimension(@RequestBody DimensionRequest request);

  @PostMapping(value = "/api/size-chart/{sizeChartCode}/update-size-chart-status")
  GdnBaseRestResponse updateSizeChartStatus(@PathVariable("sizeChartCode") String sizeChartCode,
      @RequestParam(required = false) boolean waitingDeletion,
      @RequestParam(required = false) boolean markForDelete,
      @RequestParam String businessPartnerCode);

  @GetMapping(value = "/api/dimensions/findByName")
  GdnRestSingleResponse<DimensionResponse> findByName(@RequestParam String dimensionName);

  @PostMapping(value = "/api/size-chart/filter")
  GdnRestListResponse<SizeChartFilterResponse> filter(
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "10") int size,
      @RequestBody SizeChartFilterRequest request);

  @GetMapping(value = "api/size-chart/findByNameAndBusinessPartnerCode")
  GdnRestSingleResponse<SizeChartResponse> findBySizeChartNameAndBusinessPartnerCode(
      @RequestParam String sizeChartName, @RequestParam String businessPartnerCode);

  @GetMapping(value = "api/size-chart/validCategory")
  GdnRestSingleResponse<SimpleBooleanResponse> validateCategoryCode(
      @RequestParam String categoryCode, @RequestParam String sizeChartCode);
}
