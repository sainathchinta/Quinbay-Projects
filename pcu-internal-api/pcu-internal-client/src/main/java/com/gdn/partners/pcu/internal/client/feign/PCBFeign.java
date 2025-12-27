package com.gdn.partners.pcu.internal.client.feign;

import java.util.List;

import com.gdn.partners.pcu.internal.client.model.request.BrandAuthDeleteRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthUpdateRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthorisationWipListRequest;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthWipDetailResponse;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthCreateWipResponse;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthCreateWipRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthorisationWipActionRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthFilterRequest;
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
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.factory.PCBFeignFallbackFactory;
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

/**
 * @author Pradeep Reddy
 */
@FeignClient(name = "pcbFeign", url = "${service.pcb.endpoint}", fallbackFactory = PCBFeignFallbackFactory.class)
public interface PCBFeign {

  @RequestMapping(value = "/api/category/hierarchy/filter/category-code/{id}", method = RequestMethod.GET)
  GdnRestListResponse<CategoryResponse> filterCategoryHierarchyByCategoryCode(
      @PathVariable("id") String categoryCode);

  @RequestMapping(value = "/api/category/parent-category-mapping", method = RequestMethod.GET)
  GdnRestListResponse<CategoryParentResponse> getCategoriesAndFinalCategoryMapping();

  @RequestMapping(value = "/api/category/final-parent-category", method = RequestMethod.GET)
  GdnRestSingleResponse<SingleObjectResponse> getFinalParentCategory(@RequestParam("categoryId") String categoryId);

  @RequestMapping(value = "/api/category/{id}", method = RequestMethod.GET)
  GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetail(@PathVariable("id") String categoryId);

  @RequestMapping(value = "/api/brand/delete", method = RequestMethod.GET)
  GdnBaseRestResponse deleteBrand(
      @RequestParam("brandCode") String brandCode, @RequestParam("brandDeletedReason") String brandDeletedReason);

  @RequestMapping(value = "/api/brand/filter/brand-code", method = RequestMethod.GET)
  GdnRestSingleResponse<BrandResponse> getBrandDetail(@RequestParam("brandCode") String brandCode);

  @RequestMapping(value = "/api/brand-wip/detail", method = RequestMethod.GET)
  GdnRestSingleResponse<BrandWipResponse> getBrandWipDetail(@RequestParam("brandRequestCode") String brandRequestCode);

  @RequestMapping(value = "/api/brand-wip/history/filter/summary", method = RequestMethod.POST)
  GdnRestListResponse<BrandWipHistoryResponse> getBrandWipHistory(
      @RequestBody BrandWipHistorySummaryRequest brandWipHistorySummaryRequest,
      @RequestParam("page") int page, @RequestParam("size") int size);

  @RequestMapping(value = "api/brand-wip/filter/summary", method = RequestMethod.POST)
  GdnRestListResponse<BrandWipResponse> getBrandWipList(@RequestBody BrandWipSummaryRequest brandWipSummaryRequest,
      @RequestParam("page") int page, @RequestParam("size") int size);

  @RequestMapping(value = "/api/brand-wip/{brandRequestCode}/rejectedReason", method = RequestMethod.GET)
  GdnRestSingleResponse<BrandRejectionInfoResponse> getBrandRejectionReasonByBrandRequestCode(
      @PathVariable("brandRequestCode") String brandRequestCode);

  @RequestMapping(value = "api/brand-wip/update", method = RequestMethod.POST)
  GdnBaseRestResponse update(@RequestBody BrandApproveRequest request);

  @RequestMapping(value = "/api/brand-wip/approve", method = RequestMethod.POST)
  GdnRestSingleResponse<CreateBrandResponse> approveBrand(@RequestParam("brandRequestCode") String brandRequestCode,
      @RequestBody BrandApproveRequest request);

  @RequestMapping(value = "/api/brand-wip/reject", method = RequestMethod.POST)
  GdnRestSingleResponse<BrandWipResponse> rejectBrand(@RequestParam("brandRequestCode") String brandRequestCode,
      @RequestBody BrandRejectRequest request);

  @RequestMapping(value = "/api/brand/filter/brand-code", method = RequestMethod.GET)
  GdnRestSingleResponse<BrandResponse> filterByBrandCode(@RequestParam("brandCode") String brandCode);

  @RequestMapping(value = "/api/brand-wip/filter/brand-request-code", method = RequestMethod.GET)
  GdnRestSingleResponse<BrandWipResponse> filterByBrandRequestCode(
      @RequestParam("brandRequestCode") String brandRequestCode);

  @RequestMapping(value = "/api/brand-wip/detailByBrandCode", method = RequestMethod.GET)
  GdnRestSingleResponse<BrandWipResponse> getBrandWipByBrandCode(@RequestParam("brandCode") String brandCode);

  @RequestMapping(value = "/api/brand/update", method = RequestMethod.POST)
  GdnRestSingleResponse<UpdateBrandlogoPath> updateBrand(@RequestBody UpdateBrandRequest request);

  @RequestMapping(value = "/api/product/productCode/{productCode}", method = RequestMethod.GET)
  GdnRestSingleResponse<ProductDetailResponse>filterProductDetailByProductCode(@RequestParam("productCode") String productCode);

  @RequestMapping(value = "/api/product/productCode/{productCode}/categoryCode/{categoryCode}", method =
      RequestMethod.GET)
  GdnRestSingleResponse<ProductDetailResponse> filterProductDetailByProductCodeWithOriginalImages(
      @PathVariable("productCode") String productCode, @PathVariable("categoryCode") String categoryCode,
      @RequestParam("originalImages") boolean originalImages);

  @RequestMapping(value = "/api/master-attribute/detail", method = RequestMethod.GET)
  GdnRestSingleResponse<MasterAttributeResponse> getAttributeByAttributeCode(
      @RequestParam("attributeCode") String attributeCode);

  @RequestMapping(value = "/api/postlive-config/fetchMerchantConfig", method = RequestMethod.POST)
  GdnRestListResponse<MerchantSearchResponse> fetchMerchantSearchResult(
      @RequestBody List<MerchantConfigurationRequest> merchantConfigurationRequestList);

  @RequestMapping(value = "/api/postlive-config/categories", method = RequestMethod.POST)
  GdnBaseRestResponse addCategoryConfigurationStatus(@RequestBody List<CategoryConfigurationRequest> requests);

  @RequestMapping(value = "/api/postlive-config/categories", method = RequestMethod.PUT)
  GdnBaseRestResponse updateCategoryConfigurationStatus(@RequestBody CategoryConfigurationRequest request);

  @RequestMapping(value = "/api/postlive-config/categories/{categoryCode}", method = RequestMethod.DELETE)
  GdnBaseRestResponse deleteCategoryConfigurationStatus(@PathVariable("categoryCode") String categoryCode);

  @RequestMapping(value = "/api/postlive-config/merchants", method = RequestMethod.POST)
  GdnBaseRestResponse addMerchantConfigurationStatus(@RequestBody List<MerchantConfigurationRequest> requests);

  @RequestMapping(value = "/api/postlive-config/merchants", method = RequestMethod.PUT)
  GdnBaseRestResponse updateMerchantConfigurationStatus(@RequestBody MerchantConfigurationRequest request);

  @RequestMapping(value = "/api/postlive-config/merchants/{merchantCode}", method = RequestMethod.DELETE)
  GdnBaseRestResponse deleteMerchantConfigurationStatus(@PathVariable("merchantCode") String merchantCode);

  @RequestMapping(value = "/api/category/getCategoryTreeWithReviewConfig", method = RequestMethod.GET)
  GdnRestListResponse<CategoryTreeNodeResponse> getCategoryTreeWithReviewConfig();

  @RequestMapping(value = "/api/postlive-config/get-configuration-status", method = RequestMethod.POST)
  GdnRestListResponse<ConfigurationStatusResponse> getConfigurationsStatusByMerchantAndCategoryCode(
      @RequestBody List<ConfigurationStatusRequest> requests);

  @RequestMapping(value = "/api/postlive-config/fetchConfigurationCounts", method = RequestMethod.GET)
  GdnRestSingleResponse<ConfigurationCountResponse> fetchConfigurationCounts();

  @RequestMapping(value = "/api/postlive-config/getCategoryConfigurationList", method = RequestMethod.POST)
  GdnRestListResponse<CategoryConfigurationFilterResponse> getCategoryConfigurationList(
      @RequestBody ConfigurationFilterRequest categoryConfigurationFilterRequest, @RequestParam("page") int page,
      @RequestParam("size") int size);

  @RequestMapping(value = "/api/postlive-config/getMerchantConfigurationList", method = RequestMethod.POST)
  GdnRestListResponse<MerchantConfigurationFilterResponse> getMerchantConfigurationList(
      @RequestBody ConfigurationFilterRequest merchantConfigurationFilterRequest, @RequestParam("page") int page,
      @RequestParam("size") int size);

  @RequestMapping(value = "/api/postlive-config/getMerchantConfigurationHistory/{merchantCode}", method = RequestMethod.GET)
  GdnRestListResponse<MerchantConfigurationHistoryResponse> getMerchantConfigurationHistory(
      @PathVariable("merchantCode") String merchantCode, @RequestParam("page") int page,
      @RequestParam("size") int size);

  @RequestMapping(value = "/api/postlive-config/getCategoryConfigurationHistory/{categoryCode}", method = RequestMethod.GET)
  GdnRestListResponse<CategoryConfigurationHistoryResponse> getCategoryConfigurationHistory(
      @PathVariable("categoryCode") String categoryCode, @RequestParam("page") int page,
      @RequestParam("size") int size);

  @RequestMapping(value = "/api/lookup/lookupGroup", method = RequestMethod.GET)
  GdnRestListResponse<LookupResponse> getLookupByLookupGroup(@RequestParam("lookupGroup") String lookupGroup);

  @RequestMapping(value = "/api/category/get-wholesale-config", method = RequestMethod.GET)
  GdnRestSingleResponse<WholesaleMappingResponse> getWholesaleConfigToCategory(
      @RequestParam("categoryId") String categoryId);

  @RequestMapping(value = "/api/category/getCategoryNames", method = RequestMethod.POST)
  GdnRestSingleResponse<CategoryNamesResponse> getCategoryNames(@RequestBody CategoryMultipleIdRequest categoryCodes,
      @RequestParam("page") int page, @RequestParam("size") int size);

  @RequestMapping(value = "/api/brandAuthorise/detail/{brandCode}", method = RequestMethod.GET)
  GdnRestSingleResponse<BrandAuthorisationDetailResponse> getBrandAuthorisationDetailByCode(
    @RequestParam("storeId") String storeId, @RequestParam("sellerCode") String sellerCode,
    @RequestParam("brandCode") String brandCode);

  @RequestMapping(value = "/api/brandAuthorise/delete", method = RequestMethod.POST)
  GdnBaseRestResponse delete(@RequestParam("storeId") String storeId,
    @RequestParam("username") String username,
    @RequestBody List<BrandAuthDeleteRequest> brandAuthDeleteRequestList);

  @RequestMapping(value = "/api/brandAuthorise/getAuthorisations", method = RequestMethod.POST)
  GdnRestListResponse<BrandAuthFilterResponse> getAuthorisations(
      @RequestBody BrandAuthFilterRequest brandAuthFilterRequest, @RequestParam("page") int page,
  @RequestParam("size") int size);

  @RequestMapping(value = "/api/brandAuthorise/create", method = RequestMethod.POST)
  GdnRestSingleResponse<BrandAuthCreateResponse> createBrandAuthorisation(
    @RequestParam("storeId") String storeId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestBody BrandAuthCreateRequest request);

  @RequestMapping(value = "/api/brandAuthorise/update", method = RequestMethod.POST)
  GdnBaseRestResponse updateBrandAuthorisation(@RequestParam("storeId") String storeId,
      @RequestParam("requestId") String requestId, @RequestParam("username") String username,
      @RequestBody com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest request);

  @RequestMapping(value = "/api/brandAuthorise/history/filter/summary", method = RequestMethod.POST)
  GdnRestListResponse<BrandAuthHistoryResponse> getBrandAuthHistory(
    @RequestParam("storeId") String storeId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username,
    @RequestBody BrandAuthHistoryRequest brandAuthHistoryRequest, @RequestParam("page") int page,
    @RequestParam("size") int size);

  @RequestMapping(value = "/api/product/republishProduct", method = RequestMethod.POST)
  GdnBaseRestResponse republishProductFromPCB(@RequestBody List<String> productCodes,
      @RequestParam("operationType") String operationType);

  @RequestMapping(value = "/api/product/clearProductCacheSync", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse clearPCBCache(@RequestParam("productId") String productId,
      @RequestParam("productCode") String productCode);

  @RequestMapping(value = "/api/product/update-viewable", method = RequestMethod.GET)
  GdnBaseRestResponse updateViewable(@RequestParam("productCode") String productCode,
      @RequestParam("viewable") boolean viewable);

  @RequestMapping(value = "/api/product/{productCode}/update-review-pending", method = RequestMethod.PUT)
  GdnBaseRestResponse updateReviewPending(@PathVariable("productCode") String productCode,
      @RequestParam("reviewPending") boolean reviewPending);

  @RequestMapping(value = "/api/attribute/getAttributeValuesByProductCodeAndAttributeCode", method = RequestMethod.GET)
  public GdnRestSingleResponse<SingleObjectResponse> getAttributeValuesByProductCodeAndAttributeCode(
      @RequestParam(required = true) String productCode, @RequestParam(required = true) String attributeCode);

  @RequestMapping(value = "/api/category/fetchHistoryByCategoryCode/{categoryCode}", method =
      RequestMethod.GET)
  GdnRestListResponse<CategoryHistoryResponse> fetchCategoryHistory(
      @PathVariable("categoryCode") String categoryCode, @RequestParam("page") int page,
      @RequestParam("size") int size);

  @GetMapping(value = "/api/brand-authorisation-wip/details")
  GdnRestSingleResponse<BrandAuthWipDetailResponse> fetchBrandAuthWipDetails(
      @RequestParam("storeId") String storeId, @RequestParam("status") String status,
      @RequestParam("id") String id);

    @PostMapping(value = "/api/brand-authorisation-wip/create-brand-auth")
    GdnRestSingleResponse<BrandAuthCreateWipResponse> createBrandAuthWip(
        @RequestParam("storeId") String storeId, @RequestParam("requestId") String requestId,  
    @RequestParam("username") String username, @RequestBody BrandAuthCreateWipRequest request);

  @GetMapping(value = "/api/brand-authorisation-wip/{brandCode}/{sellerCode}/validate-brand-auth-request")
  GdnRestSingleResponse<SimpleBooleanResponse> validateBrandAuthRequest(
    @RequestParam("storeId") String storeId, @RequestParam("edited") boolean edited,
    @PathVariable("brandCode") String brandCode, @PathVariable("sellerCode") String sellerCode);

  @PostMapping(value = "/api/brand-authorisation-wip/approve")
  GdnBaseRestResponse brandAuthorisationWipAction(@RequestParam("storeId") String storeId,
      @RequestParam("username") String username,
      @RequestBody BrandAuthorisationWipActionRequest brandAuthorisationWipActionRequest);

  @PostMapping(value = "/api/brand-authorisation-wip/submit-authorisation-request")
  GdnBaseRestResponse brandAuthorisationWipUpdate(@RequestParam("storeId") String storeId,
      @RequestParam("username") String username,
      @RequestBody BrandAuthUpdateRequest brandAuthUpdateRequest);

  @PostMapping(value = "/api/brand-authorisation-wip/filter/summary")
  GdnRestListResponse<BrandAuthorisationWipListResponse> filterSummary(@RequestParam("page") int page,
      @RequestParam("size") int size, @RequestBody BrandAuthorisationWipListRequest brandAuthorisationWipListRequest);

  @GetMapping(value = "/api/brand-authorisation-wip/creation-eligibility")
  GdnRestSingleResponse<SimpleBooleanResponse> creationEligibility(@RequestParam String sellerCode);

  @PutMapping(value = "/api/systemParameter/update")
  GdnBaseRestResponse updateSystemParameter(@RequestParam("storeId") String storeId,
    @RequestParam("requestId") String requestId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId,
    @RequestBody SystemParameterRequest systemParameterUpdateRequest);


}
