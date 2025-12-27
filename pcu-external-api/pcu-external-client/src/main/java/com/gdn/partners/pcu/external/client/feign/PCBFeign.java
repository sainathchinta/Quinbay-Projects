package com.gdn.partners.pcu.external.client.feign;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.factory.PCBFeignFallbackFactory;
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
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
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
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

/**
 * @author Pradeep Reddy
 */
@FeignClient(name = "pcbFeign", url = "${service.pcb.endpoint}", fallbackFactory = PCBFeignFallbackFactory.class)
public interface PCBFeign {

  @RequestMapping(value = "/api/product/{id}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductDetailResponse> getProductDetailsById(@PathVariable("id") String productId);

  @RequestMapping(value = "/api/categoryShipping/{categoryCode}/shippingWeight", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CategoryShippingWeightResponse> generateShippingWeight(
      @PathVariable("categoryCode") String categoryCode, @RequestParam("length") double length,
      @RequestParam("width") double width, @RequestParam("height") double height,
      @RequestParam("weight") double weight);

  @RequestMapping(value = "/api/predefinedAllowedAttributeValue/filter/getBrandSuggestions", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getBrandSuggestions(@RequestParam("value") String value,
      @RequestParam("businessPartnerCode") String businessPartnerCode, @RequestParam("isSearch") boolean isSearch,
      @RequestParam("isExternal") boolean isExternal, @RequestParam("page") Integer page, @RequestParam("size") Integer size);

  @RequestMapping(value = "/api/category/hierarchy/filter/category-code/{id}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<CategoryResponse> filterCategoryHierarchyByCategoryCode(@PathVariable("id") String categoryCode);

  @RequestMapping(value = "/api/product/filter/name/categoryId", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductItemDetailResponse> getProductItemSuggestionsByItemNameAndCategoryId(
      @RequestParam("productItemName") String productItemName, @RequestParam("categoryId") String categoryId,
      @RequestParam("page") Integer page, @RequestParam("size") Integer size);

  @RequestMapping(value = "/api/brand-wip/create", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CreateBrandWipResponse> create(@RequestBody CreateBrandWipRequest createBrandWipRequest);

  @RequestMapping(value = "/api/brand/filter/brand-name", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<BrandResponse> filterByBrandName(@RequestParam("brandName") String brandName,
      @RequestParam("activeBrandsOnly") boolean activeBrandsOnly);

  @RequestMapping(value = "/api/brand/filter/summary", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<BrandResponse> filterSummary(@RequestParam(name = "page", defaultValue = "0") Integer page,
      @RequestParam(name = "size", defaultValue = "10") Integer size, @RequestBody BrandSummaryRequest request);

  @RequestMapping(value = "/api/brand-wip/detail", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<BrandWipResponse> getBrandDetail(@RequestParam("brandRequestCode") String brandRequestCode);

  @RequestMapping(value = "/api/brand-wip/filter/brand-name", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<BrandWipResponse> findByBrandNameAndBusinessPartnerCode(
      @RequestParam("brandName") String brandName, @RequestParam("businessPartnerCode") String businessPartnerCode);

  @RequestMapping(value = "/api/category/getAllChildCategoriesFromC1CategoryCode", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CategoryCodeResponse> getAllChildCategoryCodesByC1CategoryCode(
      @RequestBody CategoryCodeRequest request);

  @RequestMapping(value = "/api/category/{id}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetail(@PathVariable("id") String categoryId);

  @RequestMapping(value = "/api/attribute/{id}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<AttributeResponse> getAttributeDetail(@PathVariable("id") String attributeId,
      @RequestParam("sortValues") boolean sortValues);

  @RequestMapping(value = "/api/category/filter/bulk/category-codes", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<CategoryDTO> getCategoriesByCategoryCodes(@RequestParam("page") Integer page,
      @RequestParam("size") Integer size, @RequestParam("activated") String activated, @RequestBody String requestBody);

  @RequestMapping(value = "/api/product/item/filter/upc-code", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductItemDetailResponse> getProductItemsByUPCCodeAndCategory(
      @RequestParam("page") Integer page, @RequestParam("size") Integer size, @RequestBody UPCCodeSearchRequest request,
      @RequestParam("isOnlyExternal") boolean isOnlyExternal);

  @RequestMapping(value = "/api/product/filter/category-hierarchy-with-product-count/upcCode", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<CategoryHierarchyResponse> getCategoryHierarchyByUPCCodeWithProductCount(
      @RequestParam("upcCode") String upcCode, @RequestParam("isOnlyExternal") boolean isOnlyExternal);

  @RequestMapping(value = "/api/brand/filter/{brandCode}/status/{status}", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<BrandPredefinedAttributeValueResponse> getBrandPredefinedValueDetail(
      @RequestParam("brandCode") String brandCode, @RequestParam("status") String status);

  @RequestMapping(value = "/api/category/get-wholesale-config", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<WholesaleMappingResponse> getCategoryWholesaleConfiguration(
      @RequestParam("categoryId") String categoryId, @RequestParam("categoryCode") String categoryCode);

  @RequestMapping(value = "/api/brand/getDefaultBrands", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getDefaultBrands();

  @RequestMapping(value = "/api/category/getCategoryNames", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CategoryNamesResponse> getCategoryNames(@RequestBody CategoryMultipleIdRequest categoryCodes,
      @RequestParam("page") int page, @RequestParam("size") int size);

  @RequestMapping(value = "/api/brandAuthorise/{sellerCode}/valid", method = RequestMethod.GET, produces =
      MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<SimpleBooleanResponse> validateAuthorisedBrand(
      @RequestParam("brandCode") String brandCode, @PathVariable("sellerCode") String sellerCode);

  @RequestMapping(value = "/api/brand-wip/inReviewBrands", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<BrandInReviewResponse> getAllInReviewBrands(
    @RequestParam("storeId") String storeId, @RequestParam("requestId") String requestId);

  @RequestMapping(value = "/api/product/item/filter/get-productItemBy-sku-codes", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductItemResponse> getItemDeatilBySkuCodes(@RequestBody SkuCodesRequest skuCodesRequest);

  @RequestMapping(value = "/api/size-chart/get-size-chart-name", method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<BasicSizeChartDetailMapResponse> getBasicSizeChartDetails(
      @RequestBody List<String> sizeChartCodes);

  @RequestMapping(method = RequestMethod.GET, value = "/api/distribution-info/{productCode}",
                  produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<DistributionInfoPerSkuResponse> getDistributionInfo(
      @PathVariable("productCode") String productCode,
      @RequestParam(value = "needDistributionInfoResponse", defaultValue = "false")
      boolean needDistributionInfoResponse,
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "50") int size);

  @RequestMapping(value = "/api-solr/brand/reindexByBrandRequestCode", method = RequestMethod.GET
    , produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse reindexBrandCollection(@RequestParam String brandRequestCode);
}
