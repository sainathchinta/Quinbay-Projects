package com.gdn.x.product.outbound.api.feign;

import java.util.List;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.outbound.api.feign.config.PCBFeignProperties;
import com.gdn.x.product.rest.web.model.response.SizeChartResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.request.ProductMigrationRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BasicInfoProductResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryEligibleForSizeChartResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryShippingWeightResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.ProductSalesCategoryMappingResponse;

import com.gdn.x.productcategorybase.dto.response.SimpleListStringResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "pcbFeign", url = "${pcb.feign.host}", configuration = PCBFeignProperties.class)
public interface PCBFeign {

  @GetMapping(value = "/api/product/{productCode}/get-sales-category-mappings", produces =
    MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductSalesCategoryMappingResponse> getSalesCategoryMapping(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @PathVariable("productCode") String productCode,
    @RequestParam("ignoreHalalCategories") boolean ignoreHalalCategories);

  @GetMapping(value = "/api/category/{categoryId}", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetail(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @PathVariable("categoryId") String categoryId);

  @GetMapping(value = "/api/category/hierarchy/filter/category-code/{categoryCode}", produces =
    MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<CategoryResponse> filterCategoryHierarchyByCategoryCode(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @PathVariable("categoryCode") String categoryCode);

  @GetMapping(value = "/api/product/productCode/{productCode}", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCode(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @PathVariable("productCode") String productCode,
    @RequestParam("inAllProducts") boolean inAllProducts, @RequestParam("originalImages") boolean originalImages);

  @GetMapping(value = "/api/product/productCode/{productCode}", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCodeInAllProducts(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @PathVariable("productCode") String productCode,
    @RequestParam("inAllProducts") boolean inAllProducts, @RequestParam("originalImages") boolean originalImages);

  @GetMapping(value = "/api/brand/filter/brand-code", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<BrandResponse> getBrandResponseByBrandCode(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestParam("brandCode") String brandCode);

  @GetMapping(value = "/api/product/skuCode/{skuCode}", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductItemDetailResponse> getProductItemDetailBySkuCode(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @PathVariable("skuCode") String skuCode,
    @RequestParam("originalImages") boolean originalImages);

  @GetMapping(value = "/api/category/categoryCode/{categoryCode}", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetailByCategoryCode(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @PathVariable("categoryCode") String categoryCode);

  @GetMapping(value = "/api/productCategory/get-master-parent-category-response-by-product-code",
    produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<CategoryResponse> getMasterParentCategoriesByProductCode(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestParam("productCode") String productCode);

  @GetMapping(value = "/api/categoryShipping/{categoryCode}/shippingWeight", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CategoryShippingWeightResponse> generateShippingWeight(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @PathVariable("categoryCode") String categoryCode,
    @RequestParam("length") double length, @RequestParam("width") double width,
    @RequestParam("height") double height, @RequestParam("weight") double weight);

  @PostMapping(value = "/api/category/getCategoryNames", consumes =
    MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CategoryNamesResponse> getCategoryNames(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestParam("page") Integer page,
    @RequestParam("size") Integer size, @RequestBody CategoryMultipleIdRequest categoryCodes);

  @PostMapping(value = "/api/category/getAllChildCategoriesFromC1CategoryCode", consumes =
    MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CategoryCodeResponse> getAllChildCategoriesFromC1CategoryCode(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestBody CategoryCodeRequest request);

  @GetMapping(value = "/api/product/{itemCode}/getMasterDataForTransaction", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductMasterDataResponse> getMasterProductDetailsByItemCode(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @PathVariable("itemCode") String itemCode);

  @PostMapping(value = "/api/product/item/filter/sku-codes", consumes =
    MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductItemDetailResponse> filterProductItemBySkuCodes(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestParam("originalImages") boolean originalImages,
    @RequestBody SkuCodesRequest request);

  @GetMapping(value = "/api/product/{productCode}/images", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ImageResponse> getProductImagesByProductCode(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @PathVariable("productCode") String productCode);

  @PostMapping(value = "/api/product/imagesByItemCodes", consumes =
    MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ItemImageResponse> getProductItemImagesByItemCode(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestBody  SkuCodesRequest skuCodesRequest);

  @GetMapping(value = "/api/attribute/attributeCode/{attributeCode}/detail", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<AttributeResponse> getAttributeDetailByAttributeCode(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @PathVariable("attributeCode") String attributeCode);

  @GetMapping(value = "/api/product/productBasicDetails/{productCode}", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductResponse> getProductBasicDetails(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @PathVariable("productCode") String productCode);

  @GetMapping(value = "/api/product/{productCode}/productAndAttributeDetails", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductAndAttributeDetailResponse> getProductAndAttributeDetails(
      @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
      @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
      @RequestParam("username") String username, @PathVariable("productCode") String productCode,
      @RequestParam("inAllProducts") boolean inAllProducts);

  @GetMapping(value = "/api/size-chart/{sizeChartCode}/detail", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<SizeChartResponse> fetchSizeChartDetails(@RequestParam("storeId") String storeId,
      @RequestParam("channelId") String channelId, @RequestParam("clientId") String clientId,
      @RequestParam("requestId") String requestId, @RequestParam("username") String username,
      @RequestParam("preview") boolean preview, @PathVariable("sizeChartCode") String sizeChartCode);

  @GetMapping(value = "/api/size-chart/validateSizeChartCode/{sizeChartCode}", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse validateSizeChartCode(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("sizeChartCode") String sizeChartCode);

  @PostMapping(value = "/api/category/check-category-eligible-for-size-chart-addition", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CategoryEligibleForSizeChartResponse> checkCategoryEligibleForSizeChartAddition(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestBody List<String> categoryCodes,
      @RequestParam("sizeChartAttributeCode") String sizeChartAttributeCode);

  @GetMapping(value = "/api/attribute/categoriesByAttributeCode", produces =
      MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<SimpleListStringResponse> getCategoryCodesByAttributeCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String attributeCode);

  @PostMapping(value = "/api/scheduler/update-product-migration-status")
  GdnBaseRestResponse updateProductMigrationStatus(@RequestParam("storeId") String storeId,
      @RequestParam("requestId") String requestId, ProductMigrationRequest productMigrationRequest);

  @PostMapping(value = "/api/product/product-basic-info-by-product-codes")
  GdnRestListResponse<BasicInfoProductResponse> getBasicInfoProductDetailsListByProductCodes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody List<String> productCodeList);

  @PostMapping(value = "/api/product/v2/checkOmniChannelSkuExistsOrNotBySellerCodeAndSkuList", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ValidOmniChannelSkuResponse> checkOmniChannelSkuExistsInSeller(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam("needUomInfo") boolean needUomInfo,
      OmniChannelSkuRequest omniChannelSkuRequest);
}
