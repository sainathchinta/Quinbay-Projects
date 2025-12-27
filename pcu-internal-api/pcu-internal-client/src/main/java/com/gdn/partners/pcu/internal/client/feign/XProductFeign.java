package com.gdn.partners.pcu.internal.client.feign;

import java.util.List;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.response.HalalDashboardProductsResponse;
import com.gdn.x.product.rest.web.model.response.HalalProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.internal.client.factory.XProductFeignFallbackFactory;
import com.gdn.x.product.model.vo.ProductCenterSummaryRequest;
import com.gdn.x.product.model.vo.ProductCenterSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ProductCenterHistoryResponse;
import com.gdn.x.product.rest.web.model.request.SalesCategoryMappingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.product.rest.web.model.response.ProductCenterDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;

@FeignClient(name = "xProductFeign", url = "${service.x-product.endpoint}", fallbackFactory = XProductFeignFallbackFactory.class)
public interface XProductFeign {

  @RequestMapping(value = "/api/summary/product-center/filter", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductCenterSummaryResponse> getProductCenterSummaryFilter(
      @RequestParam("storeId") String storeId, @RequestParam("requestId") String requestId,
      @RequestParam("page") Integer page, @RequestParam("size") Integer size,
      @RequestBody ProductCenterSummaryRequest activeProductRequest);

  @RequestMapping(value = "/api/product/addSalesCategory", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse addProductSalesCatalog(@RequestParam("storeId") String storeId,
      @RequestParam("requestId") String requestId, @RequestParam("catalogCode") String catalogCode,
      @RequestParam("newCategoryCode") String newCategoryCode, @RequestBody SimpleListStringRequest productSkus);

  @RequestMapping(value = "/api/product/deleteSalesCategory", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse deleteSalesCatalog(@RequestParam("storeId") String storeId,
      @RequestParam("requestId") String requestId, @RequestParam("catalogCode") String catalogCode,
      @RequestParam("oldCategoryCode") String oldCategoryCode, @RequestBody SimpleListStringRequest productSkus);

  @RequestMapping(value = "/api/product/moveSalesCategory", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse moveProductSalesCatalog(@RequestParam("storeId") String storeId,
      @RequestParam("requestId") String requestId, @RequestParam("catalogCode") String catalogCode,
      @RequestParam("oldCategoryCode") String oldCategoryCode, @RequestParam("newCategoryCode") String newCategoryCode,
      @RequestBody SimpleListStringRequest productSkus);

  @RequestMapping(value = "/api/product/{productSku}/productCenterHistory",
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductCenterHistoryResponse> getProductCenterHistory(
      @PathVariable("productSku") String productSku, @RequestParam("page") int page, @RequestParam("size") int size);

  @RequestMapping(value = "/api/product/getProductAndItems", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItems(@RequestParam("storeId") String storeId,
      @RequestParam("requestId") String requestId, @RequestParam("productSku") String productSku,
      @RequestParam("showDeleted") boolean showDeleted);

  @RequestMapping(value = "/api/product/getProductDetailsForProductCenter", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductCenterDetailResponse> getProductDetailsForProductCenter(
      @RequestParam("storeId") String storeId, @RequestParam("requestId") String requestId,
      @RequestParam("productSku") String productSku);

  @RequestMapping(value = "/api/product/updateSalesCategory", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateSalesCategory (@RequestParam("storeId") String storeId,
      @RequestParam("requestId") String requestId, @RequestParam("productSku") String productSku,
      @RequestBody SalesCategoryMappingUpdateRequest request);

  @RequestMapping(value = "/api/reindex/reindexAndClearCacheByProductSkus", method = RequestMethod.POST, produces = MediaType
      .APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse reindexByProductSkus(@RequestParam("storeId") String storeId,
      @RequestParam("requestId") String requestId, @RequestBody SimpleListStringRequest request);

  @RequestMapping(value = "/api/product/getProductDetailsByProductSku", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductL3Response> getProductDetailsByProductSku(
      @RequestParam("storeId") String storeId, @RequestParam("requestId") String requestId,
      @RequestParam("productSku") String productSku);

  @RequestMapping(value = "/api/item/updateViewConfigAndForceReview", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse takeDownOrReactivateProduct(
      @RequestParam("forceReview") boolean forceReview,
      @RequestBody List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuListRequest);

  @RequestMapping(value = "/api/product-v2/getProductDetailsforHalalProductsByProductSkuList", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<HalalProductResponse> getProductDetailsByProductSkuList(@RequestBody List<String> productSkuList);

  @RequestMapping(value = "/api/listing/getHalalDashboardProducts", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<HalalDashboardProductsResponse> getHalalDashboardProducts(
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "50") Integer size,
      @RequestBody HalalProductsFilterRequest halalProductsFilterRequest);

  @RequestMapping(value = "/api/product-v2/{productSku}/updateProductHalalConfig", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateHalalConfigOfProduct(@RequestParam("storeId") String storeId,
      @RequestParam("requestId") String requestId, @RequestParam("username") String username,
      @PathVariable("productSku") String productSku, @RequestParam("curationStatus") String curationStatus);

  @RequestMapping(value = "/api/product-v2/getProductBasicDetailsByProductSku", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductBasicResponse> getProductBasicDetails(@RequestBody SimpleListStringRequest request);

  @GetMapping(value = "/api/productList/generateProductScoreByProductSkuOrProductCode", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse generateProductScoreByProductSkuOrProductCode(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username,
    @RequestParam("updateCategory") boolean updateCategory,
    @RequestParam("productSku") String productSku, @RequestParam("productCode") String productCode);
}
