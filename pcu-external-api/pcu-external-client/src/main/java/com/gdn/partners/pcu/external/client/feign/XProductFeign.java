package com.gdn.partners.pcu.external.client.feign;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.factory.XProductFeignFallbackFactory;
import com.gdn.partners.pcu.external.client.model.BusinessPartnerPickupPointOutboundResponse;
import com.gdn.partners.pcu.external.web.model.request.ReelProductListingWebRequest;
import com.gdn.partners.pcu.external.web.model.response.ItemL5ListingResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductBasicWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ReelProductDetailWebResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSizeChartUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpcStatusRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemCodeBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel4ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductNameSuggestionResponse;
import com.gdn.x.product.rest.web.model.response.ProductPickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.ProductScoreRuleResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import com.gdn.x.product.rest.web.model.response.UpcStatusResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;


@FeignClient(name = "xProductFeign", url = "${service.x-product.endpoint}", fallbackFactory = XProductFeignFallbackFactory.class)
public interface XProductFeign {

  @RequestMapping(value = "/api/productList/getProductsByMerchantAndCategoryCode", method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ActiveProductResponse> getActiveProductListByMerchantAndCategoryCode(
      @RequestParam("page") Integer page, @RequestParam("size") Integer size,
      @RequestBody ActiveProductRequest activeProductRequest);

  @RequestMapping(value = "/api/summary/nameFilter", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ItemSummaryResponse> getActiveProductNamesByMerchantCode(@RequestParam("page") Integer page,
      @RequestParam("size") Integer size, @RequestBody ItemSummaryRequest itemSummaryRequest);

  @RequestMapping(value = "/api/productList/getSuspendedItemList", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ItemSummaryResponse> getSuspendedItemList(@RequestParam("page") int page,
      @RequestParam("size") int size, @RequestBody ActiveProductRequest activeProductRequest);

  @RequestMapping(value = "/api/rules/product-score-rules", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductScoreRuleResponse> getProductScoreRule(@RequestParam("categoryCode") String categoryCode);

  @RequestMapping(value = "/api/product/getProductAndItems", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItems(@RequestParam("storeId") String storeId,
      @RequestParam("requestId") String requestId, @RequestParam("productSku") String productSku,
      @RequestParam("showDeleted") boolean showDeleted);

  @RequestMapping(value = "/api/product/{productCode}/l3Count", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<SimpleLongResponse> getL3CountByProductCode(@RequestParam("storeId") String storeId,
      @RequestParam("requestId") String requestId, @PathVariable("productCode") String productCode);

  @RequestMapping(value = "/api/product/{productSku}/getPickupPointCodes", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductPickupPointListResponse> getPickupPointCodesByProductSku(
      @PathVariable("productSku") String productSku);

    @RequestMapping(value = "/api/product/getProductCountByType", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductCountResponse> getProductCountByType(@RequestParam("type") String type,
      @RequestParam("merchantCode") String merchantCode);

  @RequestMapping(value = "/api/product/getSecondaryFilterCounts", method = RequestMethod.GET, produces =
      MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductCountResponse> getSecondaryProductCountByType(@RequestParam("type") String type,
      @RequestParam("merchantCode") String merchantCode);

  @RequestMapping(value = "/api/summary/filter/L3", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductL3SummaryResponse> getFilterSummaryL3(@RequestParam("page") int page,
      @RequestParam("size") int size, @RequestParam("onlyDefaultViewConfig") boolean onlyDefaultViewConfig,
      @RequestBody ProductSummaryRequest productSummaryRequest);

  @RequestMapping(value = "/api/summary/productNameFilter", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductNameSuggestionResponse> getProductNamesByFilter(@RequestParam("page") Integer page,
      @RequestParam("size") Integer size, @RequestBody ProductSummaryRequest productSummaryRequest);

  @RequestMapping(value = "/api/summary/getItemNameByItemSku", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<SimpleMapStringResponse> getItemNameByItemSkus(
      @RequestBody SimpleListStringRequest simpleListStringRequest);

  @RequestMapping(value = "/api/item/getL4ItemListByProductSku", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ItemLevel4ListingResponse> getL4ItemListByProductSku(
    @RequestParam("storeId") String storeId, @RequestParam("requestId") String requestId,
    @RequestParam("page") Integer page, @RequestParam("size") Integer size,
    @RequestBody ItemLevel4ListingWebRequest request);

  @RequestMapping(value = "/api/businessPartnerPickupPoint/summary", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<BusinessPartnerPickupPointOutboundResponse> getBusinessPartnerPickupPointSummary(
      @RequestParam("page") int page, @RequestParam("size") int size,
      @RequestBody PickupPointSummaryRequest PickupPointSummaryRequest);

  @RequestMapping(value = "/api/summary/getPickupPointDetailsFromPickupPointCodes",
    method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<PickupPointDetailResponse> getPickupPointDetailByCodes(
    @RequestBody SimpleListStringRequest pickupPointCodes);

  @RequestMapping(value = "/api/businessPartnerPickupPoint/detail", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<BusinessPartnerPickupPointOutboundResponse> getPickupDetailByCodes(
    @RequestBody SimpleListStringRequest pickupPointCodes, @RequestParam("merchantCode") String merchantCode);

  @RequestMapping(value = "/api/item/getItemPickupPointsByItemSku", method = RequestMethod.POST, consumes =
      MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ItemSkuPickupPointCodeResponse> getItemPickupPointCodeByItemSkus(
      @RequestBody SimpleListStringRequest itemSkusList);

  @RequestMapping(value = "/api/item/get-item-basic-details-by-product-sku", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ItemBasicDetailV2Response> getItemBasicDetails(@RequestParam("productSku") String productSku);

  @RequestMapping(value = "/api/listing/itemL5Details", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ItemL5ListingResponse> getItemL5Details(@RequestParam("page") Integer page,
      @RequestParam("size") Integer size, @RequestParam("productSku") String productSku,
      @RequestParam("cncActivated") Boolean cncActivated,
      @RequestParam("fetchOnlyBundleVariants") boolean fetchOnlyBundleVariants,
      @RequestBody SimpleListStringRequest l5IdList);

  @RequestMapping(value = "/api/item/getBulkItemDetailByItemSkus", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ItemBasicDetailV2Response> getItemBasicDetails(
      @RequestParam("fetchBundleRecipe") boolean fetchBundleRecipe, @RequestBody SimpleListStringRequest itemSkus);

  @RequestMapping(value = "/api/product/{productCode}/sharedProduct", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<SimpleBooleanResponse> sharedProductByProductCode(@PathVariable("productCode") String productCode);

  @RequestMapping(value = "/api/item//fetchBasicItemDetailsByItemCodes", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE,
    consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ItemCodeBasicDetailResponse> fetchBasicItemDetailsByItemCodes(
    @RequestParam("storeId") String storeId, @RequestParam("requestId") String requestId,
    @RequestParam(value = "username", required = false) String username,
    @RequestParam(value = "page", defaultValue = "20") int page, @RequestParam("size") int size,
    @RequestParam("sortBy") String sortBy, @RequestParam("orderBy") String orderBy,
    @RequestParam("itemCode") String itemCode, @RequestParam("searchKey") String searchKey);

  @RequestMapping(value = "/api/size-chart/{sizeChartCode}/update-product-size-chart", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateProductSizeChart(@PathVariable("sizeChartCode") String sizeChartCode,
      @RequestBody ProductSizeChartUpdateRequest productSizeChartUpdateRequest);

  @RequestMapping(value = "/api/item/getUpcCodeStatus", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<UpcStatusResponse> getUpcStatus(@RequestBody UpcStatusRequest request);

  @RequestMapping(value = "/api/product/republishProductsToAgp", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse republishProductsToAgp(@RequestBody List<String> productSkus);

  @RequestMapping(value = "/api/item/republishItemsToAgp", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse republishItemsToAgp(@RequestBody List<String> productSkus);

  @RequestMapping(value = "/api/item/republishItemPickupPointToAgp", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse republishItemPickupPointToAgp(@RequestParam("republishToAgp") boolean republishToAgp,
      @RequestBody List<ItemPickupPointRequest> itemPickupPointRequestList);

  @RequestMapping(value = "/api/summary/productListForReels", method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE, consumes =
                      MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ReelProductDetailWebResponse> getReelProductList(
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "10") int size,
      @RequestBody ReelProductListingWebRequest reelProductListingRequest);

  @RequestMapping(value = "/api/product-v2/getProductBasicDetailsByProductSku", method =
      RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes =
      MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductBasicWebResponse> getProductBasicDetails(
      @RequestBody SimpleListStringRequest productSkus);

}
