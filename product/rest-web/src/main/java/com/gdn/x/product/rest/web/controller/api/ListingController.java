package com.gdn.x.product.rest.web.controller.api;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.HalalDashboardProductsResponse;
import com.gdn.x.product.rest.web.model.response.ItemL5ListingResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.rest.web.model.ListingApiPath;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequestV2;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponseV2;
import com.gdn.x.product.service.api.ListingService;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collections;
import java.util.Objects;

@RestController
@RequestMapping(value = ListingApiPath.BASE_PATH)
@Tag(name = "Listing Controller", description = "Listing Service Api")
@Slf4j
public class ListingController {

  @Autowired
  private ListingService listingService;

  @RequestMapping(value = ListingApiPath.ITEM_PICKUP_POINT_SUMMARY, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "ItemPickupPoint summary", description = "Fetch itemPickupPoint summary by ItemPickupPointSummaryRequest")
  public GdnRestListResponse<ItemResponseV2> getItemPickupPointSummary(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "20") int size,
      @RequestParam(required = false) String fetchViewConfigByChannel,
      @RequestBody ItemPickupPointSummaryRequest itemPickupPointSummaryRequest) {
    log.info("Fetching itemPickupPoint summary {} ", itemPickupPointSummaryRequest);
    try {
      Page<ItemResponseV2> itemResponseV2Page =
          listingService.getItemPickupPointSummary(storeId, page, size, itemPickupPointSummaryRequest, fetchViewConfigByChannel);
      return new GdnRestListResponse<>(itemResponseV2Page.getContent(), new PageMetaData(size, page, itemResponseV2Page.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error while fetching itemPickupPoint summary {} ", itemPickupPointSummaryRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = ListingApiPath.ITEM_PICKUP_POINT_BY_ITEM_SKU, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetching itemPickupPoint by itemSku")
  public GdnRestListResponse<ItemResponseV2> getItemPickupPointsByItemSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "20") int size,
      @RequestParam(required = false, defaultValue = "false") boolean excludeDistributionPickupPoint,
      @RequestBody ItemRequestV2 itemRequestV2) {
    log.info("Fetching itemPickupPoint by itemRequest {} ", itemRequestV2);
    try {
      Page<ItemResponseV2> itemResponseV2Page =
          listingService.getItemPickupPointsByItemSku(storeId, page, size, itemRequestV2,
              excludeDistributionPickupPoint);
      return new GdnRestListResponse<>(itemResponseV2Page.getContent(), new PageMetaData(size, page, itemResponseV2Page.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error while fetching itemPickupPoint by itemRequest {} ", itemRequestV2, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = ListingApiPath.PRODUCT_SUMMARY_V2, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetching product summary")
  public GdnRestListResponse<ProductSummaryResponseV2> getProductSummaryV2(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "20") int size, @RequestBody ProductSummaryRequestV2 productSummaryRequestV2) {
    log.info("Fetching product summary {} ", productSummaryRequestV2);
    try {
      Page<ProductSummaryResponseV2> itemResponseV2Page =
          listingService.getProductSummary(storeId, page, size, productSummaryRequestV2);
      return new GdnRestListResponse<>(itemResponseV2Page.getContent(),
          new PageMetaData(size, page, itemResponseV2Page.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error while fetching product summary {} ", productSummaryRequestV2, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = ListingApiPath.GET_HALAL_DASHBOARD_PRODUCTS, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetching Halal dashboard products")
  public GdnRestListResponse<HalalDashboardProductsResponse> getHalalDashboardProducts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "50") int size, @RequestBody HalalProductsFilterRequest halalProductsFilterRequest) {
    log.info("Fetching Halal dashboard products with filter request {} ", halalProductsFilterRequest);
    try {
      Page<HalalDashboardProductsResponse> halalDashboardProductsResponsePage =
          listingService.getHalalDashboardProductsResponses(storeId, page, size, halalProductsFilterRequest);
      return new GdnRestListResponse<>(halalDashboardProductsResponsePage.getContent(),
          new PageMetaData(size, page, halalDashboardProductsResponsePage.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error while fetching halal dashboard products {} ", halalProductsFilterRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }


  @RequestMapping(value = ListingApiPath.ITEM_PICKUP_LISTING, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(description = "Fetch Item PickupPoint Listing Response ")
  public GdnRestListResponse<ItemPickupPointListingResponse> getItemPickupPointList(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "20") int size,
      @RequestParam(required = false) String fetchViewConfigByChannel,
      @RequestBody ItemPickupPointListingRequest itemPickupPointListingRequest) {
    log.info("Fetching itemPickupPoint list {} ", itemPickupPointListingRequest);
    try {
      Page<ItemPickupPointListingResponse> itemResponseV2Page =
          listingService.getItemPickupPointListing(storeId, page, size, fetchViewConfigByChannel, itemPickupPointListingRequest);
      return new GdnRestListResponse<>(itemResponseV2Page.getContent(),
          new PageMetaData(size, page, itemResponseV2Page.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Fetching itemPickupPoint list {} ", itemPickupPointListingRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = ListingApiPath.ITEM_L5_DETAILS, method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(description = "Fetch Item PickupPoint Listing Response ")
  @ResponseBody
  public GdnRestListResponse<ItemL5ListingResponse> getItemL5Details(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(required = false) Integer page,
      @RequestParam(required = false) Integer size, @RequestParam(required = false) String productSku,
      @RequestParam(required = false) Boolean cncActivated,
      @RequestParam(required = false, defaultValue = "false") boolean fetchOnlyBundleVariants,
      @RequestBody SimpleListStringRequest l5IdList) {
    log.info("Fetching itemL5Details for productSku: {} ", productSku);
    try {
      Page<ItemL5ListingResponse> itemL5ListingResponse = listingService.getItemL5Listing(storeId,
          StringUtils.isBlank(productSku) ? null : Collections.singletonList(productSku), l5IdList.getValue(), page,
          size, cncActivated, fetchOnlyBundleVariants);
      return new GdnRestListResponse<>(itemL5ListingResponse.getContent(), new PageMetaData(
          Objects.isNull(size) ? itemL5ListingResponse.getTotalElements() : size,
          Objects.isNull(page) ? 0 : page, itemL5ListingResponse.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Fetching itemL5Details for productSku: {} with error - ", productSku, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }
}
