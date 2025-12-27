package com.gdn.partners.pcu.external.client.fallback;

import java.util.List;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.external.client.feign.XProductFeign;
import com.gdn.partners.pcu.external.client.model.BusinessPartnerPickupPointOutboundResponse;
import com.gdn.partners.pcu.external.model.ErrorMessages;
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
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductNameSuggestionResponse;
import com.gdn.x.product.rest.web.model.response.ProductPickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.ProductScoreRuleResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import com.gdn.x.product.rest.web.model.response.UpcStatusResponse;
import org.springframework.stereotype.Component;

@Component
public class XProductFeignFallback implements XProductFeign {

  private static PageMetaData pageMetaData = new PageMetaData(0, 0, 0);

  @Override
  public GdnRestListResponse<ActiveProductResponse> getActiveProductListByMerchantAndCategoryCode(Integer page,
      Integer size, ActiveProductRequest activeProductRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<ItemSummaryResponse> getActiveProductNamesByMerchantCode(Integer page, Integer size,
      ItemSummaryRequest itemSummaryRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<ItemSummaryResponse> getSuspendedItemList(int page, int size,
      ActiveProductRequest activeProductRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestSingleResponse<ProductScoreRuleResponse> getProductScoreRule(String categoryCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItems(String storeId, String requestId,
      String productSku, boolean showDeleted) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<SimpleLongResponse> getL3CountByProductCode(String storeId, String requestId,
      String productCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductPickupPointListResponse> getPickupPointCodesByProductSku(String productSku) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductCountResponse> getProductCountByType(String type, String merchantCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductCountResponse> getSecondaryProductCountByType(String type, String merchantCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<ProductL3SummaryResponse> getFilterSummaryL3(int page, int size,
      boolean onlyDefaultViewConfig, ProductSummaryRequest productSummaryRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<ProductNameSuggestionResponse> getProductNamesByFilter(Integer page, Integer size,
      ProductSummaryRequest productSummaryRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestSingleResponse<SimpleMapStringResponse> getItemNameByItemSkus(
      SimpleListStringRequest simpleListStringRequest) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<BusinessPartnerPickupPointOutboundResponse> getBusinessPartnerPickupPointSummary(int page,
      int size, PickupPointSummaryRequest PickupPointSummaryRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<PickupPointDetailResponse> getPickupPointDetailByCodes(
    SimpleListStringRequest simpleListStringRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<BusinessPartnerPickupPointOutboundResponse> getPickupDetailByCodes(
    SimpleListStringRequest pickupPointCodes, String merchantCode) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<ItemLevel4ListingResponse> getL4ItemListByProductSku(String storeId,
    String requestId, Integer page, Integer size, ItemLevel4ListingWebRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<ItemSkuPickupPointCodeResponse> getItemPickupPointCodeByItemSkus(
      SimpleListStringRequest itemSkusList) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<ItemBasicDetailV2Response> getItemBasicDetails(String productSku) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<ItemBasicDetailV2Response> getItemBasicDetails(boolean fetchBundleRecipe,
      SimpleListStringRequest itemSkus) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<ItemL5ListingResponse> getItemL5Details(Integer page, Integer size,
      String productSku, Boolean cncActivated, boolean fetchOnlyBundleVariants, SimpleListStringRequest l5IdList) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<SimpleBooleanResponse> sharedProductByProductCode(String productCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<ItemCodeBasicDetailResponse> fetchBasicItemDetailsByItemCodes(
    String storeId, String requestId, String username, int page, int size, String sortBy,
    String orderBy, String itemCode, String searchKey) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnBaseRestResponse updateProductSizeChart(String sizeChartCode,
      ProductSizeChartUpdateRequest productSizeChartUpdateRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestListResponse<UpcStatusResponse> getUpcStatus(UpcStatusRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnBaseRestResponse republishProductsToAgp(List<String> productSkus) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnBaseRestResponse republishItemsToAgp(List<String> productSkus) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnBaseRestResponse republishItemPickupPointToAgp(boolean republishToAgp,
      List<ItemPickupPointRequest> itemPickupPointRequestList) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestListResponse<ReelProductDetailWebResponse> getReelProductList(int page, int size,
      ReelProductListingWebRequest reelProductListingRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<ProductBasicWebResponse> getProductBasicDetails(
      SimpleListStringRequest productSkus) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }
}
