package com.gdn.partners.pcu.external.client.fallback;

import com.gda.mta.product.dto.OmniChannelExistsRequest;
import com.gda.mta.product.dto.ProductSkuAndPickupPointCodeRequest;
import com.gda.mta.product.dto.response.OmniChannelMapAndSkuResponse;
import com.gdn.partners.pcu.external.web.model.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;

import com.gda.mta.product.dto.AppealProductRequest;
import com.gda.mta.product.dto.AppealProductResponse;
import com.gda.mta.product.dto.BulkDeleteProductWipRequest;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.EditProductV2Response;
import com.gda.mta.product.dto.HistoryRequest;
import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductResponse;
import com.gda.mta.product.dto.NeedRevisionSubmitRequest;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gda.mta.product.dto.PickupPointUpdateResponse;
import com.gda.mta.product.dto.ProductBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductCopyRequest;
import com.gdn.partners.pcu.external.client.model.ProductCreationRequest;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gdn.partners.pcu.external.client.model.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3OrderResponse;
import com.gda.mta.product.dto.ProductLevel3QuickEditRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3StockInfoWebSiteResponse;
import com.gda.mta.product.dto.ProductLevel3StockRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryCountResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gda.mta.product.dto.ProductSkuListRequest;
import com.gda.mta.product.dto.ProductSystemParameterResponse;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.RejectedSkuProductResponse;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.UpdateImageRequest;
import com.gda.mta.product.dto.UpdateItemsPriceStockImagesRequest;
import com.gda.mta.product.dto.UpdateProductLevel3InfoRequest;
import com.gda.mta.product.dto.VendorNotesRequest;
import com.gda.mta.product.dto.response.AvailableToCopyProductDetailsResponse;
import com.gda.mta.product.dto.response.CogsValueResponse;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import com.gda.mta.product.dto.response.ItemBulkArchiveResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemSummaryL4Response;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.ProductCountResponse;
import com.gda.mta.product.dto.response.ProductItemNameResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailsV2Response;
import com.gda.mta.product.dto.response.ProductSkuResponseList;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gda.mta.product.dto.response.ProductSystemParameterSwitchResponse;
import com.gda.mta.product.dto.response.VendorNotesResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.entity.PickupPointCodeResponse;
import com.gdn.mta.product.entity.UniquePickupPointCodeResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pbp.dto.productcategory.CategoryHierarchyProductCountResponse;
import com.gdn.partners.pbp.dto.productlevel1.ProductSearchRequest;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3InactiveResponse;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.EstimateItemPriceResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipDetailResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipSummaryRequest;
import com.gdn.partners.pbp.dto.productlevel3.SuspensionItemResponse;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.web.model.response.AppealProductConfigResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3DetailsResponse;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

@Component
public class PBPFeignFallback implements PBPFeign {

  private static PageMetaData pageMetaData = new PageMetaData(0, 0, 0);

  @Override
  public GdnBaseRestResponse create(@RequestBody ProductBusinessPartnerRequest request){
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse createProduct(@RequestBody ProductCreationRequest request, String flowType){
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse createNewProduct(@RequestBody ProductCreationRequest request, String flowType){
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestSimpleResponse<String> generateProductCode() {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSimpleResponse<EstimateItemPriceResponse> getEstimatedPrice(String itemCode,
      double lowestPriceCoefficient) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSimpleResponse<String> generateBarCode() {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<ProductItemDetailResponse> getProductItemsByNameAndCategoryCodes(
      Integer page, Integer size, ProductSearchRequest request, boolean isOnlyExternal) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<ProductItemResponse> getProductItemSuggestions(Integer page, Integer size,
      String productItemName, String categoryId) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<CategoryHierarchyProductCountResponse> getCategoryHierarchyByKeywordWithProductCount(
      Integer page, Integer size, String keyword, String businessPartnerCode) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestSimpleResponse<Integer> getProductsCountByViewable(boolean viewable) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductLevel3WipDetailResponse> getProductDetailByProductSku(String productSku, boolean isActive) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductLevel3Response> findDetailByGdnSku(String businessPartnerCode, String gdnSku) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse copy(boolean isRetryAttempt, ProductCopyRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse copyAll(ProductCopyRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse generateQRCodeNotification(String businessPartnerCode, String filePath) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<AvailableToCopyProductDetailsResponse> productsAvailableToCopy(String businessPartnerCode,
    String linkedPartnerCode, Integer page, Integer size, ProductLevel3SummaryRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestSingleResponse<ProductLevel3SummaryCountResponse> getActiveProductStockCount(
      String businessPartnerCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<CountProductLevel3WipResponse> getInProgressProductCount(String businessPartnerCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductLevel3CountResponse> getNonActiveProductCount(String businessPartnerCode,
      String type) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<CountProductLevel3InactiveResponse> getInActiveProductCount(String businessPartnerCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<ProductLevel3SummaryResponse> filterSummary(String businessPartnerCode, Integer page,
      Integer size, String orderBy, String sortBy, ProductLevel3SummaryRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestListResponse<RejectedSkuProductResponse> filterProductBusinessPartnerSummaryByBusinessPartnerId(
      Integer page, Integer size, String businessPartnerId, String searchCriteria, String orderBy, String sortBy) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse toggleArchiveItem(String clientHost, String itemSku, boolean doArchive) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestListResponse<LogAuditTrailUpdatedProductResponse> getProductUpdateLogs(Integer page, Integer size,
      String gdnSku) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestSingleResponse<ProductLevel3OrderResponse> filterDetailOrderByGdnSku(String businessPartnerCode,
      String gdnSku) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductLevel3SummaryResponse> updateSummary(String businessPartnerCode, String gdnSku,
      String clientHost, ProductLevel3UpdateSummaryRequest request) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse bulkDeleteProductWip(String businessPartnerCode, BulkDeleteProductWipRequest request) {
    return new GdnRestSingleResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE
        .getMessage(),  false, null, null);
  }

  @Override
  public GdnBaseRestResponse retryCreate(String productBusinessPartnerId) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestSimpleResponse<Boolean> notifyMailVisibilityOptionForProductWip(String businessPartnerCode) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductLevel3SummaryResponse> filterSummaryByGdnSku(String businessPartnerCode,
      String gdnSku) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse unsynchronizeProduct(String clientHost, String productSku, String itemSku) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnBaseRestResponse synchronizeProduct(String clientHost, String productSku, String itemSku) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestListResponse<PredefinedAllowedAttributeValueResponse> activeBrandByCategoryId(String categoryId) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnBaseRestResponse sendEmailForExceededActivation(String businessPartnerCode) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestSingleResponse<ProductLevel3StockInfoWebSiteResponse> getStockInfoWebSite(String webMerchantCode,
      String webItemSku) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductLevel3Response> updateAndReturn(String clientHost, Boolean isExternalOnly, boolean hasOrder, boolean updateLogistics,
      ProductLevel3Request product) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSimpleResponse<Boolean> isPristineCategory(String categoryId) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<SuspensionItemResponse> getSuspendedItem(Integer page, Integer size,
      SummaryFilterRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<ProductLevel3WipResponse> filterSummaryWithState(Integer page, Integer size,
      ProductLevel3WipSummaryRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnBaseRestResponse updateProductImage( boolean isOnlyExternal, UpdateImageRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestSimpleResponse<Boolean> isProductMappedToMerchant(String merchantCode) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse updateItemStock(String defaultClientHost, String businessPartnerCode,
      ProductLevel3StockRequest productLevel3StockRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestSimpleResponse<CogsValueResponse> getCogsValue(String materialCode) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse updateItemPrice(String clientHost, String itemSku,
      ProductPriceAndWholesaleRequest productPriceAndWholesaleRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestSimpleResponse<Integer> getMinimumPrice() {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductSystemParameterResponse> findSystemParameter(String variable) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductSystemParameterSwitchResponse> getSystemParameterSwitch() {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<EditProductV2Response> editProductInfo(ProductLevel3Request product, String productSku, boolean isOnlyExternal) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<ProductLevel3SummaryDetailsResponse> filterSummaryDetails(String businessPartnerCode,
      Integer page, Integer size, ProductLevel3SummaryDetailsRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<PickupPointCodeResponse> getPickupPointCodes(int page, int size, String productSku,
      boolean needCorrection, String businessPartnerCode, boolean fbbActivated) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<UniquePickupPointCodeResponse> getUniquePickupPointCodes(String productSku) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> updateItemsPriceStockImages(
      String businessPartnerCode, UpdateItemsPriceStockImagesRequest updateItemsPriceStockImagesRequest) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<HistoryResponse> getProductHistorySummary(int page, int size,
      HistoryRequest historyRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnBaseRestResponse updateLogistics(Boolean isOnlyExternal, ProductLevel3UpdateRequest product,
      boolean isNeedCorrection) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestSimpleResponse<ProductSkuResponseList> getProductSkusByProductCode(String productCode) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<PickupPointUpdateResponse> updatePickupPointCodes(
      PickupPointUpdateRequest pickupPointUpdateRequest) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<ProductItemNameResponse> getProductVariantsName(int page, int size, String productSku) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductLevel3DetailResponse> getL3DetailByProductSku(String productSku,
      boolean isNeedCorrection) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse itemListingUpdate(ProductLevel3QuickEditRequest request, String productSku) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestSingleResponse<ItemBulkArchiveResponse> archiveProducts(boolean doArchive,
      SimpleListStringRequest request) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<ProductSuspensionHistoryResponse> fetchProductSuspensionHistory(
      ProductSkuListRequest productSkuListRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSimpleResponse<VendorNotesResponse> getVendorNotes(String productCode) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse updateVendorNotes(String productCode, VendorNotesRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestSingleResponse<EditProductV2Response> submitNeedRevisionProduct(NeedRevisionSubmitRequest request) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<AppealProductResponse> updateAppealInProgressProduct(
      AppealProductRequest appealProductRequest) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<EditProductV2Response> updateProductInfo(UpdateProductLevel3InfoRequest request,
      String productSku) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> updateImages(
      ProductImageEditRequest productImageEditRequest) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductL3DetailsResponse> getL3ProductDetailsByProductSku(
    String productSku, boolean isNeedCorrection, boolean concatenateValueWithValueType) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<ProductLevel3SummaryResponse> getL5SummaryByProductSkuList(int page,
    int size, String businessPartnerCode, ProductSkuAndPickupPointCodeRequest productSkuAndPickupPointCodeRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<BulkDownloadProductLevel3Response> bulkDownloadSummaryFromDb(String businessPartnerCode,
      ProductLevel3SummaryRequest request, boolean fetchProductData) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<HistoryUpdateResponse> getProductUpdateHistory(int page, int size,
    HistoryUpdateRequest historyUpdateRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnBaseRestResponse itemListingUpdateV2(String productSku,
    ProductLevel3QuickEditV2Request quickEditV2WebRequests,boolean isExternalOnly) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
      false, null);
  }

  @Override
  public GdnRestListResponse<ItemPickupPointListingL3Response> getItemPickupPointL3Listing(int page, int size,
      boolean onlyDefaultViewConfig, boolean concatenateValueWithValueType, ItemPickupPointListingL3Request itemPickupPointListingL3Request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<EditProductV2Response> editProductV2Info(
    ProductL3UpdateRequest product, String productSku, boolean isOnlyExternal) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductLevel3DetailsV2Response> fetchL3V2ProductDetailsByProductSku(
    String productSku, boolean isNeedCorrection) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> editL5PriceStockInfo(
      ProductVariantUpdateRequest productVariantUpdateRequest) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductCountResponse> getProductCountForProductLimit(String businessPartnerCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<AppealProductConfigResponse> getAppealProductConfig(String storeId,
    String requestId, String businessPartnerCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, requestId);
  }

  @Override
  public GdnRestSingleResponse<OmniChannelMapAndSkuResponse> checkOmniChannelSkuExistsInSeller(
      OmniChannelExistsRequest omniChannelSkuRequest) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<ItemSummaryL4Response> getL4ProductDetailsByProductSku(String productSku, int page,
      Integer size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnBaseRestResponse updateDistributionInfo(String productCode,
      DistributionInfoUpdateRequest distributionInfoUpdateRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }
}
