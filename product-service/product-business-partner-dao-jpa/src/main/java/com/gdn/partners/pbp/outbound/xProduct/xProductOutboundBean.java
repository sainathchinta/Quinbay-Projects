package com.gdn.partners.pbp.outbound.xProduct;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import com.gda.mta.product.dto.EditFlagChangesDTO;
import com.gda.mta.product.dto.NeedCorrectionProductActivationRequest;
import com.gda.mta.product.dto.ProductDetailEditDTO;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gdn.partners.pbp.helper.RequestHelper;
import com.gdn.partners.pbp.helper.ResponseHelper;
import com.gdn.partners.pbp.outbound.xProduct.feign.ProductBasicMasterFieldsRequest;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.rest.web.model.ActivateNeedRevisionResponse;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.L3VersionResponse;
import com.gdn.x.product.rest.web.model.enums.EditChangeType;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRequest;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest;
import com.gdn.x.product.rest.web.model.request.CogsUpdateListRequest;
import com.gdn.x.product.rest.web.model.request.CreateFbbPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointDeleteRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequestV2;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest;
import com.gdn.x.product.rest.web.model.request.ProductDetailPageEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuAndProductCodeRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;
import com.gdn.x.product.rest.web.model.response.AddProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.CreateFbbPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.DuplicateProductDetailsResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointBasicResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointL5Response;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.MinMaxItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.product.rest.web.model.response.ProductCenterDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductL5DetailResponse;

import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.CogsResponse;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.xProduct.feign.XProductFeign;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.vo.SimpleStringBooleanMapRequest;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.ItemsSummaryDetailRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.request.QuickEditUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerResponse;
import com.gdn.x.product.rest.web.model.response.DeleteOfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemImagesListResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductPickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductTypeResponse;
import com.gdn.x.product.rest.web.model.response.SharedProductBundleRecipeResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import com.gdn.x.product.rest.web.model.util.GdnRestSimpleResponse;

import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class xProductOutboundBean implements XProductOutbound {

  private static final String XPRODUCT_ERROR_MESSAGE = "Failed to get response from XPRODUCT : {}";
  private static final String ITEM_SKU_LIST_EMPTY = "itemSkusList empty error, list size : {} ";

  @Autowired
  private XProductFeign xProductFeign;

  @Value("${convert.pre.order.date.to.jkt}")
  private boolean convertPreOrderDateToJKT;

  @Override
  public ProductResponse updateProduct(boolean isOnlyExternal, ProductRequest productRequest) throws Exception {
    if (convertPreOrderDateToJKT && Objects.nonNull(productRequest) && Objects.nonNull(productRequest.getPreOrder())) {
      productRequest.getPreOrder().setConvertToJKT(true);
    }
    GdnRestSingleResponse<ProductResponse> response =
        xProductFeign.updateProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), isOnlyExternal, productRequest);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public ItemResponse updateItem(boolean isOnlyExternal, boolean isProductTypeChanged, boolean isPreOrderChanged,
      ItemRequest itemRequest)
      throws Exception {
    GdnRestSingleResponse<ItemResponse> response = xProductFeign
        .updateItem(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), isOnlyExternal, isProductTypeChanged, isPreOrderChanged, itemRequest);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public void updateItemViewConfigAndForceReview(boolean forceReview,
      List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuListRequest, boolean isArchived) {
    GdnBaseRestResponse response =
        xProductFeign.updateItemViewConfigAndForceReview(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), forceReview, isArchived, false,
            itemViewConfigAndItemSkuListRequest);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public void updateItemViewConfigAndForceReview(boolean forceReview,
      List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuListRequest, boolean isArchived, boolean scheduleRemoval) {
    GdnBaseRestResponse response =
        xProductFeign.updateItemViewConfigAndForceReview(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), forceReview, isArchived, scheduleRemoval,
            itemViewConfigAndItemSkuListRequest);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItems(boolean showDeleted, String productSku,
      boolean includeForceReview) {
    GdnRestSingleResponse<ProductAndItemsResponse> response = this.xProductFeign
        .getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), showDeleted, productSku, includeForceReview);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response;
  }

  @Override
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItemsWithProductData(boolean showDeleted,
      String productSku, boolean includeForceReview, boolean isMigrateAndSyncProduct) {
    GdnRestSingleResponse<ProductAndItemsResponse> response =
        this.xProductFeign.getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), showDeleted, productSku, includeForceReview, true,
            isMigrateAndSyncProduct);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response;
  }


  @Override
  public List<DeleteOfflineItemResponse> bulkDeleteOfflineItem(String merchantCode,
      List<DeleteOfflineItemRequest> deleteOfflineItemRequests) throws Exception {
    GdnRestListResponse<DeleteOfflineItemResponse> response = xProductFeign.bulkDeleteOfflineItem(
        GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), merchantCode, deleteOfflineItemRequests);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public void updateMigratedProductCode(String productSku, String newProductCode, boolean rollback)
      throws Exception {
    GdnBaseRestResponse response = xProductFeign
        .updateMigratedProductCode(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productSku, newProductCode, rollback);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public List<BusinessPartnerResponse> getBusinessPartnerDetails(List<String> businessPartnerCodes) throws Exception {
    GdnRestListResponse<BusinessPartnerResponse> response = xProductFeign
        .getBusinessPartnerDetails(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            new SimpleListStringRequest(businessPartnerCodes));
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      return new ArrayList<>();
    }
    return response.getContent();
  }

  @Override
  public Page<ProductSkuSummaryResponse> getProductSkuSummary(String businessPartnerCode,
      ProductSkuSummaryRequest productSkuSummaryRequest, int page, int size) {
    GdnRestListResponse<ProductSkuSummaryResponse> response = xProductFeign
        .getProductSkuSummary(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            businessPartnerCode, page, size, productSkuSummaryRequest);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public Long updatePickupPointCodes(PickupPointUpdateRequest pickupPointUpdateRequest)
      throws Exception {
    GdnRestSingleResponse<SimpleLongResponse> response = xProductFeign
        .updatePickupPointCodes(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), pickupPointUpdateRequest);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue().getValue();
  }

  @Override
  public Set<String> getPickupPointCodesByProductSku(String productSku) throws Exception {
    GdnRestSingleResponse<ProductPickupPointListResponse> response = xProductFeign
        .getPickupPointCodesByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productSku);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue().getPickupPointCodes();
  }

  @Override
  public Page<ItemSummaryDetailResponse> findSummaryDetailsByFilter(ItemsSummaryDetailRequest filter, Pageable pageRequest) throws Exception {
    GdnRestListResponse<ItemSummaryDetailResponse> response = xProductFeign
        .getItemsSummaryDetailByFilter(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            pageRequest.getPageNumber(), pageRequest.getPageSize(), Constants.ALL, filter);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), pageRequest, response.getPageMetaData().getTotalRecords());
  }

  @Override
  public GdnRestSingleResponse<ProductL3Response> getProductDetailsByProductSku(String productSku) throws Exception {
    GdnRestSingleResponse<ProductL3Response> response = this.xProductFeign
        .getProductDetailsByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productSku);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response;
  }

  @Override
  public void toggleArchiveProduct(String productSku, boolean doArchive) throws Exception {
    GdnBaseRestResponse response = this.xProductFeign
        .toggleArchiveProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), doArchive,
            productSku);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public void updateItemListing(String productSku, ProductType productType,
      List<QuickEditUpdateRequest> quickEditRequest) {
    GdnBaseRestResponse response = xProductFeign
        .updateItemListing(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productSku,
            new ItemListingUpdateRequest(productType, quickEditRequest));
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public List<ItemLevel5Response> getL5ItemListing(Set<String> productSkus, List<String> pickupPointCodes,
      List<String> promoTypes, boolean fetchB2bData, String fetchViewConfigByChannel) {
    GdnRestListResponse<ItemLevel5Response> response =
        xProductFeign.getL5ItemList(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), true, fetchB2bData,
            fetchViewConfigByChannel,
            new ItemLevel4ListingWebRequest(productSkus, pickupPointCodes, promoTypes));
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public void updateOff2OnActiveFlagByProductSku(Map<String, Boolean> off2OnFLagByProductSkuMap) {
    GdnBaseRestResponse response = xProductFeign
        .bulkUpdateOff2OnActiveFlagByProductSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(),
            new SimpleStringBooleanMapRequest(off2OnFLagByProductSkuMap));
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public L3VersionResponse generateProductScoreByProductSkuOrProductCode(String productSku, String productCode,
      boolean updateCategory) {
    log.info("Updating the product score for product with product code : {} , product sku : {}", productCode,
        productSku);
    GdnRestSingleResponse<L3VersionResponse> response = xProductFeign
        .generateProductScoreByProductSkuOrProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, updateCategory,
            productSku, productCode);
    if (!response.isSuccess()) {
      log.error("Error in updating product score for product code : {}, product sku : {} ", productCode, productSku);
    }
    return response.getValue();
  }

  @Override
  public DuplicateProductDetailsResponse validateDuplicateProductBySellerSku(String sellerSku, String merchantCode) {
    GdnRestSingleResponse<DuplicateProductDetailsResponse> response =
        xProductFeign.validateDuplicateProductBySellerSku(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.DEFAULT_USERNAME, sellerSku, merchantCode);
    if (!response.isSuccess() && !response.getErrorMessage()
        .equals(ErrorMessages.PRODUCT_ALREADY_EXIST_WITH_THE_SELLER_SKU)) {
      log.error("Error while validate duplicate product by sellerSku. sellerSku : {} , merchantCode : {} , error - ",
          sellerSku, merchantCode, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public List<PrdProductResponse> getPrdProductDetailByProductSkuOrProductCode(
      ProductSkuAndProductCodeRequest prdProductRequest) {
    GdnRestListResponse<PrdProductResponse> response =
        xProductFeign.getPrdProductDetailByProductSkuOrProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.DEFAULT_USERNAME, prdProductRequest);
    if (!response.isSuccess()) {
      log.error(
          "Error in get prd_product by productSku or productCode. prdProductRequest : {} , requestId : {} , error - ",
          prdProductRequest, GdnMandatoryRequestParameterUtil.getRequestId(), response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public Page<ItemPickupPointCodeResponse> getItemPickupPointCodeResponse(String productSku, int page, int size,
      boolean fbbActivated) {
    GdnRestListResponse<ItemPickupPointCodeResponse> response =
        xProductFeign.getItemPickupPointCodeByProductSku(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, page,
            size, productSku, fbbActivated);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void updateContentChange(String productSku, boolean contentChange, boolean publishItems) throws Exception {
    GdnBaseRestResponse response = this.xProductFeign.updateContentChange(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), productSku,
        contentChange, publishItems);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public ProductCountResponse getProductCountByType(String businessPartnerCode, String type) throws Exception {
    GdnRestSingleResponse<ProductCountResponse> response = this.xProductFeign.getProductCountByType(
        GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), type, businessPartnerCode);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndSingleItemByItemSku(String itemSku,
      boolean convertPreOrderDetails) {
    GdnRestSingleResponse<ProductAndItemsResponse> response = this.xProductFeign
        .getProductAndSingleItemByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), itemSku, convertPreOrderDetails, true);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response;
  }

  @Override
  public void updateEditedProduct(ProductEditRequest productEditRequest, boolean updateCategory) {
    if (convertPreOrderDateToJKT && Objects.nonNull(productEditRequest) && Objects.nonNull(
        productEditRequest.getProductRequest()) && Objects.nonNull(
        productEditRequest.getProductRequest().getPreOrder())) {
      productEditRequest.getProductRequest().getPreOrder().setConvertToJKT(true);
    }
    GdnBaseRestResponse response = this.xProductFeign
        .updateEditedProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), updateCategory, productEditRequest);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public List<ItemImagesListResponse> getListOfImagesByItemSkus(SimpleSetStringRequest itemSkusSet) {
    GdnRestListResponse<ItemImagesListResponse> response = this.xProductFeign
        .getListOfImagesByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), itemSkusSet);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public ActivateNeedRevisionResponse activateOnNeedCorrection(NeedCorrectionProductActivationRequest request) {
    GdnRestSimpleResponse<ActivateNeedRevisionResponse> response = this.xProductFeign
        .activateOnNeedCorrection(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), request);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public ProductTypeResponse getProductTypeByProductCode(String productCode) {
    GdnRestSingleResponse<ProductTypeResponse> response =
        new GdnRestSingleResponse<>(null, GdnMandatoryRequestParameterUtil.getRequestId());
    try {
      response = xProductFeign.getProductDetailsByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), productCode);
    } catch (ApplicationRuntimeException ex) {
      log.error(XPRODUCT_ERROR_MESSAGE + ": {}", productCode, ex.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public void updateWholeSaleActivationFlag(List<String> itemSkus, boolean wholeSalePriceActivated) {
    GdnBaseRestResponse response =
        xProductFeign.updateWholeSaleActivationFlag(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            wholeSalePriceActivated, new SimpleListStringRequest(itemSkus));
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE + " itemSkus : {}",itemSkus, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public List<BusinessPartnerPickupPointResponse> getPickupPointDetailsByListOfPickupPointCodes(
    List<String> pickupPointCodes) {
    GdnRestListResponse<BusinessPartnerPickupPointResponse> response =
      this.xProductFeign.getBusinessPartnerPickupPointDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        new SimpleListStringRequest(pickupPointCodes));
    if (!response.isSuccess()) {
      log.error("Failed to fetch pickupPointCodes : {}, error - {}", pickupPointCodes,
        response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public GdnRestListResponse<ItemSkuPickupPointCodeResponse> getItemPickupPointCodeByItemSkus(
      SimpleListStringRequest itemSkusList) {
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> gdnRestListResponse =
        new GdnRestListResponse<>(StringUtils.EMPTY, null, true, null);
    if (!itemSkusList.getValue().isEmpty()) {
      gdnRestListResponse = xProductFeign
          .getItemPickupPointCodeByItemSkus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, itemSkusList);
    } else {
      log.error(ITEM_SKU_LIST_EMPTY, itemSkusList.getValue().size());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, gdnRestListResponse.getErrorMessage());
    }
    if (!gdnRestListResponse.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, gdnRestListResponse.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, gdnRestListResponse.getErrorMessage());
    }
    return gdnRestListResponse;
  }

  @Override
  public List<ItemSummaryListResponse> getItemPickupPointSummary(List<ItemPickupPointRequest> itemPickupPointRequest,
      String fetchViewConfigByChannel) {
    GdnRestListResponse<ItemSummaryListResponse> response =
      this.xProductFeign.findItemSummaryByItemSkuAndPickupPointCode(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), fetchViewConfigByChannel, itemPickupPointRequest);
    if (!response.isSuccess()) {
      log.error("Failed to fetch itemPickupPoint Summary : {}, error - {}", itemPickupPointRequest,
        response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public void updateItemPickupPointListing(String productSku, ProductType productType,
    List<ItemPickupPointQuickEditRequest> itemPickupPointListingUpdateRequestVos) {
    GdnBaseRestResponse response =
      this.xProductFeign.updateItemPickupPointListing(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), productSku,
        new ItemPickupPointListingUpdateRequest(productType,
          itemPickupPointListingUpdateRequestVos));
    if (!response.isSuccess()) {
      log.error("Failed to update listing for itemPickupPoints : {}, error - {}",
        itemPickupPointListingUpdateRequestVos, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public void updateMasterDataFieldsInProduct(String productSku) {
    GdnBaseRestResponse response =
        xProductFeign.updateMasterDataFieldsInProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productSku);
    if (!response.isSuccess()) {
      log.error("Error in updating master data fields for product sku : {} , {}", productSku,
          response.getErrorMessage());
    }
  }

  @Override
  public AddProductAndItemsResponse addProductAndItems(String storeId, String channelId, String clientId,
      String requestId, String username, ProductAndItemActivationRequest productAndItemActivationRequest)
      throws ApplicationException {
    GdnRestSingleResponse<AddProductAndItemsResponse> response =
        xProductFeign.addProductAndItems(storeId, channelId, clientId, requestId, username,
            productAndItemActivationRequest);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public Page<ItemPickupPointListingResponse> getItemPickupPointList(String storeId, String channelId, String clientId,
      String requestId, String username, int page, int size,
      ItemPickupPointListingRequest itemPickupPointListingRequest) {
    GdnRestListResponse<ItemPickupPointListingResponse> response =
        xProductFeign.getItemPickupPointList(storeId, channelId, clientId, requestId, username, page, size,
            itemPickupPointListingRequest, Constants.ALL);
    if (Objects.isNull(response.getContent()) || !response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public List<PickupPointDetailResponse> getPickupPointDetailResponse(List<String> pickupPointCodes,
      boolean fbbActivated) {
    GdnRestListResponse<PickupPointDetailResponse> response =
        this.xProductFeign.getPickupPointDetailByCodes(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        new SimpleListStringRequest(pickupPointCodes, fbbActivated));
    if (!response.isSuccess()) {
      log.error("Failed to fetch Pickup Point Names for PP Codes : {}, error - {}",
        pickupPointCodes, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }
  @Override
  public GdnRestListResponse<ProductL3SummaryResponse> filterSummaryL3(int page,
      int size, ProductSummaryRequest productSummaryRequest) throws ApplicationException {
    GdnRestListResponse<ProductL3SummaryResponse> response = xProductFeign.getFilterSummaryL3(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        page, size, productSummaryRequest);
    if (!response.isSuccess()) {
      log.error("Failed filterSummary L3 for request : {}, error - ",
          productSummaryRequest, response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response;
  }

  @Override
  public GdnRestListResponse<ProductSummaryResponseV2> getProductSummary(int page, int size,
    ProductSummaryRequestV2 productSummaryRequestV2) throws ApplicationException {
    GdnRestListResponse<ProductSummaryResponseV2> response = xProductFeign
      .getProductSummary(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), page, size, productSummaryRequestV2);
    if (!response.isSuccess()) {
      log.error("Failed productSummaryRequestV2 L3 for request : {}, error - ",
        productSummaryRequestV2, response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
        "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response;
  }

  @Override
  public GdnRestSingleResponse<SimpleMapStringResponse> getItemNameByItemSkus(SimpleListStringRequest itemSkus,
      boolean includeMarkForDelete) throws ApplicationException {
    GdnRestSingleResponse<SimpleMapStringResponse> response =
        xProductFeign.getItemNameByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            includeMarkForDelete, itemSkus);
    if (!response.isSuccess()) {
      log.error("Failed request with item skus : {}, error - ", itemSkus, response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response;
  }

  @Override
  public EditItemResponse updateItemPickupPoints(String storeId, String productSku, ProductType productType,
      List<ItemPickupPointQuickEditRequest> quickEditUpdateRequests,
      List<ItemPickupPointQuickEditRequest> addPickupPointRequests,
      List<ItemPickupPointDeleteRequest> deletePickupPointRequests, Boolean online, boolean isOnlineFlagChanged,
      Boolean cnc, boolean isCNCFlagChangedAtL3Level, Boolean fbbActivated, boolean isFbbFlagChangedAtL3Level,
      AddDeleteVariantRequest addDeleteVariantRequest, ProductVariantUpdateRequest productVariantUpdateRequest,
      EditFlagChangesDTO editFlagChangesDTO) {
    log.info("product bundle recipe for update in x-product : {} ", productVariantUpdateRequest.getProductBundleRecipe());
    if (RequestHelper.isEligibleForXProductUpdate(quickEditUpdateRequests, addPickupPointRequests,
        deletePickupPointRequests, isOnlineFlagChanged, isCNCFlagChangedAtL3Level, isFbbFlagChangedAtL3Level,
        addDeleteVariantRequest, editFlagChangesDTO, productVariantUpdateRequest.getProductBundleRecipe())) {
      ItemPickupPointUpdateRequest itemPickupPointUpdateRequest = new ItemPickupPointUpdateRequest();
      itemPickupPointUpdateRequest.setProductSku(productSku);
      itemPickupPointUpdateRequest.setOnline(online);
      itemPickupPointUpdateRequest.setCnc(cnc);
      if (Objects.nonNull(fbbActivated)) {
        itemPickupPointUpdateRequest.setFbbActivated(fbbActivated);
      }
      itemPickupPointUpdateRequest.setProductType(productType);
      itemPickupPointUpdateRequest.setQuickEditUpdateRequests(quickEditUpdateRequests);
      itemPickupPointUpdateRequest.setAddPickupPointRequests(addPickupPointRequests);
      itemPickupPointUpdateRequest.setDeletePickupPointRequests(deletePickupPointRequests);
      itemPickupPointUpdateRequest.setAddDeleteVariantRequest(addDeleteVariantRequest);
      itemPickupPointUpdateRequest.setB2bActivated(productVariantUpdateRequest.getB2bActivated());
      itemPickupPointUpdateRequest.setB2cActivated(productVariantUpdateRequest.getB2cActivated());
      RequestHelper.setBundleProductRequests(productVariantUpdateRequest, itemPickupPointUpdateRequest);
      ResponseHelper.setItemPickupPointUpdateRequestForAutoCategoryChange(
        editFlagChangesDTO.isTakeActionOnShippingForAutoCategoryChange(),
        itemPickupPointUpdateRequest);
      GdnRestSingleResponse<EditItemResponse> response = xProductFeign
          .updateItemPickupPoints(storeId, Constants.DEFAULT_CHANNEL_ID, GdnMandatoryRequestParameterUtil.getClientId(),
              Constants.DEFAULT_REQUEST_ID, GdnMandatoryRequestParameterUtil.getUsername(), itemPickupPointUpdateRequest);
      if (!response.isSuccess()) {
        log.error("Failed to update Pickup Point for request : {}, error - {}", itemPickupPointUpdateRequest,
            response.getErrorMessage());
        ErrorCategory errorCategory = ErrorCategory.UNSPECIFIED;
        String errorMessage = response.getErrorMessage();
        if(ErrorCategory.VALIDATION.name().equals(response.getErrorCode())) {
          errorCategory = ErrorCategory.VALIDATION;
          errorMessage = errorMessage.replace(ErrorCategory.VALIDATION.getMessage(),  StringUtils.EMPTY);
        } else if (Objects.nonNull(response.getValue().getApiErrorCode())) {
          errorCategory = ErrorCategory.VALIDATION;
          errorMessage = response.getValue().getApiErrorCode().getDesc();
        }
        throw new ApplicationRuntimeException(errorCategory, errorMessage);
      }
      return response.getValue();
    }
    return new EditItemResponse();
  }

  @Override
  public List<ItemResponseV2> getItemPickupPointsByItemSku(ItemRequestV2 itemRequestV2) throws ApplicationException {
    GdnRestListResponse<ItemResponseV2> response = xProductFeign
        .getItemPickupPointsByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), 0, itemRequestV2.getItemSkuList().size(), itemRequestV2);
    if (!response.isSuccess()) {
      log.error("Failed productSummaryRequestV2 L3 for request : {}, error - ",
          itemRequestV2, response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, " [" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public Page<ItemResponseV2> getItemPickupPointSummary(int page, int size,
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest) {
    GdnRestListResponse<ItemResponseV2> response =
        this.xProductFeign.getItemPickupPointSummary(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), page, size,
            Constants.ALL, itemPickupPointSummaryRequest);
    if (!response.isSuccess()) {
      log.error("Failed to fetch itemPickup point details request : {}, page : {}, size : {}, "
          + "error - {}", itemPickupPointSummaryRequest, page, size, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
      response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void updateCncActivationFlag(List<String> itemSkus) {
    GdnBaseRestResponse response =
        this.xProductFeign.updateCncActivationFlag(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            new SimpleListStringRequest(itemSkus));
    if (!response.isSuccess()) {
      log.error("Failed to update cnc activation flag for itemSkus : {}, error - {}",
          itemSkus, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public List<DeleteItemPickupPointResponse> deleteActiveItemPickupPointByPickupPointCode(
    DeleteItemPickupPointRequest deleteItemPickupPointRequest) {
    GdnRestListResponse<DeleteItemPickupPointResponse> response = this.xProductFeign
      .deleteActiveItemPickupPointByPickupPointCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), deleteItemPickupPointRequest);
    if (!response.isSuccess()) {
      log.error("Failed to delete active l5 for pp code {}, error - {} ",
        deleteItemPickupPointRequest.getPickupPointCode(), response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public CreateFbbPickupPointResponse createFbbPickupPoint(CreateFbbPickupPointRequest createFbbPickupPointRequest) {
    GdnRestSingleResponse<CreateFbbPickupPointResponse> response =
        this.xProductFeign.createFbbPickupPoint(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            createFbbPickupPointRequest);
    if (!response.isSuccess()) {
      log.error("Failed to create Fbb pickup point in x-product for item sku = {} , pickupPointCode = {} , error = {} ",
          createFbbPickupPointRequest.getItemSku(), createFbbPickupPointRequest.getPickupPointCode(),
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public MinMaxItemPriceResponse getMinAndMaxOfferPrice(String productCode) {
    GdnRestSimpleResponse<MinMaxItemPriceResponse> response = this.xProductFeign
        .getMinAndMaxOfferPrice(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productCode);
    if (!response.isSuccess()) {
      log.error("Failed to get min and max price for productCode : {}, error : {} ",
          productCode, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public List<ProductL5DetailResponse> findProductAndItemByItemSkuAndPickupPointCode(
    List<ItemPickupPointRequest> itemPickupPointRequest) {
    GdnRestListResponse<ProductL5DetailResponse> response =
      xProductFeign.findProductAndItemByItemSkuAndPickupPointCode(
        GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), Constants.ALL, itemPickupPointRequest);
    if (!response.isSuccess()) {
      log.error("Failed to get response from xproduct  error : {} ", response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<ItemBasicDetailV2Response> findItemBasicDetailsByProductSku(String productSku) {
    GdnRestListResponse<ItemBasicDetailV2Response> response =
        xProductFeign.getItemBasicDetailsByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productSku);
    if (!response.isSuccess()) {
      log.error("Failed to get response from xproduct  error : {} ", response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public BasicProductResponse getBasicProductInfo(String productSku) {
    GdnRestSingleResponse<BasicProductResponse> response =
        xProductFeign.basicProductInfo(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productSku);
    if (!response.isSuccess()) {
      log.error("Failed to get response from x-product  error : {} ", response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public BasicProductResponse getBasicProductInfoV2(String productSku) {
    GdnRestSingleResponse<BasicProductResponse> response =
        xProductFeign.getBasicProductInfo(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productSku);
    return response.getValue();
  }

  @Override
  public CombinedEditItemResponse updateEditedProductAndItemPickupPoint(String productSku,
    boolean updateCategory, ProductDetailPageEditRequest productDetailPageEditRequest) {
    if (convertPreOrderDateToJKT && Objects.nonNull(productDetailPageEditRequest.getProductEditRequest())
        && Objects.nonNull(productDetailPageEditRequest.getProductEditRequest().getProductRequest()) && Objects.nonNull(
        productDetailPageEditRequest.getProductEditRequest().getProductRequest().getPreOrder())) {
      productDetailPageEditRequest.getProductEditRequest().getProductRequest().getPreOrder().setConvertToJKT(true);
    }
    GdnRestSingleResponse<CombinedEditItemResponse> response =
      xProductFeign.updateEditedProductAndItemPickupPoint(
        GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), updateCategory, productSku,
        productDetailPageEditRequest);
    if (!response.isSuccess()) {
      log.error("Failed to update Pickup Point for request : {}, error - {}", productDetailPageEditRequest,
          response.getErrorMessage());
      ErrorCategory errorCategory = ErrorCategory.UNSPECIFIED;
      String errorMessage = response.getErrorMessage();
      if(ErrorCategory.VALIDATION.name().equals(response.getErrorCode())) {
        errorCategory = ErrorCategory.VALIDATION;
        errorMessage = errorMessage.replace(ErrorCategory.VALIDATION.getMessage(), StringUtils.EMPTY);
      } else if (Objects.nonNull(response.getValue().getApiErrorCode())) {
        errorCategory = ErrorCategory.VALIDATION;
        errorMessage = response.getValue().getApiErrorCode().getDesc();
      }
      throw new ApplicationRuntimeException(errorCategory, errorMessage);
    }
    return response.getValue();
  }

  @Override
  public void updateItemPickupPointsForCombinedEdit(String storeId, String productSku,
      ProductType productType, List<ItemPickupPointQuickEditRequest> quickEditUpdateRequests,
      List<ItemPickupPointQuickEditRequest> addPickupPointRequests,
      List<ItemPickupPointDeleteRequest> deletePickupPointRequests, Boolean online, boolean isOnlineFlagChanged,
      Boolean cnc, boolean isCNCFlagChangedAtL3Level, Boolean fbbActivated, boolean isFbbFlagChangedAtL3Level,
      AddDeleteVariantRequest addDeleteVariantRequest, ProductVariantUpdateRequest productVariantUpdateRequest,
      EditFlagChangesDTO editFlagChangesDTO, ProductDetailEditDTO productDetailEditDTO) {
      ProductDetailPageEditRequest requestForXProduct =
      Optional.ofNullable(productDetailEditDTO.getProductDetailEditRequestForXProduct())
        .orElse(new ProductDetailPageEditRequest());
    if (RequestHelper.isEligibleForXProductUpdate(quickEditUpdateRequests, addPickupPointRequests,
        deletePickupPointRequests, isOnlineFlagChanged, isCNCFlagChangedAtL3Level, isFbbFlagChangedAtL3Level,
        addDeleteVariantRequest, editFlagChangesDTO, productVariantUpdateRequest.getProductBundleRecipe())) {
      ItemPickupPointUpdateRequest itemPickupPointUpdateRequest = new ItemPickupPointUpdateRequest();
      itemPickupPointUpdateRequest.setProductSku(productSku);
      itemPickupPointUpdateRequest.setOnline(online);
      itemPickupPointUpdateRequest.setCnc(cnc);
      if (Objects.nonNull(fbbActivated)) {
        itemPickupPointUpdateRequest.setFbbActivated(fbbActivated);
      }
      itemPickupPointUpdateRequest.setProductType(productType);
      itemPickupPointUpdateRequest.setQuickEditUpdateRequests(quickEditUpdateRequests);
      itemPickupPointUpdateRequest.setAddPickupPointRequests(addPickupPointRequests);
      itemPickupPointUpdateRequest.setDeletePickupPointRequests(deletePickupPointRequests);
      itemPickupPointUpdateRequest.setAddDeleteVariantRequest(addDeleteVariantRequest);
      itemPickupPointUpdateRequest.setB2bActivated(productVariantUpdateRequest.getB2bActivated());
      itemPickupPointUpdateRequest.setB2cActivated(productVariantUpdateRequest.getB2cActivated());
      RequestHelper.setBundleProductRequests(productVariantUpdateRequest, itemPickupPointUpdateRequest);
      requestForXProduct
          .setItemPickupPointUpdateRequest(itemPickupPointUpdateRequest);
      EditChangeType editChangeType;
      if (EditChangeType.CONTENT.equals(
          requestForXProduct.getEditChangeType())) {
        editChangeType = EditChangeType.CONTENT_AND_ITEM_PICKUP_POINT;
      } else {
        editChangeType = EditChangeType.ITEM_PICKUP_POINT;
      }
      requestForXProduct.setEditChangeType(editChangeType);
    }
    productDetailEditDTO.setProductDetailEditRequestForXProduct(requestForXProduct);
  }

  @Override
  public List<ProductBasicResponse> getProductBasicDetails(List<String> productSkus) {
    GdnRestListResponse<ProductBasicResponse> response =
        xProductFeign.getProductBasicDetails(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            new SimpleListStringRequest(productSkus));
    if (!response.isSuccess()) {
      log.error("Failed to basic detail response from xproduct productSkus : {},  error : {} ", productSkus,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<ItemBasicDetailV2Response> getItemBasicDetailV2Response(List<String> itemSkus,
      boolean fetchBundleRecipe) {
    GdnRestListResponse<ItemBasicDetailV2Response> response =
        xProductFeign.getItemBasicDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            fetchBundleRecipe, new SimpleListStringRequest(itemSkus));
    if (!response.isSuccess()) {
      log.error("Failed to basic detail response from xproduct itemsSkus : {},  error : {} ", itemSkus,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<ItemBasicDetailV2Response> getItemBasicDetailsByItemSkus(boolean inAllProducts,
      SimpleListStringRequest itemSkuList) {
    GdnRestListResponse<ItemBasicDetailV2Response> response =
        xProductFeign.getItemBasicDetailsByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), inAllProducts, itemSkuList);
    if (!response.isSuccess()) {
      log.error("Failed to get response from x-product error : {} ", response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public ProductCountResponse getSecondaryCounts(String type, String businessPartnerCode) {
    GdnRestSingleResponse<ProductCountResponse> response = xProductFeign
        .getSecondaryCounts(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), type,
            businessPartnerCode);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error("Failed to get Secondary Filter Counts from X-product error : {} ", response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public SimpleLongResponse getL5CountByItemSku(String itemSku) {
    GdnRestSingleResponse<SimpleLongResponse> response =
        xProductFeign.getL5CountByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), itemSku);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error("Failed to get counts from X-product itemSku : {} , error : {} ", itemSku, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public List<ItemPickupPointCodeResponse> reconcileProductVariants(
      AddDeleteVariantRetryRequest addDeleteVariantRetryRequest, String productSku) {
    GdnRestListResponse<ItemPickupPointCodeResponse> response =
        xProductFeign.reconcileProductVariants(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), productSku,
            addDeleteVariantRetryRequest);

    if (!response.isSuccess()) {
      log.error("error while reconcile product variants : request :{} error : {} ", addDeleteVariantRetryRequest,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<SharedProductBundleRecipeResponse> getSharedProductBundleRecipeDetails(Set<String> itemCodes) {
    GdnRestListResponse<SharedProductBundleRecipeResponse> response =
        xProductFeign.getSharedProductBundleRecipeDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            new SimpleSetStringRequest(itemCodes));

    if (!response.isSuccess()) {
      log.error("error while getting shared product bundle details, request : {} error : {} ", itemCodes,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<ItemPickupPointBasicResponse> fetchBasicDetailsByItemSkuAndPickupPointCodeList(
    List<ItemPickupPointRequest> request) {
    GdnRestListResponse<ItemPickupPointBasicResponse> response =
      xProductFeign.fetchBasicDetailsByItemSkuAndPickupPointCodeList(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), request);
    if (!response.isSuccess()) {
      log.error("Error while getting Basic L5 response details, request : {} error : {} ", request,
        response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public ProductCenterDetailResponse getProductSkuDetailResponse(String productSku) {
    GdnRestSingleResponse<ProductCenterDetailResponse> response =
      xProductFeign.getProductSkuDetailResponse(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), productSku);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error("Failed to get detail response for productSku = {}  from X-product error : {} ",
        productSku, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public void migrateProductAndL5DetailByProductSku(
    ProductAndL5MigrationRequest productAndL5MigrationRequest, String storeId, String username) {
    GdnBaseRestResponse response =
      xProductFeign.migrateProductAndL5DetailByProductSku(storeId, Constants.PBP,
        Constants.DEFAULT_CHANNEL_ID, Constants.PBP, username, productAndL5MigrationRequest);
    if (!response.isSuccess()) {
      log.error("Migration attempt Failed for Product Sku : {} , error from x-product : {} ",
        productAndL5MigrationRequest.getProductSku(), response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public GdnRestListResponse<ItemPickupPointL5Response> fetchL5ResponsesByItemSkus(
    List<String> itemSkus, int page, int size) {
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest(itemSkus, true);
    GdnRestListResponse<ItemPickupPointL5Response> response =
      xProductFeign.findByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        GdnMandatoryRequestParameterUtil.getRequestId(), page, size, simpleListStringRequest);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response;
  }

  @Override
  public void updateProductMasterDataInfo(String userName,
    ProductBasicMasterFieldsRequest productBasicMasterFieldsRequest) {
    GdnBaseRestResponse response =
      xProductFeign.updateProductMasterDataInfo(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), userName, productBasicMasterFieldsRequest);
    if (!response.isSuccess()) {
      log.error("Failed to update product master data info for product sku : {} , error : {} ",
        productBasicMasterFieldsRequest.getProductSku(), response.getErrorMessage());
      throw new ApiIncorrectInputDataException(ErrorCategory.UNSPECIFIED.getMessage(), response.getErrorMessage());
    }
  }

  @Override
  public SimpleBooleanResponse getCncAtL5ByProductSku(String productSku) {
    GdnRestSimpleResponse<SimpleBooleanResponse> cncAtL5ByProductSku =
      xProductFeign.getCncAtL5ByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), productSku);
    if (!cncAtL5ByProductSku.isSuccess() || Objects.isNull(cncAtL5ByProductSku.getValue())) {
      log.error(XPRODUCT_ERROR_MESSAGE, cncAtL5ByProductSku.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, cncAtL5ByProductSku.getErrorMessage());
    }
    return cncAtL5ByProductSku.getValue();
  }

  @Override
  public void updateCogsValue(String productSku, CogsUpdateListRequest request) throws Exception {
    GdnBaseRestResponse response =
        xProductFeign.updateCogsValue(productSku, GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), request);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public List<CogsResponse> getCogsData(String productSku, int page, int size) throws Exception {
    GdnRestListResponse<CogsResponse> response =
        xProductFeign.getCogsData(productSku, GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), page, size);
    if (!response.isSuccess()) {
      log.error(XPRODUCT_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }
}
