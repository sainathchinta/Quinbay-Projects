package com.gdn.mta.bulk.service;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.mta.bulk.models.ProductL3Response;
import com.gdn.mta.bulk.models.download.responsedata.BulkDownloadProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductBasicResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductSkuResponse;
import com.gdn.x.product.rest.web.model.L3VersionResponse;
import com.gdn.x.product.rest.web.model.request.GetProductInfoRequestV2;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.PriceRangeResponse;
import com.gdn.mta.bulk.models.GenericErrorMessages;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.ItemBasicL4Response;
import com.gdn.mta.bulk.feignConfig.XProductFeign;
import com.gdn.mta.bulk.models.GenericErrorMessages;
import com.gdn.mta.bulk.models.ProductL3Response;
import com.gdn.mta.bulk.models.download.responsedata.BulkDownloadProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductBasicResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductSkuResponse;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.product.model.vo.SimpleStringBooleanMapRequest;
import com.gdn.x.product.model.vo.UnmappedSkuResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.GetProductInfoRequestV2;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemCodeDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemL5ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.PriceRangeResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SimpleListStringResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class XProductOutboundServiceImpl implements XProductOutboundService {

  @Autowired
  private XProductFeign xProductFeign;

  @Value("${dormant.seller.error.suppress.error.codes}")
  private String dormantSellerErrorSuppressErrorCodes;

  @Override
  public List<UnmappedSkuResponse> getUnmappedSkus(List<String> categoryCodes) {
    GdnRestListResponse<UnmappedSkuResponse> response = xProductFeign
        .getUnmappedProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, categoryCodes);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return response.getContent();
  }

  @Override
  public SimpleListStringResponse bulkUpdateOff2OnByProductSkus(Map<String, Boolean> stringBooleanMap, String username,
      String requestId, Boolean updateOff2OnHistory) {
    GdnRestSingleResponse<SimpleListStringResponse> response = xProductFeign
        .bulkUpdateOff2OnActiveFlagByProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId,
            username, updateOff2OnHistory, new SimpleStringBooleanMapRequest(stringBooleanMap));
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return response.getValue();
  }

  @Override
  public Page<ProductL3SummaryResponse> getProductL3SummaryResponse(
      ProductSummaryRequest productSummaryRequest, int page, int size, String requestId,
      String username) {
    GdnRestListResponse<ProductL3SummaryResponse> response = xProductFeign
        .getFilterSummaryL3(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId,
            username, page, size, productSummaryRequest);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<ItemBasicL4Response> getL4ItemListByProductSku(int page, int size, String requestId, String username,
      ItemLevel4ListingWebRequest itemLevel4ListingWebRequest) {
    GdnRestListResponse<ItemBasicL4Response> response =
        xProductFeign.getL4ItemListByProductSku(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId,
            username, page, size, itemLevel4ListingWebRequest);
    if (CollectionUtils.isEmpty(response.getContent()) || !response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public List<ItemCodeDetailResponse> getItemDetailsByItemCodes(String requestId, String username,
      SimpleSetStringRequest itemCodesSet) {
    GdnRestListResponse<ItemCodeDetailResponse> response = xProductFeign
        .getItemDetailsByItemCodes(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId, username,
            itemCodesSet);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public Page<ItemSummaryResponse> getItemSummaryByFilter(String requestId, String username, Pageable pageable,
    ItemSummaryRequest itemSummaryRequest) {
    GdnRestListResponse<ItemSummaryResponse> response = xProductFeign
      .getItemSummaryByFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId, username, pageable.getPageNumber(),
        pageable.getPageSize(), itemSummaryRequest);
    if (!response.isSuccess()) {
      log.error("Error while fetching items by request - {}, error - {}", itemSummaryRequest, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), pageable, response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void updateProductItemViewConfig(String itemSku, ItemViewConfigBaseRequest itemViewConfigBaseRequest)
      throws Exception {
    GdnBaseRestResponse response =
        xProductFeign.updateItemViewConfigWithItemStatus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, itemSku, itemViewConfigBaseRequest);
    if (!response.isSuccess()) {
      log.error("Error while updating product item viewConfig item : {} by request - {}, error - {}", itemSku,
          itemViewConfigBaseRequest, response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public void updateItemPickupPointViewConfigWithProductStatus(String productSku,
      ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest) throws Exception {
    GdnBaseRestResponse response =
        xProductFeign.updateItemPickupPointViewConfigWithProductStatus(Constant.STORE_ID, Constant.CHANNEL_ID,
            Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, productSku,
            itemPickupPointViewConfigBaseRequest);
    if (!response.isSuccess()) {
      log.error("Error while updating product item viewConfig productSku : {} by request - {}, error - {} ", productSku,
          itemPickupPointViewConfigBaseRequest, response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItemsWithProductData(boolean showDeleted,
      String productSku, boolean includeForceReview) {
    GdnRestSingleResponse<ProductAndItemsResponse> response = this.xProductFeign
        .getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), showDeleted, productSku, includeForceReview, true);
    if (!response.isSuccess()) {
      log.error("Error while fetching the detail for product sku : {}, error - {}", productSku,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response;
  }

  @Override
  public GdnRestListResponse<ProductAndItemInfoResponseV2> getProductInfoByItemSku(GetProductInfoRequestV2 request) {
    GdnRestListResponse<ProductAndItemInfoResponseV2> response = xProductFeign
        .getProductInfoByItemSku(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), request);
    if (!response.isSuccess()) {
      log.error("Error while fetching the detail for item sku : {}, error - {}",
          request.getItemSkus().stream().findFirst().get(), response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response;
  }

  @Override
  public void addSalesCategory(String storeId, String catalogCode, String categoryCode, List<String> productSkus) {
    GdnBaseRestResponse response = xProductFeign
        .addProductSalesCatalog(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, catalogCode, categoryCode, new SimpleListStringRequest(productSkus));
    if (!response.isSuccess()) {
      log.error("Error while adding sales category. catalogCode : {}, categoryCode : {}, productSkus : {}, error : {}",
          catalogCode, categoryCode, productSkus, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public void deleteSalesCategory(String storeId, String catalogCode, String categoryCode, List<String> productSkus) {
    GdnBaseRestResponse response = xProductFeign
        .deleteSalesCatalog(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
            catalogCode, categoryCode, new SimpleListStringRequest(productSkus));
    if (!response.isSuccess()) {
      log.error(
          "Error while deleting sales category. catalogCode : {}, categoryCode : {}, productSkus : {}, error : {}",
          catalogCode, categoryCode, productSkus, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public void archiveByProductSku(String productSku, boolean doArchive, boolean suppressError) {
    GdnBaseRestResponse response =
        this.xProductFeign.toggleArchiveProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, doArchive, productSku);
    if (suppressError && Objects.nonNull(response) && StringUtils.isNotBlank(response.getErrorCode())) {
      if (dormantSellerErrorSuppressErrorCodes.contains(response.getErrorCode())) {
        log.info("Suppressing error as product is not active or already archived : {} : {}", productSku, response);
        return;
      }
    }
    if (Objects.isNull(response) || !response.isSuccess()) {
      log.error("Error on archive of product sku : {}, doArchive : {}, error - {} ", productSku, doArchive,
          Optional.ofNullable(response).orElse(new GdnBaseRestResponse()).getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          Optional.ofNullable(response).orElse(new GdnBaseRestResponse()).getErrorMessage());
    }
  }

  @Override
  public Page<ProductSkuSummaryResponse> getProductSkuSummaryResponse(String merchantCode, int page, int size,
    ProductSkuSummaryRequest productSkuSummaryRequest) {
    GdnRestListResponse<ProductSkuSummaryResponse> response =
      xProductFeign.getProductSkuSummary(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, merchantCode, page, size,
        productSkuSummaryRequest);
    if (!response.isSuccess()) {
      log.error("Error on fetching product skus summary for merchant :{}, page : {}, size : {} "
          + "request : {}, error : {}", merchantCode, page, size, productSkuSummaryRequest,
        response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
      response.getPageMetaData().getTotalRecords());
  }

  @Override
  public GdnRestListResponse<ItemSkuPickupPointCodeResponse> getItemPickupPointCodeByItemSkus(
      SimpleListStringRequest itemSkusList) {
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> gdnRestListResponse = new GdnRestListResponse<>();
    if (!CollectionUtils.isEmpty(itemSkusList.getValue())) {
      gdnRestListResponse = xProductFeign
          .getItemPickupPointCodeByItemSkus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, itemSkusList);
    }
    if (Objects.isNull(gdnRestListResponse)) {
      log.error("Error while fetching ItemSkuPickupPointCodeResponse, response is coming as null. itemSkusList : {} ",
          itemSkusList);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, GenericErrorMessages.RESPONSE_IS_NULL_ERROR);
    }
    if (!gdnRestListResponse.isSuccess()) {
      log.error(
          "Error while fetching ItemSkuPickupPointCodeResponse itemSkusList : {} , gdnRestListResponse : {} , error : {} ",
          itemSkusList, gdnRestListResponse, gdnRestListResponse.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, gdnRestListResponse.getErrorMessage());
    }
    return gdnRestListResponse;
  }

  @Override
  public Page<ItemPickupPointListingResponse> getItemPickupPointList(Pageable pageable,
    ItemPickupPointListingRequest itemPickupPointListingRequest) {
    GdnRestListResponse<ItemPickupPointListingResponse> response =
      this.xProductFeign.getItemPickupPointList(Constant.STORE_ID, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, pageable.getPageNumber(),
        pageable.getPageSize(), itemPickupPointListingRequest);
    if (!response.isSuccess()) {
      log.error("Error on fetching ItemPickup point list : PageRequest : {} "
          + "request : {}, error : {}", pageable, itemPickupPointListingRequest,
        response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), pageable,
      response.getPageMetaData().getTotalRecords());
  }

  @Override
  public List<ItemSummaryListResponse> getItemSummaryByItemSkuAndPPCode(
    List<ItemPickupPointRequest> itemPickupPointRequest) {
    GdnRestListResponse<ItemSummaryListResponse> response = this.xProductFeign
      .getItemSummaryByItemSkuAndPPCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, itemPickupPointRequest);
    if(!response.isSuccess()){
      log.error("Error on fetching item summary for the request : {} and error : {} ",
        itemPickupPointRequest,response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return Optional.of(response).map(GdnRestListResponse::getContent)
      .orElse(Collections.emptyList());
  }

  @Override
  public List<DeleteItemPickupPointResponse> deleteItemPickupPointByPickupPointCode(
      DeleteItemPickupPointRequest deleteItemPickupPointRequest) {
    GdnRestListResponse<DeleteItemPickupPointResponse> response =
        this.xProductFeign.deleteItemPickupPointByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID,
            Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, deleteItemPickupPointRequest);
    if (!response.isSuccess()) {
      log.error("Error while deleting L5  for DeleteItemPickupPoint  request : {} and error : {} ",
          deleteItemPickupPointRequest, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public Page<ProductSkuPickupPointResponseV2> getActiveProductsByPickupPointCode(String businessPartnerCode,
      String pickupPointCode, int page, int size) {
    GdnRestListResponse<ProductSkuPickupPointResponseV2> response =
        this.xProductFeign.getActiveProductsByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID,
            Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, page, size, businessPartnerCode,
            pickupPointCode);
    if (!response.isSuccess()) {
      log.error("Error while fetching active L5's with businessPartnerCode = {} and pickupPointCode = {}  ",
          businessPartnerCode, pickupPointCode, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public List<PriceRangeResponse> fetchPriceRange(String merchantCode, Set<String> skuList) {
    GdnRestListResponse<PriceRangeResponse> response =
        this.xProductFeign.getMinAndMaxPriceRange(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, merchantCode, skuList);
    if (!response.isSuccess()) {
      log.error("#XProductOutboundServiceImpl fetchPriceRange request : {} and error : {} ",
          skuList, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<ItemBasicDetailV2Response> getItemBasicDetails(String productSku) {
    GdnRestListResponse<ItemBasicDetailV2Response> response =
        this.xProductFeign.getItemBasicDetails(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, productSku);
    if (!response.isSuccess()) {
      log.error("Error while fetching active L4 for product sku = {}  ", productSku, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<ItemBasicDetailV2Response> getItemBasicDetailsByItemSku(String itemSku) {
    GdnRestListResponse<ItemBasicDetailV2Response> response = this.xProductFeign
        .getItemBasicDetailsByItemSku(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, false, false, false,
            new SimpleListStringRequest(Collections.singletonList(itemSku), false));
    if (!response.isSuccess()) {
      log.error("Error while fetching  L4 for item sku = {} and error : {} ", itemSku, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public Page<ItemL5ListingResponse> getItemL5Details(String productSku, boolean cncActivated, int page, int size) {
    GdnRestListResponse<ItemL5ListingResponse> response =
        this.xProductFeign.getItemL5Details(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, productSku, page, size, cncActivated, new SimpleListStringRequest());
    if (!response.isSuccess()) {
      log.error("Error while fetching active L5 for product sku = {}, error - {}", productSku, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<ItemL5ListingResponse> getItemL5DetailsByL5Ids(List<String> l5Ids, boolean cncActivated, int page, int size) {
    GdnRestListResponse<ItemL5ListingResponse> response =
        this.xProductFeign.getItemL5DetailsByL5Ids(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, page, size, cncActivated, new SimpleListStringRequest(l5Ids));
    if (!response.isSuccess()) {
      log.error("Error while fetching active L5 for L5 Ids = {}, error - {}", l5Ids, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public List<ItemBasicDetailV2Response> getItemBasicDetailByItemSku(String storeId, boolean fetchBundleRecipe,
      SimpleListStringRequest simpleListStringRequest) {
    GdnRestListResponse<ItemBasicDetailV2Response> response =
        this.xProductFeign.getItemBasicDetailByItemSku(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, fetchBundleRecipe, simpleListStringRequest);
    if (!response.isSuccess() || CollectionUtils.isEmpty(response.getContent())) {
      log.error("#XProductOutboundServiceImpl simpleListStringRequest request : {} and error : {} ",
          simpleListStringRequest, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public BasicProductResponse getBasicProductInfo(String storeId, String productSku) {
    GdnRestSingleResponse<BasicProductResponse> response =
        this.xProductFeign.getBasicProductInfo(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, productSku);
    if (!response.isSuccess()) {
      log.error("#XProductOutboundServiceImpl productSku : {} and error : {} ", productSku, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public boolean isSharedProduct(String storeId, String productSku, String businessPartnerCode) {
    GdnRestSingleResponse<SimpleBooleanResponse> response =
        this.xProductFeign.isSharedProduct(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, businessPartnerCode, productSku, true);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error("#XProductOutboundServiceImpl productSku : {} and error : {} ", productSku, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return Boolean.TRUE.equals(response.getValue().getResult());
  }

  @Override
  public Page<ProductSkuResponse> getProductSkuResponse(ProductSummaryRequest productSummaryRequest, int page, int size,
      String requestId, String username) {
    GdnRestListResponse<ProductSkuResponse> response =
        xProductFeign.getProductSkuResponse(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId,
            username, page, size, productSummaryRequest);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public BulkDownloadProductBasicInfoResponse getProductBasicInfoDetails(ProductLevel3SummaryRequest request)
      throws ApplicationException {
    GdnRestSingleResponse<BulkDownloadProductBasicInfoResponse> response =
        xProductFeign.getProductBasicInfoDetails(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, request);

    if (Objects.isNull(response)) {
      log.error("xproduct error : {} ", ErrorCategory.COMMUNICATION_FAILURE);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE, "xproduct error");
    }
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error("error message: {} ", response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public ProductL3Response getProductL3DetailsByProductSku(String productSku)
      throws ApplicationException {
    GdnRestSingleResponse<ProductL3Response> response =
        xProductFeign.getProductDetailsByProductSku(Constant.STORE_ID, Constant.CHANNEL_ID,
            Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, productSku);
    log.info("x-product feign response for product sku {} : {} ", productSku, response);
    if (Objects.isNull(response)) {
      log.error("x-product error for product sku {} : {} ", productSku,
          ErrorCategory.COMMUNICATION_FAILURE);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE, "xproduct error");
    }
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error("x-product feign failed with error message for product sku {} : {} ", productSku,
          response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public ProductBasicResponse getProductBasicInfo(String storeId, String productSku, boolean sharedProductInfoNeeded) {
    GdnRestSingleResponse<ProductBasicResponse> response =
        this.xProductFeign.getProductBasicInfo(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, productSku, sharedProductInfoNeeded);
    if (!response.isSuccess()) {
      log.error("#XProductOutboundServiceImpl productSku : {} and error : {} ", productSku, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public boolean mapProductToItem(String productCode) {
    GdnRestSingleResponse<L3VersionResponse> response =
        this.xProductFeign.mapProductToItem(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, productCode);
    if (!response.isSuccess()) {
      log.error("#XProductOutboundServiceImpl productCode : {} and error : {} ", productCode,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return ObjectUtils.isNotEmpty(response.getValue()) && ObjectUtils.isNotEmpty(response.getValue().getL3Version());
  }
}

