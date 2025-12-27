package com.gdn.partners.pbp.outbound.product;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.MasterProductEditDTO;
import com.gda.mta.product.dto.ProductBrandUpdateRequest;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.ProductMasterDataUpdateRequest;
import com.gda.mta.product.dto.response.ProductMasterDataUpdateResponse;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.ErrorMessage;
import com.gdn.partners.pbp.helper.RequestHelper;
import com.gdn.x.productcategorybase.dto.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;

import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.gda.mta.product.dto.CategoryAndCnDetailDto;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.notification.dto.GdnRestSimpleResponse;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.partners.pbp.outbound.xProduct.feign.XProductFeign;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.MasterCatalogRequest;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.ProductAttributeRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.UpsertOfflineItemPriceResponse;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.ListHolderRequest;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.NeedRevisionConfigRequest;
import com.gdn.x.productcategorybase.dto.request.PredictionCategoryMappingRequest;
import com.gdn.x.productcategorybase.dto.request.PredictionIdsRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodesSkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.PredictionIdAndCategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsMappedToCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
public class ProductOutboundBean implements ProductOutbound {

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private XProductFeign xProductFeign;

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductOutboundBean.class);
  private static final String PCB_ERROR_MESSAGE = "Failed to get response from PCB : {}";
  private static final String PCB_PRODUCT_CODE_ERROR_MESSAGE = "Failed to get response from PCB for productCode : {} : {} ";
  private static final String PCB_CATEGORY_ERROR_MESSAGE = "Failed to get response for category {} from PCB : {}";

  @Override
  public List<OfflineItemResponseDetail> findOfflineItems(String requestId, String username, String merchantCode,
      List<String> merchantSkus) throws Exception {
    if (CollectionUtils.isEmpty(merchantSkus)) {
      return Collections.emptyList();
    }

    GdnRestSingleResponse<OfflineItemResponse> response =
        xProductFeign.getOfflineItemsByMerchantCodeAndMerchantSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, requestId, username, merchantCode,
            new SimpleListStringRequest(merchantSkus));

    if (!response.isSuccess() || response.getValue() == null) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }

    return response.getValue().getOfflineProducts();
  }

  @Override
  public List<UpsertOfflineItemPriceResponse> upsertOfflineItemPrice(String requestId, String username,
      String merchantCode, List<OfflineItemRequest> offlineItemRequests) throws Exception {

    try {
      GdnRestListResponse<UpsertOfflineItemPriceResponse> response =
          xProductFeign.upsertOfflineItemPrice(GdnMandatoryRequestParameterUtil.getStoreId(),
              Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, requestId, username, merchantCode,
              offlineItemRequests);

      return response.isSuccess() ? response.getContent() : null;
    } catch (Exception e) {
      LOGGER.error("Failed to upsert offline item price to x-product with request {}, error {}", offlineItemRequests,
          e);

      List<UpsertOfflineItemPriceResponse> response = new ArrayList<>();
      offlineItemRequests.forEach(offlineItemRequest -> {
        UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse = new UpsertOfflineItemPriceResponse();
        BeanUtils.copyProperties(offlineItemRequest, upsertOfflineItemPriceResponse);
        upsertOfflineItemPriceResponse.setErrorMessage(e.getMessage());
        upsertOfflineItemPriceResponse.setSuccess(Boolean.FALSE);
        response.add(upsertOfflineItemPriceResponse);
      });

      return response;
    }
  }

  @Override
  public Page<ItemSummaryResponse> findSummaryInstantPickupByFilter(ItemSummaryRequest filter, Pageable pageRequest,
      SortOrder sort) throws Exception {
    String orderBy = null;
    String sortBy = null;
    if (sort != null) {
      orderBy = sort.getOrderBy();
      sortBy = sort.getSortBy();
    }
    GdnRestListResponse<ItemSummaryResponse> response =
        xProductFeign.getListOfItemSummaryByFilter(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, getRequestId(), getUsername(),
            pageRequest.getPageNumber(), pageRequest.getPageSize(), orderBy, sortBy, filter);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), pageRequest, response.getPageMetaData().getTotalRecords());
  }

  private String getUsername() {
    return StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getUsername()) ? GdnBaseLookup.DEFAULT_USERNAME
        : GdnMandatoryRequestParameterUtil.getUsername();
  }

  private String getRequestId() {
    return StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getRequestId()) ? UUID.randomUUID()
        .toString() : GdnMandatoryRequestParameterUtil.getRequestId();
  }

  @Override
  public GdnBaseRestResponse updateProductMasterCatalog(String requestId, String username,
      MasterCatalogRequest masterCatalogRequest, String productCode) throws Exception {
    GdnBaseRestResponse response =
        xProductFeign.updateProductMasterCatalog(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, requestId, username, productCode,
            masterCatalogRequest);
    this.checkResponse(response);
    return response;
  }

  @Override
  public GdnBaseRestResponse addProductAttribute(String requestId, String username,
      ProductAttributeRequest productAttributeRequest, String productCode) throws Exception {
    GdnBaseRestResponse response =
        xProductFeign.addProductAttribute(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, requestId, username, productCode, productAttributeRequest);
    this.checkResponse(response);
    return response;
  }
  
  private void checkResponse(GdnBaseRestResponse response) throws Exception {
    if(!response.isSuccess()){
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE, response.getErrorMessage());
    }
  }

  @Override
  public List<UpsertOfflineItemPriceResponse> upsertOfflineItem(String requestId, String username, String merchantCode,
      List<UpsertOfflineItemRequest> upsertOfflineItemRequests) throws Exception {

    try {
      GdnRestListResponse<UpsertOfflineItemPriceResponse> response =
          xProductFeign.upsertOfflineItem(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, requestId, username, merchantCode, upsertOfflineItemRequests);

      return response.isSuccess() ? response.getContent() : null;
    } catch (Exception e) {
      LOGGER.error("Failed to upsert offline item to x-product with request {}, error {}", upsertOfflineItemRequests,
          e);

      List<UpsertOfflineItemPriceResponse> response = new ArrayList<>();
      upsertOfflineItemRequests.forEach(upsertOfflineItemRequest -> {
        UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse = new UpsertOfflineItemPriceResponse();
        BeanUtils.copyProperties(upsertOfflineItemRequest, upsertOfflineItemPriceResponse);
        upsertOfflineItemPriceResponse.setErrorMessage(e.getMessage());
        upsertOfflineItemPriceResponse.setSuccess(Boolean.FALSE);
        response.add(upsertOfflineItemPriceResponse);
      });

      return response;
    }
  }

  @Override
  public List<ConfigurationStatusResponse> getReviewConfigurationChanges(long fromDateInMillis) {
    GdnRestListResponse<ConfigurationStatusResponse> response =
        pcbFeign.getConfigurationChanges(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), fromDateInMillis);
    if (!response.isSuccess() || Objects.isNull(response.getContent())) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<ConfigurationStatusResponse> getReviewConfiguration(List<ConfigurationStatusRequest> configurationStatusRequests) {
    GdnRestListResponse<ConfigurationStatusResponse> response =
        pcbFeign.getReviewConfiguration(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), configurationStatusRequests);
    if (!response.isSuccess() || Objects.isNull(response.getContent())) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public boolean republishProduct(String operationType, List<String> productCodes) {
    GdnBaseRestResponse response =
        pcbFeign.republishProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), operationType, productCodes);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.isSuccess();
  }

  @Override
  public CategorySummaryResponse updateProductCategory(String productCode, String categoryCode,
      boolean updateSalesCategory, boolean b2bSeller) {
    GdnRestSingleResponse<CategorySummaryResponse> response =
        pcbFeign.updateProductCategory(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productCode, categoryCode, updateSalesCategory, b2bSeller);
    if (!response.isSuccess() && ErrorCategory.VALIDATION.getCode().equals(response.getErrorCode())) {
      LOGGER.error("Error when updating product category for productCode : {} error : {} ", productCode,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, response.getErrorMessage());
    }
    if (!response.isSuccess()) {
      LOGGER.error("Error when updating product category for productCode : {} error : {} ", productCode,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public boolean upsertPredictionCategoryMapping(List<PredictionCategoryMappingRequest> requestList) {
    GdnBaseRestResponse response =
        pcbFeign.upsertPredictionCategoryMapping(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), requestList);
    if (!response.isSuccess() && ErrorCategory.VALIDATION.getCode().equals(response.getErrorCode())) {
      log.error(
          "Error when updating or inserting the predictionCategoryMapping. PredictionCategoryMappingRequestList : {} , error message : {} ",
          requestList, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, response.getErrorMessage());
    }
    if (!response.isSuccess()) {
      log.error(
          "Error when updating or inserting the predictionCategoryMapping. PredictionCategoryMappingRequestList : {} , error message : {} ",
          requestList, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.isSuccess();
  }

  @Override
  public GdnRestListResponse<PredictionIdAndCategoryCodeResponse> getPredictionIdAndCategoryCode(
      PredictionIdsRequest predictionIdsRequest) {
    GdnRestListResponse<PredictionIdAndCategoryCodeResponse> response =
        pcbFeign.getPredictionIdAndCategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), predictionIdsRequest);
    if (!response.isSuccess() && ErrorCategory.VALIDATION.getCode().equals(response.getErrorCode())) {
      log.error("Error while getting predictionId and categoryCode. predictionIdsRequest : {} , error message : {} ",
          predictionIdsRequest, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, response.getErrorMessage());
    }
    if (!response.isSuccess()) {
      log.error("Error while getting predictionId and categoryCode. predictionIdsRequest : {} , error message : {} ",
          predictionIdsRequest, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response;
  }

  @Override
  public CategoryDetailResponse getCategoryDetailByCategoryCode(String categoryCode) {
    GdnRestSingleResponse<CategoryDetailResponse> response = pcbFeign
        .getCategoryDetailByCategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), categoryCode);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public CategoryResponse getCategoryBasicDetailByCategoryCode(String categoryCode) {
    GdnRestSingleResponse<CategoryResponse> response = pcbFeign
        .getCategoryBasicDetailByCategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), categoryCode);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public AttributeResponse getAttributeDetailByAttributeCode(String attributeCode) {
    GdnRestListResponse<AttributeResponse> response = pcbFeign
        .getAttributeDetailByAttributeCode(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), attributeCode);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent().get(0);
  }

  @Override
  public List<String> getRestrictedKeywordMappedToCategory(String categoryCode) {
    GdnRestListResponse<RestrictedKeywordsMappedToCategoryResponse> responseGdnRestListResponse =
        pcbFeign.getRestrictedKeywordMappedToCategory(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), categoryCode);
    if (!responseGdnRestListResponse.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, responseGdnRestListResponse.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, responseGdnRestListResponse.getErrorMessage());
    }
    return responseGdnRestListResponse.getContent().stream()
        .map(RestrictedKeywordsMappedToCategoryResponse::getKeyword)
        .collect(Collectors.toList());
  }

  @Override
  public List<RestrictedKeywordsMappedToCategoryResponse> getRestrictedKeywordMappedToCategoryWithAction(
      String categoryCode) {
    GdnRestListResponse<RestrictedKeywordsMappedToCategoryResponse> responseGdnRestListResponse =
        pcbFeign.getRestrictedKeywordMappedToCategory(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), categoryCode);
    if (!responseGdnRestListResponse.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, responseGdnRestListResponse.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, responseGdnRestListResponse.getErrorMessage());
    }
    return responseGdnRestListResponse.getContent();
  }


  @Override
  public void clearProductCacheByProductCodes(List<String> productCodes) throws Exception {
    GdnBaseRestResponse response = pcbFeign
        .clearProductCacheByProductCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCodes);
    this.checkResponse(response);
  }

  @Override
  public void clearProductCacheSyncByProductIdAndProductCode(String productId, String productCode) {
    GdnBaseRestResponse response = pcbFeign
        .clearProductCacheSyncByProductIdAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productId, productCode);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public ActivateImageResponse updateProductImagesName(ProductActivateImageRequest request, boolean skipReview)
      throws Exception {
    GdnRestSingleResponse<ActivateImageResponse> response = pcbFeign
        .updateProductImagesName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, skipReview, request);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public String migrateProduct(String oldProductCode, String newProductCode,
      String createdMerchant, ProductRequest request) throws Exception {
    GdnRestSimpleResponse<String> response = null;
    if (Objects.nonNull(request)) {
      response = pcbFeign
          .migrateProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, oldProductCode, newProductCode, createdMerchant,
              request);
    } else {
      response = pcbFeign
          .migrateProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, oldProductCode, newProductCode,
              createdMerchant);
    }
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }


  @Override
  public void updateProductContent(ProductRequest productRequest, boolean ignoreSalesCategoryPublish) throws Exception {
    GdnBaseRestResponse response = pcbFeign
        .updateProductContent(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, ignoreSalesCategoryPublish, productRequest);
    this.checkResponse(response);
  }

  @Override
  public List<ProductItemResponse> getProductItemBySkuCodes(SkuCodesRequest skuCodesRequest) {
    GdnRestListResponse<ProductItemResponse> response = pcbFeign
        .getProductItemBySkuCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, skuCodesRequest);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public void editItemUpcCode(String productCode, List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequests) {
    GdnBaseRestResponse response = pcbFeign
        .editItemUpcCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCode, productItemUpcCodeUpdateRequests);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public List<LocationPathAndCommonImage> updateProductItemImagesByProductCode(ProductItemImageUpdateRequest productItemImageUpdateRequest) {
    GdnRestListResponse<LocationPathAndCommonImage> response = pcbFeign
        .updateProductItemImagesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productItemImageUpdateRequest);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public void updateProductAndItemImagesByProductCode(boolean setDgLevel, ProductAndItemImageRequest productAndItemImageRequest) throws Exception {
    GdnBaseRestResponse response = pcbFeign
        .updateProductAndItemImagesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,setDgLevel, productAndItemImageRequest);
    this.checkResponse(response);
  }

  @Override
  public PredefinedAllowedAttributeValueResponse getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
      String attributeCode, String value, boolean fetchByPredefinedAttributeCode) {
    GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse> response = pcbFeign
        .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(Constants.DEFAULT_STORE_ID,
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
            Constants.DEFAULT_USERNAME, attributeCode, value, fetchByPredefinedAttributeCode);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public List<AllowedAttributeValueResponse> findAllowedAttributeValue(List<AllowedAttributeValueRequest> allowedAttributeValueRequests) throws Exception {
    ListHolderRequest<AllowedAttributeValueRequest> listHolderRequest = new ListHolderRequest<>();
    listHolderRequest.setLists(allowedAttributeValueRequests);
    GdnRestListResponse<AllowedAttributeValueResponse> response =
        pcbFeign.findAllowedAttributeValue(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, listHolderRequest);
    if (!response.isSuccess() || Objects.isNull(response.getContent())) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<ProductImageResponse> filterProductImagesByProductIds(List<String> productIds, boolean mainImage) {
    ListHolderRequest<String> request = new ListHolderRequest<>();
    request.setLists(productIds);
    GdnRestListResponse<ProductImageResponse> response = pcbFeign
        .filterProductImagesByProductIds(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, request, mainImage);
    if (!response.isSuccess() || Objects.isNull(response.getContent())) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public CategoryAndCnDetailDto validCnCategory(String categoryCode) {
    GdnRestSingleResponse<CategoryResponse> categoryResponse = pcbFeign
        .validateCnCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, categoryCode);
    if(!categoryResponse.isSuccess() && Objects.nonNull(categoryResponse.getErrorMessage())){
      LOGGER.error(PCB_ERROR_MESSAGE, categoryResponse.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, categoryResponse.getErrorMessage());
    }
    return new CategoryAndCnDetailDto(categoryResponse.getValue(), categoryResponse.isSuccess());
  }

  public List<String> getItemCodesByUpcCodeAndProductCode(String productCode, ProductItemUpcCodesSkuCodesRequest productItemUpcCodesSkuCodesRequest){
    GdnRestListResponse<SingleObjectResponse<List<String>>> response = pcbFeign.getItemCodeByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCode, productItemUpcCodesSkuCodesRequest);
    if (!response.isSuccess() || CollectionUtils.isEmpty(response.getContent())) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent().get(0).getValue();
  }

  @Override
  public ProductDetailResponse getProductDetailByProductCode(String productCode, boolean inAllProducts,
      boolean originalImages) {
    GdnRestSingleResponse<ProductDetailResponse> response = pcbFeign
        .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, inAllProducts,
            productCode, originalImages);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public void updateFlagsOnNeedCorrection(String productCode, NeedRevisionConfigRequest request) {
    GdnBaseRestResponse response = pcbFeign
        .updateFlagsOnNeedCorrection(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCode,
            request);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public ProductDetailResponse getImagesForScalingByProductCode(String productCode) {
    GdnRestSingleResponse<ProductDetailResponse> response = pcbFeign
        .getImagesForScalingByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCode);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public Map<String, String> getProductItemIdsBySkuCodes(List<String> skuCodes) {
    GdnRestSingleResponse<SingleObjectResponse<Map<String, String>>> response = pcbFeign.
        getProductItemIdsBySkuCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, new SkuCodesRequest(skuCodes));
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue().getValue();
  }

  @Override
  public void updateAndMarkProductForNeedCorrection(ProductRequest productRequest) {
    GdnBaseRestResponse response =
        pcbFeign.updateAndMarkProductForNeedCorrection(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            productRequest.getProductCode(),
            productRequest);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public List<CategoryResponse> filterCategoryHierarchyByCategoryCode(String categoryCode) {
    GdnRestListResponse<CategoryResponse> response = pcbFeign.
        filterCategoryHierarchyByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, categoryCode);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_CATEGORY_ERROR_MESSAGE, categoryCode, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public void deleteOriginalImages(String productCode) {
    GdnBaseRestResponse response = pcbFeign
        .deleteOriginalImages(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCode);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_PRODUCT_CODE_ERROR_MESSAGE, productCode, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public Map<String, String> updateImages(ProductImageEditRequest request) {
    GdnRestSingleResponse<SingleObjectResponse<Map<String, String>>> response =
        pcbFeign.updateImages(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, request);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue().getValue();
  }

  @Override
  public SimpleBooleanResponse authoriseProtectedBrand(String storeId, String channelId,
    String clientId, String requestId, String username, String brandCode,
    String businessPartnerCode) {
    GdnRestSingleResponse<SimpleBooleanResponse> response = pcbFeign
      .getBrandAuthorisation(storeId, channelId, clientId, requestId, username, brandCode,
        businessPartnerCode);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }


  @Override
  public String findProductNameByProductItemId(String productItemId) {
    SingleBaseResponse<String> response = pcbFeign
        .getProductNameByProductItemId(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productItemId);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      return Constants.DEFAULT;
    }
    return response.getValue();
  }

  @Override
  public CategoryNamesResponse fetchCategoryNamesResponse(CategoryMultipleIdRequest categoryCodes, int page, int size) {
    GdnRestSingleResponse<CategoryNamesResponse> response = pcbFeign
        .getCategoryNames(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, page, size, categoryCodes);
    if (!response.isSuccess()) {
      LOGGER.error(PCB_CATEGORY_ERROR_MESSAGE, categoryCodes, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }


  @Override
  public Map<String, Map<String, String>> updateCommonImages(List<ProductImageEditRequest> requests) {
    GdnRestSingleResponse<SingleObjectResponse<Map<String, Map<String, String>>>> response =
        pcbFeign.updateCommonImages(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, requests);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue().getValue();
  }

  @Override
  public List<ItemImageResponse> getProductItemImagesByItemCode(SkuCodesRequest itemCodes,
      boolean removeOriginalImages) {
    GdnRestListResponse<ItemImageResponse> response =
        pcbFeign.getProductItemImagesByItemCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, removeOriginalImages,
            itemCodes);
    if (!response.isSuccess() || Objects.isNull(response.getContent())) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public ProductAndAttributeDetailResponse getProductAttributesByProductId(String productId) {
    GdnRestSingleResponse<ProductAndAttributeDetailResponse> response =
        pcbFeign.getProductAttributesByProductId(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productId);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public List<ProductItemDetailResponse> getProductItemByListOfProductCode(ProductCodesRequest request,
      boolean originalImages, boolean isOnlyExternal, boolean active) {
    GdnRestListResponse<ProductItemDetailResponse> response =
        pcbFeign.getProductItemByListOfProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, originalImages,
            isOnlyExternal, active, request);
    if (!response.isSuccess() || Objects.isNull(response.getContent())) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<AttributeResponse> getAttributeDetailByAttributeCodes(boolean fetchOnlyBasicAttributeDetails,
      AttributeCodesRequest attributeCodesRequest) {
    GdnRestListResponse<AttributeResponse> response =
        pcbFeign.getAttributeDetailByAttributeCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            fetchOnlyBasicAttributeDetails, attributeCodesRequest);
    if (!response.isSuccess() || Objects.isNull(response.getContent())) {
      LOGGER.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public void updateProductMasterDataInPCB(String storeId, String requestId, String username,
    ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO) {
    GdnRestSingleResponse<ProductMasterDataUpdateResponse> pcbMasterDataEditResponse;
    ProductMasterDataUpdateRequest productMasterDataUpdateRequest =
      RequestHelper.prepareMasterDataEditRequestForPCBUpdate(productMasterDataEditRequest,
        masterProductEditDTO);
     pcbMasterDataEditResponse =
      pcbFeign.updateMasterData(storeId, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        requestId, username, productMasterDataUpdateRequest);
    if (!pcbMasterDataEditResponse.isSuccess()) {
      if (Objects.isNull(pcbMasterDataEditResponse.getValue())) {
        throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          pcbMasterDataEditResponse.getErrorMessage());
      } else {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          Optional.of(pcbMasterDataEditResponse).map(GdnRestSingleResponse::getValue)
            .map(ProductMasterDataUpdateResponse::getErrorMessage).map(ErrorMessage::getMessage)
            .orElse(String.format("Error while updating product master data for product code : %s",
              productMasterDataEditRequest.getProductCode())));
      }
    }
    if (Optional.of(pcbMasterDataEditResponse).map(GdnRestSingleResponse::getValue)
      .map(ProductMasterDataUpdateResponse::getProductImagesErrorMap).filter(Map::isEmpty)
      .isPresent()) {
      LOGGER.info("Product master data Images had Errors for product code : {} with Errors : {} ",
        productMasterDataEditRequest.getProductCode(), pcbMasterDataEditResponse.getValue());
      masterProductEditDTO.setApiErrorCode(ApiErrorCode.IMAGE_UPDATE_FAILED);
    }

  }

  @Override
  public List<DistributionInfoPerSkuResponse> getDistributionInfo(String productCode) {
    GdnRestListResponse<DistributionInfoPerSkuResponse> response =
        getDistributionInfoPerSkuResponse(productCode, 0 , 100);
    List<DistributionInfoPerSkuResponse> distributionInfoPerSkuResponses =
        new ArrayList<>(response.getContent());
    int totalPages = (int) (Math.ceil(
        (double) response.getPageMetaData().getTotalRecords() / (double) response.getPageMetaData()
            .getPageSize()));
    int currentPage = 1;
    while (currentPage < totalPages) {
      response = pcbFeign.getDistributionInfo(GdnMandatoryRequestParameterUtil.getStoreId(),
          Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, getRequestId(), getUsername(),
          productCode, false, currentPage++, 100);
      distributionInfoPerSkuResponses.addAll(response.getContent());
      if (!response.isSuccess()) {
        throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
            response.getErrorMessage());
      }
    }
    return distributionInfoPerSkuResponses;
  }

  @Override
  public GdnRestListResponse<DistributionInfoPerSkuResponse> getDistributionInfoPerSkuResponse(
      String productCode, int page, int size) {
    GdnRestListResponse<DistributionInfoPerSkuResponse> response =
        pcbFeign.getDistributionInfo(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, getRequestId(),
            getUsername(), productCode, true, page, size);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response;
  }

  @Override
  public Map<String, ProductL1AndL2CodeResponse> getOmniChannelSkuToItemCode(String sellerCode,
      List<String> omniChannelSkus) {
    GdnRestSingleResponse<ValidOmniChannelSkuResponse> response =
        pcbFeign.checkOmniChannelSkuExistsInSeller(GdnMandatoryRequestParameterUtil.getStoreId(),
            getRequestId(), getUsername(), new OmniChannelSkuRequest(sellerCode, omniChannelSkus));
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue().getExistingOmniChannelSkusAndProductDetailsMap();
  }

  @Override
  public void updateDistributionInfo(String productCode,
      DistributionInfoUpdateRequest distributionInfoUpdateRequest) {
    GdnBaseRestResponse response = pcbFeign.updateDistributionInfo(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(),
        getUsername(), productCode, distributionInfoUpdateRequest);
    if (Objects.isNull(response) || !response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public ProductBrandUpdateResponse updateProductBrandValue(
      ProductBrandUpdateRequest productBrandUpdateRequest) {
    GdnRestSingleResponse<ProductBrandUpdateResponse> response =
        pcbFeign.updateProductBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            productBrandUpdateRequest);
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, Constants.EMPTY_STRING);
    }
    if (!response.isSuccess()) {
      if (org.apache.commons.lang3.StringUtils.isBlank(response.getErrorMessage())) {
        throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, Constants.EMPTY_STRING);
      } else {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            Optional.of(response).map(GdnBaseRestResponse::getErrorMessage).orElse(
                String.format("Error while updating brand for product code : %s",
                    productBrandUpdateRequest.getProductCode())));
      }
    }
    if (Objects.isNull(response.getValue())) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, Constants.EMPTY_STRING);
    }
    return response.getValue();
  }

}
