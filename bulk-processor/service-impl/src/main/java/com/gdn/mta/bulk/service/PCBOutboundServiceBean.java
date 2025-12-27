package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import com.gdn.mta.bulk.models.download.responsedata.ProductImageAndVideoResponse;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.models.download.BrandAuthFilterRequest;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.CategoryErrorResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthDeleteRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.PCBFeign;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.BatchVatUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.request.MerchantConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BatchVatUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.BulkCategoryConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.BulkMerchantConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailAndShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProtectedBrandResponse;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class PCBOutboundServiceBean implements PCBOutboundService {

  private static final Logger LOGGER = LoggerFactory.getLogger(PCBOutboundServiceBean.class);

  @Autowired
  private PCBFeign pcbFeign;

  @Value(value = "${api.brand-max.limit}")
  private int MAX_LIMIT;

  @Value(value = "${api.master.catalog.id}")
  private String catalogId;

  @Value(value = "${sort.allowed.attribute.values}")
  private boolean sortAllowedAttributeValues;

  @Value("${basic.info.download.category.hierarchy.fetch.batch.size}")
  private int categoryHierarchyFetchBatchSize;

  @Value("${max.L3.product.fetch.size}")
  private int maxProductSize;

  @Override
  public GdnRestListResponse<BulkMerchantConfigUploadResponse> bulkMerchantConfigUpload(String storeId,
      String channelId, String clientId, String requestId, String username,
      MerchantConfigurationRequestList merchantConfigurationRequestList) {
    return pcbFeign
        .bulkMerchantConfigUpload(storeId, channelId, clientId, requestId, username, merchantConfigurationRequestList);
  }

  @Override
  public GdnRestListResponse<BulkCategoryConfigUploadResponse> bulkCategoryConfigUpload(String storeId,
      String channelId, String clientId, String requestId, String username,
      CategoryConfigurationRequestList categoryConfigurationRequestList) {
  return pcbFeign
        .bulkCategoryConfigUpload(storeId, channelId, clientId, requestId, username, categoryConfigurationRequestList);
  }

  @Override
  public GdnRestListResponse<BulkConfigDataResponse> fetchConfigDetailsByCodes(String storeId, String channelId,
      String clientId, String requestId, String username, String configType, AttributeCodesRequest attributeCodesRequest) {
    return pcbFeign
        .fetchConfigDetailsByCodes(storeId, channelId, clientId, requestId, username, configType, attributeCodesRequest);
  }

  @Override
  public GdnRestListResponse<CategoryConfigurationFilterResponse> getCategoryConfigurationList(String storeId,
      String channelId, String clientId, String requestId, String username,
      ConfigurationFilterRequest configurationFilterRequest, int page, int size) {
    return pcbFeign
        .getCategoryConfigurationList(storeId, channelId, clientId, requestId, username, page, size, configurationFilterRequest);
  }

  @Override
  public GdnRestListResponse<MerchantConfigurationFilterResponse> getMerchantConfigurationList(String storeId,
      String channelId, String clientId, String requestId, String username,
      ConfigurationFilterRequest configurationFilterRequest, int page, int size) {
    return pcbFeign
        .getMerchantConfigurationList(storeId, channelId, clientId, requestId, username, page, size, configurationFilterRequest);
  }

  @Override
  public GdnRestListResponse<ConfigurationStatusResponse> getconfigurationstatus(String storeId,
      String channelId, String clientId, String requestId, String username, List<ConfigurationStatusRequest> requests) {
    return pcbFeign.getconfigurationstatus(storeId, channelId, clientId, requestId, username, requests);
  }

  @Override
  public List<PredefinedAllowedAttributeValueResponse> getBrandSuggestions(String value, String businessPartnerCode,
      boolean isSearch, boolean isExternal) {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response = pcbFeign
        .getBrandSuggestions(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, value, businessPartnerCode, isSearch,
            isExternal, 0, MAX_LIMIT);
    if (Objects.isNull(response) || !response.isSuccess()) {
      LOGGER.error("Exception while getting Brand suggestions from PCB businessPartnerCode : {}", businessPartnerCode);
      return new ArrayList<>();
    }
    return response.getContent();
  }

  @Override
  public CategoryDetailResponse getCategoryDetailResponse(String categoryCode) {
    LOGGER.debug("get category detail by categoryCode for unified bulk process : {}", categoryCode);
    GdnRestSingleResponse<CategoryDetailResponse> response = pcbFeign
        .getCategoryDetailByCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, categoryCode);
    if (Objects.isNull(response) || !response.isSuccess()) {
      LOGGER.error("Exception while getting category detail for Generic template : {}", categoryCode);
      return new CategoryDetailResponse();
    }
    return response.getValue();
  }

  @Override
  public CategoryResponse getBasicCategoryInfoAndCatalogInfo(String categoryCode) {
    LOGGER.debug("get basic category and catalog info by categoryCode for unified bulk process : {}", categoryCode);
    GdnRestSingleResponse<CategoryResponse> response = pcbFeign
        .getBasicCategoryInfoAndCatalogInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, categoryCode);
    if (Objects.isNull(response) || !response.isSuccess()) {
      LOGGER.error("Exception while getting category detail for Generic template : {}", categoryCode);
      return new CategoryResponse();
    }
    return response.getValue();
  }

  @Override
  public AttributeResponse getAttributeDetail(String attributeId) {
    LOGGER.debug("get attribute detail by attributeId unified bulk process : {}", attributeId);
    GdnRestSingleResponse<AttributeResponse> response = pcbFeign
        .getAttributeDetail(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, attributeId, sortAllowedAttributeValues);
    if (Objects.isNull(response) || !response.isSuccess()) {
      LOGGER.error("Exception while getting attribute detail for Generic template : {}", attributeId);
      return new AttributeResponse();
    }
    return response.getValue();
  }

  @Override
  public List<CategoryTreeResponse> getGenericTemplateCategories(boolean genericTemplateEligible,
    boolean ignoreB2bExclusive) {
    GdnRestListResponse<CategoryTreeResponse> response = pcbFeign
        .getGenericTemplateCategories(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, genericTemplateEligible, ignoreB2bExclusive);
    if (!response.isSuccess() || CollectionUtils.isEmpty(response.getContent())) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return response.getContent();
  }

  @Override
  public CategoryDetailAndShippingResponse getCategoryInfoByCategoryId(String categoryId) {
    GdnRestSingleResponse<CategoryDetailAndShippingResponse> response = pcbFeign
        .getCategoryInfoByCategoryId(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, categoryId);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return response.getValue();
  }

  @Override
  public List<BrandWipResponse> getInReviewBrands(BrandWipSummaryRequest brandWipSummaryRequest) {
    GdnRestListResponse<BrandWipResponse> response = pcbFeign
        .getInReviewBrands(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, 0, Integer.MAX_VALUE,
            brandWipSummaryRequest);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return response.getContent();
  }

  @Override
  public List<AttributeResponse> getAttributeByNameStartingWithAndPageable(String attributeName) {
    GdnRestListResponse<AttributeResponse> response = pcbFeign
        .getAttributeByNameStartingWithAndPageable(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, 0, Integer.MAX_VALUE, attributeName);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return response.getContent();
  }

  @Override
  public CategoryTreeResponse getCategoryTree(String categoryCode) {
    GdnRestListResponse<CategoryTreeResponse> response = pcbFeign
        .getCategoryTree(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, StringUtils.EMPTY, Arrays.asList(categoryCode));
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    GdnPreconditions
        .checkArgument(CollectionUtils.isNotEmpty(response.getContent()), ErrorCategory.DATA_NOT_FOUND.getMessage());
    return response.getContent().get(0);
  }

  @Override
  public List<BatchVatUpdateResponse> batchVatFlagUpdate(String username, String businessPartnerCode,
      Map<String, Boolean> request) {
    GdnRestListResponse<BatchVatUpdateResponse> response = pcbFeign
        .updateVatFlagBySkuCodes(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            username, new BatchVatUpdateRequest(businessPartnerCode, request));
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return response.getContent();
  }

  @Override
  public ProductDetailResponse getProductDetailByProductCode(String productCode, boolean inAllProducts,
      boolean originalImages) {
    GdnRestSingleResponse<ProductDetailResponse> response = pcbFeign
        .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, inAllProducts,
            productCode, originalImages);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public List<ProtectedBrandResponse> getProtectedBrandList(String storeId) {
    String channelId = MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER);
    String clientId = MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER);
    String requestId = MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER);
    String username = MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    GdnRestListResponse<ProtectedBrandResponse> response =
      pcbFeign.getProtectedBrandList(storeId, channelId, clientId, requestId, username);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public SimpleBooleanResponse getBrandAuthorisation(String storeId, String channelId,
    String clientId, String requestId, String username, String sellerCode, String brandCode) {
    GdnRestSingleResponse<SimpleBooleanResponse> response = pcbFeign
      .getBrandAuthorisation(storeId, channelId, clientId, requestId, username, brandCode,
        sellerCode);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    LOGGER
      .info("Brand Authorisation for brandCode {} and business partner code {} is {} ", brandCode,
        sellerCode, response.getValue().getResult());
    return response.getValue();
  }

  @Override
  public AttributeResponse getAttributeDetailById(String storeId, String attributeId, String value) {
    GdnRestSingleResponse<AttributeResponse> response =
        pcbFeign.getAttributeDetailById(storeId, MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), attributeId, value);
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "null response from pcb");
    } else if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public void deleteBrandAuthorisation(String storeId, String sellerCode, String brandCode) {
    GdnBaseRestResponse response =
        pcbFeign.deleteBrandAuthorisation(storeId, MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), sellerCode, brandCode);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public List<String> getAllChildCategoriesFromC1CategoryCode(String requestId,
    CategoryCodeRequest categoryCodeRequest, boolean filterOutInactiveCn) {
    GdnRestSingleResponse<CategoryCodeResponse> response =
      pcbFeign.getAllChildCategoriesFromC1CategoryCode(Constants.DEFAULT_STORE_ID,
        MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
        MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER), requestId,
        MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), filterOutInactiveCn,
        categoryCodeRequest);
    if (!response.isSuccess()) {
      log.error(
        "Error While fetching Child categories From PCB for Campaign upload for request : {} ",
        categoryCodeRequest);
      return Collections.emptyList();
    }
    return Optional.ofNullable(response).map(GdnRestSingleResponse::getValue)
      .map(CategoryCodeResponse::getCategoryCodes).orElse(Collections.emptyList());
  }

  @Override
  public String updateCategoriesWithRestrictedKeywords(String storeId, String categoryCode,
      CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList) {
    GdnBaseRestResponse response =
        pcbFeign.updateCategoriesWithRestrictedKeywords(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, categoryCode, categoryKeywordUpdateRequestList);
    if (!response.isSuccess()) {
      return response.getErrorMessage();
    }
    return null;
  }

  @Override
  public List<CategoryDTO> getChildFromParentByCatalogIdWithChildCount(String storeId, int page, int size,
      String filterType, String documentFilterType, String username) {
    GdnRestListResponse<CategoryDTO> response =
        pcbFeign.getChildFromParentByCatalogIdWithChildCount(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, username, page, size, catalogId, filterType, documentFilterType);
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "null response from pcb");
    } else if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<CategoryErrorResponse> validateDestinationCategory(String storeId, List<String> categoryCodes) {
    GdnRestListResponse<CategoryErrorResponse> response =
        pcbFeign.validateCategoryForRestrictedKeywordCategoryChange(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, categoryCodes);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "Error while validating destination category");
    }
    return response.getContent();
  }

  @Override
  public GdnRestListResponse<BrandAuthFilterResponse> getAuthorisations(String storeId, int page, int size,
      BrandAuthFilterRequest brandAuthFilterRequest) {
    GdnRestListResponse<BrandAuthFilterResponse> response =
        pcbFeign.getAuthorisations(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, page, size, brandAuthFilterRequest);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "Error while validating destination category");
    }
    return response;
  }

  @Override
  public String createBulkBrandAuthorisation(String storeId, BrandAuthCreateRequest brandAuthCreateRequest) {
    GdnRestSingleResponse<BrandAuthCreateResponse> response =
        pcbFeign.create(storeId, MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), brandAuthCreateRequest);
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "null response from pcb");
    } else if (!response.isSuccess()) {
      return response.getErrorMessage();
    } else {
      return null;
    }
  }

  @Override
  public String deleteBulkBrandAuthorisation(String storeId, List<BrandAuthDeleteRequest> brandAuthDeleteRequestList) {
    GdnBaseRestResponse response =
        pcbFeign.delete(storeId, MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), brandAuthDeleteRequestList);
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "null response from pcb");
    } else if (!response.isSuccess()) {
      return response.getErrorMessage();
    } else {
      return null;
    }
  }

  @Override
  public List<CategoryHierarchyResponse> filterCategoryHierarchyByCategoryCodes(List<String> categoryCodes) {
    List<CategoryHierarchyResponse> allResponses = new ArrayList<>();
    CategoryCodeRequest categoryCodeRequest = new CategoryCodeRequest();
    for (List<String> batch : Lists.partition(categoryCodes, categoryHierarchyFetchBatchSize)) {
      categoryCodeRequest.setCategoryCodes(batch);
      GdnRestListResponse<CategoryHierarchyResponse> response =
          pcbFeign.filterCategoryHierarchyByCategoryCodes(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, categoryCodeRequest);
      if (!response.isSuccess()) {
        throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
            ErrorCategory.COMMUNICATION_FAILURE.getMessage());
      }
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(response.getContent()),
          ErrorCategory.DATA_NOT_FOUND.getMessage());
      allResponses.addAll(response.getContent());
    }
    return allResponses;
  }

  @Override
  public List<ProductImageAndVideoResponse> getBasicInfoFromPCB(List<String> productCodesList) {
    List<ProductImageAndVideoResponse> productImageAndVideoResponses = new ArrayList<>();
    for (List<String> productCodes : Lists.partition(productCodesList, maxProductSize)) {
      try {
        GdnRestListResponse<ProductImageAndVideoResponse> response =
            pcbFeign.getBasicInfoProductDetailsListByProductCodes(GdnMandatoryRequestParameterUtil.getStoreId(),
                GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
                GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
                productCodes);
        if (!response.isSuccess()) {
          throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
              ErrorCategory.COMMUNICATION_FAILURE.getMessage());
        }
        GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(response.getContent()),
            ErrorCategory.DATA_NOT_FOUND.getMessage());
        productImageAndVideoResponses.addAll(response.getContent());
      } catch (Exception e) {
        LOGGER.error("Error while fetching basic product info from PCB. Size: {}, Product Codes: {}",
            productCodes.size(), productCodes, e);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ErrorCategory.COMMUNICATION_FAILURE.getMessage());
      }
    }
    return productImageAndVideoResponses;
  }

  @Override
  public AttributeResponse getAttributeDetailByIdIgnoreCase(String storeId, String attributeId,
    String value) {
    GdnRestSingleResponse<AttributeResponse> response =
      pcbFeign.getAttributeDetailByIdIgnoreCase(storeId,
        MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
        MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
        MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
        MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), attributeId, value, true);
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "null response from pcb");
    } else if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public boolean editItemUpcCode(String productCode,
      List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequests) {
    GdnBaseRestResponse response = pcbFeign.editItemUpcCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), productCode,
        productItemUpcCodeUpdateRequests);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.isSuccess();
  }

  @Override
  public List<ProductItemResponse> getProductItemBySkuCodes(SkuCodesRequest skuCodesRequest) {
    GdnRestListResponse<ProductItemResponse> response =
        pcbFeign.getProductItemBySkuCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, skuCodesRequest);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }
}