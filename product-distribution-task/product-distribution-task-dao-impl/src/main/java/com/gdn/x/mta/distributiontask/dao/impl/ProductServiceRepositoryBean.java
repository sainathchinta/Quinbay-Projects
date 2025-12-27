package com.gdn.x.mta.distributiontask.dao.impl;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

import com.gdn.x.mta.distributiontask.dao.api.feign.ProductAnalyticsFeign;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gdn.x.mta.distributiontask.dao.exception.ValidationException;
import com.gdn.x.mta.distributiontask.model.ErrorMessages;
import com.gdn.x.mta.distributiontask.model.dto.CategoryDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductCodeAndSkuRequest;
import com.gdn.x.mta.distributiontask.model.dto.ProductSkuDetailResponse;
import com.gdn.x.mta.distributiontask.model.dto.SellerAnalyticsResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Repository;

import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.AutoQcConfigChangeRequest;
import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ProductHistoryRequest;
import com.gda.mta.product.dto.RetryNeedRevisionRequest;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gda.mta.product.dto.response.RetryAutoNeedRevisionResponse;
import com.gda.mta.product.dto.response.SimpleBooleanResponse;
import com.gda.mta.product.dto.response.SimpleStringResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.dao.api.feign.XProductFeign;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.util.SingleValueResponse;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.dao.api.feign.PBPFeign;
import com.gdn.x.mta.distributiontask.dao.api.feign.PCBFeign;
import com.gdn.x.mta.distributiontask.dao.util.ConverterUtil;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.TaskHistory;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.product.rest.web.model.request.ProductSkuAndProductCodeRequest;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;

/**
 * Created by virajjasani on 25/09/16.
 */
@Repository
@Slf4j
public class ProductServiceRepositoryBean implements ProductServiceRepository {

  private static final String PCB_ERROR_MESSAGE = "Failed to get response from PCB : {}";
  private static final String PRODUCT_CODE_MESSAGE = "for product : {}";
  private static final String PBP_ERROR_MESSAGE = "Failed to get response from PBP : {}";
  private static final String X_PRODUCT_ERROR_MESSAGE = "Failed to get response from XProduct : {}";
  private static final String PBP_IMAGE_QC_ERROR_MESSAGE = "Failed to get response from PBP while getting image QC response: {}";

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private XProductFeign xProductFeign;

  @Autowired
  private ProductAnalyticsFeign productAnalyticsFeign;

  @Override
  public void deleteProductCollection(String requestId, String userName, boolean needEmailNotification,
      RejectProductDTO rejectProductDTO) throws Exception {
    GdnBaseRestResponse response =
        this.pbpFeign.deleteProductCollection(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, requestId, userName, needEmailNotification,
            ConverterUtil.toDeleteProductRequest(rejectProductDTO));
    if (!response.isSuccess()) {
      log.error(
          "error while updating product collection for Product Service Client. requestId: {}, " + "productCode: {}",
          requestId, rejectProductDTO.getProductCode());
      throw new IllegalStateException(response.getErrorMessage());
    }
  }

  @Override
  public List<ProductDetailResponse> findProductDetailsByProductCodes(String requestId,
      String username, List<String> productCodes) throws Exception {
    GdnRestListResponse<ProductDetailResponse> response =
        this.pbpFeign.getProductDetailsByProductCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, requestId, username, productCodes);
    if (!response.isSuccess()) {
      log.error(
          "error while retrieving product detail list by product code list. requestId: {}, "
              + "username: {}, productCodes: {}", requestId, username, productCodes);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public void republishToPDT(String requestId, String userName, String productCode) throws Exception {
    GdnBaseRestResponse response =
        this.pbpFeign.publishProductToPDTByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, requestId, userName, productCode);
    if (!response.isSuccess()) {
      log.error("error while republishToPDT for Product Service Client. requestId: {}, " + "productCode: {}",
          requestId, productCode);
      throw new IllegalStateException(response.getErrorMessage());
    }
  }

  @Async
  @Override
  public void submitHistory(String requestId, String userName, TaskHistory taskHistory) throws Exception {
    try {
      ProductHistoryRequest request = new ProductHistoryRequest();
      request.setProductCode(taskHistory.getProductCode());
      request.setDescription(taskHistory.getState().getDesc());
      request.setNotes(taskHistory.getReason());
      request.setState(5);
      request.setStoreId(taskHistory.getStoreId());
      request.setCreatedDate(taskHistory.getCreatedDate());
      request.setUpdatedDate(taskHistory.getUpdatedDate());
      request.setCreatedBy(taskHistory.getCreatedBy());
      request.setUpdatedBy(taskHistory.getUpdatedBy());
      GdnBaseRestResponse response =
          this.pbpFeign.submitHistory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, requestId, userName, request);
      if (!response.isSuccess()) {
        log.error("success is false while submitHistory for product service client requestId: {}, productCode: {}",
            requestId, request.getProductCode());
        throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
      }
    } catch (Exception e) {
      log.error("error while submitHistory for product service client. requestId: {}, productCode: {}", requestId,
          taskHistory.getProductCode());
    }
  }

  @Async
  @Override
  public void addToProductHistory(String requestId, String userName, ProductHistoryRequest productHistoryRequest)
      throws Exception {
    try {
      GdnBaseRestResponse response =
          this.pbpFeign.submitHistory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, requestId, userName, productHistoryRequest);
      if (!response.isSuccess()) {
        log.error("success is false while submitHistory for product service client requestId: {}, productCode: {}",
            requestId, productHistoryRequest.getProductCode());
        throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
      }
    } catch (Exception e) {
      log.error("error while submitHistory for product service client. requestId: {}, productCode: {}", requestId,
          productHistoryRequest.getProductCode());
    }
  }

  @Override
  public List<ConfigurationStatusResponse> getReviewConfigurationChanges(long fromDateInMillis) {
    GdnRestListResponse<ConfigurationStatusResponse> response =
        pcbFeign.getConfigurationChanges(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), fromDateInMillis);
    if (!response.isSuccess() || Objects.isNull(response.getContent())) {
      log.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public void updateProductCollectionAsPostLive(String storeId, String productCode) {
    GdnBaseRestResponse response =
        pbpFeign.updateProductAsPostLive(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productCode);
    if (!response.isSuccess()) {
      log.error(PBP_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public List<ConfigurationStatusResponse> getReviewConfiguration(List<ConfigurationStatusRequest> configurationStatusRequests) {
    GdnRestListResponse<ConfigurationStatusResponse> response =
        pcbFeign.getReviewConfiguration(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), configurationStatusRequests);
    if (!response.isSuccess() || Objects.isNull(response.getContent())) {
      log.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public ImageQcProcessedAndBrandResponse getImageQcPredictionResponse(String productCode) {
    GdnRestSingleResponse<ImageQcProcessedAndBrandResponse> response = pbpFeign
        .getImageQcPredictionAndBrandResponse(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productCode);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error(PBP_IMAGE_QC_ERROR_MESSAGE, response.getErrorMessage());
      return null;
    }
    return response.getValue();
  }

  @Override
  public List<CategoryHierarchyResponse> getCategoryHierarchyByCategoryCodes(CategoryCodeRequest categoryCodeRequest) {
    GdnRestListResponse<CategoryHierarchyResponse>  response = pcbFeign
        .filterCategoryHierarchyByCategoryCodes(Constants.DEFAULT_STORE_ID,
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
            Constants.DEFAULT_USERNAME, categoryCodeRequest);
    if (!response.isSuccess() || Objects.isNull(response.getContent())) {
      log.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public GdnRestSingleResponse<AutoApprovalTypeResponse> getAutoApprovalType(String storeId, String productCode,
      boolean onlyCategoryChange, AutoApprovalTypeRequest autoApprovalTypeRequest) {
    return pbpFeign.getAutoApprovalType(storeId, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, onlyCategoryChange, productCode,
        autoApprovalTypeRequest);
  }

  @Override
  public String getProductStatus(String storeId, String productCode) {
    String state = StringUtils.EMPTY;
    GdnRestSingleResponse<SimpleStringResponse> productStatus =
        pbpFeign.getProductStatus(storeId, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCode);
    if (Objects.nonNull(productStatus) && productStatus.isSuccess() && Objects.nonNull(productStatus.getValue())
        && StringUtils.isNotEmpty(productStatus.getValue().getResult())) {
      state = productStatus.getValue().getResult();
    }
    return state;
  }

  @Override
  public RetryAutoNeedRevisionResponse retryAutoNeedRevision(String storeId, RetryNeedRevisionRequest retryNeedRevisionRequest) {
    GdnRestSingleResponse<RetryAutoNeedRevisionResponse> response = pbpFeign.retryAutoNeedRevision(storeId,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, retryNeedRevisionRequest);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error(PBP_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public CategoryDetailResponse getCategoryDetailByCategoryCode(String categoryCode) {
    GdnRestSingleResponse<CategoryDetailResponse>  response = pcbFeign
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID,
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
            Constants.DEFAULT_USERNAME, categoryCode);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public boolean validateAutoQcConfigChange(AutoQcConfigChangeRequest autoQcConfigChangeRequest) {
    GdnRestSingleResponse<SingleValueResponse> response =
        pbpFeign.verifyAutoQcConfigChange(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            autoQcConfigChangeRequest);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return Boolean.valueOf(response.getValue().getValue());
  }

  @Override
  public CategoryCodeResponse getCnCategoryCodesFromC1(CategoryCodeRequest categoryCodeRequest) {
    GdnRestSingleResponse<CategoryCodeResponse> response =
        pcbFeign.getAllChildCategoriesFromC1CategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, categoryCodeRequest);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public boolean validateProtectedBrand(String brandCode, String sellerCode) {
    GdnRestSingleResponse<SimpleBooleanResponse> response =
        pcbFeign.validateAuthorisedBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, brandCode,
            sellerCode);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue().getResult();
  }

  @Override
  public BrandResponse filterByBrandName(String brandName) {
    GdnRestSingleResponse<BrandResponse> response =
        pcbFeign.filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, brandName);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public ProductResponse getProductBasicDetailByProductCode(String productCode) {
    GdnRestSingleResponse<ProductResponse> productResponse = pcbFeign
              .getProductBasicDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
                  Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
                  productCode);
    if (!productResponse.isSuccess() || Objects.isNull(productResponse.getValue())) {
      log.error(PCB_ERROR_MESSAGE + PRODUCT_CODE_MESSAGE, productResponse.getErrorMessage(), productCode);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, productResponse.getErrorMessage());
    }
    return productResponse.getValue();
  }

  @Override
  public void clearProductCacheSyncByProductIdAndProductCode(String id, String productCode) {
    GdnBaseRestResponse response = pcbFeign
        .clearProductCacheSyncByProductIdAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, id, productCode);
    if (!response.isSuccess()) {
      log.error(PCB_ERROR_MESSAGE + PRODUCT_CODE_MESSAGE, response.getErrorMessage(), productCode);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public PredefinedAllowedAttributeValueResponse getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
    String attributeCode, String value, boolean fetchByPredefinedAttributeCode) {
    GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse> response = pcbFeign
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, attributeCode, value, fetchByPredefinedAttributeCode);
    if (!response.isSuccess()) {
      log.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public ProductDetailResponse getProductDetailByProductCode(String productCode, boolean inAllProducts,
    boolean originalImages) {
    GdnRestSingleResponse<ProductDetailResponse> response = pcbFeign
      .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, inAllProducts,
        productCode, originalImages);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error(PCB_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public void processProductVendorSearchAutoHeal(String storeId, String productCode) {
    GdnBaseRestResponse response =
        pbpFeign.processProductVendorSearchAutoHeal(storeId, GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productCode);
    if (!response.isSuccess()) {
      log.error("Error while processing vendor search auto product in PBP productCode : {} , error : {}", productCode,
          response.getErrorMessage());
    }
  }

  @Override
  public List<PrdProductResponse> getProductBasicDetailByProductCodeFromXProduct(String productCode) {
    ProductSkuAndProductCodeRequest productSkuAndProductCodeRequest = new ProductSkuAndProductCodeRequest();
    productSkuAndProductCodeRequest.setProductCode(productCode);
    GdnRestListResponse<PrdProductResponse> productResponse =
        xProductFeign.getPrdProductDetailByProductSkuOrProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            productSkuAndProductCodeRequest);
    if (!productResponse.isSuccess() || CollectionUtils.isEmpty(productResponse.getContent())) {
      log.error(X_PRODUCT_ERROR_MESSAGE + PRODUCT_CODE_MESSAGE, productResponse.getErrorMessage(), productCode);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, productResponse.getErrorMessage());
    }
    return productResponse.getContent();
  }

  @Override
  public ProductSkuDetailResponse getProductDetailForProduct(String productSku,
      String productCode) {
    GdnRestSingleResponse<ProductSkuDetailResponse> response =
        pbpFeign.getProductSkuDetailResponse(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(),
            new ProductCodeAndSkuRequest(productSku, productCode));
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error("Success is false while fetching from pbp for productSku: {} with error: {}",
          productSku, response.getErrorMessage());
      if (ErrorMessages.PRODUCT_SKU_NOT_FOUND_IN_X_PRODUCT.equals(response.getErrorMessage())) {
        throw new ValidationException(
            com.gdn.x.mta.distributiontask.model.ErrorCategory.PRODUCT_SKU_NOT_FOUND.getCode(),
            com.gdn.x.mta.distributiontask.model.ErrorCategory.PRODUCT_SKU_NOT_FOUND.getMessage());
      }
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public SellerAnalyticsResponse getSellerAnalyticsResponse(String sellerCode) {
    GdnRestSingleResponse<SellerAnalyticsResponse> sellerAnalyticsDetailsResponse =
        productAnalyticsFeign.getSellerAnalyticsDetailsBySellerCode(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), sellerCode);
    if (!sellerAnalyticsDetailsResponse.isSuccess() || Objects.isNull(
        sellerAnalyticsDetailsResponse.getValue())) {
      log.error("Seller Analytics details not found for sellerCode: {} message: {} ", sellerCode,
          sellerAnalyticsDetailsResponse.getErrorMessage());
      return null;
    }
    return sellerAnalyticsDetailsResponse.getValue();
  }

  @Override
  public void suspendIprProduct(SuspensionProductRequest suspensionProductRequest) {
    GdnBaseRestResponse response =
        pbpFeign.doSuspensionAction(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), suspensionProductRequest);
    if (!response.isSuccess()) {
      log.error("Error occurred while trying to suspend product from pbp for request: {}",
          suspensionProductRequest);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public CategoryDTO fetchParentCategoryFromCnCategoryCode(String categoryCode) {
    List<CategoryHierarchyResponse> hierarchyResponses = getCategoryHierarchyByCategoryCodes(
      new CategoryCodeRequest(Collections.singletonList(categoryCode)));
    return hierarchyResponses.stream().findFirst().map(
        response -> response.getCategoryHierarchy().stream()
          .filter(categoryResponse -> Objects.isNull(categoryResponse.getParentCategoryId())).map(
            categoryResponse -> new CategoryDTO(categoryResponse.getCategoryCode(),
              categoryResponse.getName())).findFirst()
          .orElse(new CategoryDTO(StringUtils.EMPTY, StringUtils.EMPTY)))
      .orElse(new CategoryDTO(StringUtils.EMPTY, StringUtils.EMPTY));
  }
}
