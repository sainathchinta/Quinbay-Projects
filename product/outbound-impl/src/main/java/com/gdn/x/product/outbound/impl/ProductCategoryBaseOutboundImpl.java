package com.gdn.x.product.outbound.impl;

import static com.gdn.common.base.GdnPreconditions.checkState;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import com.gdn.x.productcategorybase.dto.request.ProductMigrationRequest;
import com.gdn.x.productcategorybase.dto.response.BasicInfoProductResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleListStringResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.outbound.api.feign.PCBFeign;
import com.gdn.x.product.rest.web.model.response.SizeChartResponse;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryEligibleForSizeChartResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryShippingWeightResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.ProductSalesCategoryMappingResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductCategoryBaseOutboundImpl implements ProductCategoryBaseOutbound {

  private static final String CATEGORY_NOT_FOUND_WITH_ID = "category not found with id : ";
  private static final String CATEGORY_NOT_FOUND_FOR_PRODUCT_CODE = "category not found for product code : ";

  private static final Logger LOGGER = LoggerFactory
      .getLogger(ProductCategoryBaseOutboundImpl.class);
  private static final String X_PRODUCT_CLIENT_ID = "x-product";
  private static final String X_PRODUCT_CHANNEL_ID = "x-product";
  public static final String PRODUCT_ATTRIBUTE_UPDATE = "PRODUCT_ATTRIBUTE_UPDATE";

  public static final int PAGE = 0;
  public static final int SIZE = 0;

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public CategoryDetailResponse getCategoryDetailByCategoryCode(String requestId, String username,
      String categoryCode) {
    try {
      GdnRestSingleResponse<CategoryDetailResponse> categoryResponse = this.pcbFeign
          .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID_X_PRODUCT, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), categoryCode);
      if (categoryResponse.isSuccess()) {
        return categoryResponse.getValue();
      } else {
        ProductCategoryBaseOutboundImpl.LOGGER.error(
            "#getCategoryByCategoryCode with categoryCode : {}, error : {}", categoryCode,
            categoryResponse.getErrorMessage());
        return null;
      }
    } catch (Exception e) {
      ProductCategoryBaseOutboundImpl.LOGGER.error(
          "#getCategoryByCategoryCode with categoryCode : {}, error : {}", categoryCode,
          e.getMessage(), e);
      return null;
    }
  }

  @Override
  public CategoryDetailResponse getCategoryDetail(String requestId, String username,
      String categoryId) {
    try {
      return this.pcbFeign.getCategoryDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID_X_PRODUCT, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), categoryId).getValue();
    } catch (Exception e) {
      ProductCategoryBaseOutboundImpl.LOGGER.error(
          "#getCategoryDetail with categoryId : {} , error: {}", categoryId, e.getMessage(), e);
    }
    return null;
  }

  @Override
  public List<CategoryResponse> getCategoryHierarchy(String requestId, String username,
      String categoryCode) {
    List<CategoryResponse> responses = new ArrayList<CategoryResponse>();
    try {
      responses = this.pcbFeign
          .filterCategoryHierarchyByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID_X_PRODUCT, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), categoryCode).getContent();
    } catch (Exception e) {
      ProductCategoryBaseOutboundImpl.LOGGER.error(
          "#getCategoryHierarchy with categoryCode : {}, error: {}", categoryCode, e.getMessage(),
          e);
    }
    return responses;
  }

  @Override
  public ProductDetailResponse getProductDetailByProductCode(String requestId, String username,
      String productCode) throws Exception {
    GdnRestListRequest listRequest =
        new GdnRestListRequest(ProductCategoryBaseOutboundImpl.PAGE,
            ProductCategoryBaseOutboundImpl.SIZE);
    listRequest.setRequestId(requestId);
    ProductDetailResponse response = null;
    try {
      response = this.pcbFeign.getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID_X_PRODUCT, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), productCode, false, false).getValue();
    } catch (Exception e) {
      ProductCategoryBaseOutboundImpl.LOGGER.warn(
          "#getProductDetailByProductCode with productCode : {}, error: {}", productCode,
          e.getMessage(), e);
      throw e;
    }
    return response;
  }

  @Override
  public ProductDetailResponse getProductDetailByProductCodeForAllProducts(String requestId, String username,
      String productCode, boolean inAllProducts) throws Exception {
    GdnRestListRequest listRequest =
        new GdnRestListRequest(ProductCategoryBaseOutboundImpl.PAGE, ProductCategoryBaseOutboundImpl.SIZE);
    listRequest.setRequestId(requestId);
    ProductDetailResponse response = null;
    response = this.pcbFeign
        .getProductDetailByProductCodeInAllProducts(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productCode, inAllProducts, false).getValue();
    checkCategoryResponseEmpty(response, productCode);
    CategoryResponse masterCategoryResponse = response.getProductCategoryResponses().get(0).getCategory();
    CategoryDetailResponse categoryDetailResponse =
        this.getCategoryDetail(requestId, username, masterCategoryResponse.getId());
    checkState(categoryDetailResponse != null,
        ProductCategoryBaseOutboundImpl.CATEGORY_NOT_FOUND_WITH_ID + masterCategoryResponse.getId());
    masterCategoryResponse.setCatalog(categoryDetailResponse.getCatalog());
    return response;
  }

  private static void checkCategoryResponseEmpty(ProductDetailResponse response, String productCode) {
    checkState(CollectionUtils.isNotEmpty(response.getProductCategoryResponses()) && Objects.nonNull(
            response.getProductCategoryResponses().get(0)),
        ProductCategoryBaseOutboundImpl.CATEGORY_NOT_FOUND_FOR_PRODUCT_CODE + productCode);
  }

  @Override
  public ProductItemDetailResponse getProductItemDetailByItemCode(String requestId,
      String username, String itemCode) {
    GdnRestListRequest listRequest =
        new GdnRestListRequest(ProductCategoryBaseOutboundImpl.PAGE,
            ProductCategoryBaseOutboundImpl.SIZE);
    listRequest.setRequestId(requestId);
    ProductItemDetailResponse response = null;
    try {
      response = this.pcbFeign.getProductItemDetailBySkuCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID_X_PRODUCT, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), itemCode, false).getValue();
    } catch (Exception e) {
      ProductCategoryBaseOutboundImpl.LOGGER.error(
          "#getProductItemDetailBySkuCode with itemCode : {}, error: {}", itemCode, e.getMessage(),
          e);
    }
    return response;
  }

  @Override
  public List<CategoryResponse> getMasterParentCategoryResponseByProductCode(String requestId,
      String username, String productCode) {
    List<CategoryResponse> response = Collections.emptyList();
    try {
      response = this.pcbFeign
          .getMasterParentCategoriesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID_X_PRODUCT, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), productCode).getContent();
    } catch (Exception e) {
      ProductCategoryBaseOutboundImpl.LOGGER
          .error("Error while getMasterParentCategoryResponse with productCode : {}, error: {}",
              productCode, e.getMessage(), e);
    }
    return response;
  }

  @Override
  public GdnRestSingleResponse<ProductSalesCategoryMappingResponse> getSalesCategoryMappingByProductCode(String storeId,
      String requestId, String username, String productCode, boolean ignoreHalalCategories) {
    GdnRestSingleResponse<ProductSalesCategoryMappingResponse> response = null;
    try {
      response = this.pcbFeign.getSalesCategoryMapping(storeId, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), productCode,
          ignoreHalalCategories);
    } catch (Exception e) {
      ProductCategoryBaseOutboundImpl.LOGGER
          .error("Error while getSalesCategoryMappingByProductCode with productCode : {}, error: {}",
              productCode, e.getMessage(), e);
      response = new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
    return response;
  }

  @Override
  public double generateShippingWeight(String storeId, String categoryCode, double length, double width, double height,
      double weight) {
    GdnRestSingleResponse<CategoryShippingWeightResponse> response = null;
    double shippingWeight = 0;
    try {
      response = this.pcbFeign
          .generateShippingWeight(storeId, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, X_PRODUCT_CLIENT_ID,
              X_PRODUCT_CLIENT_ID, categoryCode, length, width, height, weight);
    } catch (Exception e) {
      ProductCategoryBaseOutboundImpl.LOGGER
          .error("Error while generateShippingWeight with categoryCode : {}, error: {}", categoryCode, e.getMessage(),
              e);
      response = new GdnRestSingleResponse<>(e.getMessage(), null, false, null, X_PRODUCT_CHANNEL_ID);
    }
    if (Objects.nonNull(response) && Objects.nonNull(response.getValue())) {
      shippingWeight = response.getValue().getShippingWeight();
    }
    return shippingWeight;
  }

  @Override
  public CategoryNamesResponse getCategoryNames(List<String> categoryCodes) {
    GdnRestSingleResponse<CategoryNamesResponse> response = null;
    try {
      CategoryMultipleIdRequest categoryMultipleIdRequest = new CategoryMultipleIdRequest();
      categoryMultipleIdRequest.setCategoryCode(categoryCodes);
      response = this.pcbFeign.getCategoryNames(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
          Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, 0, categoryCodes.size(), categoryMultipleIdRequest);
    } catch (Exception e) {
      ProductCategoryBaseOutboundImpl.LOGGER
          .error("Error while fetching the category names for category codes : {}, error: {}", categoryCodes,
              e.getMessage(), e);
      response = new GdnRestSingleResponse<>(e.getMessage(), null, false, null, X_PRODUCT_CHANNEL_ID);
    }
    return response.getValue();
  }

  @Override
  public List<String> getCnCategoryCodes(List<String> categoryCodes) {
    GdnRestSingleResponse<CategoryCodeResponse> response;
    try {
      CategoryCodeRequest request = new CategoryCodeRequest();
      request.setCategoryCodes(categoryCodes);
      response = this.pcbFeign.getAllChildCategoriesFromC1CategoryCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
          Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, request);
    }catch (Exception e){
      ProductCategoryBaseOutboundImpl.LOGGER
          .error("Error while fetching the Cn category codes for C1 category codes : {}, error: {}", categoryCodes,
              e.getMessage(), e);
      response = new GdnRestSingleResponse<>(e.getMessage(), null, false, null, X_PRODUCT_CHANNEL_ID);
    }
    if (Objects.nonNull(response) && Objects.nonNull(response.getValue())) {
      return response.getValue().getCategoryCodes();
    }
    return null;
  }

  @Override
  public ValidOmniChannelSkuResponse checkOmniChannelSkuExistsInSeller(String storeId,
      String requestId, String username, boolean needUomInfo, OmniChannelSkuRequest request) throws ApplicationException {
    GdnRestSingleResponse<ValidOmniChannelSkuResponse> response =
        this.pcbFeign.checkOmniChannelSkuExistsInSeller(storeId, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            needUomInfo, request);
    if (Objects.isNull(response) || !response.isSuccess()) {
      ProductCategoryBaseOutboundImpl.LOGGER.error("checkOmniChannelSkuExistsInSeller unsuccessful. Error: {}",
          Objects.nonNull(response) ? response.getErrorMessage() : "null response");
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, ErrorMessages.FAILED_TO_FETCH_PCB_DATA);
    }
    return response.getValue();
  }

  @Override
  public ProductMasterDataResponse getMasterDataForTransaction(String itemCode) throws ApplicationException {
    GdnRestSingleResponse<ProductMasterDataResponse> response =
      this.pcbFeign.getMasterProductDetailsByItemCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, itemCode);
    if (Objects.isNull(response) || !response.isSuccess()) {
      log.error("Error fetching master data details for sku code : {}, error : {}", itemCode,
        Optional.ofNullable(response).orElse(new GdnRestSingleResponse<>()).getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, ErrorMessages.FAILED_TO_FETCH_PCB_DATA);
    }
    return response.getValue();
  }

  @Override
  public List<ProductItemDetailResponse> findProductItemDetailsBySkuCodes(List<String> skuCodes,
    boolean fetchArchived, boolean originalImages) throws Exception {
    GdnRestListResponse<ProductItemDetailResponse> response = null;
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setSkuCodes(skuCodes);
    skuCodesRequest.setFetchArchived(fetchArchived);
    try {
      response = this.pcbFeign.filterProductItemBySkuCodes(Constants.DEFAULT_STORE_ID,
        X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, originalImages, skuCodesRequest);
    } catch (Exception e) {
      ProductCategoryBaseOutboundImpl.LOGGER.error(
        "Error while fetching the Product Item Detail Response for Skus : {}, error: {}", skuCodes,
        e.getMessage(), e);
      response = new GdnRestListResponse<>(e.getMessage(), null, false, null, null,
        Constants.DEFAULT_REQUEST_ID);
    }
    if (Objects.nonNull(response) && CollectionUtils.isNotEmpty(response.getContent())) {
      return new ArrayList<>(response.getContent());
    }
    return new ArrayList<>();
  }

  @Override
  public List<ImageResponse> getProductImagesByProductCode(String productCode) {
    GdnRestListResponse<ImageResponse> response;
    try {
      response = this.pcbFeign.getProductImagesByProductCode(Constants.DEFAULT_STORE_ID,
          X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCode);
    } catch (Exception e) {
      ProductCategoryBaseOutboundImpl.LOGGER.error(
          "Error while fetching the Product Image Response for product-code : {}, error: {} ", productCode, e.getMessage(), e);
      response = new GdnRestListResponse<>(e.getMessage(), null, false, null, null,
          Constants.DEFAULT_REQUEST_ID);
    }
    if (Objects.nonNull(response) && CollectionUtils.isNotEmpty(response.getContent())) {
      return new ArrayList<>(response.getContent());
    }
    return new ArrayList<>();
  }

  @Override
  public GdnRestListResponse<ItemImageResponse> getProductItemImagesByItemCode(SkuCodesRequest skuCodesRequest) {
    GdnRestListResponse<ItemImageResponse> response;
    try {
      response = this.pcbFeign.getProductItemImagesByItemCode(Constants.DEFAULT_STORE_ID,
          X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, skuCodesRequest);
    } catch (Exception e) {
      ProductCategoryBaseOutboundImpl.LOGGER.error(
          "Error while fetching the Product Item Image Response for item-codes : {}, error: {} ", skuCodesRequest, e.getMessage(), e);
      response = new GdnRestListResponse<>(e.getMessage(), null, false, null, null,
          Constants.DEFAULT_REQUEST_ID);
    }
      return response;
  }

  @Override
  public AttributeResponse getAttributeDetailByAttributeCode(String attributeCode) throws Exception {
    GdnRestSingleResponse<AttributeResponse> response =
        pcbFeign.getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, attributeCode);
    if (Objects.isNull(response.getValue()) || !response.isSuccess()) {
      log.error("Error fetching attribute details for attribute code : {}, error - {}", attributeCode,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, ErrorMessages.FAILED_TO_FETCH_PCB_DATA);
    }
    return response.getValue();
  }

  @Override
  public ProductResponse getProductBasicDetails(String productCode) {
    GdnRestSingleResponse<ProductResponse> response =
        pcbFeign.getProductBasicDetails(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCode);
    if (Objects.isNull(response) || !response.isSuccess()) {
      log.error("Error fetching product details for product code : {}, error - {}", productCode,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, ErrorMessages.FAILED_TO_FETCH_PCB_DATA);
    }
    return response.getValue();
  }

  @Override
  public ProductAndAttributeDetailResponse getProductAndAttributeDetails(String productCode, boolean inAllProducts) {
    GdnRestSingleResponse<ProductAndAttributeDetailResponse> response =
        pcbFeign.getProductAndAttributeDetails(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCode, inAllProducts);
    if (Objects.isNull(response) || !response.isSuccess()) {
      log.error("Error fetching product and attribute details for product code : {}, error - {}", productCode,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, ErrorMessages.FAILED_TO_FETCH_PCB_DATA);
    }
    return response.getValue();
  }

  @Override
  public String getBrandLogoUrl(String brandCode) {
    String brandLogoUrl = StringUtils.EMPTY;
    GdnRestSingleResponse<BrandResponse> brandResponse =
        pcbFeign.getBrandResponseByBrandCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), brandCode);
    if (Objects.isNull(brandResponse) || !brandResponse.isSuccess()) {
      log.error("Error fetching brand details by brand code : {}, error - {}", brandCode, brandResponse);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, ErrorMessages.FAILED_TO_FETCH_PCB_DATA);
    }
    if (Objects.nonNull(brandResponse.getValue())) {
      brandLogoUrl = brandResponse.getValue().getBrandLogoPath();
    }
    return brandLogoUrl;
  }

  @Override
  public SizeChartResponse fetchSizeChartDetails(String storeId, String sizeChartCode)
      throws ApplicationRuntimeException {
    GdnRestSingleResponse<SizeChartResponse> response =
        pcbFeign.fetchSizeChartDetails(storeId, GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), true, sizeChartCode);
    if (Objects.isNull(response) || !response.isSuccess()) {
      String errorMessage =
          Objects.nonNull(response) ? response.getErrorMessage() : ErrorMessages.FAILED_TO_FETCH_PCB_DATA;
      log.error("Error fetching size chart details for size chart code : {}, error - {}", sizeChartCode, errorMessage);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, errorMessage);
    }
    return response.getValue();
  }

  @Override
  public boolean isSizeChartCodeValid(String sizeChartCode) {
    GdnBaseRestResponse response = pcbFeign.validateSizeChartCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), sizeChartCode);
    return response.isSuccess();
  }

  @Override
  public Map<String, Boolean> getCategoryAndEligibleFlagMap(String storeId, String sizeChartAttributeCode,
      List<String> categoryCodes) {
    GdnRestSingleResponse<CategoryEligibleForSizeChartResponse> response =
        pcbFeign.checkCategoryEligibleForSizeChartAddition(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            categoryCodes, sizeChartAttributeCode);
    if (Objects.isNull(response) || !response.isSuccess()) {
      String errorMessage =
          Objects.nonNull(response) ? response.getErrorMessage() : ErrorMessages.FAILED_TO_FETCH_PCB_DATA;
      log.error(
          "Error while fetching category attributes for categoryCodes : {} sizeChartAttributeCode {} , error - {}",
          categoryCodes, sizeChartAttributeCode, errorMessage);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, errorMessage);
    }
    return response.getValue().getCategoryCodeAndEligibilityMap();
  }

  @Override
  public List<String> getCategoryCodesByAttributeCode(String storeId, String attributeCode) {
    GdnRestSingleResponse<SimpleListStringResponse> response =
        pcbFeign.getCategoryCodesByAttributeCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), attributeCode);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          ErrorMessages.FAILED_TO_FETCH_PCB_DATA);
    }
    return response.getValue().getValues();
  }

  @Override
  public void updateStatusInPCBForBackFillAttributes(String productCode, String updatedStatus, String errorMessage) {
    ProductMigrationRequest productMigrationRequest = new ProductMigrationRequest();
    productMigrationRequest.setProductCode(productCode);
    productMigrationRequest.setUpdatedStatus(updatedStatus);
    productMigrationRequest.setMigrationType(PRODUCT_ATTRIBUTE_UPDATE);
    productMigrationRequest.setErrorMessage(errorMessage);
    GdnBaseRestResponse updateProductMigrationStatus =
        pcbFeign.updateProductMigrationStatus(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), productMigrationRequest);
    if (!updateProductMigrationStatus.isSuccess()) {
      log.error("Failed to update the status in pcb for product code {}: with error {} : ", productCode,
          updateProductMigrationStatus.getErrorMessage());
    }
  }

  @Override
  public List<BasicInfoProductResponse> getBasicInfoProductDetailsListByProductCodes(List<String> productCodes) {
    GdnRestListResponse<BasicInfoProductResponse> response = null;
    try {
      response =
          this.pcbFeign.getBasicInfoProductDetailsListByProductCodes(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
              X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCodes);
    } catch (Exception e) {
      log.error("Error while fetching the Product basic info Detail Response for Skus : {}, error: {}", productCodes,
          e.getMessage(), e);
      response = new GdnRestListResponse<>(e.getMessage(), null, false, null, null, Constants.DEFAULT_REQUEST_ID);
    }
    if (Objects.nonNull(response) && CollectionUtils.isNotEmpty(response.getContent())) {
      return new ArrayList<>(response.getContent());
    }
    return new ArrayList<>();
  }
}
