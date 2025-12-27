package com.gdn.x.productcategorybase.service.client;

import java.util.Date;
import java.util.List;

import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.web.client.GdnBaseRestCrudClient;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.CatalogType;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.request.AddProductAttributesRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeAddRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemMultipleUpcCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductOptionalParameterRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductImagesRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeSummaryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductImagesPathResponse;
import com.gdn.x.productcategorybase.dto.response.MapResponse;
import com.gdn.x.productcategorybase.dto.response.MasterProductResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductOptionalParameterResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;

public class ProductClient extends GdnBaseRestCrudClient {

  public final static String APPLICATION_JSON_VALUE = "application/json";

  private final ProductClientPredefinedAllowedAttributeValue productClientPredefinedAllowedAttributeValue;

  private final ProductClientAttribute productClientAttribute;

  private final ProductClientCatalog productClientCatalog;

  private final ProductClientCategory productClientCategory;

  private final ProductClientProduct productClientProduct;

  private final ProductClientProductOptionalParameter productClientProductOptionalParameter;

  public ProductClient(GdnRestClientConfiguration clientConfig, String contextPath) {
    super(clientConfig);
    this.setContextPath(contextPath);
    this.productClientAttribute = new ProductClientAttribute(clientConfig, contextPath);
    this.productClientCatalog = new ProductClientCatalog(clientConfig, contextPath);
    this.productClientCategory = new ProductClientCategory(clientConfig, contextPath);
    this.productClientProduct = new ProductClientProduct(clientConfig, contextPath);
    this.productClientProductOptionalParameter = new ProductClientProductOptionalParameter(clientConfig, contextPath);
    this.productClientPredefinedAllowedAttributeValue =
        new ProductClientPredefinedAllowedAttributeValue(clientConfig, contextPath);

  }

  public ProductClient(String username, String password, String host, Integer port, String storeId, String clientId,
      String channelId, String contextPath) {
    super(username, password, host, port, clientId, channelId, storeId, contextPath);
    this.productClientAttribute =
        new ProductClientAttribute(username, password, host, port, storeId, clientId, channelId, contextPath);
    this.productClientCatalog =
        new ProductClientCatalog(username, password, host, port, storeId, clientId, channelId, contextPath);
    this.productClientCategory =
        new ProductClientCategory(username, password, host, port, storeId, clientId, channelId, contextPath);
    this.productClientProduct =
        new ProductClientProduct(username, password, host, port, storeId, clientId, channelId, contextPath);
    this.productClientProductOptionalParameter =
        new ProductClientProductOptionalParameter(username, password, host, port, storeId, clientId, channelId,
            contextPath);
    this.productClientPredefinedAllowedAttributeValue =
        new ProductClientPredefinedAllowedAttributeValue(username, password, host, port, clientId, channelId, storeId,
            contextPath);
  }

  public GdnBaseRestResponse activateProduct(String requestId, String username, ProductRequest request)
      throws Exception {
    return this.productClientProduct.activateProduct(requestId, username, request);
  }

  public GdnBaseRestResponse checkProductByProductCode(String requestId, String username, String productCode)
      throws Exception {
    return this.productClientProduct.checkProductByProductCode(requestId, username, productCode);
  }

  public GdnBaseRestResponse checkProductItemBySkuCode(String requestId, String username, String skuCode)
      throws Exception {
    return this.productClientProduct.checkProductItemBySkuCode(requestId, username, skuCode);
  }

  public GdnBaseRestResponse createNewCatalog(String requestId, String username, CatalogRequest catalogRequest)
      throws Exception {
    return this.productClientCatalog.createNewCatalog(requestId, username, catalogRequest);
  }

  public GdnBaseRestResponse createNewProduct(String requestId, String username, ProductRequest request)
      throws Exception {
    return this.productClientProduct.createNewProduct(requestId, username, request);
  }

  public GdnBaseRestResponse createNewProductWithSpecificationDetailGeneratedBySystem(String requestId,
      String username, ProductRequest request) throws Exception {
    return this.productClientProduct.createNewProductWithSpecificationDetailGeneratedBySystem(requestId, username,
        request);
  }

  public GdnBaseRestResponse deactivated(String requestId, String username, String id) throws Exception {
    return this.productClientPredefinedAllowedAttributeValue.deactivated(requestId, username, id);
  }

  public GdnBaseRestResponse deactivateProduct(String requestId, String username, ProductRequest request)
      throws Exception {
    return this.productClientProduct.deactivateProduct(requestId, username, request);
  }

  public GdnBaseRestResponse deleteAllowedAttributeValue(String requestId, String username, SimpleRequestHolder holder)
      throws Exception {
    return this.productClientAttribute.deleteAllowedAttributeValue(requestId, username, holder);
  }

  public GdnBaseRestResponse deleteCatalog(String requestId, String username, SimpleRequestHolder holder)
      throws Exception {
    return this.productClientCatalog.deleteCatalog(requestId, username, holder);
  }

  public GdnBaseRestResponse deleteCategory(String requestId, String username, SimpleRequestHolder holder)
      throws Exception {
    return this.productClientCategory.deleteCategory(requestId, username, holder);
  }

  public GdnBaseRestResponse deleteProductOptionalParameter(String requestId, String username,
      SimpleRequestHolder holder) throws Exception {
    return this.productClientProductOptionalParameter.deleteProductOptionalParameter(requestId, username, holder);
  }

  public GdnBaseRestResponse discardProduct(String requestId, String username, ProductRequest request) throws Exception {
    return this.productClientProduct.discardProduct(requestId, username, request);
  }

  public GdnRestListResponse<CategoryResponse> filterCategoryHierarchyByCategoryCode(String requestId, String username,
      String categoryCode) throws Exception {
    return this.productClientCategory.filterCategoryHierarchyByCategoryCode(requestId, username, categoryCode);
  }
  
  public GdnRestListResponse<CategoryResponse> filterSummaryByCategoryNameAndCatalogType(GdnRestListRequest request,
      String username, String categoryName, CatalogType catalogType) throws Exception {
    return this.productClientCategory.filterSummaryByCategoryNameAndCatalogType(request, username, categoryName,
        catalogType);
  }

  public GdnRestListResponse<AttributeResponse> getAttributeByAttributeCodeAndPageable(GdnRestListRequest listRequest,
      String username, String attributeCode) throws Exception {
    return this.productClientAttribute.getAttributeByAttributeCode(listRequest, username, attributeCode);
  }

  public GdnRestListResponse<AttributeResponse> getAttributeByAttributeTypeAndPageable(GdnRestListRequest listRequest,
      String username, AttributeType attributeType) throws Exception {
    return this.productClientAttribute.getAttributeByAttributeTypeAndPageable(listRequest, username, attributeType);
  }

  public GdnRestListResponse<AttributeResponse> getAttributeByNameLikeWithAndPageable(GdnRestListRequest listRequest,
      String username, String name) throws Exception {
    return this.productClientAttribute.getAttributeByNameLikeWithAndPageable(listRequest, username, name);
  }

  public GdnRestListResponse<AttributeResponse> getAttributeByNameStartingWithAndPageable(
      GdnRestListRequest listRequest, String username, String name) throws Exception {
    return this.productClientAttribute.getAttributeByNameStartingWithAndPageable(listRequest, username, name);
  }

  public GdnRestListResponse<AttributeResponse> getAttributeBySearchAbleFalseAndPageable(
      GdnRestListRequest listRequest, String username) throws Exception {
    return this.productClientAttribute.getAttributeBySearchAbleFalseAndPageable(listRequest, username);
  }

  public GdnRestListResponse<AttributeResponse> getAttributeBySearchAbleTrueAndPageable(GdnRestListRequest listRequest,
      String username) throws Exception {
    return this.productClientAttribute.getAttributeBySearchAbleTrueAndPageable(listRequest, username);
  }

  public GdnRestSingleResponse<AttributeResponse> getAttributeDetail(GdnRestListRequest listRequest, String username,
      String attributeId) throws Exception {
    return this.productClientAttribute.getAttributeDetail(listRequest, username, attributeId);
  }
  
  public GdnRestListResponse<AttributeResponse> getAttributeDetailByAttributeCodes(String requestId, String username,
      AttributeCodesRequest attributeCodesRequest) throws Exception {
    return this.productClientAttribute.getAttributeDetailByAttributeCodes(requestId, username, attributeCodesRequest);
  }

  public GdnRestListResponse<AttributeResponse> getAttributeSummary(GdnRestListRequest listRequest, String username)
      throws Exception {
    return this.productClientAttribute.getAttributeSummary(listRequest, username);
  }

  public GdnRestListResponse<CatalogResponse> getCatalogByName(GdnRestListRequest listRequest, String username,
      String name) throws Exception {
    return this.productClientCatalog.getCatalogByName(listRequest, username, name);
  }

  public GdnRestListResponse<CatalogResponse> getCatalogByType(GdnRestListRequest listRequest, String username,
      CatalogType catalogType) throws Exception {
    return this.productClientCatalog.getCatalogByType(listRequest, username, catalogType);
  }

  public GdnRestSingleResponse<CatalogDetailResponse> getCatalogDetail(String requestId, String username,
      String catalogId) throws Exception {
    return this.productClientCatalog.getCatalogDetail(requestId, username, catalogId);
  }

  public GdnRestListResponse<CatalogResponse> getCatalogSummary(GdnRestListRequest listRequest, String username)
      throws Exception {
    return this.productClientCatalog.getCatalogSummary(listRequest, username);
  }

  public GdnRestListResponse<CategoryResponse> getCategoryByNamePageable(GdnRestListRequest listRequest,
      String username, String name) throws Exception {
    return this.productClientCategory.getCategoryByNamePageable(listRequest, username, name);
  }

  public GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetail(String requestId, String username,
      String categoryId) throws Exception {
    return this.productClientCategory.getCategoryDetail(requestId, username, categoryId);
  }

  public GdnRestListResponse<CategoryResponse> getCategorySummary(GdnRestListRequest listRequest, String username)
      throws Exception {
    return this.productClientCategory.getCategorySummary(listRequest, username);
  }

  public GdnRestListResponse<CategoryResponse> getChildFromParent(GdnRestListRequest listRequest, String username,
      String categoryId) throws Exception {
    return this.productClientCategory.getChildFromParent(listRequest, username, categoryId);
  }

  public GdnRestListResponse<CategoryDTO> getChildFromParentAndCatalogCount(GdnRestListRequest listRequest,
      String username, String categoryId, String catalogId) throws Exception {
    return this.productClientCategory.getChildFromParentByCatalogIdWithChildCount(listRequest, username, categoryId,
        catalogId);
  }

  public GdnRestListResponse<CategoryDTO> getChildFromParentCount(GdnRestListRequest listRequest, String username,
      String categoryId) throws Exception {
    return this.productClientCategory.getChildFromParentWithChildCount(listRequest, username, categoryId);
  }
  
  public GdnRestSingleResponse<SingleObjectResponse> getFinalParentCategoryCached(String requestId, 
      String username, String categoryId) throws Exception {
    
    return this.productClientCategory.getFinalParentCategoryCached(requestId, username, categoryId);
  }

  public GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getPredefinedAllowedAttributeValueSummary(
      GdnRestListRequest listRequest, String username, String attributeId, String value) throws Exception {
    return this.productClientPredefinedAllowedAttributeValue.findByStoreIdAndAttributeIdAndMarkForDeleteFalse("",
        username, listRequest, attributeId, value);
  }

  public GdnRestListResponse<ProductResponse> getProductByBrand(GdnRestListRequest listRequest, String username,
      String brandName) throws Exception {
    return this.productClientProduct.getProductByBrand(listRequest, username, brandName);
  }

  public GdnRestListResponse<ProductResponse> getProductByMarkForDelete(GdnRestListRequest listRequest,
      String username, String markForDelete) throws Exception {
    return this.productClientProduct.getProductByMarkForDelete(listRequest, username, markForDelete);
  }

  public GdnRestListResponse<ProductResponse> getProductByName(GdnRestListRequest listRequest, String username,
      String name) throws Exception {
    return this.productClientProduct.getProductByName(listRequest, username, name);
  }

  public GdnRestListResponse<ProductResponse> getProductByNameAndCreatedBy(GdnRestListRequest listRequest,
      String username, String name, String createdBy) throws Exception {
    return this.productClientProduct.getProductByNameAndCreatedBy(listRequest, username, name, createdBy);
  }

  public GdnRestListResponse<ProductResponse> getProductByNameAndViewableAndActivated(GdnRestListRequest listRequest,
      String username, String name, boolean viewable, boolean activated) throws Exception {
    return this.productClientProduct.getProductByNameAndViewableAndActivated(listRequest, username, name, viewable,
        activated);
  }

  public GdnRestListResponse<ProductResponse> getProductByNameAndViewableAndActivatedAndUpdatedBy(
      GdnRestListRequest listRequest, String username, String name, boolean viewable, boolean activated,
      String updatedBy) throws Exception {
    return this.productClientProduct.getProductByNameAndViewableAndActivatedAndUpdatedBy(listRequest, username, name,
        viewable, activated, updatedBy);
  }

  /**
   * @deprecated use {@link #getProductByProductCodeExactMatch(GdnRestListRequest, String, String)}
   * for product code exact match
   *
   * @param listRequest
   * @param username
   * @param productCode
   * @return
   * @throws Exception
   */
  @Deprecated
  public GdnRestListResponse<ProductResponse> getProductByProductCode(GdnRestListRequest
      listRequest, String username, String productCode) throws Exception {
    return this.productClientProduct.getProductByProductCode(listRequest, username, productCode);
  }

  public GdnRestListResponse<ProductResponse> getProductByShippingWeightBiggerOrEqual(GdnRestListRequest listRequest,
      String username, String shippingWeight) throws Exception {
    return this.productClientProduct.getProductByShippingWeightBiggerOrEqual(listRequest, username, shippingWeight);
  }

  public GdnRestListResponse<ProductResponse> getProductByShippingWeightLesserOrEqual(GdnRestListRequest listRequest,
      String username, String shippingWeight) throws Exception {
    return this.productClientProduct.getProductByShippingWeightLesserOrEqual(listRequest, username, shippingWeight);
  }

  public GdnRestListResponse<ProductResponse> getProductByUniqueSellingCode(GdnRestListRequest listRequest,
      String username, String uniqueSellingCode) throws Exception {
    return this.productClientProduct.getProductByUniqueSellingCode(listRequest, username, uniqueSellingCode);
  }

  @Deprecated
  public GdnRestListResponse<ProductResponse> getProductByViewable(GdnRestListRequest listRequest, String username,
      boolean viewable) throws Exception {
    return this.productClientProduct.getProductByViewable(listRequest, username, viewable);
  }

  public GdnRestListResponse<ProductResponse> getProductByViewableAndActivated(GdnRestListRequest listRequest,
      String username, boolean viewable, boolean activated) throws Exception {
    return this.productClientProduct.getProductByViewableAndActivated(listRequest, username, viewable, activated);
  }

  public GdnRestListResponse<ProductResponse> getProductByWeightBiggerOrEqual(GdnRestListRequest listRequest,
      String username, String weight) throws Exception {
    return this.productClientProduct.getProductByWeightBiggerOrEqual(listRequest, username, weight);
  }

  public GdnRestListResponse<ProductResponse> getProductByWeightLesserOrEqual(GdnRestListRequest listRequest,
      String username, String weight) throws Exception {
    return this.productClientProduct.getProductByWeightLesserOrEqual(listRequest, username, weight);
  }

  public GdnRestSingleResponse<ProductDetailResponse> getProductDetail(GdnRestListRequest listRequest, String username,
      String productId) throws Exception {
    return this.productClientProduct.getProductDetail(listRequest, username, productId);
  }

  public GdnRestSingleResponse<ProductDetailResponse> getProductDetailWithOriginalImages(GdnRestListRequest listRequest, String username,
      String productId, boolean originalImages) throws Exception {
    return this.productClientProduct.getProductDetailWithOriginalImages(listRequest, username, productId, originalImages);
  }

  public GdnRestListResponse<ProductItemResponse> getProductItemWithAttributeValues(
      GdnRestListRequest listRequest, String username, String productId) throws Exception {
    return this.productClientProduct
        .getProductItemWithAttributeValues(listRequest, username, productId);
  }

  public GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCode(GdnRestListRequest listRequest,
      String username, String productCode) throws Exception {
    return this.productClientProduct.getProductDetailByProductCode(listRequest, username, productCode);
  }

  public GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCodeWithOriginalImages(
      GdnRestListRequest listRequest, String username, String productCode, boolean originalImages) throws Exception {
    return this.productClientProduct
        .getProductDetailByProductCodeWithOriginalImages(listRequest, username, productCode, originalImages);
  }

  public GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCodeInAllProducts(
      GdnRestListRequest listRequest, String username, String productCode, boolean inAllProducts) throws Exception {
    return this.productClientProduct
        .getProductDetailByProductCodeInAllProducts(listRequest, username, productCode, inAllProducts);
  }

  public GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCodeInAllProductsWithOriginalImages(
      GdnRestListRequest listRequest, String username, String productCode, boolean inAllProducts,
      boolean originalImages) throws Exception {
    return this.productClientProduct
        .getProductDetailByProductCodeInAllProductsWithOriginalImages(listRequest, username, productCode,
            inAllProducts, originalImages);
  }

  public GdnRestSingleResponse<ProductResponse> getProductBasicDetailByProductCode(GdnRestListRequest listRequest,
      String username, String productCode) throws Exception {
    return this.productClientProduct.getProductBasicDetailByProductCode(listRequest, username, productCode);
  }

  public GdnBaseRestResponse updateRejectedProduct(String requestId, String username, ProductRequest request) throws Exception {
    return  this.productClientProduct.updateRejectedProduct(requestId, username, request);
  }

  public GdnRestListResponse<ProductItemResponse> getProductItemByMultipleUpcCode(GdnRestListRequest listRequest,
      String username, ProductItemMultipleUpcCodesRequest request) throws Exception {
    return this.productClientProduct.getProductItemByMultipleUpcCode(listRequest, username, request);
  }

  public GdnRestListResponse<ProductItemResponse> getProductItemByProductItemName(GdnRestListRequest listRequest,
      String username, String productItemName) throws Exception {
    return this.productClientProduct.getProductItemByProductItemName(listRequest, username, productItemName);
  }
  
  public GdnRestListResponse<ProductItemResponse> getProductItemByProductItemNameAndCategoryId(GdnRestListRequest listRequest,
      String username, String productItemName, String categoryId) throws Exception {
    return this.productClientProduct.getProductItemByProductItemNameAndCategoryId(listRequest, username, productItemName, categoryId);
  }

  public GdnRestListResponse<ProductItemResponse> getProductItemByUpcCode(GdnRestListRequest listRequest,
      String username, String upcCode) throws Exception {
    return this.productClientProduct.getProductItemByUpcCode(listRequest, username, upcCode);
  }

  public GdnRestListResponse<ProductItemResponse> getProductItemByViewableAndProductItemNameOrUpcCode(
      GdnRestListRequest listRequest, String username, String itemNameOrUpcCode, boolean viewable, boolean isOnlyExternal) throws Exception {
    return this.productClientProduct.getProductItemByViewableAndProductItemNameOrUpcCode(listRequest, username,
        itemNameOrUpcCode, viewable, isOnlyExternal);
  }

  public GdnRestSingleResponse<ProductItemDetailResponse> getProductItemDetailBySkuCode(GdnRestListRequest listRequest,
      String username, String skuCode) throws Exception {
    return this.productClientProduct.getProductItemDetailBySkuCode(listRequest, username, skuCode);
  }

  public GdnRestListResponse<ProductCodeResponse> getProductItemLikeNameOrUpcCode
      (GdnRestListRequest listRequest,
      String username, String productName, String upcCode, String finalCategoryid,
          List<AttributeReqModel> modelList) throws Exception {
    return this.productClientProduct.getProductItemLikeNameOrUpcCode(listRequest, username,
        productName, upcCode, finalCategoryid, modelList);

  }

  public GdnRestListResponse<ConfigurationStatusResponse> getConfigurationStatus(String requestId, String username,
      List<ConfigurationStatusRequest> modelList) throws Exception {
    return this.productClientProduct.getConfigurationStatus(requestId, username, modelList);
  }

  public GdnRestListResponse<ProductOptionalParameterResponse> getProductOptionalParameterByProductCodeAndPageable(
      GdnRestListRequest listRequest, String username, String productCode) throws Exception {
    return this.productClientProductOptionalParameter.getProductOptionalParameterByProductCodeAndPageable(listRequest,
        username, productCode);
  }

  public GdnRestSingleResponse<ProductOptionalParameterResponse> getProductOptionalParameterDetail(String requestId,
      String username, String id) throws Exception {
    return this.productClientProductOptionalParameter.getProductOptionalParameterDetail(requestId, username, id);
  }

  public GdnRestListResponse<ProductOptionalParameterResponse> getProductOptionalParameterSummary(
      GdnRestListRequest listRequest, String username) throws Exception {
    return this.productClientProductOptionalParameter.getProductOptionalParameterSummary(listRequest, username);
  }

  public GdnRestListResponse<ProductResponse> getProductSummary(GdnRestListRequest listRequest, String username)
      throws Exception {
    return this.productClientProduct.getProductSummary(listRequest, username);
  }

  public GdnBaseRestResponse saveAttribute(String requestId, String username, AttributeRequest request)
      throws Exception {
    return this.productClientAttribute.saveAttribute(requestId, username, request);
  }

  public GdnBaseRestResponse saveCategory(String requestId, String username, CategoryRequest categoryRequest)
      throws Exception {
    return this.productClientCategory.saveCategory(requestId, username, categoryRequest);
  }

  public GdnBaseRestResponse savePredefinedAttribute(String requestId, String username,
      PredefinedAllowedAttributeValueRequest request, String attributeId) throws Exception {
    return this.productClientPredefinedAllowedAttributeValue.saveAttribute(requestId, username, attributeId, request);
  }

  public GdnBaseRestResponse saveProductItem(String requestId, String username, ProductRequest request)
      throws Exception {
    return this.productClientProduct.saveProductItem(requestId, username, request);
  }

  public GdnBaseRestResponse saveProductOptionalParameter(String requestId, String username,
      ProductOptionalParameterRequest request) throws Exception {
    return this.productClientProductOptionalParameter.saveProductOptionalParameter(requestId, username, request);
  }

  public GdnBaseRestResponse updateAttribute(String requestId, String username, AttributeRequest request)
      throws Exception {
    return this.productClientAttribute.updateAttribute(requestId, username, request);
  }

  public GdnBaseRestResponse updateCatalog(String requestId, String username, CatalogRequest catalogRequest)
      throws Exception {
    return this.productClientCatalog.updateCatalog(requestId, username, catalogRequest);
  }

  public GdnBaseRestResponse updateCategory(String requestId, String username, CategoryRequest categoryRequest)
      throws Exception {
    return this.productClientCategory.updateCategory(requestId, username, categoryRequest);
  }

  public GdnBaseRestResponse updateProductItem(String requestId, String username, boolean isMergeRequest, ProductRequest request)
      throws Exception {
    return this.productClientProduct.updateProductItem(requestId, username, isMergeRequest, request);
  }

  public GdnRestSingleResponse<SimpleMasterProductUpdateResponse> updateSimpleMasterData(String requestId, String username,
    SimpleMasterProductUpdateRequest simpleMasterProductUpdateRequest) throws Exception{
    return this.productClientProduct.updateSimpleMasterData(requestId, username, simpleMasterProductUpdateRequest);
  }

  public GdnBaseRestResponse updateProductForMerge(String requestId, String username, ProductRequest request)
      throws Exception {
    return this.productClientProduct.updateProductForMerge(requestId, username, request);
  }

  public GdnBaseRestResponse updateProductOptionalParameter(String requestId, String username,
      ProductOptionalParameterRequest request) throws Exception {
    return this.productClientProductOptionalParameter.updateProductOptionalParameter(requestId, username, request);
  }

  public GdnBaseRestResponse updateProductWithSpecificationDetailGeneratedBySystem(String requestId, String username,
      ProductRequest request) throws Exception {
    return this.productClientProduct
        .updateProductWithSpecificationDetailGeneratedBySystem(requestId, username, request);
  }
  
  public GdnBaseRestResponse updateProductViewable(String requestId, String username, String productCode,
      boolean viewable) throws Exception {
    return this.productClientProduct.updateProductViewable(requestId, username, productCode, viewable);
  }

  public GdnRestSingleResponse<ActivateImageResponse> updateProductImageName(String requestId, String username,
      ActivateImageRequest request) throws Exception{
    return this.productClientProduct.updateImageName(requestId, username, request);
  }

  public GdnRestSingleResponse<ActivateImageResponse> updateProductImagesName(String requestId, String username,
      ProductActivateImageRequest request) throws Exception{
    return this.productClientProduct.updateImagesName(requestId, username, request);
  }
  
  public GdnBaseRestResponse updateProductActivated(String requestId, String username, String productCode,
      boolean activated) throws Exception {
    return this.productClientProduct.updateProductActivated(requestId, username, productCode, activated);
  }
  

  /**
   * Client to call API to get product from product code
   *
   * @param listRequest
   * @param username
   * @param productCode
   * @return
   * @throws Exception
   */
  public GdnRestListResponse<ProductResponse> getProductByProductCodeExactMatch
      (GdnRestListRequest listRequest, String username, String productCode) throws Exception {
    return this.productClientProduct.getProductByProductCodeExactMatch(listRequest, username,
        productCode);
  }

  public GdnRestSingleResponse<SingleObjectResponse> getProductCountByViewable(String requestId,
      String username, boolean viewable) throws Exception {
    return this.productClientProduct.getProductCountByViewable(requestId, username, viewable);
  }

  /**
   * Client call to retireve all categories associated with each catalog of
   * given catalog type
   *
   * @param requestId
   * @param username
   * @param catalogType
   * @return
   * @throws Exception
   */
  public GdnRestSingleResponse<CatalogDetailResponse> getCategoriesByCatalogType(String requestId,
      String username, CatalogType catalogType) throws Exception {
    return this.productClientCatalog.getCategoriesFromCatalogType(requestId, username, catalogType);
  }

  public GdnRestListResponse<ProductItemResponse> getProductItemByUpcCodeExactMatch
      (GdnRestListRequest listRequest, String username, String upcCode) throws Exception {
    return this.productClientProduct.getProductItemByUpcCodeExactMatch(listRequest, username,
        upcCode);
  }
  
  public GdnRestListResponse<SingleObjectResponse> getActiveProductIdsFromCategory(
      String requestId, String category, Date updatedAfter, int page, int size) throws Exception {
    return this.productClientProduct.getActiveProductIdsFromCategory(requestId, category,
        updatedAfter, page, size);
  }
  
  public GdnBaseRestResponse updateProductContent(String requestId, String username, ProductRequest request)
      throws Exception {
    return this.productClientProduct.updateProductContent(requestId, username, request);
  }
  
  public GdnBaseRestResponse updateProductImage(String requestId, String username, ProductRequest request)
      throws Exception {
    return this.productClientProduct.updateProductImage(requestId, username, request);
  }

  public GdnRestListResponse<ProductDetailResponse> getProductDetailsByProductCodes(String requestId, String username,
      List<String> productCodeList) throws Exception {
    return this.productClientProduct
        .getProductDetailsByProductCodes(requestId, username, productCodeList);
  }

  public GdnRestListResponse<ProductDetailResponse> getProductDetailsByProductCodesWithOriginalImages(String requestId, String username,
      List<String> productCodeList, boolean originalImages) throws Exception {
    return this.productClientProduct
        .getProductDetailsByProductCodesWithOriginalImages(requestId, username, productCodeList, originalImages);
  }

  public GdnRestListResponse<MasterProductResponse> getProductDetailsByProductCodesForBulkDownload(String requestId,
      String username, List<String> productCodeList) throws Exception {
    return this.productClientProduct
        .getProductDetailsByProductCodesForBulkDownload(requestId, username, productCodeList);
  }

  
  public GdnRestListResponse<CategoryTreeResponse> getAllCategoryTree(String requestId, 
      String username, String catalogName) throws Exception{
    return this.productClientCategory.getAllCategoryTree(requestId, username, catalogName);
  }
  
  public GdnRestListResponse<CategoryTreeResponse> getCategoryTree(String requestId, 
      String username, String catalogName, List<String> categoryCodes) throws Exception{
    return this.productClientCategory.getCategoryTree(requestId, username, catalogName, categoryCodes);
  }
  
  public GdnRestSingleResponse<CategoryAttributeSummaryResponse> getAttributeDetailByCategoryCode(
      String requestId, String username, String categoryCode) throws Exception{
    return this.productClientAttribute.getAttributeDetailByCategoryCode(requestId, username, categoryCode);
  }
  
  public GdnRestSingleResponse<ActivateImageResponse> isProductImagesActivated(String requestId,
      String username, String productCode) throws Exception {
    return this.productClientProduct.isProductImagesActivated(requestId, username, productCode);
  }
  
  public GdnBaseRestResponse activateAndUpdateImageName(String requestId, String username,
      ActivateImageRequest request) throws Exception {
    return this.productClientProduct.activateAndUpdateImageName(requestId, username,request);
  }
  
  public GdnRestSingleResponse<SingleObjectResponse> getProductCountByBrandName(String requestId,
      String username, String brandName) throws Exception {
    return this.productClientProduct.getProductCountByBrandName(requestId, username, brandName);
  }

  public GdnRestListResponse<CategoryHierarchyResponse> filterCategoryHierarchyByCategoryCodes(
      String requestId, String username, CategoryCodeRequest request) throws Exception {
    return this.productClientCategory
        .filterCategoryHierarchyByCategoryCodes(requestId, username, request);
  }

  public GdnRestListResponse<ProductItemDetailResponse> getProductItemByListOfProductCode(
      GdnRestListRequest listRequest, String username, ProductCodesRequest request, Boolean isOnlyExternal)
      throws Exception {
    return this.productClientProduct
        .getProductItemByListOfProductCode(listRequest, username, request, isOnlyExternal);
  }
  
  public GdnBaseRestResponse clearProductCache(String requestId, String username, String productId,
      String productCode) throws Exception {
    return this.productClientProduct.clearProductCache(requestId, username, productId, productCode);
  }

  public GdnBaseRestResponse clearProductCacheSync(String requestId, String username, String productId,
      String productCode) throws Exception {
    return this.productClientProduct.clearProductCacheSync(requestId, username, productId, productCode);
  }
  
  public GdnRestSingleResponse<GeneratedProductImagesPathResponse> replaceProductImages(
      String requestId, String username, ReplaceProductImagesRequest productImageRequest) throws Exception {
    return this.productClientProduct.replaceProductImages(requestId, username, productImageRequest);
  }
  
  public GdnRestSingleResponse<CategorySummaryResponse> movePrdCategoryByPrdCode(
      String requestId, String username, String productCode, String categoryCode) throws Exception {
    return this.productClientProduct.movePrdCategoryByPrdCode(requestId, username, productCode, categoryCode);
  }
  
  public GdnRestListResponse<ProductAttributeResponse> addProductAttributesByProductCode(
      String requestId, String username, AddProductAttributesRequest request) throws Exception {
    return this.productClientProduct.addProductAttributesByProductCode(requestId, username, request);
  }
  
  public GdnRestSingleResponse<SingleObjectResponse> validateProductPromoSku(String requestId,
      String username, String productId, boolean isPromoSku) throws Exception {
    return this.productClientProduct.validateProductPromoSku(requestId, username, productId,
        isPromoSku);
  }

  public GdnRestSingleResponse<MapResponse<String, String>> createProduct(String requestId, String username, ProductRequest request) throws Exception{
    return  productClientProduct.createProduct(requestId, username,  request);
  }

  public GdnBaseRestResponse updateProductAndItemImages(String requestId,
      String username, ProductAndItemImageRequest request) throws Exception {
    return  productClientProduct.updateProductAndItemImages(requestId, username,  request);
  }

  public GdnRestSingleResponse<AttributeValueResponse> addAttributeValue(String requestId, String username,
      String attributeCode, MasterAttributeAddRequest request) throws Exception {
    return this.productClientAttribute.addAttributeValue(requestId, username, attributeCode, request);
  }
}
