package com.gdn.x.productcategorybase.service.client;

import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.common.web.client.GdnBaseRestCrudClient;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.ProductApiPath;
import com.gdn.x.productcategorybase.ProductCategoryApiPath;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.PostLiveConfigurationControllerPath;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.request.AddProductAttributesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemMultipleUpcCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductImagesRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductImagesPathResponse;
import com.gdn.x.productcategorybase.dto.response.MapResponse;
import com.gdn.x.productcategorybase.dto.response.MasterProductResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;

public class ProductClientProduct extends GdnBaseRestCrudClient {

  private static final String DETAIL_BY_PRODUCT_CODE = "/productCode/";
  private static final String DETAIL_BY_SKU_CODE = "/skuCode/";
  private static final String COUNT_PRODUCT_BY_BRAND_NAME = "/count/brandName/";
  private static final String BASIC_DETAIL_BY_PRODUCT_CODE = "/productBasicDetails/";
  

  public ProductClientProduct(GdnRestClientConfiguration clientConfig, String contextPath) {
    super(clientConfig);
    this.setContextPath(contextPath);
  }

  public ProductClientProduct(String username, String password, String host, Integer port, String storeId,
      String clientId, String channelId, String contextPath) {
    super(username, password, host, port, clientId, channelId, storeId, contextPath);
  }

  public GdnBaseRestResponse activateProduct(String requestId, String username, ProductRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.ACTIVATE, requestId, username,
            additionalParameterMap);
    return this.invokePost(uri, ProductRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse checkProductByProductCode(String requestId, String username, String productCode)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("productCode", productCode);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.CHECK_PRODUCT_BY_PRODUCT_CODE, requestId, username,
            additionalParameterMap);
    return this.invokePostType(uri, null, Object.class, ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse checkProductItemBySkuCode(String requestId, String username, String skuCode)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("skuCode", skuCode);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.CHECK_PRODUCT_ITEM_BY_SKU_CODE, requestId, username,
            additionalParameterMap);
    return this.invokePostType(uri, null, Object.class, ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse createNewProduct(String requestId, String username, ProductRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.CREATE, requestId, username, additionalParameterMap);
    return this.invokePost(uri, ProductRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse createNewProductWithSpecificationDetailGeneratedBySystem(String requestId,
      String username, ProductRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(
            ProductApiPath.BASE_PATH + ProductApiPath.CREATE_WITH_SPECIFICATION_DETAIL_GENERATED_BY_SYSTEM, requestId,
            username, additionalParameterMap);
    return this.invokePost(uri, ProductRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse deactivateProduct(String requestId, String username, ProductRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.DEACTIVATE, requestId, username,
            additionalParameterMap);
    return this.invokePost(uri, ProductRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse discardProduct(String requestId, String username, ProductRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.DISCARD, requestId, username, additionalParameterMap);
    return this.invokePost(uri, ProductRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductItemDetailResponse> filterProductItemBySkuCodes(String requestId, String username,
      SkuCodesRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_FILTER_SKU_CODES, requestId, username,
            additionalParameterMap);
    return this.invokePostType(uri, request, SkuCodesRequest.class, ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<ProductItemDetailResponse>>() {});
  }

  private URI generateURI(String path, String requestId, String username, Map<String, String> additionalParameterMap)
      throws Exception {
    String location = this.getContextPath() + path;
    return this.getHttpClientHelper().getURI(this.getClientConfig().getHost(), this.getClientConfig().getPort(),
        location, this.getMandatoryParameter(this.getDefaultRequestIdValue(requestId), username),
        additionalParameterMap);
  }

  private String getDefaultRequestIdValue(String requestId) {
    if ((requestId == null) || (requestId.trim().length() == 0)) {
      return GdnUUIDHelper.generateUUID();
    }
    return requestId;
  }

  public GdnRestListResponse<ProductResponse> getProductByBrand(GdnRestListRequest listRequest, String username,
      String brandName) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("brandName", brandName);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_BRAND, listRequest.getRequestId(), username,
            additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductResponse> getProductByMarkForDelete(GdnRestListRequest listRequest,
      String username, String markForDelete) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("markForDelete", markForDelete);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_MARK_FOR_DELETE, listRequest.getRequestId(),
            username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductResponse> getProductByName(GdnRestListRequest listRequest, String username,
      String name) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("name", name);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_NAME, listRequest.getRequestId(), username,
            additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductResponse> getProductByNameAndCreatedBy(GdnRestListRequest listRequest,
      String username, String name, String createdBy) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("name", name);
    additionalParameterMap.put("createdBy", createdBy);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_NAME_CREATED_BY, listRequest.getRequestId(),
            username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductResponse> getProductByNameAndViewableAndActivated(GdnRestListRequest listRequest,
      String username, String name, boolean viewable, boolean activated) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("name", name);
    additionalParameterMap.put("viewable", String.valueOf(viewable));
    additionalParameterMap.put("activated", String.valueOf(activated));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_NAME_VIEWABLE_ACTIVATED,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductResponse> getProductByNameAndViewableAndActivatedAndUpdatedBy(
      GdnRestListRequest listRequest, String username, String name, boolean viewable, boolean activated,
      String updatedBy) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("name", name);
    additionalParameterMap.put("viewable", String.valueOf(viewable));
    additionalParameterMap.put("activated", String.valueOf(activated));
    additionalParameterMap.put("updatedBy", updatedBy);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_NAME_VIEWABLE_ACTIVATED_UPDATED_BY,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
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
      listRequest, String username,
      String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("productCode", productCode);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_PRODUCT_CODE,
            listRequest.getRequestId(),
            username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductResponse> getProductByShippingWeightBiggerOrEqual(GdnRestListRequest listRequest,
      String username, String shippingWeight) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("shippingWeight", shippingWeight);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_SHIPPING_WEIGHT_BIGGER,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductResponse> getProductByShippingWeightLesserOrEqual(GdnRestListRequest listRequest,
      String username, String shippingWeight) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("shippingWeight", shippingWeight);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_SHIPPING_WEIGHT_LESSER,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductResponse> getProductByUniqueSellingCode(GdnRestListRequest listRequest,
      String username, String uniqueSellingCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("uniqueSellingCode", uniqueSellingCode);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_UNIQUE_SELLING_CODE,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  @Deprecated
  public GdnRestListResponse<ProductResponse> getProductByViewable(GdnRestListRequest listRequest, String username,
      boolean viewable) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("viewable", String.valueOf(viewable));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_VIEWABLE, listRequest.getRequestId(),
            username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductResponse> getProductByViewableAndActivated(GdnRestListRequest listRequest,
      String username, boolean viewable, boolean activated) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("viewable", String.valueOf(viewable));
    additionalParameterMap.put("activated", String.valueOf(activated));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_VIEWABLE_ACTIVATED,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductResponse> getProductByWeightBiggerOrEqual(GdnRestListRequest listRequest,
      String username, String weight) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("weight", weight);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_WEIGHT_BIGGER, listRequest.getRequestId(),
            username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductResponse> getProductByWeightLesserOrEqual(GdnRestListRequest listRequest,
      String username, String weight) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("weight", weight);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_WEIGHT_LESSER, listRequest.getRequestId(),
            username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductItemResponse> getProductItemWithAttributeValues(
      GdnRestListRequest listRequest, String username, String productId) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("productId", productId);
    URI uri = this.generateURI(
        ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_ITEM_ATTR_VALUE_DETAIL,
        listRequest.getRequestId(), username, additionalParameterMap);
    return this
        .invokeGetSummary(uri, ProductItemResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<ProductDetailResponse> getProductDetail(GdnRestListRequest listRequest, String username,
      String productId) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + "/" + productId, listRequest.getRequestId(), username,
            additionalParameterMap);
    return this.invokeGetSingle(uri, ProductDetailResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<ProductDetailResponse> getProductDetailWithOriginalImages(GdnRestListRequest listRequest, String username,
      String productId, boolean originalImages) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("originalImages", String.valueOf(originalImages));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + "/" + productId, listRequest.getRequestId(), username,
            additionalParameterMap);
    return this.invokeGetSingle(uri, ProductDetailResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCode(GdnRestListRequest listRequest,
      String username, String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("inAllProducts", String.valueOf(false));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductClientProduct.DETAIL_BY_PRODUCT_CODE + productCode,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSingle(uri, ProductDetailResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCodeWithOriginalImages(GdnRestListRequest listRequest,
      String username, String productCode, boolean originalImages) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("inAllProducts", String.valueOf(false));
    additionalParameterMap.put("originalImages", String.valueOf(originalImages));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductClientProduct.DETAIL_BY_PRODUCT_CODE + productCode,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSingle(uri, ProductDetailResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<ProductResponse> getProductBasicDetailByProductCode(GdnRestListRequest listRequest,
      String username, String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductClientProduct.BASIC_DETAIL_BY_PRODUCT_CODE + productCode,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSingle(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCodeInAllProducts(
      GdnRestListRequest listRequest, String username, String productCode, boolean inAllProducts) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("inAllProducts", String.valueOf(inAllProducts));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductClientProduct.DETAIL_BY_PRODUCT_CODE + productCode,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSingle(uri, ProductDetailResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCodeInAllProductsWithOriginalImages(
      GdnRestListRequest listRequest, String username, String productCode, boolean inAllProducts,
      boolean originalImages) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("inAllProducts", String.valueOf(inAllProducts));
    additionalParameterMap.put("originalImages", String.valueOf(originalImages));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductClientProduct.DETAIL_BY_PRODUCT_CODE + productCode,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSingle(uri, ProductDetailResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }


  public GdnRestListResponse<ProductItemResponse> getProductItemByMultipleUpcCode(GdnRestListRequest listRequest,
      String username, ProductItemMultipleUpcCodesRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_MULTIPLE_UPC_CODE,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokePostType(uri, request, ProductItemMultipleUpcCodesRequest.class,
        ProductClient.APPLICATION_JSON_VALUE, new TypeReference<GdnRestListResponse<ProductItemResponse>>() {});
  }

  public GdnRestListResponse<ProductItemResponse> getProductItemByProductItemName(GdnRestListRequest listRequest,
      String username, String productItemName) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("productItemName", productItemName);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ITEM_NAME, listRequest.getRequestId(),
            username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductItemResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }
  
  public GdnRestListResponse<ProductItemResponse> getProductItemByProductItemNameAndCategoryId(GdnRestListRequest listRequest,
      String username, String productItemName, String categoryId) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("productItemName", productItemName);
    additionalParameterMap.put("categoryId", categoryId);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ITEM_NAME_CATEGORY_ID, listRequest.getRequestId(),
            username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductItemResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductItemResponse> getProductItemByUpcCode(GdnRestListRequest listRequest,
      String username, String upcCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("upcCode", upcCode);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_UPC_CODE, listRequest.getRequestId(),
            username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductItemResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductItemResponse> getProductItemByViewableAndProductItemNameOrUpcCode(
      GdnRestListRequest listRequest, String username, String itemNameOrUpcCode, boolean viewable,
      boolean isOnlyExternal) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("itemNameOrUpcCode", itemNameOrUpcCode);
    additionalParameterMap.put("viewable", String.valueOf(viewable));
    additionalParameterMap.put("isOnlyExternal", String.valueOf(isOnlyExternal));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ITEM_NAME_OR_UPC_CODE,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductItemResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<ProductItemDetailResponse> getProductItemDetailBySkuCode(GdnRestListRequest listRequest,
      String username, String skuCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductClientProduct.DETAIL_BY_SKU_CODE + skuCode,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSingle(uri, ProductItemDetailResponse.class, ProductClient.APPLICATION_JSON_VALUE);

  }

  public GdnRestListResponse<ProductCodeResponse> getProductItemLikeNameOrUpcCode(
      GdnRestListRequest listRequest, String username, String productName, String upcCode,
      String finalCategoryId, List<AttributeReqModel> modelList)
      throws Exception {
    final Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("productName", productName);
    additionalParameterMap.put("upcCode", upcCode);
    additionalParameterMap.put("finalCategoryId", finalCategoryId);
    final URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ITEM_NAME_UPC_CODE,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokePostType(uri, modelList, ProductCodeResponse.class, ProductClient.APPLICATION_JSON_VALUE, new
            TypeReference<GdnRestListResponse<ProductCodeResponse>>() {
        });
  }

  public GdnRestListResponse<ConfigurationStatusResponse> getConfigurationStatus(String requestId, String username,
      List<ConfigurationStatusRequest> configurationStatusRequestList) throws Exception {
    final Map<String, String> additionalParameterMap = new HashMap<String, String>();
    final URI uri = this.generateURI(
        PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.GET_CONFIGURATION_STATUS,
        requestId, username, additionalParameterMap);
    return this.invokePostType(uri, configurationStatusRequestList, ConfigurationStatusResponse.class,
        ProductClient.APPLICATION_JSON_VALUE, new TypeReference<GdnRestListResponse<ConfigurationStatusResponse>>() {
        });
  }

  public GdnRestListResponse<ProductResponse> getProductSummary(GdnRestListRequest listRequest, String username)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    URI uri = this.generateURI(ProductApiPath.BASE_PATH, listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse saveProductItem(String requestId, String username, ProductRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.SAVE, requestId, username, additionalParameterMap);
    return this.invokePost(uri, ProductRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse updateProductItem(String requestId, String username, boolean isMergeRequest, ProductRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("isMergeRequest", String.valueOf(isMergeRequest));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE, requestId, username,additionalParameterMap);
    return this.invokePost(uri, ProductRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse updateProductWithSpecificationDetailGeneratedBySystem(String requestId, String username,
      ProductRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(
            ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_WITH_SPECIFICATION_DETAIL_GENERATED_BY_SYSTEM, requestId,
            username, additionalParameterMap);
    return this.invokePost(uri, ProductRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse updateProductForMerge(String requestId, String username,
      ProductRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_FOR_MERGE, requestId,
            username, additionalParameterMap);
    return this
        .invokePost(uri, ProductRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }
  
  public GdnBaseRestResponse updateProductViewable(String requestId, String username, String productCode,
      boolean viewable) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("productCode", productCode);
    additionalParameterMap.put("viewable", String.valueOf(viewable));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_VIEWABLE, requestId, username,
            additionalParameterMap);
    return this.getHttpClientHelper().invokeGetType(uri, new TypeReference<GdnBaseRestResponse>() {},
        ProductClient.APPLICATION_JSON_VALUE, this.getClientConfig().getConnectionTimeoutInMs());
  }

  public GdnRestSingleResponse<ActivateImageResponse> updateImageName(String requestId, String username,
      ActivateImageRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGE_NAME,
            requestId, username, additionalParameterMap);
    return this.invokePostType(uri, request, ActivateImageRequest.class,
        ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestSingleResponse<ActivateImageResponse>>() {});
  }

  public GdnRestSingleResponse<ActivateImageResponse> updateImagesName(String requestId,
      String username, ProductActivateImageRequest request) throws Exception {
    URI uri = this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGES_NAME,
        requestId, username, null);
    return this.invokePostType(uri, request, ProductActivateImageRequest.class,
        ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestSingleResponse<ActivateImageResponse>>() {}, null);
  }
  
  public GdnBaseRestResponse updateProductActivated(String requestId, String username, String productCode,
      boolean activated) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("productCode", productCode);
    additionalParameterMap.put("activated", String.valueOf(activated));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_ACTIVATED, requestId, username,
            additionalParameterMap);
    return this.getHttpClientHelper().invokeGetType(uri, new TypeReference<GdnBaseRestResponse>() {},
        ProductClient.APPLICATION_JSON_VALUE, this.getClientConfig().getConnectionTimeoutInMs());
  }
  

  /**
   * CLient to call API to get product from product code
   *
   * @param listRequest
   * @param username
   * @param productCode
   * @return
   * @throws Exception
   */
  public GdnRestListResponse<ProductResponse> getProductByProductCodeExactMatch
      (GdnRestListRequest listRequest, String username,
      String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("productCode", productCode);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_PRODUCT_CODE_EXACT_MATCH,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<SingleObjectResponse> getProductCountByViewable(String requestId,
      String username,
      boolean viewable) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("viewable", String.valueOf(viewable));
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.VIEWABLE_COUNT, requestId,
            username, additionalParameterMap);
    return this.invokeGetSingle(uri, SingleObjectResponse.class, ProductClient
        .APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductItemResponse> getProductItemByUpcCodeExactMatch
      (GdnRestListRequest listRequest, String username,
      String upcCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("upcCode", upcCode);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_UPC_CODE_EXACT_MATCH,
            listRequest.getRequestId(),
            username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductItemResponse.class, ProductClient
        .APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductItemDetailResponse> getProductItemByListOfProductCode(
      GdnRestListRequest listRequest, String username, ProductCodesRequest request, Boolean isOnlyExternal)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("isOnlyExternal", String.valueOf(isOnlyExternal));
    URI uri = this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.ITEM_FILTER_PRODUCT_CODES,
        listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokePostType(uri, request, ProductCodesRequest.class,
        ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<ProductItemDetailResponse>>() {
        });
  }

  public GdnRestListResponse<SingleObjectResponse> getActiveProductIdsFromCategory(
      String requestId, String category, Date updatedAfter, int page, int size) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("categoryId", category);
    additionalParameterMap.put("page", String.valueOf(page));
    additionalParameterMap.put("size", String.valueOf(size));

    if (updatedAfter != null) {
      additionalParameterMap.put("updatedAfter", new SimpleDateFormat("dd-MM-yyyy").format(updatedAfter));
    }
    URI uri = this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_ACTIVE_PRODUCT_CATEGORY,
            requestId, null, additionalParameterMap);
    return this.invokeGetSummary(uri, SingleObjectResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }
  
  public GdnBaseRestResponse updateProductContent(String requestId, String username, ProductRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CONTENT, requestId, username,
            additionalParameterMap);
    return this.invokePost(uri, ProductRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }
  
  public GdnBaseRestResponse updateProductImage(String requestId, String username, ProductRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGE, requestId, username,
            additionalParameterMap);
    return this.invokePost(uri, ProductRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }
  
  public GdnRestSingleResponse<ActivateImageResponse> isProductImagesActivated(String requestId,
      String username, String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("productCode", productCode);
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.IS_PRODUCT_IMAGES_ACTIVATED, requestId,
            username, additionalParameterMap);
    return this.invokeGetSingle(uri, ActivateImageResponse.class,
        ProductClient.APPLICATION_JSON_VALUE);
  }
  
  public GdnBaseRestResponse activateAndUpdateImageName(String requestId, String username,
      ActivateImageRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_IMAGE_TO_ACTIVE,
            requestId, username, additionalParameterMap);
    return  this.invokePost(uri, ActivateImageRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductDetailResponse> getProductDetailsByProductCodes(String requestId, String username,
      List<String> productCodeList) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        generateURI(ProductApiPath.BASE_PATH + ProductApiPath.DETAILS_BY_PRODUCT_CODES, requestId,
            username, additionalParameterMap);
    return invokePostType(uri, productCodeList, ProductDetailResponse.class,
        ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<ProductDetailResponse>>() {
        });
  }

  public GdnRestListResponse<ProductDetailResponse> getProductDetailsByProductCodesWithOriginalImages(String requestId, String username,
      List<String> productCodeList, boolean originalImages) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("originalImages", String.valueOf(originalImages));
    URI uri =
        generateURI(ProductApiPath.BASE_PATH + ProductApiPath.DETAILS_BY_PRODUCT_CODES, requestId,
            username, additionalParameterMap);
    return invokePostType(uri, productCodeList, ProductDetailResponse.class,
        ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<ProductDetailResponse>>() {
        });
  }

  public GdnRestListResponse<MasterProductResponse> getProductDetailsByProductCodesForBulkDownload(String requestId,
      String username, List<String> productCodeList) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        generateURI(ProductApiPath.BASE_PATH + ProductApiPath.DETAILS_BY_PRODUCT_CODES_FOR_BULK_DOWNLOAD, requestId,
            username, additionalParameterMap);
    return invokePostType(uri, productCodeList, MasterProductResponse.class, ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<MasterProductResponse>>() {
        }, Collections.<String, String>emptyMap());
  }
  
  public GdnRestSingleResponse<SingleObjectResponse> getProductCountByBrandName(String requestId,
      String username, String brandName) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.BASE_PATH
            + ProductClientProduct.COUNT_PRODUCT_BY_BRAND_NAME + brandName, requestId, username,
            additionalParameterMap);
    return this.invokeGetSingle(uri, SingleObjectResponse.class,
        ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse updateRejectedProduct(String requestId, String username,
      ProductRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_REJECTED_PRODUCT,
        requestId, username, additionalParameterMap);
    return this
        .invokePost(uri, ProductRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }
  
  public GdnBaseRestResponse clearProductCache(String requestId, String username, String productId,
      String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("productId", productId);
    additionalParameterMap.put("productCode", productCode);
    URI uri = this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.CLEAR_PRODUCT_CACHE,
        requestId, username, additionalParameterMap);
    return this.invokePost(uri,  ProductRequest.class, null, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse clearProductCacheSync(String requestId, String username, String productId,
      String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("productId", productId);
    additionalParameterMap.put("productCode", productCode);
    URI uri = this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.CLEAR_PRODUCT_CACHE_SYNC,
        requestId, username, additionalParameterMap);
    return this.invokePost(uri,  ProductRequest.class, null, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<GeneratedProductImagesPathResponse> replaceProductImages(
      String requestId, String username, ReplaceProductImagesRequest productImageRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.REPLACE_PRODUCT_IMAGES,
        requestId, username, additionalParameterMap);
    return this.invokePostType(uri, productImageRequest, ReplaceProductImagesRequest.class, ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestSingleResponse<GeneratedProductImagesPathResponse>>() {});
  }
  
  public GdnRestSingleResponse<CategorySummaryResponse> movePrdCategoryByPrdCode(
      String requestId, String username, String productCode, String categoryCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("productCode", productCode);
    additionalParameterMap.put("categoryCode", categoryCode);
    URI uri = this.generateURI(ProductCategoryApiPath.MOVE_CATEGORY_BY_PRD_CODE,
        requestId, username, additionalParameterMap);
    return this.invokeGetSingle(uri, CategorySummaryResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }
  
  public GdnRestListResponse<ProductAttributeResponse> addProductAttributesByProductCode(
      String requestId, String username, AddProductAttributesRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductCategoryApiPath.ADD_PRD_ATTRIBUTES_BY_PRD_CODE,
        requestId, username, additionalParameterMap);
    return this.invokePostType(uri, request, AddProductAttributesRequest.class, ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<ProductAttributeResponse>>() {});
  }

  public GdnRestSingleResponse<SingleObjectResponse> validateProductPromoSku(String requestId,
      String username, String productId, boolean isPromoSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("productId", productId);
    additionalParameterMap.put("isPromoSku", String.valueOf(isPromoSku));
    URI uri = this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.VALIDATE_PROMO_SKU,
        requestId, username, additionalParameterMap);
    return this.invokeGetSingle(uri, SingleObjectResponse.class,
        ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<MapResponse<String, String>> createProduct(String requestId, String username,
      ProductRequest request) throws Exception {
    URI uri = this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.CREATE_PRODUCT, requestId, username,
        null);
    return this.invokePostType(uri, request, ProductRequest.class, ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestSingleResponse<MapResponse<String, String>>>() {
        }, null);
  }

  public GdnBaseRestResponse updateProductAndItemImages(String requestId, String username,
      ProductAndItemImageRequest request) throws Exception {
    URI uri = this.generateURI(
        ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_AND_ITEM_IMAGE_DETAILS_BY_PRODUCT_CODE, requestId,
        username, null);
    return this.invokePostType(uri, request, ProductAndItemImageRequest.class, ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnBaseRestResponse>() {
        }, null);
  }

  public GdnRestSingleResponse<SimpleMasterProductUpdateResponse> updateSimpleMasterData(
    String requestId, String username, SimpleMasterProductUpdateRequest request) throws Exception{
    URI uri = this.generateURI(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_SIMPLE_MASTER_DATA,
        requestId, username,null);
    return this.invokePostType(uri, request, SimpleMasterProductUpdateRequest.class, ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestSingleResponse<SimpleMasterProductUpdateResponse>>() {
        }, null);
  }
}
