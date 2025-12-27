package com.gdn.x.mta.distributiontask.client;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.web.client.GdnBaseRestCrudClient;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.client.util.PDTClientUtil;
import com.gdn.x.mta.distributiontask.client.util.ProductDistributionTaskClientPath;
import com.gdn.x.mta.distributiontask.request.DistributionTaskFilterRequest;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.response.ProductBusinessPartnerMapperResponse;
import com.gdn.x.mta.distributiontask.response.ProductCodeResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.BoostedProductFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductCodeListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.PDTProductDomainEventModelResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.ProductListResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorProductStatusResponse;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;

/**
 * Created by virajjasani on 18/09/16.
 */
public class ProductDistributionTaskClient extends GdnBaseRestCrudClient {

  public final static String APPLICATION_JSON_VALUE = "application/json";

  public ProductDistributionTaskClient(GdnRestClientConfiguration clientConfig) {
    super(clientConfig);
  }

  public ProductDistributionTaskClient(GdnRestClientConfiguration clientConfig,
      String contextPath) {
    super(clientConfig);
    this.setContextPath(contextPath);
  }

  public ProductDistributionTaskClient(String username, String password, String host, Integer port,
      String clientId, String channelId, String storeId, String contextPath) {
    super(username, password, host, port, clientId, channelId, storeId, contextPath);
  }

  private String generateRequestId(String requestId) {
    if (StringUtils.isEmpty(requestId)) {
      requestId = UUID.randomUUID().toString();
    }
    return requestId;
  }

  @SuppressWarnings("deprecation")
  private URI generateURI(String path, String requestId, String username,
      Map<String, String> additionalParameterMap) throws Exception {
    return getHttpClientHelper().getURI(getClientConfig().getHost(), getClientConfig().getPort(),
        getContextPath() + path, getMandatoryParameter(generateRequestId(requestId), username),
        additionalParameterMap);
  }

  public GdnRestListResponse<DistributionProductResponse> getDistributionSummaryByFilter(String requestId, String
      username, Integer page, Integer size, DistributionTaskFilterRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductDistributionTaskClientPath.PAGE, String.valueOf(page));
    additionalParameterMap.put(ProductDistributionTaskClientPath.SIZE, String.valueOf(size));
    URI uri = generateURI(
        ProductDistributionTaskClientPath.BASE_PATH
            + ProductDistributionTaskClientPath.GET_PRODUCT_DISTRIBUTION_SUMMARY_BY_FILTER,
        requestId, username, additionalParameterMap);
    return this.invokePostType(uri, request, DistributionTaskFilterRequest.class,
        APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<DistributionProductResponse>>() {
        }, null);
  }

  public GdnRestListResponse<DistributionProductResponse> getDistributionSummaryByMultipleFilter(
        String requestId, String username, Integer page, Integer size,
        DistributionTaskMultipleFilterRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductDistributionTaskClientPath.PAGE, String.valueOf(page));
    additionalParameterMap.put(ProductDistributionTaskClientPath.SIZE, String.valueOf(size));
    URI uri = generateURI(ProductDistributionTaskClientPath.BASE_PATH + ProductDistributionTaskClientPath
        .GET_PRODUCT_DISTRIBUTION_SUMMARY_BY_MULTIPLE_FILTER, requestId, username, additionalParameterMap);
    return this.invokePostType(uri, request, DistributionTaskMultipleFilterRequest.class, APPLICATION_JSON_VALUE, new
        TypeReference<GdnRestListResponse<DistributionProductResponse>>() {
    }, null);
  }

  public GdnRestSingleResponse<DistributionProductDetailResponse> getProductDetails(
      String requestId, String username, String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("productCode", String.valueOf(productCode));
    URI uri = generateURI(
        ProductDistributionTaskClientPath.BASE_PATH + ProductDistributionTaskClientPath.ROOT
            + ProductDistributionTaskClientPath.RETRIEVE_PRODUCT_DETAIL,
        requestId, username, additionalParameterMap);
    return invokeGetSingle(uri, DistributionProductDetailResponse.class,
        ProductDistributionTaskClientPath.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<DistributionProductDetailResponse> getDetailsForProduct(
      String requestId, String username, String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("productCode", String.valueOf(productCode));
    URI uri = generateURI(
        ProductDistributionTaskClientPath.BASE_PATH + ProductDistributionTaskClientPath.ROOT
            + ProductDistributionTaskClientPath.GET_PRODUCT_DETAIL_ALL,
        requestId, username, additionalParameterMap);
    return invokeGetSingle(uri, DistributionProductDetailResponse.class,
        ProductDistributionTaskClientPath.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductBusinessPartnerMapperResponse> getBusinessPartnerForVendor(
      String requestId, String username, String vendorCode, int page, int size) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductDistributionTaskClientPath.PAGE, String.valueOf(page));
    additionalParameterMap.put(ProductDistributionTaskClientPath.SIZE, String.valueOf(size));
    additionalParameterMap.put("vendorCode", vendorCode);
    URI uri = generateURI(ProductDistributionTaskClientPath.BASE_PATH
            + ProductDistributionTaskClientPath.GET_BUSINESS_PARTNER_LIST_FOR_VENDOR, requestId,
        username, additionalParameterMap);

    return invokeGetSummary(uri, ProductBusinessPartnerMapperResponse.class,
        APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<DistributionProductResponse> getProductListingForVendorCode(
      String requestId, String username, Integer page, Integer size, ProductListRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductDistributionTaskClientPath.PAGE, String.valueOf(page));
    additionalParameterMap.put(ProductDistributionTaskClientPath.SIZE, String.valueOf(size));
    URI uri = generateURI(
        ProductDistributionTaskClientPath.BASE_PATH
            + ProductDistributionTaskClientPath.GET_PRODUCT_SUMMARY_FOR_VENDOR,
        requestId, username, additionalParameterMap);
    return this.invokePostType(uri, request, ProductListRequest.class, APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<DistributionProductResponse>>() {}, null);
  }

  public GdnRestSingleResponse<MapResponse> countVendorProductSummaryByFilter(String requestId,
      String username, ProductListRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = generateURI(ProductDistributionTaskClientPath.BASE_PATH
            + ProductDistributionTaskClientPath.GET_PRODUCT_SUMMARY_COUNT_VENDOR, requestId,
        username, additionalParameterMap);
    return this.invokePostType(uri, request, ProductListRequest.class, APPLICATION_JSON_VALUE, new
        TypeReference<GdnRestSingleResponse<MapResponse>>() {}, null);
  }

  public GdnRestSingleResponse<VendorProductStatusResponse> countProductStatusForVendor(
      String requestId, String username, String vendorCode)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("vendorCode", String.valueOf(vendorCode));
    URI uri = generateURI(
        ProductDistributionTaskClientPath.BASE_PATH
            + ProductDistributionTaskClientPath.COUNT_PRODUCT_STATUS_FOR_VENDOR,
        requestId, username, additionalParameterMap);
    return invokeGetSingle(uri, VendorProductStatusResponse.class,
        ProductDistributionTaskClientPath.APPLICATION_JSON_VALUE, (Map<String, String>) null);
  }

  public GdnRestSingleResponse<MapResponse> countDistributionSummaryByFilter(Boolean includeStatus,
      Boolean includeVendors, String requestId, String username,
      DistributionTaskFilterRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("includeVendors", String.valueOf(includeVendors));
    additionalParameterMap.put("includeStatus", String.valueOf(includeStatus));
    URI uri = generateURI(
        ProductDistributionTaskClientPath.BASE_PATH
            + ProductDistributionTaskClientPath.COUNT_PRODUCT_DISTRIBUTION_SUMMARY_BY_FILTER,
        generateRequestId(requestId), username, additionalParameterMap);
    return this.invokePostType(uri, request, DistributionTaskFilterRequest.class,
        APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestSingleResponse<MapResponse>>() {
        }, null);
  }

  public GdnRestSingleResponse<MapResponse> countDistributionSummaryByMultipleFilter(Boolean includeStatus, Boolean
      includeVendors, String requestId, String username, DistributionTaskMultipleFilterRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("includeVendors", String.valueOf(includeVendors));
    additionalParameterMap.put("includeStatus", String.valueOf(includeStatus));
    URI uri = generateURI(ProductDistributionTaskClientPath.BASE_PATH + ProductDistributionTaskClientPath
        .COUNT_PRODUCT_DISTRIBUTION_SUMMARY_BY_FILTER, generateRequestId(requestId), username, additionalParameterMap);
    return this.invokePostType(uri, request, DistributionTaskMultipleFilterRequest.class, APPLICATION_JSON_VALUE, new
        TypeReference<GdnRestSingleResponse<MapResponse>>() {
    }, null);
  }

  public GdnRestListResponse<ProductBusinessPartnerMapperResponse> filterProductBusinessPartnerMapperByWorkFlowState(
      GdnRestListRequest request, String username, String workflowState, boolean isSearch,
      String searchCriteria) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(request.getPage()));
    additionalParameterMap.put("size", String.valueOf(request.getSize()));
    additionalParameterMap.put("isSearch", String.valueOf(isSearch));
    additionalParameterMap.put("searchCriteria", searchCriteria);
    additionalParameterMap.put("workflowState", workflowState);
    final StringBuilder sb = new StringBuilder(ProductDistributionTaskClientPath.BASE_PATH);
    sb.append(ProductDistributionTaskClientPath.ROOT)
        .append(ProductDistributionTaskClientPath.FILTER_BUSINESS_PARTNER);
    URI uri = generateURI(sb.toString(), request.getRequestId(), username, additionalParameterMap);
    return invokeGetSummary(uri, ProductBusinessPartnerMapperResponse.class,
        APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<PDTProductDomainEventModelResponse> getPDTDomainModelResponseByCode(
      String requestId, String username, String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("productCode", productCode);
    URI uri = generateURI(ProductDistributionTaskClientPath.BASE_PATH
            + ProductDistributionTaskClientPath.GET_PRODUCT_DOMAIN_MODEL_RESPONSE,
        generateRequestId(requestId), username, additionalParameterMap);
    return invokeGetSingle(uri, PDTProductDomainEventModelResponse.class,
        ProductDistributionTaskClientPath.APPLICATION_JSON_VALUE, null);
  }

  public GdnRestSimpleResponse<Boolean> isProductExistsInPDT(String requestId, String username,
      String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("productCode", productCode);
    URI uri = generateURI(ProductDistributionTaskClientPath.BASE_PATH
            + ProductDistributionTaskClientPath.IS_PRODUCT_EXIST, generateRequestId(requestId),
        username, additionalParameterMap);
    return PDTClientUtil.invokeGetType(uri, new TypeReference<GdnRestSimpleResponse<Boolean>>() {
    }, getClientConfig());
  }

  public GdnRestSingleResponse<ProductListResponse> getProductCodeList(String requestId, String username,
      ProductCodeListRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = generateURI(ProductDistributionTaskClientPath.BASE_PATH
            + ProductDistributionTaskClientPath.GET_PRODUCT_CODES, generateRequestId(requestId),
        username, additionalParameterMap);
    return this.invokePostType(uri, request, ProductCodeListRequest.class,
        APPLICATION_JSON_VALUE, new TypeReference<GdnRestSingleResponse<ProductListResponse>>() {
        }, null);
  }

  public GdnBaseRestResponse rejectProduct(String requestId, String username, RejectProductListRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<>();
    URI uri = this.generateURI(
        ProductDistributionTaskClientPath.BASE_PATH + ProductDistributionTaskClientPath.REJECT_PRODUCT,
        this.generateRequestId(requestId), username, additionalParameterMap);
    return this
        .invokePost(uri, RejectProductListRequest.class, request, ProductDistributionTaskClient.APPLICATION_JSON_VALUE,
            null);
  }

  public GdnRestListResponse<DistributionProductResponse> getProductList( String requestId, String username,
      Integer page, Integer size, FilterSummaryRequest filterSummaryRequest) throws Exception{
    Map<String, String> additionalParameterMap = new HashMap<>();
    additionalParameterMap.put(ProductDistributionTaskClientPath.PAGE, String.valueOf(page));
    additionalParameterMap.put(ProductDistributionTaskClientPath.SIZE, String.valueOf(size));
    URI uri =
        this.generateURI(ProductDistributionTaskClientPath.BASE_PATH + ProductDistributionTaskClientPath.FILTER_SUMMARY,
            this.generateRequestId(requestId), username, additionalParameterMap);
    return this.invokePostType(uri, filterSummaryRequest, FilterSummaryRequest.class,
        ProductDistributionTaskClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<DistributionProductResponse>>() {
        }, null);
  }

  public GdnBaseRestResponse sendProductBackToVendor(String requestId, String username, String productCode)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<>();
    additionalParameterMap.put("productCode", productCode);
    URI uri = generateURI(
        ProductDistributionTaskClientPath.BASE_PATH + ProductDistributionTaskClientPath.SEND_PRODUCT_BACK_TO_VENDOR,
        generateRequestId(requestId), username, additionalParameterMap);
    return this.getHttpClientHelper().invokeGetType(uri, new TypeReference<GdnBaseRestResponse>() {},
        APPLICATION_JSON_VALUE, this.getClientConfig().getConnectionTimeoutInMs());
  }

  public GdnRestListResponse<ProductCodeResponse> fetchProductsForAutoAssignment(
    String requestId, String username, Integer page, Integer size, BoostedProductFilterRequest request)
    throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<>();
    additionalParameterMap.put(ProductDistributionTaskClientPath.PAGE, String.valueOf(page));
    additionalParameterMap.put(ProductDistributionTaskClientPath.SIZE, String.valueOf(size));
    URI uri = generateURI(
      ProductDistributionTaskClientPath.BASE_PATH
        + ProductDistributionTaskClientPath.FETCH_PRODUCTS_FOR_AUTO_ASSIGNMENT,
      requestId, username, additionalParameterMap);
    return this.invokePostType(uri, request, BoostedProductFilterRequest.class, APPLICATION_JSON_VALUE,
      new TypeReference<GdnRestListResponse<ProductCodeResponse>>() {}, null);
  }

}
