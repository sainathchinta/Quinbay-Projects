package com.gdn.x.productcategorybase.service.client;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.common.web.client.GdnBaseRestCrudClient;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.dto.request.ProductOptionalParameterRequest;
import com.gdn.x.productcategorybase.dto.response.ProductOptionalParameterResponse;

public class ProductClientProductOptionalParameter extends GdnBaseRestCrudClient {
  public static final String BASE_PATH = "/api/productOptionalParameter";
  public static final String SAVE = ProductClientProductOptionalParameter.BASE_PATH + "/save";
  public static final String UPDATE = ProductClientProductOptionalParameter.BASE_PATH + "/update";
  public static final String DELETE = ProductClientProductOptionalParameter.BASE_PATH + "/delete";
  public static final String DETAIL = ProductClientProductOptionalParameter.BASE_PATH + "/{id}";
  public static final String FILTER_NAME = ProductClientProductOptionalParameter.BASE_PATH + "/filter/name";
  public static final String FILTER_PRODUCT_CODE =
      ProductClientProductOptionalParameter.BASE_PATH + "/filter/productCode";

  public ProductClientProductOptionalParameter(GdnRestClientConfiguration clientConfig) {
    super(clientConfig);
  }

  public ProductClientProductOptionalParameter(GdnRestClientConfiguration clientConfig, String contextPath) {
    super(clientConfig);
    this.setContextPath(contextPath);
  }

  public ProductClientProductOptionalParameter(String username, String password, String host, Integer port,
      String storeId, String clientId, String channelId, String contextPath) {
    super(username, password, host, port, clientId, channelId, storeId, contextPath);
  }

  public GdnBaseRestResponse deleteProductOptionalParameter(String requestId, String username,
      SimpleRequestHolder holder) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientProductOptionalParameter.DELETE, requestId, username, additionalParameterMap);
    return this.invokePost(uri, ProductOptionalParameterRequest.class, holder, ProductClient.APPLICATION_JSON_VALUE);
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

  public GdnRestListResponse<ProductOptionalParameterResponse> getProductOptionalParameterByNameAndPageable(
      GdnRestListRequest listRequest, String username, String name) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("name", name);
    URI uri = this.generateURI(ProductClientProductOptionalParameter.FILTER_NAME, listRequest.getRequestId(), username,
        additionalParameterMap);
    return this.invokeGetSummary(uri, ProductOptionalParameterResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductOptionalParameterResponse> getProductOptionalParameterByProductCodeAndPageable(
      GdnRestListRequest listRequest, String username, String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("productCode", productCode);
    URI uri = this.generateURI(ProductClientProductOptionalParameter.FILTER_PRODUCT_CODE, listRequest.getRequestId(),
        username, additionalParameterMap);
    return this.invokeGetSummary(uri, ProductOptionalParameterResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<ProductOptionalParameterResponse> getProductOptionalParameterDetail(String requestId,
      String username, String id) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductClientProductOptionalParameter.BASE_PATH + "/" + id, requestId, username,
        additionalParameterMap);
    return this.invokeGetSingle(uri, ProductOptionalParameterResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<ProductOptionalParameterResponse> getProductOptionalParameterSummary(
      GdnRestListRequest listRequest, String username) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    URI uri = this.generateURI(ProductClientProductOptionalParameter.BASE_PATH, listRequest.getRequestId(), username,
        additionalParameterMap);
    return this.invokeGetSummary(uri, ProductOptionalParameterResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse saveProductOptionalParameter(String requestId, String username,
      ProductOptionalParameterRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductClientProductOptionalParameter.SAVE, requestId, username, additionalParameterMap);
    return this.invokePost(uri, ProductOptionalParameterRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse updateProductOptionalParameter(String requestId, String username,
      ProductOptionalParameterRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientProductOptionalParameter.UPDATE, requestId, username, additionalParameterMap);
    return this.invokePost(uri, ProductOptionalParameterRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }
}
