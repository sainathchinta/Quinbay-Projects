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
import com.gdn.x.productcategorybase.CategoryShippingApiPath;
import com.gdn.x.productcategorybase.dto.request.CategoryShippingRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryShippingResponse;

public class ProductClientCategoryShipping extends GdnBaseRestCrudClient {

  public ProductClientCategoryShipping(GdnRestClientConfiguration clientConfig, String contextPath) {
    super(clientConfig);
    this.setContextPath(contextPath);
  }

  public ProductClientCategoryShipping(String username, String password, String host, Integer port, String storeId,
      String clientId, String channelId, String contextPath) {
    super(username, password, host, port, clientId, channelId, storeId, contextPath);
  }

  public GdnBaseRestResponse deleteCategoryShipping(String requestId, String username, SimpleRequestHolder holder)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.DELETE_VALUE, requestId,
        username, additionalParameterMap);
    return this.invokePost(uri, CategoryShippingRequest.class, holder, ProductClient.APPLICATION_JSON_VALUE);
  }

  private URI generateURI(String path, String requestId, String username, Map<String, String> additionalParameterMap)
      throws Exception {
    String location = this.getContextPath() + path;
    return this.getHttpClientHelper().getURI(this.getClientConfig().getHost(), this.getClientConfig().getPort(),
        location, this.getMandatoryParameter(this.getDefaultRequestIdValue(requestId), username),
        additionalParameterMap);
  }

  public GdnRestListResponse<CategoryShippingResponse> getCategoryShippingByCategoryNameAndPageable(
      GdnRestListRequest listRequest, String username, String categoryName) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("categoryName", categoryName);
    URI uri = this.generateURI(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.FILTER_CATEGORY_CODE,
        listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryShippingResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryShippingResponse> getCategoryShippingByShippingCodeAndPageable(
      GdnRestListRequest listRequest, String username, String shippingCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("shippingCode", shippingCode);
    URI uri = this.generateURI(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.FILTER_SHIPPING_CODE,
        listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryShippingResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryShippingResponse> getCategoryShippingSummary(GdnRestListRequest listRequest,
      String username) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    URI uri = this.generateURI(CategoryShippingApiPath.BASE_PATH, listRequest.getRequestId(), username,
        additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryShippingResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  private String getDefaultRequestIdValue(String requestId) {
    if ((requestId == null) || (requestId.trim().length() == 0)) {
      return GdnUUIDHelper.generateUUID();
    }
    return requestId;
  }

  public GdnBaseRestResponse saveCategoryShipping(String requestId, String username, CategoryShippingRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.SAVE, requestId, username,
        additionalParameterMap);
    return this.invokePost(uri, CategoryShippingRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse updateCategoryShipping(String requestId, String username, CategoryShippingRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.UPDATE, requestId, username,
        additionalParameterMap);
    return this.invokePost(uri, CategoryShippingRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }

}
