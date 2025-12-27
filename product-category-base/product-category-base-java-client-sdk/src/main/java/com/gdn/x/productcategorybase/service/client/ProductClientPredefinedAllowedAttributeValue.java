package com.gdn.x.productcategorybase.service.client;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.common.web.client.GdnBaseRestCrudClient;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.PredefinedAllowedAttributeValueApiPath;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

public class ProductClientPredefinedAllowedAttributeValue extends GdnBaseRestCrudClient {

  public ProductClientPredefinedAllowedAttributeValue(GdnRestClientConfiguration clientConfig, String contextPath) {
    super(clientConfig);
    this.setContextPath(contextPath);
  }

  public ProductClientPredefinedAllowedAttributeValue(String username, String password, String host, Integer port,
      String clientId, String channelId, String storeId, String contextPath) {
    super(username, password, host, port, clientId, channelId, storeId, contextPath);
  }

  public GdnBaseRestResponse deactivated(String requestId, String username, String predefinedAllowedAttributeValueId)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("id", predefinedAllowedAttributeValueId);
    URI uri = this.generateURI(
        PredefinedAllowedAttributeValueApiPath.BASE_PATH + PredefinedAllowedAttributeValueApiPath.DEACTIVATED,
        requestId, username, additionalParameterMap);
    return this.invokeGetSingle(uri, PredefinedAllowedAttributeValueResponse.class,
        ProductClient.APPLICATION_JSON_VALUE);
  }
  
  public GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse> findByStoreIdAndMatchAttributeCodeAndValue(
      String requestId, String username, String attributeCode, String value) throws Exception {
    Map<String, String> additionalParameters = new HashMap<String, String>();
    additionalParameters.put("attributeCode", attributeCode);
    additionalParameters.put("value", value);
    URI uri =
        this.generateURI(PredefinedAllowedAttributeValueApiPath.BASE_PATH
            + PredefinedAllowedAttributeValueApiPath.GET_BY_ATTRIBUTE_CODE_AND_VALUE, requestId,
            username, additionalParameters);
    return invokeGetSingle(uri, PredefinedAllowedAttributeValueResponse.class,
        ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<PredefinedAllowedAttributeValueResponse> findByStoreIdAndAttributeIdAndMarkForDeleteFalse(
      String requestId, String username, GdnRestListRequest listRequest, String attributeId, String value)
          throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("attributeId", attributeId);
    additionalParameterMap.put("value", value);
    URI uri = this.generateURI(
        PredefinedAllowedAttributeValueApiPath.BASE_PATH
            + PredefinedAllowedAttributeValueApiPath.FILTER_ATTRIBUTE_ID_AND_VALUE,
        listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, PredefinedAllowedAttributeValueResponse.class,
        ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getBrandSuggestions(String requestId,
      String username, GdnRestListRequest listRequest, String value, boolean isSearch, boolean isExternal, String businessPartnerCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    Map<String, String> headers = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("value", value);
    additionalParameterMap.put("isSearch",String.valueOf(isSearch));
    additionalParameterMap.put("isExternal", String.valueOf(isExternal));
    additionalParameterMap.put("businessPartnerCode", businessPartnerCode);
    URI uri = this.generateURI(PredefinedAllowedAttributeValueApiPath.BASE_PATH
            + PredefinedAllowedAttributeValueApiPath.FILTER_ATTRIBUTE_ID_AND_VALUE_FOR_SUGGESTIONS,
        listRequest.getRequestId(), username, additionalParameterMap);
    return this
        .invokeGetSummary(uri, PredefinedAllowedAttributeValueResponse.class, ProductClient.APPLICATION_JSON_VALUE,
            headers);
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

  public GdnBaseRestResponse saveAttribute(String requestId, String username, String attributeId,
      PredefinedAllowedAttributeValueRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("attributeId", attributeId);
    URI uri =
        this.generateURI(PredefinedAllowedAttributeValueApiPath.BASE_PATH + PredefinedAllowedAttributeValueApiPath.SAVE,
            requestId, username, additionalParameterMap);
    return this.invokePost(uri, PredefinedAllowedAttributeValueRequest.class, request,
        ProductClient.APPLICATION_JSON_VALUE);
  }

}
