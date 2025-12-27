package com.gdn.x.productcategorybase.service.client;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.util.GdnHttpClientHelper;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.common.web.client.GdnBaseRestCrudClient;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.AllowedAttributeValueApiPath;
import com.gdn.x.productcategorybase.AttributeApiPath;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.ListHolderRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeAddRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeSummaryResponse;

public class ProductClientAttribute extends GdnBaseRestCrudClient {

  private static final String VALUES = "/values";
  private static final String SLASH = "/";

  public ProductClientAttribute(GdnRestClientConfiguration clientConfig, String contextPath) {
    super(clientConfig);
    this.setContextPath(contextPath);
  }

  public ProductClientAttribute(String username, String password, String host, Integer port, String storeId,
      String clientId, String channelId, String contextPath) {
    super(username, password, host, port, clientId, channelId, storeId, contextPath);
  }

  public GdnBaseRestResponse deleteAllowedAttributeValue(String requestId, String username, SimpleRequestHolder holder)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(AttributeApiPath.BASE_PATH + AttributeApiPath.DELETE_VALUE, requestId, username,
        additionalParameterMap);
    return this.invokePost(uri, AttributeRequest.class, holder, ProductClient.APPLICATION_JSON_VALUE);
  }

  private URI generateURI(String path, String requestId, String username, Map<String, String> additionalParameterMap)
      throws Exception {
    String location = this.getContextPath() + path;
    return this.getHttpClientHelper().getURI(this.getClientConfig().getHost(), this.getClientConfig().getPort(),
        location, this.getMandatoryParameter(this.getDefaultRequestIdValue(requestId), username),
        additionalParameterMap);
  }

  public GdnRestListResponse<AttributeResponse> getAttributeByAttributeCode(GdnRestListRequest listRequest,
      String username, String attributeCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("attributeCode", attributeCode);
    URI uri = this.generateURI(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_ATTRIBUTE_CODE,
        listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, AttributeResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<AttributeResponse> getAttributeByAttributeTypeAndPageable(GdnRestListRequest listRequest,
      String username, AttributeType attributeType) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("attributeType", attributeType.toString());
    URI uri = this.generateURI(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_TYPE, listRequest.getRequestId(),
        username, additionalParameterMap);
    return this.invokeGetSummary(uri, AttributeResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<AttributeResponse> getAttributeByNameLikeWithAndPageable(GdnRestListRequest listRequest,
      String username, String name) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("name", name);
    URI uri = this.generateURI(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_NAME_LIKE,
        listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, AttributeResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<AttributeResponse> getAttributeByNameStartingWithAndPageable(
      GdnRestListRequest listRequest, String username, String name) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("name", name);
    URI uri = this.generateURI(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_NAME, listRequest.getRequestId(),
        username, additionalParameterMap);
    return this.invokeGetSummary(uri, AttributeResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<AttributeResponse> getAttributeBySearchAbleFalseAndPageable(GdnRestListRequest listRequest,
      String username) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    URI uri = this.generateURI(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_SEARCHABLE_FALSE,
        listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, AttributeResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<AttributeResponse> getAttributeBySearchAbleTrueAndPageable(GdnRestListRequest listRequest,
      String username) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    URI uri = this.generateURI(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_SEARCHABLE_TRUE,
        listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, AttributeResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<AttributeResponse> getAttributeDetail(GdnRestListRequest listRequest, String username,
      String attributeId) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    URI uri = this.generateURI(AttributeApiPath.BASE_PATH + "/" + attributeId, listRequest.getRequestId(), username,
        additionalParameterMap);
    return this.invokeGetSingle(uri, AttributeResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }
  
  public GdnRestListResponse<AttributeResponse> getAttributeDetailByAttributeCodes(String requestId,
      String username, AttributeCodesRequest attributeCodeRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_ATTRIBUTE_CODES,
        requestId, username, additionalParameterMap);
    return this.invokePostType(uri, attributeCodeRequest, AttributeCodesRequest.class,
        ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<AttributeResponse>>() {});
  }

  public GdnRestListResponse<AttributeResponse> getAttributeSummary(GdnRestListRequest listRequest, String username)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    URI uri =
        this.generateURI(AttributeApiPath.BASE_PATH, listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, AttributeResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  private String getDefaultRequestIdValue(String requestId) {
    if ((requestId == null) || (requestId.trim().length() == 0)) {
      return GdnUUIDHelper.generateUUID();
    }
    return requestId;
  }

  public GdnBaseRestResponse saveAttribute(String requestId, String username, AttributeRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(AttributeApiPath.BASE_PATH + AttributeApiPath.SAVE, requestId, username,
        additionalParameterMap);
    return this.invokePost(uri, AttributeRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse updateAttribute(String requestId, String username, AttributeRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(AttributeApiPath.BASE_PATH + AttributeApiPath.UPDATE, requestId, username,
        additionalParameterMap);
    return this.invokePost(uri, AttributeRequest.class, request, ProductClient.APPLICATION_JSON_VALUE);
  }
  
  public GdnRestSingleResponse<CategoryAttributeSummaryResponse> getAttributeDetailByCategoryCode(String requestId, String username,
      String categoryCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("categoryCode", categoryCode);
    
    URI uri = this.generateURI(AttributeApiPath.BASE_PATH + AttributeApiPath.FILTER_ATTRIBUTE_DETAIL_BY_CATEGORY_CODE, 
        requestId, username, additionalParameterMap);
    return this.invokeGetSingle(uri, CategoryAttributeSummaryResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }
  
  public GdnRestListResponse<AllowedAttributeValueResponse> getPredefinedAndDefiningAllowedAttributeValue(String requestId, String username, 
      ListHolderRequest<AllowedAttributeValueRequest> requestList) throws Exception {
    URI uri = this.generateURI(AllowedAttributeValueApiPath.BASE_PATH + AllowedAttributeValueApiPath.FIND_ALLOWED_VALUES, requestId, username, null);
    return this.invokePostType(uri, requestList, requestList.getClass(), ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<AllowedAttributeValueResponse>>(){}, null);
  }

  public GdnRestSingleResponse<AttributeValueResponse> addAttributeValue(String requestId, String username,
      String attributeCode, MasterAttributeAddRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH + SLASH + attributeCode + VALUES, requestId,
        username, additionalParameterMap);
    return this.invokePostType(uri, request, MasterAttributeAddRequest.class, ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestSingleResponse<AttributeValueResponse>>() {});
  }
}
