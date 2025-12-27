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
import com.gdn.x.productcategorybase.dto.CatalogType;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.response.CatalogDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;

public class ProductClientCatalog extends GdnBaseRestCrudClient {

  public static final String BASE_PATH = "/api/catalog";
  private static final String CREATE = ProductClientCatalog.BASE_PATH + "/create";
  private static final String FILTER_TYPE = ProductClientCatalog.BASE_PATH + "/filter/type";
  private static final String FILTER_NAME = ProductClientCatalog.BASE_PATH + "/filter/name";
  private static final String UPDATE = ProductClientCatalog.BASE_PATH + "/update";
  private static final String DELETE = ProductClientCatalog.BASE_PATH + "/delete";
  private static final String CATEGORIES_BY_CATALOGTYPE =
      ProductClientCatalog.BASE_PATH + "/filter/categories-by-catalogtype";


  public ProductClientCatalog(GdnRestClientConfiguration clientConfig, String contextPath) {
    super(clientConfig);
    this.setContextPath(contextPath);
  }

  public ProductClientCatalog(String username, String password, String host, Integer port, String storeId,
      String clientId, String channelId, String contextPath) {
    super(username, password, host, port, clientId, channelId, storeId, contextPath);
  }

  public GdnBaseRestResponse createNewCatalog(String requestId, String username, CatalogRequest catalogRequest)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductClientCatalog.CREATE, requestId, username, additionalParameterMap);
    return this.invokePost(uri, CatalogRequest.class, catalogRequest, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse deleteCatalog(String requestId, String username, SimpleRequestHolder holder)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductClientCatalog.DELETE, requestId, username, additionalParameterMap);
    return this.invokePost(uri, CatalogRequest.class, holder, ProductClient.APPLICATION_JSON_VALUE);
  }

  private URI generateURI(String path, String requestId, String username, Map<String, String> additionalParameterMap)
      throws Exception {
    String location = this.getContextPath() + path;
    return this.getHttpClientHelper().getURI(this.getClientConfig().getHost(), this.getClientConfig().getPort(),
        location, this.getMandatoryParameter(this.getDefaultRequestIdValue(requestId), username),
        additionalParameterMap);
  }

  public GdnRestListResponse<CatalogResponse> getCatalogByName(GdnRestListRequest listRequest, String username,
      String name) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("name", name);
    URI uri = this.generateURI(ProductClientCatalog.FILTER_NAME, listRequest.getRequestId(), username,
        additionalParameterMap);
    return this.invokeGetSummary(uri, CatalogResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CatalogResponse> getCatalogByType(GdnRestListRequest listRequest, String username,
      CatalogType catalogType) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("catalogType", catalogType.toString());
    URI uri = this.generateURI(ProductClientCatalog.FILTER_TYPE, listRequest.getRequestId(), username,
        additionalParameterMap);
    return this.invokeGetSummary(uri, CatalogResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<CatalogDetailResponse> getCatalogDetail(String requestId, String username,
      String catalogId) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientCatalog.BASE_PATH + "/" + catalogId, requestId, username, additionalParameterMap);
    return this.invokeGetSingle(uri, CatalogDetailResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CatalogResponse> getCatalogSummary(GdnRestListRequest listRequest, String username)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    URI uri =
        this.generateURI(ProductClientCatalog.BASE_PATH, listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, CatalogResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  private String getDefaultRequestIdValue(String requestId) {
    if ((requestId == null) || (requestId.trim().length() == 0)) {
      return GdnUUIDHelper.generateUUID();
    }
    return requestId;
  }

  public GdnBaseRestResponse updateCatalog(String requestId, String username, CatalogRequest catalogRequest)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductClientCatalog.UPDATE, requestId, username, additionalParameterMap);
    return this.invokePost(uri, CatalogRequest.class, catalogRequest, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<CatalogDetailResponse> getCategoriesFromCatalogType(String requestId,
      String username, CatalogType catalogType) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("catalogType", catalogType.toString());
    URI uri = this.generateURI(ProductClientCatalog.CATEGORIES_BY_CATALOGTYPE, requestId, username,
        additionalParameterMap);
    return this
        .invokeGetSingle(uri, CatalogDetailResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

}
