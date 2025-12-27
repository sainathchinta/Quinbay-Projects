package com.gdn.x.productcategorybase.client.categorytree;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.client.GdnBaseRestClient;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.productcategorybase.dto.categorytree.CategoryNodeFilterCategoryCodesRequest;
import com.gdn.x.productcategorybase.dto.categorytree.CategoryNodeFilterParentCategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.categorytree.CategoryNodeResponse;
import com.gdn.x.productcategorybase.dto.categorytree.CategoryTreeFilterCategoryCodesRequest;

public class CategoryTreeClient extends GdnBaseRestClient {

  public static final String APPLICATION_JSON_VALUE = "application/json";

  public CategoryTreeClient(GdnRestClientConfiguration clientConfig, String contextPath) {
    super(clientConfig);
    this.setContextPath(contextPath);
  }

  private String generateRequestId(String requestId) {
    String generatedRequestId = requestId;
    if (StringUtils.isEmpty(requestId)) {
      generatedRequestId = UUID.randomUUID().toString();
    }
    return generatedRequestId;
  }

  @SuppressWarnings("deprecation")
  URI generateURI(String path, String requestId, String username, Map<String, String> additionalParameters)
      throws Exception {
    return this.getHttpClientHelper().getURI(this.getClientConfig().getHost(), this.getClientConfig().getPort(),
        this.getContextPath() + path, this.getMandatoryParameter(this.generateRequestId(requestId), username),
        additionalParameters);
  }

  public GdnRestListResponse<CategoryNodeResponse> filterCategoryNodeByCategoryCodes(String requestId, String username,
      CategoryNodeFilterCategoryCodesRequest request) throws Exception {
    if (request == null) {
      throw new ApplicationException(ErrorCategory.VALIDATION, CategoryTreeClientErrorMessage.REQUEST_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    URI uri =
        this.generateURI(CategoryTreeClientPath.BASE_PATH + CategoryTreeClientPath.FILTER_NODE_CATEGORY_CODES,
            requestId, username, additionalParameters);
    return invokePostType(uri, request, CategoryNodeFilterCategoryCodesRequest.class,
        CategoryTreeClient.APPLICATION_JSON_VALUE, new TypeReference<GdnRestListResponse<CategoryNodeResponse>>() {});
  }

  public GdnRestListResponse<CategoryNodeResponse> filterCategoryNodeByParentCategoryCode(String requestId,
      String username, CategoryNodeFilterParentCategoryCodeRequest request) throws Exception {
    if (request == null) {
      throw new ApplicationException(ErrorCategory.VALIDATION, CategoryTreeClientErrorMessage.REQUEST_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    URI uri =
        this.generateURI(CategoryTreeClientPath.BASE_PATH + CategoryTreeClientPath.FILTER_NODE_PARENT_CATEGORY_CODE,
            requestId, username, additionalParameters);
    return invokePostType(uri, request, CategoryNodeFilterParentCategoryCodeRequest.class,
        CategoryTreeClient.APPLICATION_JSON_VALUE, new TypeReference<GdnRestListResponse<CategoryNodeResponse>>() {});
  }

  public GdnRestListResponse<CategoryNodeResponse> filterByCategoryCodes(String requestId, String username,
      CategoryTreeFilterCategoryCodesRequest request) throws Exception {
    if (request == null) {
      throw new ApplicationException(ErrorCategory.VALIDATION, CategoryTreeClientErrorMessage.REQUEST_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    URI uri =
        this.generateURI(CategoryTreeClientPath.BASE_PATH + CategoryTreeClientPath.FILTER_CATEGORY_CODES, requestId,
            username, additionalParameters);
    return invokePostType(uri, request, CategoryTreeFilterCategoryCodesRequest.class,
        CategoryTreeClient.APPLICATION_JSON_VALUE, new TypeReference<GdnRestListResponse<CategoryNodeResponse>>() {});
  }

}
