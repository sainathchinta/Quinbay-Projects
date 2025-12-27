package com.gdn.x.productcategorybase.client.category;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.client.GdnBaseRestClient;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.fasterxml.jackson.core.type.TypeReference;

public class CategoryClient extends GdnBaseRestClient {

  public static final String APPLICATION_JSON_VALUE = "application/json";
  public static final String REQUEST_MUST_NOT_BE_BLANK = "request mut not be blank";

  public CategoryClient(GdnRestClientConfiguration clientConfig, String contextPath) {
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
    return this.getHttpClientHelper()
        .getURI(this.getClientConfig().getHost(), this.getClientConfig().getPort(), this.getContextPath() + path,
            this.getMandatoryParameter(this.generateRequestId(requestId), username), additionalParameters);
  }


  public GdnRestSingleResponse<CategoryCodeResponse> getAllChildCategoriesFromC1CategoryCode(String requestId,
      String username, CategoryCodeRequest request) throws Exception {
    if (request == null) {
      throw new ApplicationException(ErrorCategory.VALIDATION, CategoryClient.REQUEST_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    URI uri = this.generateURI(CategoryClientPath.BASE_PATH + CategoryClientPath.GET_ALL_CHILDS_FROM_C1_CATERGORY_CODE,
        requestId, username, additionalParameters);
    return invokePostType(uri, request, CategoryCodeRequest.class, CategoryClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestSingleResponse<CategoryCodeResponse>>() {
        });
  }
}
