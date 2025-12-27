package com.gdn.x.productcategorybase.client.brand;

import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.client.GdnBaseRestClient;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandNamesRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandNamesResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandSummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandResponse;
import com.gdn.x.productcategorybase.dto.brand.UndeleteBrandRequest;
import com.gdn.x.productcategorybase.dto.brand.UndeleteBrandResponse;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandRequest;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class BrandClient extends GdnBaseRestClient {

  public static final String APPLICATION_JSON_VALUE = "application/json";

  public BrandClient(GdnRestClientConfiguration clientConfig, String contextPath) {
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

  public GdnRestSingleResponse<CreateBrandResponse> create(String requestId, String username, CreateBrandRequest request)
      throws Exception {
    if (request == null) {
      throw new ApplicationException(ErrorCategory.VALIDATION, BrandClientErrorMessage.REQUEST_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    URI uri =
        this.generateURI(BrandClientPath.BASE_PATH + BrandClientPath.CREATE, requestId, username, additionalParameters);
    return invokePostType(uri, request, CreateBrandRequest.class, BrandClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestSingleResponse<CreateBrandResponse>>() {});
  }

  public GdnBaseRestResponse update(String requestId, String username, UpdateBrandRequest request) throws Exception {
    if (request == null) {
      throw new ApplicationException(ErrorCategory.VALIDATION, BrandClientErrorMessage.REQUEST_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    URI uri =
        this.generateURI(BrandClientPath.BASE_PATH + BrandClientPath.UPDATE, requestId, username, additionalParameters);
    return invokePostType(uri, request, UpdateBrandRequest.class, BrandClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnBaseRestResponse>() {});
  }
  
  public GdnBaseRestResponse delete(String requestId, String username, String brandCode) throws Exception {
    if (StringUtils.isEmpty(brandCode)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          BrandClientErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    additionalParameters.put("brandCode", brandCode);
    URI uri =
        this.generateURI(BrandClientPath.BASE_PATH + BrandClientPath.DELETE, requestId, username,
            additionalParameters);
    return this.getHttpClientHelper().invokeGetType(uri,
        new TypeReference<GdnBaseRestResponse>() {}, BrandClient.APPLICATION_JSON_VALUE,
        this.getClientConfig().getConnectionTimeoutInMs());
  }
  
  public GdnRestSingleResponse<UndeleteBrandResponse> undelete(String requestId, String username,
      UndeleteBrandRequest request) throws Exception {
    if (request == null) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, BrandClientErrorMessage.REQUEST_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    URI uri =
        this.generateURI(BrandClientPath.BASE_PATH + BrandClientPath.UNDELETE, requestId, username,
            additionalParameters);
    return invokePostType(uri, request, UndeleteBrandRequest.class, BrandClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestSingleResponse<UndeleteBrandResponse>>() {});
  }

  public GdnRestSingleResponse<BrandResponse> filterByBrandCode(String requestId, String username, String brandCode)
      throws Exception {
    if (StringUtils.isEmpty(brandCode)) {
      throw new ApplicationException(ErrorCategory.VALIDATION, BrandClientErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    additionalParameters.put("brandCode", brandCode);
    URI uri =
        this.generateURI(BrandClientPath.BASE_PATH + BrandClientPath.FILTER_BRAND_CODE, requestId, username,
            additionalParameters);
    return invokeGetSingle(uri, BrandResponse.class, BrandClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<BrandResponse> filterByBrandName(String requestId, String username, String brandName,
      boolean markForDelete) throws Exception {
    if (StringUtils.isEmpty(brandName)) {
      throw new ApplicationException(ErrorCategory.VALIDATION, BrandClientErrorMessage.BRAND_NAME_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    additionalParameters.put("brandName", brandName);
    additionalParameters.put("markForDelete", String.valueOf(markForDelete));
    URI uri =
        this.generateURI(BrandClientPath.BASE_PATH + BrandClientPath.FILTER_BRAND_NAME, requestId, username,
            additionalParameters);
    return invokeGetSingle(uri, BrandResponse.class, BrandClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<BrandResponse> filterSummary(String requestId, String username, Integer page,
      Integer size, BrandSummaryRequest request) throws Exception {
    if (request == null) {
      throw new ApplicationException(ErrorCategory.VALIDATION, BrandClientErrorMessage.REQUEST_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    additionalParameters.put("page", String.valueOf(page));
    additionalParameters.put("size", String.valueOf(size));
    URI uri =
        this.generateURI(BrandClientPath.BASE_PATH + BrandClientPath.FILTER_SUMMARY, requestId, username,
            additionalParameters);
    return invokePostType(uri, request, BrandSummaryRequest.class, BrandClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<BrandResponse>>() {});
  }
  
  public GdnRestListResponse<BrandResponse> filterSummaryByName(String requestId, String username, Integer page,
      Integer size, String brandName) throws Exception {
    Map<String, String> additionalParameters = new HashMap<String, String>();
    additionalParameters.put("page", String.valueOf(page));
    additionalParameters.put("size", String.valueOf(size));
    additionalParameters.put("brandName", brandName);
    URI uri =
        this.generateURI(BrandClientPath.BASE_PATH + BrandClientPath.FILTER_SUMMARY_ORDER_BY_NAME, requestId, username,
            additionalParameters);
    return this.invokeGetSummary(uri, BrandResponse.class, BrandClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<BrandNamesResponse> getBrandNamesByBrandCodes(String requestId,
      String username, BrandNamesRequest request) throws Exception {
    if (request == null) {
      throw new ApplicationException(ErrorCategory.VALIDATION,
          BrandClientErrorMessage.REQUEST_MUST_NOT_BE_BLANK);
    }
    URI uri = this.generateURI(BrandClientPath.BASE_PATH + BrandClientPath.GET_BRAND_NAMES_BY_CODES,
        requestId, username, null);
    return invokePostType(uri, request, BrandNamesRequest.class, BrandClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<BrandNamesResponse>>() {
        });
  }
  
}
