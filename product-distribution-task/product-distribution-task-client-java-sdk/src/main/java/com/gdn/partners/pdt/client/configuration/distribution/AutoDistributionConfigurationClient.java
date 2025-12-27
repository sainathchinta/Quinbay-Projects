package com.gdn.partners.pdt.client.configuration.distribution;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.client.GdnBaseRestClient;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pdt.dto.configuration.distribution.AutoDistributionConfigurationResponse;
import com.gdn.partners.pdt.dto.configuration.distribution.CreateAutoDistributionConfigurationRequest;
import com.gdn.partners.pdt.dto.configuration.distribution.UpdateAutoDistributionConfigurationRequest;

public class AutoDistributionConfigurationClient extends GdnBaseRestClient {

  public static final String APPLICATION_JSON_VALUE = "application/json";

  public AutoDistributionConfigurationClient(GdnRestClientConfiguration clientConfig, String contextPath) {
    super(clientConfig);
    this.setContextPath(contextPath);
  }

  private String generateRequestId(String requestId) throws Exception {
    String generatedRequestId = requestId;
    if (StringUtils.isEmpty(requestId)) {
      generatedRequestId = UUID.randomUUID().toString();
    }
    return generatedRequestId;
  }

  @SuppressWarnings("deprecation")
  private URI generateURI(String path, String requestId, String username, Map<String, String> additionalParameters)
      throws Exception {
    return this.getHttpClientHelper().getURI(this.getClientConfig().getHost(), this.getClientConfig().getPort(),
        this.getContextPath() + path, this.getMandatoryParameter(this.generateRequestId(requestId), username),
        additionalParameters);
  }

  public GdnRestListResponse<AutoDistributionConfigurationResponse> filterByVendorCode(String requestId,
      String username, String vendorCode) throws Exception {
    if (StringUtils.isEmpty(vendorCode)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          AutoDistributionConfigurationClientErrorMessage.VENDOR_CODE_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    additionalParameters.put("vendorCode", vendorCode);
    URI uri =
        this.generateURI(AutoDistributionConfigurationClientPath.BASE_PATH
            + AutoDistributionConfigurationClientPath.FILTER_VENDOR_CODE, requestId, username, additionalParameters);
    return this.invokeGetSummary(uri, AutoDistributionConfigurationResponse.class,
        AutoDistributionConfigurationClient.APPLICATION_JSON_VALUE, null);
  }

  public GdnBaseRestResponse create(String requestId, String username,
      CreateAutoDistributionConfigurationRequest request) throws Exception {
    if (request == null) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          AutoDistributionConfigurationClientErrorMessage.REQUEST_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    URI uri =
        this.generateURI(AutoDistributionConfigurationClientPath.BASE_PATH
            + AutoDistributionConfigurationClientPath.CREATE, requestId, username, additionalParameters);
    return this.invokePost(uri, CreateAutoDistributionConfigurationRequest.class, request,
        AutoDistributionConfigurationClient.APPLICATION_JSON_VALUE, null);
  }

  public GdnBaseRestResponse update(String requestId, String username,
      UpdateAutoDistributionConfigurationRequest request) throws Exception {
    if (request == null) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          AutoDistributionConfigurationClientErrorMessage.REQUEST_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    URI uri =
        this.generateURI(AutoDistributionConfigurationClientPath.BASE_PATH
            + AutoDistributionConfigurationClientPath.UPDATE, requestId, username, additionalParameters);
    return this.invokePost(uri, UpdateAutoDistributionConfigurationRequest.class, request,
        AutoDistributionConfigurationClient.APPLICATION_JSON_VALUE, null);
  }

}
