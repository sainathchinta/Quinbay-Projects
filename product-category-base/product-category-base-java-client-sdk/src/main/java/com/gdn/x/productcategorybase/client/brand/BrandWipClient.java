package com.gdn.x.productcategorybase.client.brand;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.client.GdnBaseRestClient;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;

public class BrandWipClient extends GdnBaseRestClient {

  public static final String APPLICATION_JSON_VALUE = "application/json";

  public BrandWipClient(GdnRestClientConfiguration clientConfig, String contextPath) {
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

  public GdnRestSingleResponse<BrandWipResponse> findBrandWipByBrandNameAndBusinessPartnerCode(String requestId, String username,
      String brandName, String businessPartnerCode) throws Exception {
    if (StringUtils.isEmpty(brandName)) {
      throw new ApplicationException(ErrorCategory.VALIDATION, BrandClientErrorMessage.BRAND_NAME_MUST_NOT_BE_BLANK);
    }
    Map<String, String> additionalParameters = new HashMap<String, String>();
    additionalParameters.put("brandName", brandName);
    additionalParameters.put("businessPartnerCode", businessPartnerCode);
    URI uri = this.generateURI(BrandWipClientPath.BASE_PATH + BrandWipClientPath.FILTER_BRAND_NAME, requestId, username,
        additionalParameters);
    return invokeGetSingle(uri, BrandWipResponse.class, BrandWipClient.APPLICATION_JSON_VALUE);
  }
}
