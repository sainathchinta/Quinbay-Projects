package com.gdn.x.mta.distributiontask.client;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.web.client.GdnBaseRestCrudClient;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.mta.distributiontask.request.ProductDistributionTaskRequest;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

/**
 * Created by Alok on 9/22/16.
 */
public class DistributionTaskClient extends GdnBaseRestCrudClient {

  public static final String BASE_PATH = "/distirbutionTask";
  private static final String PRODUCT_VENDOR_MAPPING = "/product-vendor-mapping";
  private static final String PRODUCT_REMOVE = "/remove-product";
  public static final String APPLICATION_JSON_VALUE = "application/json";

  public DistributionTaskClient(GdnRestClientConfiguration clientConfig, String contextPath) {
    super(clientConfig);
    this.setContextPath(contextPath);
  }

  private URI generateURI(String path, String requestId, String username,
      Map<String, String> additionalParameterMap) throws Exception {
    return getHttpClientHelper()
        .getURI(getClientConfig().getHost(), getClientConfig().getPort(), getContextPath() + path,
            getMandatoryParameter(generateRequestId(requestId), username), additionalParameterMap);
  }

  private String generateRequestId(String requestId) {
    if (StringUtils.isEmpty(requestId)) {
      requestId = UUID.randomUUID().toString();
    }
    return requestId;
  }

  public GdnBaseRestResponse saveProductDistributionTask(String requestId, String username, ProductDistributionTaskRequest request)
      throws Exception {
    URI uri = generateURI(BASE_PATH + PRODUCT_VENDOR_MAPPING, requestId, username, null);
    return invokePost(uri, ProductDistributionTaskRequest.class, request, APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse removeProductFromPDT(String requestId, String username,
      RemoveProductRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("productCode", request.getProductCode());
    URI uri = generateURI(BASE_PATH + PRODUCT_REMOVE, requestId, username, additionalParameterMap);
    return invokePost(uri, RemoveProductRequest.class, request, APPLICATION_JSON_VALUE);
  }
}
