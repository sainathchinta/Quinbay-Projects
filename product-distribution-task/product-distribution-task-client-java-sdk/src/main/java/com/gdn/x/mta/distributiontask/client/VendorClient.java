package com.gdn.x.mta.distributiontask.client;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.web.client.GdnBaseRestCrudClient;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.client.util.PDTClientUtil;
import com.gdn.x.mta.distributiontask.request.VendorDetailRequest;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorCapacityResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorTaskInformationResponse;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;

/**
 * Created by Alok on 9/22/16.
 */
public class VendorClient extends GdnBaseRestCrudClient {

  private static final String BASE_PATH = "/vendor";
  private static final String CREATE_VENDOR = "/save-vendor";
  private static final String VENDOR_LIST = "/vendor-list";
  private static final String GET_VENDOR_FOR_CODE = "/get-vendor-for-code";
  public static final String APPLICATION_JSON_VALUE = "application/json";
  private static final String GET_VENDORS_CAPACITY = "/get-vendors-capacity";
  private static final String GET_VENDORS_INFORMATION_TASK = "/get-vendors-task-information";
  private static final String CURRENTLY_ASSIGNED_PRODUCT ="/assigned-product-count";
  public static final String DELETE_VENDOR ="/delete-vendor";

  public VendorClient(GdnRestClientConfiguration clientConfig, String contextPath) {
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

  public GdnBaseRestResponse createVendor(String requestId, String username,
      VendorDetailRequest vendorDetailRequest) throws Exception {
    URI uri = generateURI(BASE_PATH + CREATE_VENDOR, requestId, username, null);
    return invokePost(uri, VendorDetailRequest.class, vendorDetailRequest, APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<VendorDetailResponse> findVendorDetailResponseList(String requestId,
      String username, Integer page, Integer size) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(page));
    additionalParameterMap.put("size", String.valueOf(size));
    URI uri = generateURI(BASE_PATH + VENDOR_LIST, requestId, username, additionalParameterMap);
    return invokeGetSummary(uri, VendorDetailResponse.class, APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<VendorDetailResponse> findVendorbyVendorCode(String requestId,
      String username, String vendorCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("vendorCode", String.valueOf(vendorCode));
    URI uri =
        generateURI(BASE_PATH + GET_VENDOR_FOR_CODE, requestId, username, additionalParameterMap);
    return invokeGetSingle(uri, VendorDetailResponse.class, APPLICATION_JSON_VALUE);
  }
  
  public GdnRestListResponse<VendorCapacityResponse> countVendorsCapacity(String requestId,
      String username) throws Exception {
    URI uri =
        generateURI(BASE_PATH + GET_VENDORS_CAPACITY, generateRequestId(requestId), username, null);
    return this.invokeGetSummary(uri, VendorCapacityResponse.class, APPLICATION_JSON_VALUE, null);
  }
  
  public GdnRestListResponse<VendorTaskInformationResponse> countVendorsTask(String requestId,
      String username) throws Exception {
    URI uri =
        generateURI(BASE_PATH + GET_VENDORS_INFORMATION_TASK, generateRequestId(requestId), username, null);
    return this.invokeGetSummary(uri, VendorTaskInformationResponse.class, APPLICATION_JSON_VALUE, null);
  }

  public GdnRestSimpleResponse<Integer> assignedProductCountToVendor(String requestId,
      String username, String vendorCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("vendorCode", String.valueOf(vendorCode));
    URI uri = generateURI(BASE_PATH + CURRENTLY_ASSIGNED_PRODUCT, generateRequestId(requestId),
        username, additionalParameterMap);
    return this.getHttpClientHelper()
        .invokeGetType(uri, new TypeReference<GdnRestSimpleResponse<Integer>>() {
        }, APPLICATION_JSON_VALUE, this.getClientConfig().getConnectionTimeoutInMs());
  }

  public GdnBaseRestResponse deleteVender(String requestId, String username, String vendorCode)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("vendorCode", String.valueOf(vendorCode));
    URI uri = generateURI(BASE_PATH + DELETE_VENDOR, requestId, username, additionalParameterMap);
    return invokePostType(uri, null, Object.class, APPLICATION_JSON_VALUE,
        new TypeReference<GdnBaseRestResponse>() {}, null);
  }
}
