package com.gdn.x.mta.distributiontask.client;

import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.web.client.GdnBaseRestCrudClient;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.client.util.ProductDistributionTaskClientPath;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkVendorProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.BulkVendorProductActionsResponse;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

/**
 * Created by virajjasani on 23/09/16.
 */
public class VendorActivityClient extends GdnBaseRestCrudClient {

  public static final String BASE_PATH = "/vendor-activity";
  private static final String ROOT = "/";
  private static final String UPDATE_PRODUCT_CONTENT = ROOT + "update-product-content";
  private static final String UPDATE_PRODUCT_IMAGE_INFO = ROOT + "update-product-image";
  private static final String REJECT_PRODUCT = ROOT + "reject-product";
  private static final String VENDOR_APPROVAL = ROOT + "approval";
  private static final String VENDOR_APPROVAL_NEW = ROOT + "approval-new";
  private static final String VENDOR_CODE = "vendorCode";
  private static final String APPROVAL_TYPE = "approvalType";
  private static final String BULK_VENDOR_PRODUCT_ACTIONS = "/bulkVendorProductActions";

  public VendorActivityClient(GdnRestClientConfiguration clientConfig) {
    super(clientConfig);
  }

  public VendorActivityClient(GdnRestClientConfiguration clientConfig, String contextPath) {
    super(clientConfig);
    setContextPath(contextPath);
  }

  public VendorActivityClient(String username, String password, String host, Integer port,
      String clientId, String channelId, String storeId, String contextPath) {
    super(username, password, host, port, clientId, channelId, storeId, contextPath);
  }

  @SuppressWarnings("deprecation")
  private URI generateURI(String path, String requestId, String username,
      Map<String, String> additionalParameterMap) throws Exception {
    return getHttpClientHelper().getURI(getClientConfig().getHost(), getClientConfig().getPort(),
        getContextPath() + path, getMandatoryParameter(generateRequestId(requestId), username),
        additionalParameterMap);
  }

  private String generateRequestId(String requestId) {
    if (StringUtils.isEmpty(requestId)) {
      requestId = UUID.randomUUID().toString();
    }
    return requestId;
  }

  public GdnBaseRestResponse updateProductContent(String requestId, String username,
      String vendorCode, DistributionProductDetailRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(VENDOR_CODE, vendorCode);
    URI uri = generateURI(BASE_PATH + UPDATE_PRODUCT_CONTENT, requestId, username,
        additionalParameterMap);
    return invokePost(uri, DistributionProductDetailRequest.class, request,
        ProductDistributionTaskClientPath.APPLICATION_JSON_VALUE, null);
  }

  public GdnBaseRestResponse updateProductImage(String requestId, String username,
      String vendorCode, DistributionProductDetailRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(VENDOR_CODE, vendorCode);
    URI uri = generateURI(BASE_PATH + UPDATE_PRODUCT_IMAGE_INFO, requestId, username,
        additionalParameterMap);
    return invokePost(uri, DistributionProductDetailRequest.class, request,
        ProductDistributionTaskClientPath.APPLICATION_JSON_VALUE, null);
  }

  public GdnBaseRestResponse rejectProduct(String requestId, String username,
      String vendorCode, RejectProductVendorRequest deleteProductRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(VENDOR_CODE, vendorCode);
    URI uri = generateURI(BASE_PATH + REJECT_PRODUCT, requestId, username, additionalParameterMap);
    return invokePost(uri, RejectProductVendorRequest.class, deleteProductRequest,
        ProductDistributionTaskClientPath.APPLICATION_JSON_VALUE, null);
  }

  public GdnBaseRestResponse approveProduct(String requestId, String username,
      VendorApprovalRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = generateURI(BASE_PATH + VENDOR_APPROVAL, requestId, username, additionalParameterMap);
    return invokePost(uri, VendorApprovalRequest.class, request,
        ProductDistributionTaskClientPath.APPLICATION_JSON_VALUE, null);
  }

  public GdnBaseRestResponse approveProductNew(String requestId, String username, String approvalType,
      String vendorCode, DistributionProductDetailRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(VENDOR_CODE, vendorCode);
    additionalParameterMap.put(APPROVAL_TYPE, vendorCode);
    URI uri = generateURI(BASE_PATH + VENDOR_APPROVAL_NEW, requestId, username, additionalParameterMap);
    return invokePost(uri, VendorApprovalRequest.class, request,
        ProductDistributionTaskClientPath.APPLICATION_JSON_VALUE, null);
  }

  public GdnRestSingleResponse<BulkVendorProductActionsResponse> bulkVendorProductActions(String requestId, String username,
      BulkVendorProductActionsRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<>();
    URI uri = generateURI(BASE_PATH + BULK_VENDOR_PRODUCT_ACTIONS, requestId, username, additionalParameterMap);
    return invokePostType(uri, request, BulkVendorProductActionsRequest.class,
        ProductDistributionTaskClientPath.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestSingleResponse<BulkVendorProductActionsResponse>>() {
        }, null);
  }

}
