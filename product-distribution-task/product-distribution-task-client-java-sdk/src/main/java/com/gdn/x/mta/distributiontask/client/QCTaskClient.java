package com.gdn.x.mta.distributiontask.client;

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.web.client.GdnBaseRestCrudClient;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.request.RejectProductRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by Poornima on 9/26/16.
 */

@Slf4j
public class QCTaskClient extends GdnBaseRestCrudClient {

    private static final String BASE_PATH = "/qcTask";
    private static final String ROOT = "/";
    private static final String FILTER_QC_READY_PRODUCT = "filter/product/qcready";
    private static final String REJECT_PRODUCT = "reject-product";
    public static final String APPLICATION_JSON_VALUE = "application/json";
    private static final String APPROVE_PRODUCT = "approve-product";
    private static final String MOVE_FAILED_PRODUCT_TO_QC = ROOT + "move-failed-product-to-qc";
  private static final String GET_PRODUCT_SUMMARY_COUNT_QC = ROOT + "get-product-count-for-qc";


    public QCTaskClient(GdnRestClientConfiguration clientConfig, String contextPath) {
        super(clientConfig);
        this.setContextPath(contextPath);
    }

    private URI generateURI(String path, String requestId, String username,
        Map<String, String> additionalParameterMap) throws Exception {
        return getHttpClientHelper()
            .getURI(getClientConfig().getHost(), getClientConfig().getPort(),
                getContextPath() + path,
                getMandatoryParameter(generateRequestId(requestId), username),
                additionalParameterMap);
    }

    private String generateRequestId(String requestId) {
        if (StringUtils.isEmpty(requestId)) {
            requestId = UUID.randomUUID().toString();
        }
        return requestId;
    }

    private Map<String, String> getParametersMapForProductRequest(ProductListRequest request)
        throws IllegalAccessException, InvocationTargetException {
        Map<String, String> additionalParameterMap = new HashMap<String, String>();
        for (Method method : request.getClass().getMethods()) {
            if (isGetterMethod(method) && !method.getReturnType().equals(List.class)) {
                String attributeName = getAttributeName(method);
                if (attributeName == null || attributeName.equals("class")) {
                    continue;
                }
                String attributeValue = (String) method.invoke(request);
                if (attributeValue != null) {
                    additionalParameterMap.put(attributeName, attributeValue);
                }
            }
        }
        return additionalParameterMap;
    }

    private static boolean isGetterMethod(Method method) {
        return method.getName().startsWith("get") && method.getParameterTypes().length == 0;
    }

    private String getAttributeName(Method method) {
        try {
            Class<?> clazz = method.getDeclaringClass();
            BeanInfo info = Introspector.getBeanInfo(clazz);
            PropertyDescriptor[] propertyDescriptors = info.getPropertyDescriptors();
            for (PropertyDescriptor propertyDescriptor : propertyDescriptors) {
                if (method.equals(propertyDescriptor.getReadMethod())) {
                    return propertyDescriptor.getName();
                }
            }
        } catch (Exception e) {
          log.error("getAttributeName error {}", e.getMessage(), e);
        }
        return null;
    }

    public GdnRestListResponse<DistributionProductResponse> filterProduct(String username,
        GdnRestListRequest request , String status, ProductListRequest productListRequest) throws Exception {
        Map<String, String> additionalParameterMap = getParametersMapForProductRequest(productListRequest);
        additionalParameterMap.put("page", String.valueOf(request.getPage()));
        additionalParameterMap.put("size", String.valueOf(request.getSize()));
        additionalParameterMap.put("status", status);
        URI uri = generateURI(BASE_PATH + ROOT + FILTER_QC_READY_PRODUCT, request.getRequestId(), username,
            additionalParameterMap);
        return invokeGetSummary(uri, DistributionProductResponse.class, APPLICATION_JSON_VALUE);
    }

    public GdnBaseRestResponse rejectProduct(String requestId, String username,
        RejectProductRequest rejectProductRequest) throws Exception {
        URI uri = generateURI(BASE_PATH + ROOT + REJECT_PRODUCT, requestId, username, null);
        return invokePost(uri, RejectProductRequest.class, rejectProductRequest,
            APPLICATION_JSON_VALUE);
    }

    public GdnBaseRestResponse approveProduct(String requestId, String username, String productId)
        throws Exception {
        Map<String, String> additionalParameterMap = new HashMap<String, String>();
        additionalParameterMap.put("productId", productId);
        URI uri = generateURI(BASE_PATH + ROOT + APPROVE_PRODUCT, requestId, username,
            additionalParameterMap);
        return invokePostType(uri, null, Object.class, APPLICATION_JSON_VALUE,
            new TypeReference<GdnBaseRestResponse>() {}, null);
    }
    
    public GdnBaseRestResponse moveFailedProductToQC(String requestId, String username, String productCode)
        throws Exception {
        Map<String, String> additionalParameterMap = new HashMap<String, String>();
        additionalParameterMap.put("productCode", productCode);
        URI uri = generateURI(BASE_PATH + ROOT + MOVE_FAILED_PRODUCT_TO_QC, requestId, username,
            additionalParameterMap);
        return invokePostType(uri, null, Object.class, APPLICATION_JSON_VALUE,
            new TypeReference<GdnBaseRestResponse>() {}, null);
    }

  public GdnRestSingleResponse<MapResponse> countQCProductSummaryByFilter(String requestId,
      String username, ProductListRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = generateURI(BASE_PATH + GET_PRODUCT_SUMMARY_COUNT_QC, requestId, username,
        additionalParameterMap);
    return this.invokePostType(uri, request, ProductListRequest.class, APPLICATION_JSON_VALUE, new
        TypeReference<GdnRestSingleResponse<MapResponse>>() {}, null);
  }

}
