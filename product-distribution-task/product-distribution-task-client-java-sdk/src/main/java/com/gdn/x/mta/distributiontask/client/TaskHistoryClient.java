package com.gdn.x.mta.distributiontask.client;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.web.client.GdnBaseRestCrudClient;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.mta.distributiontask.client.util.TaskHistoryClientPath;
import com.gdn.x.mta.distributiontask.rest.model.response.TaskHistoryResponse;

public class TaskHistoryClient extends GdnBaseRestCrudClient {
  public TaskHistoryClient(GdnRestClientConfiguration clientConfig, String contextPath) {
    super(clientConfig);
    this.setContextPath(contextPath);
  }
  
  public TaskHistoryClient(String username, String password, String host, Integer port,
      String clientId, String channelId, String storeId, String contextPath) {
    super(username, password, host, port, clientId, channelId, storeId, contextPath);
  }

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

  public GdnRestListResponse<TaskHistoryResponse> getProductHistories(String requestId,
      String username, Integer page, Integer size, String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(TaskHistoryClientPath.PAGE, String.valueOf(page));
    additionalParameterMap.put(TaskHistoryClientPath.SIZE, String.valueOf(size));
    additionalParameterMap.put(TaskHistoryClientPath.PRODUCT_CODE, String.valueOf(productCode));
    URI uri = generateURI(
        TaskHistoryClientPath.BASE_PATH
            + TaskHistoryClientPath.TASK_HISTORY_SUMMARY_BY_PRODUCT_CODE,
            generateRequestId(requestId), username, additionalParameterMap);
    return invokeGetSummary(uri, TaskHistoryResponse.class,
        TaskHistoryClientPath.APPLICATION_JSON_VALUE, null);
  }
  
  public GdnRestListResponse<TaskHistoryResponse> getProductHistoriesByTaskCode(String requestId,
      String username, Integer page, Integer size, String taskCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(TaskHistoryClientPath.PAGE, String.valueOf(page));
    additionalParameterMap.put(TaskHistoryClientPath.SIZE, String.valueOf(size));
    additionalParameterMap.put(TaskHistoryClientPath.TASK_CODE, String.valueOf(taskCode));
    URI uri = generateURI(
        TaskHistoryClientPath.BASE_PATH
            + TaskHistoryClientPath.TASK_HISTORY_SUMMARY_BY_TASK_CODE,
            generateRequestId(requestId), username, additionalParameterMap);
    return invokeGetSummary(uri, TaskHistoryResponse.class,
        TaskHistoryClientPath.APPLICATION_JSON_VALUE, null);
  }
}
