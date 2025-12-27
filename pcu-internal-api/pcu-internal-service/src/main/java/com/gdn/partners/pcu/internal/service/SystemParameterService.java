package com.gdn.partners.pcu.internal.service;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.internal.client.model.request.SystemParameterRequest;
import com.gdn.partners.pcu.internal.client.model.response.ProductSystemParameterResponse;
import com.gdn.partners.pcu.internal.client.model.response.SystemParameterResponse;

import java.util.List;

public interface SystemParameterService {

  /**
   *
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param productSystemParameterRequest
   * @return GdnBaseRestResponse
   */
  void updateSystemParameter(String storeId, String channelId,
      String clientId, String requestId, String username,
      List<SystemParameterRequest> productSystemParameterRequest);

  /**
   *
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @return list of ProductSystemParameterResponse whose showOnUI true
   */
  GdnRestListResponse<ProductSystemParameterResponse> fetchSystemParameterShowOnUI(String storeId,
      String channelId, String clientId, String requestId);

  /**
   * Fetch internal system params
   * @param storeId
   * @param requestId
   * @return
   */
  SystemParameterResponse getSystemParameterSwitches(String storeId, String requestId);
}
