package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.models.AutoApprovedAssigneeRequest;
import com.gdn.mta.bulk.models.AutoApprovedSelectedDownloadRequest;
import com.gdn.mta.bulk.models.download.AutoApprovedWebRequest;
import com.gdn.mta.bulk.models.download.responsedata.AutoApprovedListWebResponse;

import java.util.List;

public interface ProductAnalyticsOutboundService {

  /**
   *
   * @param requestId
   * @param page
   * @param size
   * @param request
   * @return auto approved products list
   */
  List<AutoApprovedListWebResponse> fetchAutoApprovedProductsDownloadList(String requestId, int page,
      int size, AutoApprovedWebRequest request);

  /**
   * update the assignee for auto approved products
   *
   * @return error message if present
   */
  String processUpdateAssigneeForAutoApprovedProducts(String requestId, AutoApprovedAssigneeRequest request);

  /**
   *
   * @param requestId
   * @param request
   * @return auto approved products list for selected product code
   */
  List<AutoApprovedListWebResponse> fetchAutoApprovedProductsSelectedDownloadList(String requestId,
      AutoApprovedSelectedDownloadRequest request);

}
