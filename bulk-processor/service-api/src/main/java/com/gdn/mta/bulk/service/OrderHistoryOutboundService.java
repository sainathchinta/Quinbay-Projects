package com.gdn.mta.bulk.service;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.models.download.responsedata.OfflinePackageSummaryResponse;
import com.gdn.mta.bulk.models.download.responsedata.OrderItemSummaryResponse;
import com.gdn.x.neo.order.client.sdk.web.model.request.OrderItemSummaryRequest;

public interface OrderHistoryOutboundService {

  /**
   * @param storeId
   * @param requestId
   * @param username
   * @param orderItemSummaryRequest
   * @param page
   * @param size
   * @return
   */
  GdnRestListResponse<OrderItemSummaryResponse> getOrderItemsForBulkDownload(String storeId,
      String requestId, String username, OrderItemSummaryRequest orderItemSummaryRequest, int page,
      int size);

  /**
   * find offline order summary from solr
   * @param requestId
   * @param username
   * @param storeId
   * @param orderRequest
   * @param orderBy
   * @param sortBy
   * @param page
   * @param size
   * @return
   */
  GdnRestListResponse<OfflinePackageSummaryResponse> findOfflinePackageSummary(String storeId, String requestId,
      String username, OrderItemSummaryRequest orderRequest, String orderBy, String sortBy, int page, int size);
}
