package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.models.download.ChangeAssigneeRequest;
import com.gdn.mta.bulk.models.download.DownloadInReviewAnchorsWebRequest;
import com.gdn.mta.bulk.models.download.responsedata.InReviewAnchorDownloadResponse;

import java.util.List;

public interface MasterSkuReviewOutboundService {
  /**
   * to fetch data for anchor mapping for in-review download
   *
   * @param requestId
   * @param page
   * @param size
   * @param request
   * @return List of InReviewAnchorDownloadResponse
   */
  List<InReviewAnchorDownloadResponse> fetchAnchorMappingDownloadList(String requestId, int page,
    int size, DownloadInReviewAnchorsWebRequest request);

  /**
   * to process bulk upload assignee action
   *
   * @param requestId
   * @param request
   * @return
   */
  String processBulkUploadAssigneeAction(String requestId, ChangeAssigneeRequest request);
}
