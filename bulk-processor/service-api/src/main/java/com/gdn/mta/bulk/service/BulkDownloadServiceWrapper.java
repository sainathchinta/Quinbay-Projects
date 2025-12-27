package com.gdn.mta.bulk.service;

import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;

public interface BulkDownloadServiceWrapper {

  /**
   * Download and send vendor products to emails added in system config
   *
   * @param storeId
   * @param filterSummaryRequest
   * @param requestId
   * @param username
   * @throws Exception
   */
  void downloadAndSendVendorProductMail(String storeId, FilterSummaryRequest filterSummaryRequest,
      String requestId, String username) throws Exception;
}
