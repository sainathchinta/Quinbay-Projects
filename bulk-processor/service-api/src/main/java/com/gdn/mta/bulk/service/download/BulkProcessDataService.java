package com.gdn.mta.bulk.service.download;

import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;

/**
 * Created by keshashah on 25/10/16.
 */
public interface BulkProcessDataService {

  /**
   * Get Data for Different Types of request
   *
   * @param <>
   * @param request
   * @return
   */
  BulkDataResponse getData(BulkDownloadRequest request) throws Exception;
}
