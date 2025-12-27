package com.gdn.partners.product.analytics.service.helper;

import com.gdn.partners.product.analytics.model.BigQueryRequestInfo;

import java.util.List;

public interface BigQueryHelper {

  /**
   * Query bigQuery and get file containing results
   * @param bigQueryRequestInfo
   * @throws InterruptedException
   */
  void submitQueryToGCPAndDownloadResultsToFile(BigQueryRequestInfo bigQueryRequestInfo, List<String> localFilePathList)
      throws Exception;

}
