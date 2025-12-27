package com.gdn.mta.bulk.service;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.dto.BulkProcessStatusListingResponse;

import java.util.List;

public interface BulkProcessServiceWrapper {

  /**
   * Api to process ready to process data
   *
   * @param storeId
   * @param bulkProcessType
   */
  void processReadyToProcessData(String storeId, String bulkProcessType) throws Exception;

  /**
   * Api to process processed data
   *
   * @param storeId
   * @param bulkProcessType
   */
  void processProcessedData(String storeId, String bulkProcessType) throws Exception;

  /**
   * Api to abort struck process by process types
   *
   * @param storeId must not be null
   * @param bulkProcessTypes list of process types
   */
  void abortStruckProcessesByProcessTypes(String storeId, List<String> bulkProcessTypes);
}
