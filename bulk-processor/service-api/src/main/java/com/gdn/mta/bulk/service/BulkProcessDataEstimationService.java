package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.mta.bulk.entity.BulkProcessDataEstimation;

import java.util.List;

public interface BulkProcessDataEstimationService {


  /**
   * Populates the bulk data estimation DB with estimations for processing of Each Row/Record and
   * for each process
   *
   * @param storeId 10001
   * @param username username
   * @throws JsonProcessingException json parsing
   */
  void updateBulkDataEstimationByProcessTypes(String storeId, String username)
    throws JsonProcessingException;

  /**
   * Fetch Bulk Process Data Estimation by process Type and process level fetch
   * @param processType process type For bulk creation and updates
   */
  List<BulkProcessDataEstimation> fetchAllEstimationResponsesByProcessTypes(List<String> processType);
}
