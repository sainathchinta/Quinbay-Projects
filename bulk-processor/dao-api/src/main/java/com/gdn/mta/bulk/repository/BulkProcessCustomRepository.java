package com.gdn.mta.bulk.repository;

import java.util.List;

import com.gdn.mta.bulk.entity.BulkProcess;

public interface BulkProcessCustomRepository {

  /**
   * find bulk process based on
   *
   * @param orderByRow
   * @param limit
   * @param storeId
   * @param bulkProcessType
   * @return
   */
  List<BulkProcess> findBlpToProcess(String storeId, boolean orderByRow, int limit, String status, String bulkProcessType);
}
