package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkUpdateEventModel;

public interface BulkInstoreUpdateService {
  /**
   *
   * @param bulkUpdateEventModel
   */
  void processInstoreUpdateEvent(BulkUpdateEventModel bulkUpdateEventModel) throws Exception;
}
