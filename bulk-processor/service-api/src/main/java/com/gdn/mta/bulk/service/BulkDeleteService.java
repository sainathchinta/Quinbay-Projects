package com.gdn.mta.bulk.service;

import java.util.List;
import java.util.Set;

import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;

public interface BulkDeleteService {

  /**
   * Pre processing Bulk Delete Request for Instant Pickup Product before sending data to queue
   *
   * @param storeId must not be blank
   * @param requestId must not be blank
   * @param bulkUpdateProcessDTO must not be null
   * @param accessiblePickupPointCodes can be empty
   */
  void preProcessInstantPickupProductBulkDelete(String storeId, String requestId,
      BulkUpdateProcessDTO bulkUpdateProcessDTO, Set<String> accessiblePickupPointCodes) throws Exception;

  /**
   * Processing Bulk Delete Request for Instant Pickup Product after receiving data from queue
   *
   * @param bulkUpdateQueue must not be null
   */
  void processInstantPickupProductBulkDelete(BulkUpdateQueue bulkUpdateQueue) throws Exception;

  /**
   * Processing Bulk Delete Request for Instant Pickup Product
   *
   * @param bulkUpdateEventModel must not be null
   */
  void processEvent(BulkUpdateEventModel bulkUpdateEventModel) throws Exception;

  /**
   * Notification on instant pickup delete
   *
   * @param bulkProcess
   * @param storeId
   * @throws Exception
   */
  void setFinalStatusAndNotificationOnInstantPickupDelete(BulkProcess bulkProcess, String storeId) throws Exception;
}
