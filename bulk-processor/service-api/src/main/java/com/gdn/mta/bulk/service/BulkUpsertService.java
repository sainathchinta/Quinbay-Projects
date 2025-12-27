package com.gdn.mta.bulk.service;

import java.util.Set;

import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;

public interface BulkUpsertService {

  /**
   * Pre processing Bulk Upsert Request for Product Instant Pickup before sending data to Queue
   *
   * @param storeId must not be blank
   * @param requestId must not be blank
   * @param bulkUpdateProcessDTO must not be null
   * @param accessiblePickupPoints accessible pickup points for the user, can be empty for ALL access
   */
  void preProcessInstantPickupProductBulkUpsert(String storeId, String requestId,
    BulkUpdateProcessDTO bulkUpdateProcessDTO, Set<String> accessiblePickupPoints) throws Exception;

  /**
   * Processing Bulk Upsert Request for Product Instant Pickup before sending data to Queue
   *
   * @param bulkUpdateQueue must not be null
   * @throws Exception
   */
  void processInstantPickupProductBulkUpsert(BulkUpdateQueue bulkUpdateQueue) throws Exception;

  /**
   * Processing Bulk Upsert Request for Product Instant Pickup items
   *
   * @param bulkUpdateEventModel must not be null
   * @throws Exception
   */
  void processEvent(BulkUpdateEventModel bulkUpdateEventModel) throws Exception;

  /**
   * Notification on instant pickup upsert
   *
   * @param bulkProcess
   * @param storeId
   * @throws Exception
   */
  void setFinalStatusAndNotificationOnInstantPickupUpsert(BulkProcess bulkProcess, String storeId) throws Exception;

}
