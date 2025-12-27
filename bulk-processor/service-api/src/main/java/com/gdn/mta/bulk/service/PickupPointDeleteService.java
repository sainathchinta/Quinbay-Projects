package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.dto.PickupPointDeleteProcessDTO;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;

public interface PickupPointDeleteService {

  void processDeletePickupPointEvent(PickupPointDeleteProcessDTO pickupPointDeleteProcessDTO) throws Exception;

  /**
   * to delete L5's of a product in x-product and PBP
   *
   * @param bulkUpdateEventModel
   * @throws Exception
   */
  void processDeleteItemPickupPointEvent(BulkUpdateEventModel bulkUpdateEventModel) throws Exception;

  /**
   * process pending delete pickup point event
   *
   * @param storeId
   * @param bulkProcessType
   */
  void processPendingDeletePickupPointEvent(String storeId, String bulkProcessType);
}
