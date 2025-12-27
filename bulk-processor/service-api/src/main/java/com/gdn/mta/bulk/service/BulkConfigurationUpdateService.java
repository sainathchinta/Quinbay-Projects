package com.gdn.mta.bulk.service;

import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Page;

import com.gdn.mta.bulk.dto.product.BulkConfigurationUpdateRequest;
import com.gdn.mta.bulk.entity.BulkInternalEventModel;
import com.gdn.mta.bulk.entity.BulkInternalProcess;

public interface BulkConfigurationUpdateService {

  /**
   * Bulk update for merchant or category configuration
   *
   * @param bulkConfigurationUpdateRequest
   * @throws Exception
   */
  void processConfigurationUpdate(BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest) throws Exception;

  /**
   * Api to save the data for bulk config update
   *
   * @param bulkInternalProcess
   * @param productDataFromExcel
   * @param requestId
   * @param actionType
   * @return
   */
  void generateBulkProcessDataForConfigUpdate(BulkInternalProcess bulkInternalProcess,
      List<Map<String, String>> productDataFromExcel, String requestId, String actionType) throws Exception;

  /**
   * process bulk config update product request
   *
   * @param bulkInternalEventModel
   * @throws Exception
   */
  void processConfigUpdateEvent(BulkInternalEventModel bulkInternalEventModel) throws Exception;

  /**
   * Notification on bulk config update
   *
   * @param bulkInternalProcess
   * @param storeId
   * @throws Exception
   */
  void setFinalStatusAndNotificationOnConfigUpdate(BulkInternalProcess bulkInternalProcess, String storeId)
      throws Exception;

  /**
   * Publish data for bulk config update
   *
   * @param storeId
   * @param requestId
   * @param bulkProcess
   */
  void processToPublishForConfigUpdate(String storeId, String requestId, Page<BulkInternalProcess> bulkProcess);
}
