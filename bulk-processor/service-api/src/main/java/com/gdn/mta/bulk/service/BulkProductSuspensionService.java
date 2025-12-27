package com.gdn.mta.bulk.service;

import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Page;

import com.gdn.mta.bulk.dto.product.BulkProductSuspensionRequest;
import com.gdn.mta.bulk.entity.BulkInternalEventModel;
import com.gdn.mta.bulk.entity.BulkInternalProcess;

public interface BulkProductSuspensionService {

  /**
   * process bulk suspension product request
   *
   * @param productSuspensionRequest
   * @throws Exception
   */
  void process(BulkProductSuspensionRequest productSuspensionRequest) throws Exception;

  /**
   * Api to save the data for bulk suspension
   *
   * @param bulkInternalProcess
   * @param productDataFromExcel
   * @param requestId
   * @param actionType
   * @return
   */
  void generateBulkProcessData(BulkInternalProcess bulkInternalProcess, List<Map<String, String>> productDataFromExcel,
      String requestId, String actionType) throws Exception;

  /**
   * process bulk suspension product request
   *
   * @param bulkInternalEventModel
   * @throws Exception
   */
  void processSuspensionEvent(BulkInternalEventModel bulkInternalEventModel) throws Exception;

  /**
   * Notification on bulk suspension
   *
   * @param bulkInternalProcess
   * @param storeId
   * @throws Exception
   */
  void setFinalStatusAndNotificationOnSuspension(BulkInternalProcess bulkInternalProcess, String storeId)
      throws Exception;

  /**
   * Publish data for bulk suspension
   *
   * @param storeId
   * @param requestId
   * @param bulkProcess
   */
  void processToPublishForSuspension(String storeId, String requestId, Page<BulkInternalProcess> bulkProcess);
}
