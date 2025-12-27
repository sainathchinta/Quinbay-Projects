package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.models.WorkOrderDataModel;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;

public interface BulkWorkOrderService {


  /**
   * Process assembly disassembly request
   *
   * @param storeId                 must not be null
   * @param merchantType            must not be null
   * @param itemSku                 must not be null
   * @param productSku              must not be null
   * @param warehouseCode           must not be null
   * @param isInternationalMerchant boolean
   * @param workOrderType           must not be null
   * @param bulkProcess             must not be null
   * @param bulkProcessData         must not be null
   * @param workOrderDataModel      must not be null
   * @param productBasicDetails     must not be null
   */
  void validateAndProcessAssemblyDisassemblyRequest(String storeId, String merchantType, String itemSku,
      String productSku, String warehouseCode, boolean isInternationalMerchant, String workOrderType,
      BulkProcess bulkProcess, BulkProcessData bulkProcessData, WorkOrderDataModel workOrderDataModel,
      BasicProductResponse productBasicDetails);

  /**
   * Process assembly disassembly request
   *
   * @param storeId                 must not be null
   * @param merchantType            must not be null
   * @param itemSku                 must not be null
   * @param productSku              must not be null
   * @param warehouseCode           must not be null
   * @param isInternationalMerchant boolean
   * @param bulkProcess             must not be null
   * @param bulkProcessData         must not be null
   * @param workOrderDataModel      must not be null
   * @param productBasicDetails     must not be null
   */
  void validateAndProcessTransferRequest(String storeId, String merchantType, String itemSku, String productSku,
      String warehouseCode, boolean isInternationalMerchant, BulkProcess bulkProcess, BulkProcessData bulkProcessData,
      WorkOrderDataModel workOrderDataModel, BasicProductResponse productBasicDetails);
}
