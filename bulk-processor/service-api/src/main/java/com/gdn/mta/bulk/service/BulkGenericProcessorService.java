package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.entity.BulkCreateProductEventModel;
import com.gdn.mta.bulk.entity.BulkProcess;

import java.util.List;
import java.util.Map;

public interface BulkGenericProcessorService {

  /**
   * Api to process bulk generic event
   *
   * @param bulkCreateProductEventModel
   * @return
   * @throws Exception
   */
  void processBulkGenericEvent(BulkCreateProductEventModel bulkCreateProductEventModel) throws Exception;

  /**
   * Api to save the data and image URLs for generic bulk process
   *
   * @param bulkProcess
   * @param userInputRows
   * @param merchantType
   * @param excelBahasaHeaderList
   * @param excelEnglishHeaderList
   * @param failedExcelRows
   * @param accessiblePickupPoints
   * @param instoreEligible
   * @return
   * @throws Exception
   */
  int generateBulkProcessDataAndImage(BulkProcess bulkProcess, List<List<Object>> userInputRows,
      MerchantStatusType merchantStatusType, String merchantType, List<Object> excelBahasaHeaderList,
      List<Object> excelEnglishHeaderList, List<Integer> failedExcelRows,
      String accessiblePickupPoints, boolean instoreEligible, Map<String, String> args) throws Exception;
}
