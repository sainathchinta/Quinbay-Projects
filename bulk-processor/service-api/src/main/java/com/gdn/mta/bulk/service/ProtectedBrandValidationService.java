package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;

import java.util.List;
import java.util.Map;

public interface ProtectedBrandValidationService {

  /**
   * Validating protected brand authentication
   * @param brandCode
   * @param bulkProcess
   * @param protectedBrandNameCodeMap
   * @return
   */
  boolean validateProtectedBrandAuthorisation(String brandCode, BulkProcess bulkProcess,
    Map<String, String> protectedBrandNameCodeMap);

  /**
   * Creating protected Brand Name & Code Map
   * @param storeId
   * @return
   */
  Map<String, String> fetchProtectedBrandNameCodeMap(String storeId);

  /**
   *
   * @param row
   * @param bulkProcess
   * @param protectedBrandNameCodeMap
   * @return
   */
  boolean validateProtectedBrand(Map<String, Object> row, BulkProcess bulkProcess,
      Map<String, String> protectedBrandNameCodeMap);

  /**
   * ]
   * @param row
   * @param bulkProcess
   * @param protectedBrandNameCodeMap
   * @return
   */
  boolean validateProtectedBrandForCn(Map<String, Object> row, BulkProcess bulkProcess,
      Map<String, String> protectedBrandNameCodeMap);
}
