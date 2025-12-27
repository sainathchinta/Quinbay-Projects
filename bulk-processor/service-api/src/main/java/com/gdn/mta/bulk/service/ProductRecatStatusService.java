package com.gdn.mta.bulk.service;

import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Page;

import com.gdn.mta.bulk.dto.RecatProductSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProductSummaryResponse;
import com.gdn.mta.bulk.entity.ProductRecatStatus;

public interface ProductRecatStatusService {

  /**
   * Save recat process list
   *
   * @param recatProcessList
   */
  void saveProductRecatStatusList(List<ProductRecatStatus> recatProcessList);

  /**

   * get count of product status wise by recat request code
   * @param storeId
   * @param recatRequestCode
   * @return
   */
  Map<String, Integer> getProductCountByRecatRequestCode(String storeId, String recatRequestCode);

  /**
   * Find productRecatStatus by store id, recat requestCode and status
   *
   * @param storeId
   * @param status
   * @param batchSize
   * @return
   */
  List<ProductRecatStatus> findProductRecatStatusByStoreIdAndAndStatus(String storeId, String status, int batchSize);

  /**
   * get recat product summary
   * @param storeId
   * @param recatRequestCode
   * @param recatProductSummaryRequest
   * @param page
   * @param size
   * @return
   */
  Page<RecatProductSummaryResponse> getRecatProductSummary(String storeId, String recatRequestCode,
      RecatProductSummaryRequest recatProductSummaryRequest, int page, int size);

  /**
   * Find productRecatStatus by Id
   *
   * @param id
   * @param status
   * @return
   */
  ProductRecatStatus findByIdAndStatus(String id, String status);

  /**
   * Update product category
   *
   * @param productRecatStatus
   */
  String updateProductCategory(ProductRecatStatus productRecatStatus);

  /**
   * Validate response and save
   *
   * @param productRecatStatus
   * @param errorMessage
   */
  void validateResponseAndSave(ProductRecatStatus productRecatStatus, String errorMessage);

  /**
   * Find product count by status and recat request code
   *
   * @param storeId
   * @param status
   * @param recatRequestCode
   * @return
   */
  int findCountByStoreIdAndStatusAndRecatRequestCode(String storeId, String status,
      String recatRequestCode);

  /**
   * Find productRecatStatus by storeId, status and recatRequestCode
   *
   * @param storeId
   * @param status
   * @param recatRequestCode
   * @return
   */
  List<ProductRecatStatus> findByStoreIdAndStatusAndRecatRequestCode(String storeId, String status, String recatRequestCode);

  /**
   * Get status count by storeId and recatRequestCode
   *
   * @return
   * @param storeId
   * @param recatRequestCode
   */
  Map<String, Integer> getStatusCountByStoreIdAndRecatRequestCount(String storeId, String recatRequestCode);
}
