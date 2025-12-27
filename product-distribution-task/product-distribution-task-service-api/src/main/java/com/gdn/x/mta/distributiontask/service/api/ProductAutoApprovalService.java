package com.gdn.x.mta.distributiontask.service.api;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import com.gdn.x.mta.distributiontask.model.ProductAutoApproval;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import com.gdn.x.mta.distributiontask.response.AutoQcConfigChangeDto;


public interface ProductAutoApprovalService {

  /**
   * Add products to auto approval table
   *
   * @param storeId
   * @param productCodes
   * @param productCodeXcategoryCodeMap cnCategory to be non null for updating image QC response
   */
  void addProductsToAutoApprovalTable(String storeId, List<String> productCodes,
    Map<String,String> productCodeXcategoryCodeMap);

  /**
   *
   * @param storeId
   * @param autoApprovalStatus
   * @param maximumAllowedProductAutoApprovals
   * @return
   */
  List<ProductAutoApproval> findProductsToAutoApprovalOrderByCreatedDateAsc(String storeId,
      AutoApprovalStatus autoApprovalStatus, int maximumAllowedProductAutoApprovals);

  /**
   *
   * @param storeId
   * @param autoApprovalStatus
   * @param maximumAllowedProductAutoApprovals
   * @return
   */
  List<ProductAutoApproval> findProductsToAutoApprovalOrderByCreatedDateDesc(String storeId,
      AutoApprovalStatus autoApprovalStatus, int maximumAllowedProductAutoApprovals);

  /**
   *
   * @param storeId
   * @param autoApprovalStatus
   * @param maximumAllowedProductAutoApprovals
   * @return
   */
  List<ProductAutoApproval> findProductsToAutoApprovalOrderByUpdatedDateAsc(String storeId,
      AutoApprovalStatus autoApprovalStatus, int maximumAllowedProductAutoApprovals);

  /**
   *
   * @param storeId
   * @param autoApprovalStatus
   * @param maximumAllowedProductAutoApprovals
   * @return
   */
  List<ProductAutoApproval> findProductsToAutoApprovalOrderByUpdatedDateDesc(String storeId,
      AutoApprovalStatus autoApprovalStatus, int maximumAllowedProductAutoApprovals);


  /**
   *
   * @param productAutoApproval
   */
  void updateProductAutoApprovalDetails(ProductAutoApproval productAutoApproval);

  /**
   * @param storeId
   * @param productCode
   * @param autoApprovalStatus
   * @param isResetCategory
   */
  void updateProductAutoApprovalDetailsByProductCode(String storeId, String productCode,
      AutoApprovalStatus autoApprovalStatus, boolean isResetCategory);

  /**
   * process pending atuch qc config change
   * @param storeId
   */
  void processPendingAutoQcConfigChange(String storeId);

  /**
   * Save AutoQc config changes
   *
   * @param autoQcConfigChangeDto
   */
  void saveAutoQcConfigChanges(AutoQcConfigChangeDto autoQcConfigChangeDto) throws IOException;

}
