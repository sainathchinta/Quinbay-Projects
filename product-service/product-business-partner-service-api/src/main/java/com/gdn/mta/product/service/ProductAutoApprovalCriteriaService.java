package com.gdn.mta.product.service;

import com.gdn.mta.product.entity.ProductAutoApprovalCriteria;

public interface ProductAutoApprovalCriteriaService {

  /**
   *
   * @param productAutoApprovalCriteria
   */
  void saveProductAutoApprovalCriteria(ProductAutoApprovalCriteria productAutoApprovalCriteria);

  /**
   * delete entries from product auto approve criteria repo by storeId and productCode
   * @param storeId
   * @param productCode
   */
  void deleteProductAutoApprovalCriteriaByStoreIdAndProductCode(String storeId, String productCode);

}
