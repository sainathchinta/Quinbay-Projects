package com.gdn.mta.product.service;

import com.gdn.mta.product.entity.ProductCollection;

public interface TerminatedSellerSkuCleanupService {

  /**
   * data cleanup for terminated sellerCode for given product code
   *
   * @param productCode
   * @return
   * @throws Exception
   */
  boolean terminatedSellerSkuCleanup(ProductCollection productCollection) throws Exception;
}
