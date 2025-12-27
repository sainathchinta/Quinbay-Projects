package com.gdn.x.mta.distributiontask.service.api;

public interface ProductsPermanentDeleteService {

  /**
   * to delete all table data for product code
   *
   * @param productCode
   * @param productId
   * @param storeId
   */
  void deleteProducts(String productCode, String productId, String storeId);
}