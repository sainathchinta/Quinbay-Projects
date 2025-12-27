package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.entity.Product;

public interface ProductArchivalService {

  /**
   * copy product details to archive table
   * @param product
   */
  void copyProductDetailsToArchiveTablesAndHardDeleteProduct(Product product) throws Exception;

  /**
   * delete data from archival table
   * @param storeId
   */
  void deleteDataFromArchivalTable(String storeId) throws Exception;
}
