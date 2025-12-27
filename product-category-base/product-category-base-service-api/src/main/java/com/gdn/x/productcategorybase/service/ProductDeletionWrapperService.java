package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.entity.Product;

public interface ProductDeletionWrapperService {

  /**
   * publish product which can archived and deleted
   * @param storeId
   */
  void publishProductForDeletion(String storeId);

  /**
   * archive and delete product data by prroduct code
   * @param storeId
   * @param productCode
   */
  void archiveAndDeleteProductData(String storeId, String productCode);

  /**
   * complete product details
   * @param storeId
   * @param productCode
   * @return
   */
  Product getProductDetails(String storeId, String productCode);

  /**
   * hard delete product and clear cache
   * @param storeId
   * @param product
   */
  void hardDeleteProductAndClearCache(String storeId, Product product) throws Exception;
}
