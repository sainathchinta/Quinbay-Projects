package com.gdn.x.product.dao.api;

import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.model.entity.ProductL3SolrReindexStatus;

import java.util.List;

public interface ProductL3SolrReindexStatusRepositoryCustom {

  /**
   * Custom JPA query - update multi with product reindex status
   *
   * @param productSkuList
   * @param productReindexStatus
   */
  void updateProductReindexStatusByProductSku(List<String> productSkuList,
      ProductReindexStatus productReindexStatus);

  /**
   * Custom JPA Query - fetch by storeid, reindex status with limit on fetch
   *
   * @param storeId
   * @param productReindexStatus
   * @param limit
   * @return
   */
  List<ProductL3SolrReindexStatus> findByStoreIdAndProductReindexStatusAndMarkForDeleteFalseLimit(
      String storeId, ProductReindexStatus productReindexStatus, int limit);
}
