package com.gdn.x.product.service.api;

import java.util.List;

import com.gdn.x.product.model.entity.ProductL3SolrReindexStatus;

public interface ProductL3SolrReindexStatusService {

  /**
   * Add product sku to ProductL3SolrReindexStatus
   *
   * @param productL3SolrReindexStatus
   */
  void insertProductSkuToReindexStatusCollection(ProductL3SolrReindexStatus productL3SolrReindexStatus);


  /**
   * Add product skus to ProductL3SolrReindexStatus
   *
   * @param storeId
   * @param productL3SolrReindexStatuses
   */
  void insertProductSkusToReindexStatusCollection(String storeId,
      List<ProductL3SolrReindexStatus> productL3SolrReindexStatuses);
}
