package com.gdn.x.productcategorybase.solr.dao;

import com.gdn.x.productcategorybase.solr.model.DeltaProduct;

import java.util.List;

/**
 * Created by Kesha on 26/04/16.
 */
public interface ProductDeltaDao {

  /**
   * API to fetch product list which got updated after last solr reindexing
   *
   * @return
   */
  List<DeltaProduct> fetchUpdatedProducts();

  /**
   * API to update all products to processing fetched for delta reindex
   *
   * @param productItemIds
   * @return
   */
  int updateElementStateToProcessing(List<String> productItemIds);

  /**
   * API to delete all reindexed products from table
   *
   */
  void deleteProcessedElements();

  /**
   * API to update all elements to processing when full reindexing job is running
   *
   */
  void updateAllElementsToProcessing();
}
