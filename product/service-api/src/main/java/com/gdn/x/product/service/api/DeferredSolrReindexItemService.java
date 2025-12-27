package com.gdn.x.product.service.api;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.model.entity.DeferredSolrReindexItem;

public interface DeferredSolrReindexItemService {

  /**
   *
   * @param deferredSolrReindexItems
   * @return
   */
  List<DeferredSolrReindexItem> save(List<DeferredSolrReindexItem> deferredSolrReindexItems);

  /**
   *
   * @param deferredSolrReindexItem
   * @return
   */
  DeferredSolrReindexItem save(DeferredSolrReindexItem deferredSolrReindexItem);

  /**
   * s
   * @param storeId
   * @param pageable
   * @return
   */
  Page<DeferredSolrReindexItem> findByStoreId(String storeId, Pageable pageable);


  /**
   * Get DeferredSolrReindexItem by reindex type and status
   *
   * @param storeId
   * @param reindexType
   * @param pageable
   * @return
   */
  List<DeferredSolrReindexItem> findByStoreIdAndReindexTypeAndProductReindexStatus(String storeId, String reindexType,
      ProductReindexStatus productReindexStatus, Pageable pageable);

  /**
   * Get DeferredSolrReindexItem by item skus
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<DeferredSolrReindexItem> findByStoreIdAndItemSkuIn(String storeId, List<String> itemSkus);

}
