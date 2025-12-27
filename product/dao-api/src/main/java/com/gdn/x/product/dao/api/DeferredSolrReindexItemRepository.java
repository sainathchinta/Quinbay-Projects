package com.gdn.x.product.dao.api;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.model.entity.DeferredSolrReindexItem;


public interface DeferredSolrReindexItemRepository extends MongoRepository<DeferredSolrReindexItem, String> {

  Page<DeferredSolrReindexItem> findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDate(
      String storeId, Pageable pageable);

  List<DeferredSolrReindexItem> findByStoreIdAndItemSkuIn(String storeId, List<String> itemSkus);

  List<DeferredSolrReindexItem> findByStoreIdAndReindexTypeAndProductReindexStatusAndMarkForDeleteFalse(
      String storeId, String reindexType, ProductReindexStatus productReindexStatus, Pageable pageable);

}
