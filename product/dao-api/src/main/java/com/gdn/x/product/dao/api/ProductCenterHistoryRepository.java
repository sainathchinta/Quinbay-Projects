package com.gdn.x.product.dao.api;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.ProductCenterHistory;

public interface ProductCenterHistoryRepository extends MongoRepository<ProductCenterHistory, String> {

  Page<ProductCenterHistory> findProductCenterHistoryByStoreIdAndProductSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(
      String storeId, String productSku, Pageable pageable);
}
