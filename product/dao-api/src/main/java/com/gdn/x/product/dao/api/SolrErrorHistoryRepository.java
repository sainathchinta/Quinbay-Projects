package com.gdn.x.product.dao.api;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.SolrErrorHistory;

public interface SolrErrorHistoryRepository extends MongoRepository<SolrErrorHistory, String> {

  Page<SolrErrorHistory> findByStoreId(String storeId, Pageable pageable);

}
