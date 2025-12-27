package com.gdn.x.product.service.api;

import org.springframework.data.domain.Page;

import com.gdn.x.product.model.entity.SolrErrorHistory;

public interface SolrErrorService {

  Page<SolrErrorHistory> findTopData(String storeId, int top);

  void save(SolrErrorHistory solrErrorHistory);

}
