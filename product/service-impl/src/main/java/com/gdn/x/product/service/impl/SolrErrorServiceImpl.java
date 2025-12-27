package com.gdn.x.product.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gdn.x.product.dao.api.SolrErrorHistoryRepository;
import com.gdn.x.product.model.entity.SolrErrorHistory;
import com.gdn.x.product.service.api.SolrErrorService;

@Service
public class SolrErrorServiceImpl implements SolrErrorService {

  @Autowired
  private SolrErrorHistoryRepository solrErrorHistoryRepository;

  @Override
  public Page<SolrErrorHistory> findTopData(String storeId, int top) {
    return this.solrErrorHistoryRepository.findByStoreId(storeId, PageRequest.of(0, top));
  }

  @Override
  public void save(SolrErrorHistory solrErrorHistory) {
    this.solrErrorHistoryRepository.save(solrErrorHistory);
  }

}
