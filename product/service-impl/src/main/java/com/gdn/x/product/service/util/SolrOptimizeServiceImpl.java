package com.gdn.x.product.service.util;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.product.outbound.api.ProductSolrOutbound;

@Service
public class SolrOptimizeServiceImpl implements LuceneOptimizeService {

  @Autowired
  private ProductSolrOutbound productSolrOutbound;

  @Override
  public void optimize() {
    this.productSolrOutbound.optimize();
  }

}
