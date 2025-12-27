package com.gdn.x.product.outbound.impl;

import java.io.IOException;

import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.x.product.outbound.api.ProductSolrOutbound;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductSolrOutboundImpl implements ProductSolrOutbound {

  @Autowired
  @Qualifier("xproductClient")
  private CloudSolrClient cloudSolrClient;

  @Value("${solr.xproduct.collection}")
  private String collectionName;

  @Override
  public void optimize() {
    try {
      this.cloudSolrClient.optimize(collectionName);
    } catch (SolrServerException | IOException e) {
      log.error(e.getMessage(), e);
    }
  }

}
