package com.gdn.x.product.rest.web.properties;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
@Getter
public class SolrProperties {

  @Value("${solr.cloud.urls}")
  private String solrCloudUrls;

  @Value("${solr.cloud.urls.l3}")
  private String solrCloudUrlsL3;

  @Value("${solr.xproduct.collection}")
  private String xproductCollection;

  @Value("${solr.xproduct.l3.collection}")
  private String xproductL3Collection;

  @Value("${solr.zkConnection.timeout}")
  private String zkConnectionTimeout;

  @Value("${solr.connection.timeout}")
  private String connectionTimeout;

  @Value("${solr.connection.timeout.l3}")
  private String connectionTimeoutL3;

  @Value("${solr.http.params}")
  private String httpParams;

  @Value("${solr.http.params.l3}")
  private String httpParamsL3;

  @Value("${solr.zkClient.timeout}")
  private String zkClientTimeout;

  @Value("${solr.so.timeout}")
  private String soTimeout;

  @Value("${solr.zkConnection.timeout.l3}")
  private String zkConnectionTimeoutL3;

  @Value("${solr.zkClient.timeout.l3}")
  private String zkClientTimeoutL3;

  @Value("${solr.so.timeout.l3}")
  private String soTimeoutL3;
}
