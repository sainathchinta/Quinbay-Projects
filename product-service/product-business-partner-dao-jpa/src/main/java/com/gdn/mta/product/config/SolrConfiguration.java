package com.gdn.mta.product.config;

import java.util.Arrays;
import java.util.Optional;

import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.impl.HttpClientUtil;
import org.apache.solr.common.params.ModifiableSolrParams;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Splitter;
import lombok.Data;

@Configuration
@Data
public class SolrConfiguration {

  public static final String SEPARATOR = ";";
  public static final String SLASH = "/";
  public static final String DEFAULT_HTTP_PARAMS = "maxConnections/20;maxConnectionsPerHost/10";

  @Value("${solr.cloud.urls}")
  private String solrCloudUrls;

  // Collection Name : review_product_collection
  @Value("${solr.review.product.collection}")
  private String reviewProductCollection;

  @Value("${solr.prd_collection_core}")
  private String prdCollection;

  @Value("${solr.history.collection}")
  private String historyCollection;

  @Value("${solr.zkConnection.timeout}")
  private String zkConnectionTimeout;

  @Value("${solr.zkClient.timeout}")
  private String zkClientTimeout;

  @Value("${solr.so.timeout}")
  private String soTimeout;

  @Value("${solr.prdCollectionZk.Connection.timeout}")
  private String prdCollectionZkConnectionTimeout;

  @Value("${solr.prdCollection.connection.timeout}")
  private String prdCollectionConnectionTimeout;

  @Value("${solr.prdCollection.Client.timeout}")
  private String prdCollectionClientTimeout;

  @Value("${solr.prdCollection.so.timeout}")
  private String prdCollectionSoTimeout;

  @Value("${solr.historyCollectionZk.Connection.timeout}")
  private String historyCollectionZkConnectionTimeout;

  @Value("${solr.historyCollection.connection.timeout}")
  private String historyCollectionConnectionTimeout;

  @Value("${solr.historyCollection.zkClient.timeout}")
  private String historyCollectionClientTimeout;

  @Value("${solr.historyCollection.so.timeout}")
  private String historyCollectionSoTimeout;

  @Value("${solr.reviewProductCollection.connection.timeout}")
  private String reviewProductCollectionConnectionTimeout;

  @Value("${solr.prdCollection.http.params}")
  private String prdCollectionHttpParams;

  @Value("${solr.historyCollection.http.params}")
  private String historyCollectionHttpParams;

  @Value("${solr.reviewProductCollection.http.params}")
  private String reviewProductCollectionHttpParams;

  @Autowired
  private ObjectMapper objectMapper;

  @Bean(name = "reviewProductCollectionClient")
  public CloudSolrClient reviewProductCollectionClient() {
    CloudSolrClient cloudSolrClient =
        new CloudSolrClient.Builder(Arrays.asList(solrCloudUrls.split(",")), Optional.empty()).withConnectionTimeout(
                Integer.parseInt(reviewProductCollectionConnectionTimeout))
            .withHttpClient(HttpClientUtil.createClient(getSolrParams(reviewProductCollectionHttpParams)))
            .withSocketTimeout(Integer.parseInt(soTimeout)).build();
    cloudSolrClient.setDefaultCollection(reviewProductCollection);
    cloudSolrClient.setZkConnectTimeout(Integer.valueOf(zkConnectionTimeout));
    cloudSolrClient.setZkClientTimeout(Integer.valueOf(zkClientTimeout));
    return cloudSolrClient;
  }

  @Bean(name = "prdCollectionClient")
  public CloudSolrClient prdCollectionClient() {
    CloudSolrClient cloudSolrClient =
        new CloudSolrClient.Builder(Arrays.asList(solrCloudUrls.split(",")), Optional.empty()).withConnectionTimeout(
                Integer.parseInt(prdCollectionConnectionTimeout))
            .withHttpClient(HttpClientUtil.createClient(getSolrParams(prdCollectionHttpParams)))
            .withSocketTimeout(Integer.parseInt(prdCollectionSoTimeout)).build();
    cloudSolrClient.setDefaultCollection(prdCollection);
    cloudSolrClient.setZkConnectTimeout(Integer.valueOf(prdCollectionZkConnectionTimeout));
    cloudSolrClient.setZkClientTimeout(Integer.valueOf(prdCollectionClientTimeout));
    return cloudSolrClient;
  }

  @Bean(name = "historyCollectionClient")
  public CloudSolrClient historyCollectionClient() {
    CloudSolrClient cloudSolrClient =
        new CloudSolrClient.Builder(Arrays.asList(solrCloudUrls.split(",")), Optional.empty()).withConnectionTimeout(
                Integer.parseInt(historyCollectionConnectionTimeout))
            .withHttpClient(HttpClientUtil.createClient(getSolrParams(historyCollectionHttpParams)))
            .withSocketTimeout(Integer.parseInt(historyCollectionSoTimeout)).build();
    cloudSolrClient.setDefaultCollection(historyCollection);
    cloudSolrClient.setZkConnectTimeout(Integer.valueOf(historyCollectionZkConnectionTimeout));
    cloudSolrClient.setZkClientTimeout(Integer.valueOf(historyCollectionClientTimeout));
    return cloudSolrClient;
  }

  private ModifiableSolrParams getSolrParams(String solrHttpParams) {
    ModifiableSolrParams params = new ModifiableSolrParams();
    String solrParams = Optional.ofNullable(solrHttpParams).orElse(DEFAULT_HTTP_PARAMS);
    if (org.apache.commons.lang3.StringUtils.isNotBlank(solrParams)) {
      Splitter.on(SEPARATOR).withKeyValueSeparator(SLASH).split(solrParams).forEach(params::add);
    }
    return params;
  }
}
