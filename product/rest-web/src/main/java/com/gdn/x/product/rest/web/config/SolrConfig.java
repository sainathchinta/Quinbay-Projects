package com.gdn.x.product.rest.web.config;

import com.gdn.x.product.rest.web.properties.SolrProperties;
import com.google.common.base.Splitter;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.impl.HttpClientUtil;
import org.apache.solr.common.params.ModifiableSolrParams;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import java.util.Arrays;
import java.util.Optional;

import static com.gdn.x.product.enums.SolrConstants.SEPARATOR;

@Configuration
@EnableAutoConfiguration
public class SolrConfig {

  private final SolrProperties solrProperties;

  @Autowired
  public SolrConfig(SolrProperties solrProperties) {
    this.solrProperties = solrProperties;
  }


  @Bean(name = "xproductClient")
  public CloudSolrClient cloudSolrClient() {
    CloudSolrClient cloudSolrClient =
      new CloudSolrClient.Builder(Arrays.asList(solrProperties.getSolrCloudUrls().split(",")),
        Optional.empty()).withConnectionTimeout(
          Integer.parseInt(solrProperties.getConnectionTimeout())).withHttpClient(HttpClientUtil.createClient(getSolrParams(
          solrProperties.getHttpParams())))
        .withSocketTimeout(Integer.parseInt(solrProperties.getSoTimeout())).build();
    cloudSolrClient.setDefaultCollection(solrProperties.getXproductCollection());
    cloudSolrClient.setZkClientTimeout(Integer.parseInt(solrProperties.getZkClientTimeout()));
    cloudSolrClient.setZkConnectTimeout(Integer.parseInt(solrProperties.getZkClientTimeout()));
    return cloudSolrClient;
  }

  @Bean(name = "xproductL3Client")
  public CloudSolrClient l3CloudSolrClient() {
    CloudSolrClient cloudSolrClient =
      new CloudSolrClient.Builder(Arrays.asList(solrProperties.getSolrCloudUrlsL3().split(",")), Optional.empty()).withConnectionTimeout(
          Integer.parseInt(solrProperties.getConnectionTimeoutL3()))
        .withHttpClient(HttpClientUtil.createClient(getSolrParams(solrProperties.getHttpParamsL3())))
        .withSocketTimeout(Integer.parseInt(solrProperties.getSoTimeoutL3())).build();
    cloudSolrClient.setDefaultCollection(solrProperties.getXproductL3Collection());
    cloudSolrClient.setZkClientTimeout(Integer.parseInt(solrProperties.getZkClientTimeoutL3()));
    cloudSolrClient.setZkConnectTimeout(Integer.parseInt(solrProperties.getZkConnectionTimeoutL3()));
    return cloudSolrClient;
  }

  private ModifiableSolrParams getSolrParams(String solrHttpParams) {
    ModifiableSolrParams params = new ModifiableSolrParams();
    String solrParams =
      Optional.ofNullable(solrHttpParams).orElse(BeanConfig.DEFAULT_HTTP_PARAMS);
    if (StringUtils.isNotBlank(solrParams)) {
      Splitter.on(SEPARATOR).withKeyValueSeparator(BeanConfig.SLASH).split(solrParams).forEach(params::add);
    }
    return params;
  }
}
