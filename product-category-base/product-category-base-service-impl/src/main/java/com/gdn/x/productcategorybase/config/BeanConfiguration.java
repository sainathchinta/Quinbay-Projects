package com.gdn.x.productcategorybase.config;

import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;

import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.impl.HttpClientUtil;
import org.apache.solr.common.params.ModifiableSolrParams;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;
import org.springframework.core.task.support.ExecutorServiceAdapter;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.enums.TracerFieldKey;
import com.google.common.base.Splitter;
import brave.baggage.BaggageField;
import brave.baggage.BaggagePropagation;
import brave.baggage.BaggagePropagationConfig;
import brave.propagation.B3Propagation;
import brave.propagation.Propagation;
import lombok.Data;

/**
 * Created by Vishal on 07/05/18.
 */

@Data
@Configuration
public class BeanConfiguration {
  public static final String STORE_ID = "10001";
  public static final String CHANNEL_ID = "api";
  public static final String CLIENT_ID = "pcb";
  public static final String SEPARATOR = ";";
  public static final String SLASH = "/";
  public static final String DEFAULT_HTTP_PARAMS = "maxConnections/20;maxConnectionsPerHost/10";

  // Dev1 solr cloud url : localhost:9983
  @Value("${solr.cloud.urls}")
  private String solrCloudUrls;

  // Collection Name : brand_collection
  @Value("${solr.brand.collection}")
  private String brandCollection;

  @Value("${solr.brandCollection.connection.timeout}")
  private String brandCollectionConnectionTimeout;

  @Value("${solr.brandCollection.http.params}")
  private String brandCollectionHttpParams;

  // Collection Name : pcb_collection1
  @Value("${solr.mta.collection}")
  private String pcbCollection;

  @Value("${solr.pcbCollection.connection.timeout}")
  private String pcbCollectionConnectionTimeout;

  @Value("${solr.pcbCollection.http.params}")
  private String pcbCollectionHttpParams;

  @Value("${solr.zkConnection.timeout}")
  private String zkConnectionTimeout;

  @Value("${solr.zkClient.timeout}")
  private String zkClientTimeout;

  @Value("${solr.so.timeout}")
  private String soTimeout;

  @Value("${thread.queue.size}")
  private Integer ThreadQueueSize;

  @Value("${thread.pool.size}")
  private Integer ThreadPoolSize;

  @Autowired
  private ObjectMapper objectMapper;

  @Bean(name = "brandCollectionClient")
  public CloudSolrClient brandCollectionClient() {
    CloudSolrClient cloudSolrClient =
        new CloudSolrClient.Builder(Arrays.asList(solrCloudUrls.split(",")), Optional.empty()).withConnectionTimeout(
                Integer.parseInt(brandCollectionConnectionTimeout))
            .withHttpClient(HttpClientUtil.createClient(getSolrParams(brandCollectionHttpParams)))
            .withSocketTimeout(Integer.parseInt(soTimeout)).build();
    cloudSolrClient.setDefaultCollection(brandCollection);
    cloudSolrClient.setZkConnectTimeout(Integer.valueOf(zkConnectionTimeout));
    cloudSolrClient.setZkClientTimeout(Integer.valueOf(zkClientTimeout));
    return cloudSolrClient;
  }

  @Bean(name = "pcbCollectionClient")
  public CloudSolrClient pcbCollectionClient() {
    CloudSolrClient cloudSolrClient =
        new CloudSolrClient.Builder(Arrays.asList(solrCloudUrls.split(",")), Optional.empty()).withConnectionTimeout(
                Integer.parseInt(pcbCollectionConnectionTimeout))
            .withHttpClient(HttpClientUtil.createClient(getSolrParams(pcbCollectionHttpParams)))
            .withSocketTimeout(Integer.parseInt(soTimeout)).build();
    cloudSolrClient.setDefaultCollection(pcbCollection);
    cloudSolrClient.setZkConnectTimeout(Integer.valueOf(zkConnectionTimeout));
    cloudSolrClient.setZkClientTimeout(Integer.valueOf(zkClientTimeout));
    return cloudSolrClient;
  }

  @Bean(name = "solrClient")
  public SolrClient solrClient() {
    return new CloudSolrClient.Builder().withZkHost(Arrays.asList(solrCloudUrls.split(","))).build();
  }

  @Bean
  public ModelMapper modelMapper() {
    return new ModelMapper();
  }

  @Bean
  public Propagation.Factory extraFieldPropagation() {
    BaggagePropagation.FactoryBuilder factoryBuilder = BaggagePropagation.newFactoryBuilder(B3Propagation.FACTORY);
    TracerFieldKey.getKeys().stream().map(BaggageField::create).map(BaggagePropagationConfig.SingleBaggageField::local)
        .forEach(factoryBuilder::add);
    return factoryBuilder.build();
  }

  @Bean
  public ExecutorService executorService(TaskExecutor taskExecutor) {
    return new ExecutorServiceAdapter(taskExecutor);
  }

  @Bean
  public ThreadPoolExecutor threadPoolExecutor(TaskExecutor taskExecutor) {
    return ((ThreadPoolTaskExecutor) taskExecutor).getThreadPoolExecutor();
  }

  @Bean
  public TaskExecutor taskExecutor() {
    ThreadPoolTaskExecutor taskExecutor = new ThreadPoolTaskExecutor();
    taskExecutor.setQueueCapacity(getThreadQueueSize());
    taskExecutor.setCorePoolSize(getThreadPoolSize());
    taskExecutor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
    return taskExecutor;
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
