package com.gdn.x.mta.distributiontask.config;

import com.gdn.x.mta.distributiontask.dao.api.feign.ProductAnalyticsFeign;
import com.gdn.x.mta.distributiontask.dao.api.feign.XBPFeign;
import com.gdn.x.mta.distributiontask.dao.api.feign.XInventoryFeign;
import feign.Retryer;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.impl.HttpClientUtil;
import org.apache.solr.common.params.ModifiableSolrParams;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.dao.api.feign.PBPFeign;
import com.gdn.x.mta.distributiontask.dao.api.feign.PCBFeign;
import com.gdn.x.mta.distributiontask.dao.api.feign.XProductFeign;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.youtube.YouTube;
import com.google.common.base.Splitter;

import feign.Feign;
import feign.Request;
import feign.form.FormEncoder;
import feign.jackson.JacksonDecoder;
import feign.jackson.JacksonEncoder;
import feign.slf4j.Slf4jLogger;

import java.util.Arrays;
import java.util.Optional;

/**
 * Created by virajjasani on 25/09/16.
 */
@Configuration
public class BeanConfiguration {

  private static final String STORE_ID = "10001";
  private static final String CHANNEL_ID = "WEB";
  public static final String USER_NAME = "PDT-USER";
  private static final String APP_ID = "PDT";
  public static final String SEPARATOR = ";";
  public static final String SLASH = "/";
  public static final String DEFAULT_HTTP_PARAMS = "maxConnections/20;maxConnectionsPerHost/10";

  @Value("${api.pbp.host}")
  private String pbpHost;

  @Value("${api.pbp.port}")
  private Integer pbpPort;

  @Value("${api.pbp.clientid}")
  private String clientId;

  @Value("${solr.connection.timeout}")
  private String connectionTimeout;

  @Value("${solr.http.params}")
  private String httpParams;

  @Value("${api.pbp.context}")
  private String pbpContextPath;

  @Value("${api.pbp.timeout}")
  private Integer pbpTimeout;

  @Value("${api.pcb.feign.host}")
  private String pcbFeignHost;

  @Value("${api.pcb.port}")
  private Integer pcbPort;

  @Value("${api.pcb.timeout}")
  private Integer pcbTimeout;

  @Value("${api.pcb.context}")
  private String pcbContextPath;

  @Value("${api.productAnalytics.feign.host}")
  private String productAnalyticsFeignHost;

  @Value("${api.productAnalytics.port}")
  private Integer productAnalyticsPort;

  @Value("${api.productAnalytics.timeout}")
  private Integer productAnalyticsTimeout;

  @Value("${api.productAnalytics.context}")
  private String productAnalyticsContextPath;

  @Value("${api.x.inventory.feign.host}")
  private String xInventoryFeignHost;

  @Value("${api.x.inventory.port}")
  private Integer xInventoryPort;

  @Value("${api.x.inventory.timeout}")
  private Integer xInventoryTimeout;

  @Value("${api.x.inventory.context}")
  private String xInventoryContextPath;

  @Value("${retry.x.inventory.initialInterval}")
  private Integer xInventoryInitialInterval;

  @Value("${retry.x.inventory.maxPeriod}")
  private Integer xInventoryMaxPeriod;

  @Value("${retry.x.inventory.maxAttempts}")
  private Integer xInventoryMaxAttempts;

  @Value("${api.xbp.feign.host}")
  private String xBPFeignHost;

  @Value("${api.xbp.port}")
  private Integer xBPPort;

  @Value("${api.xbp.timeout}")
  private Integer xBPTimeout;

  @Value("${api.xbp.context}")
  private String xBPContextPath;

  @Value("${solr.vendor.product.collection}")
  private String vendorProductCollection;

  @Value("${solr.ipr.product.collection}")
  private String iprProductCollection;

  @Value("${solr.cloud.urls}")
  private String solrCloudUrls;

  @Value("${solr.zkConnection.timeout}")
  private String zkConnectionTimeout;

  @Value("${solr.zkClient.timeout}")
  private String zkClientTimeout;

  @Value("${solr.so.timeout}")
  private String soTimeout;

  @Value("${api.xProduct.timeout}")
  private Integer xProductTimeout;

  @Value("${api.xProduct.feign.host}")
  private String xProductFeignHost;

  @Value("${api.xProduct.port}")
  private Integer xProductPort;

  @Value("${api.xProduct.context}")
  private String xProductContextPath;

  @Bean
  public static DeserializationProblemHandler deserializationProblemHandler() {
    return new DeserializationProblemHandler();
  }

  @Bean
  public static ObjectMapper objectMapper() {
    return new ObjectMapper().addHandler(BeanConfiguration.deserializationProblemHandler());
  }

  @Bean
  public PCBFeign pcbFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(PCBFeign.class))
        .options(new Request.Options(pcbTimeout, pcbTimeout))
        .target(PCBFeign.class, getPCBUrl());
  }

  private String getPCBUrl() {
    return new StringBuilder(pcbFeignHost).append(Constants.COLON).append(pcbPort)
        .append(Constants.DELIMITER_SLASH).append(pcbContextPath).toString();
  }

  @Bean
  public ProductAnalyticsFeign productAnalyticsFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(ProductAnalyticsFeign.class))
        .options(new Request.Options(productAnalyticsTimeout, productAnalyticsTimeout))
        .target(ProductAnalyticsFeign.class, getProductAnalyticsUrl());
  }

  private String getProductAnalyticsUrl() {
    return new StringBuilder(productAnalyticsFeignHost).append(Constants.COLON).append(productAnalyticsPort)
        .append(Constants.DELIMITER_SLASH).append(productAnalyticsContextPath).toString();
  }

  @Bean
  public XInventoryFeign xInventoryFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(XInventoryFeign.class))
        .options(new Request.Options(xInventoryTimeout, xInventoryTimeout)).retryer(
            new Retryer.Default(xInventoryInitialInterval, xInventoryMaxPeriod,
                xInventoryMaxAttempts)).target(XInventoryFeign.class, getXInventoryUrl());
  }

  private String getXInventoryUrl() {
    return new StringBuilder(xInventoryFeignHost).append(Constants.COLON).append(xInventoryPort)
        .append(Constants.DELIMITER_SLASH).append(xInventoryContextPath).toString();
  }

  @Bean
  public XBPFeign xbpFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(XBPFeign.class))
        .options(new Request.Options(xBPTimeout, xBPTimeout))
        .target(XBPFeign.class, getXBPUrl());
  }

  private String getXBPUrl() {
    return new StringBuilder(xBPFeignHost).append(Constants.COLON).append(xBPPort)
        .append(Constants.DELIMITER_SLASH).append(xBPContextPath).toString();
  }

  @Bean
  public PBPFeign pbpFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(PBPFeign.class))
        .options(new Request.Options(pbpTimeout, pbpTimeout))
        .target(PBPFeign.class, getPBPUrl());
  }

  @Bean
  public XProductFeign xProductFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(XProductFeign.class))
        .options(new Request.Options(xProductTimeout, xProductTimeout))
        .target(XProductFeign.class, getXProductUrl());
  }

  private String getXProductUrl() {
    return new StringBuilder(xProductFeignHost).append(Constants.COLON).append(xProductPort)
        .append(Constants.DELIMITER_SLASH).append(xProductContextPath).toString();
  }

  @Bean
  public static YouTube youTube() {
    return new YouTube.Builder(new NetHttpTransport(), new JacksonFactory(), request -> {
    }).setApplicationName(APP_ID).build();
  }

  private String getPBPUrl() {
    return new StringBuilder(pbpHost).append(Constants.COLON).append(pbpPort)
        .append(Constants.DELIMITER_SLASH).append(pbpContextPath).toString();
  }


  @Bean(name = "solrClient")
  public SolrClient solrClient() {
    return getCloudSolrClient(vendorProductCollection);
  }

  @Bean(name = "vendorProductCollectionClient")
  public CloudSolrClient vendorProductCollectionClient() {
    return getCloudSolrClient(vendorProductCollection);
  }

  @Bean(name = "iprProductCollectionClient")
  public CloudSolrClient iprProductCollectionClient() {
    return getCloudSolrClient(iprProductCollection);
  }

  private CloudSolrClient getCloudSolrClient(String solrCollection) {
    CloudSolrClient cloudSolrClient =
      new CloudSolrClient.Builder(Arrays.asList(solrCloudUrls.split(",")),
        Optional.empty()).withConnectionTimeout(Integer.parseInt(connectionTimeout))
        .withHttpClient(HttpClientUtil.createClient(getSolrParams(httpParams)))
        .withSocketTimeout(Integer.parseInt(soTimeout)).build();
    cloudSolrClient.setDefaultCollection(solrCollection);
    cloudSolrClient.setZkConnectTimeout(Integer.valueOf(zkConnectionTimeout));
    cloudSolrClient.setZkClientTimeout(Integer.valueOf(zkClientTimeout));
    return cloudSolrClient;
  }

  private ModifiableSolrParams getSolrParams(String solrHttpParams) {
    ModifiableSolrParams params = new ModifiableSolrParams();
    String solrParams = Optional.ofNullable(solrHttpParams).orElse(DEFAULT_HTTP_PARAMS);
    if (StringUtils.isNotBlank(solrParams)) {
      Splitter.on(SEPARATOR).withKeyValueSeparator(SLASH).split(solrParams).forEach(params::add);
    }
    return params;
  }
}
