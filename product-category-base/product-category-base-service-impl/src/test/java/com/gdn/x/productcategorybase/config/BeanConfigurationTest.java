package com.gdn.x.productcategorybase.config;


import org.junit.jupiter.api.Test;

public class BeanConfigurationTest {

  @Test
  public void pcbCollectionAndBrandCollectionClient() {
    BeanConfiguration beanConfiguration = new BeanConfiguration();
    beanConfiguration.setSolrCloudUrls("solrCloudUrls");
    beanConfiguration.setZkClientTimeout("100");
    beanConfiguration.setZkConnectionTimeout("100");
    beanConfiguration.setSoTimeout("100");
    beanConfiguration.setBrandCollectionConnectionTimeout("100");
    beanConfiguration.setPcbCollectionConnectionTimeout("100");
    beanConfiguration.setPcbCollection(null);
    beanConfiguration.setBrandCollection(null);
    beanConfiguration.pcbCollectionClient();
    beanConfiguration.brandCollectionClient();
  }

}