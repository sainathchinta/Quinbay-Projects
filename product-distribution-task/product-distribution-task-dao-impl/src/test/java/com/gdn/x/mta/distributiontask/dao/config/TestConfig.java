package com.gdn.x.mta.distributiontask.dao.config;

import javax.sql.DataSource;

import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.mockito.Mockito;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcOperations;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.Database;
import org.springframework.orm.jpa.vendor.HibernateJpaDialect;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.annotation.EnableTransactionManagement;


@Configuration
@EnableJpaRepositories(basePackages = {"com.gdn.x.mta.distributiontask.dao.api"})
@EnableJpaAuditing
@EnableTransactionManagement
@ComponentScan(basePackages = {"com.gdn.x.mta.distributiontask.dao.api"})
public class TestConfig {
  @Bean
  DataSource dataSource() {
    return new EmbeddedDatabaseBuilder().setType(EmbeddedDatabaseType.H2).build();
  }

  @Bean
  LocalContainerEntityManagerFactoryBean entityManagerFactory() throws Exception {
    LocalContainerEntityManagerFactoryBean entityManagerFactory = new LocalContainerEntityManagerFactoryBean();
    entityManagerFactory.setDataSource(this.dataSource());
    entityManagerFactory.setJpaVendorAdapter(this.jpaVendorAdapter());
    entityManagerFactory
        .setPackagesToScan(new String[] {"com.gdn.common.base.entity", "com.gdn.x.mta.distributiontask.model"});
    return entityManagerFactory;
  }

  @Bean
  HibernateJpaDialect jpaDialect() {
    HibernateJpaDialect jpaDialect = new HibernateJpaDialect();
    return jpaDialect;
  }

  @Bean
  HibernateJpaVendorAdapter jpaVendorAdapter() {
    HibernateJpaVendorAdapter jpaVendorAdapter = new HibernateJpaVendorAdapter();
    jpaVendorAdapter.setDatabase(Database.H2);
    jpaVendorAdapter.setGenerateDdl(true);
    jpaVendorAdapter.setShowSql(true);
    return jpaVendorAdapter;
  }

  @Bean
  NamedParameterJdbcOperations namedJdbcTemplate() {
    return new NamedParameterJdbcTemplate(this.dataSource());
  }

  @Bean
  JpaTransactionManager transactionManager() throws Exception {
    JpaTransactionManager jpaTransactionManager = new JpaTransactionManager();
    jpaTransactionManager.setDataSource(this.dataSource());
    jpaTransactionManager.setJpaDialect(this.jpaDialect());
    return jpaTransactionManager;
  }

  @Bean(name = "vendorProductCollectionClient")
  CloudSolrClient vendorProductCollectionClient() {
    return Mockito.mock(CloudSolrClient.class);
  }

  @Bean(name = "iprProductCollectionClient")
  CloudSolrClient iprProductCollectionClient() {
    return Mockito.mock(CloudSolrClient.class);
  }

  @Bean
  ProductServiceRepository productServiceRepository() {
    return Mockito.mock(ProductServiceRepository.class);
  }
}
