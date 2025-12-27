package com.gdn.x.productcategorybase.config;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.AuditorAware;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.orm.jpa.EntityManagerFactoryInfo;

import com.gdn.x.config.SpringDataJpaAuditingConfig;
import jakarta.persistence.EntityManager;

@Configuration
@EntityScan(basePackages = {"com.gdn.x.productcategorybase.entity"})
@EnableJpaRepositories(basePackages = {"com.gdn.x.productcategorybase.repository",
    "com.gdn.x.productcategorybase.repository.impl"})
@EnableJpaAuditing(auditorAwareRef = "auditorProvider")
public class JpaConfiguration {

  @Autowired
  private EntityManager entityManager;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Bean
  public AuditorAware<String> auditorProvider() {
    return () -> Optional.of(mandatoryParameterHelper.getUsername());
  }

  @Bean
  public SpringDataJpaAuditingConfig springDataJpaAuditingConfig() {
    return new SpringDataJpaAuditingConfig();
  }

  @Bean
  public JdbcTemplate pcbJdbcTemplate() {
    return new JdbcTemplate(
        ((EntityManagerFactoryInfo) entityManager.getEntityManagerFactory()).getDataSource());
  }

  @Bean
  public NamedParameterJdbcTemplate namedJdbcTemplate() {
    return new NamedParameterJdbcTemplate(
        ((EntityManagerFactoryInfo) entityManager.getEntityManagerFactory()).getDataSource());
  }

}
