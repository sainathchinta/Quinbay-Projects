package com.gdn.pbp.config;

import java.util.Optional;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.AuditorAware;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.pbp.property.MandatoryParameterHelper;

@Configuration
@EntityScan(basePackages = {"com.gdn.mta.product.entity", "com.gdn.partners.pbp.entity",
    "com.gdn.partners.product.orchestrator.entity"})
@EnableJpaRepositories(basePackages = {"com.gdn.mta.product.repository", "com.gdn.partners"})
@EnableJpaAuditing(auditorAwareRef = "auditorProvider")
public class JpaConfiguration {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Bean
  public AuditorAware<String> auditorProvider() {
    return () -> Optional.of(Optional.ofNullable(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER))
        .orElse(mandatoryParameterHelper.getUsername()));
  }
}