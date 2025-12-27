package com.gdn.mta.bulk.config;

import java.util.Optional;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.AuditorAware;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;

@Configuration
@EntityScan(basePackages = {"com.gdn.mta.bulk.entity"})
@EnableJpaRepositories(basePackages = {"com.gdn.mta.bulk.repository", "com.gdn.mta.bulk.mail"})
@EnableJpaAuditing(auditorAwareRef = "auditorProvider")
public class JpaConfiguration {
  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Bean
  public AuditorAware<String> auditorProvider() {
    return () -> Optional.ofNullable(
        Optional.ofNullable(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER))
            .orElse(mandatoryParameterHelper.getUsername()));
  }
}
