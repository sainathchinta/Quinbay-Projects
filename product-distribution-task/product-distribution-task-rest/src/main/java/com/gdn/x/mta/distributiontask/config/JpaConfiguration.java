package com.gdn.x.mta.distributiontask.config;

import java.util.Optional;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.AuditorAware;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;

@Configuration
@EntityScan(basePackages = {"com.gdn.x.mta.distributiontask.model", "com.gdn.partners.pdt.entity"})
@EnableJpaRepositories(basePackages = {"com.gdn.x.mta.distributiontask.dao.api","com.gdn.partners.pdt.repository"})
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
