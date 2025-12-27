package com.gdn.x.product.service.interceptor;

import java.util.Optional;

import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.AuditorAware;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@RequiredArgsConstructor
public class AuditorAwareImpl implements AuditorAware<String> {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Override
  public Optional<String> getCurrentAuditor() {
    if (GdnMandatoryRequestParameterUtil.getUsername() != null) {
      log.info("Logged in user information {} ", GdnMandatoryRequestParameterUtil.getUsername());
      return Optional.of(GdnMandatoryRequestParameterUtil.getUsername());
    } else {
      return Optional.of(mandatoryParameterHelper.getUsername());
    }
  }



}
