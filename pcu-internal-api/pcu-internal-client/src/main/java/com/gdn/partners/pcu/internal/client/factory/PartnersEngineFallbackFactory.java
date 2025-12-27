package com.gdn.partners.pcu.internal.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.internal.client.fallback.PartnersEngineFeignFallback;
import com.gdn.partners.pcu.internal.client.feign.PartnersEngineFeign;
import com.gdn.partners.pcu.internal.properties.ApplicationProperties;

@Component
public class PartnersEngineFallbackFactory extends AbstractFallbackFactory<PartnersEngineFeign> {

  @Autowired
  private PartnersEngineFeignFallback fallback;

  @Autowired
  public PartnersEngineFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public PartnersEngineFeign doCreate(Throwable cause) {
    return fallback;
  }
}
