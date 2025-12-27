package com.gdn.partners.pcu.external.client.factory;

import com.gdn.partners.pcu.external.client.fallback.PDTFeignFallback;
import com.gdn.partners.pcu.external.client.feign.PDTFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PDTFeignFallbackFactory extends AbstractFallbackFactory<PDTFeign> {

  @Autowired
  private PDTFeignFallback fallback;

  @Autowired
  public PDTFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public PDTFeign doCreate(Throwable cause) {
    return fallback;
  }
}