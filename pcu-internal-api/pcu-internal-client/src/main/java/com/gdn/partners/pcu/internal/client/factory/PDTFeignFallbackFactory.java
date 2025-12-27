package com.gdn.partners.pcu.internal.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.internal.client.fallback.PDTFeignFallback;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.properties.ApplicationProperties;

/**
 * Created by shivam 26/06/2019 AD.
 */
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
