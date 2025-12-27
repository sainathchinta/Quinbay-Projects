package com.gdn.partners.pcu.external.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.external.client.fallback.SellerLogisticsFeignFallback;
import com.gdn.partners.pcu.external.client.feign.SellerLogisticsFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;

@Component
public class SellerLogisticsFeignFallbackFactory
    extends AbstractFallbackFactory<SellerLogisticsFeign> {

  @Autowired
  private SellerLogisticsFeignFallback fallback;

  @Autowired
  public SellerLogisticsFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public SellerLogisticsFeign doCreate(Throwable cause) {
    return fallback;
  }
}
