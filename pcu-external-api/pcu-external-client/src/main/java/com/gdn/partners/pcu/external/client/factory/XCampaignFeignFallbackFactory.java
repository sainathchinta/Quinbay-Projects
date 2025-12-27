package com.gdn.partners.pcu.external.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.external.client.fallback.XCampaignFeignFallback;
import com.gdn.partners.pcu.external.client.feign.XCampaignFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;

@Component
public class XCampaignFeignFallbackFactory extends AbstractFallbackFactory<XCampaignFeign>{

  @Autowired
  private XCampaignFeignFallback fallback;

  @Autowired
  public XCampaignFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public XCampaignFeignFallback doCreate(Throwable cause) {
    return fallback;
  }


}
