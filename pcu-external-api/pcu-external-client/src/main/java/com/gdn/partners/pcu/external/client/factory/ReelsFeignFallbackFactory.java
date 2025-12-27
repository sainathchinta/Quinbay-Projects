package com.gdn.partners.pcu.external.client.factory;

import com.gdn.partners.pcu.external.client.fallback.ReelsFallback;
import com.gdn.partners.pcu.external.client.feign.ReelsFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


@Component
public class ReelsFeignFallbackFactory extends AbstractFallbackFactory<ReelsFeign> {

    @Autowired
    private ReelsFallback fallback;

    @Autowired
    public ReelsFeignFallbackFactory(ApplicationProperties applicationProperties) {
        super(applicationProperties);
    }

    @Override
    public ReelsFeign doCreate(Throwable cause) {
        return fallback;
    }
} 