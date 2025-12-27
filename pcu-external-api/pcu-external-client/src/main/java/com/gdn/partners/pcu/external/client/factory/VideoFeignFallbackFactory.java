package com.gdn.partners.pcu.external.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.external.client.fallback.VideoFeignFallback;
import com.gdn.partners.pcu.external.client.feign.VideoFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;

@Component
public class VideoFeignFallbackFactory extends AbstractFallbackFactory<VideoFeign> {

    @Autowired
    private VideoFeignFallback fallback;

    @Autowired
    public VideoFeignFallbackFactory(ApplicationProperties applicationProperties) {
        super(applicationProperties);
    }

    @Override
    public VideoFeign doCreate(Throwable cause) {
        return fallback;
    }
} 