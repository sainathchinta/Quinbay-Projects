package com.gdn.partners.pcu.internal.client.factory;

import com.gdn.partners.pcu.internal.client.fallback.MasterCatalogFeignFallback;
import com.gdn.partners.pcu.internal.client.feign.MasterCatalogFeign;
import com.gdn.partners.pcu.internal.properties.ApplicationProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * @author Navya Naveli
 */

@Component
public class MasterCatalogFeignFallbackFactory extends AbstractFallbackFactory<MasterCatalogFeign> {

    @Autowired
    private MasterCatalogFeignFallback masterCatalogFeignFallback;

    @Autowired
    public MasterCatalogFeignFallbackFactory(ApplicationProperties applicationProperties) {
        super(applicationProperties);
    }

    @Override
    public MasterCatalogFeign doCreate(Throwable cause) {
        return masterCatalogFeignFallback;
    }
}
