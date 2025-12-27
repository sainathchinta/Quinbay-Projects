package com.gdn.aggregate.platform.module.product.listener.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@ConfigurationProperties(prefix = "module.domain.product.flashsale")
public class FlashsaleProperties {
    private List<String> promoTypes;

    public List<String> getPromoTypes() {
        return promoTypes;
    }

    public void setPromoTypes(List<String> promoTypes) {
        this.promoTypes = promoTypes;
    }
}
