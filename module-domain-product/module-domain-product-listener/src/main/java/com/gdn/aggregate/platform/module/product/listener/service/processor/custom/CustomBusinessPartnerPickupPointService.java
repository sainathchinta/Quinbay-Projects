package com.gdn.aggregate.platform.module.product.listener.service.processor.custom;

import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomBusinessPartnerPickupPoint;
import com.gdn.aggregate.platform.module.product.listener.repository.custom.CustomBusinessPartnerPickupPointRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component("ProductCustomBusinessPartnerPickupPointService")
public class CustomBusinessPartnerPickupPointService {

    @Autowired
    CustomBusinessPartnerPickupPointRepository customBusinessPartnerPickupPointRepository;

    public CustomBusinessPartnerPickupPoint getExistingCustomBusinessPartnerPickupPoint(String pickupPointCode) {
        return Optional.ofNullable(pickupPointCode)
            .flatMap(customBusinessPartnerPickupPointRepository::findById)
            .orElse(null);
    }

}
