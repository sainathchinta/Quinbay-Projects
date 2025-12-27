package com.gdn.aggregate.platform.module.product.listener.service.processor.custom;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomBusinessPartnerPickupPoint;
import com.gdn.aggregate.platform.module.product.listener.repository.custom.CustomBusinessPartnerPickupPointRepository;

@Component("productCustomBusinessPartnerPickupPointServiceV2")
public class CustomBusinessPartnerPickupPointServiceV2 {

  @Autowired
  private CustomBusinessPartnerPickupPointRepository customBusinessPartnerPickupPointRepository;

  public Optional<CustomBusinessPartnerPickupPoint> findById(String id) {
    return customBusinessPartnerPickupPointRepository.findById(id);
  }

  public CustomBusinessPartnerPickupPoint getExistingCustomBusinessPartnerPickupPoint(String pickupPointCode,
      List<CustomBusinessPartnerPickupPoint> allCustomBusinessPartnerPickupPoint) {
    return Optional.ofNullable(allCustomBusinessPartnerPickupPoint).orElseGet(ArrayList::new).stream().filter(
        customBusinessPartnerPickupPoint -> Optional.ofNullable(pickupPointCode).orElse(StringUtils.EMPTY)
            .equals(customBusinessPartnerPickupPoint.getId())).findFirst().orElse(null);
  }
}
