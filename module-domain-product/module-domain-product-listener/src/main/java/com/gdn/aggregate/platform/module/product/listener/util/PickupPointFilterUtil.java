package com.gdn.aggregate.platform.module.product.listener.util;

import java.util.HashSet;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;

import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;

public class PickupPointFilterUtil {
  public static boolean isCampaignCodePresent(PickupPoint pickupPoint, String campaignCode) {
    return Optional.ofNullable(pickupPoint).map(PickupPoint::getPrice).orElseGet(HashSet::new).stream()
        .anyMatch(price -> Optional.ofNullable(campaignCode).orElse(StringUtils.EMPTY).equals(campaignCode));
  }

  public static boolean isFinalOfferPricePresent(PickupPoint pickupPoint, double finalOfferPrice) {
    return Optional.ofNullable(pickupPoint).map(PickupPoint::getPrice).orElseGet(HashSet::new).stream()
        .anyMatch(price -> finalOfferPrice == price.getFinalOfferPrice());
  }

}
