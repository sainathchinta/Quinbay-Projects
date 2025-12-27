package com.gdn.x.mta.distributiontask.model.enums;

import org.apache.commons.lang.StringUtils;

import java.util.Arrays;
import java.util.Objects;

public enum SellerBadgeConstants {
  NONE_MERCHANT("None Merchant"), BRONZE_MERCHANT("Bronze Merchant"), SILVER_MERCHANT(
    "Silver Merchant"), GOLD_MERCHANT("Gold Merchant"), DIAMOND_MERCHANT(
    "Diamond Merchant"), OFFICIAL_STORES("Official Stores");

  private final String value;

  SellerBadgeConstants(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  public static SellerBadge fromSellerBadgeConstants(String sellerBadgeConstants) {
    if (Objects.nonNull(sellerBadgeConstants)) {
      if (Arrays.stream(SellerBadge.values())
        .anyMatch(element -> StringUtils.equalsIgnoreCase(element.name(), sellerBadgeConstants))) {
        return SellerBadge.valueOf(sellerBadgeConstants);
      } else {
        for (SellerBadgeConstants sellerBadgeConstant : SellerBadgeConstants.values()) {
          if (sellerBadgeConstant.getValue().equalsIgnoreCase(sellerBadgeConstants)) {
            return SellerBadge.valueOf(sellerBadgeConstant.name());
          }
        }
      }
    }
    return SellerBadge.NONE_MERCHANT;
  }
}

