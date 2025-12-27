package com.gdn.aggregate.platform.module.product.listener.util;

import java.util.Objects;

import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;

public class PickupPointSortUtil {

  public static double getMinFinalOfferPrice(PickupPoint pickupPoint) {
    double minFinalOfferPrice = Double.MAX_VALUE;
    if (Objects.isNull(pickupPoint.getPrice())) {
      return Double.MIN_VALUE;
    } else {
      for (PickupPoint.Price price : pickupPoint.getPrice()) {
        minFinalOfferPrice = Math.min(minFinalOfferPrice, price.getOfferPrice());
      }
      return minFinalOfferPrice;
    }
  }

  public static double getMaxFinalOfferPrice(PickupPoint pickupPoint) {
    double maxFinalOfferPrice = Double.MIN_VALUE;
    if (Objects.isNull(pickupPoint.getPrice())) {
      return Double.MIN_VALUE;
    } else {
      for (PickupPoint.Price price : pickupPoint.getPrice()) {
        maxFinalOfferPrice = Math.max(maxFinalOfferPrice, price.getFinalOfferPrice());
      }
      return maxFinalOfferPrice;
    }
  }

  public static double getMinOfferPrice(PickupPoint pickupPoint) {
    double minOfferPrice = Double.MAX_VALUE;
    if (Objects.isNull(pickupPoint.getPrice())) {
      return Double.MIN_VALUE;
    } else {
      for (PickupPoint.Price price : pickupPoint.getPrice()) {
        minOfferPrice = Math.min(minOfferPrice, price.getOfferPrice());
      }
      return minOfferPrice;
    }
  }

  public static double getMaxOfferPrice(PickupPoint pickupPoint) {
    double maxOfferPrice = Double.MIN_VALUE;
    if (Objects.isNull(pickupPoint.getPrice())) {
      return Double.MIN_VALUE;
    } else {
      for (PickupPoint.Price price : pickupPoint.getPrice()) {
        maxOfferPrice = Math.max(maxOfferPrice, price.getOfferPrice());
      }
      return maxOfferPrice;
    }
  }

  public static int getMinPriority(PickupPoint pickupPoint, boolean prioritizeLowestPrice) {
    int minPriority = Integer.MAX_VALUE;
    if (Objects.isNull(pickupPoint.getPrice())) {
      return Integer.MIN_VALUE;
    } else {
      for (PickupPoint.Price price : pickupPoint.getPrice()) {
        if (Objects.isNull(price.getPriority())) {
          // When prioritizeLowestPrice is enabled, null priorities rank worst (Integer.MAX_VALUE)
          return prioritizeLowestPrice ? Integer.MAX_VALUE : Integer.MIN_VALUE;
        } else {
          minPriority = Math.min(minPriority, price.getPriority());
        }
      }
      return minPriority;
    }
  }

  public static int getMaxPriority(PickupPoint pickupPoint) {
    int maxPriority = Integer.MIN_VALUE;
    if (Objects.isNull(pickupPoint.getPrice())) {
      return Integer.MIN_VALUE;
    } else {
      for (PickupPoint.Price price : pickupPoint.getPrice()) {
        if (Objects.isNull(price.getPriority())) {
          return Integer.MIN_VALUE;
        } else {
          maxPriority = Math.max(maxPriority, price.getPriority());
        }
      }
      return maxPriority;
    }
  }

  public static int getMinPromoCampaign(PickupPoint pickupPoint) {
    if (Objects.isNull(pickupPoint.getPrice())) {
      return Integer.MIN_VALUE;
    } else {
      for (PickupPoint.Price price : pickupPoint.getPrice()) {
        if (!price.isPromoCampaign()) {
          return Integer.MIN_VALUE + 1;
        }
      }
      return Integer.MAX_VALUE;
    }
  }

  public static int getMaxPromoCampaign(PickupPoint pickupPoint, boolean prioritizeLowestPrice) {
    if (Objects.isNull(pickupPoint.getPrice())) {
      return Integer.MIN_VALUE;
    } else {
      for (PickupPoint.Price price : pickupPoint.getPrice()) {
        if (price.isPromoCampaign()) {
          // When prioritizeLowestPrice is enabled, promo campaigns rank best (Integer.MAX_VALUE)
          return prioritizeLowestPrice ? Integer.MAX_VALUE : Integer.MIN_VALUE + 1;
        }
      }
      // When prioritizeLowestPrice is enabled, non-promo ranks worst (Integer.MIN_VALUE + 1)
      return prioritizeLowestPrice ? Integer.MIN_VALUE + 1 : Integer.MAX_VALUE;
    }
  }
}