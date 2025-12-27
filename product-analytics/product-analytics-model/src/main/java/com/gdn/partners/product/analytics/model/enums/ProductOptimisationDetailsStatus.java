package com.gdn.partners.product.analytics.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@AllArgsConstructor
public enum ProductOptimisationDetailsStatus {
  NEW(0), VIEWED(10), EDITED(20), NEGATIVE_FEEDBACK(30);

  private final int value;

  public static ProductOptimisationDetailsStatus fromValue(int value) {
    for (ProductOptimisationDetailsStatus status : ProductOptimisationDetailsStatus.values()) {
      if (status.getValue() == value) {
        return status;
      }
    }
    throw new IllegalArgumentException("Unknown value: " + value);
  }
}