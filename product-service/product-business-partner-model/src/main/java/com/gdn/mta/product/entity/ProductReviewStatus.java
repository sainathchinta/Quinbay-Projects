package com.gdn.mta.product.entity;

/**
 * Enum For product review Status, each status will have bit value to get exact progress
 * Created by Kesha on 25/07/16.
 */
public enum ProductReviewStatus {
  IN_SCREENING(1),
  SCREENING_APPROVED(2),
  IMAGE_APPROVED(4),
  CONTENT_APPROVED(8),
  ACTIVATED(16),
  SCREENING_REJECTED(32),
  IMAGE_REJECTED(32),
  CONTENT_REJECTED(32),
  BULK_REJECTED_SCREENING(32),
  DELETED(32);

  private int value;

  ProductReviewStatus(int value) {
    this.value = value;
  }

  public int getValue() {
    return value;
  }
}
