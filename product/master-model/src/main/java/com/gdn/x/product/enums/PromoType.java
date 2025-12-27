package com.gdn.x.product.enums;

public enum PromoType {
  CAMPAIGN("CAMPAIGN"),
  PROMO_BUNDLING("PROMO_BUNDLING"),
  PROMO_DISCOUNT("PROMO_DISCOUNT");

  private String description;

  PromoType(String description) {
    this.description = description;
  }

  public String getDescription() {
    return this.description;
  }
}
