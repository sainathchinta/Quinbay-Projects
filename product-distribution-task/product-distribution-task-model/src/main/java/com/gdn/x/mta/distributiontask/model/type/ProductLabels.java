package com.gdn.x.mta.distributiontask.model.type;

public enum ProductLabels {

  NSFW("Pornography"),
  TEXT("Text"),
  LOGO("Other e-commerce/social media logos"),
  BLUR("Blur"),
  MEDICINE("Illegal drugs"),
  WATERMARK("Watermark"),
  RESTRICTED_KEYWORD("Restricted Keyword"),
  GOOD("No Detection"),
  BRAND("Brand"),
  BRAND_MISMATCH("Brand mismatch"),
  GOOGLE_RESTRICTION("Google restriction"),
  PRESCRIPTION_DRUGS("Prescription drugs"),
  PROHIBITED_DRUGS("Prohibited drugs"),
  PENDING("Pending");

  private final String description;

  ProductLabels(String description) {
    this.description = description;
  }

  public String getDescription() {
    return description;
  }
}
