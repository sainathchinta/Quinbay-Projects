package com.gdn.mta.bulk;

public enum ProductSuspensionReasons {

  FAKE_PRODUCT("Fake product"), DUPLICATE_PRODUCT("Duplicate product"), ABUSE_OF_MASTER_DATA(
      "Abuse of master data"), INTERNAL_MISTAKE("Internal mistake"), MERCHANT_HAVE_REVISED_THE_PRODUCT(
      "Merchant have revised the product"), OTHERS("Others");

  private String reason;

  ProductSuspensionReasons(String reason) {
    this.reason = reason;
  }

  public String getErrorMessage() {
    return reason;
  }
}
