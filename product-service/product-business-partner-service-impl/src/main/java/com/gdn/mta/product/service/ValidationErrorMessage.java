package com.gdn.mta.product.service;

public enum ValidationErrorMessage {

  ITEM_CODE_MUST_NOT_BE_BLANK("item code must not be blank"),
  PRODUCT_ITEM_UOM_INFO_REQUESTS_MUST_NOT_BE_EMPTY("Product Item UOM Info Requests must not be empty"),
  OMNI_CHANNEL_SKU_CANNOT_BE_EMPTY("OmniChannelSku cannot be empty"),
  PRODUCT_ITEM_UOM_INFO_MUST_NOT_BE_EMPTY("Product Item UOM Info must not be empty"),
  DIMENSION_AND_UOM_UPDATE_REQUESTS_LENGTH_MUST_NOT_BE_EMPTY("Invalid UOM Update Requests length must not be empty"),
  DIMENSION_AND_UOM_UPDATE_REQUESTS_WIDTH_MUST_NOT_BE_EMPTY("Invalid UOM Update Requests width must not be empty"),
  DIMENSION_AND_UOM_UPDATE_REQUESTS_HEIGHT_MUST_NOT_BE_EMPTY("Invalid UOM Update Requests height must not be empty"),
  DIMENSION_AND_UOM_UPDATE_REQUESTS_WEIGHT_MUST_NOT_BE_EMPTY("Invalid UOM Update Requests weight must not be empty"),
  DIMENSION_AND_UOM_UPDATE_REQUESTS_UOM_CODE_MUST_NOT_BE_EMPTY("Invalid UOM Update Requests uomCode must not be empty"),
  DIMENSION_AND_UOM_UPDATE_REQUESTS_UOM_TYPE_MUST_NOT_BE_EMPTY("Invalid UOM Update Requests uomType must not be empty"),
  DIMENSION_AND_UOM_UPDATE_REQUESTS_MUST_NOT_BE_EMPTY("Dimension and UOM Update Requests must not be empty"),
  DUPLICATE_EAN_UPC_VALUE("Duplicate ean upc value"),
  INVALID_EAN_UPC_VALUE("Invalid ean upc value"),
  DUPLICATE_UOM_CODE("Duplicate uomCode"),
  BASE_UOM_CANNOT_BE_CHANGED("Base UOM cannot be changed once set"),
  INVALID_UOM_CODE("Invalid uomCode, allowed values are %s"),
  INVALID_UOM_TYPE("Invalid uomType valid values are Base and Alternate"),
  DUPLICATE_BASE_UOM("Duplicate Base uom not allowed"),
  SELLER_CODE_MUST_NOT_BE_BLANK("SellerCode shouldn't be blank"),
  ACTION_NOT_ALLOWED("distribution update action not allowed"),
  PRODUCT_CODE_MUST_NOT_BE_BLANK("product code must not be blank"),
  DISTRIBUTION_L5_CANNOT_BE_ADDED("Distribution L5 cannot be added"),;

  private String message;

  ValidationErrorMessage(String message) {
    this.message = message;
  }

  public String getMessage() {
    return this.message;
  }
}
