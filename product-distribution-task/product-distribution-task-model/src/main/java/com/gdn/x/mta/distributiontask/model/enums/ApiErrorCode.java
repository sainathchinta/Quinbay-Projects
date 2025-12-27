package com.gdn.x.mta.distributiontask.model.enums;

public enum ApiErrorCode {

  BRAND_IS_INREVIEW("ERR-PDT400001", 400, "The Brand is currently in review"),
  FAMILY_COLOUR_NOT_FOUND("ERR-PDT400002", 400, "Family Colour Field must not be null"),
  MANDATORY_FIELD_NOT_FOUND("ERR-PDT400003", 400, "Mandatory Field value must not be empty"),
  CONTAIN_RESTRICTED_KEYWORD("ERR-PDT400004", 400, "Contains Restricted Keyword"),
  FAULTY_ON_IMAGES("ERR-PDT400005",400, "Faulty on images"),
  SELLER_NOT_AUTHORISED_TO_CREATE_PRODUCT("ERR-PDT400006",
      400, "Seller not authorised to create product in this brand"),
  FAULTY_ON_PRODUCT_TEXT_CONTENT("ERR-PDT400007",400, "Faulty on product text content"),

  PRODUCT_NOT_ASSIGNED("ERR-PDT400008", 400, "Product is not yet assigned to vendor"),
  PRODUCT_ATTRIBUTES_CANNOT_BE_EMPTY("ERR-PDT400009", 400, "Product Attributes must not be empty"),
  PRODUCT_ITEMS_CANNOT_BE_EMPTY("ERR-PDT400010", 400, "Product Item list must not be empty"),
  PRODUCT_IMAGES_MUST_NOT_BE_EMPTY("ERR-PDT400011", 400, "Product Images must be present"),
  PRODUCT_ITEM_IMAGES_CANNOT_BE_EMPTY("ERR-PDT400012", 400, "Product Item Images cannot be empty"),
  PRODUCT_IMAGES_NOT_FOUND("ERR-PDT400013", 400, "Valid Product Images were not found"),
  PRODUCT_ITEMS_ARE_MFD_TRUE(
    "ERR-PDT400014", 400, "All the Items are MFD true and not eligible for Approval"),
  BRAND_CODE_NOT_ALLOWED("ERR-PDT400015", 400, "Invalid Brand code"),
  PRODUCT_IS_ALREADY_APPROVED("ERR-PDT400016", 400, "Assigned Product is Already approved"),
  PRODUCT_NOT_FOUND("ERR-PDT400017", 400, "Product not found"),
  PRODUCT_IS_IN_INVALID_STATE("ERR-PDT400018", 400, "Product is in invalid state"),
  PRODUCT_CANNOT_BE_REJECTED_BECAUSE_WAREHOUSE_STOCK_EXISTS(
      "ERR-PDT400019", 400,
      "You can't reject the product because there's still stock at selected pickup points."),
  REJECTION_OF_DISTRIBUTION_PRODUCT_IS_NOT_ALLOWED(
    "ERR-PDT400020", 400, "Rejection of distribution product is not allowed.");

  private final String code;
  private final int httpStatus;
  private final String desc;

  ApiErrorCode(String code, int httpStatus, String desc) {
    this.code = code;
    this.httpStatus = httpStatus;
    this.desc = desc;
  }

  public String getCode() {
    return code;
  }

  public String getDesc() {
    return desc;
  }

  public int getHttpStatus() {
    return httpStatus;
  }
}
