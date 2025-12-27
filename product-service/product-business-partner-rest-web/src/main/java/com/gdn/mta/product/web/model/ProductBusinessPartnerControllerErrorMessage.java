package com.gdn.mta.product.web.model;

public final class ProductBusinessPartnerControllerErrorMessage {

  public static final String ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE =
      "required field createdBy or createdDate is empty";
  public static final String ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE =
      "required field updatedBy or updatedDate is empty";
  public static final String BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK =
      "businessPartnerId must not be blank";
  public static final String PRODUCT_ID_MUST_NOT_BE_BLANK = "productId must not be blank";
  public static final String PRODUCT_ITEM_BUSINESS_PARTNERS_MUST_NOT_BE_BLANK =
      "productItemBusinessPartners must not be blank";
  public static final String PRODUCT_ITEM_ID_MUST_NOT_BE_BLANK = "productItemId must not be blank";
  public static final String PRODUCT_TYPE_MUST_NOT_BE_BLANK = "productType must not be blank";
  public static final String PRICE_MUST_NOT_BE_BLANK = "price must not be blank";
  public static final String SALE_PRICE_MUST_NOT_BE_BLANK = "salePrice must not be blank";
  public static final String MINIMUM_PRICE_VALUE_INVALID =
      "price and sale price must be greater than ";
  public static final String STOCK_MUST_NOT_BE_BLANK = "stock must not be blank";
  public static final String MINIMUM_STOCK_MUST_NOT_BE_BLANK = "minimumStock must not be blank";
  public static final String PICKUP_POINT_ID_MUST_NOT_BE_BLANK = "pickupPointId must not be blank";
  public static final String PRODUCT_NAME_MUST_NOT_BE_BLANK = "productName must not be blank";
  public static final String CATEGORY_NAME_MUST_NOT_BE_BLANK = "categoryName must not be blank";
  public static final String BRAND_MUST_NOT_BE_BLANK = "brand must not be blank";
  public static final String PRODUCT_BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK =
      "productBusinessPartnerId must not be blank";
  public static final String MISSING_PRODUCT_ITEMS = "Required field itemSKUs is missing";
  public static final String MISSING_MANDATORY_VALUE = "Mandatory attribute value is missing";
  public static final String MISSING_MANDATORY_VALUE_IN_BP_ATTRIBUTES =
      "Mandatory attribute value is missing in business partner attributes";
  public static final String MISSING_SOURCE_BUSINESS_PARTNER_ID = "source businessPartnerCode must not be blank";
}
