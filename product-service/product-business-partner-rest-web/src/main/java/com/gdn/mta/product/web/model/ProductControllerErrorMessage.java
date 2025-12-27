package com.gdn.mta.product.web.model;

import java.text.SimpleDateFormat;

public final class ProductControllerErrorMessage {

  public final SimpleDateFormat DEFAULT_DATE_FORMAT = new SimpleDateFormat(
      "yyyy-MM-dd HH:mm");
  public static final Integer DEFAULT_DANGEROUS_GOOD_LEVEL = 0;
  public static final Integer MAXIMUM_PRODUCT_NAME_LENGTH = 150;
  public static final Integer MAXIMUM_UNIQUE_SELLING_POINT_LENGTH = 400;
  public static final Integer MAXIMUM_DESCRIPTION_LENGTH = 5000;

  public static final String ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE =
      "required field createdBy or createdDate is empty";
  public static final String ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE =
      "required field updatedBy or updatedDate is empty";
  public static final String NAME_MUST_NOT_BE_BLANK = "name must not be blank";
  public static final String NAME_LENGTH_MUST_NOT_EXCEED_MAX_ALLOWED =
      "productName length must not exceed " +
          ProductControllerErrorMessage.MAXIMUM_PRODUCT_NAME_LENGTH + " characters";
  public static final String DESCRIPTION_MUST_NOT_BE_BLANK = "description must not be blank";
  public static final String LONG_DESCRIPTION_MUST_NOT_BE_BLANK =
      "long description must not be blank";
  public static final String SPECIFICATION_DETAIL_MUST_NOT_BE_BLANK =
      "specification detail must not be blank";
  public static final String LENGTH_MUST_NOT_BE_BLANK = "length must not be blank";
  public static final String WIDTH_MUST_NOT_BE_BLANK = "width must not be blank";
  public static final String HEIGHT_MUST_NOT_BE_BLANK = "height must not be blank";
  public static final String WEIGHT_MUST_NOT_BE_BLANK = "weight must not be blank";
  public static final String SHIPPING_WEIGHT_MUST_NOT_BE_BLANK =
      "shipping weight must not be blank";
  public static final String PRODUCT_CATEGORIES_MUST_NOT_BE_BLANK =
      "product categories must not be blank";
  public static final String PRODUCT_ATTRIBUTES_MUST_NOT_BE_BLANK =
      "product attributes must not be blank";
  public static final String PRODUCT_ITEM_DETAILS_MUST_NOT_BE_BLANK =
      "product item details must not be blank";
  public static final String IMAGE_NOT_FOUND = "Image not found : ";
  public static final String MAX_5_WHOLESALE_PRICE_TIERS_ARE_ALLOWED = "Maximum of 5 wholesale price tiers are allowed";
  public static final String WHOLESALE_PRICE_CANNOT_BE_LESS_THAN_1000 = "Wholesale price cannot be less than 1000";
  public static final String MIN_QUANTITY_ALLOWED_FOR_WHOLESALE_PRICE_IS_2 =
      "Minimum Quantity allowed to set wholesale price is 2";
  public static final String WHOLESALE_PRICE_TIER_RULE_VIOLATION =
      "Wholesale discount of tier 1 should be less than following tiers";
  public static final String PRODUCT_ITEMS_MUST_NOT_BE_BLANK = "product items must not be blank";
  public static final String PRODUCT_BUSINESS_PARTNERS_MUST_NOT_BE_BLANK =
      "productBusinessPartners must not be blank";
  public static final String BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK =
      "businessPartnerId must not be blank";
  public static final String PRODUCT_CODE_MUST_NOT_BE_BLANK = "productCode must not be blank";
  public static final String PRODUCT_ITEM_NAME_MUST_NOT_BE_BLANK =
      "productItemName must not be blank";
  public static final String PRODUCT_IMAGES_MUST_NOT_BE_BLANK = "images must not be blank";
  public static final String PRODUCT_IMAGES_HAS_INVALID_EXTENSION = "images have invalid extension";
  public static final String PICKUP_POINT_ID_MUST_NOT_BE_BLANK = "pickupPointId must not be blank";
  public static final String GDN_SKU_MUST_NOT_BE_BLANK = "gdnSku must not be blank";
  public static final String PRODUCT_TYPE_MUST_NOT_BE_BLANK = "productType must not be blank";
  public static final String PRICE_MUST_NOT_BE_BLANK = "price must not be blank";
  public static final String SALE_PRICE_MUST_NOT_BE_BLANK = "sale price must not be blank";
  public static final String STOCK_MUST_NOT_BE_BLANK = "stock must not be blank";
  public static final String NOTES_MUST_NOT_BE_BLANK = "notes must not be blank";
  public static final String FILENAME_MUST_NOT_BE_BLANK = "filename must not be blank";
  public static final String HASH_CODE_MUST_NOT_BE_BLANK = "hash code must not be blank";
  public static final String PRODUCT_LEVEL1_IDS_MUST_NOT_BE_BLANK = "productLevel1 Ids must not be blank";
  public static final String BULK_UPDATE_LIST_MUST_NOT_BE_BLANK = "Bulk update list must not be blank";
  public static final String INVALID_EAN_UPC_FORMAT = "EAN UPC code is not in valid format";
  public static final String MINIMUM_PRICE_VALUE_INVALID =
      "price and sale price must be greater than ";
  public static final String SALE_PRICE_GREATER_THAN_PRICE_VALUE =
      "Sale price must be lesser than prcie";
  public static final String UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS =
      "Max. 400 characters for Unique selling point";
  public static final String DESCRIPTION_MUST_NOT_BE_MORE_THAN_MAX_CHARACTERS =
      "Max. %d characters for Description";
  public static final String CHARACTER_LIMIT_REACHED_PLEASE_REDUCE_FORMATTING =
      "Character Limit reached, please reduce formatting.";
  public static final String CN_VALIDATION_ERROR_MESSAGE =
      "Product creation is not allowed in this selected category. Please select a specific category to create the product.";
  public static final String B2B_VALIDATION_ERROR_MESSAGE =
      "Product creation is not allowed in exclusive B2B category. Please select a specific category to create the product.";
  public static final String MPP_NOT_ALLOWED_FOR_SELLER = "Multi pickup point not allowed for this seller.";
  public static final String BRAND_CODE_INVALID = "brand code invalid";
  public static final String MAX_STOCK_LIMIT_ERROR =
      "Max. %s units of stock";
  public static final String B2B_MINIMUM_PRICE_VALUE_INVALID =
      "Blibli for business base price must be greater than ";
  public static final String BRAND_CODE_MUST_NOT_BE_BLANK = "brand code must not be blank";
  public static final String CATEGORY_CODE_MUST_NOT_BE_BLANK = "category code must not be blank";
  public static final String SHIPPING_UNSUPPORTED_FOR_CATEGORY_ERROR_MESSAGE = "Tipe pengiriman "
    + "ini tidak tersedia untuk kategori terpilih. Silakan ubah tipe pengiriman untuk melanjutkan.";
}
