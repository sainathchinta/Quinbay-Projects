package com.gdn.mta.product.service;

public interface ErrorMessages {
  String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";
  String PRODUCT_COLLECTION_NOT_FOUND = "product collection not found";
  String PRODUCT_NOT_FOUND = "Product not found or not active";
  String PRODUCT_NOT_REVIEWED = "Product not reviewed";
  String FREE_SAMPLE_CANNOT_BE_TRUE = "Free sample and cnc can not be true at the same time";
  String PRODUCT_MIGRATION_NOT_FOUND = "product migration not found";
  String PRODUCT_SKU_NOT_FOUND = "product sku not found in x-product";
  String ITEM_SKU_NOT_FOUND = "item sku not found in x-product";
  String USER_NAME_MUST_NOT_BE_EMPTY = "User name must not be empty";
  String ACTIVITY_MUST_NOT_BE_EMPTY = "Activity must not be empty";
  String PREVIOUS_VALUE_MUST_NOT_BE_EMPTY = "Previous value must not be empty";
  String CURRENT_VALUE_MUST_NOT_BE_EMPTY = "Current value must not be empty";
  String MASTER_CATALOG_NOT_FOUND = "master catalog is empty for product sku in x-product";
  String SKU_MUST_NOT_BE_BLANK = "Sku must not be blank";
  String DATE_MUST_NOT_BE_NULL = "Date must not be null";
  String PICKUP_POINT_CODE_MUST_NOT_BE_NULL = "Pickup point code must not be null";
  String ERROR_WHEN_UPDATING_DATA_IN_PCB = "Error when updating data in PCB";
  String VARIABLE_MUST_NOT_BE_BLANK = "variable must not be blank";
  String VALUE_MUST_NOT_BE_BLANK = "value must not be blank";
  String DESCRIPTION_MUST_NOT_BE_BLANK = "description must not be blank";
  String VALUE_NOT_FOUND = "value not found for update";
  String IMAGE_PREDICTION_TYPE_NOT_FOUND = "image prediction type not found for update";
  String PREDICTION_TYPE_MUST_NOT_BE_BLANK = "image prediction type must not be blank";
  String PREDICTION_TYPE_LIST_MUST_NOT_BE_EMPTY = "prediction type list must not be empty";
  String PREDICTION_NAME_MUST_BE_EMPTY = "image prediction name must not be empty";
  String BOTH_FORCE_REVIEW_AND_NEED_REVISION_ENABLED_CANNOT_BE_TRUE =
      "both forceReview and needRevisionEnabled cannot be true";
  String INACTIVE_BUSINESS_PARTNER_MSG = "business partner is inactive for productId : ";
  String BUSINESS_PARTNER_PROFILE_SHOULD_NOT_BE_NULL = "Business partner profile should not be null";
  String ERROR_WHILE_CONVERTING_JSON_RESPONSE = "Error while converting json to response , itemSku: ";
  String MIN_QUANTITY_ALLOWED_FOR_WHOLESALE_PRICE_IS_2 = "Minimum Quantity allowed to set wholesale price is 2";
  String WHOLESALE_PRICE_TIER_RULE_VIOLATION = "Wholesale discount of tier 1 should be less than following tiers";
  String WHOLESALE_FLAG_AUTO_DEACTIVATE =
      "We'll turn off wholesale price for this product because minimum percentage discount is different. Kindly check the detail on wholesale section";
  String WHOLESALE_PRICE_CANT_BE_LESS_THAN_MINIMUM_PRICE = "wholesale price less than minimum price %s";
  String WHOLESALE_QUANTITY_IS_DUPLICATE = "Wholesale configuration has duplicate quantity setting";
  String WHOLESALE_DISCOUNT_IS_DUPLICATE = "Wholesale configuration has duplicate discount setting";
  String WHOLESALE_DISCOUNT_LTE_ZERO = "Wholesale discount can't less or equal than 0";
  String WHOLE_SETTING_GTE_5 = "Wholesale total setting can't be more than %d for %s, the current total setting is %s";
  String WHOLE_SETTING_GTE_5_CREATE = "Wholesale total setting can't be more than %d, the current total setting is %s";
  String INVALID_URL = "video url is invalid";
  String INVALID_VERSION_ERROR = "Product content is expired";
  String ITEM_NOT_EDITABLE = "Item is not editable at the moment";
  String INVALID_DIMENSIONS = "Invalid dimension for product: {}";
  String WHOLE_SALE_VALIDATION_FAILED = "Wholesale price validation failed for sku : %s";
  String LOGISTICS_GET_FAILED = "Error when getting logistics data for item";
  String LOGISTICS_UPDATE_FAILED = "Error when updating logistics data for item";
  String PRODUCT_LIST_EMPTY = "Product list cannot be empty";
  String PRODUCT_CODE_BLANK = "Product code cannot be empty or blank";
  String SELLER_CODE_BLANK = "Seller code cannot be empty or blank";
  String DAY_MUST_NOT_BE_ZERO = "Number of days should be more than 0";
  String WEEK_MUST_BE_MORE_THAN_ZERO = "Number of week should be more than 0";
  String PREORDER_DATE_MUST_BE_GREATER_THAN_CURRENT_DATE = "PreOrder date must be greater than available date";
  String WRONG_PREORDER_TYPE = "PreOrder type must be DAYS, WEEK or DATE";
  String BRAND_CANNOT_BE_CHANGED = "Brand cannot be changed";
  String PRODUCT_CODE_COUNTER_DISABLED = "Cannot fetch counter for product code key";
  String REVISION_EVENT_NULL = "Event for marking need revision must not be null";
  String PRODUCT_ID_BLANK = "Product id cannot be empty or blank";
  String VAT_HISTORY_EVENT_NULL = "Event for vat external history must not be null";
  String PRODUCT_ITEM_ID_EMPTY = "Product item id must not be empty";
  String UPDATED_BY_EMPTY = "Updated by must not be empty";
  String PRODUCT_ITEM_BUSINESS_PARTNER_NOT_FOUND = "Product item business not found for productItemId = %s";
  String PRODUCT_BUSINESS_PARTNER_NOT_FOUND = "Product business not found for productId = %s";
  String PRODUCT_NEED_REVISION_MOBILE_ERROR =
      "Product revision is currently available for web or mobile web version only.";
  String ERROR_WHILE_FETCHING_ATUO_QC_RULES = "Error while fetching the auto Qc rules";
  String ERROR_STOCK_UPDATE_FAILED = "Stock update failed for items %s";
  String PRODUCT_STATE_INVALID = "Product state is invalid";
  String FREE_SAMPLE_MUST_NOT_BE_BUYABLE_OR_DISCOVERABLE =
    "Free sample cannot be updated as buyable or discoverable";
  String NOT_AUTHORISED_TO_CREATE_PRODUCT_UNDER_THIS_BRAND = "This seller doesn't have "
    + "authorisation for the selected brand. Please update brand and try again.";
  String TEMPLATE_ID_NOT_SPECIFIED = "template id not specified";
  String DESCRIPTION_MUST_NOT_BE_MORE_THAN_MAX_CHARACTERS =
    "Max. %d characters for Description";
  String BASE_PRICE_SHOULD_NOT_BE_NULL = "Base price should not be null";
  String STATUS_CAN_NOT_BE_OTHER_THAN_OFFLINE_FOR_PURE_INSTORE_PRODUCT = "Status can not be other"
    + " than offline for pure instore product";
  String B2B_BASE_PRICE_MUST_BE_GREATER_THAN_MINIMUM_PRICE = "Blibli for business base price must be greater than %s";
  String UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS =
    "Max. 400 characters for Unique selling point";
  String PRICE_STOCK_IMAGE_REQUEST_MUST_NOT_BE_EMPTY =
    "price, stock and image update request must not be empty";
  String ATTRIBUTE_MUST_NOT_BE_EMPTY = "attribute must not be empty";
  String ATTRIBUTE_CODE_MUST_NOT_BE_BLANK = "attribute code must not be blank";
  String ATTRIBUTE_TYPE_MUST_NOT_BE_BLANK = "attribute type must not be blank";
  String ATTRIBUTE_VALUE_MUST_NOT_BE_EMPTY =
    "attribute value must not be empty";
  String ATTRIBUTE_SKU_VALUE_MUST_NOT_BE_BLANK =
    "attribute sku value must not be blank";
  String BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK =
    "business partner code must not be blank";
  String PRODUCT_NAME_MUST_NOT_BE_BLANK = "product name must not be blank";
  String PRODUCT_NAME_LTE_LIMIT
    = "product name must have less than or equal to 150 characters";
  String BRAND_MUST_NOT_BE_BLANK = "brand must not be blank";
  String SPECIFICATION_DETAIL_MUST_NOT_BE_BLANK =
    "specification detail must not be blank";
  String BOTH_PRODUCT_SKU_AND_PICKUP_POINT_CODE_NOT_EMPTY = "Both productSku and pickupPoint code"
    + " can not be empty for fetching product history";
  String BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY = "businessPartnerCode must not be empty";
  String PRODUCT_SKU_MUST_NOT_BE_EMPTY = "productSku must not be empty";
  String ITEM_PICKUP_POINT_LIST_MUST_NOT_BE_EMPTY = "itemPickupPoint list must not be empty";
  String PRODUCT_VARIANT_UPDATE_REQUEST_MUST_NOT_BE_EMPTY = "Product variant update request must "
    + "not be empty";
  String ID_LIST_MUST_NOT_BE_NULL_OR_EMPTY = "Id list must not be null or empty";
  String PRODUCT_SHOULD_BELONG_TO_THE_MERCHANT = "Product %s should belong to merchant %s";
  String PRODUCT_SKU = " for productSku :";
  String BUSINESS_PARTNER_NOT_ELIGIBLE_FOR_PICKUP_POINT_UPDATE = "%s merchant not eligible for pickup point update";
  String ITEM_SKU_LIST_MUST_NOT_BE_EMPTY = "itemSku list must not be empty";
  String PREORDER_DATE_CANNOT_BE_GREATER_THAN_90_DAYS = "PreOrder date must be greater than "
    + "available date";
  String CATEGORY_MAPPINGS_MUST_NOT_BE_NULL = "category mappings must not be null";
  String CATEGORY_CODE_MUST_NOT_BE_BLANK = "category code must not be blank";
  String THRESHOLD_VALUE_INVALID_FOR_TYPE_IMAGE =
      "confidence threshold has to be greater than 0 and less than 101 and text confidence threshold has to be equal to 0 for type IMAGE productImagePrediction";
  String THRESHOLD_VALUE_INVALID_FOR_TYPE_TEXT =
      "text confidence threshold has to be greater than 0 and less than 101 and confidence threshold has to be equal to 0 for type TEXT productImagePrediction";
  String THRESHOLD_VALUE_INVALID_FOR_TYPE_IMAGE_AND_TEXT =
      "confidence threshold has to be greater than 0 and less than 101 and text confidence threshold has to be greater than 0 and less than 101 for type IMAGE_AND_TEXT productImagePrediction";
  String CANNOT_UPDATE_PRODUCT_IMAGE_PREDICTION_AND_CATEGORY_MAPPING_AS_COMPARE_CATEGORY_IS_FALSE =
      "cannot update productImagePrediction and categoryMapping as compareCategory is false for this predictionType";
  String MAXIMUM_STOCK_LIMIT_EXCEEDED_ERROR = "Maximum stock limit exceeded error, Stock must not be greater than %s units";
  String KEYWORD_MUST_NOT_BE_EMPTY = "keyword must not be empty";
  String GCS_SOURCE_IMAGE_FILE_NOT_FOUND = "Gcs source image file not found. filePath : %s ";
  String IMAGE_FILE_NOT_FOUND = "Image file not found. filePath : %s ";
  String ITEM_GDN_SKU_MUST_NOT_BE_BLANK = "itemGdnSku must not be blank";
  String ORDER_ITEM_STATUS_MUST_NOT_BE_BLANK = "orderItemStatus must not be blank";
  String PICKUP_POINT_CODE_MUST_NOT_BE_BLANK = "pickupPointCode must not be blank";
  String PRODUCT_GDN_SKU_MUST_NOT_BE_BLANK = "productGdnSku must not be blank";
  String CANCELLATION_ACTOR_MUST_NOT_BE_BLANK = "cancellationActor must not be blank";
  String MERCHANT_COMMISSION_TYPE_SHOULD_NOT_BE_NULL_OR_EMPTY = "MerchantCommissionType should not be null or empty";
  String ITEM_SKU_AND_PICKUP_POINT_CODE_MAP_MUST_NOT_BE_NULL = "ItemSkuAndPickupPointCodeMap must not be empty";
  String MIGRATION_TYPE_MUST_NOT_BE_EMPTY = "Migration type must not be empty";

  String INVALID_CHARACTERS_IN_PRODUCT_INFO = "Mohon gunakan karakter yang valid untuk info produk.";
  String OLD_BRAND_CODE_MUST_NOT_BE_EMPTY = "Old brand code must not be blank";
  String NEW_BRAND_CODE_MUST_NOT_BE_EMPTY = "New brand code must not be blank";
  String OLD_BRAND_CODE_AND_NEW_BRAND_CODE_SAME = "Old and new brand code are same";
  String CANNOT_DELETE_PICKUP_POINT = "Cannot Delete PickupPoint - Atleast One PickupPoint should be there";
  String EMPTY_PRODUCT_CODE = "Product code cannot be empty";
  String PRODUCT_SKU_NOT_VALID = "Product Sku not valid";
  String ITEM_SKU_NOT_VALID = "Item Sku not valid";

  String CHARACTER_LIMIT_REACHED_PLEASE_REDUCE_FORMATTING = "Character Limit : %s is reached, please reduce formatting.";
  String DEFAULT_PICKUPPOINT_SAME_AS_PICKUP_POINT = "Default pickup point is same as pickup point to be deleted";
  String RECIPE_IS_NOT_SAME_FOR_SHARED_PRODUCTS = "Product bundle recipe is not same across shared products";

  String SIZE_CHART_CODE_MUST_NOT_BE_BLANK = "sizeChartCode must not be blank";
  String DUPLICATE_DEFINING_ATTRIBUTE_VALUE_PRESENT = "Duplicate defining attribute present.";
  String SIZE_CHART_CODE_INVALID = "sizeChartCode is invalid";
  String INVALID_REQUEST = "Invalid Request";
  String STATE_MUST_NOT_BE_EMPTY = "State must not be empty";
  String SYSTEM_ERROR = "Error occurred on the system";
  String PP_DELETE_REQUEST_INVALID_FOR_WAREHOUSE_STOCK =
    "There are %s out of %s pickup points that can't be removed due to remaining stock on that pickup point.";
  String VARIANT_DELETE_REQUEST_INVALID_FOR_WAREHOUSE_STOCK =
    "There are %s out of %s variants that can't be removed due to remaining stock on these variants.";

  String ATTRIBUTE_VALUE_DELETED = "The attribute value does not exist anymore";
  String L5_DETAILS_NOT_VALID =
    "No L5 found with combination of itemSku and pickupPoint : %s from x-product";
  String REVIEW_TYPE_BLANK = "Review type cannot be empty or blank";
  String COGS_UPDATE_FAILED = "Cogs update not allowed for non distribution ppCode";
  String ERROR_PROCESSING_IMAGE_QC_RESPONSE = "Error processing image QC response";
  String BRAND_CODE_NOT_ALLOWED = "Invalid Brand Code, Value not Found";
}
