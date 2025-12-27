package com.gdn.mta.product.enums;

public enum ApiErrorCode {
  /**
   * 400 - because bad client request, it doesn't meet required API standard
   * 401 - because unauthorized issue, API token is used by different username
   * 403 - because specified client doesn't have permission for a source
   * 410 - because client access unavailable resource
   * 412 - because invalid credential that sent by client
   * 416 - because client ask excessive portion of data, it exceed the allowed limit
   * 423 - because client authentication is suspended (locked, inactive or expired)
   * 429 - because too many request issue, client total request exceed allowed limit
   * 430 - because client is not recognize (unregistered)
   *
   * 500 - because system error, it can't handle specified error
   * 520 - because unexpected error in RestSecurityFilter
   */

  FAMILY_COLOUR_NOT_FOUND("ERR-PB400001", 400, "Family Colour Field must not be null"),
  MANDATORY_FIELD_NOT_FOUND("ERR-PB400002", 400, "Mandatory Field value must not be empty"),
  WHOLESALE_QUANTITY_DUPLICATE("ERR-PBP400003", 400, "wholesale quantity is duplicate"),
  WHOLESALE_QUANTITY_LESS_THAN_2("ERR-PBP400004", 400, "wholesale quantity is lower than 2"),
  WHOLESALE_DISCOUNT_LTE_ZERO("ERR-PBP400005", 400, "wholesale discount is lte 0"),
  WHOLESALE_DISCOUNT_LTE_MINIMUM_DISCOUNT("ERR-PBP400005", 400, "wholesale discount under minimum discount"),
  WHOLESALE_DISCOUNT_SETTING_GTE_5("ERR-PBP400005", 400, "wholesale total setting per SKU is exceed maximum value"),
  DIFFERENT_THRESHOLD_ERROR_CODE("WARN_PBP400001", 200, "We'll turn off wholesale price for this product because minimum "
      + "percentage discount is different. Kindly check the detail on wholesale section"),
  SAME_THRESHOLD_ERROR_CODE("WARN_PBP400002", 200, "Wholesale price got updated"),
  WHOLESALE_PRICE_TIER_RULE_VIOLATION ("ERR-PBP400006", 400, "Wholesale discount of tier 1 should be less than following tiers"),
  WHOLESALE_DISCOUNT_DUPLICATE("ERR-PBP400007", 400, "wholesale discount is duplicate"),
  LOGISTICS_DATA_NOT_SAVED("ERR-PBP400008", 400, "Logistics Data is not saved"),
  SHIPPING_WEIGHT_EXCEEDED("ERR-PBP400009", 400, "Shipping weight exceeds than 50 Kg for regular product"),
  PICKUP_POINT_NOT_SAVED("ERR-PBP400010", 400, "Pickup point not saved"),
  ITEM_IS_SUSPENDED("ERR-PBP100001", 400, "Product is suspended"),
  ITEM_IS_REJECTED("ERR-PBP100002", 400, "Product is rejected"),
  ITEM_IS_ARCHIVED("ERR-PBP100003", 400, "Item is archived"),
  WHOLESALE_VALIDATION_FAILED("ERR-PBP100004", 200, "Wholesale validation failed"),
  WHOLESALE_PRICE_UPDATE_FAILED("ERR-PBP100005", 200, "Wholesale price update failed"),
  PRICE_UPDATE_FAILED("ERR-PBP100006", 200, "Price update failed"),
  IMAGE_UPDATE_FAILED("ERR-PBP100008", 200, "Image update failed"),
  STOCK_UPDATE_FAILED("ERR-PBP100007", 200, "Stock update failed"),
  FAULTY_IMAGE_FORCE_REVIEW("ERR-PBP100012", 400, "Product has faulty image"),
  PRODUCT_IS_TAKEN_DOWN("ERR-PBP100013", 400, "Product is taken down"),
  INVALID_STATE("ERR-PBP100009", 200, "Product content is expired"),
  UPC_CODE_UPDATE_FAILED("ERR-PBP100010",200, "EAN/UPC code update failed, EAN/UPC code already exist for Item "),
  WHOLESALE_FLAG_UPDATE_FAILED("ERR-PBP100011", 200, "Wholesale flag update failed"),
  PRODUCT_ID_MAPPED_MORE_THAN_1_L3("ERR-PBP400011", 400, "Product id is mapped to more than 1 L3"),
  PRODUCT_NOT_PRESENT("ERR-PBP400012", 400, "Product not present"),
  SHIPPING_WEIGHT_NOT_VALID("ERR-PBP400013", 400, "Shipping weight cannot be 0 for regular or big product"),
  INVALID_LOGISTICS_OPTION_SELECTED("ERR-PBP400014", 400, "Selected logistics option not allowed for the present pickup points"),
  DIMENSION_UPDATE_FAILED("ERR-PBP400015", 400, "Dimension update failed"), IMAGE_QC_CONFIG_VALUE_GT_CONFIDENCE_THRESHOLD(
      "ERR-PBP400015", 500, "Image qc config value is greater than confidence threshold"),
  MAXIMUM_STOCK_LIMIT_EXCEEDED("ERR-PBP400016", 400, "Maksimal stok 10000000"),
  MAXIMUM_SELLER_SKU_EXCEEDED("ERR-PBP400017", 400, "Maksimal Seller Sku 255"),
  PRODUCT_ATTRIBUTE_INVALID("ERR-PBP400018", 400, "Invalid product attribute update"),
  PRODUCT_NAME_IS_EMPTY("ERR-PBP400019", 400, "Product Name is empty"),
  PICKUP_POINT_INVALID("ERR-PBP400020", 400, "Pickup point invalid"),
  INVALID_DATA_INPUT("ERR-PBP400022", 400, "Invalid data input"),
  PRODUCT_HAS_ORDER("ERR-PBP400023", 400, "Product has order"),
  PRODUCT_PREORDER_INVALID("ERR-PBP400021", 400, "Product Preorder invalid"),
  CANNOT_UPDATE_IMAGE_FOR_SHARED_PRODUCT("ERR-PBP400024", 400, "cannot update image for shared product"),
  CANNOT_DELETE_MAIN_IMAGE("ERR-PBP400025", 400, "Cannot delete main image"),
  IMAGE_ALREADY_EXISTS("ERR-PBP400026", 400, "Image already exists in the item"),
  MAX_IMAGE_REACHED("ERR-PBP400027", 400, "Item already has max images"),
  IMAGE_NOT_PRESENT("ERR-PBP400028", 400, "Image not present in item"),
  ONLY_IMAGE_IN_ITEM("ERR-PBP400029", 400, "Cannot delete only image in item"),
  FREE_SAMPLE_CANNOT_BE_SET("ERR-PBP400030", 400, "Free sample cannot be true for preorder or instore product"),
  WARNA_AND_FAMILY_COLOR_ERROR("ERR-PBP400031", 400, "Both warna and family color should be present"),
  ALL_VARIANTS_ERROR("ERR-PBP400032", 400, "Could not perform action to all variants due to validation error"),
  WHOLESALE_CANNOT_BE_SET_TO_FREE_SAMPLE(
    "ERR-PBP400033", 400, "Wholesale product cannot be set as free sample "),
  MULTI_PICKUP_POINT_ENABLED_ERROR("ERR-PBP400033", 400,
    "Seller has multi pickup point enabled, cannot proceed with the request"),
  MISSING_ITEM_PICKUP_POINT("ERR-PBP400034", 400, "Couldn't find itemPickupPoint for given "
    + "itemSku and pickupPointCode"),
  PRODUCT_NOT_FOUND("ERR-PBP400035", 400, "Product not found "),
  ITEM_PICKUP_POINT_NOT_FOUND("ERR-PBP400036", 400, "Item pick up point not found "),
  PRODUCT_IS_ARCHIVED("ERR-PBP100037", 400, "Product is archived "),
  REQUIRED_L4_DATA_NOT_FOUND_IN_DB("ERR-PBP100039", 400, "The required L4 data is not found in db"),
  PICKUP_POINT_IS_ALREADY_PRESENT_IN_DB("ERR-PBP100040", 400, "Cannot add pickupPoint as the pickupPoint is already present"),
  OPERATION_NOT_PERMITTED("ERR-PBP100038", 400, "This action is not permitted to the seller "),
  PRODUCT_DELETED_STATE("ERR-PBP100039", 400, "This product is in deleted state"),
  FREE_SAMPLE_STATUS_CHANGE_ERROR("ERR-PBP100040", 400,
    "Status change not permitted for Free sample Products"),
  FREE_SAMPLE_CNC_UPDATE_ERROR(
    "ERR-PBP100041", 400, "Free sample product cannot be CNC Active"),
  PICKUP_POINT_IS_NOT_VALID(
      "ERR-PBP100042", 400, "Pickup point code is not valid"),
  MINIMUM_PRICE_VALUE_INVALID(
      "ERR-PBP100043",400,"price and sale price must be greater than "),
  ITEM_IS_IN_NEED_CORRECTION("ERR-PBP100045", 400, "Product is in need correction state"),
  ITEM_IS_IN_REVIEW("ERR-PBP100046", 400, "Product is in review state"),
  FAILED_TO_UNARCHIVE_NO_ACTIVE_L5(
      "ERR-PBP100044",400,"Product cannot be activated since it has no active L5's associated with it "),
  ONLY_ONE_FBB_PICKUP_POINT_ALLOWED_PER_L4(
      "ERR-PBP100047",400,"Only 1 FBB pickup point allowed per L4 "),
  SELLER_NOT_ALLOWED_TO_CREATE_FBB_PRODUCTS(
      "ERR-PBP100048",400,"Seller not allowed to create FBB products "),
  PICKUP_POINT_IS_NOT_FBB_ENABLED(
      "ERR-PBP100049",400,"Pickup point is not fbb enabled "),
  FAILED_TO_FETCH_DEFAULT_FBB_PICKUP_POINT("ERR-PBP100050",400,"Failed to get default pickup point for L4"),
  FAILED_TO_ADD_STOCK_FOR_DEFAULT_FBB_PICKUP_POINT("ERR-PBP100051",400,"Failed to insert stock for default fbb L5"),
  ITEM_PICKUP_POINT_ALREADY_EXISTS("ERR-PBP400050", 400, "New pickup point cannot be added as it "
    + "already exists in item"),
  DIMENSION_LESS_THAN_ZERO("ERR-PBP100052", 400,"Dimension must be greater than 0"),
  PREORDER_DAYS_LESS_THAN_ZERO("ERR-PBP100053",400, "Pre-order days must be greater than 0."),
  PREORDER_WEEK_LESS_THAN_ZERO("ERR-PBP100054",400, "Pre-order week must be greater than 0."),
  PREORDER_DAYS_EXCEEDED_LIMIT("ERR-PBP100055", 400, "pre order days must not exceed {0} days - Max {0} working days (Mon–Fri)"),
  PREORDER_WEEK_EXCEEDED_LIMIT("ERR-PBP100056", 400, "pre order week must not exceed {0} weeks "
    + "Max {0} weeks (Working days Mon-Fri)"),
  PREORDER_MAX_TIME_EXCEEDED("ERR-PBP100057", 400,"Maximum pre-order time is {0} days."),
  PREORDER_DATE_BEFORE_CURRENT_DATE("ERR-PBP100058", 400, "Pre-order time must be later than the "
    + "current date"),
  INVALID_PREORDER_TYPE("ERR-PBP100059", 400, "Pre-order time must be filled with day, week or "
    + "date."),
  PREORDER_DATE_LIMIT_EXCEEDED("ERR-PBP100060" , 400, "pre order date must not be greater than {0} days"),
  B2B_INVALID_BASE_PRICE("ERR-PBP100043", 400,"Base price for BFB must be greater than 0 "),
  INVALID_ADD_DELETE_REQUEST("ERR-PBP100061", 400, "Invalid add delete variant request "),
  BRAND_CANNOT_BE_CHANGED("ERR-PBP100097",400,"brand cannot be changed"),
  INVALID_ADD_DELETE_DUPLICATE_REQUEST("ERR-PBP100062", 400, "Invalid add delete variant request,duplicate items addition "),
  DIMENSION_EXCEEDED_THRESHOLD("ERR-PBP100063", 400, "Invalid product dimension, dimension must not exceed {0}"),
  BP_BOPIS_ELIGIBILITY_ERROR("ERR-PBP100070" , 400, "Anda tidak bisa menggunakan tipe pengiriman ini. Silahkan hubungi Seller Care untuk info lengkap."),
  EMPTY_ATTRIBUTE_VALUE("ERR-PBP100071", 400, "Attribute value cannot be empty"),
  INVALID_ADD_REQUEST("ERR-PBP100072", 400, "Invalid add request for the attribute"),
  INVALID_DELETE_REQUEST("ERR-PBP100072", 400, "Invalid delete request for the attribute"),
  DUPLICATE_UPC_CODE("ERR-PBP100073", 400, "EAN/UPC code duplicate value not allowed"),
  DELETE_NOT_ALLOWED_FOR_LAST_ATTRIBUTE_VALUE(
      "ERR-PBP100074", 400, "Cannot delete last attribute value"),
  INVALID_LOCATION_PATH("ERR-PBP100087",400,"Invalid location path"),
  BUNDLING_INFO_NOT_PRESENT_FOR_VARIANT("ERR-PBP100075", 400, "Bundling information is not present for variant"),
  BUNDLE_RECIPE_MAX_SKUS("ERR-PBP100076", 400, "Bundling recipe can have maximum 10 skus"),
  BUNDLE_RECIPE_CHILD_PRODUCTS_SHOULD_BE_AT_VARIANT_LEVEL("ERR-PBP100077", 400, "Bundling recipe child products should be variant level"),
  BUNDLE_RECIPE_CHILD_PRODUCT_NOT_FOUND("ERR-PBP100078", 400, "Bundling recipe child product %s not found"),
  BUNDLE_RECIPE_CHILD_IS_NOT_ACTIVATED("ERR-PBP100079", 400, "Bundling recipe child product %s not activated"),
  BUNDLE_RECIPE_CHILD_IS_NOT_TRADING_PRODUCT("ERR-PBP100080", 400, "Bundling recipe child product not trading product"),
  BUNDLE_RECIPE_CHILD_DOES_NOT_BELONG_TO_CURRENT_SELLER("ERR-PBP100081", 400, "Bundling recipe child product don't belong to current seller"),
  BUNDLE_RECIPE_CHILD_IS_DELETED("ERR-PBP100082", 400, "Bundling recipe child product is not valid"),
  INVALID_SALE_PRICE("ERR-PBP100098",400,"Invalid Sale Price, Sale price is greater than regular price"),
  NAME_EDIT_NOT_ALLOWED("ERR-PBP100099", 400, "Product Name Edit Is Not Allowed"),
  BUNDLE_RECIPE_LESS_THAN_EQUAL_TO_ZERO_QUANTITY("ERR-PBP100100", 400, "Bundle recipe quantity should be greater than to 0"),
  BUNDLE_RECIPE_CHILD_SKU_IS_SAME_AS_PARENT_SKU("ERR-PBP100101", 400, "Bundle recipe child sku is same as parent sku"),
  INVALID_ITEM_SKU("ERR-PBP100102", 400, "Invalid item sku present in the bundle recipe"),
  ATTRIBUTE_MAP_CONTAIN_DIFFERENT_ATTRIBUTE_FOR_NEWLY_ADDED_ITEM("ERR-PBP100103", 400, "Attribute map contains different attribute across newly added items"),
  PRODUCT_LIMIT_REACHED(
      "ERR-PBP100104", 400, "Anda telah mencapai batas produk untuk toko Anda. Silakan hapus/arsipkan produk untuk meng-upload produk baru."),
  NEGATIVE_STOCK_NOT_ALLOWED("ERR-PBP100105", 400, "Negative stock not allowed"),
  ITEM_PICKUP_POINT_NOT_FOUND_FOR_NEWLY_ADDED_VARIANTS("ERR-PBP100106", 400, "Item pickup point details not found for newly added items"),
  NO_IMAGE_FOUND_FOR_NEWLY_ADDED_VARIANT("ERR-PBP100107", 400, "No images found for newly added variants"),
  APPEAL_LIMIT_CROSSED("ERR-PBP-100108", 400, "Appeal product limit %s crossed"),
  PRODUCT_IN_INVALID_STATE("ERR-PBP-100109",400,"Product is in invalid state"),
  INVALID_SCHEDULE_DATE_TIME("ERR-PBP100108", 400,
    "Schedule Start should exceed current date and end date should be after start date"),
  SCHEDULE_NOT_ALLOWED_FOR_FREE_SAMPLE("ERR-PBP100109", 400,
      "Addition of schedules to Free sample products is not permitted"),
  SCHEDULE_INVALID_FOR_OFFLINE_PRODUCT("ERR-PBP100110", 400,
      "Shipping should be enabled at product to update schedule config at L5"),
  SCHEDULE_DATE_INCOMPLETE("ERR-PBP100111" ,400 , "Start and End Dates are mandatory for addition of Schedules"),
  INVALID_ADD_L5_REQUEST("ERR-PBP100112" ,400 , "Invalid itemSku in L5 add request"),
  ITEM_ATTRIBUTE_VALUE_INVALID("ERR-PBP100112", 400, "Item attribute value invalid"),
  PRODUCT_TYPE_NULL("ERR-PBP100117", 400, "Product Type cannot be null"),
  DUPLICATE_ATTRIBUTE_VALUE_NOT_ALLOWED("ERR-PBP100113", 400, "Duplicate attribute values are not allowed."),
  MULTI_VALUE_TYPE_NOT_ALLOWED("ERR-PBP100114", 400, "All items should belong to the same value type."),
  SCHEDULE_SUPPORTED_ONLY_FOR_OFFLINE_L5("ERR-PBP100115", 400, "Schedule setting is supported for offline L5, please make L5 offline and proceed."),
  SIZE_CHART_CODE_INVALID("ERR-PBP100116", 400, "Invalid SizeChart Code"),
  WHOLESALE_DISCOUNT_SHOULD_BE_BETWEEN_0_AND_100("ERR-PBP100116", 400, "Wholesale discount should be above 0 and less than 100 percent"),
  INVALID_BUSINESS_PARTNER_CODE("ERR-PBP100117", 400, "Invalid business partner code"),
  INSTORE_STATUS_CHANGE_ERROR("ERR-PBP100125", 400, "Cannot make product online,"
    + " Dimensions/Description is missing."),
  BOPIS_STATUS_CHANGE_ERROR("ERR-PBP100118", 400, "Cannot make product online , Please add appropriate Dimensions before making product online."),
  BOPIS_CNC_CHANGE_ERROR("ERR-PBP100119", 400, "Shipping Type BOPIS is not allowed for CNC product, either select different shipping type or mark CNC as false."),
  BOPIS_EDIT_NOT_SUPPORTED("ERR-PBP100120", 400, "Edit not allowed, Shipping type updated to BOPIS by Internal User, please change Shipping from product detail page to Proceed."),
  INVALID_PRODUCT_ITEM_STATUS("ERR-PBP100113", 400, "Invalid cnc and shipping product status combination detected"),
  PICKUP_POINT_NOT_CNC_ACTIVE("ERR-PBP100123", 400, "Selected pickup point is not active for cnc"),
  PICKUP_POINT_NOT_DELIVERY_ACTIVE("ERR-PBP100124", 400, "Selected pickup point is not active for delivery"),
  FAAS_SELLER_SYNC_STOCK_CHANGE_ERROR("ERR-PBP100121", 400, "Sync stock true not allowed for faas seller"),
  SUCCESS_VALIDATION(null, 200, "Validation successful"),
  INVALID_DELIVERY_CNC_VIEW_CONFIG_STATES("ERR-PBP100112", 400, "Item's CNC and delivery view config combinations are not allowed"), VARIANT_DELETE_REQUEST_INVALID_FOR_WAREHOUSE_STOCK(
    "ERR-PBP100125" , 400,  "The existing variant can't be deleted because there's still stock at selected pickup points. You can still add a new variant."),
  INVALID_PREORDER_TYPE_OMG("ERR-PBP100113", 400, "Invalid pre-order type for OMG seller, please select only date type"),
  INVALID_STOCK_UPDATE_PREORDER_OMG("ERR-PBP100114", 400, "Invalid stock update for OMG pre-order product, please update only pre-order quota"),
  SELLER_PENALTY_RESTRICTION(
    "ERR-PBP100122", 400, "Kami membatasi akses Anda untuk menambah produk karena telah mencapai "
    + "batas poin penalti."),
  MANDATORY_OMNI_CHANNEL_SKU(
      "ERR-PBP100126", 400, "OmniChannel Sku mandatory for distribution L5"),
  OMNI_CHANNEL_SKU_ALREADY_EXISTS_OR_DUPLICATE(
      "ERR-PBP100127", 400, "OmniChannel Skuis duplicate or already exists for a different existing product"),
  INCOMPLETE_DISTRIBUTION_INFO(
      "ERR-PBP100128", 400, "Distribution Info is incomplete"),
  SYSTEM_ERROR(
      "ERR-PBP100129", 500, "System error"),
  CONVERSION_OR_UOM_DELETION_NOT_ALLOWED_WAREHOUSE("ERR-PBP100130",400,"Conversion factor or UOM deletion not allowed as product has warehouse stock"),
  CONVERSION_OR_UOM_DELETION_NOT_ALLOWED_OMS("ERR-PBP100131",400,"Conversion factor or UOM deletion not allowed as product has pending PO"),
  BOPIS_CATEGORY_ELIGIBILTY_ERROR("ERR-PBP100132",400,"This category does not support the BOPIS shipping type"),
  ADD_DELETE_VARIANTS_NOT_ALLOWED_DISTRIBUTION_PRODUCT("ERR-PBP100133",400,"Add delete variants not allowed for a distribution product"),
  CATEGORY_EDIT_NOT_ALLOWED_SINCE_ORDER_IS_PRESENT("ERR-PBP100134", 400, "You can't change the category because you have ongoing order right now."),
  BRAND_EDIT_NOT_ALLOWED_SINCE_ORDER_IS_PRESENT(
      "ERR-PBP100135", 400, "You can't change the brand because you have ongoing order right now."),
  DISTRIBUTION_L5_CANNOT_BE_ADDED_FROM_THIS_FLOW(
      "ERR-PBP100136", 400, "Distribution L5 cannot be added from this flow.");


  private final String code;
  private final int httpStatus;
  private String desc;
  private int configuredValue;

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

  public int getMaxDays() {
    return configuredValue;
  }

  public void setConfiguredValue(int configuredValue) {
    this.configuredValue = configuredValue;
    this.desc = this.desc.replace("{0}", String.valueOf(configuredValue));
  }

  public int getHttpStatus() {
    return httpStatus;
  }

}
