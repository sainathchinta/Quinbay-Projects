package com.gdn.x.product.constants;

/**
 * Created by govind on 22/04/2019 AD.
 */
public interface ErrorMessages {
  String ITEM_SKU_MUST_NOT_BE_BLANK = "Item sku must not be blank";
  String PRODUCT_SKU_MUST_NOT_BE_BLANK = "Product sku map must not be blank";
  String SIZE_CHART_CODE_MUST_NOT_BE_BLANK = "Size chart code must not be blank";
  String STORE_ID_MUST_NOT_BE_BLANK = "StoreId must not be blank";
  String USER_NAME_MUST_NOT_BE_BLANK = "userName must not be blank";
  String REQUEST_ID_MUST_NOT_BE_BLANK = "requestId must not be blank";
  String ITEM_PICKUPPOINT_VIEW_CONFIG_MUST_NOT_BE_NULL = "itemPickupPointViewConfigBaseRequest must not be null";
  String FIELD_NAME_MUST_NOT_BE_BLANK = "Field must not be blank";
  String VALUE_MUST_NOT_BE_NULL = "Value must not be null";
  String PICKUP_POINT_CODE_MUST_NOT_BE_BLANK = "pickupPointCode must not be blank";
  String PROMO_DISCOUNT_EVENT_STATUS_MUST_NOT_BE_INACTIVE = "Promo Discount Event Status must not be inactive";
  String PROMO_DISCOUNT_EVENT_STATUS_MUST_NOT_BE_ACTIVE = "Promo Discount Event Status must not be active";
  String ITEM_MUST_NOT_BE_NULL = "Item must not be null";
  String ITEM_AND_OFFLINE_ITEM_MUST_NOT_BE_NULL = "Both item and offline item shouldn't be null";
  String PRODUCT_MUST_NOT_BE_NULL = "Product must not be null";
  String PRODUCT_CODE_AND_PRODUCT_SKU_MUST_NOT_BE_NULL = "productSku and productCode both can't be empty";
  String ITEMS_MUST_NOT_BE_NULL = "Items must not be empty";
  String BLANK_LINKED_PARTNER_CODE = "linked partner code must not be blank";
  String DISABLED_MASTER_DATA_UPDATE = "Update of master data disabled for this product";
  String PRICE_CANNOT_BE_EDITED_FOR_ITEM =
      "Failed to change the product price because it's already registered on another promo";
  String DUPLICATE_UPSERT_OFFLINE_ITEM_ERROR = "Duplicate UpsertOfflineItem Error";
  String PRODUCT_AND_ITEMS_NOT_VIEWABLE = "The product/items being fetched are not retrievable at the moment";
  String NEW_BRAND_VALUE_OR_CODE_MUST_NOT_BE_NULL = "Update of product brand failed due to null brand value";
  String TOGGLE_ARCHIVE_SAME_STATE = "Item is already in archived state for itemSku: ";
  String TOGGLE_UNARCHIVE_SAME_STATE = "Item is already in unarchived state for itemSku: ";
  String NO_VALID_ITEM_FOUND = "No valid item found mapped to this pristine Id : {}";
  String NO_PRODUCT_FOUND = "No product found for this item sku : {}";
  String NO_ITEM_FOUND = "No item found for this item sku : %s";
  String EMPTY_BUSINESS_PARTNER_LIST = "Business partner list cannot be empty";
  String EMPTY_ITEM_LIST_FOR_PRODUCT_SKU = "No items found for given product sku input";
  String REINDEX_COUNT_LIMIT_ZERO = "Requesting to reindex 0 products. Please check the input";
  String REINDEX_SUCCESS_PRODUCT =
      "Requesting to reindex products which is already success. Please check the input";
  String FAILED_TO_FETCH_INVENTORY_INFO_BY_L3 =
      "Failed to fetch inventory info for input product skus";
  String FAILED_TO_FETCH_OOS_INFO_BY_L3 =
      "Failed to fetch OOS inventory info for product sku";
  String FAILED_TO_FETCH_PRODUCT_PROFILE =
      "Failed to fetch Product profile for input business partner code";
  String EITHER_ITEM_SKU_OR_PRODUCT_SKU_MUST_NOT_BE_NULL = "Both itemSku and productSku are null";
  String ITEM_COUNT_LIMIT_FOR_TRANSACTION_API = "Crossed items count limit for transaction API";
  String ACTIVATION_DETAIL_CONNOT_BE_EMPTTY = "Activation detail can not be empty";
  String START_DATE_AND_END_DATE_SHOULD_NOT_BE_EMPTY = "start date and end date should not be empty";
  String FAILED_TO_FETCH_PCB_DATA = "Error fetching data from PCB ";
  String EMPTY_SKU_CODE_LIST = "Input sku code list is empty";
  String SELLER_DETAIL_FETCH_ERROR = "Failed to fetch sellet details from product analytics system";
  String PRODUCT_TYPE_ERROR_MESSAGE = "Found more than one product sku mapped to same product";
  String ITEM_PICKUP_POINT_MUST_NOT_BE_NULL = "Item pickup point must not be null";
  String ITEM_PICKUP_POINT_MUST_NOT_BE_EMPTY = "item pickup point must not be empty";
  String MERCHANT_CODE_MUST_NOT_BE_BLANK = "merchant code must not be blank";
  String CANNOT_CHANGE_PICKUP_POINT_FOR_THE_SKU_AS_IT_IS_BOTH_DELIVERY_AND_CNC_TRUE =
    "Cannot change pickup point for the sku as its is both delivery and cnc true";
  String ITEM_PICKUP_POINT_LIST_MUST_NOT_BE_EMPTY = "Item pickup point list must not be empty.";
  String OFFLINE_ITEM_LIST_MUST_NOT_BE_EMPTY = "Offline item list must not be empty.";

  String ITEM_LIST_NOT_EMPTY = "Item list must not be empty";

  String ITEM_LIST_TOO_LARGE = "Item list request is too large";
  String MERCHANT_CODE_EMPTY = "Merchant code should not be empty";
  String OFFLINE_ITEM_ID_MUST_NOT_BE_BLANK = "Offline item id must not be blank";
  String OFFLINE_ITEM_IDS_MUST_NOT_BE_EMPTY = "Offline item ids must not be empty";
  String PRODUCT_SKU_EMPTY = "Product Sku must not be empty";
  String FAILED_TO_UPDATE_OFFLINE_ITEM_PRICE = "failed to update offline item price";
  String ITEM_LISTING_UPDATE_REQUEST_MUST_NOT_BE_EMPTY =
    "Item Pickup Point listing update request must not be empty";
  String BUSINESS_PARTNER_CODE_MUST_NOT_BLANK = "business partner code must not be blank";
  String FILTER_SUMMARY_REQUEST_MUST_NOT_BE_NULL = "filter summary request must not be null";
  String INVALID_ITEM_SKU_FORMAT = "Invalid item sku format ";
  String PICKUP_POINT_NOT_ELIGIBLE_FOR_CNC = "pickup point code is not eligible for cnc";
  String INVALID_PICKUP_POINT = "invalid pickup point code error";
  String INVALID_BUSINESS_PARTNER_CODE = "Invalid business partner code";
  String ITEM_AKU_AND_PICKUP_POINT_CODE_MUST_NOT_BE_BLANK = "itemSku and pickupPointCode must not be blank";
  String ITEM_PICKUP_POINT_LISTING_REQUEST_CANNOT_BE_NULL = "item pickup point listing request cannot be null";
  String DELETE_NOT_ALLOWED = "Delete not allowed, product will be left with no active L5 ";
  String ITEM_CODE_OR_KEYWORD_MUST_NOT_BE_BLANK = "item code ot keyword must not be blank";
  String PRISTINE_ID_MUST_NOT_BE_BLANK = "pristine id must not be blank";
  String CANNOT_FIND_ITEM_PICKUP_POINT_BY_ITEM_SKU_AND_PICKUP_POINT_CODE = "Cannot find ItemPickupPoint by itemSku = %s and pickupPointCode %s";
  String CANNOT_FIND_ITEM_BY_ITEM_SKU = "Cannot find Item by itemSku = %s";
  String CANNOT_FIND_PRODUCT_BY_PRODUCT_SKU = "Cannot find Product by itemSku = %s";
  String CANNOT_FIND_ITEM_PICKUP_POINT_FOR_THE_REQUEST = "Cannot find ItemPickupPoint for the request";
  String ITEM_VIEW_CONFIG_MUST_NOT_BE_NULL = "itemViewConfig must not be null";
  String CAN_NOT_DELETE_L5_EN = "Failed to delete this variant because you haven't activated at least 1 pickup point. The status for this variant has been changed to offline.";
  String CAN_NOT_DELETE_L5_ID = "Gagal menghapus varian ini karena Anda belum mengaktifkan setidaknya 1 alamat pengambilan. Status varian ini telah berubah menjadi offline.";
  String FREE_SAMPLE_MUST_NOT_BE_BUYABLE_OR_DISCOVERABLE = "Free sample cannot be updated as buyable or discoverable";
  String PRODUCT_CODE_MUST_NOT_BE_BLANK = "Product code must not be blank";
  String SALES_CATALOG_MUST_NOT_BE_NULL = "Sales Catalog must not be null";
  String PRODUCT_ARCHIVE_LIST_MUST_NOT_BE_EMPTY = "Product archive list must not be empty";
  String PRODUCT_SKU_SET_MUST_NOT_BE_EMPTY = "Product sku set must not be empty";
  String ITEM_ARCHIVE_LIST_MUST_NOT_BE_EMPTY = "Item archive list must not be empty";
  String ITEM_PICKUP_POINT_ARCHIVE_LIST_MUST_NOT_BE_EMPTY = "ItemPickupPointArchiveList must not be empty";
  String PRODUCT_NOT_FOUND_ERROR = "Product not found error. productCode = %s .";
  String PRODUCT_ALREADY_EXIST_WITH_THE_SELLER_SKU = "Product with the seller sku already exist";
  String FREE_SAMPLE_MUST_NOT_BE_BUYABLE_OR_DISCOVERABLE_OR_CNC =
      "Free sample product cannot be updated as buyable or discoverable or cnc";
  String CANNOT_FIND_ITEM_BY_PRODUCT_SKU_AND_L5_MAP =
    "Failed to Fetch Items Response by ProductSku = %s and pickupPointCode = %s";
  String ITEM_PICKUP_POINT_REQUEST_BEYOND_MAX_FETCH_SIZE =
    "Failed to Serve Item PickupPoint Request for fetch size = %s , permitted fetch size is = %s";
  String DELETE_PICKUPPOINT_NOT_ALLOWED = "Delete not allowed, Item will be left with no active L5 ";

  String DELETE_PICKUPPOINT_NOT_ALLOWED_DEFAULT_PICKUP_POINT_EMPTY = "Delete not allowed, default pickup point empty";
  String CLIENT_EXCEPTION_ERROR_MESSAGE = "Client Exception when calling Feign";
  String FBB_PICKUP_POINT_ALREADY_EXISTS = "Fbb pickup point already exists";
  String PAGE_SIZE_LIMIT_EXCEEDED_ERROR = "Page size should not be greater than %s";
  String START_DATE_SHOULD_BE_LESSER_THAN_END_DATE = "StartDate should be lesser than endDate";
  String MASTER_DATA_IS_NOT_FOUND_FOR_PRODUCT = "Master data not found for product product code - %s";
  String INVALID_ITEM_SUMMARY_REQUEST = "All fields should not be null in the request";
  String ITEM_SKU_LIST_NOT_EMPTY = "Item sku list must not be empty";
  String FAILED_TO_FETCH_PRODUCT_BUNDLE_RESPONSE = "Failed to fetch product bundle response from warehouse item master system";
  String CYCLIC_BUNDLE = "Requested bundle is a cyclic bundle";
  String ITEM_SKU_LIST_SIZE_LIMIT_EXCEEDED_ERROR = "Item sku list size should not be greater than %s";
  String ITEM_NOT_FOUND = "No valid item found";
  String PRODUCT_NOT_FOUND_WITH_PRODUCT_CODE = "product not found with product code : ";
  String ERROR_WHILE_DELETING_PP_CODE_FROM_INVENTORY = "Error while deleting pickup point code from inventory";
  String DELETE_TERMINATED_SELLER_PRODUCT_EVENT_MODEL_CANNOT_BE_NULL = "Delete terminated seller product event model cannot be null";
  String RECIPE_IS_NOT_SAME_FOR_SHARED_PRODUCTS = "Product bundle recipe is not same across shared products";
  String CATEGORY_ID_NOT_FOUND_IN_PCB_RESPONSE =
    "Category ID was null in product detail Response from PCB for product - %s";

  String SIZE_CHART_CODE_MUST_NOT_BE_NULL = "Size chart code must not be blank";
  String PRODUCT_PRESENT_IN_BOTH_ADD_AND_REMOVAL_LIST_OF_SIZE_CHART = "Product present in both add and removal list of size chart";
  String INVALID_SIZE_CHART_CODE = "Invalid size chart code %s";
  String NOT_ALL_PRODUCTS_IN_SIZE_CHART_UPDATE_REQUEST_EXISTS = "Not all products in size chart update request exists";
  String DIFFERENT_SIZE_CHART_CODE_PRESENT_IN_PRODUCT = "Different size chart code present in the product";
  String BOTH_ADD_SKUS_AND_REMOVE_SKUS_IS_EMPTY_FOR_SIZE_CHART_UPDATE = "Both add skus and remove skus is empty for size chart update";
String SIZE_CHART_CODE_MUST_NOT_BE_EMPTY = "Size chart must not be empty";
  String EANUPC_CODE_MUST_NOT_BE_BLANK = "EanUpc code must not be blank";
  String VIDEO_ID_MUST_NOT_BE_BLANK = "Video id must not be blank";
  String FINAL_URL_MUST_NOT_BE_BLANK = "Final url must not be blank";
  String DISTRIBUTION_PICKUP_POINT_DELETE_NOT_ALLOWED =
      " distribution pickup point Delete not allowed";
}
