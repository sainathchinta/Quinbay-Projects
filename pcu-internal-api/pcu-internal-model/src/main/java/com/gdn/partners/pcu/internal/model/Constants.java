package com.gdn.partners.pcu.internal.model;

public interface Constants {

  String CHANNEL_ID = "channelId";

  String CLIENT_ID = "clientId";

  String STORE_ID = "storeId";

  String USER_NAME = "username";

  String REQUEST_ID = "requestId";

  String SIGNATURE = "Partners-Signature";

  String CONTEXT_PATH = "/pcu-internal-api";

  String SESSION = "SESSION";

  String BUSINESS_PARTNER_CODE = "businessPartnerCode";

  String EXTERNAL_USER = "EXTERNAL";

  String USER_TYPE = "userType";

  String USER_TYPE_INTERNAL = "INTERNAL";

  String USER_TYPE_EXTERNAL = "STORE";

  long REDIS_SESSION_TIMEOUT = 10;

  String COMMA_SEPARATOR = ",";

  String DEFAULT_FILTER_STATE = "ALL";

  String PREFIX_PRODUCT_REVIEWING = "MTA_SCREENING_";

  String PRODUCT_HISTORY_NOTES_REGEX = "\\{.*?\\}";

  String FIELD_HISTORY_REGEX = "(?<=field: ')(.*)(?=', oldValue:)";

  String OLD_VALUE_REGEX = "(?<=, oldValue: ')(.*)(?=', newValue:)";

  String NEW_VALUE_REGEX = "(?<=', newValue: ')(.*)(?='})";

  String CATEGORY_CHANGED = "Category Changed";

  String DOT = ".";
  int INDEX_COUNT = 1;

  String NOT_ALPHA_NUMERIC_REGEX = "[^A-Za-z0-9]";

  String HYPHEN = "-";

  String LOGO_EXT = "-logo.";

  String BANNER_EXT = "-banner.";

  String SLASH = "/";

  String PRODUCT_HISTORY_DESCRIPTION_FOR_UPDATE = "Diubah";

  String PRODUCT_ASSIGNMENT = "Reviewer Assigned to ";

  String PRODUCT_HISTORY_DESCRIPTION_FOR_ASSIGNMENT = "Ditugaskan";

  String PRODUCT_HISTORY_DESCRIPTION_FOR_UN_ASSIGNMENT = "Belum Ditugaskan";

  String PRODUCT_UN_ASSIGNMENT = "reviewer unassigned by ";

  String VENDOR_TYPE_CONTENT = "content";

  String VENDOR_TYPE_IMAGE = "image";

  String VENDOR_CODE = "vendorCode";

  String VENDOR_USER = "VENDOR";

  String VENDOR_UPDATE_TYPE_PATTERN = "^(content|image)$";

  String PRE_LIVE_STATUS = "Pre-live";

  String POST_LIVE = "Post-live";

  String ACTIVITY_REGISTERED_EN = "Registered";

  String ACTIVITY_REGISTERED_IN = "Terdaftar";

  String ACTIVITY_UPDATE_EN = "Update";

  String ACTIVITY_UPDATE_IN = "Diperbarui";

  String LANGUAGE = "en";

  String CONTENT_REVIEWERS = "CONTENT_REVIEWERS";

  String IMAGE_REVIEWERS = "IMAGE_REVIEWERS";

  String REVIEWERS = "REVIEWERS";

  String TYPE_CONTENT_REVIEW = "content";

  String APP_CHANNEL_ID = "App";

  String INTERNAL_PRODUCT_CENTER_ACCESSIBILITY = "INTERNAL_PRODUCT_PRODUCT-CENTER";

  String INTERNAL_RECAT_ACCESSIBILITY = "INTERNAL_PRODUCT_PRODUCT-CENTER-RECAT";

  String INTERNAL_STORE_COPY_ACCESSIBILITY = "INTERNAL_PRODUCT_PRODUCT-CENTER-COPY-STORE";
  String INTERNAL_BULK_PRICE_UPDATE_ACCESSIBILITY = "INTERNAL_PRODUCT_BULK_PRICE_UPDATE";

  String INTERNAL_UPDATE_SALES_CATEGORY_ACCESSIBILITY = "INTERNAL_PRODUCT_PRODUCT-CENTER-UPDATE-SALES-CATEGORY";

  String INTERNAL_UPDATE_BRAND_AUTHORISATION = "INTERNAL_UPDATE_BRAND_AUTHORISATION";

  String VENDOR_BULK_ASSIGN = "VENDOR-BULK-ASSIGN";

  String BULK_PROCESS_STATE_PENDING = "PENDING";

  String BULK_PROCESS_STATE_IN_PROGRESS = "IN_PROGRESS";

  String RECAT_REQUEST_CODE_KEY = "RE";

  String BULK_INTERNAL_PROCESS_CODE_KEY = "BIP";

  String MTA_APP = "MTAApp";

  String API_CHANNEL_ID = "Host-To-Host";

  String API_CLIENT_ID = "mta-api";

  String EXTERNAL_USER_MODE = "STORE";

  String INTERNAL_IMAGE_PREDICTION_ACCESSIBILITY = "INTERNAL_AUTO-QC-CONFIG";

  int FILENAME_MAX_LENGTH = 213;

  String STORE_COPY_FILE_NAME = "store_copy.xlsx";

  int PADDING_COUNT = 7;
  char PADDING_CONSTANT = '0';

  int ERROR_CODE = 400;

  String ASSIGNED_HISTORY_FORMAT = "Ditugaskan";

  String UNASSIGNED_HISTORY_FORMAT = "Not Assigned";
  String PRODUCT_SUSPENSION_TEMPLATE = "BulkSuspensionTemplate";
  String PRODUCT_REACTIVATION_TEMPLATE = "BulkReactivationTemplate";
  String PRODUCT_RECATEGORIZATION_TEMPLATE = "BulkRecatTemplate";
  String SALES_CATEGORY_UPDATE_TEMPLATE = "BulkSalesCategoryUpdateTemplate";
  String SELLER_CONFIG_UPDATE_TEMPLATE = "MerchantConfigUploadTemplate";
  String CATEGORY_CONFIG_UPDATE_TEMPLATE = "CategoryConfigUploadTemplate";
  String INTERNAL_BULK_UPDATE = "InternalBulkUpdateTemplate";
  String BRAND_AUTHORISATION_CREATE_TEMPLATE = "BrandAuthorisationCreateTemplate";
  String BRAND_AUTHORISATION_DELETE_TEMPLATE = "BrandAuthorisationDeleteTemplate";
  String ATTRIBUTE_NAME_MAPPING = "attribute_name";
  String ATTRIBUTE_VALUE_MAPPING = "attribute_value";
  String ACCEPTING_AUTO_APPROVED_PRODUCTS = "Terima produk yang disetujui secara otomatis";
  String SUSPENDING_AUTO_APPROVED_PRODUCTS = "Suspend produk yang disetujui secara otomatis";
  String AUTO_APPROVED_PRODUCT_REVIEWED = "Produk yang disetujui secara otomatis telah di-review";
  String CREATED_DATE = "createdDate";
  String ASC_SORT_ORDER = "ASC";
  String SPECIAL_CHARS_REGEX = "[^A-Za-z0-9]";
  String NEAR_EXPIRY = "NEAR_EXPIRY";
  int MAX_LENGTH_IPR_REGISTRATION_NUMBER = 100;
  String ROOT = "/";
  String MEDIUM_IMAGE = "medium-image";
  String THUMBNAIL_IMAGE = "thumbnail-image";
  int UPLOAD_FINAL_IMAGE_THUMBNAIL_WIDTH = 110;
  int UPLOAD_FINAL_IMAGE_THUMBNAIL_HEIGHT = 110;
  int UPLOAD_FINAL_IMAGE_MEDIUM_WIDTH = 380;
  int UPLOAD_FINAL_IMAGE_MEDIUM_HEIGHT = 380;
  String WEBP_FORMAT = ".webp";
  String NEW_IMAGE_TYPE = "new";
  String PRODUCT_CONSEQUENCE_LIMITATION = "productConsequenceLimitation";
}
