package com.gdn.partners.pbp.commons.constants;

import java.util.Arrays;
import java.util.List;
import java.util.Set;

/**
 * Created by hardikbohra on 14/06/18.
 */
public interface Constants {

  String REQUIRED_REACT_NAME = "required field recategorization name is empty";
  String REQUIRED_FILE_PATH = "required field file path is empty";
  String REQUIRED_ID = "required field id is empty";
  String REQUIRED_STATUS = "required field status is empty";
  String REQUIRED_PRODUCT_CODES = "ProductCodes should not be null";
  String BULK_ARCHIVE_LIMIT_ERROR_MESSAGE = "Limit on number of products to archive set to 0";
  String DESCRIPTIVE_MANDATORY_MUST_NOT_NULL = "Descriptive mandatory field should not be null";
  String DEFINING_ATTRIBUTE_MUST_NOT_NULL = "Defining Attribute should not be null";
  String DEFINING_ATTRIBUTE_VALUE_MUST_NOT_NULL = "Defining attribute value should not be null";
  String PREDEFINED_MANDATORY_MUST_NOT_NULL_OR_INVALID = "Predefined mandatory field should not be null or invalid";
  String MAXIMUM_CHARACTERS_ACCEPTED_IS_255_CHARACTERS_INCLUDING_SPACE =
      "Maximum characters accepted is 255 characters including space";
  String INVALID_PRODUCT_CODE = "Please enter valid product info to continue";
  String FAMILY_COLOR_FIELD_MUST_NOT_NULL = "Please enter valid Family Colour field value";
  String IMAGE_QC_CONFIG_VALUE_GT_CONFIDENCE_THRESHOLD = " Image qc config value is greater than confidence threshold";
  String RETRY_SUSPENSION = "Something went wrong. Please try again";
  String PRODUCT_CODE_PREFIX = "MTA";
  String DASH_DELIMITER = "-";
  String COMMA = ",";
  String OPEN_BRACKET = "[";
  String CLOSED_BRACKET = "]";
  String COMMA_WITH_SLASH = "\",\"";
  String OPEN_BRACKET_WITH_SLASH = "[\"";
  String CLOSED_BRACKET_WITH_SLASH = "\"]";
  String CATEGORY_CODE = "category_code";
  String PRODUCT_NAME = "product_name";
  String FAMILY_COLOUR = "Family Colour";
  String PRODUCT_NAME_OR_PRODUCT_CODE = "product_name OR product_code";
  String PRODUCT_CODE = "product_code";
  String REVIEW_PENDING = "review_pending";
  String SCRIPT = "<script";
  String DEF_TYPE = "defType";
  String QF = "qf";
  String FL = "fl";
  String STOPWORDS = "stopwords";
  String LOWERCASE_OPERATORS = "lowercaseOperators";
  String GROUP = "group";
  String FIELD = "field";
  String N_GROUPS = "ngroups";
  String LIMIT = "limit";
  String DOT = ".";
  String COLON = ":";
  String EDISMAX_OPERATION = "edismax";
  String FACET = "facet";
  String FACET_FIELD = "facet.field";
  String FACET_LIMIT = "facet.limit";
  String FACET_MIN_COUNT = "facet.mincount";
  String DEFAULT_CLIENT_ID = "pbp";
  String DEFAULT_CHANNEL_ID = "web";
  String UPDATE_OPERATION_TYPE = "UPDATE";
  String DEFAULT_USERNAME = "system";
  String DEFAULT_STORE_ID = "10001";
  String DEFAULT_REQUEST_ID = "requestId";
  String SEARCH_KEYWORD = "s";
  String AMPERSAND = "&";
  String STORE_ID_URL_PARAMETER = "storeId={storeId}";
  String CHANNEL_ID_URL_PARAMETER = "channelId={channelId}";
  String CLIENT_ID_URL_PARAMETER = "clientId={clientId}";
  String REQUEST_ID_URL_PARAMETER = "requestId={requestId}";
  String USERNAME_URL_PARAMETER = "username={username}";
  String SEARCH_KEYWORD_URL_PARAMETER = "s={s}";
  String NOT_APPLICABLE = "NA";
  String FULL_IMAGE_PREFIX = "/full/";
  String MEDIUM_IMAGE_PREFIX = "/medium/";
  String THUMBNAIL_IMAGE_PREFIX = "/thumbnail/";
  String DELIMITER_SLASH = "/";
  String XGP_USER = "XGP_USER";
  String DEFAULT_ASSIGNEE = "NA";
  String DEFAULT_ASSINGER = "SYSTEM";
  String DELIMITER = "#_#";
  String DELIMITER_DASH = "-";
  String WARNA = "Warna";
  String BRAND = "Brand";
  String STORE_ID = "storeId";
  String INTERNAL = "INTERNAL";
  String ACTIVE = "ACTIVE";
  String INACTIVE = "INACTIVE";
  String DRAFT = "DRAFT";
  String DEFAULT = "DEFAULT";
  String MINIMUM_PRICE = "minimumPrice";
  String YOUTUBE_URL_VALIDATION_SWITCH = "youTubeUrlValidationSwitch";
  String PERCENTAGE = "PERCENTAGE";
  String PRODUCT_MIGRATIONS_BATCH_SIZE = "productMigrationBatchSize";
  String PRODUCT_MIGRATION_THREAD_COUNT = "productMigrationThreadCount";
  String PRODUCT_MIGRATION_QUERY_LIMIT = "productMigrationQueryLimit";
  String PRODUCT_MIGRATION_SLEEP_TIME_IN_MS = "productMigrationSleepTimeInMs";
  String PRODUCT_MIGRATION_SWITCH = "productMigrationSwitch";
  String PRODUCT_MIGRATION_BEFORE_MINUTES= "productMigrationBeforeMinutes";
  String CM_MERCHANT = "CM";
  String CC_MERCHANT = "CC";
  String TD_MERCHANT = "TD";
  String TC_MERCHANT = "TC";
  List<String> INTERNAL_SELLERS = Arrays.asList(TD_MERCHANT, TC_MERCHANT);
  List<String> MERCHANT_EXCLUDED_PRODUCT_TYPE = Arrays.asList(CC_MERCHANT, TD_MERCHANT, TC_MERCHANT);
  String MIGRATION = "MIGRATION";
  String SYSTEM = "System";
  Integer MINIMUM_STOCK = 0;
  String MESSAGE_DIGEST_ALGORITHM = "MD5";
  String NO_EDITED_IMAGES_FOR_RESIZE = "No edited Images present with productCode";
  String ORDER_BY_IS_ARCHIVED = "isArchived";
  String SORT_BY_ASC = "asc";
  String ALL_VARIANTS = "All variants";
  String ALL_PICKUP_POINTS = "All pick up points";
  String CAMPAIGN_PRICE_VALIDATION = "campaignPriceValidationSwitch";
  String SKU_CODE = "sku-code";
  String SKU_CODE_2 = "sku-code-2";
  String UPC_CODE = "upc-code";
  String EDITED_FALSE = "\"edited\":false";
  String EDITED_TRUE = "\"edited\":true";
  String COGS_ERROR_CODE = "ERR-00000";
  String MERCHANT_PROMO_DISCOUNT = "merchantPromoDiscount";
  String CAMPAIGN = "campaign";
  int DAYS_IN_ONE_WEEK = 7;
  String DATE_PATTERN = "dd/MM/yyyy";
  long TOTAL_HOURS = 24 * 60 * 60 * 1000;
  String UPCOMING_STATUS = "Upcoming";
  String BUYABLE_LINK_STATUS="Buyable link";
  String DATE = "DATE";
  String NEED_REVISION = " - Need revision";
  String NEED_REVISION_WITHOUT_SPACE = "- Need revision";
  String TYPE_OF_PRODUCT = "ACTIVE";
  Integer DEFAULT_DANGEROUS_GOODS_LEVEL = 0;
  String ACTIVITY = "ACTIVITY";
  Integer ONE_DAY = 1;
  Integer ONE_MONTH = 1;
  Integer THREE_MONTH = 3;
  Integer TWELVE_MONTH = 12;
  Integer DAYS_IN_MONTH = 30;
  String DELETED_STATE = "DELETED";
  String HYPHEN = "-";
  Integer ZERO = 0;
  String DESCRIPTION = "description";
  String DIMENSION = "packageDimension";
  String LEADINGZERO = "^0+(?!$)";
  Integer STATE_5 = 5;
  Integer SIZE_ONE = 1;


  // AUTO QC OPERATIONS
  String GREATER_THAN_OR_EQUAL_TO = ">=";
  String LESS_THAN_OR_EQUAL_TO = "<=";
  String LESS_THAN = "<";
  String GREATER_THAN= ">";
  String EQUAL_TO = "==";
  String NOT_EQUALS_TO = "!=";
  String CONTAINS = "contains";

  // AUTO QC KEY-NAMES
  String BLUR = "blur";
  String WATERMARK = "watermark";
  String TEXT = "text";
  String ADULT = "Adult";
  String IS_EDITED_BY_INTERNAL_USER = "is_edited_by_internal_user";

  String BLUR_PREDICTION = "blur_predictions";
  String WATERMARK_PREDICTION = "watermark_predictions";
  String TEXT_PREDICTION = "text_predictions";
  String ADULT_PREDICTION = "nsfw_predictions";

  // AUTO QC VALUE TYPE
  String BOOLEAN = "boolean";
  String LIST_STRING = "ListString";
  String STRING = "string";
  String INT = "int";
  String DOUBLE = "double";
  String ELIGIBLE_FOR_AUTO_APPROVAL = "Eligible for auto-approval";

  String POST_LIVE = "Post-live";
  String PRE_LIVE = "Pre-live";
  String IN_PROGRESS_STATE = "IN_PROGRESS";
  String ACTIVE_STATUS = "ACTIVE";
  String INACTIVE_STATUS = "INACTIVE";

  String CONTENT_AUTO_APPROVAL_CHECK = "CONTENT";

  String PCB_CLIENT_ID = "pcb";
  String WEB_CHANNEL_ID = "web";

  String OTHERS_NOTES_REASON = "others";
  String OTHERS_PRE_LIVE_NOTES_REASON = "others - ";
  String AUTO_NEED_REVISION_ACTION = "AUTO_NEED_REVISION";
  String AUTO_NEED_REVISION_FLAG = "AutoNeedRevision";
  String VALIDATE_DRAFT_STATE = "ValidateDraftState";
  String SCREENING_ACTION = "ScreeningAction";

  String AGP_ITEM_STATUS = "DF,X";

  String PBP = "PBP";
  Integer REGULAR = 1;
  Integer BIG_PRODUCT = 2;
  Integer BOPIS = 3;

  String USP_EDITED = "uniqueSellingPoint";
  String URL_VIDEO_EDITED = "url";
  String PACKAGE_DIMENSION = "packageDimension";
  String COPY_ALL_STATUS = "COPY_ALL_STATUS";
  String FAIL = "FAIL";
  String NULL = "null";
  String PICKUP_POINT_CODE_MUST_NOT_BE_BLANK =
      "Pickup point code must not be blank";
  String STORE_ID_MUST_NOT_BE_EMPTY = "Store Id must not be empty";
  String ITEM_SKU_MUST_NOT_BE_EMPTY = "Item SKU must not be empty";
  String NOTIFY_MERCHANT_IDENTIFIER_KEY = "requestId";
  String PRODUCT_AUTO_REJECTION_SUBJECT = "Produk Anda Dihapus";
  String PRODUCT_AUTO_REJECTION_SUBJECT_ENGLISH = "Products Deleted";
  String TEMPLATE_AUTO_DELETE_NEED_CORRECTION_PRODUCT = "autoDeleteNeedCorrectionProduct";
  String TEMPLATE_AUTO_DELETE_NEED_CORRECTION_PRODUCT_ENGLISH =
      "autoDeleteNeedCorrectionProductEnglish";
  String PRIMARY = "primary";
  String SECONDARY = "secondary";
  String IMAGE_QC_BATCH_SIZE = "imageQcBacklogBatchSize";

  String IFRAME = "iframe";
  String ALIGN = "align";
  String ALT = "alt";
  String HEIGHT = "height";
  String SRC = "src";
  String TITLE = "title";
  String WIDTH = "width";
  String ALLOW_FULL_SCREEN = "allowfullscreen";
  String P = "p";
  String STYLE = "style";
  String DIV = "SIV";
  String SPAN = "span";
  String LT = "&lt;";
  String GT = "&gt;";
  String AMP = "&amp;";
  String HTTPS = "https://";

  String BACKLOG = "Backlog-";
  String CATEGORY_DELIMITER = " > ";

  String SHOW_L3_STOCK = "showL3Stock";
  int NO_VARIANTS_COUNT = 1;
  String WHOLESALE_PRICE = "WHOLESALE_PRICE";
  String NOTIFY_MERCHANT_PROCDUCT_ACTIVATED_TEMPLATE = "merchantProductAdded";
  String FAILED_RETRY_PRODUCT_TEMPLATE_ID = "failedRetryAlert";
  String FAILED_RETRY_PRODUCT_EMAIL_SUBJECT = " Blibli Seller Center - Failed Retry Products";
  String OBJECT = "object";
  String EMAIL_OBJECT = "obj";
  String TOTAL = "total";
  String DOT_REGEX = "[.]";
  String MAIL_FOR_EXCEED_ACTIVATION_FOR_PRODUCT_WIP_ID =
      "MAIL_FOR_EXCEED_ACTIVATION_FOR_PRODUCT_WIP_ID";
  String EXCEED_ACTIVATION_MAIL_DESCRIPTION = "Permohonan Aktivasi {0} Produk";
  String MAIL_SENDER = "no-reply@blibli.com";
  String MESSAGE_IDENTIFIER_KEY = "username";
  String BATCH_SIZE = "batchSize";
  String RETRY_PRODUCT_ACTIVATION_EMAIL = "business.relation@blibli.com";
  String RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID = "productStuckAlert";
  String RETRY_PRODUCT_POST_LIVE_EMAIL_SUBJECT =
      "[Post-Live] Blibli Seller Center - Product Stuck Alert";
  String HIERARCHY_DELIMITER = " < ";
  int CATEGORY_PAGE_SIZE = 50;
  String SUCCESS = "SUCCESS";
  String PARTIAL_SUCCESS = "PARTIAL_SUCCESS";
  String CONTENT = "content";
  String IMAGE = "image";
  String AUTO_INACTIVE = "AUTO_INACTIVE";
  String PROTECTED_BRAND_PREDICTION_TYPE = "protected_brand_predictions";
  String CATEGORY_MISMATCH_PREDICTION_TYPE = "category_mismatch";
  String SUSPICIOUS_BRAND = "Brand mismatch";
  String CATEGORY_MISMATCH = "Category mismatch";
  String CATEGORY_MISMATCH_IN = "Kategori tidak cocok";
  String CURRENT_VALUE = "current_value";
  String PREVIOUS_VALUE = "previous_value";
  String HISTORY_ACTIVITY = "activity";
  String ITEM_SKU = "itemSku";
  String PICKUP_POINT_CODE = "pickupPointCode";
  String ITEM_NAME = "itemName";
  String SUSPICIOUS_BRAND_IN = "Brand tidak cocok";
  String NEW = "new";
  String UPDATE = "update";
  String TYPE_MUST_NOT_BE_EMPTY = "Type must not be empty";
  String FILESTORE_PATH = "/filestore";
  String NEED_CORRECTION = "NEED_CORRECTION";
  String CATALOG_ORIGINATOR = "Catalog";
  String UPDATE_PRODUCT_STOCK = "Update Product Stock";
  String CREATE_WEB_STOCK = "Create Web Stock";
  String INCREASE_ORIGINAL_AND_AVAILABLE_FOR_UPDATE_STOCK = "INCREASE_ORIGINAL_AND_AVAILABLE_FOR_UPDATE_STOCK";
  String DECREASE_ORIGINAL_AND_AVAILABLE_FOR_UPDATE_STOCK = "DECREASE_ORIGINAL_AND_AVAILABLE_FOR_UPDATE_STOCK";
  String INCREASE_ORIGINAL_AND_AVAILABLE_FOR_CREATE_WEB_STOCK = "INCREASE_ORIGINAL_AND_AVAILABLE_FOR_CREATE_WEB_STOCK";
  String ATTRIBUTE_AUTO_FILL  = "Attribute Autofill";
  String AUTO_CANCEL = "autocancel";
  String SELLER = "seller";
  String MTA_API = "mta-api";
  String ORDER_CENTER = "order center";
  String CANCELLATION_ACTOR_OFF = "off";
  String ORDER_CENTER_INTERNAL = "Order Center Internal";
  String AUTO_CANCEL_ORDER_CANCELLATION = "Autocancel Order Cancellation";
  String ORDER_CANCELLATION = "Order Cancellation";
  String ORDER_ITEM_STATUS_OFF = "OFF";
  String PDT_RETRY_DELETE = "PDT_RETRY_DELETE";
  String PDT_AUTO_REJECTION = "PDT_AUTO_REJECT";
  String FBB_ACTIVE = "fbbActive";
  String AUTO_NEED_REVISION = "auto-need-revision";
  String AUTO_REJECT = "auto-reject";
  String DEFAULT_ADDRESS = "defaultAddress";
  String B2B_CHANNEL = "B2B";
  String B2C_CHANNEL = "B2C";
  String B2C_SELLER_CHANNEL = "BLIBLI";
  String B2B_SELLER_CHANNEL = "BLIBLI FOR BUSINESS";
  String PBP_SCALING_SKIPPED_ACTION = "PBP Scaling Skipped Action";
  String POST_LIVE_REVIEW_TYPE = "Post-live";
  String PRE_LIVE_REVIEW_TYPE = "Pre-live";
  String GDN_PRODUCT_ITEM_SKU = "gdnProductItemSku";
  String WAREHOUSE_SELLER_INVENTORY_FULFILLMENT = "BL";
  String CREATED_DATE = "createdDate";
  String STRING_ZERO = "0";
  String BUNDLE_ACTIVE_PRODUCT_URL = "%s?bundleProduct=true";
  String INVENTORY_NOT_FOUND_ERROR_MESSAGE = "Can not process invalid input data :webInventory must not be null";
  String PRODUCT_LIMIT_CACHE_MANAGER = "productLimitsCacheManager";
  String PRODUCT_LIMIT_SYSTEM_PARAMETER = "productLimitThreshold";
  String SKIP_DEFINITIVE_ACTIVITY = "Tindakan dilewati.";
  String SKIP_DEFINITIVE_DESCRIPTION = "SKU ini telah memiliki penjualan sebelumnya.";
  String SKIP_REJECTION = "Gagal menolak produk.";
  String SKIP_REJECTION_DESCRIPTION = "Masih ada stok di gudang.";
  String B2C_RETAIL = "B2C_RETAIL";
  String ITEM_IMAGES_ADDED = " Item images added ";
  double DOUBLE_ZERO = 0.0;
  int ONE = 1;
  int TWO = 2;
  String SPACE = " ";
  String BUYABLE_SCHEDULE_UPDATE = "BUYABLE_SCHEDULE_UPDATE";
  String DISCOVERABLE_SCHEDULE_UPDATE = "DISCOVERABLE_SCHEDULE_UPDATE";
  String PRODUCT_LEVEL_3_COUNT_CACHE = "productLevel3CountCache";
  String CNC_CHANNEL = "CNC";
  String ALL = "ALL";
  String MTA_API_CHANNEL_ID = "Host-To-Host";

  // Business partner profile response flags
  String PRODUCT_LIMIT = "productLimit";
  String FAAS_ACTIVATED = "faasActivated";
  String BLIBLI_OMG = "blibliOMG";
  String PRODUCT_CONSEQUENCE_LIMITATION = "productConsequenceLimitation";
  String DIMENSIONS_MISSING = "dimensionsMissing";
  String DESCRIPTION_MISSING = "descriptionMissing";
  Set<String> MISSING_FIELDS_SET =
    Set.of(DIMENSIONS_MISSING, DESCRIPTION_MISSING);
  Long DEFAULT_LONG_VALUE = 0L;
  String TWENTY_ONE_PRODUCT = "21+ product";
  int SKIP_ALL_ACTIONS = -1;
  String DISTRIBUTION_FLAG_KEY = "warehouseDistribution";
  String UPDATE_PRE_ORDER_QUOTA = "UPDATE_PRE_ORDER_QUOTA";
  String WEBP_EXTENSION = ".webp";
  String UPDATE_PO_DATE_BY_L3 = "UPDATE_PO_DATE_BY_L3";
  String SEND_EMAIL_TO_OTHERS_EVENT = "com.gdn.x.message.send.email.others";

  String NON_DISTRIBUTION = "NON_DISTRIBUTION";
  String DISTRIBUTION = "DISTRIBUTION";
  String PURE_DISTRIBUTION = "PURE_DISTRIBUTION";

  String PRODUCT_NAME_KEY = "productName";
  String CATEGORY_NAME_KEY = "categoryName";

  String SEND_EVENT_FOR_DISTRIBUTION_HISTORY = "com.gdn.pbp.product.distribution.history";
  String EMPTY_STRING = "";
  String CATEGORY = "Category";
  String CONVERT_TO_JKT_VARIABLE_FOR_HISTORY = ", convertToJKT";
  String CLOSE_PARENTHESIS = ")";
}
