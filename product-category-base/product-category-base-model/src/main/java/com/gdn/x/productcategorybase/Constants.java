package com.gdn.x.productcategorybase;

import java.util.List;
import java.util.regex.Pattern;

public interface Constants {

  String REGEX_FOR_SPECIAL_CHARACTERS = "[^A-Za-z0-9]";
  Pattern PATTERN_FOR_SPECIAL_CHARACTERS = Pattern.compile(REGEX_FOR_SPECIAL_CHARACTERS);
  String HYPHEN = "-";
  String NEUTRAL_STATUS = "Neutral";
  String PRE_LIVE_FLAG = "Pre-live";
  String ACTIVITY = "Registered";
  String UPDATE_ACTIVITY = "Update";
  String NAME = "Name";
  String REQ_ID = "RequestId";
  String PRE_LIVE_STATUS = "Pre-live";
  String POST_LIVE_STATUS = "Post-live";
  String SUCCESS = "Success";
  String FLOW_1 = "FLOW-1";
  String FLOW_3 = "FLOW-3";
  String DEFAULT_STORE_ID = "10001";
  String BRAND = "Brand";
  String DEFAULT = "DEFAULT";
  String PATH_SEPARATOR = "/";
  String APPROVED_STATUS = "APPROVED";
  String DRAFT_STATUS = "DRAFT";
  String REJECTED_STATUS = "REJECTED";
  String DELETED_STATUS = "DELETED";
  String WHOLESALE_CONFIG_TYPE_PERCENTAGE = "PERCENTAGE";
  String FAMILY_COLOUR = "Family Colour";
  String MIGRATION = "MIGRATION";
  String BRAND_CODE = "BR-M036969";
  String OLD_BRAND_CODE = "BR-0036969";
  String NO_BRAND = "no brand";
  String SYSTEM = "system";
  String DOT = ".";
  String ACTIVE_IMAGE_SWITCH = "activeSwitch";
  String MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY = "maximumAllowedSizeForBrandSummary";
  int MAX_CHARACTER_FOR_NOTIFICATION = 255;
  int MAX_IMAGE_PER_VARIANT = 16;
  String MARK_FOR_DELETE = "markForDelete";
  String ACTIVATED = "activated";
  String OSC_LONG_TEXT = "oscLongText";
  String OSC_CODE = "oscCode";
  String ANY_STRING = "%";
  String CHANNEL_ID = "pcb";
  String CLIENT_ID = "pcb";
  String DELIMITER_SLASH = "/";
  String COLON = ":";
  String MAIN_IMAGE = "MainImage";
  String IN_REVIEW = "IN_REVIEW";

  //Product Attribute Extraction Constants
  String PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_NUMBER = "productAttributeExtractionMaxPageNumber";
  String PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_SIZE = "productAttributeExtractionMaxPageSize";
  String PRODUCT_ATTRIBUTE_EXTRACTION_SORT_ORDER_CONFIGURATION = "productAttributeExtractionSortOder";
  String MATRIX = "MATRIX";

  //Common Image flag migration Constants
  String BATCH_SIZE_TO_PUBLISH_COMMON_IMAGES_MIGRATION_RECORDS = "commonImageBatchSize";
  String MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_COMMON_IMAGE_MIGRATION = "commonImageMaxProductsFromDb";

  //GFS to GCS migration Constants
  String BATCH_SIZE_TO_PUBLISH_GFS_TO_GCS_MIGRATION_RECORDS = "gfsToGcsBatchSize";
  String MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_GFS_TO_GCS_MIGRATION = "gfsToGcsProductsFromDb";
  String BATCH_SIZE_TO_PUBLISH_GFS_TO_GCS_FINAL_IMAGE_MIGRATION_RECORDS = "gfsToGcsFinalImageBatchSize";
  String MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_GFS_TO_GCS_FINAL_IMAGE_MIGRATION = "gfsToGcsFinalImageProductsFromDb";


  String BATCH_SIZE_TO_PUBLISH_REJECTED_PRODUCTS = "rejectedProductDeletionBatchSize";
  String DAYS_THRESHOLD_FOR_PRODUCT_DELETION = "daysThresholdForProductDeletion";
  String BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS = "archivedProductDeletionBatchSize";
  String DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION = "daysThresholdForArchivedProductDeletion";

  String ASC_SORT_ORDER = "ASC";
  String UPDATED_DATE = "updatedDate";
  String ID = "id";
  String COPY_ALL_STATUS = "COPY_ALL_STATUS";
  String INTERNAL = "INTERNAL";
  String BRAND_NOTIFICATION = "Brand ";
  String BRAND_APPROVED_NOTIFICATION = " disetujui";
  String BRAND_REJECTED_NOTIFICATION = " ditolak. Alasan : ";
  String ALL = "ALL";
  String SKU_CREATION_ALLOWED_FOR_ALL_SELLERS = "skuCreationAllowedForAllSellers";

  String RESIZE = "resize";
  String OLD_PRODUCT = "oldProduct";
  String NEW_PRODUCT = "newProduct";
  String COMMA = ",";

  String SPACE = " ";

  String REDIS_COLON_DELIMITER = "::";
  String SALES_CATALOG = "SALES_CATALOG";

  //UPC MIGRATION PARAMS
  String BATCH_SIZE_TO_PUBLISH_UPC_MIGRATION_RECORDS = "upcMigrationBatchSize";
  String MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_UPC_MIGRATION = "upcMigrationProductsFromDb";

  String PARENT_CATEGORY_ID = "parentCategoryId";
  String B2B_EXCLUSIVE = "b2bExclusive";
  String HALAL_CATEGORY = "halalCategory";
  String CATALOG_ID = "catalogId";

  String RESTRICTED_KEYWORD_CACHE_MANAGER = "restrictedKeywordCacheManager";
  String CAFFEINE_CACHE_MANAGER = "caffeineCacheManager";
  String VALUE_TYPES_CONFIG = "valueTypes";
  double MINIMUM_DIMENSION_ALLOWED = 0.0;

  int MILLI_SECONDS = 1000;
  int SECONDS = 60;
  String PREFIX_SIZE_CHART_CODE = "SIZ";
  String PADDING_STRING = "0";
  int SIZE_CHART_CODE_SIZE = 6;
  String ATTRIBUTE_TYPE = "attributeType";
  String VALUE_TYPES = "valueTypes";

  int TWO = 2;
  int ZERO = 0;
  int ONE = 1;
  String UNDER_REVIEW = "UNDER_REVIEW";
  String AUTHORIZED_BRAND = "AUTHORIZED_BRAND";
  String UPCOMING = "UPCOMING";
  String MAX_PENDING_BRAND_AUTH_WIP_REQUESTS = "maxPendingBrandAuthWipRequests";
  int TWENTY_THIRD_HOUR = 23;
  int FIFTY_NINE = 59;

  String OPEN_BRACKET = "(";
  String CLOSING_BRACKET = ")";
  List<BrandAuthorizationWipStatus> creationAllowedStatus =
    List.of(BrandAuthorizationWipStatus.IN_REVIEW, BrandAuthorizationWipStatus.NEED_REVISION,
      BrandAuthorizationWipStatus.UPCOMING);
  List<BrandAuthorizationWipStatus> editAllowedStatus =
    List.of(BrandAuthorizationWipStatus.IN_REVIEW, BrandAuthorizationWipStatus.NEED_REVISION);
  String MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_PRODUCT_ATTRIBUTE_MIGRATION = "recordsFetchSizeForAttributeMigration";
  String BATCHES_TO_FETCH_FOR_PRODUCT_ATTRIBUTE_MIGRATION = "fetchBatchSizeForAttributeMigration";
  String DEFAULT_DATE_PATTERN = "dd-MM-yyyy";
  String STANDARD_DATE_PATTERN = "dd/MM/yyyy";
  String PREFIX_DATE_FORMATTER = "yyyyMMdd";
  String ATTRIBUTE_AUTO_HEAL = "attributeAutoHeal";
  String DS_EXTRACTION = "DS_EXTRACTION";
  String DS_EXTRACTED_ATTRIBUTE = "dsExtractedAttribute";
  String DS_ATTRIBUTE_CHANGE = "DS attribute change";
  String OPEN_SQUARE_BRACKET = "[";
  String CLOSE_SQUARE_BRACKET = "]";
  String NEXT_LINE = "\n";
  String PRODUCT_CODE = "productCode";
  String UPDATE_PRODUCT_ACTIVITY_UPDATE_NAME = "Ubah nama produk";
  String UPDATE_PRODUCT_ACTIVITY_UPDATE_DESCRIPTION = "Ubah deskripsi produk";
  String UPDATE_PRODUCT_ACTIVITY_UPDATE_LENGTH = "Ubah panjang";
  String UPDATE_PRODUCT_ACTIVITY_UPDATE_WIDTH = "Ubah lebar";
  String UPDATE_PRODUCT_ACTIVITY_UPDATE_HEIGHT = "Ubah tinggi";
  String UPDATE_PRODUCT_ACTIVITY_UPDATE_WEIGHT = "Ubah berat";
  String UPDATE_PRODUCT_ACTIVITY_UPDATE_SHIPPING_WEIGHT = "SHIPPING_WEIGHT_CHANGE";
  String UPDATE_PRODUCT_ACTIVITY_UPDATE_URL = "Ubah url video";
  String UPDATE_PRODUCT_ACTIVITY_ADD_IMAGE = "Image added";
  String UPDATE_PRODUCT_ACTIVITY_DELETE_IMAGE = "Image deleted";
  String UPDATE_PRODUCT_ACTIVITY_MAIN_IMAGE_UPDATED = "Image updated";


  // Hard delete entries Sys Param Names
  String MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES =
      "recordsFetchSizeForDeleteMfdTrueImagesAndAttributes";
  String BATCH_SIZE_TO_PUBLISH_DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES =
      "batchSizeToPublishDeleteMfdTrueImagesAndAttributes";

  String SELLER_API = "mta-api";
}
