package com.gdn.partners.bulk.util;

import java.util.Arrays;
import java.util.List;
import java.util.TreeSet;

import org.apache.commons.lang3.StringUtils;

import com.gdn.mta.bulk.dto.GenericTemplateFileType;

public interface Constant {

  String CHANNEL_ID = "api";
  String CLIENT_ID = "x-bulk";
  String INTERNAL_BULK_UPDATE = "Internal-bulk-update";
  String USER_NAME = "x-bulk";
  String SUCCESS = "success";
  Integer PRODUCT_TYPE_REGULAR = 1;
  Double PRODUCT_TYPE_REGULAR_WEIGHT_THRESHOLD = 50D;
  String BLIBLI_SKU_HEADER = "Blibli SKU";
  String BLIBLI_SKU_HEADER_WITH_EXAMPLE = "Blibli SKU (contoh: BLI-10000-00011-00001)";
  String ITEM_SKU_PATTERN = "^[A-Z0-9]{1,}-[0-9]{5,}-[0-9]{5,}-[0-9]{5,}$";
  String PRODUCT_SKU_PATTERN = "^[A-Z0-9]{1,}-[0-9]{5,}-[0-9]{5,}$";
  String EXCEL_FILE_MUST_NOT_BE_BLANK = "Excel File tidak boleh kosong";
  String EXCEL_FILE_NAME_MUST_NOT_BE_BLANK = "Excel File nama tidak boleh kosong";
  String PROCESSOR_SERVICE = "ProcessorService";
  String EXCEL_FILE_NAME = "excelFilename";
  String UPDATED_PICKUP_POINTS = "-ppCodeUpdated.xlsx";
  String SHEET_1 = "sheet1";
  String SHEET_2 = "sheet2";
  String SHEET_3 = "sheet3";
  String SHEET_4 = "sheet4";
  String SHEET = "sheet";
  String FINISH_UPLOADING = " selesai diunggah. ";
  String MASTER_BULK_PROCESS_LOG_MESSAGE =
    "Invoking process for internal user bulk update. storeId: {}, bulkProcessCode: {}, userName: {}";
  String VENDOR_PRODUCT_BULK_ASSIGN_LOG_MESSAGE =
    "Invoking process for vendor product bulk assign. storeId: {}, bulkProcessCode: {}, userName: {}";
  String FAILED_PRODUCT_SAVE_ERROR =
    "Error while saving failed products to file, bulkProcessCode: {} ";
  String VALIDATION_ERROR = "Validation Error: ";
  String DANGEROUS_GOOD_LEVEL_EMPTY_ERROR = VALIDATION_ERROR + "DangerousGoodLevel is Empty data: {}";
  String HEIGHT_EMPTY_ERROR = VALIDATION_ERROR + "Height is Empty data: {}";
  String WEIGHT_EMPTY_ERROR = VALIDATION_ERROR + "Weight is Empty data: {}";
  String WIDTH_EMPTY_ERROR = VALIDATION_ERROR + "Width is Empty data: {}";
  String LENGTH_EMPTY_ERROR = VALIDATION_ERROR + "Length is Empty data: {}";
  String BRAND_EMPTY_ERROR = VALIDATION_ERROR + "Brand is Empty data: {}";
  String PRODUCT_NAME_EMPTY_ERROR = VALIDATION_ERROR + "ProductName is Empty data: {}";
  String PRODUCT_CODE_EMPTY_ERROR = VALIDATION_ERROR + "ProductCode is Empty data: {}";
  String PRODUCT_SKU_EMPTY_ERROR = VALIDATION_ERROR + "ProductSku is Empty data: {}";
  String PRODUCT_SKU_NOT_VALID_ERROR = VALIDATION_ERROR + "ProductSku is not valid data: {}";
  String OFF2ON_EMPTY_ERROR = VALIDATION_ERROR + "Off2On is Empty data: {}";
  String OFF2ON_VALUE_INVALID = VALIDATION_ERROR + "Off2On value is invalid: {}";
  String CONTENT_ASSIGNEE_EMPTY = VALIDATION_ERROR + "Content Assignee is empty data: {}";
  String ASSIGNEE_EMPTY = VALIDATION_ERROR + "Assignee is empty data: {}";
  String IMAGE_ASSIGNEE_EMPTY = VALIDATION_ERROR + "Image Assignee is empty data: {}";
  String CONTENT_ASSIGNEE_INVALID = VALIDATION_ERROR + "This user has no permissions to perform an action: {}";
  String ASSIGNEE_INVALID = VALIDATION_ERROR + "This user has no permissions to perform an action: {}";
  String IMAGE_ASSIGNEE_INVALID = VALIDATION_ERROR + "internal user has no permissions to perform an action: {}";
  String VALIDATION_ERROR_IN = "Validation Error in ";
  String ERROR_IN_DG_LEVEL = VALIDATION_ERROR_IN + MasterDataBulkParameters.DANGEROUS_GOOD_LEVEL;
  String ERROR_IN_HEIGHT = VALIDATION_ERROR_IN + MasterDataBulkParameters.HEIGHT;
  String ERROR_IN_WEIGHT = VALIDATION_ERROR_IN + MasterDataBulkParameters.WEIGHT;
  String ERROR_IN_WIDTH = VALIDATION_ERROR_IN + MasterDataBulkParameters.WIDTH;
  String ERROR_IN_LENGTH = VALIDATION_ERROR_IN + MasterDataBulkParameters.LENGTH;
  String ERROR_IN_BRAND = VALIDATION_ERROR_IN + MasterDataBulkParameters.BRAND;
  String ERROR_IN_PRODUCT_NAME = VALIDATION_ERROR_IN + MasterDataBulkParameters.PRODUCT_NAME;
  String ERROR_IN_PRODUCT_CODE = VALIDATION_ERROR_IN + MasterDataBulkParameters.PRODUCT_CODE;
  String ERROR_IN_INSTORE_FLAG = VALIDATION_ERROR_IN + "InStore flag";
  String ERROR_IN_ITEM_SKU = VALIDATION_ERROR_IN + MasterDataBulkParameters.ITEM_SKU_VALIDATION_ERROR;
  String ERROR_IN_ITEM_SKU_UPDATE = VALIDATION_ERROR_IN + MasterDataBulkParameters.ITEM_SKU_UPDATE_ERROR;
  String ERROR_IN_SKU_CODE_UPDATE = VALIDATION_ERROR_IN + MasterDataBulkParameters.SKU_CODE_UPDATE_ERROR;
  String ERROR_IN_CONTENT_ASSIGNEE = StringUtils.SPACE + VALIDATION_ERROR_IN + VendorProductDataBulkParameters.CONTENT_ASSIGNEE;
  String ERROR_IN_ASSIGNEE = StringUtils.SPACE + VALIDATION_ERROR_IN + VendorProductDataBulkParameters.ASSIGNEE;
  String ERROR_IN_IMAGE_ASSIGNEE = StringUtils.SPACE + VALIDATION_ERROR_IN + VendorProductDataBulkParameters.IMAGE_ASSIGNEE;
  String ERROR_IN_CATEGORY_PREDICTION = "Silakan pilih kategori yang sesuai dari daftar kategori";
  String ERROR_IN_BRAND_PREDICTION = "Silakan pilih brand yang sesuai dari daftar brand";
  String BRAND_SOURCE_FINAL_RECOMMENDATION = "final_recommendations";
  String FAILED_REASON_SEPARATOR = ",";
  String ERROR_IN_FILE_DELETION = "File Deletion failed : {}";
  String MASTER_DATA_BULK_UPDATE_COMPLETED_MESSAGE =
    "Master Data Bulk Update completed. MasterDataBulkUpdateRequest: {}";
  String VENDOR_BULK_ASSIGNMENT_COMPLETED_MESSAGE = "Vendor Bulk Assignment completed. Request: {}";
  String PIPE = "|";
  String BRACKET_RIGHT = ")";
  String SPACE = " ";
  String SLASH = "/";
  String DASH = "-";
  String UNDERSCORE = "_";
  String SUSPENSION_EMPTY_PRODUCT_CODE_ERROR = "Please enter the product code to continue {} ";
  String SUSPENSION_EMPTY_MERCHANT_CODE_ERROR = "Please enter the merchant code to continue {} ";
  String SUSPENSION_INVALID_REASON_ERROR = "Please select one of the available reasons to continue";
  String SUSPENSION_INVALID_REASON_DESCRIPTION_ERROR = "Reason description must be filled {} ";
  String BULK_PRODUCT_SUSPENSION_LOG_MESSAGE =
    "Bulk Product Suspension Log for request : {} with storeId : {} updatedBy : {} ";
  String BULK_PRODUCT_SUSPENSION_LOG_COMPLETED_MESSAGE =
    "Bulk Product Suspension completed for request : {} ";
  String FAILED_PRODUCT_SUSPENSION_ERROR =
    "Error while saving failed products to file for suspension, bulkProcessCode: {}";
  String FAILED_INTERNAL_BULK_UPLOAD_ERROR =
    "Error while saving failed products to file for internal bulk upload, bulkProcessCode: {}";
  String BRAND_APPROVAL_STATUS_APPROVE = "APPROVED";
  String EMPTY_CODE_ERROR = "Please enter the correct code to upload configuration for ";
  String EMPTY_NAME_ERROR = "Please enter the correct name to upload configuration for ";
  String BULK_CONFIGURATION_UPDATE_LOG_MESSAGE =
    "Bulk Configuration Update Log for request : {} with storeId : {} updatedBy : {} ";
  String BULK_CONFIGURATION_UPDATE_LOG_COMPLETED_MESSAGE =
    "Bulk Configuration Update completed for request : {} with storeId : {} and updatedBy : {} ";
  String CREATE_PRD_API_FEATURE_NAME = "PRD-CRT";
  String INVALID_CONFIG_VALUE =
    "Please select one of the available configuration value in correct format for ";
  String FAILED_BULK_CONFIGURATION_ERROR =
    "Error while saving failed bulk configuration file for bulkProcessCode : {}";
  String HTTPS_PREFIX = "https:/";
  String HTTP_PREFIX = "http:/";
  String FILE_TYPE_XLS = "xls";
  String FILE_TYPE_XLSX = "xlsx";
  String FILE_TYPE_XLSM = "xlsm";
  String DOT = ".";
  String CATEGORY_CODE = "categoryCode";
  String MINIMUM_PRICE = "minimumPrice";
  String NOT_ALPHANUMERIC_REGEX = "[^a-zA-Z0-9]+";
  String BRAND_VALUES_SHEET = "inputs1";
  String BRAND_VALUE_HEADER = "List of all Brands";
  String BRAND_COLUMN = "Brand Names";
  String C1_CATEGORY_COLUMN = "c1_name";
  String STORE_ID = "10001";
  String ARROW_DELIMITER = "->";
  String CATEGORY_ATTRIBUTE_SHEET = "inputs";
  String BLIBLI_MASS_UPLOAD_GENERIC_TEMPLATE = "Blibli-mass-upload-template.xlsm";
  String BLIBLI_MASS_UPLOAD_GENERIC_TEMPLATE_EN = "Blibli-mass-upload-template-en.xlsm";
  String GENERAL_TEMPLATE = "general_template.xlsm";
  String NEW_GENERAL_TEMPLATE = "general_template";
  String NEW_GENERAL_TEMPLATE_EXTENSION = ".xlsm";
  String ABORT_PROCESS_PENDING_BEFORE_SECONDS = "abortProcessPendingBeforeSeconds";
  String XBP = "XBP";
  String PCB = "PCB";
  String UNIFIED_DOWNLOAD_IN_PROGRESS = "IN_PROGRESS";
  String UNIFIED_DOWNLOAD_COMPLETE = "COMPLETE";
  String UNIFIED_DOWNLOAD_ABORT = "ABORT";
  String COMMA = ",";
  int CAMPAIGN_BULK_DATA_START = 5;
  int CAMPAIGN_BULK_HEADER_INDEX = 5;
  String INSTANT_PICKUP_PRODUCT = "InstantPickupProductUpsert";
  String PICKUP_POINT_SHEET_HEADER = "Input 11 - warerhouse code";
  String PICKUP_POINT_CODE_HEADER = "warehouse_code";
  String PICKUP_POINT_NAME_HEADER = "warehouse_name";
  String BRAND_IN_REVIEW = " (IN_REVIEW)";
  String PARAGRAPH_START = "<p>";
  String PARAGRAPH_END = "</p>";
  String ATTRIBUTE_NAME_BRAND = "Brand";
  String REQUEST_ID = "x-bulk";
  String SELLER_SKU = "Seller SKU";
  String UNMAPPED_PRODUCT = "Unmapped Products";
  String NO_UNMAPPED_PRODUCT_MESSAGE = "There is no unmapped SKU! Good job!";
  String UNMAPPED_PRODUCTS_CREATED_IN = "Unmapped SKU for products created in ";
  String GENERATED_DATE = "Generated by %s on %s";
  String CATALOG_CODE = "Catalog Code";
  String CATEGORY = "Category ";
  String PRODUCT_NAME = "Product Name ";
  String SKU = "SKU ";
  String PRODUCT_CREATION_DATE = "Product creation date ";
  String BAHASA_CODE = "IN";
  String DEFAULT_USERNAME = "username";
  String OBJECT = "object";
  String TOTAL = "total";
  String PENDING_OBJECT = "pendingObject";
  String TOTAL_PENDING = "totalPending";
  String IN_PROGRESS_OBJECT = "inProgressObject";
  String TOTAL_IN_PROGRESS = "totalInProgress";
  String ABORTED_OBJECT = "abortedObject";
  String TOTAL_ABORTED = "totalAborted";
  String ABORTED_TIME = "abortedTime";
  String EMAIL_OBJECT = "obj";
  String THRESHOLD_VALUE_FOR_PENDING_BULK_REQUESTS = "thresholdValueForPendingBulkRequests";
  String MAILS_FOR_PENDING_BULK_REQUESTS = "mailsForPendingBulkRequests";
  String THRESHOLD_VALUE_FOR_ABORTED_BULK_REQUESTS = "thresholdValueForAbortedBulkRequests";
  String ABORTED_REQUESTS_BASED_ON_ENDTIME_IN_MINUTES = "AbortedRequestsBasedOnEndTimeInMinutes";
  String IMAGE_PROCESSING_FETCH_BATCH_SIZE = "imageProcessingFetchBatchSize";
  String IMAGE_PROCESSING_FETCH_BATCH_SIZE_FOR_PRIORITY_1 =
    "imageProcessingFetchBatchSizePriority1";
  String IMAGE_PROCESSING_FETCH_BATCH_SIZE_FOR_PRIORITY_2 =
    "imageProcessingFetchBatchSizePriority2";
  String VIDEO_PROCESSING_FETCH_BATCH_SIZE_FOR_PRODUCT_BASIC_INFO = "videoProcessingFetchBatchSizeForProductBasicInfo";

  String PUBLISHED_FETCH_BATCH_SIZE = "publishedProcessingFetchBatchSize";
  String DOT_SPLIT = "[.]";
  String DOT_REGEX = "\\.";
  String ACCEPT_HEADER = "Accept";
  String IMAGE_HEADER = "image/*";
  String FBB = "Fulfilment By Blibli";

  String HTTPS_URL = "https";
  TreeSet<String> ALLOWED_IMAGE_TYPE = new TreeSet<String>(String.CASE_INSENSITIVE_ORDER) {
    long serialVersionUID = 1L;

    {
      add("jpg");
      add("png");
      add("jpeg");
    }
  };
  String IMAGE = "image";
  TreeSet<String> ALLOWED_IMAGE_MIME_TYPE = new TreeSet<String>(String.CASE_INSENSITIVE_ORDER) {
    long serialVersionUID = 1L;

    {
      add("image/jpeg");
      add("image/png");
      add("image/jpg");
    }
  };
  String DEFAULT_BRAND_DESCRIPTION_EN =
    "English: Please select only if you can't find the brand name in the list of brands";
  String DEFAULT_BRAND_DESCRIPTION =
    "Bahasa Indonesia: Pastikan Anda memilih brand di bawah ini hanya jika nama brand yang Anda cari tidak ditemukan.";
  String INSTORE_BULK_PROCESS_TYPE = "InStore";
  String ARCHIVE_BULK_PROCESS_TYPE = "Archive";
  String SYSTEM_USER = "Merchant API";
  String LOWER_THAN_SYMBOL = "<";
  String SYSTEM_ERROR = "System error";
  String INVALID_CATEGORY = "Category is invalid";
  char OPEN_CURLY_BRACES = '{';
  String FAILED_PRODUCTS = "failed-products";
  String USER_NAME_SPLIT = "@";
  char CLOSE_CURLY_BRACES = '}';
  String AUTO_UPLOAD_URL_SUBSTRING = "autoUploadTrackerKeyWord";
  String CATEGORY_CODE_PARTITION_SIZE = "categoryCodePartitionSize";
  String INTERNAL_UPLOAD_SWITCH = "newInternalUploadFlowSwitch";
  String BULK_SUSPENSION_SWITCH = "newBulkSuspensionWorkflowSwitch";
  String BULK_CONFIG_UPDATE_SWITCH = "newBulkConfigUpdateWorkflowSwitch";
  String QR_CODE_RGB = "qrCodeRGB";
  String IMAGE_DOWNLOAD_BATCH_SIZE = "imageDownloadBatchSize";
  int TRANSACTION_TIMEOUT = 1200;
  int ZERO = 0;
  int ONE = 1;
  String PERIOD = ". ";
  String HYPHEN = "-";
  String HYPHEN_SPACED = " - ";
  String IMAGE_KEYWORD_FULL = "_full";
  String ZERO_STRING = "0";
  String ONE_STRING = "1";
  long LONG_ONE = 1L;
  String SALES_CATALOG = "SALES_CATALOG";
  String DEFAULT = "DEFAULT";
  String IN_REVIEW = "IN_REVIEW";
  String IN_REVIEW_BRAND_SUFFIX = " (IN_REVIEW)";
  String WARNA = "Warna";
  String ROW = "Baris : ";
  String EAN_UPC = "Model/EAN/UPC";
  String COLOR = "Color";
  int ERROR_COUNT = 100;
  int PRODUCT_NAME_LENGTH = 150;
  int DESCRIPTION_LENGTH = 5000;
  int USP_LENGTH = 400;
  int WEIGHT = 0;
  String GENERIC = "GENERIC";
  String EXTERNAL_UPLOAD = "EXTERNAL_UPLOAD";
  String COLOUR_FAMILY = "Family Colour";
  String WARNING_PREFIX = "Warning, unable to parse: ";
  Double MAXIMUM_DISCOUNT_VALUE = 70.0;
  Integer MAX_IMAGE_COUNT = 7;
  String GARANSI = "Garansi";
  String WARRANTY = "Warranty";
  String BRAND = "Brand";
  String PRODUCT_UPLOAD_SLA_NOTIFICATION = "PRODUCT_UPLOAD_SLA_NOTIFICATION";
  String BAHASA = "in";
  String ENGLISH = "en";
  String NOTIFICATION_PLACEHOLDER = "{0}";
  String DESCRIPTION_SUCCESS = "Sukses: ";
  String DESCRIPTION_PRODUCTS = " produk";
  String OUT_OF = "out of";
  String DESCRIPTION_BULK_UPLOAD_SUFFIX = "selesai diunggah.";
  String POST_LIVE = "Post-live";
  String DARI = "dari";
  String HAS_BEEN_UPLOADED = "has been uploaded. ";
  String TELAH_DI_UPLOAD = "telah di-upload. ";
  String POST_LIVE_PRODUCT_UPLOAD_SLA_NOTIFICATION = "POST_LIVE_PRODUCT_UPLOAD_SLA_NOTIFICATION";
  String DESCRIPTION_UPLOADING_FAILED = "Proses upload gagal ";
  String ROW_NUMBER = "rowNumber";
  String PRIVILEGED_MAP = "privilegedMap";
  String DEFAULT_CLIENT_HOST = "MTA_WEB";
  String CREATED_DATE = "createdDate";
  String ID = "id";
  String STORE_COPY_DOWNLOAD_BASE_TEMPLATE_XLSX = "store-copy-download-base-template.xlsx";
  String STORE_COPY_UPLOAD_BASE_TEMPLATE_XLSX = "store-copy-upload-base-template.xlsx";
  String PICKUP_POINT_SHEET = "PickupPoint";
  String SYSTEM = "system";
  String CC_MERCHANT = "CC";
  String TD_MERCHANT = "TD";
  String TC_MERCHANT = "TC";
  String ONE_DECIMAL_STRING = "1.0";
  String ZERO_DECIMAL_STRING = "0.0";
  String TWO_DECIMAL_STRING = "2.0";
  String THREE_DECIMAL_STRING = "3.0";
  String DATA_ROW_NUMBER = "RowNumber";
  List<String> MERCHANT_EXCLUDED_PRODUCT_TYPE = Arrays.asList(CC_MERCHANT, TD_MERCHANT, TC_MERCHANT);
  String INTERNAL = "INTERNAL";
  String INTERNAL_BULK_UPLOAD = "INTERNAL_BULK_UPLOAD";
  String INVALID_PRODUCT_SKU = "Input product sku is invalid";
  String ERROR_PRODUCT_SKU_ARCHIVE = "Error on archival of product sku";
  String ERROR_OFF_2_ON_UPDATE = "Error on off2On update in x-product for product sku %s";

  String FILE_CREATION_MESSAGE = "Penciptaan file ";

  String DOWNLOAD_MESSAGE = " berhasil. Silakan untuk mendownload file.";

  String PARTIAL_DOWNLOAD_MESSAGE = "Telah mencapai batas maksimum baris di file Excel. Anda bisa download %s baris. Untuk data lainnya, silakan gunakan filter.";

  String SEND_EMAIL_MESSAGE = " berhasil. Silakan cek email untuk mendownload file.";

  String FAILED_MESSAGE = "Penciptaan file gagal. Silakan coba beberapa saat lagi.";
  String DEFAULT_FILE_NAME = "bulk-download-file";
  String FILE_BULK_UPDATE_PRODUCT_TEMPLATE = "bulk-update-product-template";
  String FILE_BULK_UPDATE_PRODUCT_EAN_TEMPLATE = "bulk-update-product-ean-template";
  String FILE_BULK_UPDATE_PRODUCT_BASIC_INFO_TEMPLATE = "bulk-basic-info-update-template";

  String FILE_BULK_UPDATE_PRODUCT_TEMPLATE_FOR_INTERNAL = "bulk-update-master-product-template";

  String FILE_BULK_ORDER_DOWNLOAD_TEMPLATE = "bulk-order-download-template";
  String BULK_PRICE_INFORMATION = "bulk-price-information";
  String FILE_BULK_PRODUCT_VENDOR_TEMPLATE = "bulk-product-vendor";
  String FILE_CAMPAIGN_PRODUCT_LIST = "produk-promo-";
  String FILE_CAMPAIGN_FAILED_PRODUCT_LIST = "produk-promo-gagal";
  String FILE_BULK_UPLOAD_OFFLINE_ITEMS_TEMPLATE = "bulk-upload-offline-items-template";
  String OF = " of ";
  String FEW_PRODUCTS_SUCCESSFULLY_ASSIGNED = " products are successfully assigned." + "Check all failed assign product at unassigned filter";
  String ALL_PRODUCTS_SUCCESSFULLY_ASSIGNED = "All products successfully assigned";
  String MERCHANT_TYPE_CM = "CM";
  String ACTIVE = "ACTIVE";
  String CATEGORY_UPLOAD_TEMPLATE_EN = "categoryUploadTemplateFileEnglish";
  String CATEGORY_UPLOAD_TEMPLATE = "categoryUploadTemplateFile";
  String DEFAULT_DELIVERY_STATUS = "1";
  String DEFAULT_INSTORE_STATUS = "0";
  String DEFAULT_CNC_STATUS = "0";
  String DEFAULT_STOCK_VALUE = "0";
  String INSTORE_SELLER_DEFAULT_DELIVERY_STATUS = "0";

  String TERMINATE = "TERMINATE";
  String INACTIVE = "INACTIVE";
  String VENDOR_AUTO_ASSIGNMENT = "vendor_auto_assignment";
  String RESTRICTED_KEYWORD_UPSERT = "RESTRICTED_KEYWORD_UPSERT";
  String RESTRICTED_KEYWORD_DELETE = "RESTRICTED_KEYWORD_DELETE";
  String BULK_PRICE_UPDATE = "BULK_PRICE_UPDATE";
  String BULK_PRICE_PRODUCT_TYPE_TAGGING = "BULK_PRICE_PRODUCT_TYPE_TAGGING";
  String BULK_PRICE_REBATE = "BULK_PRICE_REBATE";
  String BULK_SKU_LEVEL_REBATE = "BULK_SKU_LEVEL_REBATE";
  String BULK_PRICE_UPDATE_NEW = "BULK_PRICE_UPDATE_NEW";
  String ERROR_MESSAGE_SEPARATOR = ", ";
  String PRODUK = "Produk ";
  String KONTEN = "Konten ";
  String FOTO = "Foto ";
  String COLON = ":";
  String B2B_SELLER_CHANNEL = "BLIBLI FOR BUSINESS";
  String DEFAULT_BFB_BUYABLE_FLAG = "0";
  String DEFAULT_BFB_MANAGED_FLAG = "0";
  Integer ONLINE = 1;
  List<GenericTemplateFileType> FILE_TYPES =
      Arrays.asList(GenericTemplateFileType.PURE_DELIVERY_FILE, GenericTemplateFileType.CNC_FILE,
          GenericTemplateFileType.BFB_NON_CNC_FILE, GenericTemplateFileType.BFB_CNC_FILE,
          GenericTemplateFileType.PURE_DELIVERY_BUNDLING_FILE, GenericTemplateFileType.CNC_BUDNLING_FILE,
          GenericTemplateFileType.BFB_NON_CNC_BUNDLING_FILE, GenericTemplateFileType.BFB_CNC_BUNDLING_FILE,
          GenericTemplateFileType.PURE_DELIVERY_INSTORE_FILE, GenericTemplateFileType.CNC_INSTORE_FILE,
          GenericTemplateFileType.BFB_NON_CNC_INSTORE_FILE, GenericTemplateFileType.BFB_CNC_INSTORE_FILE,
          GenericTemplateFileType.PURE_DELIVERY_BUNDLING_INSTORE_FILE,
          GenericTemplateFileType.CNC_BUDNLING_INSTORE_FILE, GenericTemplateFileType.BFB_NON_CNC_BUNDLING_INSTORE_FILE,
          GenericTemplateFileType.BFB_CNC_BUNDLING_INSTORE_FILE);
  String B2B_CHANNEL = "B2B";
  String DEFAULT_CHANNEL = "DEFAULT";
  String CNC = "CNC";
  String BRAND_INDEX = "5";

  //for QR codes type
  String STORE = "STORE";
  String PRODUCT = "PRODUCT";
  String ITEM = "ITEM";
  String ITEM_PICKUP_POINT = "ITEM_PICKUP_POINT";
  String ALL_PRODUCTS = "ALL_PRODUCTS";
  String LIHAT_SEMUA_PRODUK = "Lihat semua produk";
  String ALL_LOCATIONS = "ALL_LOCATIONS";
  String ADD_TO_BAG = "ADD_TO_BAG";
  String THEME_DARK = "DARK";
  String THEME_LIGHT = "LIGHT";
  String MIME_TYPE_ZIP = "application/zip";
  String INVALID_REASON_MESSAGE = "Alasan yang diberikan tidak valid.";
  String INSTORE = "Blibli Instore";
  String SUPERMARKET = "Supermarket";
  String SCAN_AND_GO = "Scan & Go";
  String CLICK_AND_COLLECT = "Click and Collect";
  String LOCALE_LANGUAGE = "in";
  String LOCALE_COUNTRY = "ID";
  String DORMANT_SELLER_IN_PROGRESS = "IN_PROGRESS";
  String SYSTEM_ABORTED = "SYSTEM_ABORTED";
  String FAILED = "FAILED";
  String ITEM_SKU_REGEX = "^%s-\\d{5}-\\d{5}$";
  int READ_START_FROM_INDEX = 1;
  int HEADER_ROW_ID = 0;
  int OFFSET = 0;
  String BULK_UPLOAD_TYPE = "UPLOAD";
  String BULK_DOWNLOAD_TYPE = "DOWNLOAD";
  String BULK_INTERNAL_UPLOAD = "INTERNAL_UPLOAD";
  int TWO = 2;
  String DUPLICATE_ROW_REBATE_ERROR_MESSAGE = "Duplicate Rebate Combination";
  String EQUALS = "=";
  String UNIQUE = "unique";
  String DUPLICATE = "duplicate";
  String YES = "Yes";
  String NO = "No";
  String ALL = "All";
  String PRICE_RANGE = "Rp %s - Rp %s";
  String SELLING_PRICE_CHANGE_NOT_ALLOWED = "Selling Price Change Not Allowed";
  String CAMPAIGN_PRICE_CHANGE_NOT_ALLOWED = "Campaign Price Change Not Allowed";
  String ACCESSIBLE_PICKUP_POINTS = "accessiblePickupPointCodes";
  String INPUT_BY_BR = "Input By BR";
  String REBATE_HISTORY = "Seller: %s\nSeller & Category: %s\nSeller & Brand: %s\nSeller, Category & Brand: %s";
  String NEW_LINE = "\n";
  String BULK_NEED_REVISION_POST_LIVE_DELETION = "Bulk Need Revision postLive Deletion";
  String DIMENSIONS_MISSING = "dimensionsMissing";
  String DESCRIPTION_MISSING = "descriptionMissing";
  Double DEFAULT_SHIPPING_WEIGHT = 0.0;
  String RESPONSE_OK = "OK";
  Integer RESPONSE_200 = 200;
  String DATE_FORMAT = "dd/MM/yyyy";
  String INTERNAL_SERVER_ERROR = "Internal Server Error";
  String CAMPAIGN_GENRIC_ERROR = "Failed to update campaign, please try again";
  String AT_SIGN = "@";
  String SMALL_LABEL = "BY39";
  String TRIANGLE_ACRYLIC = "BY919";
  String CATEGORY_HIERARCHY_DELIMITER = " > ";
  String SEMI_COLON = "; ";
  String BULK_PROCESS_CODE = "bulk_process_code";
  String MEDIUM_IMAGE = "medium-image";
  String THUMBNAIL_IMAGE = "thumbnail-image";
  int UPLOAD_FINAL_IMAGE_THUMBNAIL_WIDTH = 110;
  int UPLOAD_FINAL_IMAGE_THUMBNAIL_HEIGHT = 110;
  int UPLOAD_FINAL_IMAGE_MEDIUM_WIDTH = 380;
  int UPLOAD_FINAL_IMAGE_MEDIUM_HEIGHT = 380;
  String SAMPLE_PRODUCT_NAME = "productName";
  String SAMPLE_BRAND_NAME = "brandName";
  String REQUEST_CODE = "request_code";
  String FILE_NAMES = "fileNames";
  String UNICODE_DELIMITER = "\u001F";
  String DORMANT_SELLER_ABORTED_BY = "ABORTED_BY_DORMANT_RUN_DECK";
  String WEBP_IMAGE_MIME_TYPE = "image/webp";
  String FILE_TYPE_ZIP = ".zip";
  String CONVERTED_GENERAL_TEMPLATE = "converted_general_template";
  String ERROR_COLUMN = "Error";
  String BRAND_RECOMMENDATION = "brandRecommendation";
  String CATEGORY_RECOMMENDATION = "categoryRecommendation";
  String EXTERNAL_GENERAL_TEMPLATE = "external_template";
  String MANDATORY_FIELD_IS_BLANK = "Mandatory field %s is blank";
  String SEMI_COLONS = ";";
}
