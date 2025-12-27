package com.gdn.partners.pcu.external.model;

import java.text.SimpleDateFormat;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import org.springframework.util.StringUtils;

import com.google.common.collect.ImmutableSet;

public interface Constants {

  String CHANNEL_ID = "channelId";
  String CLIENT_ID = "clientId";
  String STORE_ID = "storeId";
  String USER_NAME = "username";
  String REQUEST_ID = "requestId";
  String STORE_CODE = "storeCode";
  String SIGNATURE = "Partners-Signature";
  String CONTEXT_PATH = "/pcu-external-api";
  String SESSION = "SESSION";
  String MEDIUM_IMAGE = "medium-image";
  String THUMBNAIL_IMAGE = "thumbnail-image";
  int UPLOAD_FINAL_IMAGE_THUMBNAIL_WIDTH = 110;
  int UPLOAD_FINAL_IMAGE_THUMBNAIL_HEIGHT = 110;
  int UPLOAD_FINAL_IMAGE_MEDIUM_WIDTH = 380;
  int UPLOAD_FINAL_IMAGE_MEDIUM_HEIGHT = 380;
  String PAGE = "page";
  String SIZE = "size";
  String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  String IS_EXTERNAL = "isExternal";
  String EXTERNAL_USER_MODE = "STORE";
  String IS_EXTERNAL_ONLY = "isExternalOnly";
  String DEFAULT_BP_CODE = "NA";
  String BUSINESS_PARTNER_NAME = "businessPartnerName";
  String LINKED_BUSINESS_PARTNER_CODE = "linkedStoreCode";
  String DATA_BASE_DIR = (!StringUtils.isEmpty(System.getenv().get("DATA_DIR")) ? System.getenv().get("DATA_DIR") :
      System.getProperty("java.io.tmpdir")) + "/mta/";
  String SUBJECT_TO_VAT_FOLDER = "subjectToVat";
  String ROOT = "/";
  String INVENTORY_FULFILLMENT_BLIBLI = "BL";
  String INVENTORY_FULFILLMENT_BUSINESS_PARTNER = "BP";
  String PURCHASE_TERM_COMISSION = "CM";
  String PURCHASE_TERM_REBATE = "RB";
  String PURCHASE_TERM_PURCHASE_ORDER = "PO";
  String PURCHASE_TERM_PURCHASE_CONSIGNMENT = "PC";
  String MERCHANT_TYPE_CC = "CC";
  String MERCHANT_TYPE_RC = "RC";
  String MERCHANT_TYPE_TD = "TD";
  String MERCHANT_TYPE_TC = "TC";
  String MERCHANT_TYPE_CM = "CM";
  String MERCHANT_TYPE_RB = "RB";
  String MERCHANT_TYPE_MP = "MP";
  String COMMA_DELIMITER = ", ";
  String COMMA_DELIMITER_NO_SPACE = ",";
  String COLON_DELIMITER = ": ";
  String APP_CHANNEL_ID = "App";
  String API_CHANNEL_ID = "Host-To-Host";
  String APP_CLIENT_ID = "MTAApp";
  String IOS_CLIENT_ID = "ios";

  String API_CLIENT_ID = "mta-api";
  String APP_ID = "pcu_external_api";
  String VIDEO_LIST = "snippet,contentDetails,statistics";
  String YOUTUBE_VALIDATION_SWITCH = "youTubeUrlValidationSwitch";
  String SHOW_L3_STOCK = "showL3Stock";
  String PERCENTAGE = "PERCENTAGE";
  String CATEGORY_SHEET = "Category";
  String BULK_UPSERT_SHEET = "bulkUpsert";
  String CATEGORY = "/xl/worksheets/sheet9.xml";
  Pattern CATEGORY_PATTERN = Pattern.compile(CATEGORY);
  String DEFAULT_BRAND_DESCRIPTION_EN =
      "English : Please select only if you can't find the brand name in the list of brands";
  String DEFAULT_BRAND_DESCRIPTION =
      "Bahasa Indonesia : Pastikan Anda memilih brand di bawah ini hanya jika nama brand yang Anda cari tidak ditemukan.";
  String NO_BRAND = "no brand";
  String OEM = "OEM";
  String CATEGORY_BRAND_SCHEDULER_RUN_TIME = "category_brand_sheet_regenerate_time";
  int MAXIMUM_UNIQUE_SELLING_POINT_LENGTH = 400;
  int MAXIMUM_DESCRIPTION_LENGTH = 5000;
  String AGP_ITEM_STATUS = "DF,X";
  String BULK_PROCESS_TYPE = "ProductLevel3";
  String BULK_PROCESS_TYPE_EAN = "EANProductLevel4";
  String BULK_PROCESS_MASTER_INFO_UPDATE = "ProductBasicInfo";
  String OFF2ON_BULK_PROCESS_TYPE = "InStore";
  String ARCHIVE_BULK_PROCESS_TYPE = "Archive";
  String SUBJECT_TO_VAT_TYPE = "SubjectToVat";
  String BULK_PRODUCT_CREATION_TYPE = "ProductCreationUpload";
  String BULK_PRODUCT_EXTERNAL_CREATION = "ExternalProductCreationUpload";
  int NO_VARIANTS_COUNT = 1;
  String DOT_SEPARATOR = ".";
  String DASH_SEPARATOR = "-";
  float PRODUCT_EXCEL_HEADER_HEIGHT = 48;
  SimpleDateFormat PREORDER_DATE_FORMAT = new SimpleDateFormat("dd/MM/yyyy");
  long TOTAL_HOURS = 24 * 60 * 60 * 1000;
  String WHOLESALE_PRICE = "WHOLESALE_PRICE";
  String SPECIAL_CHARS_REGEX = "[^A-Za-z0-9]";
  int FILENAME_MAX_LENGTH = 250;
  String FILENAME = "FILENAME";
  int INDEX_COUNT = 1;
  String DOT = ".";
  String SINGLE_QUOTE = "'";
  String DOT_REGEX = "\\.";
  String SPACE_REGEX = "\\s+";
  String IMAGE = "image";
  TreeSet<String> ALLOWED_IMAGE_MIME_TYPE = new TreeSet<String>(String.CASE_INSENSITIVE_ORDER) {
    private static final long serialVersionUID = 1L;

    {
      add("image/jpeg");
      add("image/png");
      add("image/jpg");
    }
  };
  String ACCEPT_HEADER = "Accept";
  String IMAGE_HEADER = "image/*";
  String DATA_RAW_DIR = "raw";
  String HTTPS_PREFIX = "https:/";
  String HTTP_PREFIX = "http:/";
  String FULL_PREFIX = "full01";
  String JPEG = ".jpeg";
  String HYPHEN = "_";
  String PRODUCT_SKU_REGEX = "^[A-Z0-9]{1,}-[0-9]{5,}-[0-9]{5,}$";
  Pattern PRODUCT_SKU_PATTERN = Pattern.compile(PRODUCT_SKU_REGEX);
  String ACTIVE_STATUS = "ACTIVE";
  String INACTIVE_STATUS = "INACTIVE";
  String PRIMARY = "PRIMARY";
  String SECONDARY = "SECONDARY";
  String ACTIVE = "ACTIVE";
  String PRODUCT_SETTING = "PRODUCT_SETTING";
  String PRODUCT_ARCHIEVE_TEMPLATE = "ARCHIEVE_TEMPLATE";
  String INSTORE = "INSTORE";
  String L5_DELETE = "L5_DELETE";
  String VAT = "VAT";
  String LOGISTICS = "LOGISTICS";
  String FBB = "Fulfilment by Blibli";
  String DATE_FORMAT = "dd/MM/yyyy";
  String B2B_SELLER_CHANNEL = "BLIBLI FOR BUSINESS";
  String BLIBLI = "BLIBLI";
  int BFB_COLUMNS_COUNT = 3;
  int CNC_COLUMNS_COUNT = 2;
  int PURE_DELIVERY_COLUMNS_COUNT = 1;
  int BFF_SELLER_TYPE_VALUE = 2;
  String HYPHEN_VALUE = "-";
  String B2B_CHANNEL = "B2B";
  String DEFAULT_CHANNEL = "DEFAULT";
  String CNC_CHANNEL = "CNC";
  int AMPHI_BFB_STATUS_COLUMN_WIDTH = 28 * 256;
  String WARNA = "Warna";
  String FAMILY_COLOUR = "Family Colour";
  int ZERO = 0;

  String ASSEMBLY_REQUEST = "ASSEMBLY_REQUEST";
  String DISASSEMBLY_REQUEST = "DISASSEMBLY_REQUEST";
  String TRANSFER_REQUEST = "TRANSFER_REQUEST";

  Set<String> VALID_BULK_WORK_ORDER_TYPE =
      ImmutableSet.of(Constants.ASSEMBLY_REQUEST, Constants.DISASSEMBLY_REQUEST, Constants.TRANSFER_REQUEST);
  Set<String> VALID_PROCESS_TYPE_FOR_CREATION =
      ImmutableSet.of(Constants.GENERIC_UPLOAD, Constants.CATEGORY_UPLOAD, Constants.BULK_UPSERT,
          Constants.CONVERTED_PRODUCT_CREATION_UPLOAD);
  String GENERIC_UPLOAD = "GENERIC_UPLOAD";
  String CATEGORY_UPLOAD = "CATEGORY_UPLOAD";
  String BULK_UPSERT = "BULK_UPSERT";
  String CONVERTED_PRODUCT_CREATION_UPLOAD = "ConvertedProductCreationUpload";
  String BULK_UPLOAD_TYPE = "UPLOAD";
  String CODE = "code";
  String FLOW_3 = "flow3";
  String PRODUCT_LIMIT = "productLimit";
  String FAAS_ACTIVATED = "faasActivated";
  String PRODUCT_CONSEQUENCE_LIMITATION = "productConsequenceLimitation";
  String MANDATORY_COLUMN_ID = "(Wajib)";
  String OPTIONAL_COLUMN_ID = "(Opsional)";
  String NON_EDITABLE_COLUMN_ID = "Tidak bisa diedit";
  String EDITABLE_COLUMN_ID = "Bisa diedit";
  String OOS_AUTO_HEAL = "OOS_AUTO_HEAL";

  String CLIENT_TYPE_APP = "app";
  String CLIENT_TYPE_SELLER_API = "seller-api";
  String CLIENT_TYPE_WEB = "web";
  String SUCCESS = "SUCCESS";

  String VIDEO = "video";
  String BSC = "bsc";
  String REELS = "reels";

  String PWP_MAIN_PENDING = "PWP_MAIN_PENDING";
  String PWP_MAIN_ACTIVE = "PWP_MAIN_ACTIVE";
  String PWP_ADDITIONAL_PENDING = "PWP_ADDITIONAL_PENDING";
  String PWP_ADDITIONAL_ACTIVE = "PWP_ADDITIONAL_ACTIVE";
  String UNICODE_DELIMITER = "\u001F";
  String WEBP_FORMAT = "webp";
  String WEBP_EXTENSION = ".webp";
  String BLIBLI_OMG = "blibliOMG";
}
