package com.gdn.partners.pcu.external.model;

/**
 * @author Pradeep Reddy
 */
public interface ErrorMessages {

  String ERR_INVALID_RESPONSE = "Invalid Response";
  String IMAGE_NOT_FOUND = "Image not found at path : ";
  String CLIENT_EXCEPTION_ERROR_MESSAGE = "Mohon maaf, telah terjadi kesalahan pada sistem, namun kami sedang "
      + "memperbaikinya. Mohon kembali dalam beberapa saat dan coba lagi";
  String FALLBACK_ERR_MESSAGE = "FALLBACK";
  String FILE_EMPTY_ERROR_MESSAGE = "File is empty !!";
  String IMAGE_VALIDATION_ERR_MESSAGE = "File type must be jpg/png/jpeg ";
  String IMAGE_UPLOAD_ERR_MESSAGE = "Error when trying to upload image ";
  String ERR_ITEM_CODE_NULL = "Item Code cannot be null";
  String ERR_MER_CODE_NULL = "Merchant Code cannot be null in the request";
  String ERR_SEARCH_KEY_NULL = "Search key cannot be null in the request";
  String INVALID_REQUEST = "Not a valid parameter value";
  String INVALID_WAREHOUSE_CODE = "Not a valid warehouse code value";
  String INVALID_STATE = "Invalid state";
  String WRITER_EXCEPTION_ERR_MESSAGE = "Couldn't produce the image";
  String CREATE_BRAND_INVALID_CHARACTER_ERR_MESSAGE =
      "Invalid character while creating the brand. Please try again after removing characters other than alphabets and "
          + "numbers and special characters.";
  String INVALID_DAYS_ERROR_MESSAGE = "Not a valid value for days";
  String ERR_INVALID_TYPE = "Not a valid type. Allowed values are pdf or png.";
  String ERR_EMPTY_PRODUCT_LIST = " Product list cannot be empty";
  String ERR_EMPTY_OMNI_CHANNEL_SKU_LIST = " Omni channel sku list cannot be empty";
  String INVALID_TYPE_ERR = "Invalid value for type request parameter for inActive api";
  String INVALID_GDN_SKU = "Accessing details of incorrect business partner";
  String INVALID_PATTERN_SKU = "Product sku is not of valid format";
  String INVALID_BUSINESS_PARTNER_CODE = "Not an active business partner : ";
  String SELLING_PRICE_MUST_BE_LOWER_THAN_REGULAR_PRICE = "harga jual harus lebih rendah dari harga normal";
  String REQUIRED_PARAMETER = "Incomplete required parameter data";
  String FILE_INVALID = "Anda tidak dapat meng-upload file kosong";
  String EXCEL_FILE_TYPE_INVALID = "file harus dari jenis excel";
  String EXCEL_WORKORDER_TYPE_IS_INVALID = "excel workorder type is invalid";
  String EMPTY_FILE = "You can't upload empty file";
  String EXCEL_ZIP_FILE_TYPE_INVALID = "You can't upload file other than excel(.xlsx and .xlsm) and zip file";
  String EXCEL_FILE_TYPE_INVALID_VAT_MESSAGE = "You can't upload file other than excel(.xlsx)";
  String PDF_FILE_TYPE_INVALID_MESSAGE = "You can't upload file other than PDF(.pdf)";
  String GDN_SKUS_MUST_NOT_BE_BLANK = "gdnSkus must not be blank";
  String ERROR_PRODUCT_NOT_FOUND = "could not find product with business partner code {} and gdn sku {}";
  String ERR_ITEM_SKU_NULL = "Item sku cannot be null in the request";
  String ERR_ITEM_SKU_LIMIT_EXCEED = "number of allowed item skus exceeded";
  String ERR_EMPTY_PRODUCT_BUSINESS_PARTNER_LIST = " Product business partner ID list cannot be empty";
  String DATA_NOT_COMPLETE = "Data is not complete";
  String CATEGORY_CODES_MUST_NOT_BE_EMPTY = "category codes list must not be empty";
  String INVALID_PICKUP_POINT = "Pickup point invalid";
  String BUSINESS_PARTNER_CODE_CANNOT_BE_EMPTY = "business partner code cannot be empty";
  String BUSINESS_PARTNER_NOT_ACTIVE = "business partner not active";
  String BRAND_CANNOT_BE_CHANGED = "Brand cannot be changed";
  String UPDATING_SUSPENDED_ITEM = "Item Sku is suspended";
  String UPDATING_REJECTED_ITEM = "Item Sku is rejected";
  String UPDATING_ARCHIVED_ITEM = "Item Sku is archived";
  String ITEM_NOT_FOUND = "item not found";
  String UNAUTHORIZED_ERR_MESSAGE = "You are not authorized";
  String BRAND_REJECTED_ERR_MESSAGE = "Brand is Rejected";
  String INVALID_TEMPLATE = "The excel template uploaded is an old template. Please download new template.";
  String ERR_UNIFIED_TEMPLATE = "The excel template download failed. ";
  String NO_ACCESS_TO_EDIT_PRICE = "No access to edit Price";
  String NO_ACCESS_TO_EDIT_STOCK = "No access to edit stock";
  String ERROR_UPLOADING_FILE = "Error while uploading file";
  String UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS =
      "Unique selling point - exceeded maximum characters length.";
  String DESCRIPTION_MUST_NOT_BE_MORE_THAN_5000_CHARACTERS =
      "Description - exceeded maximum characters length.";
  String ARCHIVAL_FAILED = "Archival action failed for product sku : ";
  String PENDING_UPLOAD_FILES = "Cannot process the upload as there are pending bulk upload requests";
  String ERROR_PRODUCT_CODE_NOT_EMPTY = "Product code cannot be empty";
  String ERROR_PRODUCT_SKU_NOT_EMPTY = "Product sku cannot be empty";
  String ERROR_FILE_PATH_OR_URL_SHOULD_NOT_BE_EMPTY =
      "Evidence URL or Evidence file path should not be empty";
  String ERROR_INVALID_YOUTUBE_URL = "Invalid youtube url";
  String ERROR_INVALID_PRODUCT_TYPE = "Invalid product Type";
  String ERROR_PRODUCT_LIMIT_EXCEEDED = "Anda telah mencapai batas produk untuk toko Anda. Silakan hapus/arsipkan produk untuk meng-upload produk baru.";
  String INVALID_ATTRIBUTE = "Invalid attribute type and attribute mapping : ";
  String INVALID_URL = "Not a valid URL type";
  String IMAGE_READ_ERROR = "Could not get image from request";
  String IMAGE_UPLOAD_ERROR = "Could not upload image";
  String ERROR_GETTING_DATA_FROM_GCS = "Error occurred while getting data from GCS. Empty file "
    + "path!";
  String BULK_BRAND_DATA_CANNOT_BE_EMPTY = "Bulk Brand Data cannot be null or empty";
  String EMPTY_BULK_PROCESS_TYPE = "Error Bulk Process Type cannot be empty";
  String EMPTY_LOCATION_PATH = "Location Path cannot be empty";
  String INVALID_NUMBER_OF_QR_PER_PAGE =
      "selected template does not support number of QR per page selected";
  String INVALID_SELLER_TYPE_FOR_ASSEMBLY = "seller is not eligible for assembly, disassembly or transfer request";
  String INVALID_WORK_ORDER_TYPE = "Invalid work order type : %S";
  String NO_ACCESS_TO_PICKUP_POINT = "User does not have access to the selected pickup point(s)";
  String ERROR_PRODUCT_COUNT = "Error while fetching product count";
  String NAN_NOT_ALLOWED = "NaN value is Not Allowed";
  String INVALID_FILE_TYPE_NOT_SUPPORTED = "Error in uploading file, file format is not supported";
  String VIDEO_SIGNED_URL_REQUEST_NOT_EMPTY = "Video Signed url request cannot be null";
  String VIDEO_CONFIGURATION_REQUEST_NOT_EMPTY = "Video Configuration request cannot be null";
  String VIDEO_ACCESSED_BY_DIFFERENT_SELLER = "Video from Different seller cannot be Accessed";
  String VIDEO_ID_CANNOT_BE_EMPTY = "video id cannot be empty";
  String REELS_UPDATE_REQUEST_CANNOT_BE_NULL = "Reels update request cannot be null";
  String PRODUCT_SKUS_CANNOT_BE_EMPTY_OR_EXCEED_LIMIT_IN_REELS =
        "Product SKUs cannot be empty or exceed %s in Reels update request";
  String UN_AUTHORISED_TO_ACCESS_REELS = "You are not authorised to access Reels";
  String FAILED_TO_GENERATE_SIGNED_URL = "Failed to generate signed URL";
  String FILE_NAME_CANNOT_BE_EMPTY = "File name cannot be empty";
  String PROCESS_TYPE_CANNOT_BE_EMPTY = "Process type cannot be empty";
  String PROCESS_CODE_CANNOT_BE_EMPTY = "bulkProcessCode cannot be empty";
  String INVALID_PROCESS_TYPE = "Invalid process type";
  String CREATION_INVALID_PROCESS_TYPE = "Invalid processType: %s. Allowed values are: %s ";
}
