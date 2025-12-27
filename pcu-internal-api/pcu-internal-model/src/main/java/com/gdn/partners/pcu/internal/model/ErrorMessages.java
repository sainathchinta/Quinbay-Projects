package com.gdn.partners.pcu.internal.model;

/**
 * @author Pradeep Reddy
 */
public interface ErrorMessages {

  String ERR_INVALID_RESPONSE = "Invalid Response";
  String FALLBACK_ERR_MESSAGE = "FALLBACK";
  String ERR_ASSIGNED_BY_EMPTY = "AssignedBy cannot be empty";
  String ERR_ASSIGNED_TO_EMPTY = "AssignedTo cannot be empty";
  String ERR_PRODUCT_CODE_EMPTY = "ProductCode cannot be empty";
  String CLIENT_EXCEPTION_ERROR_MESSAGE = "Mohon maaf, telah terjadi kesalahan pada sistem, namun kami sedang "
      + "memperbaikinya. Mohon kembali dalam beberapa saat dan coba lagi";
  String CONSTRAINT_EXCEPTION_ERROR_MESSAGE = "Bad request, input data should not be null";
  String ERR_PRODUCT_ID_EMPTY = "ProductId cannot be empty";
  String ERR_PRODUCT_SKU_EMPTY = "ProductSku cannot be empty";
  String IMAGE_NOT_FOUND = "Image not found";
  String KEYWORD_EMPTY = "Search keyword should not be empty";
  String ERR_CATEGORY_ID_EMPTY = "CategoryId cannot be empty";
  String BRAND_HAS_PRODUCTS_MAPPED = "This brand is being used by some product";
  String IMAGE_VALIDATION_ERR_MESSAGE = "File type must be jpg/png/jpeg ";
  String BRAND_CODE_AND_BRAND_REQUEST_CODE_SENT = "Send either brand request code or brand code";
  String BRAND_CODE_AND_BRAND_REQUEST_CODE_EMPTY = "Both brand code and brand request code are empty. Send either one.";
  String EMPTY_VENDOR_CODE_ERROR = "Vendor Code should not be empty";
  String BRAND_APPROVAL_STATUS_ERROR = "Brand Approval Status cannot be empty";
  String INCORRECT_UPDATE_TYPE = "Incorrect Update Type";
  String ERR_VENDOR_CODE_EMPTY = "VendorCode cannot be empty";
  String ERR_VERSION_EMPTY = "version cannot be empty";
  String PRODUCT_CODE_LIST_EMPTY = "Reject productCode list is empty";
  String SUSPENSION_TYPE_EMPTY = "Suspension type cannot be empty";
  String RESTRICTED_KEYWORD_BULK_UPLOAD_EMPTY = "Restricted keyword bulk upload type cannot be empty";
  String BRAND_AUTH_BULK_UPLOAD_EMPTY = "Brand authorization bulk upload type cannot be empty.";
  String BULK_REVIEW_UPLOAD_EMPTY = "Bulk Review upload type cannot be empty.";
  String INVALID_SUSPENSION_TYPE = "Not a valid suspension type";
  String ASSIGNMENT_TYPE_EMPTY = "Assignment type cannot be empty";
  String NOTES_TYPE_EMPTY = "Notes cannot be empty";
  String REASON_TYPE_EMPTY = "reason cannot be empty";
  String ACTION_TYPE_INVALID = "Not a valid action type";
  String EMPTY_PRODUCT_LIST = "Product list cannot be empty";
  String EMPTY_MERCHANT_SEARCH_KEYWORD = "Merchant search keyword cannot be empty";
  String EMPTY_MERCHANT_CODE = "Merchant code cannot be empty";
  String EMPTY_CATEGORY_CODE = "Category code cannot be empty";
  String CONFIGURATION_TYPE_EMPTY = "Configuration type cannot be empty";
  String CONFIGURATION_TYPE_INVALID = "Configuration type is invalid";
  String FILE_INVALID = "Anda tidak dapat meng-upload file kosong";
  String EXCEL_FILE_TYPE_INVALID = "file harus dari jenis excel";
  String INVALID_SOURCE = "Not a valid source";
  String ERR_WHILE_FETCHING_DATA = "Error while fetching data";
  String PRODUCT_STATE_INVALID = "Product state is invalid, please refresh the page ";
  String ERR_SOURCE_EMPTY = "Source cannot be empty";
  String INVALID_TIME_FILTER_TYPE = "Not a valid time filter type";
  String PRODUCT_PRESENT_IN_PDT = "Product already present in vendor";
  String INVALID_REQUEST = "Not a valid parameter value";
  String DEFINING_ATTRIBUTE_MISMATCH = "Defining attributes mismatch";
  String WHOLESALE_CONFIGURATIONS_MISMATCH = "Wholesale rules config fall in different tiers";
  String UNAUTHORIZED_ERR_MESSAGE = "You are not authorized";
  String XLSX_TYPE_ONLY = "File must be of xlsx extension";
  String RECAT_EXCEL_UPLOAD_FAILED = "Failed to upload excel for recategorization";
  String PRODUCT_NEED_REVISION_MOBILE_ERROR =
      "Product revision is currently available for web or mobile web version only.";
  String EMPTY_BRAND_CODE_ERROR = "Brand Code cannot be Empty";
  String EMPTY_SELLER_CODE_ERROR = "Seller Code cannot be Empty";
  String EMPTY_IPR_REGISTRATION_NUMBER_ERROR = "Ipr registration number cannot be Empty";
  String EMPTY_IPR_REGISTRATION_NUMBER_EXCEEDED_MAX_LENGTH = "Ipr registration number exceeded max length";
  String EMPTY_BRAND_NAME_ERROR = "Brand Name cannot be Empty";
  String EMPTY_AUTH_STATUS_ERROR = "Authorisation state cannot be Empty";
  String EMPTY_DOCUMENT_ERROR = "Document cannot be Empty";
  String AUTH_DOC_NOT_FOUND = "Uploaded auth document cannot be found for file ";
  String AUTH_START_DATE_MUST_NOT_BE_NULL = "Auth start date must not be null";
  String AUTH_END_DATE_MUST_NOT_BE_BEFORE_START_DATE = "Auth end date must not be before "
    + "start date";
  String PREDICTION_CATEGORY_MAPPING_WEB_REQUEST_LIST_NULL_ERROR =
      "PredictionCategoryMappingWebRequestList must not be null";
  String ERR_DELETE_REQUEST_FOR_BRAND_AUTH =
    "Delete Request failed and hence could not be processed for given seller and brand code";

  String PREDICTION_TYPE_LIST_NULL_ERROR =
      "Prediction Type List must not be NULL";
  String FILE_EMPTY_ERROR_MESSAGE = "File is empty !!";
  String ERROR_GETTING_DATA_FROM_GCS = "Error occurred while getting data from GCS. filepath : %s ";
  String ERR_AUTO_ASSIGNMENT = "Error occurred while processing the assignment request";
  String ERR_VENDOR_EMAIL_EMPTY = "Vendor email cannot be empty";
  String ERR_PROCESS_TYPE = "Process type cannot be empty";
  String INVALID_RESTRICTED_KEYWORD_TYPE = "Not a valid restricted keyword type";
  String INVALID_BULK_REVIEW_TYPE = "Not a valid review type";
  String INVALID_BRAND_AUTH_TYPE = "Not a valid brand authorization type";
  String FILE_NAME_EMPTY_ERROR = "File name is empty !!";
  String INVALID_EXTENSION = "File extension is invalid ";

  String MAX_FILE_ALLOWED_CROSSED = "Cannot upload more than 5 files.";
  String EMPTY_LOCATION_PATH = "Location path cannot be empty";
  String EMPTY_CERTIFICATION_NUMBER = "Certification No. cannot be empty";
  String BPJPH_RESPONSE_FAILED = "Failed to get response from BPJPH API";
  String BULK_UPLOAD_ACTION_TYPE_CANNOT_BE_NULL = "Bulk upload action type cannot be null.";
  String PROCESS_TYPE_CANNOT_BE_EMPTY = "Process type cannot be empty";
  String INVALID_PROCESS_TYPE = "Invalid process type provided";
  String INVALID_ACTION_TYPE = "Not a valid action type";
  String INVALID_USER_NAME = "Invalid username.";
  String INVALID_ACTION_FOR_AUTO_APPROVED_PRODUCTS = "Invalid action for auto-approved products";
  String REASON_CANNOT_BE_EMPTY_FOR_SUSPENSION_OF_AUTO_APPROVED_PRODUCT =
      "Reason cannot be empty for suspension of auto-approved product";
  String NOTES_CANNOT_BE_EMPTY_FOR_SUSPENSION_OF_AUTO_APPROVED_PRODUCT =
      "Notes cannot be empty for suspension of auto-approved product";

  String EMPTY_STORE_ID_ERROR = "Store id must not be empty";

  String ERROR_ON_IMAGE_UPLOAD = "Error when trying to upload image %s.";
  String ERROR_ON_IMAGE_UPLOAD_CODE = "IMG_EXT40001";
  String INVALID_IMAGE_TYPE = "Invalid image file. File Type must be jpg/png/jpeg.";
  String INVALID_IMAGE_TYPE_CODE = "IMG_EXT40002";
  String INVALID_IMAGE_SIZE = "Invalid image size. Size should not exceed %s MB.";
  String EXCEPTION_ERR_MSG = "Exception while writing content into file image {} for productCode : {} ";
}
