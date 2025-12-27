package com.gdn.x.mta.distributiontask.service.api;

public interface ErrorMessages {

  String STORE_ID_MUST_NOT_BE_NULL = "StoreId cannot be null";
  String PRODUCT_CODE_MUST_NOT_BE_EMPTY = "ProductCode must not be empty";
  String PRODUCT_ID_MUST_NOT_BE_EMPTY = "ProductId must not be empty";
  String PRODUCT_DISTRIBUTION_TASK_NOT_EMPTY = "product distribution task should not be empty";
  String AUTO_APPROVAL_PRODUCT_NOT_POST_LIVE = "product is not post-live";
  String AUTO_APPROVAL_PRODUCT_BRAND_NOT_ACTIVE = "product brand is not approved";
  String AUTO_APPROVAL_PRODUCT_NOT_IN_REVIEW = "product not in review, it's in state ";
  String AUTO_APPROVAL_PRODUCT_ASSIGNMENT = "product is assigned";
  String GCS_SOURCE_IMAGE_FILE_NOT_FOUND = "Gcs source image file not found. filePath : %s ";
  String STORE_ID_MUST_NOT_BE_EMPTY = "StoreId must not be empty";
  String OLD_PATH_MUST_NOT_BE_EMPTY = "OldPath must not be empty";
  String NEW_PATH_MUST_NOT_BE_EMPTY = "NewPath must not be empty";
  String INVALID_PRODUCT_STATE =
    "Cannot reject this product code : %s as product is in invalid state.";
  String ERROR_PUBLISHING_EVENT =
    "Error while publishing auto approval events to PDT. Added Product with Product Code: {} to Auto Approval Table. ";
  String DATE_MUST_NOT_BE_NULL = "Date must not be null";
  String PRODUCT_NOT_FOUND = "Product does not exist";
  String NOTES_MUST_NOT_BE_EMPTY = "Notes must not be empty";
}
