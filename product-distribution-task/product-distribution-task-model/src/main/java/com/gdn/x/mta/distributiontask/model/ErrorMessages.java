package com.gdn.x.mta.distributiontask.model;

public interface ErrorMessages {
  String STORE_ID_MUST_NOT_BE_EMPTY = "Store id must not be empty";
  String PRODUCT_CODE_MUST_NOT_BE_EMPTY = "Product code must not be empty";
  String STATUS_MUST_NOT_BE_EMPTY = "Status must not be empty";
  String MIGRATION_TYPE_MUST_NOT_BE_EMPTY = "Migration type must not be empty";
  String BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY = "Business partner code type must not be empty";
  String PRODUCT_SKU_MUST_NOT_BE_EMPTY = "Product sku must not be empty";
  String PRODUCT_WAITING_TO_GET_ACTIVATED =
      "Produk nonaktif. Produk akan ditambahkan ke review IPR setelah diaktifkan.";
  String PRODUCT_WHITELISTED = "Produk sudah di-whitelist";
  String PRODUCT_EVIDENCE_REQUESTED =
      "Produk tidak bisa dirilis/review karena bukti telah diminta.";
  String PRODUCT_EVIDENCE_SUBMITTED = "Produk tidak bisa di-review karena bukti telah dikirim.";
  String PRODUCT_SKU_NOT_FOUND_IN_X_PRODUCT =
      "Can not process invalid input data :product sku not found in x-product";
  String PRODUCT_SUSPENDED_FROM_INTERNAL = "Produk tidak bisa dirilis/review karena telah di-suspend.";
  String PRODUCT_REJECTED_FROM_INTERNAL = "Produk sudah ditolak.";
  String INVALID_STATE_TO_SUBMIT_EVIDENCE = "Invalid state to submit evidence";
  String EVIDENCE_FILE_OR_URL_IS_NEEDED_FOR_SUBMITTING_EVIDENCE =
    "Evidence file path or url is needed for submitting evidence";
  String EVIDENCE_SUBMITTED_NOTES_CANNOT_BE_BLANK = "Evidence submitted notes cannot be blank";
  String PRODUCT_NOT_FOUND_IN_IPR = "Product not found in IPR";
  String PRODUCT_IS_NOT_IN_EVIDENCE_REQUESTED_STATE = "Product is not in evidence requested state.";
  String INVALID_IPR_HISTORY_ACTIVITY = "Invalid Ipr history activity";
  String ITEM_IMAGE_MUST_NOT_BE_EMPTY = "Item image must not be empty";
}
