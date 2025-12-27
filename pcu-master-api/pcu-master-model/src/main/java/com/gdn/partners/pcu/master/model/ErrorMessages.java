package com.gdn.partners.pcu.master.model;

public interface ErrorMessages {
  String ERR_INVALID_RESPONSE = "Invalid Response";
  String ERR_INVALID_CATALOG_TYPE = "Invalid catalog type";
  String ERR_INVALID_ATTRIBUTE_TYPE = "Invalid attribute type";
  String EMPTY_CATALOG_ID_OR_CATALOG_TYPE = "Empty catalog Id or catalog type";
  String ERR_EMPTY_CATALOG_ID_OR_CATEGORY_NAME = "Catalog_Id or Category name cannot be null";
  String ERR_EMPTY_ATTRIBUTE_CODE = "Empty attribute code";
  String ERR_EMPTY_ATTRIBUTE_VALUE = "Empty attribute value response";
  String FALLBACK_ERR_MESSAGE = "FALLBACK";
  String SYSTEM_ERROR_MESSAGE = "Mohon maaf, telah terjadi kesalahan pada sistem, namun kami sedang "
      + "memperbaikinya. Mohon kembali dalam beberapa saat dan coba lagi";
  String MARGIN_NOT_SET_ERR_MESSAGE = " karena kategori belum memiliki margin";
  String DESCRIPTION_EMPTY_ERR_MESSAGE = " karena kategori deskripsi tidak boleh kosong";

  String KEYWORD_MUST_NOT_BE_NULL = "Keyword must not be null";
  String CATEGORY_SHIPPING_CODE_NOT_SET_ERR_MESSAGE = " karena kategori belum memiliki shipping code";
  String ACTIVATION_FAILURE_ERR_MESSAGE = "Anda tidak berhasil mengaktifkan kategori ";
  String CATEGORY_CANNOT_BE_MADE_INACTIVE = "Category can not be made inactive";
  String BRAND_REJECTED_ERR_MESSAGE = "Brand is Rejected";
  String CATEGORY_LIST_MUST_NOT_BE_NULL = "categoryCodeList must not be null";
  String UNAUTHORIZED_ERR_MESSAGE = "You are not authorized";
  String ERR_INVALID_RESTRICTED_KEYWORD_ACTION_TYPE = "Invalid restricted keyword action type";
  String CLIENT_EXCEPTION_ERROR_MESSAGE = "Mohon maaf, telah terjadi kesalahan pada sistem, namun kami sedang "
          + "memperbaikinya. Mohon kembali dalam beberapa saat dan coba lagi";
  String DIMENSION_NAME_SHOULD_NOT_BE_BLANK = "Name should not be blank";
  String DIMENSION_NAME_ENGLISH_SHOULD_NOT_BE_BLANK = "Name in english should not be blank";
  String DIMENSION_DESCRIPTION_SHOULD_NOT_BE_BLANK = "Description should not be blank";
  String DIMENSION_DESCRIPTION_ENGLISH_SHOULD_NOT_BE_BLANK = "Description in english should not be blank";
  String DIMENSION_EXAMPLE_SHOULD_NOT_BE_BLANK = "Example should not be blank";
}
