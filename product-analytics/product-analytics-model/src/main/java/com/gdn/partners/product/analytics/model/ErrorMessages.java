package com.gdn.partners.product.analytics.model;

public interface ErrorMessages {

  String CLIENT_EXCEPTION_ERROR_MESSAGE = "Mohon maaf, telah terjadi kesalahan pada sistem, namun kami sedang "
      + "memperbaikinya. Mohon kembali dalam beberapa saat dan coba lagi";
  String MERCHANT_CODE_MUST_NOT_BE_BLANK = "merchant code must not be blank";
  String CATEGORY_CODE_MUST_NOT_BE_BLANK = "category code must not be blank";
  String SELLER_CODE_MUST_NOT_BE_BLANK = "seller code must not be blank";
  String FALLBACK_ERR_MESSAGE = "FALLBACK";
  String SELLER_ANALYTICS_DATA_NOT_FOUND = "seller data analytics not found";
}
