package com.gdn.partners.product.analytics.model.enums;

public enum ErrorCode {

  FIND_AUTO_QC("PA_50001", "An error occurred in getting auto qc detail"),
  FIND_SELLER_DETAIL("PA_50002", "An error occurred in getting Seller detail"),
  PRODUCT_CODE_MUST_NOT_BE_BLANK("PA_50003","Kode produk wajib diisi."),
  PRODUCT_NOT_FOUND("PA_50004","Produk tidak ditemukan."),
  PRODUCT_IS_ALREADY_UNASSIGNED("PA_50005","Produk telah dipindah tugaskan."),
  PRODUCT_CODE_LIST_EMPTY("PA_50006", "Product code list must not be empty."),
  PRODUCT_CODE_EMPTY("PA_50007", "Product code must not be empty.");

  private final String code;
  private final String message;

  ErrorCode(String code, String message) {
    this.code = code;
    this.message = message;
  }

  public String getCode() {
    return this.code;
  }

  public String getMessage() {
    return message;
  }
}
