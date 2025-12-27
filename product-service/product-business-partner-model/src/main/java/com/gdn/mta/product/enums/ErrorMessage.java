package com.gdn.mta.product.enums;

public enum ErrorMessage {

  INVALID_SIZE_CHART_CODE("Size chart tidak ditemukan, periksa kembali sesuai kode di halaman size chart"),
  INVALID_SIZE_CHART_CODE_ERROR_CODE("ERR-SIZ-00029"),
  SIZE_CHART_IS_NOT_VALID_FOR_THE_CATEGORY("Size chart yang digunakan tidak sesuai dengan kategori produk"),
  SIZE_CHART_IS_NOT_VALID_FOR_THE_CATEGORY_ERROR_CODE("ERR-SIZ-00030"),
  CATEGORY_CODE_MUST_NOT_BE_BLANK("Category code must not be blank");
  private String message;

  ErrorMessage(String message) {
    this.message = message;
  }

  public String getMessage() {
    return this.message;
  }
}