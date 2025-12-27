package com.gdn.mta.bulk.models;

/**
 * Created by priyanka on 09/02/17.
 */
public enum BulkErrorCategory {
  INPUT_ERROR("Input Error"),
  SYSTEM_ERROR("System Error"),
  DATA_NOT_FOUND("Data Not Found"),
  EXCEL_HEADER_ERROR("Input Error"),
  DUPLICATE_PRODUCT_SKUS("Produk sudah terdaftar dan berjalan di campaign lain"),
  FILE_NOT_FOUND("Input Error"),
  FILE_TYPE_INVALID("Input Error"),
  STOCK_MUST_BE_NUMBER("Stok harus berupa angka"),
  PO_QUOTA_MUST_BE_NUMBER("PO Quota harus berupa angka"),
  EAN_MUST_BE_NUMBER("Format tidak valid. EAN/UPC hanya boleh berisi angka."),
  INVALID_STATUS_COMBINATION("invalid status combination");

  private String description;

  BulkErrorCategory(String description){
    this.description = description;
  }

  public String getDescription() {
    return description ;
  }

}
