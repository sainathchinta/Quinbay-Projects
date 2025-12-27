package com.gdn.x.product.exception;

public enum ApiErrorCodes {
  PRODUCT_IS_ARCHIVED("ERR-XPRODUCT400001", "Product is Archived "),
  INVALID_SKU("INVALID_SKU", "Invalid sku : "),
  INVALID_PP("INVALID_PP", "Invalid pp code : "),
  PRODUCT_SUSPENDED("PRODUCT_SUSPENDED", "Product is suspended : "),
  PRODUCT_PERMANENTLY_DELETED("PRODUCT_PERMANENTLY_DELETED", "Product is permanently deleted : "),
  SYSTEM_ERROR("SYSTEM_ERROR", "System Error "),
  INVALID_PRODUCT("INVALID_PRODUCT", "Invalid product"),
  TAKEN_DOWN("TAKEN_DOWN", "Taken down product"),
  SKUS_NOT_FOUND("SKUS_NOT_FOUND", "Returned Empty response from db"),
  ITEM_DELETED("ITEM_DELETED", "Item Deleted"),
  CURATION_STATUS_INVALID("INVALID_CURATION_STATUS","invalid curation status"),
  PRODUCT_BATCH_SIZE_EXCEEDED("PRODUCT_BATCH_SIZE_EXCEEDED", "Product basic info batch size exceeded beyond ");



  private final String errorCode;
  private final String errorMessage;

  ApiErrorCodes(String errorCode,String errorMessage) {
    this.errorCode = errorCode;
    this.errorMessage = errorMessage;
  }

  public String getErrorCode() {
    return errorCode;
  }
  public String getErrorMessage()
  {
    return  errorMessage;
  }

}