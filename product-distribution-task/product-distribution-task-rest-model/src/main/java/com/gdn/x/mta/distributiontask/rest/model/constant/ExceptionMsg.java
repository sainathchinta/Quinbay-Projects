package com.gdn.x.mta.distributiontask.rest.model.constant;

/**
 * Created by Vishal on 14/12/16.
 */
public enum ExceptionMsg implements EnumValue<String> {
  EXCEPTION_VENDOR_CODE_INVALID_OR_NULL("EXCEPTION_INVALID_VENDOR_CODE"),
  EXCEPTION_PRODUCT_CODE_NULL("EXCEPTION_PRODUCT_CODE_NULL"),
  EXCEPTION_PRODUCT_ID_NULL("EXCEPTION_PRODUCT_ID_NULL"),
  EXCEPTION_REQUEST_NULL("EXCEPTION_REQUEST_NULL");

  private final String code;

  ExceptionMsg(String value) {
    this.code = value;
  }

  @Override public String getValue() {
    return this.code;
  }
}
