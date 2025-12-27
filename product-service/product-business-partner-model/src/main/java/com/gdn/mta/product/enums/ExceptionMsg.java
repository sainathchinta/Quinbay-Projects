package com.gdn.mta.product.enums;

/**
 * Created by Vishal on 21/02/17.
 */
public enum ExceptionMsg implements EnumValue<String>{
  EXCEPTION_TO_FETCH_GIVEN_PRODUCT("EXCEPTION_TO_FETCH_GIVEN_PRODUCT"),
  EXCEPTION_NULL_VALUED_FOR_ITEM_SKU("EXCEPTION_NULL_VALUED_FOR_ITEM_SKU");

  private final String exceptionMsg;

  ExceptionMsg(String exceptionMsg) {
    this.exceptionMsg = exceptionMsg;
  }

  @Override
  public String getValue() {
    return this.exceptionMsg;
  }
}
