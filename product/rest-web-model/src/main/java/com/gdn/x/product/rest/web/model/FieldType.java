package com.gdn.x.product.rest.web.model;

public enum FieldType {

  INT,
  DOUBLE,
  BYTE,
  SHORT,
  LONG,
  FLOAT,
  BOOLEAN,
  CHAR,
  JAVALANGINTEGER,
  JAVALANGDOUBLE,
  JAVALANGBYTE,
  JAVALANGSHORT,
  JAVALANGLONG,
  JAVALANGFLOAT,
  JAVALANGBOOLEAN,
  JAVALANGCHARACTER,
  JAVALANGSTRING,
  JAVAMATHBIGDECIMAL,
  JAVAUTILDATE,
  JAVAUTILMAP,
  JAVAUTILLIST,
  JAVAUTILSET,
  OBJECT;

  public static final FieldType value(String value) {
    try {
      return valueOf(value);
    } catch (Exception e) {
      return OBJECT;
    }
  }

}
