package com.gdn.mta.bulk.dto;

/**
 * Created by hardikbohra on 05/06/18.
 */
public enum GenericTemplateFileType {
  DEFAULT_FILE("0",true),
  PURE_DELIVERY_FILE("1",true),
  CNC_FILE("2",true),
  BFB_NON_CNC_FILE("3",false),
  BFB_CNC_FILE("4",false),
  PURE_DELIVERY_BUNDLING_FILE("5", true),
  CNC_BUDNLING_FILE("6",true),
  BFB_NON_CNC_BUNDLING_FILE("7",false),
  BFB_CNC_BUNDLING_FILE("8",false),
  PURE_DELIVERY_INSTORE_FILE("9",true),
  CNC_INSTORE_FILE("10",true),
  BFB_NON_CNC_INSTORE_FILE("11",false),
  BFB_CNC_INSTORE_FILE("12",false),
  PURE_DELIVERY_BUNDLING_INSTORE_FILE("13", true),
  CNC_BUDNLING_INSTORE_FILE("14",true),
  BFB_NON_CNC_BUNDLING_INSTORE_FILE("15",false),
  BFB_CNC_BUNDLING_INSTORE_FILE("16",false);

  private String value;
  private boolean ignoreB2bExclusive;

  GenericTemplateFileType(String value, boolean ignoreB2bExclusive) {
    this.value = value;
    this.ignoreB2bExclusive = ignoreB2bExclusive;
  }

  public boolean isIgnoreB2bExclusive() {
    return ignoreB2bExclusive;
  }

  public String getValue() {
    return value;
  }

}