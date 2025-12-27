package com.gdn.mta.product.commons.constant;

/**
 * Created by hardikbohra on 23/03/18.
 */
public enum UpdateProductAccessChannel {
  MTA_WEB_UPDATE_DETAIL("MTA web update detail"),
  MTA_WEB_UPDATE_BULK("MTA web bulk update"),
  MTA_WEB_UPDATE_LIST("MTA web update list"),
  MTA_API_UPDATE_DETAIL("MTA-API update detail"),
  MTA_API_UPDATE_SUMMARY("MTA-API update summary"),
  MTA_API_UPDATE_SINGLE("MTA-API update single");
  private String desc;

  UpdateProductAccessChannel(String desc) {
    this.desc = desc;
  }

  public String getDesc() {
    return desc;
  }
}
