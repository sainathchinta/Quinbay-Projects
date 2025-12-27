package com.gdn.x.product.model.vo;

import java.io.Serializable;

import org.apache.commons.lang3.builder.ToStringBuilder;

public class DefaultItemSkuVO implements Serializable {

  private static final long serialVersionUID = -6119653667654203572L;
  private String defaultItemSku;

  public DefaultItemSkuVO(){
  }

  public DefaultItemSkuVO(String defaultItemSku) {
    this.defaultItemSku = defaultItemSku;
  }

  public String getDefaultItemSku() {
    return defaultItemSku;
  }

  public void setDefaultItemSku(String defaultItemSku) {
    this.defaultItemSku = defaultItemSku;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("defaultItemSku", defaultItemSku)
        .toString();
  }
}
