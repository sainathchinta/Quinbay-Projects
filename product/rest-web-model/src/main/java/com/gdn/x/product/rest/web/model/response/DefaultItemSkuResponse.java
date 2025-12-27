package com.gdn.x.product.rest.web.model.response;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.gdn.common.web.base.BaseResponse;

public class DefaultItemSkuResponse extends BaseResponse {

  private static final long serialVersionUID = -185288528L;

  private String defaultItemSku;

  public String getDefaultItemSku() {
    return defaultItemSku;
  }

  public void setDefaultItemSku(String defaultItemSku) {
    this.defaultItemSku = defaultItemSku;
  }

  public DefaultItemSkuResponse() {

  }

  public DefaultItemSkuResponse(String defaultItemSku) {
    this.defaultItemSku = defaultItemSku;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("defaultItemSku", defaultItemSku)
        .toString();
  }
}
