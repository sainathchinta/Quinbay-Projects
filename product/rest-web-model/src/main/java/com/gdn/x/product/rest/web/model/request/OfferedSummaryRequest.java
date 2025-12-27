package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * Created by w.william on 2/22/2018.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class OfferedSummaryRequest extends BaseRequest {

  private static final long serialVersionUID = 1L;

  private String pristineId;
  private String itemCode;
  private String itemSku;
  private String defaultSku;

  public OfferedSummaryRequest() {
  }

  public OfferedSummaryRequest(String pristineId, String itemCode, String itemSku,
      String defaultSku) {
    this.pristineId = pristineId;
    this.itemCode = itemCode;
    this.itemSku = itemSku;
    this.defaultSku = defaultSku;
  }

  public String getDefaultSku() {
    return defaultSku;
  }

  public void setDefaultSku(String defaultSku) {
    this.defaultSku = defaultSku;
  }

  public String getPristineId() {
    return pristineId;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
  }

  public String getItemCode() {
    return itemCode;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("pristineId", pristineId).append("itemCode", itemCode)
        .append("itemSku", itemSku).append("defaultSku", defaultSku).toString();
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }
}
