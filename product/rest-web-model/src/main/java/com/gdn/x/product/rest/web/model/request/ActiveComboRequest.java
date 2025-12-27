package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
@JsonIgnoreProperties(ignoreUnknown = true)
public class ActiveComboRequest extends BaseRequest {

  private static final long serialVersionUID = 1L;

  private String pristineId;
  private String itemCode;
  private String itemSku;

  public ActiveComboRequest() {
  }

  public ActiveComboRequest(String pristineId, String itemCode, String itemSku) {
    this.pristineId = pristineId;
    this.itemCode = itemCode;
    this.itemSku = itemSku;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getItemCode() {
    return itemCode;
  }

  public String getItemSku() {
    return itemSku;
  }

  public String getPristineId() {
    return pristineId;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
  }

  @Override
  public String toString() {
    return String.format("ActiveComboRequest [pristineId=%s, itemCode=%s, itemSku=%s]",
        this.pristineId, this.itemCode, this.itemSku);
  }
}
