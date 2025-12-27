package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.io.Serializable;

public class ActiveComboRequestVO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String pristineId;
  private String itemCode;
  private String itemSku;

  public ActiveComboRequestVO() {
  }

  public ActiveComboRequestVO(String pristineId, String itemCode, String itemSku) {
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
    return new ToStringBuilder(this).append("pristineId", pristineId).append("itemCode", itemCode)
        .append("itemSku", itemSku).toString();
  }
}
