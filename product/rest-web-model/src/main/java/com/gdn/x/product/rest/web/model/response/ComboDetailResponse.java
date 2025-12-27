package com.gdn.x.product.rest.web.model.response;

import com.gdn.common.base.GdnObjects;

import java.io.Serializable;

public class ComboDetailResponse extends ComboResponse implements Serializable {
  private static final long serialVersionUID = 3947125757021840680L;
  private int total;

  public int getTotal() {
    return total;
  }

  public void setTotal(int total) {
    this.total = total;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ComboDetailResponse{");
    sb.append("total=").append(total);
    sb.append('}');
    return sb.toString();
  }
}
