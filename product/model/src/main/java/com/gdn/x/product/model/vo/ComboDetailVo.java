package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;

import java.io.Serializable;

public class ComboDetailVo extends ComboVO implements Serializable {

  private static final long serialVersionUID = -7278660080015085883L;
  private int total;

  public int getTotal() {
    return total;
  }

  public void setTotal(int total) {
    this.total = total;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(obj, this);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ComboDetailVo{");
    sb.append(", total=").append(total);
    sb.append('}');
    return sb.toString();
  }
}
