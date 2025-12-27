package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class ComboResponseVO implements Serializable {

  private static final long serialVersionUID = 8229004504844191421L;

  private List<ComboVO> comboList = new ArrayList<>();
  private int totalRecords;

  public ComboResponseVO() {
  }

  public ComboResponseVO(List<ComboVO> comboList, int totalRecords) {
    this.comboList = comboList;
    this.totalRecords = totalRecords;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public List<ComboVO> getComboList() {
    return comboList;
  }

  public int getTotalRecords() {
    return totalRecords;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setComboList(List<ComboVO> comboList) {
    this.comboList = comboList;
  }

  public void setTotalRecords(int totalRecords) {
    this.totalRecords = totalRecords;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ComboResponseVO{");
    sb.append("comboList=").append(comboList);
    sb.append(", totalRecords=").append(totalRecords);
    sb.append('}');
    return sb.toString();
  }
}
