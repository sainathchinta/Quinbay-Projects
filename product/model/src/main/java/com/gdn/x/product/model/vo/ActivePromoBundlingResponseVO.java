package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class ActivePromoBundlingResponseVO implements Serializable {

  private static final long serialVersionUID = 2986292535797314712L;

  private List<PromoBundlingDetailResponseVO> promoBundlingDetailResponseVOList = new ArrayList<>();
  private int totalRecords;

  public ActivePromoBundlingResponseVO() {
  }

  public ActivePromoBundlingResponseVO(
      List<PromoBundlingDetailResponseVO> promoBundlingDetailResponseVOList, int totalRecords) {
    this.promoBundlingDetailResponseVOList = promoBundlingDetailResponseVOList;
    this.totalRecords = totalRecords;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public List<PromoBundlingDetailResponseVO> getPromoBundlingDetailResponseVOList() {
    return promoBundlingDetailResponseVOList;
  }

  public int getTotalRecords() {
    return totalRecords;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setPromoBundlingDetailResponseVOList(
      List<PromoBundlingDetailResponseVO> promoBundlingDetailResponseVOList) {
    this.promoBundlingDetailResponseVOList = promoBundlingDetailResponseVOList;
  }

  public void setTotalRecords(int totalRecords) {
    this.totalRecords = totalRecords;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ActivePromoBundlingResponseVO{");
    sb.append("promoBundlingDetailResponseVOList=").append(promoBundlingDetailResponseVOList);
    sb.append(", totalRecords=").append(totalRecords);
    sb.append('}');
    return sb.toString();
  }
}
