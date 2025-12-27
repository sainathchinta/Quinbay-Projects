package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class ComboVO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String promoBundlingId;
  private String promoBundlingName;
  private String PromoBundlingType;
  private Date startDate;
  private Date endDate;
  private List<ComboItemVO> comboItems = new ArrayList<>();

  public ComboVO() {
  }

  public ComboVO(String promoBundlingId, String itemSku, String merchantCode,
      String promoBundlingName, String promoBundlingType, Date startDate, Date endDate,
      List<ComboItemVO> comboItems) {
    this.promoBundlingId = promoBundlingId;
    this.promoBundlingName = promoBundlingName;
    PromoBundlingType = promoBundlingType;
    this.startDate = startDate;
    this.endDate = endDate;
    this.comboItems = comboItems;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public Date getEndDate() {
    return endDate;
  }

  public String getPromoBundlingId() {
    return promoBundlingId;
  }

  public List<ComboItemVO> getComboItems() {
    return comboItems;
  }

  public String getPromoBundlingName() {
    return promoBundlingName;
  }

  public String getPromoBundlingType() {
    return PromoBundlingType;
  }

  public Date getStartDate() {
    return startDate;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setEndDate(Date endDate) {
    this.endDate = endDate;
  }

  public void setPromoBundlingId(String promoBundlingId) {
    this.promoBundlingId = promoBundlingId;
  }

  public void setComboItems(List<ComboItemVO> comboItems) {
    this.comboItems = comboItems;
  }

  public void setPromoBundlingName(String promoBundlingName) {
    this.promoBundlingName = promoBundlingName;
  }

  public void setPromoBundlingType(String promoBundlingType) {
    PromoBundlingType = promoBundlingType;
  }

  public void setStartDate(Date startDate) {
    this.startDate = startDate;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("promoBundlingId", promoBundlingId)
        .append("promoBundlingName", promoBundlingName)
        .append("PromoBundlingType", PromoBundlingType).append("startDate", startDate)
        .append("endDate", endDate).append("comboItems", comboItems).toString();
  }
}
