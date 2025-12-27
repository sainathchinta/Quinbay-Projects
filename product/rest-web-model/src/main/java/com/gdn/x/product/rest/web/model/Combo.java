package com.gdn.x.product.rest.web.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Combo implements Serializable {

  private static final long serialVersionUID = 4888894237113192908L;

  private String promoBundlingId;
  private String promoBundlingName;
  private String promoBundlingType;
  private Date startDate;
  private Date endDate;
  private List<ComboItem> comboItems = new ArrayList<ComboItem>();

  public Combo() {
  }

  public Combo(String promoBundlingId, String promoBundlingName, String promoBundlingType,
      Date startDate, Date endDate, List<ComboItem> comboItems) {
    this.promoBundlingId = promoBundlingId;
    this.promoBundlingName = promoBundlingName;
    this.promoBundlingType = promoBundlingType;
    this.startDate = startDate;
    this.endDate = endDate;
    this.comboItems = comboItems;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public List<ComboItem> getComboItems() {
    return comboItems;
  }

  public Date getEndDate() {
    return endDate;
  }

  public String getPromoBundlingId() {
    return promoBundlingId;
  }

  public String getPromoBundlingName() {
    return promoBundlingName;
  }

  public String getPromoBundlingType() {
    return promoBundlingType;
  }

  public Date getStartDate() {
    return startDate;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setComboItems(List<ComboItem> comboItems) {
    this.comboItems = comboItems;
  }

  public void setEndDate(Date endDate) {
    this.endDate = endDate;
  }

  public void setPromoBundlingId(String promoBundlingId) {
    this.promoBundlingId = promoBundlingId;
  }

  public void setPromoBundlingName(String promoBundlingName) {
    this.promoBundlingName = promoBundlingName;
  }

  public void setPromoBundlingType(String promoBundlingType) {
    this.promoBundlingType = promoBundlingType;
  }

  public void setStartDate(Date startDate) {
    this.startDate = startDate;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("Combo{");
    sb.append("promoBundlingId='").append(promoBundlingId).append('\'');
    sb.append(", promoBundlingName='").append(promoBundlingName).append('\'');
    sb.append(", promoBundlingType='").append(promoBundlingType).append('\'');
    sb.append(", startDate=").append(startDate);
    sb.append(", endDate=").append(endDate);
    sb.append(", comboItems=").append(comboItems);
    sb.append('}');
    return sb.toString();
  }
}
