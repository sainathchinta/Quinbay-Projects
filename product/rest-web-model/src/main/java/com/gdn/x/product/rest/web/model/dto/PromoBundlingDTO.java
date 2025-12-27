package com.gdn.x.product.rest.web.model.dto;

import com.gdn.common.base.GdnObjects;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class PromoBundlingDTO implements Serializable {

  private static final long serialVersionUID = 9207875421541645110L;
  private String promoBundlingId;
  private String promoBundlingName;
  private String promoBundlingType;
  private Date startDate;
  private Date endDate;
  private List<ComboDTO> comboRules = new ArrayList<ComboDTO>();

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(obj, this);
  }

  public Date getEndDate() {
    return endDate;
  }

  public List<ComboDTO> getComboRules() {
    return comboRules;
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

  public void setEndDate(Date endDate) {
    this.endDate = endDate;
  }

  public void setComboRules(List<ComboDTO> comboRules) {
    this.comboRules = comboRules;
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
    final StringBuilder sb = new StringBuilder("PromoBundlingDTO{");
    sb.append("promoBundlingId='").append(promoBundlingId).append('\'');
    sb.append(", promoBundlingName='").append(promoBundlingName).append('\'');
    sb.append(", promoBundlingType='").append(promoBundlingType).append('\'');
    sb.append(", startDate=").append(startDate);
    sb.append(", endDate=").append(endDate);
    sb.append(", comboRules=").append(comboRules);
    sb.append('}');
    return sb.toString();
  }
}
