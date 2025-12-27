package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

/**
 * Created by w.william on 3/9/2018.
 */
public class PromoBundlingDetailResponseVO implements Serializable {

  private static final long serialVersionUID = 1L;
  private String promoBundlingId;
  private String promoBundlingType;
  private String promoBundlingStatus;
  private Date startDate;
  private Date endDate;
  private int originalQuota;
  private int soldQuantity;
  private String itemSku;
  private String merchantCode;
  private String promoBundlingName;
  private List<ComboRuleVO> comboRules = new ArrayList<>();
  private List<WholesaleRuleVO> wholesaleRules = new ArrayList<>();
  private Set<String> channelRules;

  public PromoBundlingDetailResponseVO() {}

  public PromoBundlingDetailResponseVO(String id, String storeId, Date createdDate,
      String createdBy, Date updatedDate, String updatedBy, String promoBundlingId,
      String promoBundlingType, String promoBundlingStatus, Date startDate, Date endDate,
      int originalQuota, int soldQuantity, String itemSku, String merchantCode,
      String promoBundlingName, List<ComboRuleVO> comboRules, List<WholesaleRuleVO> wholesaleRules,
      Set<String> channelRules) {
    this.promoBundlingId = promoBundlingId;
    this.promoBundlingType = promoBundlingType;
    this.promoBundlingStatus = promoBundlingStatus;
    this.startDate = startDate;
    this.endDate = endDate;
    this.originalQuota = originalQuota;
    this.soldQuantity = soldQuantity;
    this.itemSku = itemSku;
    this.merchantCode = merchantCode;
    this.promoBundlingName = promoBundlingName;
    this.comboRules = comboRules;
    this.wholesaleRules = wholesaleRules;
    this.channelRules = channelRules;
  }

  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public Set<String> getChannelRules() {
    return this.channelRules;
  }

  public List<ComboRuleVO> getComboRules() {
    return this.comboRules;
  }

  public Date getEndDate() {
    return this.endDate;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public String getMerchantCode() {
    return this.merchantCode;
  }

  public int getOriginalQuota() {
    return this.originalQuota;
  }

  public String getPromoBundlingId() {
    return this.promoBundlingId;
  }

  public String getPromoBundlingName() {
    return this.promoBundlingName;
  }

  public String getPromoBundlingType() {
    return this.promoBundlingType;
  }

  public String getPromoBundlingStatus() {
    return this.promoBundlingStatus;
  }

  public int getSoldQuantity() {
    return this.soldQuantity;
  }

  public Date getStartDate() {
    return this.startDate;
  }

  public List<WholesaleRuleVO> getWholesaleRules() {
    return this.wholesaleRules;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setChannelRules(Set<String> channelRules) {
    this.channelRules = channelRules;
  }

  public void setComboRules(List<ComboRuleVO> comboRules) {
    this.comboRules = comboRules;
  }

  public void setEndDate(Date endDate) {
    this.endDate = endDate;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setOriginalQuota(int originalQuota) {
    this.originalQuota = originalQuota;
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

  public void setPromoName(String promoBundlingName) {
    this.promoBundlingName = promoBundlingName;
  }

  public void setSoldQuantity(int soldQuantity) {
    this.soldQuantity = soldQuantity;
  }

  public void setStartDate(Date startDate) {
    this.startDate = startDate;
  }

  public void setWholesaleRules(List<WholesaleRuleVO> wholesaleRules) {
    this.wholesaleRules = wholesaleRules;
  }

  public void setPromoBundlingStatus(String promoBundlingStatus) {
    this.promoBundlingStatus = promoBundlingStatus;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("promoBundlingId", promoBundlingId)
        .append("promoBundlingType", promoBundlingType)
        .append("promoBundlingStatus", promoBundlingStatus).append("startDate", startDate)
        .append("endDate", endDate).append("originalQuota", originalQuota)
        .append("soldQuantity", soldQuantity).append("itemSku", itemSku)
        .append("merchantCode", merchantCode).append("promoBundlingName", promoBundlingName)
        .append("comboRules", comboRules).append("wholesaleRules", wholesaleRules)
        .append("channelRules", channelRules).toString();
  }
}

