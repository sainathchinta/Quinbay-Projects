package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.Date;

import jakarta.persistence.Enumerated;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

/**
 * Created by govind on 04/02/2019 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class DiscountPrice implements Serializable {

  private static final long serialVersionUID = 241609259142598221L;

  private double discountPrice;

  private Date startDateTime;

  private Date endDateTime;

  private String adjustmentName;

  private String campaignCode;

  private int priority;

  @Enumerated
  private AdjustmentType adjustmentType;

  public DiscountPrice() {

  }

  public DiscountPrice(double discountPrice, Date startDateTime, Date endDateTime,
      String adjustmentName, AdjustmentType adjustmentType) {
    super();
    this.discountPrice = Math.round(discountPrice);
    this.startDateTime = startDateTime;
    this.endDateTime = endDateTime;
    this.adjustmentName = adjustmentName;
    this.adjustmentType = adjustmentType;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getAdjustmentName() {
    return this.adjustmentName;
  }

  public AdjustmentType getAdjustmentType() {
    return this.adjustmentType;
  }

  public double getDiscountPrice() {
    return  Math.round(this.discountPrice);
  }

  public Date getEndDateTime() {
    return this.endDateTime;
  }

  public Date getStartDateTime() {
    return this.startDateTime;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setAdjustmentName(String adjustmentName) {
    this.adjustmentName = adjustmentName;
  }

  public void setAdjustmentType(AdjustmentType adjustmentType) {
    this.adjustmentType = adjustmentType;
  }

  public void setDiscountPrice(double discountPrice) {
    this.discountPrice = Math.round(discountPrice);
  }

  public void setEndDateTime(Date endDateTime) {
    this.endDateTime = endDateTime;
  }

  public void setStartDateTime(Date startDateTime) {
    this.startDateTime = startDateTime;
  }

  public String getCampaignCode() {
    return campaignCode;
  }

  public void setCampaignCode(String campaignCode) {
    this.campaignCode = campaignCode;
  }

  public int getPriority() {
    return priority;
  }

  public void setPriority(int priority) {
    this.priority = priority;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("discountPrice", discountPrice)
        .append("startDateTime", startDateTime).append("endDateTime", endDateTime)
        .append("adjustmentName", adjustmentName).append("adjustmentType", adjustmentType).append(
        "priority", priority).append("campaignCode", campaignCode)
        .toString();
  }
}
