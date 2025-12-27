package com.gdn.partners.pbp.model.promo;

import java.io.Serializable;
import java.util.Date;

import com.gdn.common.base.GdnObjects;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PromoMerchantHistory implements Serializable {
  private static final long serialVersionUID = -7588110338120192686L;

  private String promoId;
  private String activity;
  private String newValue;
  private String oldValue;
  private Date updatedDate;
  private String updatedBy;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getActivity() {
    return this.activity;
  }

  public String getNewValue() {
    return this.newValue;
  }

  public String getOldValue() {
    return this.oldValue;
  }

  public String getPromoId() {
    return this.promoId;
  }

  public String getUpdatedBy() {
    return this.updatedBy;
  }

  public Date getUpdatedDate() {
    return this.updatedDate;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setActivity(String activity) {
    this.activity = activity;
  }

  public void setNewValue(String newValue) {
    this.newValue = newValue;
  }

  public void setOldValue(String oldValue) {
    this.oldValue = oldValue;
  }

  public void setPromoId(String promoId) {
    this.promoId = promoId;
  }

  public void setUpdatedBy(String updatedBy) {
    this.updatedBy = updatedBy;
  }

  public void setUpdatedDate(Date updatedDate) {
    this.updatedDate = updatedDate;
  }

  @Override
  public String toString() {
    return String.format(
        "PromoMerchantHistory [promoId=%s, activity=%s, newValue=%s, oldValue=%s, updatedDate=%s, updatedBy=%s, toString()=%s]",
        this.promoId, this.activity, this.newValue, this.oldValue, this.updatedDate, this.updatedBy,
        super.toString());
  }
}
