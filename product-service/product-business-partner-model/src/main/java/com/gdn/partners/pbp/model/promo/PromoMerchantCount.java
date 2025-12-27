package com.gdn.partners.pbp.model.promo;

import java.io.Serializable;

import com.gdn.common.base.GdnObjects;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PromoMerchantCount implements Serializable {
  private static final long serialVersionUID = 6951002100091249893L;

  private int all;
  private int notYetActive;
  private int active;
  private int expired;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public int getActive() {
    return this.active;
  }

  public int getAll() {
    return this.all;
  }

  public int getExpired() {
    return this.expired;
  }

  public int getNotYetActive() {
    return this.notYetActive;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setActive(int active) {
    this.active = active;
  }

  public void setAll(int all) {
    this.all = all;
  }

  public void setExpired(int expired) {
    this.expired = expired;
  }

  public void setNotYetActive(int notYetActive) {
    this.notYetActive = notYetActive;
  }

  @Override
  public String toString() {
    return String.format(
        "PromoMerchantCount [all=%s, notYetActive=%s, active=%s, expired=%s, toString()=%s]",
        this.all, this.notYetActive, this.active, this.expired, super.toString());
  }
}
