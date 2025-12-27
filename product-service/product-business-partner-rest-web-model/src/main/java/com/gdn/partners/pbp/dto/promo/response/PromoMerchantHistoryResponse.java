package com.gdn.partners.pbp.dto.promo.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PromoMerchantHistoryResponse extends BaseResponse {
  private static final long serialVersionUID = -5259283235027623424L;

  private String promoId;
  private String activity;
  private String newValue;
  private String oldValue;

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

  @Override
  public String toString() {
    return String.format(
        "PromoMerchantHistoryResponse [promoId=%s, activity=%s, newValue=%s, oldValue=%s, toString()=%s]",
        this.promoId, this.activity, this.newValue, this.oldValue, super.toString());
  }
}
