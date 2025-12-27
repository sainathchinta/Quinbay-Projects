package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemResponse extends BaseResponse{

  private static final long serialVersionUID = -3251968256791605219L;

  private List<OfflineItemResponseDetail> offlineProducts;

  public OfflineItemResponse() {
  }

  public OfflineItemResponse(List<OfflineItemResponseDetail> offlineProducts) {
    this.offlineProducts = offlineProducts;
  }

  public List<OfflineItemResponseDetail> getOfflineProducts() {
    return offlineProducts;
  }

  public void setOfflineProducts(List<OfflineItemResponseDetail> offlineProducts) {
    this.offlineProducts = offlineProducts;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override
  public String toString() {
    return String.format("OfflineItemResponse [offlineProducts=%s, toString()=%s]", this.offlineProducts, super.toString());
  }
}
