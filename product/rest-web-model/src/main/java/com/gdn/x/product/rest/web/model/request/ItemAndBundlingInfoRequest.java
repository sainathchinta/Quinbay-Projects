package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;

import java.util.Set;
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemAndBundlingInfoRequest extends BaseRequest {

  private static final long serialVersionUID = -5861056327071034963L;

  private Set<String> itemSkus;
  private Set<String> promoBundlingIds;

  public ItemAndBundlingInfoRequest() {
  }

  public ItemAndBundlingInfoRequest(Set<String> itemSkus, Set<String> promoBundlingIds) {
    this.itemSkus = itemSkus;
    this.promoBundlingIds = promoBundlingIds;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public Set<String> getItemSkus() {
    return itemSkus;
  }

  public Set<String> getPromoBundlingIds() {
    return promoBundlingIds;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItemSkus(Set<String> itemSkus) {
    this.itemSkus = itemSkus;
  }

  public void setPromoBundlingIds(Set<String> promoBundlingIds) {
    this.promoBundlingIds = promoBundlingIds;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ItemAndBundlingInfoRequest{");
    sb.append("itemSkus=").append(itemSkus);
    sb.append(", promoBundlingIds=").append(promoBundlingIds);
    sb.append('}');
    return sb.toString();
  }
}
