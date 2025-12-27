package com.gdn.x.product.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * @author anand
 * @since Nov 2019
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemSyncEvent extends GdnBaseDomainEventModel {

  private String storeId;

  private String itemSku;

  private String linkedPartnerCode;

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String getLinkedPartnerCode() {
    return linkedPartnerCode;
  }

  public void setLinkedPartnerCode(String linkedPartnerCode) {
    this.linkedPartnerCode = linkedPartnerCode;
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
    return new ToStringBuilder(this)
      .append("storeId", storeId)
      .append("itemSku", itemSku)
      .append("linkedPartnerCode", linkedPartnerCode)
      .toString();
  }

}
