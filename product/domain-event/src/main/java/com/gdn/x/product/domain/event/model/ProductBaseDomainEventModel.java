package com.gdn.x.product.domain.event.model;

import com.gdn.common.base.GdnObjects;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

public abstract class ProductBaseDomainEventModel extends GdnBaseDomainEventModel {

  private static final long serialVersionUID = -4613555985558824781L;

  private String storeId;

  private boolean markForDelete;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getStoreId() {
    return storeId;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductBaseDomainEventModel [storeId=%s, markForDelete=%s, toString()=%s]", storeId,
        markForDelete, super.toString());
  }

}
