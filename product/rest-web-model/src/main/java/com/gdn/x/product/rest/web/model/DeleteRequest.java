package com.gdn.x.product.rest.web.model;

import com.gdn.common.base.GdnObjects;

public class DeleteRequest {
  private String id;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getId() {
    return this.id;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setId(String id) {
    this.id = id;
  }

  @Override
  public String toString() {
    return String.format("DeleteRequest [id=%s, toString()=%s]", this.id, super.toString());
  }

}
