package com.gdn.x.productcategorybase.dto.request;

import com.gdn.common.base.GdnObjects;

public class AttributeAndValueByTypeRequest {

  public  enum type {
    ATTRIBUTE, ALLOWED_ATTRIBUTE_VALUE, PREDEFINED_ATTRIBUTE_VALUE_BY_ID, PREDEFINED_ATTRIBUTE_VALUE_BY_CODE
  }

  private String id;
  private Enum type;

  public AttributeAndValueByTypeRequest(String id, Enum type) {
    this.id = id;
    this.type = type;
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public Enum getType() {
    return type;
  }

  public void setType(Enum type) {
    this.type = type;
  }

  @Override
  public boolean equals(Object o) {
    return GdnObjects.equals(this, o);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("AttributeAndValueByTypeRequest{");
    sb.append("id='").append(id).append('\'');
    sb.append(", type=").append(type);
    sb.append('}');
    return sb.toString();
  }
}
