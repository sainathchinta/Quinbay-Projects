package com.gdn.x.productcategorybase.domain.event.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemAttributeValueDomainEventModel implements Serializable {

  private static final long serialVersionUID = -3774320226015191255L;
  private AttributeDomainEventModel attribute;

  private String value;

  public ProductItemAttributeValueDomainEventModel() {}

  public ProductItemAttributeValueDomainEventModel(AttributeDomainEventModel attribute, String value) {
    this.attribute = attribute;
    this.value = value;
  }

  public AttributeDomainEventModel getAttribute() {
    return attribute;
  }

  public void setAttribute(AttributeDomainEventModel attribute) {
    this.attribute = attribute;
  }

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = value;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductItemAttributeValueDomainEventModel{");
    sb.append("attribute=").append(attribute);
    sb.append(", value='").append(value).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
