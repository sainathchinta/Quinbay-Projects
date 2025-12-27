package com.gdn.x.product.rest.web.model.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SortedDefiningAttributeDTO {

  private String attributeName;

  private List<String> definingAttributes;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getAttributeName() {
    return attributeName;
  }

  public List<String> getDefiningAttributes() {
    return definingAttributes;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setAttributeName(String attributeName) {
    this.attributeName = attributeName;
  }

  public void setDefiningAttributes(List<String> definingAttributes) {
    this.definingAttributes = definingAttributes;
  }

  @Override
  public String toString() {
    return String.format("SortedDefiningAttribute [attributeName=%s, definingAttributes=%s]",
        attributeName, definingAttributes);
  }

}

