package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SalesCategorySequenceDTO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String categoryCode;
  private int sequence;

  public SalesCategorySequenceDTO() {
    super();
  }

  public SalesCategorySequenceDTO(String categoryCode, int sequence) {
    super();
    this.categoryCode = categoryCode;
    this.sequence = sequence;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getCategoryCode() {
    return this.categoryCode;
  }

  public int getSequence() {
    return this.sequence;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public void setSequence(int sequence) {
    this.sequence = sequence;
  }

  @Override
  public String toString() {
    return String.format("SalesCategorySequenceDTO [categoryCode=%s, sequence=%s, toString()=%s]",
        this.categoryCode, this.sequence, super.toString());
  }
}
