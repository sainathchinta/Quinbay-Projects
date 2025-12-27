package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Created by govind on 02/07/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ParentCategory implements Serializable {

  private static final long serialVersionUID = -2444083325505796216L;

  private String categoryCode;
  private String name;

  public ParentCategory() {
  }

  public ParentCategory(String categoryCode, String name) {
    this.categoryCode = categoryCode;
    this.name = name;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("categoryCode", categoryCode)
        .append("name", name).toString();
  }
}
