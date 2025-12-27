package com.gdn.x.productcategorybase.dto.categorytree;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryNodeFilterCategoryCodesRequest implements Serializable {

  private static final long serialVersionUID = 4555781378298848288L;
  private String catalogCode;
  private List<String> categoryCodes = new ArrayList<String>();
  private boolean active = false;

  public CategoryNodeFilterCategoryCodesRequest() {}

  public CategoryNodeFilterCategoryCodesRequest(String catalogCode, List<String> categoryCodes, boolean active) {
    super();
    this.catalogCode = catalogCode;
    this.categoryCodes = categoryCodes;
    this.active = active;
  }

  public String getCatalogCode() {
    return catalogCode;
  }

  public void setCatalogCode(String catalogCode) {
    this.catalogCode = catalogCode;
  }

  public List<String> getCategoryCodes() {
    return categoryCodes;
  }

  public void setCategoryCodes(List<String> categoryCodes) {
    this.categoryCodes = categoryCodes;
  }

  public boolean isActive() {
    return active;
  }

  public void setActive(boolean active) {
    this.active = active;
  }

  @Override
  public String toString() {
    return "CategoryNodeFilterCategoryCodesRequest [catalogCode=" + catalogCode + ", categoryCodes=" + categoryCodes
        + ", active=" + active + "]";
  }

}
