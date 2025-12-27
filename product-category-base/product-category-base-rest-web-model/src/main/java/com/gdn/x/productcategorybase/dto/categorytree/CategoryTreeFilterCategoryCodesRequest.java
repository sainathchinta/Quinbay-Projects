package com.gdn.x.productcategorybase.dto.categorytree;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryTreeFilterCategoryCodesRequest implements Serializable {

  private static final long serialVersionUID = 5315487639344236025L;
  private String catalogCode;
  private List<String> categoryCodes = new ArrayList<String>();
  private boolean active = false;
  private boolean buildTree = false;

  public CategoryTreeFilterCategoryCodesRequest() {}

  public CategoryTreeFilterCategoryCodesRequest(String catalogCode, List<String> categoryCodes, boolean active,
      boolean buildTree) {
    super();
    this.catalogCode = catalogCode;
    this.categoryCodes = categoryCodes;
    this.active = active;
    this.buildTree = buildTree;
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

  public boolean isBuildTree() {
    return buildTree;
  }

  public void setBuildTree(boolean buildTree) {
    this.buildTree = buildTree;
  }

  @Override
  public String toString() {
    return String.format(
        "CategoryTreeFilterCategoryCodesRequest [catalogCode=%s, categoryCodes=%s, active=%s, buildTree=%s]",
        catalogCode, categoryCodes, active, buildTree);
  }

}
