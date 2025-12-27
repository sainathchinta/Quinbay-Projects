package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryReferenceRequest extends BaseDTORequest {

  private static final long serialVersionUID = -2567671260895907361L;
  private String salesCategoryId;
  private String masterCategoryId;

  public CategoryReferenceRequest() {}

  public CategoryReferenceRequest(String salesCategoryId, String masterCategoryId) {
    this.salesCategoryId = salesCategoryId;
    this.masterCategoryId = masterCategoryId;
  }

  public String getMasterCategoryId() {
    return this.masterCategoryId;
  }

  public String getSalesCategoryId() {
    return this.salesCategoryId;
  }

  public void setMasterCategoryId(String masterCategoryId) {
    this.masterCategoryId = masterCategoryId;
  }

  public void setSalesCategoryId(String salesCategoryId) {
    this.salesCategoryId = salesCategoryId;
  }

  @Override
  public String toString() {
    return "CategoryReferenceRequest{" + "salesCategoryId='" + this.salesCategoryId + '\'' + ", masterCategoryId="
        + this.masterCategoryId + '}';
  }
}
