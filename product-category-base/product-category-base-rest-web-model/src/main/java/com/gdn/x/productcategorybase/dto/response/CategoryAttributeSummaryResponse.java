package com.gdn.x.productcategorybase.dto.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryAttributeSummaryResponse extends BaseResponse {
  private String categoryCode;
  private String name;
  private String englishName;
  private List<AttributeSummaryResponse> attributes;
  
  public CategoryAttributeSummaryResponse() {
    super();
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
  public List<AttributeSummaryResponse> getAttributes() {
    return attributes;
  }
  public void setAttributes(List<AttributeSummaryResponse> attributes) {
    this.attributes = attributes;
  }

  public String getEnglishName() {
    return englishName;
  }

  public void setEnglishName(String englishName) {
    this.englishName = englishName;
  }
}
