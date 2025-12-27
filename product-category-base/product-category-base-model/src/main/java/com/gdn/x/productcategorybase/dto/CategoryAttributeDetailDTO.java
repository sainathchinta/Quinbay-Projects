package com.gdn.x.productcategorybase.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryAttributeDetailDTO {
  
  private String categoryCode;
  private String name;
  private String englishName;
  private List<AttributeDetailDTO> attributes;
  
  public CategoryAttributeDetailDTO() {
    super();
  }

  public CategoryAttributeDetailDTO(String categoryCode, String name,
      List<AttributeDetailDTO> attributes) {
    super();
    this.categoryCode = categoryCode;
    this.name = name;
    this.attributes = attributes;
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

  public List<AttributeDetailDTO> getAttributes() {
    return attributes;
  }

  public void setAttributes(List<AttributeDetailDTO> attributes) {
    this.attributes = attributes;
  }

  public String getEnglishName() {
    return englishName;
  }

  public void setEnglishName(String englishName) {
    this.englishName = englishName;
  }
}
