package com.gdn.x.productcategorybase.dto.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class CategoryTreeResponse extends BaseResponse {
  
  private String categoryCode;
  private String categoryName;
  private String parentCategory;
  private String categoryEnglishName;
  private List<String> documentType;
  private List<CategoryTreeResponse> children;
  private boolean genericTemplateEligible;
  private boolean b2bExclusive;
  
  public CategoryTreeResponse() {
    super();
  }
  
  public String getCategoryCode() {
    return categoryCode;
  }
  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }
  public String getCategoryName() {
    return categoryName;
  }
  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }
  public String getParentCategory() {
    return parentCategory;
  }
  public void setParentCategory(String parentCategory) {
    this.parentCategory = parentCategory;
  }
  public List<CategoryTreeResponse> getChildren() {
    return children;
  }
  public void setChildren(List<CategoryTreeResponse> children) {
    this.children = children;
  }

  public String getCategoryEnglishName() {
    return categoryEnglishName;
  }

  public void setCategoryEnglishName(String categoryEnglishName) {
    this.categoryEnglishName = categoryEnglishName;
  }

  public boolean isGenericTemplateEligible() {
    return genericTemplateEligible;
  }

  public void setGenericTemplateEligible(boolean genericTemplateEligible) {
    this.genericTemplateEligible = genericTemplateEligible;
  }

  public List<String> getDocumentType() {
    return documentType;
  }

  public void setDocumentType(List<String> documentType) {
    this.documentType = documentType;
  }

  public boolean isB2bExclusive() {
    return b2bExclusive;
  }

  public void setB2bExclusive(boolean b2bExclusive) {
    this.b2bExclusive = b2bExclusive;
  }
}
