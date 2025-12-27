package com.gdn.x.productcategorybase.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryTreeDTO implements Serializable{
  private static final long serialVersionUID = 3264458994785648746L;
  private String id;
  private String categoryCode;
  private String categoryName;
  private String categoryEnglishName;
  private String parentCategory;
  private List<CategoryTreeDTO> children;
  private List<String> documentType;
  private boolean genericTemplateEligible;
  private boolean b2bExclusive;
  
  public CategoryTreeDTO() {
    super();
  }
  
  public CategoryTreeDTO(String id, String categoryCode, String categoryName, String parentCategory) {
    super();
    this.id = id;
    this.categoryName = categoryName;
    this.categoryCode = categoryCode;
    
    if(! StringUtils.isEmpty(parentCategory)){
      this.parentCategory = parentCategory;
    } else{
      this.parentCategory = "";
    }
  }

  public CategoryTreeDTO(String id, String categoryCode, String categoryName, String categoryEnglishName,
      String parentCategory, boolean genericTemplateEligible) {
    super();
    this.id = id;
    this.categoryName = categoryName;
    this.categoryEnglishName = categoryEnglishName;
    this.categoryCode = categoryCode;
    this.genericTemplateEligible = genericTemplateEligible;

    if (!StringUtils.isEmpty(parentCategory)) {
      this.parentCategory = parentCategory;
    } else {
      this.parentCategory = "";
    }
  }

  public CategoryTreeDTO(String id, String categoryCode, String categoryName, String categoryEnglishName,
      String parentCategory, boolean genericTemplateEligible, String document) {
    super();
    this.id = id;
    this.categoryName = categoryName;
    this.categoryEnglishName = categoryEnglishName;
    this.categoryCode = categoryCode;
    this.genericTemplateEligible = genericTemplateEligible;
    if (!StringUtils.isEmpty(parentCategory)) {
      this.parentCategory = parentCategory;
    } else {
      this.parentCategory = StringUtils.EMPTY;
    }
    if (StringUtils.isNotEmpty(document)) {
      documentType = new ArrayList<>();
      this.documentType.addAll(Arrays.asList(document.split(",")));
    }
  }

  public CategoryTreeDTO(String id, String categoryCode, String categoryName, String categoryEnglishName,
      String parentCategory) {
    super();
    this.id = id;
    this.categoryName = categoryName;
    this.categoryEnglishName = categoryEnglishName;
    this.categoryCode = categoryCode;
    if (!StringUtils.isEmpty(parentCategory)) {
      this.parentCategory = parentCategory;
    } else {
      this.parentCategory = "";
    }
  }

  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }
  public String getCategoryName() {
    return categoryName;
  }
  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }
  public String getCategoryCode() {
    return categoryCode;
  }
  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }
  public String getParentCategory() {
    return parentCategory;
  }
  public void setParentCategory(String parentCategory) {
    this.parentCategory = parentCategory;
  }
  public List<CategoryTreeDTO> getChildren() {
    return children;
  }
  public void setChildren(List<CategoryTreeDTO> children) {
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

  public CategoryTreeDTO(String id, String categoryCode, String categoryName, String categoryEnglishName,
    String parentCategory, boolean genericTemplateEligible, String document, boolean b2bExclusive) {
    super();
    this.id = id;
    this.categoryName = categoryName;
    this.categoryEnglishName = categoryEnglishName;
    this.categoryCode = categoryCode;
    this.genericTemplateEligible = genericTemplateEligible;
    this.b2bExclusive = b2bExclusive;
    if (!StringUtils.isEmpty(parentCategory)) {
      this.parentCategory = parentCategory;
    } else {
      this.parentCategory = StringUtils.EMPTY;
    }
    if (StringUtils.isNotEmpty(document)) {
      documentType = new ArrayList<>();
      this.documentType.addAll(Arrays.asList(document.split(",")));
    }
  }

  @Override
  public String toString() {
    return "CategoryTreeDTO{" + "id='" + id + '\'' + ", categoryCode='" + categoryCode + '\''
      + ", categoryName='" + categoryName + '\'' + ", categoryEnglishName='" + categoryEnglishName
      + '\'' + ", parentCategory='" + parentCategory + '\'' + ", children=" + children
      + ", documentType=" + documentType + ", genericTemplateEligible=" + genericTemplateEligible
      + ", b2bExclusive=" + b2bExclusive + '}';
  }
}
