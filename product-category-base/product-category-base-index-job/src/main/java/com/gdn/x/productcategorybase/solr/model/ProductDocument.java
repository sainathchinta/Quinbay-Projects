package com.gdn.x.productcategorybase.solr.model;

import java.util.Date;
import java.util.List;
import java.util.Set;

/**
 * Created by Kesha on 24/04/16.
 */
public class ProductDocument {

  private String id;
  private String productCode;
  
  // refers to parent most category
  private String categoryId;
  private String productName;
  private List<AttributeModel> attributes;
  private Set<String> upcCodes;
  private Date updatedDate;
  // refers to the product category
  private String productCategoryId;

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getCategoryId() {
    return categoryId;
  }

  public void setCategoryId(String categoryId) {
    this.categoryId = categoryId;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public List<AttributeModel> getAttributes() {
    return attributes;
  }

  public void setAttributes(List<AttributeModel> attributes) {
    this.attributes = attributes;
  }

  public Set<String> getUpcCodes() {
    return upcCodes;
  }

  public void setUpcCodes(Set<String> upcCodes) {
    this.upcCodes = upcCodes;
  }

  public Date getUpdatedDate() {
    return updatedDate;
  }

  public void setUpdatedDate(Date updatedDate) {
    this.updatedDate = updatedDate;
  }

  public String getProductCategoryId() {
    return productCategoryId;
  }

  public void setProductCategoryId(String productCategoryId) {
    this.productCategoryId = productCategoryId;
  }

}
