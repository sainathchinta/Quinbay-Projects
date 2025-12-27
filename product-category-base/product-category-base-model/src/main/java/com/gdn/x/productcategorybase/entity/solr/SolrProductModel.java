package com.gdn.x.productcategorybase.entity.solr;

import java.util.Date;
import java.util.List;

/**
 * Created by Kesha on 02/05/16.
 */
public class SolrProductModel {
  private String name;
  private String upcCode;
  private String finalCategoryId;
  private List<AttributeModel> attributeModelList;
  private Date updatedDate;
  private String productCategoryId;

  public SolrProductModel() {
    // default constructor
  }
  
  public SolrProductModel(String name, String upcCode, String finalCategoryId,
      List<AttributeModel> attributeModelList, Date updatedDate) {
    this.name = name;
    this.upcCode = upcCode;
    this.finalCategoryId = finalCategoryId;
    this.attributeModelList = attributeModelList;
    this.updatedDate = updatedDate;
  }

  public String getName() {
    return name;
  }

  public String getUpcCode() {
    return upcCode;
  }

  public String getFinalCategoryId() {
    return finalCategoryId;
  }

  public List<AttributeModel> getAttributeModelList() {
    return attributeModelList;
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

  public void setName(String name) {
    this.name = name;
  }
}
