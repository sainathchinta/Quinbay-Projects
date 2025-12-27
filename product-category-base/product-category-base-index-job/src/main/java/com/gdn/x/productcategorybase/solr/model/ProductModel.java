package com.gdn.x.productcategorybase.solr.model;

import java.util.Date;

/**
 * Created by Kesha on 24/04/16.
 */
public class ProductModel {
  private String id;
  private String productCode;
  private String name;
  private Date updatedDate;


  public ProductModel(String id, String name, String productCode, Date updatedDate) {
    this.id = id;
    this.productCode = productCode;
    this.name = name;
    this.updatedDate = updatedDate;
  }

  public String getId() {
    return id;
  }

  public String getProductCode() {
    return productCode;
  }

  public String getName() {
    return name;
  }

  public Date getUpdatedDate() {
    return updatedDate;
  }


}

