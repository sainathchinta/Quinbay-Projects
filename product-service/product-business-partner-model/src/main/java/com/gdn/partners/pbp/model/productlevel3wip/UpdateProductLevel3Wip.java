package com.gdn.partners.pbp.model.productlevel3wip;

import java.util.ArrayList;
import java.util.List;

import com.gdn.common.web.base.BaseResponse;

public class UpdateProductLevel3Wip extends BaseResponse {

  private static final long serialVersionUID = -5690420300738886177L;
  private String businessPartnerCode;
  private String productLevel1Id;
  private String productSku;
  private String productName;
  private String brandName;
  private String categoryName;
  private List<UpdateProductLevel3ItemWip> items = new ArrayList<>();
  private List<UpdateProductLevel3AttributeWip> attributes = new ArrayList<>();

  public UpdateProductLevel3Wip() {

  }

  public UpdateProductLevel3Wip(String businessPartnerCode, String productLevel1Id, String productSku,
      String productName, String brandName, String categoryName, List<UpdateProductLevel3ItemWip> items,
      List<UpdateProductLevel3AttributeWip> attributes) {
    this.businessPartnerCode = businessPartnerCode;
    this.productLevel1Id = productLevel1Id;
    this.productSku = productSku;
    this.productName = productName;
    this.brandName = brandName;
    this.categoryName = categoryName;
    this.items = items;
    this.attributes = attributes;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getProductLevel1Id() {
    return productLevel1Id;
  }

  public void setProductLevel1Id(String productLevel1Id) {
    this.productLevel1Id = productLevel1Id;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getBrandName() {
    return brandName;
  }

  public void setBrandName(String brandName) {
    this.brandName = brandName;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }

  public List<UpdateProductLevel3ItemWip> getItems() {
    return items;
  }

  public void setItems(List<UpdateProductLevel3ItemWip> items) {
    this.items = items;
  }

  public List<UpdateProductLevel3AttributeWip> getAttributes() {
    return attributes;
  }

  public void setAttributes(List<UpdateProductLevel3AttributeWip> attributes) {
    this.attributes = attributes;
  }

  @Override
  public String toString() {
    return "UpdateProductLevel3Wip{" + "businessPartnerCode='" + businessPartnerCode + '\'' + ", productLevel1Id='"
        + productLevel1Id + '\'' + ", productSku='" + productSku + '\'' + ", productName='" + productName + '\''
        + ", brandName='" + brandName + '\'' + ", categoryName='" + categoryName + '\'' + ", items="
        + items + ", attributes=" + attributes + '}';
  }
}
