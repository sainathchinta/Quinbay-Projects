package com.gdn.partners.pbp.dto.productlevel3;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.BaseRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateProductLevel3WipRequest extends BaseRequest {

  private static final long serialVersionUID = 1358377900608120890L;
  private String businessPartnerCode;
  private String productLevel1Id;
  private String productSku;
  private String productName;
  private String brandName;
  private String categoryName;
  private List<UpdateProductLevel3ItemWipRequest> items = new ArrayList<>();
  private List<UpdateProductLevel3AttributeWipRequest> attributes = new ArrayList<>();

  public UpdateProductLevel3WipRequest() {

  }

  public UpdateProductLevel3WipRequest(String businessPartnerCode, String productLevel1Id, String productSku,
      String productName, String brandName, String categoryName,
      List<UpdateProductLevel3ItemWipRequest> items,
      List<UpdateProductLevel3AttributeWipRequest> attributes) {
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

  public List<UpdateProductLevel3ItemWipRequest> getItems() {
    return items;
  }

  public void setItems(List<UpdateProductLevel3ItemWipRequest> items) {
    this.items = items;
  }

  public List<UpdateProductLevel3AttributeWipRequest> getAttributes() {
    return attributes;
  }

  public void setAttributes(
      List<UpdateProductLevel3AttributeWipRequest> attributes) {
    this.attributes = attributes;
  }

  @Override
  public String toString() {
    return "UpdateProductLevel3WipRequest{" + "businessPartnerCode='" + businessPartnerCode + '\''
        + ", productLevel1Id='" + productLevel1Id + '\'' + ", productSku='" + productSku + '\'' + ", productName='"
        + productName + '\'' + ", brandName='" + brandName + '\'' + ", categoryName='" + categoryName + '\''
        + ", items=" + items + ", attributes="
        + attributes + '}';
  }
}
