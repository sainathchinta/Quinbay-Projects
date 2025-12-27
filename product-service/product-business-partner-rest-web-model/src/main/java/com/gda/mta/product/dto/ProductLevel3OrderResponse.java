package com.gda.mta.product.dto;

import java.util.List;

import com.gdn.common.web.base.BaseResponse;

public class ProductLevel3OrderResponse extends BaseResponse {

  private static final long serialVersionUID = 5396987035880931557L;
  private String description;
  private String specificationDetail;
  private String productStory;
  private String brand;
  private List<ProductItemLevel3OrderResponse> items;
  private Boolean installationRequired;

  public ProductLevel3OrderResponse() {
    // do nothing
  }

  public ProductLevel3OrderResponse(String description, String specificationDetail,
      String productStory, List<ProductItemLevel3OrderResponse> items, Boolean installationRequired) {
    super();
    this.description = description;
    this.specificationDetail = specificationDetail;
    this.productStory = productStory;
    this.items = items;
    this.installationRequired = installationRequired;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public String getSpecificationDetail() {
    return specificationDetail;
  }

  public void setSpecificationDetail(String specificationDetail) {
    this.specificationDetail = specificationDetail;
  }

  public String getProductStory() {
    return productStory;
  }

  public void setProductStory(String productStory) {
    this.productStory = productStory;
  }

  public List<ProductItemLevel3OrderResponse> getItems() {
    return items;
  }

  public void setItems(List<ProductItemLevel3OrderResponse> items) {
    this.items = items;
  }

  public Boolean getInstallationRequired() {
    return installationRequired;
  }

  public void setInstallationRequired(Boolean installationRequired) {
    this.installationRequired = installationRequired;
  }

  public String getBrand() {
    return brand;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductLevel3OrderResponse [description=%s, specificationDetail=%s, productStory=%s, items=%s, installationRequired=%s, getDescription()=%s, getSpecificationDetail()=%s, getProductStory()=%s, getItems()=%s, getInstallationRequired()=%s, brand=%s]",
            description, specificationDetail, productStory, items, installationRequired,
            getDescription(), getSpecificationDetail(), getProductStory(), getItems(),
            getInstallationRequired(), getBrand());
  }

}
