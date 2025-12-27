package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataProduct implements Serializable {

  private static final long serialVersionUID = 3268067485630463730L;

  private String brand;

  private double shippingWeight;

  private String specificationDetail;

  private String productName;

  private String description;

  private String longDescription;

  private String uniqueSellingPoint;

  private boolean isActivated;

  private boolean isViewable;

  private String productStory;

  private String uom;

  private List<MasterDataProductImage> masterDataProductImages;

  private List<MasterDataProductAttribute> masterDataProductAttributes;

  private String url;

  private double length;

  private double width;

  private double height;

  private double weight;

  private String brandLogoUrl;

  public MasterDataProduct() {

  }

  public MasterDataProduct(String brand, double shippingWeight, String specificationDetail,
      String productName, String description, String longDescription, String uniqueSellingPoint,
      boolean isActivated, boolean isViewable, String productStory, String uom,
      List<MasterDataProductImage> masterDataProductImages,
      List<MasterDataProductAttribute> masterDataProductAttributes, String url, double length,
      double width, double height, double weight) {
    super();
    this.brand = brand;
    this.shippingWeight = shippingWeight;
    this.specificationDetail = specificationDetail;
    this.productName = productName;
    this.description = description;
    this.longDescription = longDescription;
    this.uniqueSellingPoint = uniqueSellingPoint;
    this.isActivated = isActivated;
    this.isViewable = isViewable;
    this.productStory = productStory;
    this.uom = uom;
    this.masterDataProductImages = masterDataProductImages;
    this.masterDataProductAttributes = masterDataProductAttributes;
    this.url = url;
    this.length = length;
    this.width = width;
    this.height = height;
    this.weight = weight;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getBrand() {
    return this.brand;
  }

  public String getDescription() {
    return this.description;
  }

  public double getHeight() {
    return this.height;
  }

  public double getLength() {
    return this.length;
  }

  public String getLongDescription() {
    return this.longDescription;
  }

  public List<MasterDataProductAttribute> getMasterDataProductAttributes() {
    return this.masterDataProductAttributes;
  }

  public List<MasterDataProductImage> getMasterDataProductImages() {
    return this.masterDataProductImages;
  }

  public String getProductName() {
    return this.productName;
  }

  public String getProductStory() {
    return this.productStory;
  }

  public double getShippingWeight() {
    return this.shippingWeight;
  }

  public String getSpecificationDetail() {
    return this.specificationDetail;
  }

  public String getUniqueSellingPoint() {
    return this.uniqueSellingPoint;
  }

  public String getUom() {
    return this.uom;
  }

  public String getUrl() {
    return this.url;
  }

  public double getWeight() {
    return this.weight;
  }

  public double getWidth() {
    return this.width;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isActivated() {
    return this.isActivated;
  }

  public boolean isViewable() {
    return this.isViewable;
  }

  public void setActivated(boolean isActivated) {
    this.isActivated = isActivated;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public void setHeight(double height) {
    this.height = height;
  }

  public void setLength(double length) {
    this.length = length;
  }

  public void setLongDescription(String longDescription) {
    this.longDescription = longDescription;
  }

  public void setMasterDataProductAttributes(
      List<MasterDataProductAttribute> masterDataProductAttributes) {
    this.masterDataProductAttributes = masterDataProductAttributes;
  }

  public void setMasterDataProductImages(List<MasterDataProductImage> masterDataProductImages) {
    this.masterDataProductImages = masterDataProductImages;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setProductStory(String productStory) {
    this.productStory = productStory;
  }

  public void setShippingWeight(double shippingWeight) {
    this.shippingWeight = shippingWeight;
  }

  public void setSpecificationDetail(String specificationDetail) {
    this.specificationDetail = specificationDetail;
  }

  public void setUniqueSellingPoint(String uniqueSellingPoint) {
    this.uniqueSellingPoint = uniqueSellingPoint;
  }

  public void setUom(String uom) {
    this.uom = uom;
  }

  public void setUrl(String url) {
    this.url = url;
  }

  public void setViewable(boolean isViewable) {
    this.isViewable = isViewable;
  }

  public void setWeight(double weight) {
    this.weight = weight;
  }

  public void setWidth(double width) {
    this.width = width;
  }

  public String getBrandLogoUrl() {
    return brandLogoUrl;
  }

  public void setBrandLogoUrl(String brandLogoUrl) {
    this.brandLogoUrl = brandLogoUrl;
  }

  @Override
  public String toString() {
    return String
        .format(
            "MasterDataProduct [brand=%s, brandLogoUrl=%s, shippingWeight=%s, specificationDetail=%s, productName=%s, description=%s, longDescription=%s, uniqueSellingPoint=%s, isActivated=%s, isViewable=%s, productStory=%s, uom=%s, productImages=%s, productAttributes=%s, url=%s, length=%s, width=%s, height=%s, weight=%s, toString()=%s]",
            this.brand, this.brandLogoUrl, this.shippingWeight, this.specificationDetail, this.productName,
            this.description, this.longDescription, this.uniqueSellingPoint, this.isActivated,
            this.isViewable, this.productStory, this.uom, this.masterDataProductImages,
            this.masterDataProductAttributes, this.url, this.length, this.width, this.height,
            this.weight, super.toString());
  }

}
