package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataProductInfo implements Serializable {
  private static final long serialVersionUID = 1L;

  private String brand;
  private double shippingWeight;
  private String specificationDetail;
  private String productName;
  private String description;
  private String longDescription;
  private String uniqueSellingPoint;
  private List<MasterDataProductImageDTO> masterDataProductImages;
  private List<MasterDataProductAttributeDTO> masterDataProductAttributes;
  private List<SortedDefiningAttributeDTO> sortedDefiningAttributes;
  private String url;
  private String brandLogoUrl;

  public MasterDataProductInfo() {

  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public String getBrand() {
    return brand;
  }

  public String getBrandLogoUrl() {
    return brandLogoUrl;
  }

  public String getDescription() {
    return description;
  }

  public String getLongDescription() {
    return longDescription;
  }

  public List<MasterDataProductAttributeDTO> getMasterDataProductAttributes() {
    return masterDataProductAttributes;
  }

  public List<MasterDataProductImageDTO> getMasterDataProductImages() {
    return masterDataProductImages;
  }

  public String getProductName() {
    return productName;
  }

  public double getShippingWeight() {
    return shippingWeight;
  }

  public List<SortedDefiningAttributeDTO> getSortedDefiningAttributes() {
    return sortedDefiningAttributes;
  }

  public String getSpecificationDetail() {
    return specificationDetail;
  }

  public String getUniqueSellingPoint() {
    return uniqueSellingPoint;
  }

  public String getUrl() {
    return url;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setBrandLogoUrl(String brandLogoUrl) {
    this.brandLogoUrl = brandLogoUrl;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public void setLongDescription(String longDescription) {
    this.longDescription = longDescription;
  }

  public void setMasterDataProductAttributes(
      List<MasterDataProductAttributeDTO> masterDataProductAttributes) {
    this.masterDataProductAttributes = masterDataProductAttributes;
  }

  public void setMasterDataProductImages(List<MasterDataProductImageDTO> masterDataProductImages) {
    this.masterDataProductImages = masterDataProductImages;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setShippingWeight(double shippingWeight) {
    this.shippingWeight = shippingWeight;
  }

  public void setSortedDefiningAttributes(List<SortedDefiningAttributeDTO> sortedDefiningAttributes) {
    this.sortedDefiningAttributes = sortedDefiningAttributes;
  }

  public void setSpecificationDetail(String specificationDetail) {
    this.specificationDetail = specificationDetail;
  }

  public void setUniqueSellingPoint(String uniqueSellingPoint) {
    this.uniqueSellingPoint = uniqueSellingPoint;
  }

  public void setUrl(String url) {
    this.url = url;
  }

  @Override
  public String toString() {
    return String.format(
        "MasterDataProductInfo [brand=%s, shippingWeight=%s, specificationDetail=%s, productName=%s, description=%s, longDescription=%s, uniqueSellingPoint=%s, masterDataProductImages=%s, masterDataProductAttributes=%s, sortedDefiningAttributes=%s, url=%s, brandLogoUrl=%s, toString()=%s]",
        brand, shippingWeight, specificationDetail, productName, description, longDescription,
        uniqueSellingPoint, masterDataProductImages, masterDataProductAttributes,
        sortedDefiningAttributes, url, brandLogoUrl, super.toString());
  }

}
