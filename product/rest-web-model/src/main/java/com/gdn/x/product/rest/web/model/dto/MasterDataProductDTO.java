package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataProductDTO implements Serializable {
  private static final long serialVersionUID = 1L;

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
  private String sizeAttributeValueType;
  private List<MasterDataProductImageDTO> masterDataProductImages;
  private List<MasterDataProductAttributeDTO> masterDataProductAttributes;
  private List<SortedDefiningAttributeDTO> sortedDefiningAttributes;
  private String url;
  private VideoUrl video;
  private String brandLogoUrl;

  public MasterDataProductDTO() {

  }

  public MasterDataProductDTO(String brand, double shippingWeight, String specificationDetail,
      String productName, String description, String longDescription, String uniqueSellingPoint,
      boolean isActivated, boolean isViewable, String productStory, String uom,
      List<MasterDataProductImageDTO> masterDataProductImages,
      List<MasterDataProductAttributeDTO> masterDataProductAttributes, String url,
      String brandLogoUrl) {
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
    this.brandLogoUrl = brandLogoUrl;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public String getBrand() {
    return this.brand;
  }

  public String getDescription() {
    return this.description;
  }

  public String getLongDescription() {
    return this.longDescription;
  }

  public List<MasterDataProductAttributeDTO> getMasterDataProductAttributes() {
    return this.masterDataProductAttributes;
  }

  public List<MasterDataProductImageDTO> getMasterDataProductImages() {
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

  public List<SortedDefiningAttributeDTO> getSortedDefiningAttributes() {
    return sortedDefiningAttributes;
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
  
  public String getBrandLogoUrl() {
    return this.brandLogoUrl;
  }

  public String getSizeAttributeValueType() {
    return this.sizeAttributeValueType;
  }

  public void setSizeAttributeValueType(String sizeAttributeValueType) {
    this.sizeAttributeValueType = sizeAttributeValueType;
  }

  public VideoUrl getVideo() {
    return video;
  }

  public void setVideo(VideoUrl video) {
    this.video = video;
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

  public void setProductStory(String productStory) {
    this.productStory = productStory;
  }

  public void setShippingWeight(double shippingWeight) {
    this.shippingWeight = shippingWeight;
  }

  public void setSortedDefiningAttributes(
      List<SortedDefiningAttributeDTO> sortedDefiningAttributes) {
    this.sortedDefiningAttributes = sortedDefiningAttributes;
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
  
  public void setBrandLogoUrl(String brandLogoUrl) {
    this.brandLogoUrl = brandLogoUrl;
  }

  public void setViewable(boolean isViewable) {
    this.isViewable = isViewable;
  }

  @Override
  public String toString() {
    return String.format(
        "MasterDataProductDTO [brand=%s, shippingWeight=%s, specificationDetail=%s, productName=%s, description=%s, longDescription=%s, uniqueSellingPoint=%s, isActivated=%s, isViewable=%s, productStory=%s, uom=%s, masterDataProductImages=%s, masterDataProductAttributes=%s, sortedDefiningAttributes=%s, url=%s, brandLogoUrl=%s, toString()=%s]",
        brand, shippingWeight, specificationDetail, productName, description, longDescription,
        uniqueSellingPoint, isActivated, isViewable, productStory, uom, masterDataProductImages,
        masterDataProductAttributes, sortedDefiningAttributes, url, brandLogoUrl, super.toString());
  }

}
