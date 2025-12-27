package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Created by govind on 16/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleMasterDataProductDTO implements Serializable{

  private static final long serialVersionUID = 2605628553786140418L;
  private String brand;
  private String productName;
  private String description;
  private String uniqueSellingPoint;
  private boolean isActivated;
  private boolean isViewable;
  private String brandLogoUrl;
  private double shippingWeight;
  private List<MasterDataProductImageDTO> masterDataProductImages =
      new ArrayList<MasterDataProductImageDTO>();
  private List<MasterDataProductAttributeDTO> masterDataProductAttributes =
      new ArrayList<MasterDataProductAttributeDTO>();
  private List<ProductAttributeDetailDTO> descriptiveAttributes =
      new ArrayList<ProductAttributeDetailDTO>();
  private String masterCategoryCode;
  private List<ItemCatalogDTO> itemCatalogs;
  private List<SalesCategorySequenceDTO> salesCategorySequences;
  private boolean reviewPending;
  private String url;
  private VideoDTO video;

  public String getBrand() {
    return brand;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public boolean isActivated() {
    return isActivated;
  }

  public void setActivated(boolean activated) {
    isActivated = activated;
  }

  public boolean isViewable() {
    return isViewable;
  }

  public void setViewable(boolean viewable) {
    isViewable = viewable;
  }

  public String getBrandLogoUrl() {
    return brandLogoUrl;
  }

  public void setBrandLogoUrl(String brandLogoUrl) {
    this.brandLogoUrl = brandLogoUrl;
  }

  public double getShippingWeight() {
    return shippingWeight;
  }

  public void setShippingWeight(double shippingWeight) {
    this.shippingWeight = shippingWeight;
  }

  public List<MasterDataProductImageDTO> getMasterDataProductImages() {
    return masterDataProductImages;
  }

  public void setMasterDataProductImages(List<MasterDataProductImageDTO> masterDataProductImages) {
    this.masterDataProductImages = masterDataProductImages;
  }

  public List<MasterDataProductAttributeDTO> getMasterDataProductAttributes() {
    return masterDataProductAttributes;
  }

  public void setMasterDataProductAttributes(
      List<MasterDataProductAttributeDTO> masterDataProductAttributes) {
    this.masterDataProductAttributes = masterDataProductAttributes;
  }

  public List<ProductAttributeDetailDTO> getDescriptiveAttributes() {
    return descriptiveAttributes;
  }

  public void setDescriptiveAttributes(List<ProductAttributeDetailDTO> descriptiveAttributes) {
    this.descriptiveAttributes = descriptiveAttributes;
  }

  public List<ItemCatalogDTO> getItemCatalogs() {
    return itemCatalogs;
  }

  public void setItemCatalogs(List<ItemCatalogDTO> itemCatalogs) {
    this.itemCatalogs = itemCatalogs;
  }

  public List<SalesCategorySequenceDTO> getSalesCategorySequences() {
    return salesCategorySequences;
  }

  public void setSalesCategorySequences(List<SalesCategorySequenceDTO> salesCategorySequences) {
    this.salesCategorySequences = salesCategorySequences;
  }

  public String getUniqueSellingPoint() {
    return uniqueSellingPoint;
  }

  public void setUniqueSellingPoint(String uniqueSellingPoint) {
    this.uniqueSellingPoint = uniqueSellingPoint;
  }

  public boolean isReviewPending() {
    return reviewPending;
  }

  public void setReviewPending(boolean reviewPending) {
    this.reviewPending = reviewPending;
  }

  public String getMasterCategoryCode() {
    return masterCategoryCode;
  }

  public void setMasterCategoryCode(String masterCategoryCode) {
    this.masterCategoryCode = masterCategoryCode;
  }

  public String getUrl() {
    return url;
  }

  public void setUrl(String url) {
    this.url = url;
  }

  public VideoDTO getVideo() {
    return video;
  }

  public void setVideo(VideoDTO video) {
    this.video = video;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("brand", brand).append("productName", productName)
        .append("description", description).append("uniqueSellingPoint", uniqueSellingPoint)
        .append("isActivated", isActivated).append("isViewable", isViewable).append("brandLogoUrl", brandLogoUrl)
        .append("shippingWeight", shippingWeight).append("masterDataProductImages", masterDataProductImages)
        .append("masterDataProductAttributes", masterDataProductAttributes)
        .append("descriptiveAttributes", descriptiveAttributes).append("itemCatalogs", itemCatalogs)
        .append("salesCategorySequences", salesCategorySequences).append("masterCategoryCode", masterCategoryCode)
        .append("url", url).append("video", video)
        .toString();
  }
}
