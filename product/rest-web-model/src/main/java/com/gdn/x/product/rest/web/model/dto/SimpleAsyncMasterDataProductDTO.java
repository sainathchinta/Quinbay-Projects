package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;


/**
 * Created by govind on 01/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleAsyncMasterDataProductDTO implements Serializable{

  private static final long serialVersionUID = 2297818839485395473L;
  private String productName;
  private String brand;
  private String description;
  private String uniqueSellingPoint;
  private boolean isActivated;
  private boolean isViewable;
  private String brandLogoUrl;
  private double shippingWeight;
  private List<MasterDataProductImageDTO> masterDataProductImages =
      new ArrayList<MasterDataProductImageDTO>();
  private List<ProductAttributeDetailDTO> descriptiveAttributes = new ArrayList<ProductAttributeDetailDTO>();
  private List<ItemCatalogDTO> itemCatalogs;
  private List<SalesCategorySequenceDTO> salesCategorySequences;
  private boolean reviewPending;

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getBrand() {
    return brand;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public String getUniqueSellingPoint() {
    return uniqueSellingPoint;
  }

  public void setUniqueSellingPoint(String uniqueSellingPoint) {
    this.uniqueSellingPoint = uniqueSellingPoint;
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

  public List<MasterDataProductImageDTO> getMasterDataProductImages() {
    return masterDataProductImages;
  }

  public void setMasterDataProductImages(List<MasterDataProductImageDTO> masterDataProductImages) {
    this.masterDataProductImages = masterDataProductImages;
  }

  public boolean isReviewPending() {
    return reviewPending;
  }

  public void setReviewPending(boolean reviewPending) {
    this.reviewPending = reviewPending;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productName", productName).append("brand", brand)
        .append("description", description).append("uniqueSellingPoint",uniqueSellingPoint)
        .append("isActivated", isActivated).append("isViewable", isViewable)
        .append("brandLogoUrl", brandLogoUrl).append("shippingWeight", shippingWeight)
        .append("descriptiveAttributes", descriptiveAttributes).append("itemCatalogs", itemCatalogs)
        .append("salesCategorySequences", salesCategorySequences)
        .append("masterDataProductImages", masterDataProductImages).toString();
  }
}
