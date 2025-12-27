package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.MasterDataProductImage;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.util.ProductAttributeUtil;

/**
 * Created by govind on 01/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleAsyncMasterDataProductVO implements Serializable{

  private static final long serialVersionUID = -4726787254432727637L;
  private String productName;
  private String brand;
  private String description;
  private String uniqueSellingPoint;
  private boolean isActivated;
  private boolean isViewable;
  private String brandLogoUrl;
  private double shippingWeight;
  private List<MasterDataProductImage> masterDataProductImages = new ArrayList<>();
  private List<MasterDataProductAttribute> masterDataProductAttributes =
      new ArrayList<MasterDataProductAttribute>();
  private List<ProductAttributeDetail> descriptiveAttributes = new ArrayList<>();
  private List<ItemCatalogVOV2> itemCatalogs;
  private List<SalesCatalog> salesCatalogs;
  private List<SalesCategorySequence> salesCategorySequences;
  private boolean reviewPending;
  private String masterCategoryCode;


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

  public List<MasterDataProductAttribute> getMasterDataProductAttributes() {
    return masterDataProductAttributes;
  }

  public void setMasterDataProductAttributes(
      List<MasterDataProductAttribute> masterDataProductAttributes) {
    this.masterDataProductAttributes = masterDataProductAttributes;
  }

  public List<ProductAttributeDetail> getDescriptiveAttributes() {
    return descriptiveAttributes;
  }

  public void setDescriptiveAttributes(List<ProductAttributeDetail> descriptiveAttributes) {
    this.descriptiveAttributes = descriptiveAttributes;
  }

  public List<ItemCatalogVOV2> getItemCatalogs() {
    return itemCatalogs;
  }

  public void setItemCatalogs(List<ItemCatalogVOV2> itemCatalogs) {
    this.itemCatalogs = itemCatalogs;
  }

  public List<SalesCatalog> getSalesCatalogs() {
    return salesCatalogs;
  }

  public void setSalesCatalogs(List<SalesCatalog> salesCatalogs) {
    this.salesCatalogs = salesCatalogs;
  }

  public List<SalesCategorySequence> getSalesCategorySequences() {
    return salesCategorySequences;
  }

  public void setSalesCategorySequences(List<SalesCategorySequence> salesCategorySequences) {
    this.salesCategorySequences = salesCategorySequences;
  }

  public List<MasterDataProductImage> getMasterDataProductImages() {
    return masterDataProductImages;
  }

  public void setMasterDataProductImages(List<MasterDataProductImage> masterDataProductImages) {
    this.masterDataProductImages = masterDataProductImages;
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

  @JsonIgnore
  public static SimpleAsyncMasterDataProductVO toSimpleAsyncMasterDataProductVO(Product product){

    SimpleAsyncMasterDataProductVO simpleAsyncMasterDataProductVO =
        new SimpleAsyncMasterDataProductVO();
    MasterDataProduct masterDataProduct = product.getMasterDataProduct();
    if(Objects.nonNull(masterDataProduct)) {
      simpleAsyncMasterDataProductVO.setProductName(masterDataProduct.getProductName());
      simpleAsyncMasterDataProductVO.setActivated(masterDataProduct.isActivated());
      simpleAsyncMasterDataProductVO.setDescription(masterDataProduct.getDescription());
      simpleAsyncMasterDataProductVO.setUniqueSellingPoint(masterDataProduct.getUniqueSellingPoint());
      simpleAsyncMasterDataProductVO.setViewable(masterDataProduct.isViewable());
      simpleAsyncMasterDataProductVO.setDescriptiveAttributes(ProductAttributeUtil
          .fetchDescriptiveAttributeFromMasterDataProduct(
              masterDataProduct.getMasterDataProductAttributes()));
      simpleAsyncMasterDataProductVO.setShippingWeight(masterDataProduct.getShippingWeight());
        simpleAsyncMasterDataProductVO.setSalesCatalogs(product.getSalesCatalogs());
        simpleAsyncMasterDataProductVO
            .setSalesCategorySequences(product.getSalesCategorySequences());
        simpleAsyncMasterDataProductVO.setBrand(masterDataProduct.getBrand());
        simpleAsyncMasterDataProductVO.setBrandLogoUrl(masterDataProduct.getBrandLogoUrl());
        simpleAsyncMasterDataProductVO
            .setMasterDataProductImages(masterDataProduct.getMasterDataProductImages());
        simpleAsyncMasterDataProductVO.setReviewPending(masterDataProduct.isReviewPending());
      if (Objects.nonNull(masterDataProduct.getMasterCatalog()) && Objects.nonNull(
          masterDataProduct.getMasterCatalog().getCategory())) {
        simpleAsyncMasterDataProductVO.setMasterCategoryCode(
            masterDataProduct.getMasterCatalog().getCategory().getCategoryCode());
      }
    }

    return simpleAsyncMasterDataProductVO;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productName", productName).append("brand", brand)
        .append("description", description).append("uniqueSellingPoint",uniqueSellingPoint)
        .append("isActivated", isActivated).append("isViewable", isViewable)
        .append("brandLogoUrl", brandLogoUrl).append("shippingWeight", shippingWeight)
        .append("masterDataProductAttributes", masterDataProductAttributes)
        .append("descriptiveAttributes", descriptiveAttributes).append("itemCatalogs", itemCatalogs)
        .append("salesCatalogs", salesCatalogs)
        .append("salesCategorySequences", salesCategorySequences)
        .append("masterDataProductImages", masterDataProductImages)
        .append("masterCategoryCode", masterCategoryCode).toString();
  }
}
