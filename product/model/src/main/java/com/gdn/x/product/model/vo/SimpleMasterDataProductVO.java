package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.MasterDataProductImage;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.util.ProductAttributeUtil;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 01/08/2018 AD.
 */
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleMasterDataProductVO implements Serializable{

  private static final long serialVersionUID = -4466110192910410219L;
  private String brand;
  private String productName;
  private String description;
  private String uniqueSellingPoint;
  private boolean isActivated;
  private boolean isViewable;
  private boolean archived;
  private boolean suspended;
  private boolean cncActive;
  private boolean off2OnActiveFlag;
  private boolean markForDelete;
  private String brandLogoUrl;
  private double shippingWeight;
  private String masterCategoryCode;
  private String url;
  private VideoDTO video;
  private List<MasterDataProductImage> masterDataProductImages = new ArrayList<>();
  private List<MasterDataProductAttribute> masterDataProductAttributes = new ArrayList<>();
  private List<ProductAttributeDetail> descriptiveAttributes = new ArrayList<>();
  private List<ItemCatalogVOV2> itemCatalogs;
  private List<SalesCatalog> salesCatalogs;
  private List<SalesCategorySequence> salesCategorySequences;
  private boolean reviewPending;
  private Map<String, String> categoryCodeAndCategoryNameMap;

  public SimpleMasterDataProductVO(String brand, String productName, String description, String uniqueSellingPoint,
      boolean isActivated, boolean isViewable, String brandLogoUrl, double shippingWeight, String masterCategoryCode,
      List<MasterDataProductImage> masterDataProductImages,
      List<MasterDataProductAttribute> masterDataProductAttributes, List<ProductAttributeDetail> descriptiveAttributes,
      List<ItemCatalogVOV2> itemCatalogs, List<SalesCatalog> salesCatalogs,
      List<SalesCategorySequence> salesCategorySequences, boolean reviewPending) {
    this.brand = brand;
    this.productName = productName;
    this.description = description;
    this.uniqueSellingPoint = uniqueSellingPoint;
    this.isActivated = isActivated;
    this.isViewable = isViewable;
    this.brandLogoUrl = brandLogoUrl;
    this.shippingWeight = shippingWeight;
    this.masterCategoryCode = masterCategoryCode;
    this.masterDataProductImages = masterDataProductImages;
    this.masterDataProductAttributes = masterDataProductAttributes;
    this.descriptiveAttributes = descriptiveAttributes;
    this.itemCatalogs = itemCatalogs;
    this.salesCatalogs = salesCatalogs;
    this.salesCategorySequences = salesCategorySequences;
    this.reviewPending = reviewPending;
  }

  public Map<String, String> getCategoryCodeAndCategoryNameMap() {
    return categoryCodeAndCategoryNameMap;
  }

  public void setCategoryCodeAndCategoryNameMap(Map<String, String> categoryCodeAndCategoryNameMap) {
    this.categoryCodeAndCategoryNameMap = categoryCodeAndCategoryNameMap;
  }

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

  public boolean isArchived() {
    return archived;
  }

  public void setArchived(boolean isArchived) {
    archived = isArchived;
  }

  public boolean isCncActive() {
    return cncActive;
  }

  public void setCncActive(boolean isCncActive) {
    cncActive = isCncActive;
  }

  public boolean isSuspended() {
    return suspended;
  }

  public void setSuspended(boolean isSuspended) {
    suspended = isSuspended;
  }

  public boolean isOff2OnActive() {
    return off2OnActiveFlag;
  }

  public void setOff2OnActive(boolean off2OnActive) {
    off2OnActiveFlag = off2OnActive;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean isMarkForDelete) {
    markForDelete = isMarkForDelete;
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

  public List<MasterDataProductImage> getMasterDataProductImages() {
    return masterDataProductImages;
  }

  public void setMasterDataProductImages(List<MasterDataProductImage> masterDataProductImages) {
    this.masterDataProductImages = masterDataProductImages;
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

  public boolean isReviewPending() {
    return reviewPending;
  }

  public void setReviewPending(boolean reviewPending) {
    this.reviewPending = reviewPending;
  }

  public String getUniqueSellingPoint() {
    return uniqueSellingPoint;
  }

  public void setUniqueSellingPoint(String uniqueSellingPoint) {
    this.uniqueSellingPoint = uniqueSellingPoint;
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

  @JsonIgnore
  public static SimpleMasterDataProductVO toMasterDataProductVo(MasterDataProductAndItemsVO masterDataProductAndItemsVO) {
    SimpleMasterDataProductVO simpleMasterDataProductVo = new SimpleMasterDataProductVO();
    MasterDataProduct masterDataProduct = masterDataProductAndItemsVO.getMasterDataProduct();
    simpleMasterDataProductVo.setProductName(masterDataProduct.getProductName());
    simpleMasterDataProductVo.setActivated(masterDataProduct.isActivated());
    simpleMasterDataProductVo.setBrand(masterDataProduct.getBrand());
    simpleMasterDataProductVo.setDescription(masterDataProduct.getDescription());
    simpleMasterDataProductVo.setUniqueSellingPoint(masterDataProduct.getUniqueSellingPoint());
    simpleMasterDataProductVo.setViewable(masterDataProduct.isViewable());
    simpleMasterDataProductVo.setBrandLogoUrl(masterDataProduct.getBrandLogoUrl());
    simpleMasterDataProductVo.setShippingWeight(masterDataProduct.getShippingWeight());
    simpleMasterDataProductVo.setReviewPending(masterDataProduct.isReviewPending());
    simpleMasterDataProductVo.setMasterDataProductImages(masterDataProduct.getMasterDataProductImages());
    simpleMasterDataProductVo.setDescriptiveAttributes(ProductAttributeUtil
        .fetchDescriptiveAttributeFromMasterDataProduct(masterDataProduct.getMasterDataProductAttributes()));
    simpleMasterDataProductVo.setCategoryCodeAndCategoryNameMap(
        masterDataProductAndItemsVO.getCategoryCodeAndCategoryNameMap());
    simpleMasterDataProductVo.setUrl(masterDataProduct.getUrl());
    if (Objects.nonNull(masterDataProduct.getMasterCatalog()) && Objects
        .nonNull(masterDataProduct.getMasterCatalog().getCategory())) {
      simpleMasterDataProductVo
          .setMasterCategoryCode(masterDataProduct.getMasterCatalog().getCategory().getCategoryCode());
    }
    return simpleMasterDataProductVo;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("brand", brand).append("productName", productName)
        .append("description", description).append("uniqueSellingPoint", uniqueSellingPoint)
        .append("isActivated", isActivated).append("isViewable", isViewable).append("brandLogoUrl", brandLogoUrl)
        .append("shippingWeight", shippingWeight).append("masterDataProductImages", masterDataProductImages)
        .append("masterDataProductAttributes", masterDataProductAttributes)
        .append("descriptiveAttributes", descriptiveAttributes).append("itemCatalogs", itemCatalogs)
        .append("salesCatalogs", salesCatalogs).append("salesCategorySequences", salesCategorySequences)
        .append("masterCategoryCode", masterCategoryCode)
        .append("categoryCodeAndCategoryNameMap", categoryCodeAndCategoryNameMap).toString();
  }
}
