package com.gdn.x.productcategorybase.domain.event.model;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.x.productcategorybase.ProductChangeType;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;

@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
@Data
public class ProductDomainEventModel extends GdnBaseDomainEventModel {

  private String name;
  private String productCode;
  private Double length;
  private Double width;
  private Double weight;
  private Double height;
  private Double shippingWeight;
  private boolean productMarkForDelete;
  private String id;
  private String storeId;
  private byte[] description;
  private String specificationDetail;
  private String productStory;
  private String brand;
  private String uniqueSellingPoint;
  private String uom;
  private boolean activated;
  private boolean viewable;
  private String url;
  private List<ProductItemDomainEventModel> productItems;
  private List<ProductCategoryDomainEventModel> productCategories;
  private List<ProductAttributeDomainEventModel> productAttributes;
  private List<ImageDomainEventModel> images;
  private boolean isNewProduct;
  private String brandLogoUrl;
  private ProductSalesCategoryMapping productSalesCategoryMapping;
  private Set<ProductChangeType> productChangeTypes = new HashSet<>();
  private String flowType;
  private boolean brandChanged;
  private boolean scoreUpdated;
  private Boolean pristineCategory;
  private boolean solrUpdateRequired;
  private boolean migratedProduct;
  private boolean itemUpdatePublish;
  private boolean reviewPending;
  private boolean updateMasterData = true;
  private String migrationType;
  private boolean ignoreSalesCategoryPublish;
  private Set<String> eventTypes;
  private Set<String> updatedFields = new HashSet<>();
  private boolean bopisEligible;
  private Set<String> deletedItems = new HashSet<>();
  @Setter
  @Getter
  private DistributionInfoEventModel distributionInfo;
  @Setter
  @Getter
  private String sellerCode;

  public ProductDomainEventModel() {
    // do nothing
  }

  public ProductDomainEventModel(String name, String productCode, Double length, Double width, Double weight,
      Double height, Double shippingWeight, byte[] description, String specificationDetail, String productStory,
      String brand, String uniqueSellingPoint, String uom, boolean activated, boolean viewable, String url,
      List<ProductItemDomainEventModel> productItems, List<ProductCategoryDomainEventModel> productCategories,
      List<ProductAttributeDomainEventModel> productAttributes, List<ImageDomainEventModel> images, 
      boolean productMarkForDelete, String id, String storeId) {
    super();
    this.name = name;
    this.productCode = productCode;
    this.length = length;
    this.width = width;
    this.weight = weight;
    this.height = height;
    this.shippingWeight = shippingWeight;
    this.description = description;
    this.specificationDetail = specificationDetail;
    this.productStory = productStory;
    this.brand = brand;
    this.uniqueSellingPoint = uniqueSellingPoint;
    this.uom = uom;
    this.activated = activated;
    this.viewable = viewable;
    this.url = url;
    this.productItems = productItems;
    this.productCategories = productCategories;
    this.productAttributes = productAttributes;
    this.images = images;
    this.productMarkForDelete = productMarkForDelete;
    this.id = id;
    this.storeId = storeId;
  }
  
  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public boolean isproductMarkForDelete() {
    return productMarkForDelete;
  }

  public void setproductMarkForDelete(boolean productMarkForDelete) {
    this.productMarkForDelete = productMarkForDelete;
  }

  public String getBrand() {
    return brand;
  }

  public byte[] getDescription() {
    return description;
  }

  public Double getHeight() {
    return height;
  }

  public List<ImageDomainEventModel> getImages() {
    return images;
  }

  public Double getLength() {
    return length;
  }

  public String getName() {
    return name;
  }

  public List<ProductAttributeDomainEventModel> getProductAttributes() {
    return productAttributes;
  }

  public List<ProductCategoryDomainEventModel> getProductCategories() {
    return productCategories;
  }

  public String getProductCode() {
    return productCode;
  }

  public List<ProductItemDomainEventModel> getProductItems() {
    return productItems;
  }

  public String getProductStory() {
    return productStory;
  }

  public Double getShippingWeight() {
    return shippingWeight;
  }

  public String getSpecificationDetail() {
    return specificationDetail;
  }

  public String getUniqueSellingPoint() {
    return uniqueSellingPoint;
  }

  public String getUom() {
    return uom;
  }

  public String getUrl() {
    return url;
  }

  public Double getWeight() {
    return weight;
  }

  public Double getWidth() {
    return width;
  }

  public boolean isActivated() {
    return activated;
  }

  public boolean isViewable() {
    return viewable;
  }

  public Set<String> getDeletedItems() {
    return deletedItems;
  }

  public void setDeletedItems(Set<String> deletedItems) {
    this.deletedItems = deletedItems;
  }

  public void setActivated(boolean activated) {
    this.activated = activated;
  }

  public boolean isNewProduct() {
    return isNewProduct;
  }

  public void setNewProduct(boolean newProduct) {
    isNewProduct = newProduct;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setDescription(byte[] description) {
    this.description = description;
  }

  public void setHeight(Double height) {
    this.height = height;
  }

  public void setImages(List<ImageDomainEventModel> images) {
    this.images = images;
  }

  public void setLength(Double length) {
    this.length = length;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setProductAttributes(List<ProductAttributeDomainEventModel> productAttributes) {
    this.productAttributes = productAttributes;
  }

  public void setProductCategories(List<ProductCategoryDomainEventModel> productCategories) {
    this.productCategories = productCategories;
  }

  public Set<ProductChangeType> getProductChangeTypes() {
    return productChangeTypes;
  }

  public void setProductChangeTypes(Set<ProductChangeType> productChangeTypes) {
    this.productChangeTypes = productChangeTypes;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductItems(List<ProductItemDomainEventModel> productItems) {
    this.productItems = productItems;
  }

  public void setProductStory(String productStory) {
    this.productStory = productStory;
  }

  public void setShippingWeight(Double shippingWeight) {
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

  public void setViewable(boolean viewable) {
    this.viewable = viewable;
  }

  public void setWeight(Double weight) {
    this.weight = weight;
  }

  public void setWidth(Double width) {
    this.width = width;
  }

  public String getBrandLogoUrl() {
    return brandLogoUrl;
  }

  public void setBrandLogoUrl(String brandLogoUrl) {
    this.brandLogoUrl = brandLogoUrl;
  }

  public ProductSalesCategoryMapping getProductSalesCategoryMapping() {
    return productSalesCategoryMapping;
  }

  public void setProductSalesCategoryMapping(ProductSalesCategoryMapping productSalesCategoryMapping) {
    this.productSalesCategoryMapping = productSalesCategoryMapping;
  }

  public String getFlowType() {
    return flowType;
  }

  public void setFlowType(String flowType) {
    this.flowType = flowType;
  }

  public boolean isBrandChanged() { return brandChanged; }

  public void setBrandChanged(boolean brandChanged) { this.brandChanged = brandChanged; }

  public boolean isScoreUpdated() {
    return scoreUpdated;
  }

  public void setScoreUpdated(boolean scoreUpdated) {
    this.scoreUpdated = scoreUpdated;
  }

  public boolean isSolrUpdateRequired() {
    return solrUpdateRequired;
  }

  public void setSolrUpdateRequired(boolean solrUpdateRequired) {
    this.solrUpdateRequired = solrUpdateRequired;
  }

  public Boolean getPristineCategory() {
    return pristineCategory;
  }

  public void setPristineCategory(Boolean pristineCategory) {
    this.pristineCategory = pristineCategory;
  }

  public boolean isMigratedProduct() {
    return migratedProduct;
  }

  public void setMigratedProduct(boolean migratedProduct) {
    this.migratedProduct = migratedProduct;
  }

  public boolean isItemUpdatePublish() { return itemUpdatePublish; }

  public void setItemUpdatePublish(boolean itemUpdatePublish) { this.itemUpdatePublish = itemUpdatePublish; }

  public boolean isReviewPending() {
    return reviewPending;
  }

  public void setReviewPending(boolean reviewPending) {
    this.reviewPending = reviewPending;
  }

  public boolean isUpdateMasterData() {
    return updateMasterData;
  }

  public void setUpdateMasterData(boolean updateMasterData) {
    this.updateMasterData = updateMasterData;
  }

  public String getMigrationType() {
    return migrationType;
  }

  public void setMigrationType(String migrationType) {
    this.migrationType = migrationType;
  }

  public Set<String> getEventTypes() {
    return eventTypes;
  }

  public void setEventTypes(Set<String> eventTypes) {
    this.eventTypes = eventTypes;
  }

  public boolean isIgnoreSalesCategoryPublish() {
    return ignoreSalesCategoryPublish;
  }

  public void setIgnoreSalesCategoryPublish(boolean ignoreSalesCategoryPublish) {
    this.ignoreSalesCategoryPublish = ignoreSalesCategoryPublish;
  }

  public Set<String> getUpdatedFields() {
    return updatedFields;
  }

  public void setUpdatedFields(Set<String> updatedFields) {
    this.updatedFields = updatedFields;
  }

  public boolean isBopisEligible() {
    return bopisEligible;
  }

  public void setBopisEligible(boolean bopisEligible) {
    this.bopisEligible = bopisEligible;
  }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }
}
