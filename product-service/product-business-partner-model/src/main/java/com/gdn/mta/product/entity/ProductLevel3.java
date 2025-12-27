package com.gdn.mta.product.entity;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.mta.product.commons.constant.DimensionHolder;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3 extends BaseResponse implements Serializable, DimensionHolder {

  private static final long serialVersionUID = -3170967969331156399L;
  private String productSku;
  private String productCode;
  private String businessPartnerCode;
  private Boolean synchronize;
  private String productName;
  private Integer productType;
  private String categoryCode;
  private String categoryName;
  private String oldCategoryName;
  private String categoryHierarchy;
  private String brand;
  private String brandCode;
  private String oldBrandName;
  private String description;
  private String specificationDetail;
  private String uniqueSellingPoint;
  private String productStory;
  private List<ProductItemLevel3> items = new ArrayList<>();

  private List<ProductItemLevel3> newlyAddedItems = new ArrayList<>();
  private List<String> deletedItems = new ArrayList<>();

  private List<ProductLevel3Attribute> attributes = new ArrayList<>();
  private List<ProductLevel3Image> images = new ArrayList<>();
  private String url;
  private Boolean installationRequired;
  private String categoryId;
  private String accessChannel;
  private boolean forceReview;
  private boolean wholesalePriceConfigEnabled;
  private ProductScore productScore;
  private Boolean productEditable;
  private boolean isSuspended;
  private List<String> pickupPointCodes;
  private boolean off2OnChannelActive;
  private PreOrderDTO preOrder;
  private boolean needCorrection;
  private boolean freeSample;
  private Boolean online;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private Integer dangerousGoodsLevel;
  private String sizeChartCode;
  private boolean sizeChartChanged;
  private Boolean b2cActivated;
  @Getter @Setter
  private VideoAddEditRequest videoAddEditRequest;
  private boolean brandUpdated;
  private boolean categoryUpdated;
  private Boolean videoUpdated;
  private boolean sellerOmg;
  private boolean distributionInfoUpdated;

  public ProductLevel3(String productSku, String productCode, String businessPartnerCode,
      Boolean synchronize, String productName, Integer productType, String categoryCode,
      String categoryName, String categoryHierarchy, String brand, String description,
      String specificationDetail, String uniqueSellingPoint, String productStory,
      List<ProductItemLevel3> items, List<ProductLevel3Attribute> attributes,
      List<ProductLevel3Image> images) {
    super();
    this.productSku = productSku;
    this.productCode = productCode;
    this.businessPartnerCode = businessPartnerCode;
    this.synchronize = synchronize;
    this.productName = productName;
    this.productType = productType;
    this.categoryCode = categoryCode;
    this.categoryName = categoryName;
    this.categoryHierarchy = categoryHierarchy;
    this.brand = brand;
    this.description = description;
    this.specificationDetail = specificationDetail;
    this.uniqueSellingPoint = uniqueSellingPoint;
    this.productStory = productStory;
    this.items = items;
    this.attributes = attributes;
    this.images = images;
  }

  public ProductLevel3(String productSku, String productCode, String businessPartnerCode,
      Boolean synchronize, String productName, Integer productType, String categoryCode,
      String categoryName, String categoryHierarchy, String brand, String description,
      String specificationDetail, String uniqueSellingPoint, String productStory,
      List<ProductItemLevel3> items, List<ProductLevel3Attribute> attributes,
      List<ProductLevel3Image> images, String url) {
    this(productSku, productCode, businessPartnerCode, synchronize, productName, productType,
        categoryCode, categoryName, categoryHierarchy, brand, description, specificationDetail,
        uniqueSellingPoint, productStory, items, attributes, images);
    this.url = url;
  }

  public ProductLevel3(String productSku, String productCode, String businessPartnerCode,
      Boolean synchronize, String productName, Integer productType, String categoryCode,
      String categoryName, String categoryHierarchy, String brand, String description,
      String specificationDetail, String uniqueSellingPoint, String productStory,
      List<ProductItemLevel3> items, List<ProductLevel3Attribute> attributes,
      List<ProductLevel3Image> images, String url, Boolean installationRequired) {
    this(productSku, productCode, businessPartnerCode, synchronize, productName, productType,
        categoryCode, categoryName, categoryHierarchy, brand, description, specificationDetail,
        uniqueSellingPoint, productStory, items, attributes, images, url);
    this.installationRequired = installationRequired;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public Boolean getSynchronize() {
    return synchronize;
  }

  public void setSynchronize(Boolean synchronize) {
    this.synchronize = synchronize;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public Integer getProductType() {
    return productType;
  }

  public void setProductType(Integer productType) {
    this.productType = productType;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }

  public String getCategoryHierarchy() {
    return categoryHierarchy;
  }

  public void setCategoryHierarchy(String categoryHierarchy) {
    this.categoryHierarchy = categoryHierarchy;
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

  public String getSpecificationDetail() {
    return specificationDetail;
  }

  public void setSpecificationDetail(String specificationDetail) {
    this.specificationDetail = specificationDetail;
  }

  public String getUniqueSellingPoint() {
    return uniqueSellingPoint;
  }

  public void setUniqueSellingPoint(String uniqueSellingPoint) {
    this.uniqueSellingPoint = uniqueSellingPoint;
  }

  public String getProductStory() {
    return productStory;
  }

  public void setProductStory(String productStory) {
    this.productStory = productStory;
  }

  public List<ProductItemLevel3> getItems() {
    return items;
  }

  public void setItems(List<ProductItemLevel3> items) {
    this.items = items;
  }

  public List<ProductLevel3Attribute> getAttributes() {
    return attributes;
  }

  public void setAttributes(List<ProductLevel3Attribute> attributes) {
    this.attributes = attributes;
  }

  public List<ProductLevel3Image> getImages() {
    return images;
  }

  public void setImages(List<ProductLevel3Image> images) {
    this.images = images;
  }

  public String getUrl() {
    return url;
  }

  public void setUrl(String url) {
    this.url = url;
  }

  public String getAccessChannel() {
    return accessChannel;
  }

  public void setAccessChannel(String accessChannel) {
    this.accessChannel = accessChannel;
  }

  public Boolean getInstallationRequired() {
    return installationRequired;
  }

  public void setInstallationRequired(Boolean installationRequired) {
    this.installationRequired = installationRequired;
  }

  public String getCategoryId() {
    return categoryId;
  }

  public void setCategoryId(String categoryId) {
    this.categoryId = categoryId;
  }

  public boolean isForceReview() {
    return forceReview;
  }

  public void setForceReview(boolean forceReview) {
    this.forceReview = forceReview;
  }

  public boolean isWholesalePriceConfigEnabled() {
    return wholesalePriceConfigEnabled;
  }

  public void setWholesalePriceConfigEnabled(boolean wholesalePriceConfigEnabled) {
    this.wholesalePriceConfigEnabled = wholesalePriceConfigEnabled;
  }

  public ProductScore getProductScore() {
    return productScore;
  }

  public void setProductScore(ProductScore productScore) {
    this.productScore = productScore;
  }

  public Boolean getProductEditable() {
    return productEditable;
  }

  public void setProductEditable(Boolean productEditable) {
    this.productEditable = productEditable;
  }

  public boolean isSuspended() {
    return isSuspended;
  }

  public void setSuspended(boolean suspended) {
    isSuspended = suspended;
  }

  public List<String> getPickupPointCodes() {
    return pickupPointCodes;
  }

  public void setPickupPointCodes(List<String> pickupPointCodes) {
    this.pickupPointCodes = pickupPointCodes;
  }

  public boolean isOff2OnChannelActive() {
    return off2OnChannelActive;
  }

  public void setOff2OnChannelActive(boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public PreOrderDTO getPreOrder() {
    return preOrder;
  }

  public void setPreOrder(PreOrderDTO preOrder) {
    this.preOrder = preOrder;
  }

  public boolean isNeedCorrection() {
    return needCorrection;
  }

  public void setNeedCorrection(boolean needCorrection) {
    this.needCorrection = needCorrection;
  }

  public boolean isFreeSample() {
    return freeSample;
  }

  public void setFreeSample(boolean freeSample) {
    this.freeSample = freeSample;
  }

  public Double getLength() {
    return length;
  }

  public void setLength(Double length) {
    this.length = length;
  }

  public Double getWidth() {
    return width;
  }

  public void setWidth(Double width) {
    this.width = width;
  }

  public Double getHeight() {
    return height;
  }

  public void setHeight(Double height) {
    this.height = height;
  }

  public Double getWeight() {
    return weight;
  }

  public void setWeight(Double weight) {
    this.weight = weight;
  }

  public Double getShippingWeight() {
    return shippingWeight;
  }

  public void setShippingWeight(Double shippingWeight) {
    this.shippingWeight = shippingWeight;
  }

  public Integer getDangerousGoodsLevel() {
    return dangerousGoodsLevel;
  }

  public void setDangerousGoodsLevel(Integer dangerousGoodsLevel) {
    this.dangerousGoodsLevel = dangerousGoodsLevel;
  }

  public List<ProductItemLevel3> getNewlyAddedItems() {
    return newlyAddedItems;
  }

  public void setNewlyAddedItems(List<ProductItemLevel3> newlyAddedItems) {
    this.newlyAddedItems = newlyAddedItems;
  }

  public List<String> getDeletedItems() {
    return deletedItems;
  }

  public void setDeletedItems(List<String> deletedItems) {
    this.deletedItems = deletedItems;
  }

  public String getSizeChartCode() {
    return sizeChartCode;
  }

  public void setSizeChartCode(String sizeChartCode) {
    this.sizeChartCode = sizeChartCode;
  }

  public boolean isSizeChartChanged() {
    return sizeChartChanged;
  }

  public void setSizeChartChanged(boolean sizeChartChanged) {
    this.sizeChartChanged = sizeChartChanged;
  }

  public Boolean getB2cActivated() {
    return b2cActivated;
  }

  @Override
  public boolean isInstore() {
    return this.off2OnChannelActive;
  }

  public void setB2cActivated(Boolean b2cActivated) {
    this.b2cActivated = b2cActivated;
  }

  public Boolean getVideoUpdated() {
    return this.videoUpdated;
  }

  public void setVideoUpdated(Boolean videoUpdated) {
    this.videoUpdated = videoUpdated;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductLevel3 [productSku=").append(productSku).append(", productCode=")
        .append(productCode).append(", businessPartnerCode=").append(businessPartnerCode)
        .append(", synchronize=").append(synchronize).append(", productName=").append(productName)
        .append(", productType=").append(productType).append(", categoryCode=")
        .append(categoryCode).append(", categoryName=").append(categoryName)
        .append(", categoryHierarchy=").append(categoryHierarchy).append(", brand=").append(brand)
        .append(", description=").append(description).append(", specificationDetail=")
        .append(specificationDetail).append(", uniqueSellingPoint=").append(uniqueSellingPoint)
        .append(", productStory=").append(productStory).append(", items=").append(items)
        .append(", attributes=").append(attributes).append(", images=").append(images)
        .append(", url=").append(url).append(", installationRequired=")
        .append(installationRequired).append(", getProductSku()=").append(getProductSku())
        .append(", getProductCode()=").append(getProductCode())
        .append(", getBusinessPartnerCode()=").append(getBusinessPartnerCode())
        .append(", getSynchronize()=").append(getSynchronize()).append(", getProductName()=")
        .append(getProductName()).append(", getProductType()=").append(getProductType())
        .append(", getCategoryCode()=").append(getCategoryCode()).append(", getCategoryName()=")
        .append(getCategoryName()).append(", getCategoryHierarchy()=")
        .append(getCategoryHierarchy()).append(", getBrand()=").append(getBrand())
        .append(", getDescription()=").append(getDescription())
        .append(", getSpecificationDetail()=").append(getSpecificationDetail())
        .append(", getUniqueSellingPoint()=").append(getUniqueSellingPoint())
        .append(", getProductStory()=").append(getProductStory()).append(", getItems()=")
        .append(getItems()).append(", getAttributes()=").append(getAttributes())
        .append(", getImages()=").append(getImages()).append(", getUrl()=").append(getUrl())
        .append(", getInstallationRequired()=").append(getInstallationRequired())
        .append(", isForceReview()=").append(isForceReview())
        .append(", getCategoryId()=").append(getCategoryId())
        .append(", getProductScore()=").append(getProductScore())
        .append(", getPreOrder()=").append(getPreOrder())
        .append(", getProductEditable()=").append(getProductEditable())
        .append(", isSuspended()=").append(isSuspended())
        .append(", getPickupPointCodes()=").append(getPickupPointCodes())
        .append(", isOff2OnChannelActive()=").append(isOff2OnChannelActive())
        .append(", needCorrection()=").append(isNeedCorrection())
        .append(", freeSample()=").append(isFreeSample())
        .append(", deletedItems()=").append(getDeletedItems())
        .append(", sizeChartCode()=").append(getSizeChartCode())
        .append(", sizeChartChanged()=").append(isSizeChartChanged())
        .append(", b2cActivated()=").append(getB2cActivated())
        .append("]");
    return builder.toString();
  }

}
