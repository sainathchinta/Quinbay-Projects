package com.gdn.x.mta.distributiontask.rest.model.request;

import java.util.Arrays;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.mta.distributiontask.request.VendorDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.base.DistributionBaseRequest;
import com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState;

/**
 * Created by virajjasani on 21/09/16.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class DistributionProductRequest extends DistributionBaseRequest {

  public static class Builder {
    private Date productCreatedDate;
    private String productCode;
    private String productName;
    private String videoUrl;
    private String uom;
    private String categoryName;
    private String categoryCode;
    private String brand;
    private byte[] description;
    private byte[] longDescription;
    private String uniqueSellingPoint;
    private String productStory;
    private Double length;
    private Double width;
    private Double height;
    private Double weight;
    private String businessPartnerCode;
    private String businessPartnerName;
    private Double shippingWeight;
    private WorkflowWebState state;
    private Integer rejectedCount;
    private VendorDetailRequest currentVendor;
    private String vendorId;
    private boolean contentApproved;
    private boolean imageApproved;
    private String vendorCode;
    private String storeId;
    private String productId;
    private boolean postLive;
    private boolean marginExceeded;
    private String difficultyLevel;
    private String notes;
    private boolean promoSKU;
    private String brandCode;
    private String brandApprovalStatus;
    private boolean edited;
    private Integer productType;
    private ProductNotesRequest productNotes;

    public DistributionProductRequest build() {
      return new DistributionProductRequest(this);
    }

    public Builder setBrand(String brand) {
      this.brand = brand;
      return this;
    }

    public Builder setBusinessPartnerCode(String businessPartnerCode) {
      this.businessPartnerCode = businessPartnerCode;
      return this;
    }

    public Builder setBusinessPartnerName(String businessPartnerName) {
      this.businessPartnerName = businessPartnerName;
      return this;
    }

    public Builder setCategoryCode(String categoryCode) {
      this.categoryCode = categoryCode;
      return this;
    }

    public Builder setCategoryName(String categoryName) {
      this.categoryName = categoryName;
      return this;
    }

    public Builder setContentApproved(boolean contentApproved) {
      this.contentApproved = contentApproved;
      return this;
    }

    public Builder setCurrentVendor(VendorDetailRequest currentVendor) {
      this.currentVendor = currentVendor;
      return this;
    }

    public Builder setDescription(byte[] description) {
      this.description = description;
      return this;
    }

    public Builder setHeight(Double height) {
      this.height = height;
      return this;
    }

    public Builder setImageApproved(boolean imageApproved) {
      this.imageApproved = imageApproved;
      return this;
    }

    public Builder setLength(Double length) {
      this.length = length;
      return this;
    }

    public Builder setLongDescription(byte[] longDescription) {
      this.longDescription = longDescription;
      return this;
    }

    public Builder setProductCode(String productCode) {
      this.productCode = productCode;
      return this;
    }

    public Builder setProductCreatedDate(Date productCreatedDate) {
      this.productCreatedDate = productCreatedDate;
      return this;
    }

    public Builder setProductId(String productId) {
      this.productId = productId;
      return this;
    }

    public Builder setProductName(String productName) {
      this.productName = productName;
      return this;
    }

    public Builder setProductStory(String productStory) {
      this.productStory = productStory;
      return this;
    }

    public Builder setRejectedCount(Integer rejectedCount) {
      this.rejectedCount = rejectedCount;
      return this;
    }

    public Builder setShippingWeight(Double shippingWeight) {
      this.shippingWeight = shippingWeight;
      return this;
    }

    public Builder setState(WorkflowWebState state) {
      this.state = state;
      return this;
    }

    public Builder setStoreId(String storeId) {
      this.storeId = storeId;
      return this;
    }

    public Builder setUniqueSellingPoint(String uniqueSellingPoint) {
      this.uniqueSellingPoint = uniqueSellingPoint;
      return this;
    }

    public Builder setUom(String uom) {
      this.uom = uom;
      return this;
    }

    public Builder setVendorCode(String vendorCode) {
      this.vendorCode = vendorCode;
      return this;
    }

    public Builder setVendorId(String vendorId) {
      this.vendorId = vendorId;
      return this;
    }

    public Builder setVideoUrl(String videoUrl) {
      this.videoUrl = videoUrl;
      return this;
    }

    public Builder setWeight(Double weight) {
      this.weight = weight;
      return this;
    }

    public Builder setWidth(Double width) {
      this.width = width;
      return this;
    }

    public Builder setPostLive(boolean postLive) {
      this.postLive = postLive;
      return this;
    }

    public Builder setMarginExceeded(boolean marginExceeded) {
      this.marginExceeded = marginExceeded;
      return this;
    }

    public Builder setDifficultyLevel(String difficultyLevel) {
      this.difficultyLevel = difficultyLevel;
      return this;
    }

    public Builder setNotes(String notes) {
      this.notes = notes;
      return this;
    }

    public Builder setPromoSKU(boolean promoSKU) {
      this.promoSKU = promoSKU;
      return this;
    }

    public Builder setBrandCode(String brandCode) {
      this.brandCode = brandCode;
      return this;
    }

    public Builder setBrandApprovalStatus(String brandApprovalStatus) {
      this.brandApprovalStatus = brandApprovalStatus;
      return this;
    }

    public Builder setEdited(boolean edited) {
      this.edited = edited;
      return this;
    }

    public Builder setProductType(Integer productType) {
      this.productType = productType;
      return this;
    }

    public ProductNotesRequest getProductNotes() {
      return productNotes;
    }

    public void setProductNotes(ProductNotesRequest productNotes) {
      this.productNotes = productNotes;
    }

    @Override
    public String toString() {
      final StringBuilder sb = new StringBuilder("DistributionProductRequest{");
      sb.append("productCreatedDate=").append(productCreatedDate);
      sb.append(", productCode='").append(productCode).append('\'');
      sb.append(", productName='").append(productName).append('\'');
      sb.append(", videoUrl='").append(videoUrl).append('\'');
      sb.append(", uom='").append(uom).append('\'');
      sb.append(", categoryName='").append(categoryName).append('\'');
      sb.append(", categoryCode='").append(categoryCode).append('\'');
      sb.append(", brand='").append(brand).append('\'');
      sb.append(", description=").append(Arrays.toString(description));
      sb.append(", longDescription=").append(Arrays.toString(longDescription));
      sb.append(", uniqueSellingPoint='").append(uniqueSellingPoint).append('\'');
      sb.append(", productStory='").append(productStory).append('\'');
      sb.append(", length=").append(length);
      sb.append(", width=").append(width);
      sb.append(", height=").append(height);
      sb.append(", weight=").append(weight);
      sb.append(", businessPartnerCode='").append(businessPartnerCode).append('\'');
      sb.append(", businessPartnerName='").append(businessPartnerName).append('\'');
      sb.append(", shippingWeight=").append(shippingWeight);
      sb.append(", state=").append(state);
      sb.append(", rejectedCount=").append(rejectedCount);
      sb.append(", currentVendor=").append(currentVendor);
      sb.append(", vendorId='").append(vendorId).append('\'');
      sb.append(", contentApproved=").append(contentApproved);
      sb.append(", imageApproved=").append(imageApproved);
      sb.append(", vendorCode='").append(vendorCode).append('\'');
      sb.append(", storeId='").append(storeId).append('\'');
      sb.append(", productId='").append(productId).append('\'');
      sb.append(", postLive='").append(postLive).append('\'');
      sb.append(", marginExceeded='").append(marginExceeded).append('\'');
      sb.append(", difficultyLevel='").append(difficultyLevel).append('\'');
      sb.append(", notes='").append(notes).append('\'');
      sb.append(", promoSKU='").append(promoSKU).append('\'');
      sb.append(", brandCode='").append(brandCode).append('\'');
      sb.append(", brandApprovalStatus='").append(brandApprovalStatus).append('\'');
      sb.append(", edited='").append(edited).append('\'');
      sb.append(", productNotes='").append(productNotes).append('\'');
      sb.append(", productType='").append(productType).append('\'');
      sb.append('}');
      return sb.toString();
    }
  }

  private static final long serialVersionUID = -4175902498620592387L;
  private Date productCreatedDate;
  private String productCode;
  private String productName;
  private String videoUrl;
  private String uom;
  private String categoryName;
  private String categoryCode;
  private String brand;
  private byte[] description;
  private byte[] longDescription;
  private String uniqueSellingPoint;
  private String productStory;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private String businessPartnerCode;
  private String businessPartnerName;
  private Double shippingWeight;
  private WorkflowWebState state;
  private Integer rejectedCount;
  private VendorDetailRequest currentVendor;
  private String vendorId;
  private boolean contentApproved;
  private boolean imageApproved;
  private String vendorCode;
  private String storeId;
  private String productId;
  private boolean postLive;
  private boolean marginExceeded;
  private String difficultyLevel;
  private String notes;
  private boolean promoSKU;
  private String brandCode;
  private String brandApprovalStatus;
  private boolean edited;
  private Integer productType;
  private ProductNotesRequest productNotes;

  public DistributionProductRequest() {
    // no implementation
  }

  public DistributionProductRequest(Builder builder) {
    this.productCreatedDate = builder.productCreatedDate;
    this.productCode = builder.productCode;
    this.productName = builder.productName;
    this.videoUrl = builder.videoUrl;
    this.uom = builder.uom;
    this.categoryName = builder.categoryName;
    this.categoryCode = builder.categoryCode;
    this.brand = builder.brand;
    this.description = builder.description;
    this.longDescription = builder.longDescription;
    this.uniqueSellingPoint = builder.uniqueSellingPoint;
    this.productStory = builder.productStory;
    this.length = builder.length;
    this.width = builder.width;
    this.height = builder.height;
    this.weight = builder.weight;
    this.businessPartnerCode = builder.businessPartnerCode;
    this.businessPartnerName = builder.businessPartnerName;
    this.shippingWeight = builder.shippingWeight;
    this.state = builder.state;
    this.rejectedCount = builder.rejectedCount;
    this.currentVendor = builder.currentVendor;
    this.vendorId = builder.vendorId;
    this.contentApproved = builder.contentApproved;
    this.imageApproved = builder.imageApproved;
    this.vendorCode = builder.vendorCode;
    this.storeId = builder.storeId;
    this.productId = builder.productId;
    this.postLive = builder.postLive;
    this.marginExceeded = builder.marginExceeded;
    this.difficultyLevel = builder.difficultyLevel;
    this.notes = builder.notes;
    this.promoSKU = builder.promoSKU;
    this.brandCode = builder.brandCode;
    this.brandApprovalStatus = builder.brandApprovalStatus;
    this.edited = builder.edited;
    this.productNotes = builder.productNotes;
    this.productType = builder.productType;
  }

  public String getBrand() {
    return brand;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public String getBusinessPartnerName() {
    return businessPartnerName;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public VendorDetailRequest getCurrentVendor() {
    return currentVendor;
  }

  public byte[] getDescription() {
    return description;
  }

  public Double getHeight() {
    return height;
  }

  public Double getLength() {
    return length;
  }

  public byte[] getLongDescription() {
    return longDescription;
  }

  public String getProductCode() {
    return productCode;
  }

  public Date getProductCreatedDate() {
    return productCreatedDate;
  }

  public String getProductId() {
    return productId;
  }

  public String getProductName() {
    return productName;
  }

  public String getProductStory() {
    return productStory;
  }

  public Integer getRejectedCount() {
    return rejectedCount;
  }

  public Double getShippingWeight() {
    return shippingWeight;
  }

  public WorkflowWebState getState() {
    return state;
  }

  @Override
  public String getStoreId() {
    return storeId;
  }

  public String getUniqueSellingPoint() {
    return uniqueSellingPoint;
  }

  public String getUom() {
    return uom;
  }

  public String getVendorCode() {
    return vendorCode;
  }

  public String getVendorId() {
    return vendorId;
  }

  public String getVideoUrl() {
    return videoUrl;
  }

  public Double getWeight() {
    return weight;
  }

  public Double getWidth() {
    return width;
  }

  public boolean isContentApproved() {
    return contentApproved;
  }

  public boolean isImageApproved() {
    return imageApproved;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public void setBusinessPartnerName(String businessPartnerName) {
    this.businessPartnerName = businessPartnerName;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }

  public void setContentApproved(boolean contentApproved) {
    this.contentApproved = contentApproved;
  }

  public void setCurrentVendor(VendorDetailRequest currentVendor) {
    this.currentVendor = currentVendor;
  }

  public void setDescription(byte[] description) {
    this.description = description;
  }

  public void setHeight(Double height) {
    this.height = height;
  }

  public void setImageApproved(boolean imageApproved) {
    this.imageApproved = imageApproved;
  }

  public void setLength(Double length) {
    this.length = length;
  }

  public void setLongDescription(byte[] longDescription) {
    this.longDescription = longDescription;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductCreatedDate(Date productCreatedDate) {
    this.productCreatedDate = productCreatedDate;
  }

  public void setProductId(String productId) {
    this.productId = productId;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setProductStory(String productStory) {
    this.productStory = productStory;
  }

  public void setRejectedCount(Integer rejectedCount) {
    this.rejectedCount = rejectedCount;
  }

  public void setShippingWeight(Double shippingWeight) {
    this.shippingWeight = shippingWeight;
  }

  public void setState(WorkflowWebState state) {
    this.state = state;
  }

  @Override
  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public void setUniqueSellingPoint(String uniqueSellingPoint) {
    this.uniqueSellingPoint = uniqueSellingPoint;
  }

  public void setUom(String uom) {
    this.uom = uom;
  }

  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }

  public void setVendorId(String vendorId) {
    this.vendorId = vendorId;
  }

  public void setVideoUrl(String videoUrl) {
    this.videoUrl = videoUrl;
  }

  public void setWeight(Double weight) {
    this.weight = weight;
  }

  public void setWidth(Double width) {
    this.width = width;
  }

  public boolean isPostLive() {
    return postLive;
  }

  public void setPostLive(boolean postLive) {
    this.postLive = postLive;
  }

  public boolean isMarginExceeded() {
    return marginExceeded;
  }

  public void setMarginExceeded(boolean marginExceeded) {
    this.marginExceeded = marginExceeded;
  }

  public String getDifficultyLevel() {
    return difficultyLevel;
  }

  public void setDifficultyLevel(String difficultyLevel) {
    this.difficultyLevel = difficultyLevel;
  }

  public String getNotes() {
    return notes;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  public boolean isPromoSKU() {
    return promoSKU;
  }

  public void setPromoSKU(boolean promoSKU) {
    this.promoSKU = promoSKU;
  }

  public String getBrandCode() {
    return brandCode;
  }

  public void setBrandCode(String brandCode) {
    this.brandCode = brandCode;
  }

  public String getBrandApprovalStatus() {
    return brandApprovalStatus;
  }

  public void setBrandApprovalStatus(String brandApprovalStatus) {
    this.brandApprovalStatus = brandApprovalStatus;
  }

  public boolean isEdited() {
    return edited;
  }

  public void setEdited(boolean edited) {
    this.edited = edited;
  }

  public ProductNotesRequest getProductNotes() {
    return productNotes;
  }

  public void setProductNotes(ProductNotesRequest productNotes) {
    this.productNotes = productNotes;
  }

  public Integer getProductType() {
    return productType;
  }

  public void setProductType(Integer productType) {
    this.productType = productType;
  }
}
