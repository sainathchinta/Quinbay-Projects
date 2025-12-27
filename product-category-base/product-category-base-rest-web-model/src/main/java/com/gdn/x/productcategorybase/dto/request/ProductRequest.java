package com.gdn.x.productcategorybase.dto.request;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse;
import lombok.Getter;
import lombok.Setter;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductRequest extends BaseDTORequest {

  private static final long serialVersionUID = -1267506909270859719L;

  private String productCode;
  private String name;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private byte[] description;
  private byte[] longDescription;
  private String brand;
  private String brandCode;
  private String brandApprovalStatus;
  private String uniqueSellingPoint;
  private String uom;
  private List<ProductCategoryRequest> productCategories = new ArrayList<>();
  private List<ProductAttributeRequest> productAttributes = new ArrayList<>();
  private List<ProductItemRequest> productItems = new ArrayList<>();
  private boolean activated = false;
  private boolean viewable = false;
  private String productStory;
  private String specificationDetail;
  private String url;
  private List<Image> images = new ArrayList<>();
  private List<Image> commonImages = new ArrayList<>();
  private boolean promoSKU;
  private String notes;
  private boolean isMarginExceed;
  private boolean forReview;
  private boolean postLive;
  private String forceReviewNotes;
  private boolean reviewPending;
  private String createdMerchant;
  private boolean ignoreMissingItems;
  private Boolean pristineCategory;
  private boolean scoreUpdated = true;
  private Integer productType;
  private boolean publishProductEvent = true;
  private int prioritySeller;

  @Setter
  @Getter
  private VideoAddEditRequest videoAddEditRequest;
  private Boolean videoUpdated;
  private List<ProductItemRequest> newlyAddedProductItems = new ArrayList<>();
  private boolean updateFromVendor;
  @Setter
  @Getter
  private Map<String, String> distributionInfoRequest;
  @Setter
  @Getter
  private AiGeneratedFieldsResponse aiGeneratedFieldsResponse;

  public ProductRequest() {}

  public static class Builder {

    private String productCode;
    private String name;
    private Double length;
    private Double width;
    private Double height;
    private Double weight;
    private Double shippingWeight;
    private byte[] description;
    private byte[] longDescription;
    private String brand;
    private String brandCode;
    private String brandApprovalStatus;
    private String uniqueSellingPoint;
    private String uom;
    private List<ProductCategoryRequest> productCategories;
    private List<ProductAttributeRequest> productAttributes;
    private List<ProductItemRequest> productItems;
    private boolean activated = false;
    private boolean viewable = false;
    private String productStory;
    private String specificationDetail;
    private String storeId;
    private String url;
    private List<Image> images;
    private boolean promoSKU;
    private String notes;
    private boolean isMarginExceed;
    private boolean forReview;
    private boolean postLive;
    private String forceReviewNotes;
    private boolean reviewPending;
    private String createdMerchant;
    private boolean publishProductEvent;
    private int prioritySeller;
    private Boolean videoUpdated;
    private AiGeneratedFieldsResponse aiGeneratedFieldsResponse;

    public ProductRequest.Builder productCategories(List<ProductCategoryRequest> productCategories) {
      this.productCategories = productCategories;
      return this;
    }

    public ProductRequest.Builder productAttributes(List<ProductAttributeRequest> productAttributes) {
      this.productAttributes = productAttributes;
      return this;
    }

    public ProductRequest.Builder productItems(List<ProductItemRequest> productItems) {
      this.productItems = productItems;
      return this;
    }

    public ProductRequest.Builder storeId(String storeId) {
      this.storeId = storeId;
      return this;
    }

    public ProductRequest.Builder productCode(String storeId) {
      this.productCode = storeId;
      return this;
    }

    public ProductRequest.Builder name(String name) {
      this.name = name;
      return this;
    }

    public ProductRequest.Builder length(Double length) {
      this.length = length;
      return this;
    }

    public ProductRequest.Builder width(Double width) {
      this.width = width;
      return this;
    }

    public ProductRequest.Builder weight(Double weight) {
      this.weight = weight;
      return this;
    }

    public ProductRequest.Builder height(Double height) {
      this.height = height;
      return this;
    }

    public ProductRequest.Builder shippingWeight(Double shippingWeight) {
      this.shippingWeight = shippingWeight;
      return this;
    }

    public ProductRequest.Builder description(byte[] description) {
      this.description = description;
      return this;
    }

    public ProductRequest.Builder longDescription(byte[] longDescription) {
      this.longDescription = longDescription;
      return this;
    }

    public ProductRequest.Builder brand(String brand) {
      this.brand = brand;
      return this;
    }

    public ProductRequest.Builder brandCode(String brandCode) {
      this.brandCode = brandCode;
      return this;
    }

    public ProductRequest.Builder brandApprovalStatus(String brandApprovalStatus) {
      this.brandApprovalStatus = brandApprovalStatus;
      return this;
    }

    public ProductRequest.Builder uniqueSellingPoint(String uniqueSellingPoint) {
      this.uniqueSellingPoint = uniqueSellingPoint;
      return this;
    }

    public ProductRequest.Builder activated(boolean activated) {
      this.activated = activated;
      return this;
    }

    public ProductRequest.Builder viewable(boolean viewable) {
      this.viewable = viewable;
      return this;
    }

    public ProductRequest.Builder promoSKU(boolean promoSKU) {
      this.promoSKU = promoSKU;
      return this;
    }

    public ProductRequest.Builder productStory(String productStory) {
      this.productStory = productStory;
      return this;
    }

    public ProductRequest.Builder specificationDetail(String specificationDetail) {
      this.specificationDetail = specificationDetail;
      return this;
    }

    public ProductRequest.Builder url(String url) {
      this.url = url;
      return this;
    }

    public ProductRequest.Builder uom(String uom) {
      this.uom = uom;
      return this;
    }

    public ProductRequest.Builder images(List<Image> images) {
      this.images = images;
      return this;
    }

    public ProductRequest.Builder notes(String notes) {
      this.notes = notes;
      return this;
    }

    public ProductRequest.Builder isMarginExceed(boolean isMarginExceed) {
      this.isMarginExceed = isMarginExceed;
      return this;
    }

    public ProductRequest.Builder isForReview(boolean isForReview) {
      this.forReview = isForReview;
      return this;
    }

    public ProductRequest.Builder forceReviewNotes(String forceReviewNotes) {
      this.forceReviewNotes = forceReviewNotes;
      return this;
    }

    public ProductRequest.Builder isPostLive(boolean isPostLive) {
      this.postLive = isPostLive;
      return this;
    }

    public ProductRequest.Builder isReviewPending(boolean isReviewPending) {
      this.reviewPending = isReviewPending;
      return this;
    }

    public ProductRequest.Builder createdMerchant(String createdMerchant) {
      this.createdMerchant = createdMerchant;
      return this;
    }

    public ProductRequest.Builder publishProductEvent(boolean publishProductEvent) {
      this.publishProductEvent = publishProductEvent;
      return this;
    }

    public ProductRequest.Builder prioritySeller(int prioritySeller){
      this.prioritySeller = prioritySeller;
      return this;
    }

    public ProductRequest.Builder videoUpdated(Boolean videoUpdated) {
      this.videoUpdated = videoUpdated;
      return this;
    }

    public ProductRequest build() {
      return new ProductRequest(this);
    }
  }

  protected ProductRequest(ProductRequest.Builder builder) {
    this.productCode = builder.productCode;
    this.name = builder.name;
    this.length = builder.length;
    this.width = builder.width;
    this.weight = builder.weight;
    this.height = builder.height;
    this.shippingWeight = builder.shippingWeight;
    this.description = builder.description;
    this.longDescription = builder.longDescription;
    this.brand = builder.brand;
    this.brandCode = builder.brandCode;
    this.brandApprovalStatus = builder.brandApprovalStatus;
    this.uniqueSellingPoint = builder.uniqueSellingPoint;
    this.uom = builder.uom;
    this.activated = builder.activated;
    this.viewable = builder.viewable;
    this.productStory = builder.productStory;
    this.specificationDetail = builder.specificationDetail;
    this.url = builder.url;
    this.images = builder.images;
    this.promoSKU = builder.promoSKU;
    this.productAttributes = builder.productAttributes;
    this.productItems = builder.productItems;
    this.productCategories = builder.productCategories;
    this.notes = builder.notes;
    this.isMarginExceed = builder.isMarginExceed;
    this.forReview = builder.forReview;
    this.postLive = builder.postLive;
    this.forceReviewNotes = builder.forceReviewNotes;
    this.reviewPending = builder.reviewPending;
    this.createdMerchant = builder.createdMerchant;
    this.prioritySeller = builder.prioritySeller;
    this.videoUpdated = builder.videoUpdated;
    setStoreId(builder.storeId);
  }

  public String getBrand() {
    return this.brand;
  }

  public String getBrandCode() {
    return brandCode;
  }

  public String getBrandApprovalStatus() {
    return brandApprovalStatus;
  }

  public byte[] getDescription() {
    return this.description;
  }

  public Double getHeight() {
    return this.height;
  }

  public List<Image> getImages() {
    if (this.images == null) {
      this.images = new ArrayList<Image>();
    }
    return this.images;
  }

  public Double getLength() {
    return this.length;
  }

  public byte[] getLongDescription() {
    return this.longDescription;
  }

  public String getName() {
    return this.name;
  }

  public List<ProductAttributeRequest> getProductAttributes() {
    return this.productAttributes;
  }

  public List<ProductCategoryRequest> getProductCategories() {
    return this.productCategories;
  }

  public String getProductCode() {
    return this.productCode;
  }

  public List<ProductItemRequest> getProductItems() {
    return this.productItems;
  }

  public String getProductStory() {
    return this.productStory;
  }

  public Double getShippingWeight() {
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

  public Double getWeight() {
    return this.weight;
  }

  public Double getWidth() {
    return this.width;
  }

  public int getPrioritySeller() {
    return prioritySeller;
  }

  public boolean isActivated() {
    return this.activated;
  }

  public boolean isViewable() {
    return this.viewable;
  }

  public void setActivated(boolean activated) {
    this.activated = activated;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setBrandCode(String brandCode) {
    this.brandCode = brandCode;
  }

  public void setBrandApprovalStatus(String brandApprovalStatus) {
    this.brandApprovalStatus = brandApprovalStatus;
  }

  public void setDescription(byte[] description) {
    this.description = description;
  }

  public void setHeight(Double height) {
    this.height = height;
  }

  public void setImages(List<Image> images) {
    this.images = images;
  }

  public void setLength(Double length) {
    this.length = length;
  }

  public void setLongDescription(byte[] longDescription) {
    this.longDescription = longDescription;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setProductAttributes(List<ProductAttributeRequest> productAttributes) {
    this.productAttributes = productAttributes;
  }

  public void setProductCategories(List<ProductCategoryRequest> productCategories) {
    this.productCategories = productCategories;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductItems(List<ProductItemRequest> productItems) {
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

  public void setPrioritySeller(int prioritySeller) {
    this.prioritySeller = prioritySeller;
  }

  public boolean isPromoSKU() {
    return promoSKU;
  }

  public void setPromoSKU(boolean promoSKU) {
    this.promoSKU = promoSKU;
  }

  public String getNotes() {
    return notes;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  public boolean isMarginExceed() {
    return isMarginExceed;
  }

  public void setMarginExceed(boolean marginExceed) {
    isMarginExceed = marginExceed;
  }

  public boolean isForReview() {
    return forReview;
  }

  public void setForReview(boolean forReview) {
    this.forReview = forReview;
  }

  public boolean isPostLive() {
    return postLive;
  }

  public void setPostLive(boolean postLive) {
    this.postLive = postLive;
  }

  public String getForceReviewNotes() {
    return forceReviewNotes;
  }

  public void setForceReviewNotes(String forceReviewNotes) {
    this.forceReviewNotes = forceReviewNotes;
  }

  public boolean isReviewPending() {
    return reviewPending;
  }

  public void setReviewPending(boolean reviewPending) {
    this.reviewPending = reviewPending;
  }

  public String getCreatedMerchant() {
    return createdMerchant;
  }

  public void setCreatedMerchant(String createdMerchant) {
    this.createdMerchant = createdMerchant;
  }

  public boolean isIgnoreMissingItems() {
    return ignoreMissingItems;
  }

  public void setIgnoreMissingItems(boolean ignoreMissingItems) {
    this.ignoreMissingItems = ignoreMissingItems;
  }

  public Boolean getPristineCategory() {
    return pristineCategory;
  }

  public void setPristineCategory(Boolean pristineCategory) {
    this.pristineCategory = pristineCategory;
  }

  public boolean isScoreUpdated() {
    return scoreUpdated;
  }

  public void setScoreUpdated(boolean scoreUpdated) {
    this.scoreUpdated = scoreUpdated;
  }

  public Integer getProductType() {
    return productType;
  }

  public void setProductType(Integer productType) {
    this.productType = productType;
  }

  public boolean isPublishProductEvent() {
    return publishProductEvent;
  }

  public void setPublishProductEvent(boolean publishProductEvent) {
    this.publishProductEvent = publishProductEvent;
  }

  public List<Image> getCommonImages() {
    return commonImages;
  }

  public void setCommonImages(List<Image> commonImages) {
    this.commonImages = commonImages;
  }

  public List<ProductItemRequest> getNewlyAddedProductItems() {
    return newlyAddedProductItems;
  }

  public void setNewlyAddedProductItems(List<ProductItemRequest> newlyAddedProductItems) {
    this.newlyAddedProductItems = newlyAddedProductItems;
  }

  public boolean isUpdateFromVendor(){
    return updateFromVendor;
  }

  public void setUpdateFromVendor(boolean updateFromVendor) {
    this.updateFromVendor = updateFromVendor;
  }

  public Boolean getVideoUpdated() {
    return videoUpdated;
  }

  public void setVideoUpdated(Boolean videoUpdated) {
    this.videoUpdated = videoUpdated;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductRequest [productCode=").append(productCode).append(", name=").append(name)
            .append(", length=").append(length).append(", width=").append(width).append(", height=").append(height)
            .append(", weight=").append(weight).append(", shippingWeight=").append(shippingWeight).append(", description=")
            .append(Arrays.toString(description)).append(", longDescription=").append(Arrays.toString(longDescription))
            .append(", brand=").append(brand).append(", brandCode=").append(brandCode).append(", brandApprovalStatus=")
            .append(brandApprovalStatus).append(", uniqueSellingPoint=").append(uniqueSellingPoint).append(", uom=")
            .append(uom).append(", productCategories=").append(productCategories.toString()).append(", productAttributes=")
            .append(productAttributes.toString()).append(", activated=").append(activated).append(", viewable=")
            .append(viewable).append(", productStory=").append(productStory).append(", specificationDetail=")
            .append(specificationDetail).append(", url=").append(url).append(", images=").append(images).append(", notes=")
            .append(notes).append(", promoSKU=").append(promoSKU).append(", isMarginExceed=").append(isMarginExceed)
            .append(", postLive=").append(postLive).append(", forReview=").append(forReview).append(", reviewPending=")
            .append(reviewPending).append(", forceReviewNotes=").append(forceReviewNotes).append(", createdMerchant")
            .append(createdMerchant).append(", ignoreMissingItems").append(ignoreMissingItems).append(", pristineCategory=")
            .append(pristineCategory).append(", scoreUpdated=").append(scoreUpdated).append(", productType=")
            .append(productType).append(", publishProductEvent=").append(publishProductEvent).append(", prioritySeller=").append(prioritySeller)
            .append(", newlyAddedProductItems=").append(newlyAddedProductItems).append(", productItems=")
            .append(productItems).append(", videoUpdated=").append(videoUpdated)
            .append(", distributionInfoRequest=").append(distributionInfoRequest).append(", toString()=")
            .append(super.toString());
    return builder.toString();
  }
}
