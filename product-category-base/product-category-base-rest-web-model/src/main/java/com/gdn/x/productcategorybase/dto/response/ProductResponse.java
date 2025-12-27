package com.gdn.x.productcategorybase.dto.response;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import com.gdn.x.productcategorybase.dto.Image;
import lombok.Getter;
import lombok.Setter;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductResponse extends BaseDTOResponse {

  private static final long serialVersionUID = 4269765990909644406L;

  private String productCode;

  private String name;

  private Double length;

  private Double width;

  private Double weight;

  private Double height;

  private Double shippingWeight;

  private byte[] description;

  private byte[] longDescription;

  private String brand;

  private String brandCode;

  private String brandApprovalStatus;

  private String uniqueSellingPoint;

  private String uom;

  private boolean activated;

  private boolean viewable;

  private String productStory;

  private String specificationDetail;

  private String url;

  private List<Image> images;

  private Integer productType;

  private boolean promoSKU;

  private String reviewerNotes;

  private String businessPartnerCode;

  private boolean forReview;

  private boolean reviewPending;

  private boolean postLive;

  private boolean restrictedKeywordsPresent;

  private boolean enableImageFeedback;

  private boolean edited;

  private boolean revised;

  @Getter
  @Setter
  private DistributionInfoResponse distributionInfo;

  @Getter
  @Setter
  private String sellerCode;

  public ProductResponse() {
  }

  public static class Builder {

    private String productCode;
    private String name;
    private Double length;
    private Double width;
    private Double weight;
    private Double height;
    private Double shippingWeight;
    private byte[] description;
    private byte[] longDescription;
    private String brand;
    private String brandCode;
    private String brandApprovalStatus;
    private String uniqueSellingPoint;
    private String uom;
    private boolean activated;
    private boolean viewable;
    private String productStory;
    private String specificationDetail;
    private String url;
    private List<Image> images;
    private Integer productType;
    private boolean promoSKU;
    private String reviewerNotes;
    private String businessPartnerCode;
    private boolean forReview;
    private boolean reviewPending;
    private boolean postLive;
    private boolean restrictedKeywordsPresent;
    private boolean enableImageFeedback;
    private boolean edited;
    private boolean revised;

    public Builder productCode(String productCode) {
      this.productCode = productCode;
      return this;
    }

    public Builder promoSKU(boolean promoSKU) {
      this.promoSKU = promoSKU;
      return this;
    }

    public Builder name(String name) {
      this.name = name;
      return this;
    }

    public Builder length(Double length) {
      this.length = length;
      return this;
    }

    public Builder width(Double width) {
      this.width = width;
      return this;
    }

    public Builder weight(Double weight) {
      this.weight = weight;
      return this;
    }

    public Builder height(Double height) {
      this.height = height;
      return this;
    }

    public Builder shippingWeight(Double shippingWeight) {
      this.shippingWeight = shippingWeight;
      return this;
    }

    public Builder description(byte[] description) {
      this.description = description;
      return this;
    }

    public Builder longDescription(byte[] longDescription) {
      this.longDescription = longDescription;
      return this;
    }

    public Builder brand(String brand) {
      this.brand = brand;
      return this;
    }

    public Builder brandCode(String brandCode) {
      this.brandCode = brandCode;
      return this;
    }

    public Builder brandApprovalStatus(String brandApprovalStatus) {
      this.brandApprovalStatus = brandApprovalStatus;
      return this;
    }

    public Builder uniqueSellingPoint(String uniqueSellingPoint) {
      this.uniqueSellingPoint = uniqueSellingPoint;
      return this;
    }

    public Builder activated(boolean activated) {
      this.activated = activated;
      return this;
    }

    public Builder viewable(boolean viewable) {
      this.viewable = viewable;
      return this;
    }

    public Builder productStory(String productStory) {
      this.productStory = productStory;
      return this;
    }

    public Builder specificationDetail(String specificationDetail) {
      this.specificationDetail = specificationDetail;
      return this;
    }

    public Builder url(String url) {
      this.url = url;
      return this;
    }

    public Builder uom(String uom) {
      this.uom = uom;
      return this;
    }

    public Builder images(List<Image> images) {
      this.images = images;
      return this;
    }

    public Builder productType(Integer productType) {
      this.productType = productType;
      return this;
    }

    public Builder reviewerNotes(String reviewerNotes) {
      this.reviewerNotes = reviewerNotes;
      return this;
    }

    public Builder forReview(boolean forReview) {
      this.forReview = forReview;
      return this;
    }

    public Builder reviewPending(boolean reviewPending) {
      this.reviewPending = reviewPending;
      return this;
    }

    public Builder postLive(boolean postLive) {
      this.postLive = postLive;
      return this;
    }

    public Builder restrictedKeywordsPresent(boolean restrictedKeywordsPresent) {
      this.restrictedKeywordsPresent = restrictedKeywordsPresent;
      return this;
    }

    public Builder isEdited(boolean edited) {
      this.edited = edited;
      return this;
    }

    public Builder isRevised(boolean revised) {
      this.revised = revised;
      return this;
    }

    public ProductResponse build() {
      return new ProductResponse(this);
    }
  }

  protected ProductResponse(Builder builder) {
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
    this.productType = builder.productType;
    this.promoSKU = builder.promoSKU;
    this.reviewerNotes = builder.reviewerNotes;
    this.businessPartnerCode = builder.businessPartnerCode;
    this.forReview = builder.forReview;
    this.reviewPending = builder.reviewPending;
    this.postLive = builder.postLive;
    this.restrictedKeywordsPresent = builder.restrictedKeywordsPresent;
    this.enableImageFeedback = builder.enableImageFeedback;
    this.edited = builder.edited;
    this.revised=builder.revised;
  }

  public String getReviewerNotes() {
    return reviewerNotes;
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

  public boolean isPromoSKU() {
    return promoSKU;
  }

  public void setPromoSKU(boolean promoSKU) {
    this.promoSKU = promoSKU;
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

  public String getProductCode() {
    return this.productCode;
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

  public void setProductCode(String productCode) {
    this.productCode = productCode;
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

  public Integer getProductType() {
    return productType;
  }

  public void setReviewerNotes(String reviewerNotes) {
    this.reviewerNotes = reviewerNotes;
  }

  public void setProductType(Integer productType) {
    this.productType = productType;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public boolean isForReview() {
    return forReview;
  }

  public void setForReview(boolean forReview) {
    this.forReview = forReview;
  }

  public boolean isReviewPending() {
    return reviewPending;
  }

  public void setReviewPending(boolean reviewPending) {
    this.reviewPending = reviewPending;
  }

  public boolean isPostLive() {
    return postLive;
  }

  public void setPostLive(boolean postLive) {
    this.postLive = postLive;
  }

  public boolean isRestrictedKeywordsPresent() {
    return restrictedKeywordsPresent;
  }

  public void setRestrictedKeywordsPresent(boolean restrictedKeywordsPresent) {
    this.restrictedKeywordsPresent = restrictedKeywordsPresent;
  }

  public boolean isEnableImageFeedback() {
    return enableImageFeedback;
  }

  public void setEnableImageFeedback(boolean enableImageFeedback) {
    this.enableImageFeedback = enableImageFeedback;
  }

  public boolean isEdited() {
    return edited;
  }

  public void setEdited(boolean edited) {
    this.edited = edited;
  }

  public boolean isRevised() {
    return revised;
  }

  public void setRevised(boolean revised) {
    this.revised = revised;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductResponse{");
    sb.append("productCode='").append(productCode).append('\'');
    sb.append(", name='").append(name).append('\'');
    sb.append(", length=").append(length);
    sb.append(", width=").append(width);
    sb.append(", weight=").append(weight);
    sb.append(", height=").append(height);
    sb.append(", shippingWeight=").append(shippingWeight);
    sb.append(", description=").append(Arrays.toString(description));
    sb.append(", longDescription=").append(Arrays.toString(longDescription));
    sb.append(", brand='").append(brand).append('\'');
    sb.append(", brandCode='").append(brandCode).append('\'');
    sb.append(", brandApprovalstatus='").append(brandApprovalStatus).append('\'');
    sb.append(", uniqueSellingPoint='").append(uniqueSellingPoint).append('\'');
    sb.append(", uom='").append(uom).append('\'');
    sb.append(", activated=").append(activated);
    sb.append(", viewable=").append(viewable);
    sb.append(", productStory='").append(productStory).append('\'');
    sb.append(", specificationDetail='").append(specificationDetail).append('\'');
    sb.append(", url='").append(url).append('\'');
    sb.append(", images=").append(images);
    sb.append(", productType=").append(productType);
    sb.append(", promoSKU=").append(promoSKU);
    sb.append(", reviewerNotes=").append(reviewerNotes);
    sb.append(", businessPartnerCode").append(businessPartnerCode);
    sb.append(", forReview").append(forReview);
    sb.append(", reviewPending").append(reviewPending);
    sb.append(", postLive").append(postLive);
    sb.append(", restrictedKeywordsPresent").append(restrictedKeywordsPresent);
    sb.append(", enableImageFeedback").append(enableImageFeedback);
    sb.append(", edited").append(edited);
    sb.append(", revised").append(revised);
    sb.append('}');
    return sb.toString();
  }
}
