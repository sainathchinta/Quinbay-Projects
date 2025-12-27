package com.gdn.x.mta.distributiontask.domain.event.model;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PDTProductDomainEventModel extends GdnBaseDomainEventModel {
  public static class Builder {
    private String name;
    private String productCode;
    private Double length;
    private Double width;
    private Double weight;
    private Double height;
    private Double shippingWeight;
    private Integer productType;
    private byte[] description;
    private String specificationDetail;
    private String createdBy;
    private String updatedBy;
    private Date createdDate;
    private String productStory;
    private String brand;
    private String uniqueSellingPoint;
    private String migratedProductState;
    private String uom;
    private boolean activated;
    private boolean viewable;
    private String url;
    private List<PDTProductItemDomainEventModel> productItems;
    private List<ProductCategoryDomainEventModel> productCategories;
    private List<ProductAttributeDomainEventModel> productAttributes;
    private List<ImageDomainEventModel> images;
    private List<PDTImageDomainEventModel> pdtImageDomainEventModels;
    private String merchantName;
    private String merchantCode;
    private boolean forReview;
    private boolean postLive;
    private boolean reviewPending;
    private boolean marginExceeded;
    private boolean promoSKU;
    private boolean edited;
    private boolean revised;
    private PDTProductNotesDomainEventModel productNotes;
    private boolean markForDelete;
    private WorkflowState state;
    private ReviewType reviewType;

    public PDTProductDomainEventModel.Builder activated(boolean activated) {
      this.activated = activated;
      return this;
    }

    public PDTProductDomainEventModel.Builder brand(String brand) {
      this.brand = brand;
      return this;
    }

    public PDTProductDomainEventModel build() {
      return new PDTProductDomainEventModel(this);
    }

    public PDTProductDomainEventModel.Builder description(byte[] description) {
      this.description = description;
      return this;
    }

    public PDTProductDomainEventModel.Builder height(Double height) {
      this.height = height;
      return this;
    }

    public PDTProductDomainEventModel.Builder images(List<ImageDomainEventModel> images) {
      this.images = images;
      return this;
    }

    public PDTProductDomainEventModel.Builder length(Double length) {
      this.length = length;
      return this;
    }

    public PDTProductDomainEventModel.Builder merchantCode(String merchantCode) {
      this.merchantCode = merchantCode;
      return this;
    }

    public PDTProductDomainEventModel.Builder merchantName(String merchantName) {
      this.merchantName = merchantName;
      return this;
    }

    public PDTProductDomainEventModel.Builder name(String name) {
      this.name = name;
      return this;
    }

    public PDTProductDomainEventModel.Builder productType(Integer productType) {
      this.productType = productType;
      return this;
    }

    public PDTProductDomainEventModel.Builder productAttributes(
        List<ProductAttributeDomainEventModel> productAttributes) {
      this.productAttributes = productAttributes;
      return this;
    }

    public PDTProductDomainEventModel.Builder pdtImageDomainEventModels(
        List<PDTImageDomainEventModel> pdtImageDomainEventModels) {
      this.pdtImageDomainEventModels = pdtImageDomainEventModels;
      return this;
    }

    public PDTProductDomainEventModel.Builder productCategories(
        List<ProductCategoryDomainEventModel> productCategories) {
      this.productCategories = productCategories;
      return this;
    }

    public PDTProductDomainEventModel.Builder productCode(String productCode) {
      this.productCode = productCode;
      return this;
    }

    public PDTProductDomainEventModel.Builder productItems(
        List<PDTProductItemDomainEventModel> productItems) {
      this.productItems = productItems;
      return this;
    }

    public PDTProductDomainEventModel.Builder productStory(String productStory) {
      this.productStory = productStory;
      return this;
    }

    public PDTProductDomainEventModel.Builder shippingWeight(Double shippingWeight) {
      this.shippingWeight = shippingWeight;
      return this;
    }

    public PDTProductDomainEventModel.Builder specificationDetail(String specificationDetail) {
      this.specificationDetail = specificationDetail;
      return this;
    }

    public PDTProductDomainEventModel.Builder uniqueSellingPoint(String uniqueSellingPoint) {
      this.uniqueSellingPoint = uniqueSellingPoint;
      return this;
    }

    public PDTProductDomainEventModel.Builder migratedProductState(String migratedProductState) {
      this.migratedProductState = migratedProductState;
      return this;
    }

    public PDTProductDomainEventModel.Builder uom(String uom) {
      this.uom = uom;
      return this;
    }

    public PDTProductDomainEventModel.Builder url(String url) {
      this.url = url;
      return this;
    }

    public PDTProductDomainEventModel.Builder viewable(boolean viewable) {
      this.viewable = viewable;
      return this;
    }

    public PDTProductDomainEventModel.Builder weight(Double weight) {
      this.weight = weight;
      return this;
    }

    public PDTProductDomainEventModel.Builder width(Double width) {
      this.width = width;
      return this;
    }

    public PDTProductDomainEventModel.Builder createdBy(String createdBy) {
      this.createdBy = createdBy;
      return this;
    }

    public PDTProductDomainEventModel.Builder updatedBy(String updatedBy) {
      this.updatedBy = updatedBy;
      return this;
    }

    public PDTProductDomainEventModel.Builder createdDate(Date createdDate) {
      this.createdDate = createdDate;
      return this;
    }

    public PDTProductDomainEventModel.Builder forReview(boolean forReview) {
      this.forReview = forReview;
      return this;
    }

    public PDTProductDomainEventModel.Builder postLive(boolean postLive) {
      this.postLive = postLive;
      return this;
    }

    public PDTProductDomainEventModel.Builder reviewPending(boolean reviewPending) {
      this.reviewPending = reviewPending;
      return this;
    }

    public PDTProductDomainEventModel.Builder marginExceeded(boolean marginExceeded) {
      this.marginExceeded = marginExceeded;
      return this;
    }

    public PDTProductDomainEventModel.Builder promoSKU(boolean promoSKU) {
      this.promoSKU = promoSKU;
      return this;
    }

    public PDTProductDomainEventModel.Builder edited(boolean edited) {
      this.edited = edited;
      return this;
    }

    public PDTProductDomainEventModel.Builder revised(boolean revised) {
      this.revised = revised;
      return this;
    }

    public PDTProductDomainEventModel.Builder productNotes(PDTProductNotesDomainEventModel productNotes) {
      this.productNotes = productNotes;
      return this;
    }

    public PDTProductDomainEventModel.Builder state(WorkflowState state) {
      this.state = state;
      return this;
    }

    public PDTProductDomainEventModel.Builder markForDelete(boolean markForDelete) {
      this.markForDelete = markForDelete;
      return this;
    }

    public PDTProductDomainEventModel.Builder reviewType(ReviewType reviewType) {
      this.reviewType = reviewType;
      return this;
    }
  }

  private String name;
  private String productCode;
  private Double length;
  private Double width;
  private Double weight;
  private Double height;
  private Double shippingWeight;
  private byte[] description;
  private String specificationDetail;
  private String productStory;
  private String createdBy;
  private String updatedBy;
  private Date createdDate;
  private String brand;
  private String uniqueSellingPoint;
  private String migratedProductState;
  private String uom;
  private boolean activated;
  private boolean viewable;
  private String url;
  private boolean forReview;
  private boolean postLive;
  private boolean reviewPending;
  private boolean marginExceeded;
  private boolean promoSKU;
  private String brandApprovalStatus;
  private String brandCode;
  private boolean edited;
  private boolean revised;
  private PDTProductNotesDomainEventModel productNotes;
  private Integer productType;
  private boolean markForDelete;
  private WorkflowState state;
  private ReviewType reviewType;

  private List<PDTProductItemDomainEventModel> productItems;

  private List<ProductCategoryDomainEventModel> productCategories;

  private List<ProductAttributeDomainEventModel> productAttributes;

  private List<ImageDomainEventModel> images;

  private String merchantName;

  private String merchantCode;

  private List<PDTImageDomainEventModel> pdtImageDomainEventModels = new ArrayList<>();

  public PDTProductDomainEventModel() {}

  protected PDTProductDomainEventModel(PDTProductDomainEventModel.Builder builder) {
    this.productCode = builder.productCode;
    this.name = builder.name;
    this.length = builder.length;
    this.width = builder.width;
    this.weight = builder.weight;
    this.height = builder.height;
    this.createdDate = builder.createdDate;
    this.createdBy = builder.createdBy;
    this.updatedBy = builder.updatedBy;
    this.shippingWeight = builder.shippingWeight;
    this.description = builder.description;
    this.brand = builder.brand;
    this.uniqueSellingPoint = builder.uniqueSellingPoint;
    this.migratedProductState = builder.migratedProductState;
    this.uom = builder.uom;
    this.productStory = builder.productStory;
    this.url = builder.url;
    this.images = builder.images;
    this.productAttributes = builder.productAttributes;
    this.productCategories = builder.productCategories;
    this.productItems = builder.productItems;
    this.pdtImageDomainEventModels = builder.pdtImageDomainEventModels;
    this.merchantCode = builder.merchantCode;
    this.merchantName = builder.merchantName;
    this.forReview = builder.forReview;
    this.postLive = builder.postLive;
    this.reviewPending = builder.reviewPending;
    this.marginExceeded = builder.marginExceeded;
    this.promoSKU = builder.promoSKU;
    this.edited = builder.edited;
    this.revised = builder.revised;
    this.productNotes = builder.productNotes;
    this.productType = builder.productType;
    this.state = builder.state;
    this.markForDelete = builder.markForDelete;
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

  public String getMerchantCode() {
    return merchantCode;
  }

  public String getMerchantName() {
    return merchantName;
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

  public List<PDTProductItemDomainEventModel> getProductItems() {
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

  public String getMigratedProductState() {
    return migratedProductState;
  }

  public void setMigratedProductState(String migratedProductState) {
    this.migratedProductState = migratedProductState;
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

  public void setActivated(boolean activated) {
    this.activated = activated;
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

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setMerchantName(String merchantName) {
    this.merchantName = merchantName;
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

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductItems(List<PDTProductItemDomainEventModel> productItems) {
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

  public Date getCreatedDate() {
    return createdDate;
  }

  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  public String getCreatedBy() {
    return createdBy;
  }

  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  public String getUpdatedBy() {
    return updatedBy;
  }

  public void setUpdatedBy(String updatedBy) {
    this.updatedBy = updatedBy;
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

  public boolean isReviewPending() {
    return reviewPending;
  }

  public void setReviewPending(boolean reviewPending) {
    this.reviewPending = reviewPending;
  }

  public boolean isMarginExceeded() {
    return marginExceeded;
  }

  public void setMarginExceeded(boolean marginExceeded) {
    this.marginExceeded = marginExceeded;
  }

  public boolean isPromoSKU() {
    return promoSKU;
  }

  public void setPromoSKU(boolean promoSKU) {
    this.promoSKU = promoSKU;
  }

  public String getBrandApprovalStatus() {
    return brandApprovalStatus;
  }

  public void setBrandApprovalStatus(String brandApprovalStatus) {
    this.brandApprovalStatus = brandApprovalStatus;
  }

  public String getBrandCode() {
    return brandCode;
  }

  public void setBrandCode(String brandCode) {
    this.brandCode = brandCode;
  }

  public List<PDTImageDomainEventModel> getPdtImageDomainEventModels() {
    return pdtImageDomainEventModels;
  }

  public void setPdtImageDomainEventModels(List<PDTImageDomainEventModel> pdtImageDomainEventModels) {
    this.pdtImageDomainEventModels = pdtImageDomainEventModels;
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

  public PDTProductNotesDomainEventModel getProductNotes() {
    return productNotes;
  }

  public void setProductNotes(PDTProductNotesDomainEventModel productNotes) {
    this.productNotes = productNotes;
  }

  public Integer getProductType() {
    return productType;
  }

  public void setProductType(Integer productType) {
    this.productType = productType;
  }

  public boolean isMarkForDelete() { return markForDelete; }

  public void setMarkForDelete(boolean markForDelete) { this.markForDelete = markForDelete; }

  public WorkflowState getState() { return state; }

  public void setState(WorkflowState state) { this.state = state; }

  public ReviewType getReviewType() { return reviewType; }

  public void setReviewType(ReviewType reviewType) { this.reviewType = reviewType; }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("PDTProductDomainEventModel{");
    sb.append("name='").append(name).append('\'');
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", length=").append(length);
    sb.append(", width=").append(width);
    sb.append(", weight=").append(weight);
    sb.append(", height=").append(height);
    sb.append(", shippingWeight=").append(shippingWeight);
    sb.append(", description=").append(Arrays.toString(description));
    sb.append(", specificationDetail='").append(specificationDetail).append('\'');
    sb.append(", productStory='").append(productStory).append('\'');
    sb.append(", createdBy='").append(createdBy).append('\'');
    sb.append(", updatedBy='").append(updatedBy).append('\'');
    sb.append(", createdDate=").append(createdDate);
    sb.append(", brand='").append(brand).append('\'');
    sb.append(", uniqueSellingPoint='").append(uniqueSellingPoint).append('\'');
    sb.append(", migratedProductState='").append(migratedProductState).append('\'');
    sb.append(", uom='").append(uom).append('\'');
    sb.append(", activated=").append(activated);
    sb.append(", viewable=").append(viewable);
    sb.append(", url='").append(url).append('\'');
    sb.append(", productItems=").append(productItems);
    sb.append(", productCategories=").append(productCategories);
    sb.append(", productAttributes=").append(productAttributes);
    sb.append(", images=").append(images);
    sb.append(", merchantName='").append(merchantName).append('\'');
    sb.append(", merchantCode='").append(merchantCode).append('\'');
    sb.append(", forReview='").append(forReview).append('\'');
    sb.append(", postLive='").append(postLive).append('\'');
    sb.append(", reviewPending='").append(reviewPending).append('\'');
    sb.append(", marginExceeded=").append(marginExceeded).append('\'');
    sb.append(", promoSKU=").append(promoSKU).append('\'');
    sb.append(", brandApprovalStatus=").append(brandApprovalStatus).append('\'');
    sb.append(", brandCode=").append(brandCode).append('\'');
    sb.append(", edited=").append(edited).append('\'');
    sb.append(", revised=").append(revised).append('\'');
    sb.append(", ").append(productNotes).append('\'');
    sb.append(", productType=").append(productType).append('\'');
    sb.append(", state=").append(state).append('\'');
    sb.append(", markForDelete=").append(markForDelete).append('\'');
    sb.append(", reviewType=").append(reviewType).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
