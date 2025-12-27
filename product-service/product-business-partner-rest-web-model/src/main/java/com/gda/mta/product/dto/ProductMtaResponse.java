package com.gda.mta.product.dto;

import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;

import java.util.List;

public class ProductMtaResponse extends ProductResponse {

  private static final long serialVersionUID = -7394868592614728635L;
  private Boolean isReviewedContent;
  private Boolean isReviewedImage;

  public ProductMtaResponse() {
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
    private String uniqueSellingPoint;
    private String uom;
    private boolean activated;
    private boolean viewable;
    private String productStory;
    private String specificationDetail;
    private String url;
    private List<Image> images;
    private Integer productType;
    private Boolean isReviewedContent;
    private Boolean isReviewedImage;
    private String storeId;
    private String id;

    public ProductMtaResponse.Builder productCode(String productCode) {
      this.productCode = productCode;
      return this;
    }

    public ProductMtaResponse.Builder id(String id) {
      this.id = id;
      return this;
    }

    public ProductMtaResponse.Builder storeId(String storeId) {
      this.storeId = storeId;
      return this;
    }

    public ProductMtaResponse.Builder name(String name) {
      this.name = name;
      return this;
    }

    public ProductMtaResponse.Builder length(Double length) {
      this.length = length;
      return this;
    }

    public ProductMtaResponse.Builder width(Double width) {
      this.width = width;
      return this;
    }

    public ProductMtaResponse.Builder weight(Double weight) {
      this.weight = weight;
      return this;
    }

    public ProductMtaResponse.Builder height(Double height) {
      this.height = height;
      return this;
    }

    public ProductMtaResponse.Builder shippingWeight(Double shippingWeight) {
      this.shippingWeight = shippingWeight;
      return this;
    }

    public ProductMtaResponse.Builder description(byte[] description) {
      this.description = description;
      return this;
    }

    public ProductMtaResponse.Builder longDescription(byte[] longDescription) {
      this.longDescription = longDescription;
      return this;
    }

    public ProductMtaResponse.Builder brand(String brand) {
      this.brand = brand;
      return this;
    }

    public ProductMtaResponse.Builder uniqueSellingPoint(String uniqueSellingPoint) {
      this.uniqueSellingPoint = uniqueSellingPoint;
      return this;
    }

    public ProductMtaResponse.Builder activated(boolean activated) {
      this.activated = activated;
      return this;
    }

    public ProductMtaResponse.Builder viewable(boolean viewable) {
      this.viewable = viewable;
      return this;
    }

    public ProductMtaResponse.Builder isReviewedContent(Boolean isReviewedContent) {
      this.isReviewedContent = isReviewedContent;
      return this;
    }

    public ProductMtaResponse.Builder isReviewedImage(Boolean isReviewedImage) {
      this.isReviewedImage = isReviewedImage;
      return this;
    }

    public ProductMtaResponse.Builder productStory(String productStory) {
      this.productStory = productStory;
      return this;
    }

    public ProductMtaResponse.Builder specificationDetail(String specificationDetail) {
      this.specificationDetail = specificationDetail;
      return this;
    }

    public ProductMtaResponse.Builder url(String url) {
      this.url = url;
      return this;
    }

    public ProductMtaResponse.Builder uom(String uom) {
      this.uom = uom;
      return this;
    }

    public ProductMtaResponse.Builder images(List<Image> images) {
      this.images = images;
      return this;
    }

    public ProductMtaResponse.Builder productType(Integer productType) {
      this.productType = productType;
      return this;
    }

    public ProductMtaResponse build() {
      return new ProductMtaResponse(this);
    }
  }

  private ProductMtaResponse(ProductMtaResponse.Builder builder) {
    super(new ProductResponse.Builder().productCode(builder.productCode).name(builder.name)
        .length(builder.length).width(builder.width).weight(builder.weight).height(builder.height)
        .shippingWeight(builder.shippingWeight).description(builder.description)
        .longDescription(builder.longDescription).brand(builder.brand)
        .uniqueSellingPoint(builder.uniqueSellingPoint).uom(builder.uom)
        .activated(builder.activated).viewable(builder.viewable).productStory(builder.productStory)
        .productType(builder.productType).url(builder.url).images(builder.images)
        .specificationDetail(builder.specificationDetail));
    this.isReviewedImage = builder.isReviewedImage;
    this.isReviewedContent = builder.isReviewedContent;
    setStoreId(builder.storeId);
    setId(builder.id);
  }

  public Boolean getIsReviewedContent() {
    return isReviewedContent;
  }

  public Boolean getIsReviewedImage() {
    return isReviewedImage;
  }

  public void setIsReviewedContent(Boolean isReviewedContent) {
    this.isReviewedContent = isReviewedContent;
  }

  public void setIsReviewedImage(Boolean isReviewedImage) {
    this.isReviewedImage = isReviewedImage;
  }

}
