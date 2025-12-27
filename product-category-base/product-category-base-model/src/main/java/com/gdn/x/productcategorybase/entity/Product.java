package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.BatchSize;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name = Product.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {Product.COLUMN_PRODUCT_CODE})})
public class Product extends GdnBaseEntity {

  private static final long serialVersionUID = -743305312879823998L;
  public static final String TABLE_NAME = "PCC_PRODUCT";
  public static final String COLUMN_NAME = "NAME";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  // public static final String COLUMN_PRODUCT_CATEGORY_ID = "PRODUCT_CATEGORY_ID";
  public static final String COLUMN_LENGTH = "LENGTH";
  public static final String COLUMN_WIDTH = "WIDTH";
  public static final String COLUMN_HEIGHT = "HEIGHT";
  public static final String COLUMN_WEIGHT = "WEIGHT";
  public static final String COLUMN_SHIPPING_WEIGHT = "SHIPPING_WEIGHT";
  public static final String COLUMN_DESCRIPTION = "DESCRIPTION";
  public static final String COLUMN_BRAND = "BRAND";
  public static final String COLUMN_UNIQUE_SELLING_POINT = "UNIQUE_SELLING_POINT";
  public static final String COLUMN_UOM = "UOM";
  public static final String COLUMN_ACTIVATED = "IS_ACTIVATED";
  public static final String COLUMN_PROMOSKU = "IS_PROMO_SKU";
  public static final String COLUMN_VIEWABLE = "IS_VIEWABLE";
  public static final String COLUMN_PRODUCT_STORY = "PRODUCT_STORY";
  public static final String COLUMN_SPECIFICATION_DETAIL = "SPECIFICATION_DETAIL";
  public static final String COLUMN_URL = "URL";
  public static final String COLUMN_FOR_REVIEW = "IS_FOR_REVIEW";
  public static final String COLUMN_REVIEW_PENDING = "REVIEW_PENDING";
  public static final String COLUMN_CREATED_MERCHANT = "CREATED_MERCHANT";
  public static final String COLUMN_EDITED = "EDITED";
  public static final String COLUMN_REVISED = "REVISED";
  public static final String COLUMN_PICKED_FOR_DELETION = "PICKED_FOR_DELETION";
  private static final String COLUMN_VIDEO = "VIDEO";
  private static final String COLUMN_DISTRIBUTION_INFO = "DISTRIBUTION_INFO";
  private static final String COLUMN_AI_GENERATED_FIELDS = "AI_GENERATED_FIELDS";

  @Column(name = Product.COLUMN_PRODUCT_CODE)
  private String productCode;

  @Column(name = Product.COLUMN_NAME)
  private String name;

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "product", fetch = FetchType.LAZY)
  private List<ProductCategory> productCategories = new ArrayList<ProductCategory>();
  
  @Column(name = Product.COLUMN_LENGTH)
  private Double length;

  @Column(name = Product.COLUMN_WIDTH)
  private Double width;

  @Column(name = Product.COLUMN_HEIGHT)
  private Double height;

  @Column(name = Product.COLUMN_WEIGHT)
  private Double weight;

  @Column(name = Product.COLUMN_SHIPPING_WEIGHT)
  private Double shippingWeight;

  @Column(name = Product.COLUMN_DESCRIPTION)
  private byte[] description;

  @Column(name = Product.COLUMN_BRAND)
  private String brand;

  @Column(name = Product.COLUMN_UNIQUE_SELLING_POINT)
  private String uniqueSellingPoint;

  @Column(name = Product.COLUMN_UOM)
  private String uom;

  @Column(name = Product.COLUMN_ACTIVATED)
  private boolean activated = false;

  @Column(name = Product.COLUMN_VIEWABLE)
  private boolean viewable = false;

  @Column(name = Product.COLUMN_PRODUCT_STORY, columnDefinition = "text")
  private String productStory;

  @Column(name = Product.COLUMN_SPECIFICATION_DETAIL, columnDefinition = "text")
  private String specificationDetail;

  @Column(name = Product.COLUMN_URL)
  private String url;

  @Column(name = Product.COLUMN_FOR_REVIEW)
  private boolean forReview = false;

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "product", fetch = FetchType.LAZY)
  @BatchSize(size = 3)
  private List<ProductAttribute> productAttributes = new ArrayList<ProductAttribute>();

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "product", fetch = FetchType.LAZY)
  @BatchSize(size = 4)
  private List<ProductImage> productImages = new ArrayList<ProductImage>();

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "product", fetch = FetchType.LAZY)
  @BatchSize(size = 4)
  private List<ProductItem> productItems = new ArrayList<ProductItem>();

  @Column(name = Product.COLUMN_PROMOSKU , columnDefinition = "boolean default false")
  private boolean promoSKU;

  @Column(name = Product.COLUMN_REVIEW_PENDING, nullable = false)
  private boolean reviewPending = false;

  @Column(name = Product.COLUMN_CREATED_MERCHANT)
  private String createdMerchant;

  @Column(name = Product.COLUMN_EDITED, nullable = false)
  private boolean edited = false;

  @Column(name = Product.COLUMN_REVISED, nullable = false)
  private boolean revised = false;

  @Column(name = Product.COLUMN_PICKED_FOR_DELETION, nullable = false)
  private boolean pickedForDeletion = false;

  @Column(name = Product.COLUMN_VIDEO)
  private String video;

  @Column(name = Product.COLUMN_DISTRIBUTION_INFO)
  private String distributionInfo;

  @Column(name = Product.COLUMN_AI_GENERATED_FIELDS)
  private String aiGeneratedFields;

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
    private String uniqueSellingPoint;
    private String uom;
    private boolean activated = false;
    private boolean viewable = false;
    private String productStory;
    private String specificationDetail;
    private String url;
    private boolean promoSKU;
    private List<ProductAttribute> productAttributes = new ArrayList<>();
    private List<ProductImage> productImages = new ArrayList<>();
    private List<ProductItem> productItems = new ArrayList<>();
    private List<ProductCategory> productCategories = new ArrayList<>();
    private String storeId;
    private boolean forReview;
    private boolean reviewPending;
    private String createdMerchant;
    private boolean edited;
    private boolean revised;
    private boolean pickedForDeletion;

    public Product.Builder productCode(String productCode) {
      this.productCode = productCode;
      return this;
    }

    public Product.Builder storeId(String storeId) {
      this.storeId = storeId;
      return this;
    }

    public Product.Builder productImages(List<ProductImage> productImages) {
      this.productImages = productImages;
      return this;
    }

    public Product.Builder productAttributes(List<ProductAttribute> productAttributes) {
      this.productAttributes = productAttributes;
      return this;
    }

    public Product.Builder productCategories(List<ProductCategory> productCategories) {
      this.productCategories = productCategories;
      return this;
    }

    public Product.Builder productItems(List<ProductItem> productItems) {
      this.productItems = productItems;
      return this;
    }

    public Product.Builder name(String name) {
      this.name = name;
      return this;
    }

    public Product.Builder length(Double length) {
      this.length = length;
      return this;
    }

    public Product.Builder width(Double width) {
      this.width = width;
      return this;
    }

    public Product.Builder weight(Double weight) {
      this.weight = weight;
      return this;
    }

    public Product.Builder height(Double height) {
      this.height = height;
      return this;
    }

    public Product.Builder shippingWeight(Double shippingWeight) {
      this.shippingWeight = shippingWeight;
      return this;
    }

    public Product.Builder description(byte[] description) {
      this.description = description;
      return this;
    }

    public Product.Builder brand(String brand) {
      this.brand = brand;
      return this;
    }

    public Product.Builder uniqueSellingPoint(String uniqueSellingPoint) {
      this.uniqueSellingPoint = uniqueSellingPoint;
      return this;
    }

    public Product.Builder activated(boolean activated) {
      this.activated = activated;
      return this;
    }

    public Product.Builder viewable(boolean viewable) {
      this.viewable = viewable;
      return this;
    }

    public Product.Builder promoSKU(boolean promoSKU) {
      this.promoSKU = promoSKU;
      return this;
    }

    public Product.Builder productStory(String productStory) {
      this.productStory = productStory;
      return this;
    }

    public Product.Builder specificationDetail(String specificationDetail) {
      this.specificationDetail = specificationDetail;
      return this;
    }

    public Product.Builder url(String url) {
      this.url = url;
      return this;
    }

    public Product.Builder uom(String uom) {
      this.uom = uom;
      return this;
    }

    public Product.Builder isForReview(boolean forReview) {
      this.forReview = forReview;
      return this;
    }

    public Product.Builder isReviewPending(boolean reviewPending) {
      this.reviewPending = reviewPending;
      return this;
    }

    public Product.Builder createdMerchant(String createdMerchant) {
      this.createdMerchant = createdMerchant;
      return this;
    }

    public Product.Builder isEdited(boolean edited) {
      this.edited = edited;
      return this;
    }

    public Product.Builder isRevised(boolean revised) {
      this.revised = revised;
      return this;
    }

    public Product.Builder pickedForDeletion(boolean pickedForDeletion) {
      this.pickedForDeletion = pickedForDeletion;
      return this;
    }


    public Product build() {
      return new Product(this);
    }
  }

  protected Product(Product.Builder builder) {
    this.productCode = builder.productCode;
    this.name = builder.name;
    this.length = builder.length;
    this.width = builder.width;
    this.weight = builder.weight;
    this.height = builder.height;
    this.shippingWeight = builder.shippingWeight;
    this.description = builder.description;
    this.brand = builder.brand;
    this.uniqueSellingPoint = builder.uniqueSellingPoint;
    this.uom = builder.uom;
    this.activated = builder.activated;
    this.viewable = builder.viewable;
    this.productStory = builder.productStory;
    this.specificationDetail = builder.specificationDetail;
    this.url = builder.url;
    this.promoSKU = builder.promoSKU;
    this.setStoreId(builder.storeId);
    this.productImages = builder.productImages;
    this.productAttributes = builder.productAttributes;
    this.productItems = builder.productItems;
    this.productCategories = builder.productCategories;
    this.forReview = builder.forReview;
    this.reviewPending = builder.reviewPending;
    this.createdMerchant = builder.createdMerchant;
    this.edited = builder.edited;
    this.revised = builder.revised;
  }

  @Override public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("Product [productCode=").append(productCode).append(", name=").append(name)
        .append(", length=").append(length).append(", width=").append(width).append(", height=")
        .append(height).append(", shippingWeight=").append(shippingWeight).append(", description=")
        .append(Arrays.toString(description)).append(", longDescription=")
        .append(", uniqueSellingPoint=").append(uniqueSellingPoint).append(", uom=").append(uom)
        .append(", activated=").append(activated).append(", viewable=").append(viewable)
        .append(", productStory=").append(productStory).append(", specificationDetail=")
        .append(specificationDetail).append(", url=").append(url)
        .append(", productAttributes=").append(productAttributes).append(", forReview=").append(forReview)
        .append(", reviewPending=").append(reviewPending)
        .append(", createdMerchant").append(createdMerchant)
        .append(", edited").append(edited)
        .append(", revised").append(revised)
        .append(", pickedForDeletion").append(pickedForDeletion)
        .append(", video").append(video)
        .append(", distributionInfo=").append(distributionInfo)
        .append(", toString()=")
        .append(super.toString());
    return builder.toString();
  }
}
