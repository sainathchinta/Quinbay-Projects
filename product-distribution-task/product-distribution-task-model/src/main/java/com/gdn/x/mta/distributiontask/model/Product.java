package com.gdn.x.mta.distributiontask.model;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import com.gdn.x.mta.distributiontask.model.enums.SellerBadge;
import com.gdn.x.mta.distributiontask.model.enums.SellerType;
import jakarta.persistence.Transient;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.BatchSize;

import com.gdn.x.mta.distributiontask.model.type.DifficultyLevel;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

import lombok.ToString;

@ToString
@Table(name = "PDT_PRODUCT")
@Entity
@Data
@NoArgsConstructor
public class Product extends GdnBaseEntity {
  public static class Builder {
    private String id;
    private Date productCreatedDate;
    private String productCode;
    private String productName;
    private String videoUrl;
    private String uom;
    private String categoryName;
    private String categoryCode;
    private String brand;
    private String brandCode;
    private String brandApprovalStatus;
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
    private List<ProductImage> productImages = new ArrayList<ProductImage>();
    private List<ProductItem> productItems = new ArrayList<ProductItem>();
    private List<ProductAttribute> productAttributes = new ArrayList<ProductAttribute>();
    private WorkflowState state;
    private Integer rejectedCount;
    private Vendor currentVendor;
    private String storeId;
    private boolean markForDelete;
    private int qcRetryCount;
    private boolean postLive;
    private boolean marginExceeded;
    private DifficultyLevel imageDifficultyLevel;
    private DifficultyLevel contentDifficultyLevel;
    private boolean restrictedKeywordsPresent;
    private boolean promoSKU;
    private int productPredictionScore;
    private String imageViolations;
    private String textViolations;
    private boolean edited;
    private boolean revised;
    private String productNotes;
    private ReviewType reviewType; //Allowed states CONTENT, IMAGE, CONTENT_AND_IMAGE
    private String restrictedKeywordsDetected;
    private boolean autoNeedRevision;
    private Integer productType;
    private String predictedBrand;
    private boolean forceReview;
    private int prioritySeller;
    private SellerType sellerType;
    private SellerBadge sellerBadge;
    private boolean b2cActivated;
    private boolean b2bActivated;
    private boolean pickedForDeletion;
    private boolean appealedProduct;
    private int distributionMappingStatus;
    private String aiGeneratedFields;
    private String productCreationType;

    public Product.Builder distributionMappingStatus(int distributionMappingStatus) {
      this.distributionMappingStatus = distributionMappingStatus;
      return this;
    }

    public Product.Builder aiGeneratedFields(String aiGeneratedFields) {
      this.aiGeneratedFields = aiGeneratedFields;
      return this;
    }

    public Product.Builder productCreationType(String productCreationType) {
      this.productCreationType = productCreationType;
      return this;
    }

    public Product.Builder restrictedKeywordsPresent(Boolean restrictedKeywordsPresent){
      this.restrictedKeywordsPresent = restrictedKeywordsPresent;
      return this;
    }

    public Product.Builder brand(String brand) {
      this.brand = brand;
      return this;
    }

    public Product.Builder brandCode(String brandCode) {
      this.brandCode = brandCode;
      return this;
    }

    public Product.Builder id(String id) {
      this.id = id;
      return this;
    }

    public Product build() {
      return new Product(this);
    }

    public Product.Builder businessPartnerCode(String businessPartnerCode) {
      this.businessPartnerCode = businessPartnerCode;
      return this;
    }

    public Product.Builder businessPartnerName(String businessPartnerName) {
      this.businessPartnerName = businessPartnerName;
      return this;
    }

    public Product.Builder categoryCode(String categoryCode) {
      this.categoryCode = categoryCode;
      return this;
    }

    public Product.Builder categoryName(String categoryName) {
      this.categoryName = categoryName;
      return this;
    }

    public Product.Builder currentVendor(Vendor currentVendor) {
      this.currentVendor = currentVendor;
      return this;
    }

    public Product.Builder description(byte[] description) {
      this.description = description;
      return this;
    }

    public Product.Builder height(Double height) {
      this.height = height;
      return this;
    }

    public Product.Builder length(Double length) {
      this.length = length;
      return this;
    }

    public Product.Builder longDescription(byte[] longDescription) {
      this.longDescription = longDescription;
      return this;
    }

    public Product.Builder markForDelete(boolean markForDelete) {
      this.markForDelete = markForDelete;
      return this;
    }

    public Product.Builder appealProduct(boolean appealedProduct) {
      this.appealedProduct = appealedProduct;
      return this;
    }

    public Product.Builder productAttributes(List<ProductAttribute> productAttributes) {
      this.productAttributes = productAttributes;
      return this;
    }

    public Product.Builder productCode(String productCode) {
      this.productCode = productCode;
      return this;
    }

    public Product.Builder productCreatedDate(Date productCreatedDate) {
      this.productCreatedDate = productCreatedDate;
      return this;
    }

    public Product.Builder productImages(List<ProductImage> productImages) {
      this.productImages = productImages;
      return this;
    }

    public Product.Builder productItems(List<ProductItem> productItems) {
      this.productItems = productItems;
      return this;
    }

    public Product.Builder productName(String productName) {
      this.productName = productName;
      return this;
    }

    public Product.Builder productStory(String productStory) {
      this.productStory = productStory;
      return this;
    }

    public Product.Builder rejectedCount(Integer rejectedCount) {
      this.rejectedCount = rejectedCount;
      return this;
    }

    public Product.Builder shippingWeight(Double shippingWeight) {
      this.shippingWeight = shippingWeight;
      return this;
    }

    public Product.Builder state(WorkflowState state) {
      this.state = state;
      return this;
    }

    public Product.Builder storeId(String storeId) {
      this.storeId = storeId;
      return this;
    }

    public Product.Builder uniqueSellingPoint(String uniqueSellingPoint) {
      this.uniqueSellingPoint = uniqueSellingPoint;
      return this;
    }

    public Product.Builder uom(String uom) {
      this.uom = uom;
      return this;
    }

    public Product.Builder videoUrl(String videoUrl) {
      this.videoUrl = videoUrl;
      return this;
    }

    public Product.Builder weight(Double weight) {
      this.weight = weight;
      return this;
    }

    public Product.Builder width(Double width) {
      this.width = width;
      return this;
    }

    public Product.Builder postLive(Boolean postLive) {
      this.postLive = postLive;
      return this;
    }

    public Product.Builder marginExceeded(Boolean marginExceeded) {
      this.marginExceeded = marginExceeded;
      return this;
    }

    public Product.Builder imageDifficultyLevel(DifficultyLevel difficultyLevel) {
      this.imageDifficultyLevel = difficultyLevel;
      return this;
    }
    public Product.Builder contentDifficultyLevel(DifficultyLevel difficultyLevel) {
      this.contentDifficultyLevel = difficultyLevel;
      return this;
    }

    public Product.Builder promoSKU(Boolean promoSKU) {
      this.promoSKU = promoSKU;
      return this;
    }

    public Product.Builder edited(boolean edited) {
      this.edited = edited;
      return this;
    }

    public Product.Builder revised(boolean revised) {
      this.revised = revised;
      return this;
    }

    public Product.Builder brandApprovalStatus(String brandApprovalStatus) {
      this.brandApprovalStatus = brandApprovalStatus;
      return this;
    }

    public Product.Builder reviewType(ReviewType reviewType) {
      this.reviewType = reviewType;
      return this;
    }

    public Product.Builder productNotes(String productNotes) {
      this.productNotes = productNotes;
      return this;
    }

    public Product.Builder productType(Integer productType) {
      this.productType = productType;
      return this;
    }

    public Product.Builder restrictedKeywordsDetected(String restrictedKeywordsDetected) {
      this.restrictedKeywordsDetected = restrictedKeywordsDetected;
      return this;
    }

    public Product.Builder autoNeedRevision(boolean autoNeedRevision) {
      this.autoNeedRevision = autoNeedRevision;
      return this;
    }

    public Product.Builder predictedBrand(String predictedBrand) {
      this.predictedBrand = predictedBrand;
      return this;
    }

    public Product.Builder forceReview(boolean forceReview) {
      this.forceReview = forceReview;
      return this;
    }

    public Product.Builder sellerType(SellerType sellerType) {
      this.sellerType = sellerType;
      return this;
    }

    public Product.Builder sellerBadge(SellerBadge sellerBadge) {
      this.sellerBadge = sellerBadge;
      return this;
    }

    public Product.Builder b2cActivated(boolean b2cActivated) {
      this.b2cActivated = b2cActivated;
      return this;
    }

    public Product.Builder b2bActivated(boolean b2bActivated) {
      this.b2bActivated = b2bActivated;
      return this;
    }

    public Product.Builder pickedForDeletion(boolean pickedForDeletion) {
      this.pickedForDeletion = pickedForDeletion;
      return this;
    }

  }

  public int getPrioritySeller() {
    return prioritySeller;
  }

  public void setPrioritySeller(int prioritySeller) {
    this.prioritySeller = prioritySeller;
  }

  private static final long serialVersionUID = 1L;

  @Column(name = "PRODUCT_CREATED_DATE", nullable = false)
  @Temporal(value = TemporalType.TIMESTAMP)
  private Date productCreatedDate;

  @Column(name = "PRODUCT_CODE", nullable = false, unique = true)
  private String productCode;

  @Column(name = "PRODUCT_NAME", nullable = false)
  private String productName;

  @Column(name = "VIDEO_URL")
  private String videoUrl;

  @Column(name = "UOM")
  private String uom;

  @Column(name = "CATEGORY_NAME")
  private String categoryName;

  @Column(name = "CATEGORY_CODE")
  private String categoryCode;

  @Column(name = "BRAND")
  private String brand;

  @Column(name = "BRAND_CODE")
  private String brandCode;

  @Column(name = "BRAND_APPROVAL_STATUS")
  private String brandApprovalStatus;

  @Column(name = "DESCRIPTION")
  private byte[] description;

  @Column(name = "LONG_DESCRIPTION")
  private byte[] longDescription;

  @Column(name = "UNIQUE_SELLING_POINT", length = 10000)
  private String uniqueSellingPoint;

  @Column(name = "PRODUCT_STORY")
  private String productStory;

  @Column(name = "LENGHT")
  private Double length;

  @Column(name = "WIDTH")
  private Double width;

  @Column(name = "HEIGHT")
  private Double height;

  @Column(name = "WEIGHT")
  private Double weight;

  @Column(name = "BUSINESS_PARTNER_CODE")
  private String businessPartnerCode;

  @Column(name = "BUSINESS_PARTNER_NAME")
  private String businessPartnerName;

  @Column(name = "SHIPPING_WEIGHT")
  private Double shippingWeight;

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "product", fetch = FetchType.LAZY,
      orphanRemoval = true)
  @BatchSize(size = 5)
  private List<ProductImage> productImages = new ArrayList<ProductImage>();

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "product", fetch = FetchType.LAZY,
      orphanRemoval = true)
  @BatchSize(size = 5)
  private List<ProductItem> productItems = new ArrayList<ProductItem>();

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "product", fetch = FetchType.LAZY,
      orphanRemoval = true)
  @BatchSize(size = 5)
  private List<ProductAttribute> productAttributes = new ArrayList<ProductAttribute>();

  @Column(name = "STATE")
  @Enumerated(EnumType.STRING)
  private WorkflowState state;

  @Column(name = "REJECTED_COUNT")
  private Integer rejectedCount;

  @JoinColumn(name = "CURRENT_VENDOR")
  @ManyToOne
  private Vendor currentVendor;

  @Column(name = "CURRENT_VENDOR", updatable = false, insertable = false)
  private String vendorId;

  @Column(name = "QC_RETRY_COUNT")
  private int qcRetryCount;

  @Column(name = "IS_POST_LIVE")
  private boolean postLive;

  @Column(name = "IS_MARGIN_EXCEEDED")
  private boolean marginExceeded;

  @Column(name = "RESTRICTED_KEYWORD_PRESENT")
  private boolean restrictedKeywordsPresent;

  @Column(name = "IMAGE_DIFFICULTY_LEVEL",  nullable = false)
  @Enumerated(EnumType.STRING)
  private DifficultyLevel imageDifficultyLevel = DifficultyLevel.NA;

  @Column(name = "CONTENT_DIFFICULTY_LEVEL",  nullable = false)
  @Enumerated(EnumType.STRING)
  private DifficultyLevel contentDifficultyLevel = DifficultyLevel.NA;

  @Column(name = "PRODUCT_PREDICTION_SCORE")
  private int productPredictionScore;

  @Column(name = "IMAGE_VIOLATIONS")
  private String imageViolations;

  @Column(name = "TEXT_VIOLATIONS")
  private String textViolations;

  @Column(name = "IS_PROMO_SKU")
  private boolean promoSKU;

  @Column(name = "IS_EDITED")
  private boolean edited;

  @Column(name = "REVIEW_TYPE")
  @Enumerated(EnumType.STRING)
  private ReviewType reviewType;

  @Column(name = "IS_REVISED")
  private boolean revised;

  @Column(name = "PRODUCT_NOTES")
  private String productNotes;

  @Column(name = "RESTRICTED_KEYWORDS_DETECTED")
  private String restrictedKeywordsDetected;

  @Column(name = "AUTO_NEED_REVISION")
  private boolean autoNeedRevision;

  @Column(name = "PRODUCT_TYPE")
  private Integer productType;

  @Column(name = "PREDICTED_BRAND")
  private String predictedBrand;

  @Column(name = "FORCE_REVIEW", nullable = false)
  private boolean forceReview;

  @Column(name = "PRIORITY_SELLER", nullable = false)
  private int prioritySeller;

  @Column(name = "SELLER_TYPE")
  @Enumerated(EnumType.STRING)
  private SellerType sellerType;

  @Column(name = "SELLER_BADGE")
  @Enumerated(EnumType.STRING)
  private SellerBadge sellerBadge;

  @Column(name = "B2C_ACTIVATED", nullable = false)
  private boolean b2cActivated;

  @Column(name = "B2B_ACTIVATED", nullable = false)
  private boolean b2bActivated;

  @Column(name = "PICKED_FOR_DELETION", nullable = false)
  private boolean pickedForDeletion;

  @Column(name = "APPEALED_PRODUCT")
  private boolean appealedProduct;

  @Column(name = "DISTRIBUTION_MAPPING_STATUS")
  private int distributionMappingStatus;

  @Column(name = "AI_GENERATED_FIELDS")
  private String aiGeneratedFields;

  @Column(name = "PRODUCT_CREATION_TYPE")
  private String productCreationType;

  @Transient
  private String c1CategoryCode;

  @Transient
  private String c1CategoryName;

  protected Product(Product.Builder builder) {
    this.setId(builder.id);
    this.productCode = builder.productCode;
    this.productName = builder.productName;
    this.length = builder.length;
    this.width = builder.width;
    this.weight = builder.weight;
    this.height = builder.height;
    this.shippingWeight = builder.shippingWeight;
    this.description = builder.description;
    this.longDescription = builder.longDescription;
    this.brand = builder.brand;
    this.brandCode = builder.brandCode;
    this.uniqueSellingPoint = builder.uniqueSellingPoint;
    this.uom = builder.uom;
    this.productStory = builder.productStory;
    this.videoUrl = builder.videoUrl;
    this.setStoreId(builder.storeId);
    this.setMarkForDelete(builder.markForDelete);
    this.productImages = builder.productImages;
    this.productAttributes = builder.productAttributes;
    this.productItems = builder.productItems;
    this.rejectedCount = builder.rejectedCount;
    this.businessPartnerCode = builder.businessPartnerCode;
    this.businessPartnerName = builder.businessPartnerName;
    this.state = builder.state;
    this.productCreatedDate = builder.productCreatedDate;
    this.categoryCode = builder.categoryCode;
    this.categoryName = builder.categoryName;
    this.currentVendor = builder.currentVendor;
    this.qcRetryCount = builder.qcRetryCount;
    this.postLive = builder.postLive;
    this.marginExceeded = builder.marginExceeded;
    this.imageDifficultyLevel = builder.imageDifficultyLevel;
    this.contentDifficultyLevel = builder.contentDifficultyLevel;
    this.restrictedKeywordsPresent = builder.restrictedKeywordsPresent;
    this.promoSKU = builder.promoSKU;
    this.productPredictionScore = builder.productPredictionScore;
    this.imageViolations = builder.imageViolations;
    this.textViolations = builder.textViolations;
    this.edited = builder.edited;
    this.reviewType = builder.reviewType;
    this.revised = builder.revised;
    this.productNotes = builder.productNotes;
    this.restrictedKeywordsDetected = builder.restrictedKeywordsDetected;
    this.autoNeedRevision = builder.autoNeedRevision;
    this.productType = builder.productType;
    this.predictedBrand = builder.predictedBrand;
    this.forceReview = builder.forceReview;
    this.prioritySeller = builder.prioritySeller;
    this.sellerType = builder.sellerType;
    this.sellerBadge = builder.sellerBadge;
    this.pickedForDeletion = builder.pickedForDeletion;
    this.appealedProduct = builder.appealedProduct;
    this.distributionMappingStatus = builder.distributionMappingStatus;
    this.aiGeneratedFields = builder.aiGeneratedFields;
    this.productCreationType = builder.productCreationType;
  }
}
