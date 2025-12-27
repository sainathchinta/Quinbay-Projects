package com.gdn.x.productcategorybase.entity;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import org.hibernate.annotations.BatchSize;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name = ProductItem.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(
    columnNames = {ProductItem.COLUMN_SKU_CODE})})
public class ProductItem extends GdnBaseEntity {

  private static final long serialVersionUID = 6501309786256802180L;
  public static final String TABLE_NAME = "PCC_PRODUCT_ITEM";
  public static final String COLUMN_PRODUCT_ID = "PRODUCT_ID";
  public static final String COLUMN_GENERATED_ITEM_NAME = "GENERATED_ITEM_NAME";
  public static final String COLUMN_HASH = "ITEM_HASH";
  public static final String COLUMN_UPC_CODE = "UPC_CODE";
  public static final String COLUMN_SKU_CODE = "SKU_CODE";
  public static final String COLUMN_ACTIVATED = "IS_ACTIVATED";
  public static final String COLUMN_VIEWABLE = "IS_VIEWABLE";
  public static final String COLUMN_DANGEROUS_GOODS_LEVEL = "DANGEROUS_GOODS_LEVEL";
  public static final String COLUMN_INTERNAL_UPDATE = "INTERNAL_UPDATE";
  public static final String COLUMN_CONTENT_CHANGED = "IS_CONTENT_CHANGED";
  public static final String COLUMN_SOURCE_ITEM_CODE = "SOURCE_ITEM_CODE";
  public static final String COLUMN_VAT_APPLICABLE = "VAT_APPLICABLE";
  public static final String COLUMN_NEWLY_ADDED_ITEM = "NEWLY_ADDED_ITEM";
  public static final String COLUMN_OMNI_CHANNEL_SKU = "OMNI_CHANNEL_SKU";
  public static final String COLUMN_CREATED_MERCHANT = "CREATED_MERCHANT";

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = ProductItem.COLUMN_PRODUCT_ID)
  private Product product;

  @Column(name = ProductItem.COLUMN_PRODUCT_ID, insertable = false, updatable = false)
  private String productId;

  @Column(name = ProductItem.COLUMN_GENERATED_ITEM_NAME)
  private String generatedItemName;

  @Column(name = ProductItem.COLUMN_UPC_CODE)
  private String upcCode;

  @Column(name = ProductItem.COLUMN_SKU_CODE)
  private String skuCode;

  @Column(name = ProductItem.COLUMN_ACTIVATED)
  private boolean activated = false;

  @Column(name = ProductItem.COLUMN_VIEWABLE)
  private boolean viewable = false;

  @Column(name = ProductItem.COLUMN_INTERNAL_UPDATE, nullable = false)
  private boolean internalUpdate = false;

  @Column(name = ProductItem.COLUMN_HASH)
  private byte[] hash;

  @Column(name = ProductItem.COLUMN_SOURCE_ITEM_CODE)
  private String sourceItemCode;

  @Column(name = ProductItem.COLUMN_CONTENT_CHANGED, nullable = false)
  private boolean contentChanged = false;

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "productItem", fetch = FetchType.LAZY)
  @BatchSize(size = 10)
  private List<ProductItemAttributeValue> productItemAttributeValues = new ArrayList<ProductItemAttributeValue>();

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "productItem", fetch = FetchType.LAZY)
  @BatchSize(size = 10)
  private List<ProductItemImage> productItemImages = new ArrayList<>();

  @OneToOne(cascade = CascadeType.ALL, mappedBy = "productItem", fetch = FetchType.LAZY)
  @BatchSize(size = 10)
  private ProductItemUomInfo productItemUomInfo;

  @Column(name = ProductItem.COLUMN_DANGEROUS_GOODS_LEVEL)
  private Integer dangerousGoodsLevel;

  @Column(name = ProductItem.COLUMN_VAT_APPLICABLE)
  private Boolean vatApplicable;

  @Column(name = ProductItem.COLUMN_NEWLY_ADDED_ITEM)
  private boolean newlyAddedItem = false;

  @Column(name = ProductItem.COLUMN_OMNI_CHANNEL_SKU)
  private String omniChannelSku;

  @Column(name = Product.COLUMN_CREATED_MERCHANT)
  private String createdMerchant;
  
  public ProductItem(Product product, String upcCode, String skuCode, String generatedItemName, byte[] hash,
      String storeId) {
    this.product = product;
    this.upcCode = upcCode;
    this.skuCode = skuCode;
    this.generatedItemName = generatedItemName;
    this.hash = hash;
    this.setStoreId(storeId);
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductItem [product=").append(product).append(", generatedItemName=").append(generatedItemName)
        .append(", upcCode=").append(upcCode).append(", skuCode=").append(skuCode).append(", activated=")
        .append(activated).append(", viewable=").append(viewable).append(", hash=").append(Arrays.toString(hash))
        .append(", productItemAttributeValues=").append(productItemAttributeValues).append(", productItemImages=")
        .append(productItemImages).append(", dangerousGoodsLevel=").append(dangerousGoodsLevel)
        .append(", vatApplicable=").append(vatApplicable).append(", getDangerousGoodsLevel()=")
        .append(getDangerousGoodsLevel()).append(", getGeneratedItemName()=").append(getGeneratedItemName())
        .append(", getHash()=").append(Arrays.toString(getHash())).append(", getProduct()=").append(getProduct())
        .append(", getProductItemAttributeValues()=").append(getProductItemAttributeValues())
        .append(", getProductItemImages()=").append(getProductItemImages()).append(", getSkuCode()=")
        .append(getSkuCode()).append(", getUpcCode()=").append(getUpcCode()).append(", isActivated()=")
        .append(isActivated()).append(", isViewable()=").append(isViewable()).append(", isInternalUpdate()=")
        .append(isInternalUpdate()).append(", isContentChanged()=").append(isContentChanged())
        .append(", getSourceItemCode()=").append(getSourceItemCode()).append(", getVatApplicable=")
        .append(getVatApplicable()).append(", isNewlyAddedItem()=")
        .append(isNewlyAddedItem()).append(", isMarkForDelete()=")
        .append(getOmniChannelSku()).append(", getOmniChannelSku()=")
        .append(getCreatedMerchant()).append(", getCreatedMerchant()=")
        .append(getProductItemUomInfo()).append(", getProductItemUomInfos()=")
        .append(isMarkForDelete()).append("]");
    return builder.toString();
  }
}
