package com.gdn.mta.product.entity;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.gdn.GdnBaseEntity;

import lombok.Data;
import lombok.EqualsAndHashCode;

@Entity
@EqualsAndHashCode
@Table(name = ProductItemBusinessPartner.TABLE_NAME)
@Data
public class ProductItemBusinessPartner extends GdnBaseEntity {

  private static final long serialVersionUID = 8262411931233473393L;
  public static final String TABLE_NAME = "PRD_PRODUCT_ITEM_BUSINESS_PARTNER";
  public static final String COLUMN_PRODUCT_BUSINESS_PARTNER_ID = "PRODUCT_BUSINESS_PARTNER_ID";
  public static final String COLUMN_PRODUCT_ITEM_ID = "PRODUCT_ITEM_ID";
  public static final String COLUMN_PRODUCT_TYPE = "PRODUCT_TYPE";
  public static final String COLUMN_MERCHANT_SKU = "MERCHANT_SKU";
  public static final String COLUMN_GDN_PRODUCT_ITEM_SKU = "GDN_PRODUCT_ITEM_SKU";
  public static final String COLUMN_PRICE = "PRICE";
  public static final String COLUMN_SALE_PRICE = "SALE_PRICE";
  public static final String COLUMN_SALE_START_DATE = "SALE_START_DATE";
  public static final String COLUMN_SALE_END_DATE = "SALE_END_DATE";
  public static final String COLUMN_STOCK = "STOCK";
  public static final String COLUMN_MINIMUM_STOCK = "MINIMUM_STOCK";
  public static final String COLUMN_PICKUP_POINT_ID = "PICKUP_POINT_ID";
  public static final String COLUMN_DISPLAY = "DISPLAY";
  public static final String COLUMN_BUYABLE = "BUYABLE";
  public static final String COLUMN_INSTALLATION = "INSTALLATION";
  public static final String COLUMN_CNC_ACTIVE = "CNC_ACTIVE";
  public static final String COLUMN_FBB_ACTIVE = "FBB_ACTIVE";
  public static final String COLUMN_B2B_PRICE = "B2B_PRICE";
  public static final String COLUMN_B2B_BUYABLE = "B2B_BUYABLE";
  public static final String COLUMN_B2B_DISCOVERABLE = "B2B_DISCOVERABLE";
  public static final String COLUMN_B2B_MANAGED = "B2B_MANAGED";
  public static final String COLUMN_BUNDLE_RECIPE = "BUNDLE_RECIPE";
  public static final String COLUMN_CNC_BUYABLE = "CNC_BUYABLE";
  public static final String COLUMN_CNC_DISCOVERABLE = "CNC_DISCOVERABLE";
  public static final String COLUMN_DISTRIBUTION = "DISTRIBUTION";
  public static final String COLUMN_PREORDER_QUOTA = "PREORDER_QUOTA";

  @ManyToOne
  @JoinColumn(name = COLUMN_PRODUCT_BUSINESS_PARTNER_ID)
  private ProductBusinessPartner productBusinessPartner;

  @Column(name = COLUMN_PRODUCT_BUSINESS_PARTNER_ID, insertable = false, updatable = false)
  private String productBusinessPartnerId;

  @Column(name = COLUMN_PRODUCT_ITEM_ID, nullable = false)
  private String productItemId;

  @Column(name = COLUMN_PRODUCT_TYPE, nullable = false)
  private Integer productType;
  
  @Column(name = COLUMN_MERCHANT_SKU)
  private String merchantSku;

  @Column(name = COLUMN_GDN_PRODUCT_ITEM_SKU)
  private String gdnProductItemSku;

  @Column(name = COLUMN_PRICE, nullable = false)
  private Double price;

  @Column(name = COLUMN_SALE_PRICE, nullable = false)
  private Double salePrice;

  @Column(name = COLUMN_SALE_START_DATE)
  private Date saleStartDate;

  @Column(name = COLUMN_SALE_END_DATE)
  private Date saleEndDate;

  @Column(name = COLUMN_STOCK, nullable = false)
  private Integer stock;

  @Column(name = COLUMN_PREORDER_QUOTA)
  private Integer preOrderQuota;

  @Column(name = COLUMN_MINIMUM_STOCK, nullable = false)
  private Integer minimumStock;

  @Column(name = COLUMN_PICKUP_POINT_ID, nullable = false)
  private String pickupPointId;

  @Column(name = COLUMN_DISPLAY, nullable = false)
  private boolean display = false;

  @Column(name = COLUMN_BUYABLE, nullable = false)
  private boolean buyable = false;

  @Column(name = COLUMN_INSTALLATION, nullable = false)
  private boolean installation = false;

  @Column(name = COLUMN_CNC_ACTIVE, nullable = false)
  private boolean cncActive;

  @Column(name = COLUMN_FBB_ACTIVE, nullable = false)
  private boolean fbbActive;

  @Column(name = COLUMN_B2B_PRICE)
  private Double b2bPrice;

  @Column(name = COLUMN_B2B_BUYABLE)
  private boolean b2bBuyable;

  @Column(name = COLUMN_B2B_DISCOVERABLE)
  private boolean b2bDiscoverable;

  @Column(name = COLUMN_B2B_MANAGED)
  private boolean b2bManaged;

  @Column(name = COLUMN_BUNDLE_RECIPE)
  private String bundleRecipe;

  @Column(name = COLUMN_CNC_BUYABLE)
  private boolean cncBuyable;

  @Column(name = COLUMN_CNC_DISCOVERABLE)
  private boolean cncDiscoverable;

  @Column(name = COLUMN_DISTRIBUTION)
  private boolean distribution;

  public boolean isCncActivated() {
    return cncActive;
  }

  public void setCncActivated(boolean cncActivated) {
    this.cncActive = cncActivated;
  }

  public ProductItemBusinessPartner() {}
 
  public static class Builder {
    
    private ProductBusinessPartner productBusinessPartner;
    private String productItemId;
    private Integer productType;
    private String merchantSku;
    private String gdnProductItemSku;
    private Double price;
    private Double salePrice;
    private Date saleStartDate;
    private Date saleEndDate;
    private Integer stock;
    private Integer minimumStock;
    private String pickupPointId;
    private boolean display = false;
    private boolean buyable = false;
    private boolean installation = false;
    private String createdBy;
    private Date createdDate;
    private String storeId;
    private boolean cncActivated;
    private boolean fbbActivated;
    private Double b2bPrice;
    private boolean b2bBuyable;
    private boolean b2bDiscoverable;
    private boolean b2bManaged;
    private String bundleRecipe;
    private boolean cncBuyable;
    private boolean cncDiscoverable;
    private Integer preOrderQuota;

    public Builder productBusinessPartner(ProductBusinessPartner productBusinessPartner){
      this.productBusinessPartner = productBusinessPartner;
      return this;
    }
    
    public Builder productItemId(String productItemId){
      this.productItemId = productItemId;
      return this;
    }
    
    public Builder productType(Integer productType){
      this.productType = productType;
      return this;
    }
    
    public Builder merchantSku(String merchantSku){
      this.merchantSku = merchantSku;
      return this;
    }
    
    public Builder gdnProductItemSku(String gdnProductItemSku){
      this.gdnProductItemSku = gdnProductItemSku;
      return this;
    }
    public Builder price(Double price){
      this.price = price;
      return this;
    }
    
    public Builder salePrice(Double salePrice){
      this.salePrice = salePrice;
      return this;
    }
    
    public Builder saleStartDate(Date saleStartDate){
      this.saleStartDate = saleStartDate;
      return this;
    }
    
    public Builder saleEndDate(Date saleEndDate){
      this.saleEndDate = saleEndDate;
      return this;
    } 
    
    public Builder stock(Integer stock){
      this.stock = stock;
      return this;
    }
    
    public Builder minimumStock(Integer minimumStock){
      this.minimumStock = minimumStock;
      return this;
    }
    
    public Builder pickupPointId(String pickupPointId){
      this.pickupPointId = pickupPointId;
      return this;
    }
    
    public Builder display(boolean display){
      this.display = display;
      return this;
    }
    
    public Builder buyable(boolean buyable){
      this.buyable = buyable;
      return this;
    }
    
    public Builder installation(boolean installation){
      this.installation = installation;
      return this;
    }
    
    public Builder createdBy(String createdBy){
      this.createdBy = createdBy;
      return this;
    }
    
    public Builder createdDate(Date createdDate){
      this.createdDate = createdDate;
      return this;
    } 
    
    public Builder storeId(String storeId){
      this.storeId = storeId;
      return this;
    }

    public Builder cncActivated(boolean cncActivated){
      this.cncActivated = cncActivated;
      return this;
    }

    public Builder fbbActivated(boolean fbbActivated){
      this.fbbActivated = fbbActivated;
      return this;
    }

    public Builder b2bPrice(Double b2bPrice) {
      this.b2bPrice = b2bPrice;
      return this;
    }

    public Builder b2bBuyable(boolean b2bBuyable) {
      this.b2bBuyable = b2bBuyable;
      return this;
    }

    public Builder b2bDiscoverable(boolean b2bDiscoverable) {
      this.b2bDiscoverable = b2bDiscoverable;
      return this;
    }

    public Builder b2bManaged(boolean b2bManaged) {
      this.b2bManaged = b2bManaged;
      return this;
    }

    public Builder bundleRecipe(String bundleRecipe) {
      this.bundleRecipe = bundleRecipe;
      return this;
    }

    public Builder cncBuyable(boolean cncBuyable) {
      this.cncBuyable = cncBuyable;
      return this;
    }

    public Builder cncDiscoverable(boolean cncDiscoverable) {
      this.cncDiscoverable = cncDiscoverable;
      return this;
    }

    public Builder preOrderQuota(Integer preOrderQuota) {
      this.preOrderQuota = preOrderQuota;
      return this;
    }

    public ProductItemBusinessPartner build() {
      return new ProductItemBusinessPartner(this);
  }
    
  }
  
  private ProductItemBusinessPartner(Builder builder) {
    this.productBusinessPartner = builder.productBusinessPartner;
    this.productItemId = builder.productItemId;
    this.productType = builder.productType;
    this.merchantSku = builder.merchantSku;
    this.gdnProductItemSku = builder.gdnProductItemSku;
    this.price = builder.price;
    this.salePrice = builder.salePrice;
    this.saleStartDate = builder.saleStartDate;
    this.saleEndDate = builder.saleEndDate;
    this.stock = builder.stock;
    this.minimumStock = builder.minimumStock;
    this.pickupPointId = builder.pickupPointId;
    this.display = builder.display;
    this.buyable = builder.buyable;
    this.installation = builder.installation;
    this.cncActive = builder.cncActivated;
    this.fbbActive = builder.fbbActivated;
    this.b2bPrice = builder.b2bPrice;
    this.b2bBuyable = builder.b2bBuyable;
    this.b2bDiscoverable = builder.b2bDiscoverable;
    this.b2bManaged = builder.b2bManaged;
    this.cncBuyable = builder.cncBuyable;
    this.cncDiscoverable = builder.cncDiscoverable;
    this.preOrderQuota = builder.preOrderQuota;
    setCreatedBy(builder.createdBy);
    setCreatedDate(builder.createdDate);
    setStoreId(builder.storeId);    
}
}
