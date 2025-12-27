package com.gdn.partners.pbp.entity.productlevel3;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import com.gdn.GdnBaseEntity;

@Entity
@Table(name = ProductLevel3ItemWip.TABLE_NAME)
public class ProductLevel3ItemWip extends GdnBaseEntity {

  private static final long serialVersionUID = -7202115119144489338L;
  public static final String TABLE_NAME = "PRD_PRODUCT_ITEM_BUSINESS_PARTNER";
  public static final String COLUMN_PRODUCT_LEVEL3_WIP_ID = "PRODUCT_BUSINESS_PARTNER_ID";
  public static final String COLUMN_PRODUCT_LEVEL1_ITEM_ID = "PRODUCT_ITEM_ID";
  public static final String COLUMN_GDN_SKU = "GDN_PRODUCT_ITEM_SKU";
  public static final String COLUMN_MERCHANT_SKU = "MERCHANT_SKU";
  public static final String COLUMN_PICKUP_POINT_CODE = "PICKUP_POINT_ID";
  public static final String COLUMN_PRODUCT_TYPE = "PRODUCT_TYPE";
  public static final String COLUMN_REGULAR_PRICE = "PRICE";
  public static final String COLUMN_SELLING_PRICE = "SALE_PRICE";
  public static final String COLUMN_STOCK = "STOCK";
  public static final String COLUMN_MINIMUM_STOCK = "MINIMUM_STOCK";
  public static final String COLUMN_DISPLAYABLE = "DISPLAY";
  public static final String COLUMN_BUYABLE = "BUYABLE";
  public static final String COLUMN_NEED_INSTALLATION = "INSTALLATION";

  @ManyToOne
  @JoinColumn(name = ProductLevel3ItemWip.COLUMN_PRODUCT_LEVEL3_WIP_ID)
  private ProductLevel3Wip productLevel3Wip;

  @Column(name = ProductLevel3ItemWip.COLUMN_PRODUCT_LEVEL1_ITEM_ID, nullable = false)
  private String productLevel1ItemId;

  @Column(name = ProductLevel3ItemWip.COLUMN_GDN_SKU)
  private String gdnSku;

  @Column(name = ProductLevel3ItemWip.COLUMN_MERCHANT_SKU)
  private String merchantSku;

  @Column(name = ProductLevel3ItemWip.COLUMN_PICKUP_POINT_CODE, nullable = false)
  private String pickupPointCode;

  @Column(name = ProductLevel3ItemWip.COLUMN_PRODUCT_TYPE, nullable = false)
  private Integer productType;

  @Column(name = ProductLevel3ItemWip.COLUMN_REGULAR_PRICE, nullable = false)
  private Double regularPrice;

  @Column(name = ProductLevel3ItemWip.COLUMN_SELLING_PRICE, nullable = false)
  private Double sellingPrice;

  @Column(name = ProductLevel3ItemWip.COLUMN_STOCK, nullable = false)
  private Integer stock;

  @Column(name = ProductLevel3ItemWip.COLUMN_MINIMUM_STOCK, nullable = false)
  private Integer minimumStock;

  @Column(name = ProductLevel3ItemWip.COLUMN_DISPLAYABLE, nullable = false)
  private boolean displayable = false;

  @Column(name = ProductLevel3ItemWip.COLUMN_BUYABLE, nullable = false)
  private boolean buyable = false;

  @Column(name = ProductLevel3ItemWip.COLUMN_NEED_INSTALLATION, nullable = false)
  private boolean needInstallation = false;

  public ProductLevel3ItemWip() {}

  public ProductLevel3ItemWip(ProductLevel3Wip productLevel3Wip, String productLevel1ItemId, String gdnSku,
      String merchantSku, String pickupPointCode, Integer productType, Double regularPrice, Double sellingPrice,
      Integer stock, Integer minimumStock, boolean displayable, boolean buyable, boolean needInstallation) {
    super();
    this.productLevel3Wip = productLevel3Wip;
    this.productLevel1ItemId = productLevel1ItemId;
    this.gdnSku = gdnSku;
    this.merchantSku = merchantSku;
    this.pickupPointCode = pickupPointCode;
    this.productType = productType;
    this.regularPrice = regularPrice;
    this.sellingPrice = sellingPrice;
    this.stock = stock;
    this.minimumStock = minimumStock;
    this.displayable = displayable;
    this.buyable = buyable;
    this.needInstallation = needInstallation;
  }

  public ProductLevel3Wip getProductLevel3Wip() {
    return productLevel3Wip;
  }

  public void setProductLevel3Wip(ProductLevel3Wip productLevel3Wip) {
    this.productLevel3Wip = productLevel3Wip;
  }

  public String getProductLevel1ItemId() {
    return productLevel1ItemId;
  }

  public void setProductLevel1ItemId(String productLevel1ItemId) {
    this.productLevel1ItemId = productLevel1ItemId;
  }

  public String getGdnSku() {
    return gdnSku;
  }

  public void setGdnSku(String gdnSku) {
    this.gdnSku = gdnSku;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public Integer getProductType() {
    return productType;
  }

  public void setProductType(Integer productType) {
    this.productType = productType;
  }

  public Double getRegularPrice() {
    return regularPrice;
  }

  public void setRegularPrice(Double regularPrice) {
    this.regularPrice = regularPrice;
  }

  public Double getSellingPrice() {
    return sellingPrice;
  }

  public void setSellingPrice(Double sellingPrice) {
    this.sellingPrice = sellingPrice;
  }

  public Integer getStock() {
    return stock;
  }

  public void setStock(Integer stock) {
    this.stock = stock;
  }

  public Integer getMinimumStock() {
    return minimumStock;
  }

  public void setMinimumStock(Integer minimumStock) {
    this.minimumStock = minimumStock;
  }

  public boolean isDisplayable() {
    return displayable;
  }

  public void setDisplayable(boolean displayable) {
    this.displayable = displayable;
  }

  public boolean isBuyable() {
    return buyable;
  }

  public void setBuyable(boolean buyable) {
    this.buyable = buyable;
  }

  public boolean isNeedInstallation() {
    return needInstallation;
  }

  public void setNeedInstallation(boolean needInstallation) {
    this.needInstallation = needInstallation;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductLevel3ItemWip [productLevel3Wip=%s, productLevel1ItemId=%s, gdnSku=%s, merchantSku=%s, pickupPointCode=%s, productType=%s, regularPrice=%s, sellingPrice=%s, stock=%s, minimumStock=%s, displayable=%s, buyable=%s, needInstallation=%s]",
            productLevel3Wip, productLevel1ItemId, gdnSku, merchantSku, pickupPointCode, productType, regularPrice,
            sellingPrice, stock, minimumStock, displayable, buyable, needInstallation);
  }

}
