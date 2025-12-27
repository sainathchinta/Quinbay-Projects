package com.gdn.partners.pbp.entity.mv;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

import com.gdn.GdnBaseEntity;
import com.gdn.partners.pbp.model.vo.MaterializedView;
import com.gdn.partners.pbp.commons.util.CommonUtils;

/**
 * this entity is used as merchant product materialized view for MTA
 * 
 * @author andrew.winata
 */
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = MerchantProductMV.TABLE_NAME)
public class MerchantProductMV extends GdnBaseEntity implements MaterializedView {
  private static final long serialVersionUID = -5890805204579558072L;
  public static final String TABLE_NAME = "prd_merchant_product_mv";
  public static final String COLUMN_BUSINESS_PARTNER_CODE = "business_partner_code";
  public static final String COLUMN_CATEGORY_CODE = "category_code";
  public static final String COLUMN_GDN_SKU = "item_sku";
  public static final String COLUMN_NAME = "name";
  public static final String COLUMN_PICKUP_POINT_CODE = "pickup_point_code";
  public static final String COLUMN_IS_DISPLAYABLE = "is_displayable";
  public static final String COLUMN_IS_BUYABLE = "is_buyable";
  public static final String COLUMN_IS_ARCHIVED = "is_archived";
  public static final String COLUMN_BELOW_MINIMUM_STOCK_STATUS = "is_below_minimum_stock";
  public static final String COLUMN_OOS_STATUS = "is_out_of_stock";
  public static final String COLUMN_LAST_INDEXED_PRODUCT_DATE = "last_indexed_product_date";
  public static final String COLUMN_LAST_INDEXED_INVENTORY_DATE = "last_indexed_inventory_date";
  public static final String COLUMN_LAST_INDEXED_DATE = "last_indexed_date";
  public static final String COLUMN_MERCHANT_SKU = "merchant_sku";

  @Column(name = MerchantProductMV.COLUMN_BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Column(name = MerchantProductMV.COLUMN_CATEGORY_CODE)
  private String categoryCode;

  @Column(name = MerchantProductMV.COLUMN_GDN_SKU, unique = true)
  private String itemSku;

  @Column(name = MerchantProductMV.COLUMN_NAME)
  private String name;

  @Column(name = MerchantProductMV.COLUMN_PICKUP_POINT_CODE)
  private String pickupPointCode;

  @Column(name = MerchantProductMV.COLUMN_MERCHANT_SKU)
  private String merchantSku;

  @Column(name = MerchantProductMV.COLUMN_IS_DISPLAYABLE)
  private Boolean displayable;

  @Column(name = MerchantProductMV.COLUMN_IS_BUYABLE)
  private Boolean buyable;

  @Column(name = MerchantProductMV.COLUMN_IS_ARCHIVED)
  private Boolean archived;

  @Column(name = MerchantProductMV.COLUMN_BELOW_MINIMUM_STOCK_STATUS)
  private Boolean belowMinimumStockStatus;

  @Column(name = MerchantProductMV.COLUMN_OOS_STATUS)
  private Boolean outOfStockStatus;

  @Column(name = MerchantProductMV.COLUMN_LAST_INDEXED_PRODUCT_DATE)
  private Date lastIndexedProductDate;

  @Column(name = MerchantProductMV.COLUMN_LAST_INDEXED_INVENTORY_DATE)
  private Date lastIndexedInventoryDate;

  @Column(name = MerchantProductMV.COLUMN_LAST_INDEXED_DATE)
  private Date lastIndexedDate;

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public Boolean getDisplayable() {
    return displayable;
  }

  public void setDisplayable(Boolean displayable) {
    this.displayable = displayable;
  }

  public Boolean getBuyable() {
    return buyable;
  }

  public void setBuyable(Boolean buyable) {
    this.buyable = buyable;
  }

  public Boolean getArchived() {
    return archived;
  }

  public void setArchived(Boolean archived) {
    this.archived = archived;
  }

  public Boolean getBelowMinimumStockStatus() {
    return belowMinimumStockStatus;
  }

  public void setBelowMinimumStockStatus(Boolean belowMinimumStockStatus) {
    this.belowMinimumStockStatus = belowMinimumStockStatus;
  }

  public Boolean getOutOfStockStatus() {
    return outOfStockStatus;
  }

  public void setOutOfStockStatus(Boolean outOfStockStatus) {
    this.outOfStockStatus = outOfStockStatus;
  }

  public Date getLastIndexedProductDate() {
    return lastIndexedProductDate;
  }

  public void setLastIndexedProductDate(Date lastIndexedProductDate) {
    this.lastIndexedProductDate = lastIndexedProductDate;
  }

  public Date getLastIndexedInventoryDate() {
    return lastIndexedInventoryDate;
  }

  public void setLastIndexedInventoryDate(Date lastIndexedInventoryDate) {
    this.lastIndexedInventoryDate = lastIndexedInventoryDate;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public Date getLastIndexedDate() {
    return lastIndexedDate;
  }

  public void setLastIndexedDate(Date lastIndexedDate) {
    this.lastIndexedDate = lastIndexedDate;
  }

  @Override
  public String toString() {
    return CommonUtils.stringifyBean(this);
  }
}
