package com.gdn.x.product.rest.web.model.request;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
@JsonIgnoreProperties(ignoreUnknown = true)
public class Level2InventoryRequestRestWeb {
  private String level1Id;
  private String level2Id;
  private Integer stock;
  private String merchantCode;
  private String level2MerchantCode;
  private boolean syncedToLevel1;
  private Integer originalStock;

  public Level2InventoryRequestRestWeb() {}

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getLevel1Id() {
    return this.level1Id;
  }

  public String getLevel2Id() {
    return this.level2Id;
  }

  public String getLevel2MerchantCode() {
    return this.level2MerchantCode;
  }

  public String getMerchantCode() {
    return this.merchantCode;
  }

  public Integer getOriginalStock() {
    return this.originalStock;
  }

  public Integer getStock() {
    return this.stock;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isSyncedToLevel1() {
    return this.syncedToLevel1;
  }

  public void setLevel1Id(String level1Id) {
    this.level1Id = level1Id;
  }

  public void setLevel2Id(String level2Id) {
    this.level2Id = level2Id;
  }

  public void setLevel2MerchantCode(String level2MerchantCode) {
    this.level2MerchantCode = level2MerchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setOriginalStock(Integer originalStock) {
    this.originalStock = originalStock;
  }

  public void setStock(Integer stock) {
    this.stock = stock;
  }

  public void setSyncedToLevel1(boolean syncedToLevel1) {
    this.syncedToLevel1 = syncedToLevel1;
  }

  @Override
  public String toString() {
    return String
        .format(
            "Level2InventoryRequestRestWeb [level1Id=%s, level2Id=%s, stock=%s, originalStock=%s, merchantCode=%s, level2MerchantCode=%s, syncedToLevel1=%s, toString()=%s]",
            new Object[] {this.level1Id, this.level2Id, this.stock, this.originalStock,
                this.merchantCode, this.level2MerchantCode, Boolean.valueOf(this.syncedToLevel1),
                super.toString()});
  }
}
