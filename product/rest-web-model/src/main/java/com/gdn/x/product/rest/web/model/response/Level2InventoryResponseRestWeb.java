package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnBaseBuilder;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Level2InventoryResponseRestWeb extends BaseResponse implements Serializable {
  public static class Builder implements GdnBaseBuilder<Level2InventoryResponseRestWeb> {
    private String level1Id;
    private String level2Id;
    private Integer stock;
    private Integer originalStock;
    private String merchantCode;
    private String level2MerchantCode;
    private Date updatedDate;
    private Date createdDate;
    private String updatedBy;
    private String createdBy;
    private boolean syncedToLevel1;

    public Builder() {

    }

    @Override
    public Level2InventoryResponseRestWeb build() {
      return new Level2InventoryResponseRestWeb(this);
    }

    public Level2InventoryResponseRestWeb.Builder setCreatedBy(String createdBy) {
      this.createdBy = createdBy;
      return this;
    }

    public Level2InventoryResponseRestWeb.Builder setCreatedDate(Date createdDate) {
      this.createdDate = createdDate;
      return this;
    }

    public Level2InventoryResponseRestWeb.Builder setFfmCtrId(String level2Id) {
      this.level2Id = level2Id;
      return this;
    }

    public Level2InventoryResponseRestWeb.Builder setLevel1Id(String level1Id) {
      this.level1Id = level1Id;
      return this;
    }

    public Level2InventoryResponseRestWeb.Builder setLevel2MerchantCode(String level2MerchantCode) {
      this.level2MerchantCode = level2MerchantCode;
      return this;
    }

    public Level2InventoryResponseRestWeb.Builder setMerchantCode(String merchantCode) {
      this.merchantCode = merchantCode;
      return this;
    }

    public Level2InventoryResponseRestWeb.Builder setOriginalStock(Integer originalStock) {
      this.originalStock = originalStock;
      return this;
    }

    public Level2InventoryResponseRestWeb.Builder setStock(Integer stock) {
      this.stock = stock;
      return this;
    }

    public Level2InventoryResponseRestWeb.Builder setSyncedToLevel1(boolean syncedToLevel1) {
      this.syncedToLevel1 = syncedToLevel1;
      return this;
    }

    public Level2InventoryResponseRestWeb.Builder setUpdatedBy(String updatedBy) {
      this.updatedBy = updatedBy;
      return this;
    }

    public Level2InventoryResponseRestWeb.Builder setUpdatedDate(Date updatedDate) {
      this.updatedDate = updatedDate;
      return this;
    }

    @Override
    public String toString() {
      return String
          .format(
              "Builder [level1Id=%s, level2Id=%s, stock=%s,originalStock=%s, merchantCode=%s, level2MerchantCode=%s, syncedToLevel1=%s, updatedDate=%s, createdDate=%s, updatedBy=%s, createdBy=%s, toString()=%s]",
              new Object[] {this.level1Id, this.level2Id, this.stock, this.originalStock,
                  this.merchantCode, this.level2MerchantCode, Boolean.valueOf(this.syncedToLevel1),
                  this.updatedDate, this.createdDate, this.updatedBy, this.createdBy,
                  super.toString()});
    }
  }

  private static final long serialVersionUID = 1L;
  private String level1Id;
  private String level2Id;
  private Integer stock;
  private Integer originalStock;
  private String merchantCode;
  private String level2MerchantCode;
  private Date updatedDate;
  private Date createdDate;
  private String updatedBy;
  private String createdBy;

  private boolean syncedToLevel1;

  public Level2InventoryResponseRestWeb() {

  }

  public Level2InventoryResponseRestWeb(Level2InventoryResponseRestWeb.Builder builder) {
    this.level1Id = builder.level1Id;
    this.level2Id = builder.level2Id;
    this.stock = builder.stock;
    this.merchantCode = builder.merchantCode;
    this.level2MerchantCode = builder.level2MerchantCode;
    this.syncedToLevel1 = builder.syncedToLevel1;
    this.updatedDate = builder.updatedDate;
    this.createdDate = builder.createdDate;
    this.updatedBy = builder.updatedBy;
    this.createdBy = builder.createdBy;
    this.originalStock = builder.originalStock;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public String getCreatedBy() {
    return this.createdBy;
  }

  @Override
  public Date getCreatedDate() {
    return this.createdDate;
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
  public String getUpdatedBy() {
    return this.updatedBy;
  }

  @Override
  public Date getUpdatedDate() {
    return this.updatedDate;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isSyncedToLevel1() {
    return this.syncedToLevel1;
  }

  @Override
  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  @Override
  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
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
  public void setUpdatedBy(String updatedBy) {
    this.updatedBy = updatedBy;
  }

  @Override
  public void setUpdatedDate(Date updatedDate) {
    this.updatedDate = updatedDate;
  }

  @Override
  public String toString() {
    return String
        .format(
            "Level2InventoryResponseRestWeb [level1Id=%s, level2Id=%s, stock=%s, originalStock=%s, merchantCode=%s, level2MerchantCode=%s, updatedDate=%s, createdDate=%s, updatedBy=%s, createdBy=%s, syncedToLevel1=%s, toString()=%s]",
            new Object[] {this.level1Id, this.level2Id, this.stock, this.originalStock,
                this.merchantCode, this.level2MerchantCode, this.updatedDate, this.createdDate,
                this.updatedBy, this.createdBy, Boolean.valueOf(this.syncedToLevel1),
                super.toString()});
  }
}
