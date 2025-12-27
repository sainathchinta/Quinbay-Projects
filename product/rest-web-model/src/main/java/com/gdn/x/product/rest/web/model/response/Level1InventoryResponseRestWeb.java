package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnBaseBuilder;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Level1InventoryResponseRestWeb extends BaseResponse implements Serializable {
  public static class Builder implements GdnBaseBuilder<Level1InventoryResponseRestWeb> {
    private String level1Id;
    private Double lat;
    private Double lon;
    private Integer stockAfterReserved;
    private Integer originalStock;
    private String merchantCode;
    private String warehouseCity;
    private Date updatedDate;
    private Date createdDate;
    private String updatedBy;
    private String createdBy;
    private boolean syncedToLevel2;

    public Builder() {}

    @Override
    public Level1InventoryResponseRestWeb build() {
      return new Level1InventoryResponseRestWeb(this);
    }

    public Level1InventoryResponseRestWeb.Builder setCreatedBy(String createdBy) {
      this.createdBy = createdBy;
      return this;
    }

    public Level1InventoryResponseRestWeb.Builder setCreatedDate(Date createdDate) {
      this.createdDate = createdDate;
      return this;
    }

    public Level1InventoryResponseRestWeb.Builder setLat(Double lat) {
      this.lat = lat;
      return this;
    }

    public Level1InventoryResponseRestWeb.Builder setLevel1Id(String level1Id) {
      this.level1Id = level1Id;
      return this;
    }

    public Level1InventoryResponseRestWeb.Builder setLon(Double lon) {
      this.lon = lon;
      return this;
    }

    public Level1InventoryResponseRestWeb.Builder setMerchantCode(String merchantCode) {
      this.merchantCode = merchantCode;
      return this;
    }

    public Level1InventoryResponseRestWeb.Builder setOriginalStock(Integer originalStock) {
      this.originalStock = originalStock;
      return this;
    }

    public Level1InventoryResponseRestWeb.Builder setStockAfterReserved(Integer stockAfterReserved) {
      this.stockAfterReserved = stockAfterReserved;
      return this;
    }

    public Level1InventoryResponseRestWeb.Builder setSyncedToLevel2(boolean syncedToLevel2) {
      this.syncedToLevel2 = syncedToLevel2;
      return this;
    }

    public Level1InventoryResponseRestWeb.Builder setUpdatedBy(String updatedBy) {
      this.updatedBy = updatedBy;
      return this;
    }

    public Level1InventoryResponseRestWeb.Builder setUpdatedDate(Date updatedDate) {
      this.updatedDate = updatedDate;
      return this;
    }

    public Level1InventoryResponseRestWeb.Builder setWarehouseCity(String warehouseCity) {
      this.warehouseCity = warehouseCity;
      return this;
    }

    @Override
    public String toString() {
      return String
          .format(
              "Builder [level1Id=%s, lat=%s, lon=%s, stockAfterReserved=%s, originalStock=%s, merchantCode=%s, warehouseCity=%s, syncedToLevel2=%s, updatedDate=%s, createdDate=%s, updatedBy=%s, createdBy=%s]",
              new Object[] {this.level1Id, this.lat, this.lon, this.stockAfterReserved,
                  this.originalStock, this.merchantCode, this.warehouseCity,
                  Boolean.valueOf(this.syncedToLevel2), this.updatedDate, this.createdDate,
                  this.updatedBy, this.createdBy});
    }
  }

  private static final long serialVersionUID = 1L;
  private String level1Id;
  private Double lat;
  private Double lon;
  private Integer stockAfterReserved;
  private Integer originalStock;
  private String merchantCode;
  private String warehouseCity;
  private Date updatedDate;
  private Date createdDate;
  private String updatedBy;
  private String createdBy;

  private boolean syncedToLevel2;

  public Level1InventoryResponseRestWeb() {}

  public Level1InventoryResponseRestWeb(Level1InventoryResponseRestWeb.Builder builder) {
    this.level1Id = builder.level1Id;
    this.lat = builder.lat;
    this.lon = builder.lon;
    this.stockAfterReserved = builder.stockAfterReserved;
    this.originalStock = builder.originalStock;
    this.merchantCode = builder.merchantCode;
    this.warehouseCity = builder.warehouseCity;
    this.updatedDate = builder.updatedDate;
    this.createdDate = builder.createdDate;
    this.updatedBy = builder.updatedBy;
    this.createdBy = builder.createdBy;
    this.syncedToLevel2 = builder.syncedToLevel2;
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

  public Double getLat() {
    return this.lat;
  }

  public String getLevel1Id() {
    return this.level1Id;
  }

  public Double getLon() {
    return this.lon;
  }

  public String getMerchantCode() {
    return this.merchantCode;
  }

  public Integer getOriginalStock() {
    return this.originalStock;
  }

  public Integer getStockAfterReserved() {
    return this.stockAfterReserved;
  }

  @Override
  public String getUpdatedBy() {
    return this.updatedBy;
  }

  @Override
  public Date getUpdatedDate() {
    return this.updatedDate;
  }

  public String getWarehouseCity() {
    return this.warehouseCity;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isSyncedToLevel2() {
    return this.syncedToLevel2;
  }

  @Override
  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  @Override
  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  public void setLat(Double lat) {
    this.lat = lat;
  }

  public void setLevel1Id(String level1Id) {
    this.level1Id = level1Id;
  }

  public void setLon(Double lon) {
    this.lon = lon;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setOriginalStock(Integer originalStock) {
    this.originalStock = originalStock;
  }

  public void setStockAfterReserved(Integer stockAfterReserved) {
    this.stockAfterReserved = stockAfterReserved;
  }

  public void setSyncedToLevel2(boolean syncedToLevel2) {
    this.syncedToLevel2 = syncedToLevel2;
  }

  @Override
  public void setUpdatedBy(String updatedBy) {
    this.updatedBy = updatedBy;
  }

  @Override
  public void setUpdatedDate(Date updatedDate) {
    this.updatedDate = updatedDate;
  }

  public void setWarehouseCity(String warehouseCity) {
    this.warehouseCity = warehouseCity;
  }

  @Override
  public String toString() {
    return String
        .format(
            "Level1InventoryResponseRestWeb [level1Id=%s, lat=%s, lon=%s, stockAfterReserved=%s, originalStock=%s, merchantCode=%s, warehouseCity=%s, updatedDate=%s, createdDate=%s, updatedBy=%s, createdBy=%s, syncedToLevel2=%s, toString()=%s]",
            new Object[] {this.level1Id, this.lat, this.lon, this.stockAfterReserved,
                this.originalStock, this.merchantCode, this.warehouseCity, this.updatedDate,
                this.createdDate, this.updatedBy, this.createdBy,
                Boolean.valueOf(this.syncedToLevel2), super.toString()});
  }
}
