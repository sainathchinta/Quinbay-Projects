package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemBusinessPartnerRequest extends BaseRequest {

  private static final long serialVersionUID = -3544023375425788655L;
  private String productItemId;
  private String productHashCode;
  private Integer productType;
  private String merchantSku;
  private String gdnProductItemSku;
  private Double price;
  private Double salePrice;
  private Date saleStartDate;
  private Date saleEndDate;
  private Integer stock;
  private boolean markDefaultAddress;
  private Integer minimumStock;
  private String pickupPointId;
  private boolean display = false;
  private boolean buyable = false;
  private boolean installation = false;
  private String itemCode;
  private List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests = new ArrayList<>();
  private Boolean wholesalePriceActivated;
  private List<ProductItemBusinessPartnerLogisticsRequest> productItemBusinessPartnerLogisticsRequests =
      new ArrayList<ProductItemBusinessPartnerLogisticsRequest>();

  public static class Builder {
    private String productItemId;
    private String productHashCode;
    private Integer productType;
    private String merchantSku;
    private String gdnProductItemSku;
    private Double price;
    private Double salePrice;
    private Date saleStartDate;
    private Date saleEndDate;
    private Integer stock;
    private Integer minimumStock;
    private boolean markDefaultAddress;
    private String pickupPointId;
    private boolean display = false;
    private boolean buyable = false;
    private boolean installation = false;
    private String id;
    private String storeId;
    private Date createdDate;
    private String createdBy;
    private Date updatedDate;
    private String updatedBy;
    private boolean markForDelete = false;
    private String itemCode;
    private List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests = new ArrayList<>();
    private Boolean wholesalePriceActivated;
    private List<ProductItemBusinessPartnerLogisticsRequest> productItemBusinessPartnerLogisticsRequests =
        new ArrayList<>();
            
    public Builder productItemId(String productItemId) {
      this.productItemId = productItemId;
      return this;
    }
    
    public Builder productType(Integer productType) {
      this.productType = productType;
      return this;
    }
    
    public Builder merchantSku(String merchantSku) {
      this.merchantSku = merchantSku;
      return this;
    }
    
    public Builder gdnProductItemSku(String gdnProductItemSku) {
      this.gdnProductItemSku = gdnProductItemSku;
      return this;
    }
    
    public Builder price(Double price) {
      this.price = price;
      return this;
    }
    
    public Builder salePrice(Double salePrice) {
      this.salePrice = salePrice;
      return this;
    }
    
    public Builder saleStartDate(Date saleStartDate) {
      this.saleStartDate = saleStartDate;
      return this;
    }
    
    public Builder saleEndDate(Date saleEndDate) {
      this.saleEndDate = saleEndDate;
      return this;
    }
    
    public Builder stock(Integer stock) {
      this.stock = stock;
      return this;
    }

    public Builder minimumStock(Integer minimumStock) {
      this.minimumStock = minimumStock;
      return this;
    }

    public Builder pickupPointId(String pickupPointId) {
      this.pickupPointId = pickupPointId;
      return this;
    }

    public Builder markDefaultAddress(boolean markDefaultAddress) {
      this.markDefaultAddress = markDefaultAddress;
      return this;
    }

    public Builder display(boolean display) {
      this.display = display;
      return this;
    }

    public Builder buyable(boolean buyable) {
      this.buyable = buyable;
      return this;
    }

    public Builder installation(boolean installation) {
      this.installation = installation;
      return this;
    }
    
    public Builder id(String id) {
      this.id = id;
      return this;
    }

    public Builder storeId(String storeId) {
      this.storeId = storeId;
      return this;
    }
    
    public Builder createdDate(Date createdDate) {
      this.createdDate = createdDate;
      return this;
    }
    
    public Builder createdBy(String createdBy) {
      this.createdBy = createdBy;
      return this;
    }
    
    public Builder updatedDate(Date updatedDate) {
      this.updatedDate = updatedDate;
      return this;
    }
    
    public Builder updatedBy(String updatedBy) {
      this.updatedBy = updatedBy;
      return this;
    }
    
    public Builder markForDelete(boolean markForDelete) {
      this.markForDelete = markForDelete;
      return this;
    }

    public Builder productHashCode(String productHashCode) {
      this.productHashCode = productHashCode;
      return this;
    }

    public Builder productItemWholesalePriceRequests(
        List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests) {
      this.productItemWholesalePriceRequests = productItemWholesalePriceRequests;
      return this;
    }

    public Builder itemCode(String itemCode) {
      this.itemCode = itemCode;
      return this;
    }

    public Builder wholesalePriceActivated(Boolean wholesalePriceActivated) {
      this.wholesalePriceActivated = wholesalePriceActivated;
      return this;
    }
    
    public Builder productItemBusinessPartnerLogisticsRequests(
        List<ProductItemBusinessPartnerLogisticsRequest> productItemBusinessPartnerLogisticsRequests) {
      this.productItemBusinessPartnerLogisticsRequests =
          productItemBusinessPartnerLogisticsRequests;
      return this;
    }

    public ProductItemBusinessPartnerRequest build() {
      return new ProductItemBusinessPartnerRequest(this);
    }

  }
  
  private ProductItemBusinessPartnerRequest(Builder builder) {   
    setId(builder.id);
    setStoreId(builder.storeId);
    setCreatedDate(builder.createdDate);
    setCreatedBy(builder.createdBy);
    setUpdatedDate(builder.updatedDate);
    setUpdatedBy(builder.updatedBy);
    setMarkForDelete(builder.markForDelete);
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
    this.markDefaultAddress = builder.markDefaultAddress;
    this.pickupPointId = builder.pickupPointId;
    this.display = builder.display;
    this.buyable = builder.buyable;
    this.installation = builder.installation;
    this.productHashCode = builder.productHashCode;
    this.productItemWholesalePriceRequests = builder.productItemWholesalePriceRequests;
    this.itemCode = builder.itemCode;
    this.productItemBusinessPartnerLogisticsRequests = builder.productItemBusinessPartnerLogisticsRequests;
}

  public ProductItemBusinessPartnerRequest() {}

  public String getMerchantSku() {
    return merchantSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }
  
  public String getGdnProductItemSku() {
    return gdnProductItemSku;
  }

  public Integer getMinimumStock() {
    return minimumStock;
  }

  public String getPickupPointId() {
    return pickupPointId;
  }

  public Double getPrice() {
    return price;
  }

  public String getProductItemId() {
    return productItemId;
  }

  public Integer getProductType() {
    return productType;
  }

  public Date getSaleEndDate() {
    return saleEndDate;
  }

  public Double getSalePrice() {
    return salePrice;
  }

  public Date getSaleStartDate() {
    return saleStartDate;
  }

  public Integer getStock() {
    return stock;
  }

  public boolean isBuyable() {
    return buyable;
  }

  public boolean isDisplay() {
    return display;
  }

  public boolean isInstallation() {
    return installation;
  }

  public void setBuyable(boolean buyable) {
    this.buyable = buyable;
  }

  public void setDisplay(boolean display) {
    this.display = display;
  }

  public void setGdnProductItemSku(String gdnProductItemSku) {
    this.gdnProductItemSku = gdnProductItemSku;
  }

  public void setInstallation(boolean installation) {
    this.installation = installation;
  }

  public void setMinimumStock(Integer minimumStock) {
    this.minimumStock = minimumStock;
  }

  public void setPickupPointId(String pickupPointId) {
    this.pickupPointId = pickupPointId;
  }

  public void setPrice(Double price) {
    this.price = price;
  }

  public void setProductItemId(String productItemId) {
    this.productItemId = productItemId;
  }

  public void setProductType(Integer productType) {
    this.productType = productType;
  }

  public void setSaleEndDate(Date saleEndDate) {
    this.saleEndDate = saleEndDate;
  }

  public void setSalePrice(Double salePrice) {
    this.salePrice = salePrice;
  }

  public void setSaleStartDate(Date saleStartDate) {
    this.saleStartDate = saleStartDate;
  }

  public void setStock(Integer stock) {
    this.stock = stock;
  }

  public boolean isMarkDefaultAddress() {
    return markDefaultAddress;
  }

  public void setMarkDefaultAddress(boolean markDefaultAddress) {
    this.markDefaultAddress = markDefaultAddress;
  }

  public String getProductHashCode() {
    return productHashCode;
  }

  public void setProductHashCode(String productHashCode) {
    this.productHashCode = productHashCode;
  }

  public List<ProductItemWholesalePriceRequest> getProductItemWholesalePriceRequests() {
    return productItemWholesalePriceRequests;
  }

  public void setProductItemWholesalePriceRequests(
      List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests) {
    this.productItemWholesalePriceRequests = productItemWholesalePriceRequests;
  }

  public String getItemCode() {
    return itemCode;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public Boolean getWholesalePriceActivated() {
    return wholesalePriceActivated;
  }

  public void setWholesalePriceActivated(Boolean wholesalePriceActivated) {
    this.wholesalePriceActivated = wholesalePriceActivated;
  }

  public List<ProductItemBusinessPartnerLogisticsRequest> getProductItemBusinessPartnerLogisticsRequests() {
    return productItemBusinessPartnerLogisticsRequests;
  }

  public void setProductItemBusinessPartnerLogisticsRequests(
      List<ProductItemBusinessPartnerLogisticsRequest> productItemBusinessPartnerLogisticsRequests) {
    this.productItemBusinessPartnerLogisticsRequests = productItemBusinessPartnerLogisticsRequests;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productItemId", productItemId)
        .append("productType", productType).append("merchantSku", merchantSku)
        .append("gdnProductItemSku", gdnProductItemSku).append("price", price)
        .append("salePrice", salePrice).append("saleStartDate", saleStartDate)
        .append("saleEndDate", saleEndDate).append("stock", stock)
        .append("markDefaultAddress", markDefaultAddress).append("minimumStock", minimumStock)
        .append("pickupPointId", pickupPointId).append("display", display)
        .append("productHashCode", productHashCode)
        .append("buyable", buyable).append("installation", installation)
        .append("productItemWholesalePriceRequests", productItemWholesalePriceRequests)
        .append("itemCode", itemCode)
        .append("wholesalePriceActivated", wholesalePriceActivated)
        .append("productItemBusinessPartnerLogisticsRequests", productItemBusinessPartnerLogisticsRequests).toString();
  }
}
