package com.gda.mta.product.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3SummaryMinifiedResponse extends BaseResponse{
  private static final long serialVersionUID = -5657905284401873792L;
  private String itemSku;
  private String skuCode;
  private String merchantSku;
  private String itemName;
  private Integer availableStockLevel2;
  private Integer reservedStockLevel2;
  private List<ProductLevel3PriceResponse> prices;
  private List<ProductLevel3ViewConfigResponse> viewConfigs;
  private List<ProductLevel3ImageResponse> images;
  private boolean promoBundling;
  
  public ProductLevel3SummaryMinifiedResponse(){}
  
  public String getItemSku() {
    return itemSku;
  }
  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }
  public String getSkuCode() {
    return skuCode;
  }
  public void setSkuCode(String skuCode) {
    this.skuCode = skuCode;
  }
  public String getMerchantSku() {
    return merchantSku;
  }
  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }
  public String getItemName() {
    return itemName;
  }
  public void setItemName(String itemName) {
    this.itemName = itemName;
  }
  public Integer getAvailableStockLevel2() {
    return availableStockLevel2;
  }
  public void setAvailableStockLevel2(Integer availableStockLevel2) {
    this.availableStockLevel2 = availableStockLevel2;
  }
  public Integer getReservedStockLevel2() {
    return reservedStockLevel2;
  }
  public void setReservedStockLevel2(Integer reservedStockLevel2) {
    this.reservedStockLevel2 = reservedStockLevel2;
  }
  public List<ProductLevel3PriceResponse> getPrices() {
    return prices;
  }
  public void setPrices(List<ProductLevel3PriceResponse> prices) {
    this.prices = prices;
  }
  public List<ProductLevel3ViewConfigResponse> getViewConfigs() {
    return viewConfigs;
  }
  public void setViewConfigs(List<ProductLevel3ViewConfigResponse> viewConfigs) {
    this.viewConfigs = viewConfigs;
  }
  public List<ProductLevel3ImageResponse> getImages() {
    return images;
  }
  public void setImages(List<ProductLevel3ImageResponse> images) {
    this.images = images;
  }
  public static long getSerialversionuid() {
    return serialVersionUID;
  }

  public boolean isPromoBundling() {
    return promoBundling;
  }

  public void setPromoBundling(boolean promoBundling) {
    this.promoBundling = promoBundling;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductLevel3SummaryMinifiedResponse [itemSku=%s, skuCode=%s, merchantSku=%s, itemName=%s, availableStockLevel2=%s, reservedStockLevel2=%s, prices=%s, viewConfigs=%s, images=%s, getItemSku()=%s, getSkuCode()=%s, getMerchantSku()=%s, getItemName()=%s, getAvailableStockLevel2()=%s, getReservedStockLevel2()=%s, getPrices()=%s, getViewConfigs()=%s, getImages()=%s]",
            itemSku, skuCode, merchantSku, itemName, availableStockLevel2, reservedStockLevel2,
            prices, viewConfigs, images, getItemSku(), getSkuCode(), getMerchantSku(),
            getItemName(), getAvailableStockLevel2(), getReservedStockLevel2(), getPrices(),
            getViewConfigs(), getImages());
  }

  
}
