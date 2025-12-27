package com.gda.mta.product.dto;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemLevel3Request extends BaseRequest {

  private static final long serialVersionUID = 2003051275890098137L;
  private String itemSku;
  private String skuCode;
  private String merchantSku;
  private String upcCode;
  private String itemName;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private Integer dangerousGoodsLevel;
  private Boolean lateFulfillment;
  private String pickupPointCode;
  private String pickupPointName;
  private Integer deltaStock;
  private Integer minimumStock;
  private Boolean synchronizeStock;
  private Boolean off2OnActiveFlag;
  private List<ProductLevel3PriceRequest> prices;
  private List<ProductLevel3ViewConfigRequest> viewConfigs;
  private List<ProductLevel3ImageRequest> images;
  private boolean isContentChanged;

  public ProductItemLevel3Request() {}

  public ProductItemLevel3Request(String itemSku, String skuCode, String merchantSku,
                                  String upcCode, String itemName, Double length, Double width, Double height, Double weight,
                                  Double shippingWeight, Integer dangerousGoodsLevel, Boolean lateFulfillment,
                                  String pickupPointCode, String pickupPointName, Integer deltaStock, Integer minimumStock, Boolean synchronizeStock,
                                  List<ProductLevel3PriceRequest> prices, List<ProductLevel3ViewConfigRequest> viewConfigs,
                                  List<ProductLevel3ImageRequest> images) {
    super();
    this.itemSku = itemSku;
    this.skuCode = skuCode;
    this.merchantSku = merchantSku;
    this.upcCode = upcCode;
    this.itemName = itemName;
    this.length = length;
    this.width = width;
    this.height = height;
    this.weight = weight;
    this.shippingWeight = shippingWeight;
    this.dangerousGoodsLevel = dangerousGoodsLevel;
    this.lateFulfillment = lateFulfillment;
    this.pickupPointCode = pickupPointCode;
    this.pickupPointName = pickupPointName;
    this.deltaStock = deltaStock;
    this.minimumStock = minimumStock;
    this.synchronizeStock = synchronizeStock;
    this.prices = prices;
    this.viewConfigs = viewConfigs;
    this.images = images;
  }

  public Boolean getOff2OnActiveFlag() {
    return off2OnActiveFlag;
  }

  public void setOff2OnActiveFlag(Boolean off2OnActiveFlag) {
    this.off2OnActiveFlag = off2OnActiveFlag;
  }

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

  public String getUpcCode() {
    return upcCode;
  }

  public void setUpcCode(String upcCode) {
    this.upcCode = upcCode;
  }

  public String getItemName() {
    return itemName;
  }

  public void setItemName(String itemName) {
    this.itemName = itemName;
  }

  public Double getLength() {
    return length;
  }

  public void setLength(Double length) {
    this.length = length;
  }

  public Double getWidth() {
    return width;
  }

  public void setWidth(Double width) {
    this.width = width;
  }

  public Double getHeight() {
    return height;
  }

  public void setHeight(Double height) {
    this.height = height;
  }

  public Double getWeight() {
    return weight;
  }

  public void setWeight(Double weight) {
    this.weight = weight;
  }

  public Double getShippingWeight() {
    return shippingWeight;
  }

  public void setShippingWeight(Double shippingWeight) {
    this.shippingWeight = shippingWeight;
  }

  public Integer getDangerousGoodsLevel() {
    return dangerousGoodsLevel;
  }

  public void setDangerousGoodsLevel(Integer dangerousGoodsLevel) {
    this.dangerousGoodsLevel = dangerousGoodsLevel;
  }

  public Boolean getLateFulfillment() {
    return lateFulfillment;
  }

  public void setLateFulfillment(Boolean lateFulfillment) {
    this.lateFulfillment = lateFulfillment;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public String getPickupPointName() {
    return pickupPointName;
  }

  public void setPickupPointName(String pickupPointName) {
    this.pickupPointName = pickupPointName;
  }

  public Integer getDeltaStock() {
    return deltaStock;
  }

  public void setDeltaStock(Integer deltaStock) {
    this.deltaStock = deltaStock;
  }

  public Boolean getSynchronizeStock() {
    return synchronizeStock;
  }

  public void setSynchronizeStock(Boolean synchronizeStock) {
    this.synchronizeStock = synchronizeStock;
  }

  public List<ProductLevel3PriceRequest> getPrices() {
    return prices;
  }

  public void setPrices(List<ProductLevel3PriceRequest> prices) {
    this.prices = prices;
  }

  public List<ProductLevel3ViewConfigRequest> getViewConfigs() {
    return viewConfigs;
  }

  public void setViewConfigs(List<ProductLevel3ViewConfigRequest> viewConfigs) {
    this.viewConfigs = viewConfigs;
  }

  public List<ProductLevel3ImageRequest> getImages() {
    return images;
  }

  public void setImages(List<ProductLevel3ImageRequest> images) {
    this.images = images;
  }

  public Integer getMinimumStock() {
    return minimumStock;
  }

  public void setMinimumStock(Integer minimumStock) {
    this.minimumStock = minimumStock;
  }

  public static long getSerialversionuid() {
    return serialVersionUID;
  }

  public boolean isContentChanged() {
    return isContentChanged;
  }

  public void setContentChanged(boolean contentChanged) {
    isContentChanged = contentChanged;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductItemLevel3Request [itemSku=%s, skuCode=%s, merchantSku=%s, upcCode=%s, itemName=%s, length=%s," +
                " width=%s, height=%s, weight=%s, shippingWeight=%s, dangerousGoodsLevel=%s, lateFulfillment=%s, " +
                "pickupPointCode=%s, pickupPointName=%s, deltaStock=%s, synchronizeStock=%s, prices=%s, " +
                "viewConfigs=%s, images=%s, getItemSku()=%s, getSkuCode()=%s, getMerchantSku()=%s, getUpcCode()=%s," +
                " getItemName()=%s, getLength()=%s, getWidth()=%s, getHeight()=%s, getWeight()=%s, getShippingWeight()=%s," +
                " getDangerousGoodsLevel()=%s, getLateFulfillment()=%s, getPickupPointCode()=%s, getPickupPointName()=%s, " +
                "getDeltaStock()=%s, getSynchronizeStock()=%s, getPrices()=%s, getViewConfigs()=%s, getImages()=%s, "
                + "isContentChanged()=&s]",
            itemSku, skuCode, merchantSku, upcCode, itemName, length, width, height, weight,
            shippingWeight, dangerousGoodsLevel, lateFulfillment, pickupPointCode, pickupPointName,
            deltaStock, minimumStock, synchronizeStock, prices, viewConfigs, images, getItemSku(), getSkuCode(),
            getMerchantSku(), getUpcCode(), getItemName(), getLength(), getWidth(), getHeight(),
            getWeight(), getShippingWeight(), getDangerousGoodsLevel(), getLateFulfillment(),
            getPickupPointCode(), getPickupPointName(), getDeltaStock(), getMinimumStock(), getSynchronizeStock(),
            getPrices(), getViewConfigs(), getImages(), isContentChanged());
  }

}