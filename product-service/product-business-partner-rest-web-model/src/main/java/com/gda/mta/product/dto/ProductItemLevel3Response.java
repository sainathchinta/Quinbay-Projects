package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductItemWholesalePriceResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemLevel3Response extends BaseResponse {

  private static final long serialVersionUID = 1321315652200356490L;
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
  private Integer availableStockLevel1;
  private Integer reservedStockLevel1;
  private Integer availableStockLevel2;
  private Integer reservedStockLevel2;
  private Integer minimumStock;
  private Boolean synchronizeStock;
  private Boolean off2OnActiveFlag;
  private String pristineId;
  private List<ProductLevel3PriceResponse> prices;
  private List<ProductLevel3ViewConfigResponse> viewConfigs;
  private List<ProductLevel3ImageResponse> images;
  private Boolean wholesalePriceActivated;
  private boolean wholesalePromoActivated;
  private List<ProductItemWholesalePriceResponse> productItemWholesalePriceResponses = new ArrayList<>();
  private Double cogs;
  private String cogsErrorCode;
  private boolean promoBundling;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean disableUnSync;
  private boolean priceEditDisabled;
  private List<ProductItemLevel3LogisticResponse> productItemLevel3LogisticResponse;
  private boolean markForDelete;
  private Boolean isArchived;

  public ProductItemLevel3Response() {
    // do nothing
  }

  public ProductItemLevel3Response(String itemSku, String skuCode, String merchantSku,
                                   String upcCode, String itemName, Double length, Double width, Double height, Double weight,
                                   Double shippingWeight, Integer dangerousGoodsLevel, Boolean lateFulfillment,
                                   String pickupPointCode, String pickupPointName, Integer availableStockLevel1,
                                   Integer reservedStockLevel1, Integer availableStockLevel2, Integer reservedStockLevel2,
                                   Integer minimumStock, Boolean synchronizeStock, List<ProductLevel3PriceResponse> prices,
                                   List<ProductLevel3ViewConfigResponse> viewConfigs, List<ProductLevel3ImageResponse> images,
                                   List<ProductItemLevel3LogisticResponse> productItemLevel3LogisticResponse) {
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
    this.availableStockLevel1 = availableStockLevel1;
    this.reservedStockLevel1 = reservedStockLevel1;
    this.availableStockLevel2 = availableStockLevel2;
    this.reservedStockLevel2 = reservedStockLevel2;
    this.minimumStock = minimumStock;
    this.synchronizeStock = synchronizeStock;
    this.prices = prices;
    this.viewConfigs = viewConfigs;
    this.images = images;
    this.productItemLevel3LogisticResponse = productItemLevel3LogisticResponse;
  }

  public ProductItemLevel3Response(String itemSku, String skuCode, String merchantSku,
                                   String upcCode, String itemName, Double length, Double width, Double height, Double weight,
                                   Double shippingWeight, Integer dangerousGoodsLevel, Boolean lateFulfillment,
                                   String pickupPointCode, String pickupPointName, Integer availableStockLevel1,
                                   Integer reservedStockLevel1, Integer availableStockLevel2, Integer reservedStockLevel2,
                                   Integer minimumStock, Boolean synchronizeStock, List<ProductLevel3PriceResponse> prices,
                                   List<ProductLevel3ViewConfigResponse> viewConfigs, List<ProductLevel3ImageResponse> images,
                                   Double cogs, List<ProductItemLevel3LogisticResponse> productItemLevel3LogisticResponse) {
    this(itemSku, skuCode, merchantSku, upcCode, itemName, length, width, height, weight,
        shippingWeight, dangerousGoodsLevel, lateFulfillment, pickupPointCode, pickupPointName,
        availableStockLevel1, reservedStockLevel1, availableStockLevel2, reservedStockLevel2,
        minimumStock, synchronizeStock, prices, viewConfigs, images, productItemLevel3LogisticResponse);
    this.cogs = cogs;
  }

  public ProductItemLevel3Response(String itemSku, String skuCode, String merchantSku,
                                   String upcCode, String itemName, Double length, Double width, Double height, Double weight,
                                   Double shippingWeight, Integer dangerousGoodsLevel, Boolean lateFulfillment,
                                   String pickupPointCode, String pickupPointName, Integer availableStockLevel1,
                                   Integer reservedStockLevel1, Integer availableStockLevel2, Integer reservedStockLevel2,
                                   Integer minimumStock, Boolean synchronizeStock, String pristineId, List<ProductLevel3PriceResponse> prices,
                                   List<ProductLevel3ViewConfigResponse> viewConfigs, List<ProductLevel3ImageResponse> images,
                                   Double cogs, List<ProductItemLevel3LogisticResponse> productItemLevel3LogisticResponse) {
    this(itemSku, skuCode, merchantSku, upcCode, itemName, length, width, height, weight,
        shippingWeight, dangerousGoodsLevel, lateFulfillment, pickupPointCode, pickupPointName,
        availableStockLevel1, reservedStockLevel1, availableStockLevel2, reservedStockLevel2,
        minimumStock, synchronizeStock, prices, viewConfigs, images, cogs, productItemLevel3LogisticResponse);
    this.pristineId = pristineId;
  }

  public String getPristineId() {
    return pristineId;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
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

  public Double getCogs() {
    return cogs;
  }

  public void setCogs(Double cogs) {
    this.cogs = cogs;
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

  public Integer getAvailableStockLevel1() {
    return availableStockLevel1;
  }

  public void setAvailableStockLevel1(Integer availableStockLevel1) {
    this.availableStockLevel1 = availableStockLevel1;
  }

  public Integer getReservedStockLevel1() {
    return reservedStockLevel1;
  }

  public void setReservedStockLevel1(Integer reservedStockLevel1) {
    this.reservedStockLevel1 = reservedStockLevel1;
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

  public Boolean getSynchronizeStock() {
    return synchronizeStock;
  }

  public void setSynchronizeStock(Boolean synchronizeStock) {
    this.synchronizeStock = synchronizeStock;
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

  public Integer getMinimumStock() {
    return minimumStock;
  }

  public void setMinimumStock(Integer minimumStock) {
    this.minimumStock = minimumStock;
  }

  public String getCogsErrorCode() {
    return cogsErrorCode;
  }

  public void setCogsErrorCode(String cogsErrorCode) {
    this.cogsErrorCode = cogsErrorCode;
  }

  public boolean isPromoBundling() {
    return promoBundling;
  }

  public void setPromoBundling(boolean promoBundling) {
    this.promoBundling = promoBundling;
  }

  public boolean isMerchantPromoDiscount() {
    return merchantPromoDiscount;
  }

  public void setMerchantPromoDiscount(boolean merchantPromoDiscount) {
    this.merchantPromoDiscount = merchantPromoDiscount;
  }

  public boolean isMerchantPromoDiscountActivated() {
    return merchantPromoDiscountActivated;
  }

  public void setMerchantPromoDiscountActivated(boolean merchantPromoDiscountActivated) {
    this.merchantPromoDiscountActivated = merchantPromoDiscountActivated;
  }

  public Boolean getDisableUnSync() {
    return disableUnSync;
  }

  public void setDisableUnSync(Boolean disableUnSync) {
    this.disableUnSync = disableUnSync;
  }

  public boolean isPriceEditDisabled() {
    return priceEditDisabled;
  }

  public void setPriceEditDisabled(boolean priceEditDisabled) {
    this.priceEditDisabled = priceEditDisabled;
  }

  public Boolean getWholesalePriceActivated() {
    return wholesalePriceActivated;
  }

  public void setWholesalePriceActivated(Boolean wholesalePriceActivated) {
    this.wholesalePriceActivated = wholesalePriceActivated;
  }

  public boolean isWholesalePromoActivated() {
    return wholesalePromoActivated;
  }

  public void setWholesalePromoActivated(boolean wholesalePromoActivated) {
    this.wholesalePromoActivated = wholesalePromoActivated;
  }

  public List<ProductItemWholesalePriceResponse> getProductItemWholesalePriceResponses() {
    return productItemWholesalePriceResponses;
  }

  public void setProductItemWholesalePriceResponses(List<ProductItemWholesalePriceResponse> productItemWholesalePriceResponses) {
    this.productItemWholesalePriceResponses = productItemWholesalePriceResponses;
  }

  public List<ProductItemLevel3LogisticResponse> getProductItemLevel3LogisticResponse() {
    return productItemLevel3LogisticResponse;
  }

  public void setProductItemLevel3LogisticResponse(
      List<ProductItemLevel3LogisticResponse> productItemLevel3LogisticResponse) {
    this.productItemLevel3LogisticResponse = productItemLevel3LogisticResponse;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public Boolean getArchived() {
    return isArchived;
  }

  public void setArchived(Boolean archived) {
    isArchived = archived;
  }

  @Override
  public String toString() {
    return String
        .format("ProductItemLevel3Response [itemSku=%s, skuCode=%s, merchantSku=%s, upcCode=%s, itemName=%s, length=%s,"
                + " width=%s, height=%s, weight=%s, shippingWeight=%s, dangerousGoodsLevel=%s, lateFulfillment=%s,"
                + " pickupPointCode=%s, pickupPointName=%s, availableStockLevel1=%s, reservedStockLevel1=%s, "
                + "availableStockLevel2=%s, reservedStockLevel2=%s, synchronizeStock=%s, pristineId=%s,"
                + " prices=%s, viewConfigs=%s, images=%s, cogs=%s, productItemLevel3LogisticResponse=%s,"
                + " getItemSku()=%s, getCogs()=%s, getSkuCode()=%s,"
                + " getMerchantSku()=%s, getUpcCode()=%s, getItemName()=%s, getLength()=%s, getWidth()=%s, getHeight()=%s,"
                + " getWeight()=%s, getShippingWeight()=%s, getDangerousGoodsLevel()=%s, getLateFulfillment()=%s, "
                + "getPickupPointCode()=%s, getPickupPointName()=%s, getAvailableStockLevel1()=%s, "
                + "getReservedStockLevel1()=%s,"
                + " getAvailableStockLevel2()=%s, getReservedStockLevel2()=%s, getSynchronizeStock()=%s, getPrices()"
                + "=%s," + " getViewConfigs()=%s, getImages()=%s, isMerchantPromoDiscount()=%s, "
                + "isMerchantPromoDiscountActivated()=%s"
                + "disableUnSync=%s, priceEditDisabled=%s, isWholesalePromoActivated()=%s, isWholesalePriceActivated"
                + "()=%s, getProductItemLevel3LogisticResponse()=%s" + "getProductItemWholesalePriceResponses()=%s"
                + "isRejected()=%s, getIsArchived()=%s, getIsArchived()]",
            itemSku, skuCode, merchantSku, upcCode, itemName, length, width, height, weight,
            shippingWeight, dangerousGoodsLevel, lateFulfillment, pickupPointCode, pickupPointName,
            availableStockLevel1, reservedStockLevel1, availableStockLevel2, reservedStockLevel2,
            minimumStock, synchronizeStock, pristineId, prices, viewConfigs, images, cogs, productItemLevel3LogisticResponse, getItemSku(), getCogs(),
            getSkuCode(), getMerchantSku(), getUpcCode(), getItemName(), getLength(), getWidth(),
            getHeight(), getWeight(), getShippingWeight(), getDangerousGoodsLevel(),
            getLateFulfillment(), getPickupPointCode(), getPickupPointName(),
            getAvailableStockLevel1(), getReservedStockLevel1(), getAvailableStockLevel2(),
            getReservedStockLevel2(), getMinimumStock(), getSynchronizeStock(), getPristineId(), getPrices(),
            getViewConfigs(), getImages(), isMerchantPromoDiscount(), isMerchantPromoDiscountActivated(), disableUnSync,
            priceEditDisabled, isWholesalePromoActivated(), getWholesalePriceActivated(),
            getProductItemWholesalePriceResponses(), getProductItemLevel3LogisticResponse(), isMarkForDelete());
  }


}
