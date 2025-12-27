package com.gdn.mta.product.entity;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import com.gdn.common.web.base.BaseResponse;
import lombok.ToString;

@ToString
public class ProductItemLevel3 extends BaseResponse implements Serializable {

  private static final long serialVersionUID = 4030360904569422482L;
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
  private List<ProductLevel3Price> prices;
  private List<ProductLevel3ViewConfig> viewConfigs;
  private List<ProductLevel3Image> images = new ArrayList<>();
  private Boolean wholesalePriceActivated;
  private boolean wholesalePromoActivated;
  private List<ProductItemWholesalePriceVo> productItemWholesalePrices = new ArrayList<>();
  private Double cogs;
  private Boolean isArchived;
  private String cogsErrorCode;
  private boolean promoBundling;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean priceEditDisabled;
  private boolean disableUnSync;
  private List<ProductLevel3Logistics> logistics;
  private boolean isContentChanged;
  private boolean markForDelete;
  private boolean freeSample;
  private List<ProductLevel3Attribute> itemAttributes = new ArrayList<>();
  private TreeMap<String, String> itemAttributesMap = new TreeMap<>();
  private TreeMap<String, String> attributesValueTypeMap = new TreeMap<>();


  public ProductItemLevel3() {
    // do nothing
  }

  public ProductItemLevel3(String itemSku, String skuCode, String merchantSku, String upcCode,
                           String itemName, Double length, Double width, Double height, Double weight,
                           Double shippingWeight, Integer dangerousGoodsLevel, Boolean lateFulfillment,
                           String pickupPointCode, String pickupPointName, Integer availableStockLevel1,
                           Integer reservedStockLevel1, Integer availableStockLevel2, Integer reservedStockLevel2,
                           Integer minimumStock, Boolean synchronizeStock, List<ProductLevel3Price> prices,
                           List<ProductLevel3ViewConfig> viewConfigs, List<ProductLevel3Image> images, List<ProductLevel3Logistics> logistics) {
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
    this.logistics = logistics;
  }

  public ProductItemLevel3(String itemSku, String skuCode, String merchantSku, String upcCode,
                           String itemName, Double length, Double width, Double height, Double weight,
                           Double shippingWeight, Integer dangerousGoodsLevel, Boolean lateFulfillment,
                           String pickupPointCode, String pickupPointName, Integer availableStockLevel1,
                           Integer reservedStockLevel1, Integer availableStockLevel2, Integer reservedStockLevel2,
                           Integer minimumStock, Boolean synchronizeStock, List<ProductLevel3Price> prices,
                           List<ProductLevel3ViewConfig> viewConfigs, List<ProductLevel3Image> images, Double cogs, List<ProductLevel3Logistics> logistics) {
    this(itemSku, skuCode, merchantSku, upcCode, itemName, length, width, height, weight,
        shippingWeight, dangerousGoodsLevel, lateFulfillment, pickupPointCode, pickupPointName,
        availableStockLevel1, reservedStockLevel1, availableStockLevel2, reservedStockLevel2,
        minimumStock, synchronizeStock, prices, viewConfigs, images, logistics);
    this.cogs = cogs;
  }

  public ProductItemLevel3(String itemSku, String skuCode, String merchantSku, String upcCode,
                           String itemName, Double length, Double width, Double height, Double weight,
                           Double shippingWeight, Integer dangerousGoodsLevel, Boolean lateFulfillment,
                           String pickupPointCode, String pickupPointName, Integer availableStockLevel1,
                           Integer reservedStockLevel1, Integer availableStockLevel2, Integer reservedStockLevel2,
                           Integer minimumStock, Boolean synchronizeStock, String pristineId, List<ProductLevel3Price> prices,
                           List<ProductLevel3ViewConfig> viewConfigs, List<ProductLevel3Image> images, Double cogs, List<ProductLevel3Logistics> logistics) {
    this(itemSku, skuCode, merchantSku, upcCode, itemName, length, width, height, weight,
        shippingWeight, dangerousGoodsLevel, lateFulfillment, pickupPointCode, pickupPointName,
        availableStockLevel1, reservedStockLevel1, availableStockLevel2, reservedStockLevel2,
        minimumStock, synchronizeStock, prices, viewConfigs, images, cogs, logistics);
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

  public List<ProductLevel3Price> getPrices() {
    return prices;
  }

  public void setPrices(List<ProductLevel3Price> prices) {
    this.prices = prices;
  }

  public List<ProductLevel3ViewConfig> getViewConfigs() {
    return viewConfigs;
  }

  public void setViewConfigs(List<ProductLevel3ViewConfig> viewConfigs) {
    this.viewConfigs = viewConfigs;
  }

  public List<ProductLevel3Image> getImages() {
    return images;
  }

  public void setImages(List<ProductLevel3Image> images) {
    this.images = images;
  }

  public Integer getMinimumStock() {
    return minimumStock;
  }

  public void setMinimumStock(Integer minimumStock) {
    this.minimumStock = minimumStock;
  }

  public Boolean getArchived() { return isArchived; }

  public void setArchived(Boolean archived) { isArchived = archived; }

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

  public List<ProductItemWholesalePriceVo> getProductItemWholesalePrices() {
    return productItemWholesalePrices;
  }

  public void setProductItemWholesalePrices(List<ProductItemWholesalePriceVo> productItemWholesalePrices) {
    this.productItemWholesalePrices = productItemWholesalePrices;
  }

  public List<ProductLevel3Logistics> getLogistics() {
    return logistics;
  }

  public void setLogistics(List<ProductLevel3Logistics> logistics) {
    this.logistics = logistics;
  }

  public boolean isContentChanged() {
    return isContentChanged;
  }

  public void setContentChanged(boolean contentChanged) {
    isContentChanged = contentChanged;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public boolean getFreeSample() {
    return freeSample;
  }

  public void setFreeSample(boolean freeSample) {
    this.freeSample = freeSample;
  }

  public TreeMap<String, String> getItemAttributesMap() {
    return itemAttributesMap;
  }

  public void setItemAttributesMap(TreeMap<String, String> itemAttributesMap) {
    this.itemAttributesMap = itemAttributesMap;
  }

  public List<ProductLevel3Attribute> getItemAttributes() {
    return itemAttributes;
  }

  public void setItemAttributes(List<ProductLevel3Attribute> itemAttributes) {
    this.itemAttributes = itemAttributes;
  }

  public TreeMap<String, String> getAttributesValueTypeMap() {
    return attributesValueTypeMap;
  }

  public void setAttributesValueTypeMap(TreeMap<String, String> attributesValueTypeMap) {
    this.attributesValueTypeMap = attributesValueTypeMap;
  }
}
