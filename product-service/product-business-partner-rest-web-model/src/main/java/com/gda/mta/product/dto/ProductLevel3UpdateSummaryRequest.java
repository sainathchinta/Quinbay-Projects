package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.partners.pbp.commons.constants.Constants;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3UpdateSummaryRequest implements Serializable {

  private static final long serialVersionUID = -2558427933977009048L;
  private String productName;
  private String merchantSku;
  private Integer productType;
  private String pickupPointCode;
  private Boolean lateFulfillment;
  private Integer deltaStock;
  private Boolean synchronizeStock;
  private List<ProductLevel3PriceRequest> prices;
  private List<ProductLevel3ViewConfigRequest> viewConfigs;
  private Boolean off2OnActiveFlag;
  private String accessChannel;
  private Boolean wholesalePriceActivated;
  private Long version;
  
  public ProductLevel3UpdateSummaryRequest() {
    // do nothing
  }

  public ProductLevel3UpdateSummaryRequest(String merchantSku, Integer productType,
      String pickupPointCode, Boolean lateFulfillment, Integer deltaStock,
      Boolean synchronizeStock, List<ProductLevel3PriceRequest> prices,
      List<ProductLevel3ViewConfigRequest> viewConfigs) {
    super();
    this.merchantSku = merchantSku;
    this.productType = productType;
    this.pickupPointCode = pickupPointCode;
    this.lateFulfillment = lateFulfillment;
    this.deltaStock = deltaStock;
    this.synchronizeStock = synchronizeStock;
    this.prices = prices;
    this.viewConfigs = viewConfigs;
  }

  public ProductLevel3UpdateSummaryRequest(String merchantSku, Integer productType, String pickupPointCode,
      Boolean lateFulfillment, Integer deltaStock, Boolean synchronizeStock, List<ProductLevel3PriceRequest> prices,
      List<ProductLevel3ViewConfigRequest> viewConfigs, Boolean off2OnActiveFlag) {
    super();
    this.merchantSku = merchantSku;
    this.productType = productType;
    this.pickupPointCode = pickupPointCode;
    this.lateFulfillment = lateFulfillment;
    this.deltaStock = deltaStock;
    this.synchronizeStock = synchronizeStock;
    this.prices = prices;
    this.viewConfigs = viewConfigs;
    this.off2OnActiveFlag = off2OnActiveFlag;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public Integer getProductType() {
    return productType;
  }

  public void setProductType(Integer productType) {
    this.productType = productType;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public Boolean getLateFulfillment() {
    return lateFulfillment;
  }

  public void setLateFulfillment(Boolean lateFulfillment) {
    this.lateFulfillment = lateFulfillment;
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

  public Boolean getOff2OnActiveFlag() {
    return off2OnActiveFlag;
  }

  public void setOff2OnActiveFlag(Boolean off2OnActiveFlag) {
    this.off2OnActiveFlag = off2OnActiveFlag;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getAccessChannel() {
    return accessChannel;
  }

  public void setAccessChannel(String accessChannel) {
    this.accessChannel = accessChannel;
  }

  public Boolean getWholesalePriceActivated() {
    return wholesalePriceActivated;
  }

  public void setWholesalePriceActivated(Boolean wholesalePriceActivated) {
    this.wholesalePriceActivated = wholesalePriceActivated;
  }

  public Long getVersion() {
    return version;
  }

  public void setVersion(Long version) {
    this.version = version;
  }

  public boolean validateRequest() {
    if (StringUtils.isNotEmpty(this.merchantSku) && this.merchantSku.toLowerCase().contains(Constants.SCRIPT)) {
      return false;
    }
    if (StringUtils.isNotEmpty(this.accessChannel) && this.accessChannel.toLowerCase().contains(Constants.SCRIPT)) {
      return false;
    }
    if (StringUtils.isNotEmpty(this.pickupPointCode) && this.pickupPointCode.toLowerCase().contains(Constants.SCRIPT)) {
      return false;
    }
    if (StringUtils.isNotEmpty(this.productName) && this.productName.toLowerCase().contains(Constants.SCRIPT)) {
      return false;
    }
    if (Objects.nonNull(this.prices)) {
      for (ProductLevel3PriceRequest productLevel3PriceRequest : this.prices) {
        if (Objects.nonNull(productLevel3PriceRequest) && StringUtils
            .isNotEmpty(productLevel3PriceRequest.getPromotionName()) && productLevel3PriceRequest.getPromotionName()
            .toLowerCase().contains(Constants.SCRIPT)) {
          return false;
        }
        if (Objects.nonNull(productLevel3PriceRequest) && StringUtils
            .isNotEmpty(productLevel3PriceRequest.getChannelId()) && productLevel3PriceRequest.getChannelId()
            .toLowerCase().contains(Constants.SCRIPT)) {
          return false;
        }
      }
    }
    if (Objects.nonNull(this.viewConfigs)) {
      for (ProductLevel3ViewConfigRequest productLevel3ViewConfigRequest : this.viewConfigs) {
        if (Objects.nonNull(productLevel3ViewConfigRequest) && StringUtils
            .isNotEmpty(productLevel3ViewConfigRequest.getChannelId()) && productLevel3ViewConfigRequest.getChannelId()
            .toLowerCase().contains(Constants.SCRIPT)) {
          return false;
        }
      }
    }
    return true;
  }

  @Override
  public String toString() {
    final StringBuilder sb =
        new StringBuilder("ProductLevel3UpdateSummaryRequest{");
    sb.append("productName='").append(productName).append('\'');
    sb.append(", merchantSku='").append(merchantSku).append('\'');
    sb.append(", productType=").append(productType);
    sb.append(", pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append(", lateFulfillment=").append(lateFulfillment);
    sb.append(", deltaStock=").append(deltaStock);
    sb.append(", synchronizeStock=").append(synchronizeStock);
    sb.append(", prices=").append(prices);
    sb.append(", viewConfigs=").append(viewConfigs);
    sb.append(", off2OnActiveFlag=").append(off2OnActiveFlag);
    sb.append(", wholesalePriceActivated=").append(wholesalePriceActivated);
    sb.append(", version=").append(version);
    sb.append('}');
    return sb.toString();
  }
}
