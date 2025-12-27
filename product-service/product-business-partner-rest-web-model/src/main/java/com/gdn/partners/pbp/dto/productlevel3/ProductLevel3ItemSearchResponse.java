package com.gdn.partners.pbp.dto.productlevel3;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@NoArgsConstructor
@AllArgsConstructor
public class ProductLevel3ItemSearchResponse extends BaseResponse {
  private static final long serialVersionUID = -4443095076323774490L;

  private String businessPartnerCode;
  private String productSku;
  private String productCode;
  private String itemSku;
  private String itemCode;
  private String merchantSku;
  private String itemName;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getBusinessPartnerCode() {
    return this.businessPartnerCode;
  }

  public String getItemCode() {
    return this.itemCode;
  }

  public String getItemName() {
    return this.itemName;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public String getMerchantSku() {
    return this.merchantSku;
  }

  public String getProductCode() {
    return this.productCode;
  }

  public String getProductSku() {
    return this.productSku;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public void setItemName(String itemName) {
    this.itemName = itemName;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
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

  @Override
  public String toString() {
    return String.format(
        "ProductLevel3ItemSearchResponse [businessPartnerCode=%s, productSku=%s, productCode=%s, itemSku=%s, itemCode=%s, merchantSku=%s, itemName=%s, merchantPromoDiscount=%s, merchantPromoDiscountActivated=%s, toString()=%s]",
        this.businessPartnerCode, this.productSku, this.productCode, this.itemSku, this.itemCode,
        this.merchantSku, this.itemName, this.merchantPromoDiscount,
        this.merchantPromoDiscountActivated, super.toString());
  }
}
