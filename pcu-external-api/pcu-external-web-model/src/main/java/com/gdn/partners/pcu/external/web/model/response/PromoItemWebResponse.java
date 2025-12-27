package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class PromoItemWebResponse {
  private PromoMerchantWebResponse promoAdjustment;
  private String itemSku;
  private String productSku;
  private int originalQuota;
  private double priceDiscountPercentage;
  private double discountPrice;
  private int soldQuantity;
  private String detailUrl;
}
