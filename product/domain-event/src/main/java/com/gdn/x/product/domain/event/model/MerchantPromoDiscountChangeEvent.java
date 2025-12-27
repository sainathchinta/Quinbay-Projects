package com.gdn.x.product.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

/**
 * Created by govind on 22/04/2019 AD.
 */
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class MerchantPromoDiscountChangeEvent extends ProductBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -6391831694666874911L;
  private String itemSku;
  private String productSku;
  private String merchantCode;
  private Set<Price> price = new HashSet<Price>();
  private String pickupPointCode;
  @Override
  public String toString() {
    return String.format(
      "MerchantPromoDiscountChangeEvent [itemSku=%s, productSku=%s, merchantCode=%s, price=%s, toString()=%s]",
      this.itemSku, this.productSku, this.merchantCode, this.price, this.pickupPointCode,
      super.toString());
  }
}
