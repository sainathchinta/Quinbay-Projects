package com.gdn.x.product.rest.web.model.request;

import java.io.Serializable;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.product.model.vo.B2bFieldsVo;
import com.gdn.x.product.model.vo.BundleRecipeVo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class NeedCorrectionItemActivationRequest implements Serializable {

  private static final long serialVersionUID = -5703352454787557769L;
  private String itemSku;
  private double offerPrice;
  private double listPrice;
  private boolean isBuyable;
  private boolean isDiscoverable;
  private boolean cncBuyable;
  private boolean cncDiscoverable;
  private boolean cncActive;
  private boolean fbbActivated;
  private boolean distribution;
  private String merchantSku;
  private String pickupPointCode;
  private Boolean wholesalePriceActivated;
  private B2bFieldsVo b2bFields;
  private Set<BundleRecipeVo> bundleRecipe;

  public NeedCorrectionItemActivationRequest(String itemSku, double offerPrice, double listPrice, boolean isBuyable,
      boolean isDiscoverable, boolean cncActive, String merchantSku, String pickupPointCode,
      Boolean wholesalePriceActivated) {
    this.itemSku = itemSku;
    this.offerPrice = offerPrice;
    this.listPrice = listPrice;
    this.isBuyable = isBuyable;
    this.isDiscoverable = isDiscoverable;
    this.cncActive = cncActive;
    this.merchantSku = merchantSku;
    this.pickupPointCode = pickupPointCode;
    this.wholesalePriceActivated = wholesalePriceActivated;
  }
}
