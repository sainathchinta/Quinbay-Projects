package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by Vishal on 22/06/18.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class PickupPointCreateRequest implements Serializable {
  private static final long serialVersionUID = -4039038624164664258L;

  private String pickupPointId;
  private Integer stock;
  private Integer preOrderQuota;
  private Integer minimumStock;
  private boolean display;
  private boolean buyable;
  private boolean cncBuyable;
  private boolean cncDisplay;
  private Double price;
  private Double salePrice;
  private boolean cncActive;
  private Boolean wholesalePriceActivated;
  private B2bDetailsDTO b2bFields;
  private List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests = new ArrayList<>();
  private boolean fbbActivated;
  private boolean distribution;

  public PickupPointCreateRequest(String pickupPointId, Integer stock, Integer minimumStock, boolean display,
      boolean buyable, Double price, Double salePrice, boolean cncActive, Boolean wholesalePriceActivated,
      List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests) {
    this.pickupPointId = pickupPointId;
    this.stock = stock;
    this.minimumStock = minimumStock;
    this.display = display;
    this.buyable = buyable;
    this.price = price;
    this.salePrice = salePrice;
    this.cncActive = cncActive;
    this.wholesalePriceActivated = wholesalePriceActivated;
    this.productItemWholesalePriceRequests = productItemWholesalePriceRequests;
  }

  public Integer getPreOrderQuotaNullSafe() {
    return Optional.ofNullable(preOrderQuota).orElse(0);
  }
}
