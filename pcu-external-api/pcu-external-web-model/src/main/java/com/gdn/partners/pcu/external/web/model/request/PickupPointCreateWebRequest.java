package com.gdn.partners.pcu.external.web.model.request;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class PickupPointCreateWebRequest implements Serializable {

  private static final long serialVersionUID = -187293248529897870L;

  private String id;
  private String pickupPointId;
  private Integer minimumStock;
  private Integer stock;
  private Integer preOrderQuota;
  private boolean display;
  private boolean buyable;
  private boolean cncBuyable;
  private boolean cncDisplay;
  private Double discount;
  private double price;
  private double salePrice;
  private boolean cncActive;
  private Boolean wholesalePriceActivated;
  private List<ProductItemWholesalePriceWebRequest> productItemWholesalePriceRequests = new ArrayList<>();
  private boolean fbbActivated;
  private B2bFieldsWebRequest b2bFields;

  public PickupPointCreateWebRequest(String id, String pickupPointId, Integer minimumStock, Integer stock,
      boolean display, boolean buyable, Double discount, double price, double salePrice, boolean cncActive,
      Boolean wholesalePriceActivated, List<ProductItemWholesalePriceWebRequest> productItemWholesalePriceRequests) {
    this.id = id;
    this.pickupPointId = pickupPointId;
    this.minimumStock = minimumStock;
    this.stock = stock;
    this.display = display;
    this.buyable = buyable;
    this.discount = discount;
    this.price = price;
    this.salePrice = salePrice;
    this.cncActive = cncActive;
    this.wholesalePriceActivated = wholesalePriceActivated;
    this.productItemWholesalePriceRequests = productItemWholesalePriceRequests;
  }
}
