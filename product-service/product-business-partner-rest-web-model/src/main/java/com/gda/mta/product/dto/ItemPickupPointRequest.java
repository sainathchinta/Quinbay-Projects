package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPickupPointRequest implements Serializable {

  private static final long serialVersionUID = 2275832462830197312L;
  private String pickupPointId;
  private String itemSku;
  private Double price;
  private Double salePrice;
  private Integer stock;
  private Integer initialPreOrderQuota;
  private Integer minimumStock;
  private boolean buyable;
  private boolean display;
  private boolean cncBuyable;
  private boolean cncDisplay;
  private boolean cncActive;
  private Boolean fbbActive;
  private boolean distribution;
  private Boolean wholesalePriceActivated;
  private Boolean synchronizeStock;
  private String sellerSku;
  private B2BFields b2bFields;
  private BuyableScheduleRequest buyableSchedule;
  private DiscoverableScheduleRequest discoverableSchedule;
  private List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests = new ArrayList<>();
  private boolean ppCodeChangedForNonMppSeller;
  private boolean scheduleRemoval;


  public Integer getInitialPreOrderQuota() {
    return Objects.nonNull(initialPreOrderQuota) ? initialPreOrderQuota : 0;
  }
}
