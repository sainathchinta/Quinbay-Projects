package com.gdn.partners.pcu.external.web.model.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.partners.pcu.external.web.model.B2BFields;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPickupPointWebRequest {

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
  private DiscoverableScheduleWebRequest discoverableSchedule;
  private BuyableScheduleWebRequest buyableSchedule;
  private boolean cncActive;
  private boolean fbbActive;
  private Boolean wholesalePriceActivated;
  private Boolean synchronizeStock;
  private String sellerSku;
  private B2BFields b2bFields;
  private Boolean pickupPointNotUpdated;
  private List<ProductItemWholesalePriceWebRequest> productItemWholesalePriceRequests = new ArrayList<>();
  private boolean scheduleRemoval;
}
