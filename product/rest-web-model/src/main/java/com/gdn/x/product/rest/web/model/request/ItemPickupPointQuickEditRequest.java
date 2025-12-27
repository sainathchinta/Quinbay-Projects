package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPickupPointQuickEditRequest {

  private String itemSku;
  private PriceDTO price;
  private String status;
  private String cncStatus;
  private Boolean wholeSaleActivated;
  private Boolean off2OnActiveFlag;
  private String merchantSku;
  private String pickupPointCode;
  private Long version;
  private boolean cncActivated;
  private boolean fbbActivated;
  private Boolean distribution;
  private String merchantCode;
  private boolean ppCodeChangedForNonMppSeller;
  private B2bFields b2bFields;
  private boolean scheduleUpdate;
  private BuyableScheduleRequest buyableSchedule;
  private DiscoverableScheduleRequest discoverableSchedule;

}
