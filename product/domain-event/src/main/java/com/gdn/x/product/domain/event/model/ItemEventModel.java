package com.gdn.x.product.domain.event.model;

import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemEventModel extends BaseResponse {
  private String itemSku;
  private String itemCode;
  private boolean isBuyable;
  private boolean isDiscoverable;
  private String channel;
  private Set<String> activePromoBundlings = new HashSet<>();
  private boolean isSynchronized;
  private String generatedItemName; // In MasterDataItem
  private Set<PriceModel> price;
  private boolean merchantPromoDiscount;
  private String merchantCode;
  private String merchantSku;
  private Boolean isLateFulfillment;
  private String pickupPointCode;
  private String ticketTemplateCode;
  private boolean off2OnChannelActive;
  private boolean isArchived;
  private boolean promoBundling;
  private boolean cncActivated;
  private String pristineId; // In PristineDataItem
  private boolean wholesalePriceExists;
  private boolean freeSample;
  private boolean markForDelete;
}
