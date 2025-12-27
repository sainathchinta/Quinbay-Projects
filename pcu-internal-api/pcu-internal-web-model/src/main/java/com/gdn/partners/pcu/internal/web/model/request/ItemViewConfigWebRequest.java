package com.gdn.partners.pcu.internal.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ItemViewConfigWebRequest {

  private String itemSku;
  private String pickupPointCode;
  private boolean isBuyable;
  private boolean isDiscoverable;
  private String channel;

}