package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ViewConfigResponse {
  private String id;
  private String channelId;
  private boolean display;
  private boolean buyable;
  private BuyableScheduleResponse buyableScheduleResponse;
  private DiscoverableScheduleResponse discoverableScheduleResponse;

  public ViewConfigResponse(String id, String channelId, boolean display, boolean buyable) {
    this.id = id;
    this.channelId = channelId;
    this.display = display;
    this.buyable = buyable;
  }
}
