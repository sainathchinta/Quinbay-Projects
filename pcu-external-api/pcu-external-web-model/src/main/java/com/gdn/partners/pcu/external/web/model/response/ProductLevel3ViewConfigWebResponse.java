package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3ViewConfigWebResponse {
  private static final long serialVersionUID = 6328542631160859870L;
  private String id;
  private String channelId;
  private Boolean display;
  private Boolean buyable;
  private BuyableScheduleWebResponse buyableSchedule;
  private DiscoverableScheduleWebResponse discoverableSchedule;
}
