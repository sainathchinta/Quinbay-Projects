package com.gdn.partners.pcu.internal.client.model.request;

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
public class MasterSkuItemsListWebRequest {
  private String keyword;
  private String categoryCode;
  private Long startDate;
  private Long endDate;
  private String masterItemSku;
}
