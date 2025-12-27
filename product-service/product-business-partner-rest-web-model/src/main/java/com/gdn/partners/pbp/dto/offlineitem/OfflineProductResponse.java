package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineProductResponse {

  private String pickupPointCode;
  private Integer offlineSafetyStock;
  private Integer offlineOriginalStock;
  private Integer offlineAvailableStock;
  private Double listPrice;
  private Double price;
}
