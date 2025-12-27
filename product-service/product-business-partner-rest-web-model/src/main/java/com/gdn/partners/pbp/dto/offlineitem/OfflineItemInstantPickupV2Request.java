package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemInstantPickupV2Request implements Serializable {

  private static final long serialVersionUID = 1202926964289457148L;
  private String merchantCode;
  private List<String> gdnSkus;
  private List<String> merchantSkus;
  private List<String> pickupPointCodes;
  private String itemName;
  private String categoryCode;
}
