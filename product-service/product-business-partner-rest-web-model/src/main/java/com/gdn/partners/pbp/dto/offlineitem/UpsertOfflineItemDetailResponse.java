package com.gdn.partners.pbp.dto.offlineitem;

import java.io.Serializable;

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
public class UpsertOfflineItemDetailResponse implements Serializable {

  private static final long serialVersionUID = -8178296850548246452L;

  private String itemSku;
  private String pickupPointCode;
  private Integer stock;
  private Double listPrice;
  private Double price;
  private boolean cncActive;
  private boolean isBuyable;
  private boolean isDiscoverable;
}
