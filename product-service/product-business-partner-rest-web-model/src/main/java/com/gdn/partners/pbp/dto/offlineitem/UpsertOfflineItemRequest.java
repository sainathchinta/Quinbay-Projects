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
public class UpsertOfflineItemRequest implements Serializable {

  private static final long serialVersionUID = -4240442409564790120L;

  private String itemSku;
  private String pickupPointCode;
  private Integer stock;
  private Double listPrice;
  private Double offerPrice;
  private String fileName;
  private boolean cncActive = true;
  private boolean isBuyable = false;
  private boolean isDiscoverable = false;
}
