package com.gdn.mta.bulk.dto;

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
public class QrCodeRowItemInfo {
  private String productSku;
  private String productName;
  private String productPrice;
  private String itemSku;
  private String pickupPointCode;
  private String pickupPointName;
  private boolean cncActivated;
  private String qrCodeBase64;
}
