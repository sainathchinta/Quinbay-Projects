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
public class ProductDetailsRequest {

  private String productSku;
  private String itemSku;
  private String pickupPointName;
  private String pickupPointCode;
  private String productPrice;
  private String itemName;
}
