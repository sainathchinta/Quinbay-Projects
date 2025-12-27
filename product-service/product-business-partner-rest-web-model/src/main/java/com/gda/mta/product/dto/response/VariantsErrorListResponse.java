package com.gda.mta.product.dto.response;

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
public class VariantsErrorListResponse {
  private String itemSku;
  private String itemName;
  private String code;
  private String message;
  private String pickupPointCode;

  public VariantsErrorListResponse(String itemSku, String itemName, String code, String message) {
    this.itemSku = itemSku;
    this.itemName = itemName;
    this.code = code;
    this.message = message;
  }
}
