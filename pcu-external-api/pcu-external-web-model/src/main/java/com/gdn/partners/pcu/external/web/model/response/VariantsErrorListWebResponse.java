package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class VariantsErrorListWebResponse {
  private String itemSku;
  private String itemName;
  private String code;
  private String message;
  private String pickupPointId;

  public VariantsErrorListWebResponse(String itemSku, String itemName, String code, String message) {
    this.itemSku = itemSku;
    this.itemName = itemName;
    this.code = code;
    this.message = message;
  }
}
