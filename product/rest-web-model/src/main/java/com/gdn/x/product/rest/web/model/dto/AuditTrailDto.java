package com.gdn.x.product.rest.web.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
public class AuditTrailDto {
  private String businessPartnerCode;
  private String gdnSku;
  private String actionKey;
  private String oldValue;
  private String newValue;
  private String attributeName;
  private String productSku;
  private String name;
  private String pickupPointCode;
  private boolean onlineStatus;

  public AuditTrailDto(String businessPartnerCode, String gdnSku, String actionKey, String oldValue,
      String newValue, String attributeName, String productSku, String name) {
    this.businessPartnerCode = businessPartnerCode;
    this.gdnSku = gdnSku;
    this.actionKey = actionKey;
    this.oldValue = oldValue;
    this.newValue = newValue;
    this.attributeName = attributeName;
    this.productSku = productSku;
    this.name = name;
  }
}
