package com.gdn.x.productcategorybase.dto.response;

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
}
