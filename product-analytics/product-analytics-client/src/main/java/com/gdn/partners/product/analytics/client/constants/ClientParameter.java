package com.gdn.partners.product.analytics.client.constants;

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
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude()
public class ClientParameter {
  private String storeId;
  private String channelId;
  private String clientId;
  private String requestId;
  private String username;
  private String businessPartnerCode;
  private String userType;
  private String vendorCode;
}
