package com.gdn.partners.pcu.external.web.model.response;

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
public class WholesalePromoV2Response {

  private String itemSku;
  private boolean wholesalePromo;
  private String pickupPointCode;
}
