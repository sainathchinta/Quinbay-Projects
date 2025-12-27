package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
public class PickupPointDetailWebResponse {

  private String pickupPointCode;
  private String pickupPointName;
}
