package com.gdn.partners.pcu.external.web.model.request;

import java.util.List;

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
public class DefaultConfigurationAndPickupPointRequest {
  private boolean pickupCodesUpdated;
  private boolean defaultConfigurationUpdated;
  private List<String> pickupCodes;
  private ProductSettingsRequest productSettings;
}
