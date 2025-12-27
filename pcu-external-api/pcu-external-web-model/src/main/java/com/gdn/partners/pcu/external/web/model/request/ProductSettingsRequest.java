package com.gdn.partners.pcu.external.web.model.request;

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
public class ProductSettingsRequest {
  private boolean regular;
  private boolean fbb;
  private boolean offlineToOnline;
  private boolean cnc;
}
