package com.gdn.partners.pcu.internal.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class AutoApprovedProductsActionWebRequest {

  private String action;
  private String notes;
  private String reason;
  private String productName;
  private boolean contentUpdated;
  private ProductWebRequest productWebRequest;
}
