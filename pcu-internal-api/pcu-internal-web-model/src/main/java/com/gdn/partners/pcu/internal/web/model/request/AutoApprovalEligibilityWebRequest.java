package com.gdn.partners.pcu.internal.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class AutoApprovalEligibilityWebRequest {

  private String categoryCode;
  private boolean edited;
  private String reviewType;
  private boolean revised;
  private String productCode;
  private String storeId;

}