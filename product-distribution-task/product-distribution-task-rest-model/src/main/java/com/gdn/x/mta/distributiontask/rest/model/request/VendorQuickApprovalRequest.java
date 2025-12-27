package com.gdn.x.mta.distributiontask.rest.model.request;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
@NoArgsConstructor
public class VendorQuickApprovalRequest {

  private String vendorCode;
  private String productCode;
  private String Notes;
  private boolean isBulkAction;
}
