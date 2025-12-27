package com.gdn.partners.pcu.internal.web.model.response;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandRejectionWebResponse {

  private String id;
  private String rejectionReason;
  private String brandName;
  private String brandRequestCode;

}
