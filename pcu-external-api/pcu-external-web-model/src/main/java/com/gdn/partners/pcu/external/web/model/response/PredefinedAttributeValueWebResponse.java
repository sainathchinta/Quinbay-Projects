package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class PredefinedAttributeValueWebResponse {

  private String id;
  private String predefinedAllowedAttributeCode;
  private String value;
  private Integer sequence;
  private String brandApprovalStatus;
  private boolean protectedBrand;
}
