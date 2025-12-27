package com.gdn.partners.pcu.internal.web.model.request;

import java.io.Serializable;

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
@JsonInclude

public class VendorAutoAssignmentFilterWebRequest implements Serializable {

  private static final long serialVersionUID = -432095098021847332L;
  private String timeFilterWebType;
  private Boolean contentPending;
  private Boolean imagePending;
  private String businessPartnerCode;
  private Boolean postLive;
  private String faultyType;
  private Boolean brandPending;
  private Boolean edited;
  private Boolean revised;
  private Boolean restrictedKeyword;
}
