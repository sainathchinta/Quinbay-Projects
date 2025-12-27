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
@JsonInclude(JsonInclude.Include.ALWAYS)
public class VendorSummaryFilterWebRequest implements Serializable {


  private static final long serialVersionUID = 2596889198761241294L;

  private String keyword;
  private String timeFilterWebType;
  private Boolean contentPending;
  private Boolean imagePending;
  private Boolean assignment;
  private String categoryCode;
  private Boolean isCnCategory;
  private String businessPartnerCode;
  private String assigneeEmailId;
  private String sortOrder = "asc";
  private Boolean postLive;
  private String faultyType;
  private Boolean brandPending;
  private Boolean edited;
  private Boolean revised;
  private Boolean restrictedKeyword;
  private Boolean b2cActivated;
  private Boolean b2bActivated;
  private Boolean appealedProduct;
}
