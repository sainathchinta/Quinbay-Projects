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
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCountWebResponse {

  private Long outOfStock;
  private Long available;
  private Long minimumStock;
  private Long archived;
  private Long suspended;
  private Long rejected;
  private Long needCorrection;
  private Long needAction;
  private Long waitingForApproval;
  private Long totalCounts;
  private String businessPartnerCode;
}
