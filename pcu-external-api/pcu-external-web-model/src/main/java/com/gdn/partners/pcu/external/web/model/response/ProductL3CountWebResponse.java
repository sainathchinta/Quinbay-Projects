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
public class ProductL3CountWebResponse {
  private Long all;
  private Long active;
  private Long outOfStock;
  private Long inReview;
  private Long needCorrection;
  private Long archived;
  private Long suspended;
  private Long rejected;
  private String businessPartnerCode;
}
