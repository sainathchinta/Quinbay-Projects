package com.gdn.partners.pcu.master.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
@Builder
public class ProfitMarginWebRequest {

  private String categoryCode;
  private String businessPartnerCode;
  private double basicProductPrice;
  private double sellingPrice;
}
