package com.gdn.partners.pcu.master.web.model.response;

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
public class ProfitMarginWebResponse {

  private String categoryCode;
  private Double transactionFee;
  private Double minimumValue;
  private Double maximumValue;
  private String marginType;
  private double commission;
  private Double commissionPercentage;
  private double profitMargin;
  private double profitMarginPercentage;
}