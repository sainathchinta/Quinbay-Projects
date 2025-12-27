package com.gda.mta.product.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class DalamProductListRequest {

  private String storeId;
  private Boolean viewable;
  private Boolean activated;
  private String businessPartnerCode;
  private String categoryCode;
  private String keyword;
  private String endAge;
  private String startAge;
  private String lessThanAge;
  private String timeFilterType;
  private int page;
  private int size;
}
