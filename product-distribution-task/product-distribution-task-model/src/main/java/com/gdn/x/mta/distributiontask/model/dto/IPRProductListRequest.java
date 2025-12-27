package com.gdn.x.mta.distributiontask.model.dto;


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
public class IPRProductListRequest {

  private static final long serialVersionUID = 6358965271155090308L;
  private String keyword;
  private String timeFilterWebType;
  private String state;
  private String businessPartnerCode;
  private String categoryCode;
  private String brandCode;
  private String sortOrder;
  private String assignedTo;
  private Boolean assigned;
}
