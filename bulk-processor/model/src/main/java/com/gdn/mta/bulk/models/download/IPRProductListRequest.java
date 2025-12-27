package com.gdn.mta.bulk.models.download;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class IPRProductListRequest implements Serializable {
  private static final long serialVersionUID = -4808915635004049945L;
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
