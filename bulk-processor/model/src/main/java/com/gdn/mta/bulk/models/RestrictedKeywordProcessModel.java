package com.gdn.mta.bulk.models;

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
public class RestrictedKeywordProcessModel {
  private String storeId;
  private String internalBulkRequestId;
  private String internalBulkRequestCode;
  private String categoryCode;
  private String type;
}
