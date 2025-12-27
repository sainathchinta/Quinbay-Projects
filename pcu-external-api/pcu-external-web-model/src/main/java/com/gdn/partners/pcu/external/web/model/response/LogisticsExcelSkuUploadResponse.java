package com.gdn.partners.pcu.external.web.model.response;

import java.util.List;
import java.util.Set;

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
public class LogisticsExcelSkuUploadResponse {
  private List<String> invalidItemSkus;
  private List<String> invalidLogisticEntry;
  private Set<String> invalidLogisticProductHeader;
}
