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
public class SellerLogisticsProductResponse {
  private String logisticProductCode;
  private boolean isSelected;
  private String logisticOptionCode;
  private String logisticOptionName;
  private boolean requiredLongLat;
  private String additionalInformation;
  private String logisticProductName;
  private String highlightedInformation;
  private boolean isDownloadable;
  private boolean isHistoryAvailable;
  private String downloadInformation;
}
