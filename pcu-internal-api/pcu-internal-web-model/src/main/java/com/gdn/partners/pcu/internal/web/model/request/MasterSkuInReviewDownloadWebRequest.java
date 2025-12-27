package com.gdn.partners.pcu.internal.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class MasterSkuInReviewDownloadWebRequest {
  private String assignedTo;
  private String keyword;
  private String categoryCode;
  private long startDate;
  private long endDate;
  private List<AnchorMappingWebRequest> clusterRequestList;
}
