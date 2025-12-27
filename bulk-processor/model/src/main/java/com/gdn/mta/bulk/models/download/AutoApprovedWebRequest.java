package com.gdn.mta.bulk.models.download;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoApprovedWebRequest {
  private String keyword;
  private String categoryCode;
  private String assignedTo;
  private String sellerCode;
  private String sortOrder;
  private Boolean b2bActivated;
  private List<String> productCodeList = new ArrayList<>();
}
