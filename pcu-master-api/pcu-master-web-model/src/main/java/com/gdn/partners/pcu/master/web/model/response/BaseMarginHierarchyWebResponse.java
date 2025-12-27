package com.gdn.partners.pcu.master.web.model.response;

import java.util.ArrayList;
import java.util.List;

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
public class BaseMarginHierarchyWebResponse {

  private String categoryCode;
  private String categoryName;
  private String categoryNameEnglish;
  private int level;
  private List<MarginDetailsWebResponse> marginDetails = new ArrayList<>();
}
