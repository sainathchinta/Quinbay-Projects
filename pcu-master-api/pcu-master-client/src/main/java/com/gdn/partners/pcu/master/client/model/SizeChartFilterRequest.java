package com.gdn.partners.pcu.master.client.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
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
@JsonIgnoreProperties(ignoreUnknown = true)
public class SizeChartFilterRequest {
  private String sizeChartName;
  private String brandCode;
  private String sizeAttributeCode;
  private List<String> businessPartnerCodes = new ArrayList<>();
  private String sortByFieldName;
  private String sortOrder;
  private Boolean waitingDeletion;
}
