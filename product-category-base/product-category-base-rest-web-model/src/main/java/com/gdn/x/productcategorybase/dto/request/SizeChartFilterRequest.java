package com.gdn.x.productcategorybase.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@RequiredArgsConstructor
@AllArgsConstructor
public class SizeChartFilterRequest {
  private String sizeChartName;
  private String sizeAttributeCode;
  private String brandCode;
  private List<String> businessPartnerCodes = new ArrayList<>();
  private String sortByFieldName;
  private String sortOrder;
  private Boolean waitingDeletion;
}