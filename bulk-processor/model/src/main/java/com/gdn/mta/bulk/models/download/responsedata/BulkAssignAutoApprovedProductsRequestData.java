package com.gdn.mta.bulk.models.download.responsedata;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class BulkAssignAutoApprovedProductsRequestData {
  private String productCode;
  private String productName;
  private String categoryName;
  private String storeName;
  private String assignee;
  private int excelRowNumber;
}
