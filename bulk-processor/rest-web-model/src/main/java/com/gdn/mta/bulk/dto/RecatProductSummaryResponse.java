package com.gdn.mta.bulk.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class RecatProductSummaryResponse extends BaseResponse {
  private String recatRequestCode;
  private String productCode;
  private String productName;
  private String categoryCode;
  private String categoryName;
  private String newCategoryCode;
  private String newCategoryName;
  private String status;
  private Date updatedDate;
  private String notes;
}
