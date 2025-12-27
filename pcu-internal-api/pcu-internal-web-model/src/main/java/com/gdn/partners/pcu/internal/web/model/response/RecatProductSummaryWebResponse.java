package com.gdn.partners.pcu.internal.web.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class RecatProductSummaryWebResponse {
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
