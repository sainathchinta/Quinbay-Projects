package com.gdn.mta.bulk.models.download.responsedata;

import java.io.Serializable;

import org.codehaus.jackson.annotate.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkPriceRebateRequestData implements Serializable {
  private String month;
  private String year;
  private String storeId;
  private String brandName;
  private String mainCategoryCode;
  private String categoryCode;
  private String projectedRebate;
  private int rowNumber;
}
