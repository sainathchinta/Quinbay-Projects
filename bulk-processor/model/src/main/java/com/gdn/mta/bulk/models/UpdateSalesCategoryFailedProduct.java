package com.gdn.mta.bulk.models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UpdateSalesCategoryFailedProduct extends InternalBulkProcessFailedData {
  private String productSku;
  private String operationType;
  private String cnCategoryCode;
  private String errorMessage;
}
