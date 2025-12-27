package com.gda.mta.product.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UomStockValidationResponse {

  private String itemCode;
  private Boolean hasExistingStock;
  private Boolean hasPendingDocuments;
  private boolean success;
  private String errorMessage;
}