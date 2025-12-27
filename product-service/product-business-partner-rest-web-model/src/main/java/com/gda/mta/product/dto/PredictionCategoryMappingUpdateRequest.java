package com.gda.mta.product.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PredictionCategoryMappingUpdateRequest {
  private String categoryCode;
  private boolean markForDelete;
}
