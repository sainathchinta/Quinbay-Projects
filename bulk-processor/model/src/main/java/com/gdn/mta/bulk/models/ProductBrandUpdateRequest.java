package com.gdn.mta.bulk.models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
public class ProductBrandUpdateRequest {
  private String productCode;
  private String oldBrandCode;
  private String newBrandCode;
}
