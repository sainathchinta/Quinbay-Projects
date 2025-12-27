package com.gda.mta.product.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class FailedRetryProductsResponse {

  private String productSku;
  private String createdDate;
  private String updatedDate;
}
