package com.gdn.x.productcategorybase.outbound.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@AllArgsConstructor
@NoArgsConstructor
@Data
@ToString
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class MatrixAttributeExtractionRequest {
  private String cn_code;
  private String title;
  private String description;
  private String brand;
  private String product_usp;
  private String product_code;
}
