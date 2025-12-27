package com.gdn.x.productcategorybase.outbound.model;

import java.util.HashMap;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@AllArgsConstructor
@NoArgsConstructor
@Data
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class MatrixAttributeExtractionResponse {
  private String product_code;
  private String cn_code;
  private String title;
  private String brand;
  private String description;
  private String product_usp;
  private HashMap<String, Object> extractedAttributes;
}
