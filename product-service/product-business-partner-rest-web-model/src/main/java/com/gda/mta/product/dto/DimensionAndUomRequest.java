package com.gda.mta.product.dto;

import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class DimensionAndUomRequest {

  private String uomCode;
  private String uomType;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double conversion;
  private Set<String> upcEanList = new HashSet<>();
}
