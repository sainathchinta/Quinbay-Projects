package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

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
public class SuspensionProductRequest implements Serializable {

  private static final long serialVersionUID = 2596123498761241294L;
  private List<ProductLevel3Request> products = new ArrayList<>();
  private String action;
  private String reason;
  private String notes;
}
