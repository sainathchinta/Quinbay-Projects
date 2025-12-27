package com.gdn.mta.bulk.models;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class ProductBasicDetail implements Serializable {

  private static final long serialVersionUID = -4535532197959068729L;

  private String productCode;
  private String productName;
  private String brand;
}
