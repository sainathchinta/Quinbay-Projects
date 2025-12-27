package com.gdn.x.product.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class XProdAttributeMigrationEventModel {
  private String productCode;
  private String attributeCode;
  private String attributeName;
  private String attributeValue;
  private boolean skuValue;
  private String productSku;
}
