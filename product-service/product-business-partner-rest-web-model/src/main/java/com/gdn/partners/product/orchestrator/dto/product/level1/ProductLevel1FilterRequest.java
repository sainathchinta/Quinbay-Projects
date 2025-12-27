package com.gdn.partners.product.orchestrator.dto.product.level1;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProductLevel1FilterRequest implements Serializable {

  private String storeCode;
  private String storeName;
  private String code;
  private String name;
  private String brandCode;
  private String brandName;
  private String categoryCode;
  private String categoryName;
  private String createdBy;
  private String merchantSku;

  @Builder.Default
  private Set<String> states = new HashSet<>();
  private String sortedBy;
  private String sortDirection;

}
