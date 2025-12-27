package com.gdn.partners.product.orchestrator.model.product.level1;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.domain.Sort;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProductLevel1Filter implements Serializable {

  private String storeCode;
  private String storeName;

  @Builder.Default
  private Set<String> productIds = new HashSet<>();
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

  @Builder.Default
  private Sort.Direction sortDirection = Sort.Direction.ASC;

}
