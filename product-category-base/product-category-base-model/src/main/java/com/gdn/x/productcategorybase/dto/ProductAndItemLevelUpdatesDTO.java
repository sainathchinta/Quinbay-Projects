package com.gdn.x.productcategorybase.dto;

import java.util.HashSet;
import java.util.Set;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class ProductAndItemLevelUpdatesDTO {

  private boolean productLevelDataUpdated = true;
  private Set<String> updatedItemSkuCodes = new HashSet<>();

}
