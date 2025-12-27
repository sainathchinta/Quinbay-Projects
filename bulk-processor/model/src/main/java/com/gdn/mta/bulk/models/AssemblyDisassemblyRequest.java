package com.gdn.mta.bulk.models;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
public class AssemblyDisassemblyRequest {

  private String productSku;
  private String itemSku;
  private String itemCode;
  private String itemName;
  private String sourceItemSku;
  private String sourceItemCode;
  private String sourceItemName;
  private String sourceProductSku;
  private int stockQuota;
  private boolean physicalBundle;
  private String merchantCode;
  private String merchantType;
  private String warehouseCode;
  private List<BundleRecipeRequest> bundleRecipe;
}
