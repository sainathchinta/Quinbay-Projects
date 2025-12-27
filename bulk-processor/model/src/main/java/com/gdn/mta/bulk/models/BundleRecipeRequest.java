package com.gdn.mta.bulk.models;

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
public class BundleRecipeRequest {

  private String itemSku;
  private int quantity;
  private String itemCode;
  private String itemName;
  private int cogsPercent;
}
