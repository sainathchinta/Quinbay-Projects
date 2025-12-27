package com.gdn.partners.pcu.external.web.model.request;

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
