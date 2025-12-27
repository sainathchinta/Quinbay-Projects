package com.gdn.x.product.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ItemAndPickupPointBasicDetailVo {
  private String itemSku;
  private String itemCode;
  private String itemName;
  private String pickupPointCode;
  private String mainImageUrl;
}
