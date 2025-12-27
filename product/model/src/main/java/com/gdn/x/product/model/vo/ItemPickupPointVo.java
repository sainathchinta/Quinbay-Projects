package com.gdn.x.product.model.vo;

import com.gdn.x.product.model.entity.ItemPickupPoint;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class ItemPickupPointVo extends ItemPickupPoint {
  private String itemName;
  private String pickupPointName;
  private Boolean wholesalePriceActivated;
}
