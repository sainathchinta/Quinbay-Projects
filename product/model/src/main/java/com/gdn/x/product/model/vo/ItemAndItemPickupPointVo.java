package com.gdn.x.product.model.vo;

import java.util.List;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ItemAndItemPickupPointVo {
  private List<Item> item;
  private List<ItemPickupPoint> itemPickupPoints;
  private long totalL5Count;
}
