package com.gdn.x.product.model.vo;

import com.gdn.x.product.model.entity.Item;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Set;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ItemVo extends Item {
  private boolean cncActive;
  private Set<String> sellerActivePromoBundlings;
  private List<ItemPickupPointVo> itemPickupPointVoList;
}
