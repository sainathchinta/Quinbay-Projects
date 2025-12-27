package com.gdn.x.product.model.vo;

import java.util.HashSet;
import java.util.Set;

import com.gdn.x.product.model.entity.ItemViewConfig;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class B2bFieldsVo {
  private boolean managed;
  private Double basePrice;
  private Set<ItemViewConfig> b2bItemViewConfigs = new HashSet<>();
}
