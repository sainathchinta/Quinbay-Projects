package com.gdn.x.product.model.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class BundleRecipe implements GdnBaseEmbedded {
  private static final long serialVersionUID = -5802883661352363363L;
  private String itemSku;
  private int quantity;
}
