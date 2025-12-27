package com.gdn.x.product.model.vo;

import java.util.ArrayList;
import java.util.List;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductCollectionsVo {
  private List<Product> products = new ArrayList<>();
  private List<Item> items = new ArrayList<>();
  private List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
}
