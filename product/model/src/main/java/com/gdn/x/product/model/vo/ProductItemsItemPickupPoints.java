package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemsItemPickupPoints implements Serializable {

  private static final long serialVersionUID = 7382609526027999919L;

  private Product product;
  private Map<String, Item> allItemMap = new HashMap<>();
  private Map<String, Item> updatedItemMap = new HashMap<>();
  private Map<String, ItemPickupPoint> allItemPickupPointMap = new HashMap<>();
  private Map<String, ItemPickupPoint> updatedItemPickupPointMap = new HashMap<>();
  private ProductDetailResponse productDetailResponse;
}
