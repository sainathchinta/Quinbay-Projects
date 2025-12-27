package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.apache.commons.lang3.tuple.Pair;

import java.io.Serializable;
import java.util.Map;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductDetailEditDTO implements Serializable {
  private static final long serialVersionUID = -6209831522297146702L;
  Pair<Product, Map<String, Map<String, String>>> productImageMapPair;
  Pair<Map<String, ProductDTO>, Map<ProductItem, String>> productAndProductItemMap;
  Product finalProduct;
  boolean productDetailsChanged;
  ProductAndItemLevelUpdatesDTO productAndItemLevelUpdatesDTO;
}
