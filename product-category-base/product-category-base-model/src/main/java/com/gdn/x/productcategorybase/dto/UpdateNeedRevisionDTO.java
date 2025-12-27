package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.entity.Product;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateNeedRevisionDTO {
  Product product;
  ProductSalesCategoryMapping productSalesCategoryMapping;
  boolean isBrandChanged;
}
