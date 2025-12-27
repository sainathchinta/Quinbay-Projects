package com.gdn.x.productcategorybase.dto;

import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.entity.Product;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductPublishUpdateDTO {

  private Product product;
  private boolean productLevelDataUpdated = true;
  private Set<String> updatedItemSkuCodes = new HashSet<>();
  private ProductDTO productDTO;
  public ProductPublishUpdateDTO(Product product, boolean productLevelDataUpdated,
    Set<String> updatedItemSkuCodes) {
    this.product = product;
    this.productLevelDataUpdated = productLevelDataUpdated;
    this.updatedItemSkuCodes = updatedItemSkuCodes;
  }
}
