package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.mta.product.entity.ProductBundleRecipe;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductBundleRecipeRequest implements Serializable {

  private static final long serialVersionUID = 1990172346403305356L;
  private String itemSku;
  private Set<ProductBundleRecipe> bundleRecipe;
}
