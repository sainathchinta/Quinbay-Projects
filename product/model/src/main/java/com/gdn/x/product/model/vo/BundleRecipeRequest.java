package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class BundleRecipeRequest implements Serializable {
  private static final long serialVersionUID = 733707391371267671L;
  private String itemSku;
  private Set<BundleRecipeVo> bundleRecipe = new HashSet<>();
}
