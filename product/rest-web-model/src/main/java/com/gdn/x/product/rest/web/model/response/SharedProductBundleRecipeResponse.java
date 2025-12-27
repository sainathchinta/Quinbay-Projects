package com.gdn.x.product.rest.web.model.response;

import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class SharedProductBundleRecipeResponse extends BaseResponse {
  private boolean sharedProduct;
  private String itemCode;
  private Set<SkuCodeBundleRecipeResponse> bundleRecipe;
}
