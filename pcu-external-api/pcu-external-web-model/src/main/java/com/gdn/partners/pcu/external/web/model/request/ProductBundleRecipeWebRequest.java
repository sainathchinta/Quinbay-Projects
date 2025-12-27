package com.gdn.partners.pcu.external.web.model.request;

import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductBundleRecipeWebRequest extends BaseRequest {

  private static final long serialVersionUID = 2528481489046605272L;

  private String itemSku;
  private Set<ProductBundleWebRecipe> bundleRecipe = new HashSet<>();
}
