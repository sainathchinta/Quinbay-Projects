package com.gdn.x.product.rest.web.model.request;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductBundleCreationRequest implements Serializable {

  private static final long serialVersionUID = -2771077236501570690L;
  private List<BundleRecipeVo> bundleRecipeList;
}
