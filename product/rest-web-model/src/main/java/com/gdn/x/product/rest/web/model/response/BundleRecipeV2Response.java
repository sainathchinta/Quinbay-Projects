package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BundleRecipeV2Response extends ItemBasicDetailV2Response {

  private static final long serialVersionUID = -1161052721641354496L;

  private String productStatus;
  private int quantity;
}
