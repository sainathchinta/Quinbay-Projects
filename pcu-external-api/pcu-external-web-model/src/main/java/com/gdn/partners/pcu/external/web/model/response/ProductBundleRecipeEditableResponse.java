package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ProductBundleRecipeEditableResponse extends BaseResponse {
  private static final long serialVersionUID = -1864286363046685129L;
  private String itemCode;
  private boolean bundleRecipeEditable;
}
