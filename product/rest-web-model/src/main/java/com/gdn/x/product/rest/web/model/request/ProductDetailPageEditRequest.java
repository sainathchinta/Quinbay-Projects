package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.rest.web.model.enums.EditChangeType;
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
public class ProductDetailPageEditRequest extends BaseRequest {

  private static final long serialVersionUID = 2892309312963038724L;

  private ProductEditRequest productEditRequest;
  private ItemPickupPointUpdateRequest itemPickupPointUpdateRequest;
  private EditChangeType editChangeType;
}
