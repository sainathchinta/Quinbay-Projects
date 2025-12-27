package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.model.entity.ItemViewConfig;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.util.HashSet;
import java.util.Set;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductSkuPickupPointResponse extends BaseResponse {

  @Serial
  private static final long serialVersionUID = 7937859329173919026L;
  private String productSku;
  private String pickupPointCode;
  private String itemSku;
  private Set<ItemViewConfig> itemViewConfig = new HashSet<>();
}
