package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductSkuPickupPointResponseV2 extends BaseResponse implements Serializable {

  @Serial
  private static final long serialVersionUID = 7937859329173919027L;
  private String productSku;
  private String pickupPointCode;
  private String itemSku;
  private Set<ItemViewConfigDTO> itemViewConfig = new HashSet<>();
}
