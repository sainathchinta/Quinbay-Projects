package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.model.entity.ItemViewConfig;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.HashSet;
import java.util.Set;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemSkuPickupPointResponse extends BaseResponse {

  private static final long serialVersionUID = 5390074572241750827L;
  private String itemSku;
  private String pickupPointCode;
  private Set<ItemViewConfig> itemViewConfig = new HashSet<>();

}
