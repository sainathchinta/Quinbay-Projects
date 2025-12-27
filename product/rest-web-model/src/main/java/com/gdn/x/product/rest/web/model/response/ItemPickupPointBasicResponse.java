package com.gdn.x.product.rest.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPickupPointBasicResponse extends BaseResponse {
  private String itemSku;
  private String pickupPointCode;
  private ViewConfigResponse viewConfigResponse;
  private List<ItemViewConfigDTO> allItemViewConfigDTO;
}
