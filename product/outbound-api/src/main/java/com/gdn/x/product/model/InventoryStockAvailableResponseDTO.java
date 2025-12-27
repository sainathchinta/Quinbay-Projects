package com.gdn.x.product.model;

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
public class InventoryStockAvailableResponseDTO extends BaseResponse {
  private static final long serialVersionUID = 7729585129032346720L;
  private boolean stockAvailable;
}
