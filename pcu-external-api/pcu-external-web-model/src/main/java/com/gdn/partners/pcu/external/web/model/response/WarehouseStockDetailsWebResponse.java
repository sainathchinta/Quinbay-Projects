package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class WarehouseStockDetailsWebResponse extends BaseResponse {
  private static final long serialVersionUID = -593495513336030097L;
  private String itemCode;
  private boolean warehouseStockAvailable;
  private boolean pendingInBounds;
}
