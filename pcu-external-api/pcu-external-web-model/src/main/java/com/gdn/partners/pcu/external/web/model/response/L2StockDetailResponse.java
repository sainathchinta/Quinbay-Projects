package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class L2StockDetailResponse extends BaseResponse {
  private static final long serialVersionUID = 6844430800910542803L;
  private String warehouseItemSku;
  private boolean distributionWarehouseAvailable;
  private boolean nonDistributionWarehouseAvailable;
}