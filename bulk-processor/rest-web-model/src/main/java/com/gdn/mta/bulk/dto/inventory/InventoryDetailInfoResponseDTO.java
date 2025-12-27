package com.gdn.mta.bulk.dto.inventory;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class InventoryDetailInfoResponseDTO extends BaseResponse {
  private static final long serialVersionUID = -4807461217652566268L;
  private String webItemSku;
  private String webMerchantCode;
  private WebInventoryResponseDTO webInventoryResponse;
  private List<WarehouseInventoryResponseDTO> warehouseInventoryResponseList;
  private List<WarehouseInventoryResponseDTO> nonDistributionWarehouseInventoryResponseList;
}