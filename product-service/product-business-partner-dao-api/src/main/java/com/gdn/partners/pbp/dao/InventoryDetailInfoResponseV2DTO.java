package com.gdn.partners.pbp.dao;

import java.util.List;

import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryResponseDTO;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class InventoryDetailInfoResponseV2DTO extends BaseResponse {

  private String webItemSku;
  private String webMerchantCode;
  private WebInventoryResponseV2DTO webInventoryResponse;
  private List<WarehouseInventoryResponseDTO> warehouseInventoryResponseList;
  private List<WarehouseInventoryResponseDTO> nonDistributionWarehouseInventoryResponseList;
}
