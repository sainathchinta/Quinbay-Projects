package com.gda.mta.product.dto;

import java.util.List;

import com.gdn.x.businesspartner.dto.ProfileResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SyncStockUpdateOrInsertVo {
  private UpdateOrInsertStockVo updateOrInsertStockVo;
  private List<ItemSkuPickupPointSyncStockDto> pickupPointSyncStockDtoList;
}
