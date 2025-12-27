package com.gdn.mta.bulk.dto.inventory;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;


@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class WarehouseInventoryResponseDTO extends BaseResponse implements Serializable {

  private static final long serialVersionUID = 7671323481537127760L;
  private String warehouseItemSku;
  private String warehouseMerchantCode;
  private Integer originalStock;
  private Integer availableStock;
  private String warehouseCode;
  private String oosSchedulerMarker;
}