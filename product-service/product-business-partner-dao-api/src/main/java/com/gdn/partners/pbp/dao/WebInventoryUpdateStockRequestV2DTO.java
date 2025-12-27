package com.gdn.partners.pbp.dao;

import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdateStockRequestDTO;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString(callSuper = true)
public class WebInventoryUpdateStockRequestV2DTO extends WebInventoryUpdateStockRequestDTO {

  private int preOrderQuota;
}
