package com.gdn.partners.pbp.dao;

import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryResponseDTO;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@EqualsAndHashCode(callSuper = true)
public class WebInventoryResponseV2DTO extends WebInventoryResponseDTO {

  private Integer initialPreorderQuota;
  private Integer actualAvailableStock;
}
