package com.gdn.partners.pbp.dao;

import java.util.Date;

import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryInsertRequestDTO;

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
public class WebInventoryInsertRequestV2DTO extends WebInventoryInsertRequestDTO {

  private Date preOrderEndDate;
  private Integer initialPoQuota;
  private boolean distributionPickupPoint;
}
