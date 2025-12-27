package com.gdn.partners.pbp.dao;

import java.util.Date;

import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryRequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UpdatePoDateByL3RequestDTO extends InventoryRequest {

  private String productSku;
  private Date preOrderEndDate;
}
