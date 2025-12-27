package com.gdn.x.mta.distributiontask.domain.event.model;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class VendorSearchAutoHealEventModel extends GdnBaseDomainEventModel {
  private String storeId;
  private String productCode;
}
