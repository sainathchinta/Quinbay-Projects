package com.gdn.partners.pcu.master.client.model;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SizeChartDeletionEventModel extends GdnBaseDomainEventModel {
  private String businessPartnerCode;
  private String sizeChartCode;
  private String storeId;
  private String username;
  private String requestId;
}
