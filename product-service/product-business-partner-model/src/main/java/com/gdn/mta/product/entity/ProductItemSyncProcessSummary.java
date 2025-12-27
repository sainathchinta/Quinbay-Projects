package com.gdn.mta.product.entity;

import com.gdn.mta.product.enums.ProductSyncStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ProductItemSyncProcessSummary {

  private ProductSyncStatus productSyncStatus;

  private long count;

  private String businessPartnerCode;

  private String processId;

}
