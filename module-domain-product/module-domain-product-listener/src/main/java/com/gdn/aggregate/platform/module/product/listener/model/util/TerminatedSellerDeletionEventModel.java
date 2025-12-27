package com.gdn.aggregate.platform.module.product.listener.model.util;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TerminatedSellerDeletionEventModel {
  private String productCode;
  private String sellerCode;
  private String productSku;
  private boolean sharedProduct;
}