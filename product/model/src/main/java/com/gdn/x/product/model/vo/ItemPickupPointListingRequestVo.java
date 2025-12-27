package com.gdn.x.product.model.vo;

import java.util.Set;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ItemPickupPointListingRequestVo {
  private String businessPartnerCode;
  private String productSku;
  private Set<String> itemSkus;
  private Set<String> pickupPointCodes;
  private boolean isFbbSortRequired;
}
