package com.gdn.x.product.model.vo;

import java.util.List;
import java.util.Set;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ItemPickupPointSummaryRequestVo {
  private String merchantCode;
  private List<String> productSkuList;
  private List<ItemPickupPointRequestVo> itemPickupPointCode;
  private List<String> pickupPointCodes;
  private Set<String> itemSkus;
  private Boolean onlineOrCnc;
  private Boolean online;
  private String sortOrder;
  private String sortField;
}
