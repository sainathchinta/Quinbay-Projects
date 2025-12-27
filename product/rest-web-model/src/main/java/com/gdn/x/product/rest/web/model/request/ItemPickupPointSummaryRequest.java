package com.gdn.x.product.rest.web.model.request;

import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPickupPointSummaryRequest {
  private String merchantCode;
  private String itemSku;
  private Set<String> itemCodes;
  private String keyword;
  private List<String> productSkuList;
  private List<ItemPickupPointRequest> itemPickupPointCode;
  private List<String> pickupPointCodes;
  private Boolean onlineOrCnc;
  private Boolean online;
  private String sortOrder;
  private String sortField;
}
