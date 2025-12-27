package com.gdn.partners.pcu.external.client.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryRequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString(callSuper = true)
public class InventoryDetailStockInfoRequestDTO
    extends InventoryRequest {
  private static final long serialVersionUID = 1078863932549198859L;
  private String webMerchantCode;
  private List<String> webProductSkuList;
  private List<String> pickupPointCodes = new ArrayList<>();
}
