package com.gdn.partners.pcu.external.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PriceChangeCompatibleRequest {
  private String categoryCode;
  private String itemSku;
  private double salePrice;
  private String pickUpPointCode;
}
