package com.gdn.partners.pcu.external.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@AllArgsConstructor
@NoArgsConstructor
@Data
@ToString
public class OrderPlacedWebResponse {
  private String productSku;
  private boolean isSuccessfulOrderPlaced;
}
