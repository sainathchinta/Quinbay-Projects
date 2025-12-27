package com.gdn.x.product.rest.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class B2BResponse {
  private boolean managed;
  private Double basePrice;
}
