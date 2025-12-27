package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class CreateFbbPickupPointRequest extends BaseRequest {
  private static final long serialVersionUID = -7320457808576846372L;

  private String businessPartnerCode;
  private String itemSku;
  private String pickupPointCode;
  private boolean isBuyable;
  private boolean isDiscoverable;
}
