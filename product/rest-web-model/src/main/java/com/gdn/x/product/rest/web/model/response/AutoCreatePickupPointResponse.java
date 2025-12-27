package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoCreatePickupPointResponse extends BaseResponse {
  private static final long serialVersionUID = 6402682977582848430L;
  private String webItemSku;
  private String merchantCode;
  private String pickupPointCode;
  private boolean buyable;
  private boolean discoverable;
  private boolean cncBuyable;
  private boolean cncDiscoverable;
  private boolean newlyCreated;
  private double price;
  private String errorMessage;
  private boolean success;
}
