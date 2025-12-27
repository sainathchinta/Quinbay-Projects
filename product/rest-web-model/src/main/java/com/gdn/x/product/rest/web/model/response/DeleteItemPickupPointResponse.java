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
public class DeleteItemPickupPointResponse extends BaseResponse {
  private static final long serialVersionUID = 3410870922510178849L;
  private String itemSku;
  private String pickupPointCode;
  private String reason;
  private String newPickupPointCode;
}
