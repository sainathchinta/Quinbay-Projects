package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class InProgressProductsByPickupPointCodeResponse extends BaseResponse {
  private static final long serialVersionUID = -2335598169566595375L;
  private String productSku;
  private String pickupPointCode;
  private String itemSku;

}
