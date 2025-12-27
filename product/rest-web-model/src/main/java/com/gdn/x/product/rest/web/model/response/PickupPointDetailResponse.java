package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
public class PickupPointDetailResponse extends BaseResponse {

  private String pickupPointCode;
  private String pickupPointName;
}
