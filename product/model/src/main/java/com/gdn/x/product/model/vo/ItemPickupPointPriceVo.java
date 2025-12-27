package com.gdn.x.product.model.vo;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPickupPointPriceVo extends BaseResponse {

  private String offlineItemId;
  private String merchantCode;
  private String itemSku;
  private String pickupPointCode;
  private Double listPrice;
  private Double offerPrice;
  private Long version;
}
