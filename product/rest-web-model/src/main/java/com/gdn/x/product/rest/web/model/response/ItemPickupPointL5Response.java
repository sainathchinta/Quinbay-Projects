package com.gdn.x.product.rest.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
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
@JsonInclude
public class ItemPickupPointL5Response extends BaseResponse {
  private String productSku;
  private String itemSku;
  private String merchantSku;
  private String merchantCode;
  private String pickUpPointCode;
  private String pickUpPointName;
  private boolean cncActive;
  private boolean fbbActive;
  private List<PriceResponse> prices;
  private List<ViewConfigResponse> viewConfigs;
  private boolean markForDelete;
}
