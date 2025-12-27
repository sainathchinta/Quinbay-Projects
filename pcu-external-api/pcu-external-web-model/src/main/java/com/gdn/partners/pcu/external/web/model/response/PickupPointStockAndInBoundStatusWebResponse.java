package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class PickupPointStockAndInBoundStatusWebResponse extends BaseResponse{
  String productSku;
  String pickupPointCode;
  boolean warehouseStockAvailable;
  boolean pendingInBounds;
}
