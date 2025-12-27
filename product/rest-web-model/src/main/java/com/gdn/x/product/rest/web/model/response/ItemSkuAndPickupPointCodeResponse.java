package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ItemSkuAndPickupPointCodeResponse extends BaseResponse implements Serializable {
  private static final long serialVersionUID = 2824969734721984925L;
  private String itemSku;
  private String pickupPointCode;
  private boolean buyable;
  private boolean discoverable;
  private boolean cncActive;
  private boolean markForDelete;

  public ItemSkuAndPickupPointCodeResponse(String itemSku, String pickupPointCode) {
    this.itemSku = itemSku;
    this.pickupPointCode = pickupPointCode;
  }
}

