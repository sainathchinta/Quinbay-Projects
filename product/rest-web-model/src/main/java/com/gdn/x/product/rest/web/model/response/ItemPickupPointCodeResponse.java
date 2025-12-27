package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.Date;

@Data
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPickupPointCodeResponse extends BaseResponse {
  private String itemSku;
  private String itemName;
  private String pickupPointCode;
  private String pickupPointName;
  private Date preOrderDate;

  public ItemPickupPointCodeResponse(String itemSku, String itemName, String pickupPointCode,
      String pickupPointName) {
    this.itemSku = itemSku;
    this.itemName = itemName;
    this.pickupPointCode = pickupPointCode;
    this.pickupPointName = pickupPointName;
  }

  public ItemPickupPointCodeResponse(String itemSku, String itemName, String pickupPointCode,
      String pickupPointName, Date preOrderDate) {
    this.itemSku = itemSku;
    this.itemName = itemName;
    this.pickupPointCode = pickupPointCode;
    this.pickupPointName = pickupPointName;
    this.preOrderDate = preOrderDate;
  }
}
