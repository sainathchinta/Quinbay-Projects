package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

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
@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemInstantPickupResponse extends BaseResponse {

  private static final long serialVersionUID = 5833766070622173444L;
  private String itemSku;
  private String merchantSku;
  private String itemName;
  private String categoryName;
  private String pickupPointCode;
  private String pickupPointName;
  private Double listPrice;
  private Double price;
  private Integer offlineSafetyStock;
  private Integer offlineOriginalStock;
  private Integer offlineAvailableStock;
  private String productSku;

  @Override
  public String toString() {
    return "OfflineItemInstantPickupResponse{" + "itemSku='" + itemSku + '\'' + ", merchantSku='"
        + merchantSku + '\'' + ", itemName='" + itemName + '\'' + ", categoryName='" + categoryName
        + '\'' + ", pickupPointCode='" + pickupPointCode + '\'' + ", pickupPointName='"
        + pickupPointName + '\'' + ", listPrice=" + listPrice + '\'' + ", price=" + price
        + '\'' + ", offlineSafetyStock=" + offlineSafetyStock + '\'' + ", offlineOriginalStock="
        + offlineOriginalStock + '\'' + ", offlineAvailableStock=" + offlineAvailableStock + '}';
  }
}

