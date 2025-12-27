package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class CheapestPrice extends BaseData {

  private String campaignCode;

  private List<SkuCheapestPriceDetailEventModel> skuCheapestPriceDetails;

  @Override
  public List<String> toIds() {
    String id = MainUtil.toNotNullString(this.campaignCode);
    return MainUtil.toList(id);
  }

  @Data
  @Builder
  @AllArgsConstructor
  @NoArgsConstructor
  public static class SkuCheapestPriceDetailEventModel {

    private String itemSku;

    private Integer cheapestPriceDays;

    @JsonProperty("pickUpPointCode")
    private String pickupPointCode;

    @JsonProperty("itemPickUpPointId")
    private String itemPickupPointId;

  }

}
