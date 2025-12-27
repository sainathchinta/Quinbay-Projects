package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.CHEAPEST_PRICE_DAY)
public class CheapestPriceDay extends BaseData {

  private String campaignCode;

  private String productSku;

  private String itemSku;

  private String pickupPointCode;

  private Integer days;

  @Override
  public List<String> toIds() {
    return ModuleProductUtil.toCheapestPriceDayIds(this.campaignCode,this.itemSku,this.pickupPointCode);
  }

}
