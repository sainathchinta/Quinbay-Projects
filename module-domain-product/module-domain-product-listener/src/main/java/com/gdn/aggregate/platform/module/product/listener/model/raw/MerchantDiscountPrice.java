package com.gdn.aggregate.platform.module.product.listener.model.raw;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.MERCHANT_DISCOUNT_PRICE)
public class MerchantDiscountPrice extends BaseData {

  private String merchantCode;

  private String productSku;

  private String itemSku;

  private String pickupPointCode;

  private Set<PickupPoint.Price> price = new HashSet<>();

  @Override
  public List<String> toIds() {
    return ModuleProductUtil.toMerchantDiscountPriceIds(this.itemSku,this.pickupPointCode);
  }

}
