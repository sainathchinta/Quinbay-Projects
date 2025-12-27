package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.PICKUP_POINT_INVENTORY)
public class PickupPointInventory extends BaseData {

  private String itemSku;

  private String pickupPointCode;

  private Boolean syncStock;

  private boolean inStock;

  private String eventSource;

  private long eventTimestamp;

  @Override
  public List<String> toIds() {
    String id = MainUtil.toNotNullString(this.itemSku);
    String subId = MainUtil.toNotNullString(this.pickupPointCode);
    return MainUtil.toList(id,subId);
  }
}
