package com.gdn.aggregate.platform.module.product.listener.model.raw;

import java.io.Serial;
import java.io.Serializable;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class PickupPointUpsertCombinedEventModel extends BaseData implements Serializable {
  @Serial
  private static final long serialVersionUID = 3302051310879943838L;

  private String eventTrigger;
  private PickupPoint pickupPoint;
  private InventoryInfoChange inventoryInfoChange;
  private StockUpdateSearchEvent stockUpdateSearchEvent;
  private Level2InventoryQuantityChangedEvent level2InventoryQuantityChangedEvent;
  private boolean migration;
  private boolean fullReconstruct;
  private String traceId;
}
