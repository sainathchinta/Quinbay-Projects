package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

@EqualsAndHashCode(callSuper = false)
@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class Level2InventoryQuantityChangedEvent extends BaseData implements Serializable {

  @Serial
  private static final long serialVersionUID = -5150585124321560037L;
  private String storeId;

  private String level2Id;

  private String level2MerchantCode;

  private int availableStock;

  private int originalStock;

  private int safetyStock;

  private boolean cnc;

  private List<WarehouseInfo> warehouseInfos;

  private String pickupPointCode;

  private Boolean syncStock;

  private String warehouseItemSku;

  private boolean preOrder;

  private boolean dataUpdated;
}