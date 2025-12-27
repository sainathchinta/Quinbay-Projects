package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
@Data
@EqualsAndHashCode(callSuper = false)
@AllArgsConstructor
@NoArgsConstructor
public class StockUpdateSearchEvent extends BaseData implements Serializable {

  @Serial
  private static final long serialVersionUID = -8344449984335581543L;

  private boolean syncStock;
  private boolean syncStockChanged;
  private UpdateSearchEventWarehouseDetails warehouseDetails;
  private String webMerchantCode;
  private String webItemSku;
  private String pickupPointCode;
  private boolean outOfStock;
  private boolean newlyCreated;
  private boolean dataUpdated;

  @Override
  public List<String> toIds() {
    String id = MainUtil.toNotNullString(this.webItemSku);
    String subId = MainUtil.toNotNullString(this.pickupPointCode);
    return MainUtil.toList(id,subId);
  }
}
